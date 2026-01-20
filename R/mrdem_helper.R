`%||%` <- function(a, b) if (!is.null(a)) a else b

# Known-good fallbacks (VRTs published for streaming)
.MRDEM_DTM_VRT        <- "https://datacube-prod-data-public.s3.ca-central-1.amazonaws.com/store/elevation/mrdem/mrdem-30/mrdem-30-dtm.vrt"
.MRDEM_DTM_HILLSHADE  <- "https://datacube-prod-data-public.s3.ca-central-1.amazonaws.com/store/elevation/mrdem/mrdem-30/mrdem-30-dtm-hillshade.vrt"

# ---- Safe driver detection ----
.gdal_supports_cog <- function() {
  drv <- suppressWarnings(try(terra::gdal(drivers=T), silent = TRUE))
  if (inherits(drv, "try-error") || is.null(drv)) return(FALSE)
  if (!is.data.frame(drv) || !"name" %in% names(drv)) return(FALSE)
  any(tolower(drv$name) %in% "cog")
}

.local_aeqd <- function(lon, lat) {
  stopifnot(is.numeric(lon), is.numeric(lat), lon >= -180, lon <= 180, lat >= -90, lat <= 90)
  sprintf("+proj=aeqd +lat_0=%f +lon_0=%f +datum=WGS84 +units=m +no_defs", lat, lon)
}

.square_around_centroid_aeqd <- function(lon, lat, buffer_m) {
  stopifnot(is.numeric(buffer_m), buffer_m > 0)
  aeqd <- .local_aeqd(lon, lat)
  half <- buffer_m
  ring <- rbind(
    c(-half, -half), c( half, -half),
    c( half,  half), c(-half,  half),
    c(-half, -half)
  )
  poly <- sf::st_sfc(sf::st_polygon(list(ring)), crs = aeqd)
  attr(poly, "aeqd_crs") <- aeqd
  poly
}

# Build AOI polygon (sfc) from either bbox or centroid+buffer; AOI CRS can be any valid CRS
.build_aoi <- function(bbox = NULL, centroid = NULL, buffer_m = NULL, aoi_crs = NULL) {
  have_bbox <- !is.null(bbox)
  have_centroid <- !is.null(centroid) && !is.null(buffer_m)
  if (!have_bbox && !have_centroid) stop("Provide either `bbox` or (`centroid` + `buffer_m`).")

  if (have_bbox) {
    if (inherits(bbox, "sfc") || inherits(bbox, "sf")) {
      if (is.na(st_crs(bbox))) stop("AOI has no CRS; supply `aoi_crs`.")
      g <- if (inherits(bbox, "sf")) sf::st_geometry(bbox) else bbox
      return(list(type = "bbox", aoi_sfc = sf::st_as_sfc(st_bbox(g))))
    } else if (inherits(bbox, "bbox")) {
      s <- sf::st_as_sfc(bbox); if (is.na(st_crs(s))) stop("`bbox` has missing CRS; supply `aoi_crs`.")
      return(list(type = "bbox", aoi_sfc = s))
    } else {
      stopifnot(is.numeric(bbox), length(bbox) == 4)
      if (is.null(aoi_crs)) stop("`aoi_crs` is required when `bbox` is numeric.")
      s <- sf::st_as_sfc(st_bbox(c(xmin=bbox[1], ymin=bbox[2], xmax=bbox[3], ymax=bbox[4]),
                             crs = st_crs(aoi_crs)))
      return(list(type = "bbox", aoi_sfc = s))
    }
  }

  # centroid path
  if (have_centroid) {
    if (inherits(centroid, "sfc") || inherits(centroid, "sf")) {
      if (is.na(st_crs(centroid))) stop("Centroid has no CRS; supply `aoi_crs`.")
      g <- if (inherits(centroid, "sf")) sf::st_geometry(centroid) else centroid
      stopifnot(all(st_geometry_type(g, by_geometry = TRUE) %in% c("POINT","MULTIPOINT")))
      wgs <- st_transform(g, 4326)
      xy <- st_coordinates(wgs)[1, ]
      return(list(type = "centroid", centroid_lonlat = c(xy["X"], xy["Y"]), buffer_m = buffer_m))
    } else {
      stopifnot(is.numeric(centroid), length(centroid) == 2)
      if (is.null(aoi_crs)) stop("`aoi_crs` is required when `centroid` is numeric.")
      pt <- sf::st_sfc(sf::st_point(centroid), crs = st_crs(aoi_crs))
      wgs <- st_transform(pt, 4326)
      xy <- st_coordinates(wgs)[1, ]
      return(list(type = "centroid", centroid_lonlat = c(xy["X"], xy["Y"]), buffer_m = buffer_m))
    }
  }

  stop("Invalid AOI specification.")
}

# ---- STAC: robust fetch (no open connections), honors ignore_ssl ----
.stac_assets <- function(
    stac_url = "https://datacube.services.geo.ca/stac/api/",
    collection_id = "mrdem-30",
    item_id = "mrdem",
    ignore_ssl = FALSE
) {
  item_endpoint <- paste0(stac_url, "collections/", collection_id, "/items/", item_id)
  j <- tryCatch({
    if (ignore_ssl) {
      if (!requireNamespace("curl", quietly = TRUE))
        stop("Package 'curl' is required when ignore_ssl=TRUE. install.packages('curl')")
      h <- curl::new_handle(ssl_verifypeer = 0L, ssl_verifyhost = 0L)
      res <- curl::curl_fetch_memory(item_endpoint, handle = h)
      jsonlite::fromJSON(rawToChar(res$content))
    } else {
      jsonlite::fromJSON(item_endpoint)
    }
  }, error = function(e) list(assets = NULL))
  if (is.null(j$assets)) return(list())
  hrefs <- vapply(j$assets, function(a) a$href %||% NA_character_, character(1))
  names(hrefs) <- names(j$assets)
  hrefs
}

# ---- Pick assets; tolerant of naming/driver variants ----
.pick_mrdem_assets <- function(hrefs) {
  if (length(hrefs) == 0) return(list(dtm=NA_character_, slope=NA_character_, aspect=NA_character_, hillshade=NA_character_))
  nm <- tolower(names(hrefs))
  pick <- function(patterns) {
    i <- which(Reduce(`|`, lapply(patterns, function(p) grepl(p, nm, fixed = TRUE))))
    if (length(i)) hrefs[[i[1]]] else NA_character_
  }
  list(
    # prefer a COG ".tif" for DTM if present; else VRT
    dtm       = pick(c("dtm.tif","dtm-vrt","-dtm")),
    hillshade = pick(c("hillshade-dtm","dtm-hillshade"))
  )
}

# ---- Safe remote open with auto .tif→.vrt fallback and known-good fallbacks ----
.safe_rast_open <- function(href, fallback_href = NULL) {
  if (is.null(href) || is.na(href) || !nzchar(href)) {
    if (!is.null(fallback_href)) {
      return(tryCatch(terra::rast(fallback_href), error = function(e) NULL))
    }
    return(NULL)
  }
  r <- suppressWarnings(try(terra::rast(href), silent = TRUE))
  if (!inherits(r, "try-error")) return(r)

  # try auto-swap .tif -> .vrt
  if (grepl("\\.tif(f)?$", href, ignore.case = TRUE)) {
    href2 <- sub("\\.tif(f)?$", ".vrt", href, ignore.case = TRUE)
    r2 <- suppressWarnings(try(terra::rast(href2), silent = TRUE))
    if (!inherits(r2, "try-error")) return(r2)
  }

  # last-resort fallback
  if (!is.null(fallback_href)) {
    r3 <- suppressWarnings(try(terra::rast(fallback_href), silent = TRUE))
    if (!inherits(r3, "try-error")) return(r3)
  }
  NULL
}

# ---- Local derivatives from a single-band DEM ----
.build_derivatives_local <- function(r, which = c("slope","aspect","hillshade","roughness"),
                                     hs_azimuth = 315, hs_altitude = 45, slope_unit = "degrees") {
  out <- list()
  which <- intersect(which, c("slope","aspect","hillshade","roughness"))
  if ("slope" %in% which)   out$slope <- terra::terrain(r, v = "slope", unit = slope_unit)
  if ("aspect" %in% which)  out$aspect <- terra::terrain(r, v = "aspect", unit = "degrees")
  if ("hillshade" %in% which) {
    slp <- out$slope %||% terra::terrain(r, v = "slope", unit = "degrees")
    asp <- out$aspect %||% terra::terrain(r, v = "aspect", unit = "degrees")
    out$hillshade <- terra::shade(slope = slp, aspect = asp, angle = hs_altitude, direction = hs_azimuth)
  }
  if ("roughness" %in% which) out$roughness <- terra::terrain(r, v = "roughness")
  out
}

#' MRDEM Subset Stac
#' -----------------------------------------------------------------------------
#' MRDEM-30 subset & derivatives from the Government of Canada Data Cube.
#' Robust to occasionally-missing .tif assets (auto-fallback to .vrt & local compute).
#' Subset MRDEM-30 DTM by bbox or centroid in any CRS; optionally fetch slope/aspect/hillshade
#' from STAC or compute locally; write COGs w/ overviews; optional aligned & exact outputs.
mrdem_subset_stac <- function(
#'
#'
#' @param bbox Bounding box for the area of interest. _Default=NULL_
#' @param centroid If no bounding box is provided, a centroid can be used. _Default=NULL_
#' @param buffer_m Distance in meters to buffer the centroid by. _Default=NULL_
#' @param aoi_crs The coordinate reference system (Projection) of the area of interest. _Default=NULL_
#' @param target_res_m The target resolution in meters. _Default=100_
#' @param target_crs The target coordinate reference system. (Projection) _Default=NULL_
#' @param stac_url The location of the data. _Default="https://datacube.services.geo.ca/stac/api/"_
#' @param dtm_href The tag for digital terrain model. _Default=NULL_
#' @param ignore_ssl A flag to allow a download without ssl (this should only be used for testing, fix your network, don't step around it.) _Default=FALSE_
#' @param use_vsimem Do the work in memory. _Default=FALSE_
#'
#' @importFrom terra gdal rast terrain shade setGDALconfig crs writeRaster mask project vect ext res crop
#' @importFrom sf st_bbox st_crs gdal_utils st_geometry st_as_sfc st_polygon
#' @importFrom jsonlite fromJSON
#'
#' @return SpatRast
#' @export mrdem_subset_stac
#'

    output_base,
    bbox = NULL,
    centroid = NULL,
    aoi_crs = NULL,
    buffer_m = NULL,
    return_versions = c("exact_aeqd","aligned_source"),
    derivatives = c("slope","aspect"),
    prefer_remote_derivatives = FALSE,
    dtm_href = NULL,
    stac_url = "https://datacube.services.geo.ca/stac/api/",
    target_res_m = 100,
    resampling = "bilinear",
    write_cog = TRUE,
    cog_options = list(
      gdal = c(
        "COMPRESS=LZW",
        "BLOCKSIZE=512",
        "OVERVIEWS=AUTO",
        "RESAMPLING=BILINEAR",
        "NUM_THREADS=ALL_CPUS",
        "BIGTIFF=IF_SAFER"
      )
    ),
    overwrite = TRUE,
    ignore_ssl = FALSE
) {
  stopifnot(is.character(output_base), length(output_base) == 1)
  dir.create(dirname(output_base), recursive = TRUE, showWarnings = FALSE)

  # --- SSL toggle scoped to this call ---
  old_env <- Sys.getenv(c("CPL_CURL_VERIFY_SSL","GDAL_HTTP_UNSAFESSL"), unset = "")
  on.exit({
    Sys.setenv(CPL_CURL_VERIFY_SSL = old_env[[1]], GDAL_HTTP_UNSAFESSL = old_env[[2]])
    if ("setGDALconfig" %in% getNamespaceExports("terra")) {
      terra::setGDALconfig(c("CPL_CURL_VERIFY_SSL","GDAL_HTTP_UNSAFESSL"), c("", ""))
    }
  }, add = TRUE)
  if (isTRUE(ignore_ssl)) {
    Sys.setenv(CPL_CURL_VERIFY_SSL = "NO", GDAL_HTTP_UNSAFESSL = "YES")
    if ("setGDALconfig" %in% getNamespaceExports("terra")) {
      terra::setGDALconfig(c("CPL_CURL_VERIFY_SSL=NO","GDAL_HTTP_UNSAFESSL=YES"))
    }
  }

  # AOI
  roi <- .build_aoi(bbox = bbox, centroid = centroid, buffer_m = buffer_m, aoi_crs = aoi_crs)
  want_exact   <- "exact_aeqd" %in% return_versions
  want_aligned <- "aligned_source" %in% return_versions
  if (!(want_exact || want_aligned)) stop("`return_versions` must include 'exact_aeqd' and/or 'aligned_source'.")

  # STAC discovery (safe; honors ignore_ssl)
  hrefs <- .stac_assets(stac_url = stac_url, ignore_ssl = ignore_ssl)
  picks <- .pick_mrdem_assets(hrefs)

  # DTM URL (fallback to official VRT if needed)
  dtm_url <- dtm_href %||% picks$dtm %||% .MRDEM_DTM_VRT
  r_src <- .safe_rast_open(dtm_url, fallback_href = .MRDEM_DTM_VRT)
  if (is.null(r_src)) stop("Unable to open MRDEM DTM from STAC or fallback VRT.")
  src_crs <- terra::crs(r_src, proj = TRUE)

  # Driver choice + fault-tolerant write
  use_cog <- isTRUE(write_cog) && .gdal_supports_cog()
  primary_driver  <- if (use_cog) "COG" else "GTiff"
  gdal_opts <- cog_options$gdal %||% character(0)
  .write_raster_with_cog <- function(r, path) {
    ok <- TRUE
    tryCatch({
      terra::writeRaster(r, filename = path, overwrite = overwrite, filetype = primary_driver, gdal = gdal_opts)
    }, error = function(e) { ok <<- FALSE })
    if (!ok && primary_driver == "COG") {
      # fallback to GeoTIFF without overviews flags that COG driver expects
      terra::writeRaster(r, filename = path, overwrite = overwrite, filetype = "GTiff",
                         gdal = c("COMPRESS=LZW","PREDICTOR=2","TILED=YES","BIGTIFF=IF_SAFER"))
      return("GTiff")
    }
    primary_driver
  }

  results <- list()

  # Build AOIs in needed CRSs
  if (roi$type == "centroid") {
    lon <- roi$centroid_lonlat[1]; lat <- roi$centroid_lonlat[2]
    aoi_aeqd <- .square_around_centroid_aeqd(lon, lat, roi$buffer_m)  # exact square in meters
    aoi_src  <- st_transform(aoi_aeqd, src_crs)
  } else {
    bb_wgs <- st_transform(roi$aoi_sfc, 4326)
    bb <- st_bbox(bb_wgs)
    lon_ctr <- (bb["xmin"] + bb["xmax"]) / 2
    lat_ctr <- (bb["ymin"] + bb["ymax"]) / 2
    aeqd_crs <- .local_aeqd(lon_ctr, lat_ctr)
    aoi_aeqd <- st_transform(roi$aoi_sfc, aeqd_crs)
    aoi_src  <- st_transform(roi$aoi_sfc, src_crs)
  }

  # ---- Exact AEQD version ----
  if (want_exact) {
    bb_aeqd <- st_bbox(aoi_aeqd)
    tmpl <- terra::rast(xmin = bb_aeqd["xmin"], xmax = bb_aeqd["xmax"],
                        ymin = bb_aeqd["ymin"], ymax = bb_aeqd["ymax"],
                        crs  = st_crs(aoi_aeqd)$wkt, resolution = target_res_m)
    r_exact <- terra::project(r_src, tmpl, method = resampling)
    r_exact <- terra::mask(r_exact, terra::vect(aoi_aeqd), touches = TRUE)

    # Remote derivatives (tolerant): project & mask if open succeeds
    remote_exact <- list()
    if (prefer_remote_derivatives) {
      # Slope
      rs <- .safe_rast_open(picks$slope)
      if (!is.null(rs)) {
        rs2 <- suppressWarnings(try(terra::project(rs, tmpl, method = resampling), silent = TRUE))
        if (!inherits(rs2, "try-error")) remote_exact$slope <- terra::mask(rs2, terra::vect(aoi_aeqd), touches = TRUE)
      }
      # Aspect (use 'near' to avoid artefacts)
      ra <- .safe_rast_open(picks$aspect)
      if (!is.null(ra)) {
        ra2 <- suppressWarnings(try(terra::project(ra, tmpl, method = "near"), silent = TRUE))
        if (!inherits(ra2, "try-error")) remote_exact$aspect <- terra::mask(ra2, terra::vect(aoi_aeqd), touches = TRUE)
      }
      # Hillshade (many catalogs publish VRT only; use known-good fallback if needed)
      rh <- .safe_rast_open(picks$hillshade, fallback_href = .MRDEM_DTM_HILLSHADE)
      if (!is.null(rh)) {
        rh2 <- suppressWarnings(try(terra::project(rh, tmpl, method = resampling), silent = TRUE))
        if (!inherits(rh2, "try-error")) remote_exact$hillshade <- terra::mask(rh2, terra::vect(aoi_aeqd), touches = TRUE)
      }
    }

    # Write bundle (computes any missing derivs locally)
    drv <- .write_raster_with_cog(r_exact, paste0(output_base, "_exact.tif"))
    derivs <- list()
    need <- intersect(c("slope","aspect","hillshade","roughness"), derivatives)
    for (nm in need) {
      if (!nm %in% names(remote_exact)) {
        # compute locally if not fetched
        loc <- .build_derivatives_local(r_exact, which = nm)
        rr  <- loc[[nm]]
      } else {
        rr <- remote_exact[[nm]]
      }
      .write_raster_with_cog(rr, paste0(output_base, "_exact_", nm, ".tif"))
      derivs[[nm]] <- rr
    }

    results$exact_aeqd <- list(
      raster = r_exact,
      derivatives = derivs,
      meta = list(
        extent = terra::ext(r_exact),
        crs    = terra::crs(r_exact, proj = TRUE),
        res    = terra::res(r_exact),
        ncol   = ncol(r_exact),
        nrow   = nrow(r_exact),
        ncell  = terra::ncell(r_exact),
        driver = drv,
        path   = paste0(output_base, "_exact.tif")
      )
    )
  }

  # ---- Aligned-to-source (pixel-aligned crop) ----
  if (want_aligned) {
    aoi_vect_src <- terra::vect(aoi_src)
    r_aligned <- terra::crop(r_src, aoi_vect_src, snap = "out")

    remote_aligned <- list()
    if (prefer_remote_derivatives) {
      rs <- .safe_rast_open(picks$slope)
      if (!is.null(rs)) remote_aligned$slope <- suppressWarnings(try(terra::crop(rs, aoi_vect_src, snap = "out"), silent = TRUE))
      ra <- .safe_rast_open(picks$aspect)
      if (!is.null(ra)) remote_aligned$aspect <- suppressWarnings(try(terra::crop(ra, aoi_vect_src, snap = "out"), silent = TRUE))
      rh <- .safe_rast_open(picks$hillshade, fallback_href = .MRDEM_DTM_HILLSHADE)
      if (!is.null(rh)) remote_aligned$hillshade <- suppressWarnings(try(terra::crop(rh, aoi_vect_src, snap = "out"), silent = TRUE))
    }

    drv <- .write_raster_with_cog(r_aligned, paste0(output_base, "_aligned.tif"))
    derivs <- list()
    need <- intersect(c("slope","aspect","hillshade","roughness"), derivatives)
    for (nm in need) {
      if (!is.null(remote_aligned[[nm]]) && !inherits(remote_aligned[[nm]], "try-error")) {
        rr <- remote_aligned[[nm]]
      } else {
        loc <- .build_derivatives_local(r_aligned, which = nm)
        rr  <- loc[[nm]]
      }
      .write_raster_with_cog(rr, paste0(output_base, "_aligned_", nm, ".tif"))
      derivs[[nm]] <- rr
    }

    results$aligned_source <- list(
      raster = r_aligned,
      derivatives = derivs,
      meta = list(
        extent = terra::ext(r_aligned),
        crs    = terra::crs(r_aligned, proj = TRUE),
        res    = terra::res(r_aligned),
        ncol   = ncol(r_aligned),
        nrow   = nrow(r_aligned),
        ncell  = terra::ncell(r_aligned),
        driver = drv,
        path   = paste0(output_base, "_aligned.tif")
      )
    )
  }

  invisible(results)
}

#' MRDEM Windowed
#' -----------------------------------------------------------------------------
#' MRDEM-30 subset & derivatives from the Government of Canada Data Cube.
#' Robust to occasionally-missing .tif assets (auto-fallback to .vrt & local compute).
#' -----------------------------------------------------------------------------
#'
#'
#' @param bbox Bounding box for the area of interest. _Default=NULL_
#' @param centroid If no bounding box is provided, a centroid can be used. _Default=NULL_
#' @param buffer_m Distance in meters to buffer the centroid by. _Default=NULL_
#' @param aoi_crs The coordinate reference system (Projection) of the area of interest. _Default=NULL_
#' @param target_res_m The target resolution in meters. _Default=100_
#' @param target_crs The target coordinate reference system. (Projection) _Default=NULL_
#' @param stac_url The location of the data. _Default="https://datacube.services.geo.ca/stac/api/"_
#' @param dtm_href The tag for digital terrain model. _Default=NULL_
#' @param ignore_ssl A flag to allow a download without ssl (this should only be used for testing, fix your network, don't step around it.) _Default=FALSE_
#' @param use_vsimem Do the work in memory. _Default=FALSE_
#'
#' @importFrom terra gdal rast terrain shade setGDALconfig crs writeRaster mask project vect ext res crop
#' @importFrom sf st_bbox st_crs gdal_utils st_geometry st_as_sfc st_polygon
#' @importFrom jsonlite fromJSON
#'
#' @return SpatRast
#' @export mrdem_subset_windowed
#' @export
#'

mrdem_subset_windowed <- function(
    bbox = NULL,
    centroid = NULL,
    buffer_m = NULL,
    aoi_crs = NULL,
    target_res_m = 100,
    target_crs = NULL,
    stac_url = "https://datacube.services.geo.ca/stac/api/",
    dtm_href = NULL,
    ignore_ssl = FALSE,
    use_vsimem = F
) {
  # 1) AOI build
  roi <- .build_aoi(bbox = bbox, centroid = centroid, buffer_m = buffer_m, aoi_crs = aoi_crs)

  # 2) Open source DTM
  hrefs <- .stac_assets(stac_url = stac_url, ignore_ssl = ignore_ssl)
  picks <- .pick_mrdem_assets(hrefs)
  dtm_url <- dtm_href %||% picks$dtm %||% .MRDEM_DTM_VRT
  r_src <- .safe_rast_open(dtm_url, fallback_href = .MRDEM_DTM_VRT)
  if (is.null(r_src)) stop("Unable to open MRDEM DTM from STAC or fallback VRT.")
  src_wkt <- terra::crs(r_src, proj = TRUE)

  # 3) AOI in source CRS
  aoi_src <- if (roi$type == "centroid") {
    lon <- roi$centroid_lonlat[1]; lat <- roi$centroid_lonlat[2]
    st_transform(.square_around_centroid_aeqd(lon, lat, roi$buffer_m), src_wkt)
  } else {
    st_transform(roi$aoi_sfc, src_wkt)
  }
  aoi_src <- st_buffer(aoi_src,dist=10000)
  bb_src <- round(sf::st_bbox(aoi_src),-2)
  aoi_src <- st_crop(aoi_src,
                     bb_src)

  # 4) Pre-cut window in source CRS using gdal_translate (to VRT)
  dest <- if (isTRUE(use_vsimem)) "/vsimem/mrdem_sub.vrt" else tempfile(fileext = ".vrt")
  # sf::gdal_utils wraps GDAL; faster windowed reads for VRT/COG
  sf::gdal_utils(
    util = "translate",
    source = dtm_url,
    destination = dest,
    options = c(
      "-of", "VRT",
      "-projwin_srs", src_wkt,
      "-projwin", bb_src["xmin"], bb_src["ymax"], bb_src["xmax"], bb_src["ymin"]
    )
  )
  r_win <- terra::rast(dest) # small windowed subset in source CRS

  # 5) Build target template (extent = AOI transformed to target CRS)

  aoi_tgt <- st_transform(aoi_src, target_crs)
  bb_tgt <- round(sf::st_bbox(aoi_tgt),-2)
  aoi_tgt <- st_crop(aoi_tgt, bb_tgt,snap="out")
  tmpl <- terra::rast(
    xmin = bb_tgt["xmin"],
    xmax = bb_tgt["xmax"],
    ymin = bb_tgt["ymin"],
    ymax = bb_tgt["ymax"],
    crs  = sf::st_crs(aoi_tgt)$wkt,
    resolution = target_res_m
  )

  # 6) Reproject only the pre-cut window; then mask to AOI
  r_out <- terra::project(r_win, tmpl, method = "bilinear")
  r_out <- terra::crop(r_out, terra::vect(st_buffer(aoi_tgt,dist=-10000)), touches = TRUE)

  return(r_out)
}
