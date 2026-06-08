# BurnP3.HelpR 0.11.1

-`spread_event_days`
- Updated documentation for the `threshold` argument to help clarify what that actually does.

# BurnP3.HelpR 0.11.0

-`spread_event_days`
- Fixed the redistribution above the threshold to proportionally redistribute across the available spread event days.
- Added a final adjustment to add in any extra remainder evenly as the proportional redistribution does not achieve 100 all the time.
- Can now assign the output to an object, it was not being returned previously

# BurnP3.HelpR 0.10.9

# BurnP3.HelpR 0.10.8

-`grid_grab`
- Adjusted the methodology, it now uses the initial AOI and converts into the desired CRS, this is to provide an aesthetically pleasing square output that is aligned. At worst the fuels will have clipped edges, but the elevation will be complete.

# BurnP3.HelpR 0.10.7

-`grid_grab`
- Improved the Fuel NA detection to ensure NAs that do exist don't trigger the warning to use the WCS.

# BurnP3.HelpR 0.10.6

-`grid_grab`
- Added EPSG code to the end of the file so we know what projection is being used.


# BurnP3.HelpR 0.10.5

-`grid_grab`
- Added ref_is_fuel to allow the user to define the reference grid as the fuel grid.

# BurnP3.HelpR 0.10.4

-`grid_grab`
- Has a fuel argument to allow the user to define if they want a fuel grid pull

# BurnP3.HelpR 0.10.3

- package level definition by function

# BurnP3.HelpR 0.10.2

- reduce redundant requests (fuels)

# BurnP3.HelpR 0.10.1

# BurnP3.HelpR 0.10.0

---

## Enhancements

- `grid_grab`
- Now uses the mrdem data and collects them via the vrt

# BurnP3.HelpR 0.9.0

# version 0.9.0

---

## Enhancments

- `grid_grab` can now be used to acquire fuels and elevation
- Added documentation for datasets


## Fixes

- Fixed the examples so they actually work
- Deprecated `elev_grab` -> `grid_grab`
