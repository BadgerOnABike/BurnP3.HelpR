%% TODO: [WORK-12] Add docs for igntion grid function
\name{ign.grid}
\alias{Ignition Grids}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Ignition Grid
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
This function will generate an ignition grid for a given area describing the potential igntion by cause and by season with a variety of models. Random Forest 'rf', Gradient Boosted Machine 'gbm' and a non-optimized Random Forest 'rf_old' may be selected.
}
\usage{
ign.grid(fire_data, indicator_stack, hum_indicators, ltg_indicators, causes, season_description, output_location, min_fire_size = "", model = "rf")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fire_data}{Spatial DataFrame containing wildfire information with Cause and Season information.}
  \item{indicator_stack}{A raster stack containing all predictor variables. A presence absence grid will be generated within the function and should not be a part of the indicator stack.}
  \item{indicators_1}{A vector of indicators that should be used when the cause is the first cause.}
  \item{indicators_2}{A vector of indicators that should be used when the cause is the second cause.}
  \item{causes}{A vector of up to 2 causes. Typically Human and Lightning.}
  \item{season_description}{A vector of season descriptions that is in the same order as the numerical seasons in the fire_data.}
  \item{output_location}{A vector describing the location that ignition grids should be written to. In the future the BP3_Directory_Generator will define this internally.}
  \item{min_fire_size = ""}{A numerical value defining the minimum fire size that should be included in the ignition gridding process.}
  \item{model = "rf"}{A character value defining one of "rf","gbm",or "rf_old". By default an auto-tuning Random Forest model is selected.}
%%     ~~Describe \code{x} here~~
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
Brett Moore
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (x) 
{
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
