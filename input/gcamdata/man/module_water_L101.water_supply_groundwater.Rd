% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/zwater_L101.water_supply_groundwater.R
\name{module_water_L101.water_supply_groundwater}
\alias{module_water_L101.water_supply_groundwater}
\title{module_water_L101.water_supply_groundwater}
\usage{
module_water_L101.water_supply_groundwater(command, ...)
}
\arguments{
\item{command}{API command to execute}

\item{...}{other optional parameters, depending on command}
}
\value{
Depends on \code{command}: either a vector of required inputs,
a vector of output names, or (if \code{command} is "MAKE") all
the generated outputs: \code{L101.DepRsrcCurves_ground_uniform_bm3},
\code{L101.groundwater_grades_constrained_bm3}, \code{L101.groundwater_depletion_bm3}. The corresponding file in the
original data system was \code{L100.water_supply_runoff.R} (Water level1).
}
\description{
Prepare GCAM basin groundwater supply curves
}
\details{
Prepares groundwater resource curves and sets up groundwater calibration data.
}
\author{
ST September 2018
}
