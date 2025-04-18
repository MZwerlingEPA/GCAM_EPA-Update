% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/zenergy_L100.IEA_downscale_ctry.R
\name{module_energy_L100.IEA_downscale_ctry}
\alias{module_energy_L100.IEA_downscale_ctry}
\title{module_energy_L100.IEA_downscale_ctry}
\usage{
module_energy_L100.IEA_downscale_ctry(command, ...)
}
\arguments{
\item{command}{API command to execute}

\item{...}{other optional parameters, depending on command}
}
\value{
Depends on \code{command}: either a vector of required inputs,
a vector of output names, or (if \code{command} is "MAKE") all
the generated outputs: \code{L100.IEA_en_bal_ctry_hist}. The corresponding file in the
original data system was \code{LA100.IEA_downscale_ctry.R} (energy level1).
}
\description{
Downscale proprietary IEA energy balance data to 201 countries, by iso code, FLOW, PRODUCT, and historical year.
}
\details{
Combine OECD and non-OECD data; perform upfront adjustments for other Africa, Turkey, and South
Africa; split out and handle the 1990 split of Yugoslavia and USSR, back-projecting individual country
values based on 1990 shares; use population to downscale IEA composite regions (Other Africa, Other
non-OECD Americas, Other non-OECD Asia) to individual countries; filter out countries without data in any year.
}
\note{
We build from the raw (and proprietary) \code{IEA_EnergyBalances_2019} file if
they are available; if not, this chunk reads in a pre-generated \code{L100.IEA_en_bal_ctry_hist}
file and returns that. (In other words, our output is an optional input.)
}
\author{
BBL May 2017
}
