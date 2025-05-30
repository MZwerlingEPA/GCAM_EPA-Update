% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/zemissions_xml_all_unmgd_emissions.R
\name{module_emissions_all_unmgd_emissions_xml}
\alias{module_emissions_all_unmgd_emissions_xml}
\title{module_emissions_all_unmgd_emissions_xml}
\usage{
module_emissions_all_unmgd_emissions_xml(command, ...)
}
\arguments{
\item{command}{API command to execute}

\item{...}{other optional parameters, depending on command}
}
\value{
Depends on \code{command}: either a vector of required inputs,
a vector of output names, or (if \code{command} is "MAKE") all
the generated outputs: \code{all_unmgd_emissions.xml}. The corresponding file in the
original data system was \code{batch_all_unmgd_emissions.xml} (emissions XML).
}
\description{
Construct XML data structure for \code{all_unmgd_emissions.xml}.
}
