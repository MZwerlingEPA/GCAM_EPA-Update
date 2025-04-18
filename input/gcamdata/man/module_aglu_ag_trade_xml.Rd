% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/zaglu_xml_ag_trade.R
\name{module_aglu_ag_trade_xml}
\alias{module_aglu_ag_trade_xml}
\title{module_aglu_ag_trade_xml}
\usage{
module_aglu_ag_trade_xml(command, ...)
}
\arguments{
\item{command}{API command to execute}

\item{...}{other optional parameters, depending on command}
}
\value{
Depends on \code{command}: either a vector of required inputs,
a vector of output names, or (if \code{command} is "MAKE") all
the generated outputs: \code{ag_trade.xml}.
}
\description{
Construct XML data structure for \code{ag_trade.xml}.
}
