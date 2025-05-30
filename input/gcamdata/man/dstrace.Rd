% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dstrace.R
\name{dstrace}
\alias{dstrace}
\title{A tracing utility for the GCAM Data System.}
\usage{
dstrace(object_name, direction = "upstream", graph = FALSE,
  gcam_data_map = NULL, previous_tracelist = NULL, recurse = TRUE,
  print = TRUE, ...)
}
\arguments{
\item{object_name}{Name of object to trace (can be either a data object or a code chunk)}

\item{direction}{Trace direction ("upstream", the default; "downstream"; or "both"), character}

\item{graph}{Plot a directed graph of relationships? Logical}

\item{gcam_data_map}{A tibble of metadata information; normally a built-in package dataset}

\item{previous_tracelist}{Information about previous objects printed}

\item{recurse}{Recurse to print information about precursor objects? Logical}

\item{print}{Print trace to console? Logical}

\item{...}{Extra arguments passed on to \code{\link{dstrace_plot}}}
}
\value{
A tibble with the trace information (object name and trace number), invisibly
}
\description{
A tracing utility for the GCAM Data System.
}
\details{
What other data products feed into some particular data system object?
Conversely, to what other products does some object feed? These are the kinds
of questions that \code{dstrace} can help answer.
}
\examples{
dstrace("L100.FAO_ag_Exp_t")
dstrace("L100.FAO_ag_Exp_t", direction = "downstream")
dstrace("L100.FAO_ag_Exp_t", direction = "both", graph = TRUE)
}
\author{
BBL
}
