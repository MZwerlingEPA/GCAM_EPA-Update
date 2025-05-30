% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{chunk_outputs}
\alias{chunk_outputs}
\title{chunk_outputs}
\usage{
chunk_outputs(chunks = find_chunks()$name,
  call_flag = driver.DECLARE_OUTPUTS)
}
\arguments{
\item{chunks}{A character vector of chunks names}

\item{call_flag}{Flag to look for in outputs}
}
\value{
A tibble with columns 'name' (chunk name), 'output' (name of data),
and 'to_xml' (whether or not this is an XML structure).
}
\description{
List all chunk outputs.
}
