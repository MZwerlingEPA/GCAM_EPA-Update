% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/xml.R
\name{add_logit_tables_xml_generate_levels}
\alias{add_logit_tables_xml_generate_levels}
\title{Add a table to convert to XML but generate additional levels of XML nesting
providing a special case for logit tables.}
\usage{
add_logit_tables_xml_generate_levels(dot, data, header, old_tag, new_tag,
  num_levels, rename_final, column_name = old_tag,
  column_order_lookup = header, base_logit_header = header)
}
\arguments{
\item{dot}{The current state of the pipeline started from \code{create_xml}}

\item{data}{The tibble of data to add to the conversion}

\item{header}{The base header tag to can be looked up in the header file to
convert \code{data} besides the additional levels of nesting.}

\item{old_tag}{The XML tag which is being expanded to add more levels}

\item{new_tag}{The XML tag which will serve as the additional levels}

\item{num_levels}{The number of additional levels to generate}

\item{rename_final}{Whether to rename the final \code{old_tag} to
\code{new_tag} in the original header}

\item{column_name}{The base name of the column in \code{data} which will get
expanded by appending \code{paste0(column_name, (seq_len(num_levels) - 1))}}

\item{column_order_lookup}{A tag that can be used to look up \code{LEVEL2_DATA_NAMES}
to reorder the columns of data before XML conversion to ensure they correspond
with the ModelInterface header.  Note by default the \code{header} is used then we
append the additional columns generated by \code{column_name} + \code{num_levels}. If
given \code{NULL} no column reordering will be done.}

\item{base_logit_header}{The base header tag to use for the generated logit type
tables such that \code{paste(base_logit_header, logit.type, sep = "_")} corresponds
to the appropriate model interface header.  Note by default this value is the
same as \code{header} as often this is the case but certainly not always.}
}
\value{
A "data structure" to hold the various parts needed to run the model
interface CSV to XML conversion.
}
\description{
This is basically a combination of \code{add_xml_data_generate_levels} and
\code{add_logit_tables_xml} check the documentation for each to understand
the individual behaviors.
}
\author{
Pralit Patel
}
