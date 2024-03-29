% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/determine_codes.R
\name{determine_variables}
\alias{determine_variables}
\title{Determine which model variables need to be found}
\usage{
determine_variables(specification, dictionary)
}
\arguments{
\item{specification}{A tibble or data.frame as returned by
\code{\link{check_config_table}}.}

\item{dictionary}{A tibble or data.frame storing the Eurostat variable code
in column 'eurostat_code'. If \code{download == TRUE} then the dictionary
also requires column named 'dataset_id' that stores the Eurostat dataset
id. When \code{NULL}, the \link[=dict]{default dictionary} is used.}
}
\value{
Returns a data.frame that is a subset of the dictionary (required
variables only) and an additional column \code{found}, which is set to
\code{FALSE} initially and keeps track of the data that has been found
through the (down)load functions.
}
\description{
Takes the specification and returns a dictionary-like data frame with the
required model variables, their filters, and where they can be found.
}
