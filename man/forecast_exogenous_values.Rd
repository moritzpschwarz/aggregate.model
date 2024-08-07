% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/forecast_exogenous_values.R
\name{forecast_exogenous_values}
\alias{forecast_exogenous_values}
\title{Internal function to forecast exogenous variables}
\usage{
forecast_exogenous_values(
  model,
  exog_vars,
  exog_predictions,
  exog_fill_method,
  ar.fill.max,
  n.ahead,
  quiet
)
}
\arguments{
\item{model}{A model object of class 'osem'.}

\item{exog_vars}{The set of exogenous variables to be forecasted.}

\item{exog_predictions}{A data.frame or tibble with values for the exogenous values. The number of rows of this data must be equal to n.ahead.}

\item{exog_fill_method}{Character, either 'AR', 'auto', or 'last'. When no exogenous values have been provided, these must be inferred. When option 'exog_fill_method = "AR"' then an autoregressive model is used to further forecast the exogenous values. With 'last', simply the last available value is used. 'auto' is an \code{\link[forecast]{auto.arima}} model.}

\item{ar.fill.max}{Integer. When no exogenous values have been provided, these must be inferred. If option 'exog_fill_method = "AR"' then an autoregressive model is used to further forecast the exogenous values. This options determines the number of AR terms that should be used. Default is 4.}

\item{n.ahead}{Periods to forecast ahead}

\item{quiet}{Logical. Should messages about the forecast procedure be suppressed?}
}
\value{
A dataset containing the set of forecasted exogenous values.
}
\description{
Internal function to forecast exogenous variables
}
