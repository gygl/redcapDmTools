% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{reshape_formular}
\alias{reshape_formular}
\title{Reformat formular data from excel sheet}
\usage{
reshape_formular(formular, formular_name)
}
\arguments{
\item{formular}{formular data}

\item{formular_name}{formular name}
}
\value{
formatted formular
}
\description{
rename col according to first row then remove unnecessary rows at the
beginning and at the end according to the landmark hash ("#")
put before the first and after the last lines
}
