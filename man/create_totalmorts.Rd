% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_totalmorts.R
\name{create_totalmorts}
\alias{create_totalmorts}
\title{Create TotalMorts Column}
\usage{
create_totalmorts(df)
}
\arguments{
\item{df}{A dataframe of imported APPS data including the TakeAction, ExpTake, and IndMort columns}
}
\description{
Create a new column summing the total mortalities for each take line
}
\details{
The total number of authorized mortalities is actually the sum of uninentional lethal takes plus the sum of intentional lethal takes. This function identifies whether the Take Action is Intentional/Directed Mortality, and counts authorized takes as mortalities in those cases.
}
