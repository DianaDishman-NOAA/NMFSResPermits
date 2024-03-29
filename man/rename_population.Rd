% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rename_population.R
\name{rename_population}
\alias{rename_population}
\title{Rename Population}
\usage{
rename_population(df)
}
\arguments{
\item{df}{A dataframe of imported APPS data with an unaltered Population column}
}
\description{
Rename any population that is not currently labeled as its applicable ESU or DPS
}
\details{
When originally added to APPS some populations are not listed, such as 10j experimental populations,
but later become listed or otherwise need to be considered part of a listed ESU or DPS. The Deschutes River
steelhead Nonessential Experimental Population is one such population, which is considered part of the Middle
Columbia River steelhead DPS. This function renames those populations so they group with the correct listed unit
for calculations.
}
