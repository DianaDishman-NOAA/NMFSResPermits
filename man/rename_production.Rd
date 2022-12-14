% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/abbrev_production.R
\name{rename_production}
\alias{rename_production}
\title{Abbreviate production}
\usage{
rename_production(df)
}
\arguments{
\item{df}{A dataframe of imported APPS data including the Species and Production columns}
}
\description{
Abbreviate production descriptions for pretty printing in tables
}
\details{
Takes a dataframe that already has the Production variable and updates Production values to abbreviations. This should only be used immediately before printing, as changing these names will cause other functions to fail.
}
