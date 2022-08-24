# *** abbrev_takeaction ---------
#' @title Abbreviate Take Action
#'
#' @description Abbreviate take actions to reader-friendly format for tables
#'
#' @details Used only for formatting values for print-friendly tables, but shouldn't be implemented during data management
#'
#' @export


# ***** takes a dataframe that already has the TakeAction variable and updates the wording
# ***** for TakeAction based on whatever equalities are listed in the recode() portion.
abbrev_takeaction <- function(df){
  df <- df %>%
    mutate(TakeAction= recode(TakeAction,
                              "Intentional \\(Directed\\) Mortality" = "IM",
                              "Capture/Handle/Release Fish" = "C/H/R",
                              "Capture/Mark, Tag, Sample Tissue/Release Live Animal" = "C/M, T, ST/R",
                              "Observe/Harass" = "O/H",
                              "Observe/Sample Tissue Dead Animal" = "O/ST D",
                              "Collect, Sample, and Transport Live Animal" = "C,S,T"))
}
