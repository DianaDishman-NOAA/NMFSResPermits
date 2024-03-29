#' @title Rename Population
#'
#' @description Rename any population that is not currently labeled as its applicable ESU or DPS
#'
#' @details When originally added to APPS some populations are not listed, such as 10j experimental populations,
#' but later become listed or otherwise need to be considered part of a listed ESU or DPS. The Deschutes River
#' steelhead Nonessential Experimental Population is one such population, which is considered part of the Middle
#' Columbia River steelhead DPS. This function renames those populations so they group with the correct listed unit
#' for calculations.
#'
#' @param df A dataframe of imported APPS data with an unaltered Population column
#'
#' @export
# *** takes a dataframe that already has the Population variable and updates the wording
# *** for Population based on whatever equalities are listed in the recode() portion.
rename_population <- function(df){
  df <- df %>%
    dplyr::mutate(Population = dplyr::recode(Population,
                               "Deschutes River Steelhead NEP" = "Middle Columbia River"))
  return(df)
}

