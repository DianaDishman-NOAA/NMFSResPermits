# *** abbrev_production ---------
#' @title Abbreviate production
#'
#' @description Abbreviate production descriptions for pretty printing in tables
#'
#' @details Takes a dataframe that already has the Production variable and updates Production values to abbreviations. This should only be used immediately before printing, as changing these names will cause other functions to fail.
#'
#' @param df A dataframe of imported APPS data including the Species and Production columns
#'
#' @export

rename_production <- function(df){
  df <- df %>%
    dplyr::mutate(Production = dplyr::recode(Production,
                               "Listed Hatchery Adipose Clip" = "LHAC",
                               "Listed Hatchery Intact Adipose" = "LHIA",
                               "Listed Hatchery, Clipped and Intact" = "LHAC_LHIA",
                               "Listed Hatchery and Natural Origin" = "LHAC_LHIA_NOR"))
  return(df)
}
