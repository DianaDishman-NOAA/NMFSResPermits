# *** rename_commonname ---------
#' @title Rename CommonName
#'
#' @description Modify species common names to reader-friendly format and combine with population name to get ESU or DPS unit name
#'
#' @details takes a dataframe that already has the CommonName variable and updates the wording FROM the left hand side of the ~ TO the right hand side of the ~
#'
#' @param df A dataframe of imported APPS data with an unaltered CommonName column that you want to re-format into print-friendly order
#'


# *** rename_commonname ---------
# ***** takes a dataframe that already has the CommonName variable and updates the
# ***** wording FROM the left hand side of the ~ TO the right hand side of the ~
rename_commonname <- function(df){
  df <- df %>%
    dplyr::mutate(CommonName = dplyr::recode(CommonName,
                               "Salmon, coho" = "coho salmon",
                               "Steelhead" = "steelhead",
                               "Eulachon" = "eulachon",
                               "Salmon, Chinook" = "Chinook salmon",
                               "Salmon, chum" = "chum salmon",
                               "Salmon, sockeye" = "sockeye salmon",
                               "Sturgeon, green" = "green sturgeon",
                               "Rockfish, Canary" = "canary rockfish",
                               "Rockfish, Bocaccio" = "bocaccio",
                               "Rockfish, Yelloweye" = "yelloweye rockfish"))
  return(df)
}
