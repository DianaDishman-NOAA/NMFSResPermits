# *** create_speciesvar ---------
#' @title Create species variable
#'
#' @description Modify species common names to reader-friendly format and combine with population name to get ESU or DPS unit name
#'
#' @details This function first re-orders the CommonName field exported from APPS, then concatenates it with the Population
#'
#' @param df A dataframe of imported APPS data including the CommonName and Population columns
#'
#' @export

# ***** combines Population and CommonName variables to create the Species variable
create_speciesvar <- function(df){
  df <- df %>%
    rename_commonname() %>%
    rename_population() %>%
    dplyr::mutate(Species = paste(Population, CommonName, sep = " "))
  return(df)
}
