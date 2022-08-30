# *** create_speciesvar ---------
#' @title Create species variable
#'
#' @description Modify species common names to reader-friendly format and combine with population name to get ESU or DPS unit name
#'
#' @details This function first re-orders the CommonName field exported from APPS, then concatenates it with the Population
#'
#' @export

# ***** combines Population and CommonName variables to create the Species variable
create_speciesvar <- function(df){
  df <- df %>%
    rename_commonname() %>%
    rename_population() %>%
    mutate(Species = paste(Population, CommonName, sep = " "))
  return(df)
}
