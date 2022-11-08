# *** create_total_morts ---------
#' @title Create TotalMorts Column
#'
#' @description Create a new column summing the total mortalities for each take line
#'
#' @details The total number of authorized mortalities is actually the sum of uninentional lethal takes plus the sum of intentional lethal takes. This function identifies whether the Take Action is Intentional/Directed Mortality, and counts authorized takes as mortalities in those cases.
#'
#' @param df A dataframe of imported APPS data including the TakeAction, ExpTake, and IndMort columns
#'
#' @export

create_totalmorts <- function(df){
  df <- df %>%
    dplyr::mutate(TotalMorts = ifelse(
      TakeAction=='Intentional (Directed) Mortality',
      ExpTake + IndMort, IndMort))
  return(df)
}
