# *** create_totalactmorts ---------
#' @title Create Total Actual Morts Column
#'
#' @description Create a new column summing the total actual/reported mortalities for each take line
#'
#' @details The total number of actual mortalities is the sum of reported uninentional lethal takes plus the sum of reported intentional lethal takes. This function identifies whether the Take Action is Intentional/Directed Mortality, and counts reported takes as mortalities in those cases.
#'
#' @export

create_totalactmorts <- function(df){
  df <- df %>%
    mutate(TotActMorts = ifelse(
      TakeAction=='Intentional (Directed) Mortality',
      ActTake + ActMort, ActMort))
  return(df)
}
