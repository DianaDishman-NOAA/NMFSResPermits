# *** order_table ----------
#' @title Order Table
#'
#' @description Order a dataframe or table by Species, LifeStage, and Production
#'
#' @details This arrangement is set in a North to South species pattern. Currently set up to create ordered factors out of these columns first, then order all at once.
#'
#' @param df A dataframe of imported APPS data that you want to put in print order, including the Species, LifeStage, and Production columns
#'
#' @export

# ** Species order: actually order a tibble by how we want species to appear in tables
order_table <- function(df){
  df <- df %>%
    dplyr::mutate(Species = factor(Species, sp.order)) %>%
    dplyr::mutate(LifeStage = factor(LifeStage, ls.order)) %>%
    dplyr::mutate(Production = factor(Production, pr.order)) %>%
    dplyr::arrange(Species, LifeStage, Production)
  return(df)
}
