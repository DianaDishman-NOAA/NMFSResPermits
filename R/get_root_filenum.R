# *** get_root_filenum ---------
#' @title Get Root FileNumber
#'
#' @description Extract the root number from any FileNumber exported from APPS
#'
#' @details The portion of a FileNumber to the left of a dash is the 'root' FileNumber tied to the project, while the number and letter to the right of the dash represent the iteration and type of change for each record. Renewals, amendments, and modifications will have sequentially higher numbers and variable letters R, A, or M, but the root file number will be the same. This function eliminates the hyphen and subsequent characters so root FileNumbers can be matched between past, current, and applied for permits.
#'
#' @param file_list A list of permit numbers associated with APPS data, matching the FileNumber column
#'
#' @export


get_root_filenum <- function(file_list){
  return(unique(stringr::str_extract(file_list, "[^- | //s]+")))
}
