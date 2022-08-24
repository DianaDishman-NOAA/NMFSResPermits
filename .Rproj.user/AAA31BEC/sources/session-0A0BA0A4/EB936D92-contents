# *** consolidate_lifestage ---------
#' @title Consolidate LifeStage
#'
#' @description Consolidate detailed life stage categories into generalized juvenile and adult categories
#'
#' @details Life stage categories of smolt, fry, yearling, sub-yearling and parr are recoded as "juveniles" for summary analyses. Jacks and subadult salmon and steelhead are recoded as "adults", though their reproductive value to the species is expected to be less than fully mature adults. Kelt steelhead are not recoded as adults, but left separate. The egg lifestage is removed for salmon and steelhead but retained for sturgeon, rockfish, and eulachon, as is the subadult category.
#'
#' @export

# ***** takes a dataframe that already has the LifeStage variable and updates the wording
# ***** for LifeStagen based on whatever equalities are listed in the recode() portion.
# ***** the ifelse statement makes it so that changes to Adults are only done on salmonids
consolidate_lifestage <- function(df){
  df <- df %>%
    mutate(LifeStage = recode(LifeStage,
                              "Smolt" = "Juvenile",
                              "Fry" = "Juvenile",
                              "Yearling" = "Juvenile",
                              "Sub-Yearling" = "Juvenile",
                              "Parr" = "Juvenile")) %>%
    mutate(LifeStage = ifelse(grepl(c("salmon|steelhead"), Species),
                              recode(LifeStage,
                                     "Subadult" = "Adult",
                                     "Jack" = "Adult"),
                              LifeStage)) %>%
    filter(!(grepl(c("salmon|steelhead"), Species)==TRUE & LifeStage == "Egg"))
  return(df)
}
