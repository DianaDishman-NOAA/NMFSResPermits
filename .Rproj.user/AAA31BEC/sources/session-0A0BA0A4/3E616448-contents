# *** pretty_flextable_fx ---------
#' @title Pretty flextable
#'
#' @description Takes a tibble, creates pretty, formatted flextable for export into word docs
#'
#' @details This function only FORMATS the flextable - you must open a docx, add flextables to the body, and then PRINT the docx file to export, although a preview can be printed if desired
#'
#' @export


pretty_flextable <- function(df, ftname){

  if("TakeAction" %in% colnames(df)){
    df <- df %>%
      mutate(TakeAction= recode(TakeAction,
                                "Intentional \\(Directed\\) Mortality" = "IM",
                                "Capture/Handle/Release Fish" = "C/H/R",
                                "Capture/Handle/Release Animal" = "C/H/R",
                                "Capture/Mark, Tag, Sample Tissue/Release Live Animal" = "C/M, T, ST/R",
                                "Observe/Harass" = "O/H",
                                "Observe/Sample Tissue Dead Animal" = "O/ST D",
                                "Collect, Sample, and Transport Live Animal" = "C,S,T"))}

  if("Production" %in% colnames(df)){
    df <- df %>%
      mutate(Production = recode(Production,
                                 "Listed Hatchery Adipose Clip" = "LHAC",
                                 "Listed Hatchery Intact Adipose" = "LHIA",
                                 "Listed Hatchery, Clipped and Intact" = "LHAC & LHIA",
                                 "Listed Hatchery and Natural Origin" = "LHAC, LHIA & NOR"))}

  ft <- flextable(df)

  ft <- autofit(ft) %>%
    font(fontname = "Times", part = "all") %>%
    align(align = "center", part = "header") %>%
    align(align = "center", part = "body") %>%
    align(align = "left", j = 1, part = "all") %>%
    add_header_lines(values = ftname)

  if("Species" %in% colnames(df)){
    ft <- ft %>%
      width(j= "Species", 1) %>%
      width(j= "LifeStage", 0.9) %>%
      width(j= "Production", 0.8) %>%
      border_inner_h(part="all", border = officer::fp_border(color="gray")) %>%
      merge_v(j = c("Species", "LifeStage")) %>%
      set_header_labels(Production = "Origin", LifeStage = "Life Stage")
  }

  if("abundance" %in% colnames(df)){
    ft <- ft %>%
      set_header_labels(abundance = "Abundance") %>%
      colformat_num(j = "abundance", big.mark=",", na_str = "-")
  }

  if("ExpTake" %in% colnames(df)){
    ft <- ft %>%
      width(j= ~ExpTake + TotalMorts, 0.8) %>%
      set_header_labels(ExpTake = "Requested Take", TotalMorts = "Lethal Take") %>%
      colformat_double(j = c("ExpTake", "TotalMorts"), big.mark=",", digits = 0, na_str = "-")
  }

  if("PercESUtake" %in% colnames(df)){
    ft <- ft %>%
      width(j= ~PercESUtake + PercESUkill, 0.9) %>%
      merge_v(j = c("PercESUtake", "PercESUkill")) %>%
      set_header_labels(PercESUtake = "Percent of ESU/DPS taken",
                        PercESUkill = "Percent of ESU/DPS killed") %>%
      colformat_char(j = c("PercESUtake", "PercESUkill"), na_str = "-")
  }

  if("TakeAction" %in% colnames(df)){
    ft <- ft %>%
      width(j= "TakeAction", 1.1) %>%
      set_header_labels(TakeAction = "Take Action")
  }

  if("PriorExpTake" %in% colnames(df)){
    ft <- ft %>%
      width(j= ~PriorExpTake + PriorTotalMorts, 0.8) %>%
      set_header_labels(PriorExpTake = "Prior Total Take", PriorTotalMorts = "Prior Lethal Take") %>%
      colformat_double(j = c("PriorExpTake", "PriorTotalMorts"), big.mark=",", digits = 0, na_str = "-")
  }
  return(ft)
  #print(ft, preview = "docx")                                                                     #print test view if desired
}
