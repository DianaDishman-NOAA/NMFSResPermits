# *** perc_ESU_DPS ----------
#' @title Percent of ESU/DPS
#'
#' @description Calculate the percent of the ESU or DPS abundance that will be taken or killed by proposed research
#'
#' @details A dataframe of proposed take is merged with a dataframe of abundance to calculate the percentage of abundance the take summary represents. This can be applied to an individual permit summary or a baseline or program-wide take summary as long as the necessary columns (Species, LifeStage, Production, ExpTake, TotalMorts, abundance) are present
#'
#' @param df A dataframe of imported APPS data that includes including the Species, LifeStage, and Production columns, as well as ExpTake and TotalMorts columns totaling all and lethal-only take (see ?create_totalmorts())
#' @param abund A dataframe of abundance date for all listed species of interest, organized at matching ESU/DPS (Species), LifeStage, and Production (hatchery vs natural-origin) values, as available for each ESU/DPS. Columns should be labeled 'Species', 'LifeStage', 'Production', and 'abundance' with values for the first three that correspond with valid values in the df dataframe
#'
#' @export

# ***** Combining take tables with abundance is tricky because we don't have abundance data
# ***** for all life stages and HOR/NOR components individually

perc_ESU_DPS <- function(df, abund){
  #Bring in ESU/DPS abundance data for percent of ESU/DPS taken and killed columns
  #merge baseline take and abundance tables
  df_abund <- merge(df, abund, by = c("Species", "LifeStage", "Production"), all = TRUE)

  #Calculate take sums as perc of ESU/DPS using adult abundance only
  #For rockfish and eulachon
  df_by_sp_adults <- df_abund %>%
    dplyr::group_by(Species) %>%
    tidyr::replace_na(list(ExpTake = 0, TotalMorts = 0, abundance = 0)) %>%
    dplyr::summarise(ExpTake = sum(ExpTake), TotalMorts = sum(TotalMorts), abundance = sum(abundance)) %>%
    dplyr::mutate(PercESUtake = (ExpTake/abundance)*100) %>%
    dplyr::mutate(PercESUkill = (TotalMorts/abundance)*100)

  df_sp <- df %>%
    dplyr::filter(Species %in% c("Puget Sound/Georgia Basin DPS bocaccio",
                          "Puget Sound/Georgia Basin DPS yelloweye rockfish",
                          "Southern DPS eulachon")) %>%
    dplyr::left_join(select(df_by_sp_adults, c(Species, abundance, PercESUtake, PercESUkill)),
              by = "Species")

  #Calculate take sums as perc of ESU/DPS by species and lifestage
  #For species where you can't distinguish hatchery and natural origin adults
  df_by_sp_ls <- df_abund %>%
    dplyr::group_by(Species, LifeStage) %>%
    tidyr::replace_na(list(ExpTake = 0, TotalMorts = 0, abundance = 0)) %>%
    dplyr::summarise(ExpTake = sum(ExpTake), TotalMorts = sum(TotalMorts), abundance = sum(abundance)) %>%
    dplyr::mutate(PercESUtake = (ExpTake/abundance)*100) %>%
    dplyr::mutate(PercESUkill = (TotalMorts/abundance)*100) %>%
    dplyr::filter(!abundance == 0)

  df_ls_list <- unique(abund[abund$Production == "Listed Hatchery and Natural Origin", "Species"])

  df_ls <- df %>%
    dplyr::filter(Species %in% unlist(df_ls_list)) %>%
    dplyr::filter(LifeStage == "Adult") %>%
    dplyr::left_join(select(df_by_sp_ls, c(Species, LifeStage, abundance, PercESUtake, PercESUkill)),
              by = c("Species", "LifeStage"))

  #Calculate take sums as perc of ESU/DPS by species, lifestage, and hatchery origin
  #For species where you can't distinguish clipped and unclipped hatchery fish
  df_by_sp_ls_HOR <- df_abund %>%
    dplyr::mutate(Production = stringr::str_remove(Production, c(" Intact Adipose| Adipose Clip"))) %>%
    dplyr::group_by(Species, LifeStage, Production) %>%
    tidyr::replace_na(list(ExpTake = 0, TotalMorts = 0, abundance = 0)) %>%
    dplyr::summarise(ExpTake = sum(ExpTake), TotalMorts = sum(TotalMorts), abundance = sum(abundance)) %>%
    dplyr::mutate(PercESUtake = (ExpTake/abundance)*100) %>%
    dplyr::mutate(PercESUkill = (TotalMorts/abundance)*100) %>%
    dplyr::filter(!Production == "Natural")

  df_HOR_list <- unique(abund[abund$Production == "Listed Hatchery", "Species"])

  df_HOR <- df %>%
    dplyr::filter(Species %in% unlist(df_HOR_list)) %>%
    dplyr::filter(LifeStage == "Adult") %>%
    dplyr::filter(!Production == "Natural") %>%
    dplyr::left_join(select(df_by_sp_ls_HOR, c(Species, LifeStage, abundance, PercESUtake, PercESUkill)),
              by = c("Species", "LifeStage"))


  #Calculate take sums as perc of ESU/DPS when you have all component information
  #For salmonid juveniles and most salmonid adults, green sturgeon juvs, subadults, and adults
  df_by_all <- df_abund %>%
    dplyr::group_by(Species, LifeStage, Production) %>%
    tidyr::replace_na(list(ExpTake = 0, TotalMorts = 0, abundance = 0)) %>%
    dplyr::summarise(ExpTake = sum(ExpTake), TotalMorts = sum(TotalMorts), abundance = sum(abundance)) %>%
    dplyr::mutate(PercESUtake = (ExpTake/abundance)*100) %>%
    dplyr::mutate(PercESUkill = (TotalMorts/abundance)*100) %>%
    dplyr::filter(!abundance == 0)
  #filter(!ExpTake == 0)

  df_sturg <- df %>%
    dplyr::filter(Species == "Southern DPS green sturgeon") %>%
    dplyr::left_join(select(df_by_all, c(Species, LifeStage, Production, abundance, PercESUtake, PercESUkill)),
              by = c("Species", "LifeStage", "Production"))


  df_NOR <- df %>%
    dplyr::filter(LifeStage == "Adult" & Production == "Natural") %>%
    dplyr::filter(!Species %in% c("Southern DPS green sturgeon",
                           "Puget Sound/Georgia Basin DPS bocaccio",
                           "Puget Sound/Georgia Basin DPS yelloweye rockfish",
                           "Southern DPS eulachon")) %>%
    dplyr::left_join(select(df_by_all, c(Species, LifeStage, Production, abundance, PercESUtake, PercESUkill)),
              by = c("Species", "LifeStage", "Production")) %>%
    dplyr::filter(!abundance == 0)

  df_juvs <- df %>%
    dplyr::filter(LifeStage == "Juvenile") %>%
    dplyr::filter(!Species %in% c("Southern DPS green sturgeon",
                           "Puget Sound/Georgia Basin DPS bocaccio",
                           "Puget Sound/Georgia Basin DPS yelloweye rockfish",
                           "Southern DPS eulachon")) %>%
    dplyr::left_join(select(df_by_all, c(Species, LifeStage, Production, abundance, PercESUtake, PercESUkill)),
              by = c("Species", "LifeStage", "Production"))

  perc_ESU_DPS_table <- bind_rows(df_ls, df_sp, df_HOR, df_NOR, df_sturg, df_juvs) %>%
    dplyr::relocate(LifeStage, .after = Species) %>%
    dplyr::select(!abundance) %>%
    dplyr::mutate(PercESUtake = dplyr::case_when(
      PercESUtake == 0 | PercESUtake >=0.001 ~ scales::number(PercESUtake, big.mark = "", accuracy = .001),
      PercESUtake >0 & PercESUtake <0.001 ~ scales::number(PercESUtake, big.mark = "", accuracy = .0001))) %>%
    dplyr::mutate(PercESUtake = as.character(PercESUtake)) %>%
    dplyr::mutate(PercESUtake = stringr::str_replace_all(PercESUtake, c(
      "0.0000" = "<0.001",
      "0.0001" = "<0.001",
      "0.0002" = "<0.001",
      "0.0003" = "<0.001",
      "0.0004" = "<0.001",
      "0.0005" = "<0.001",
      "0.0006" = "<0.001",
      "0.0007" = "<0.001",
      "0.0008" = "<0.001",
      "0.0009" = "<0.001"))) %>%
    dplyr::mutate(PercESUkill = dplyr::case_when(
      PercESUkill == 0 | PercESUkill >=0.001 ~ scales::number(PercESUkill, big.mark = "", accuracy = .001),
      PercESUkill >0 & PercESUkill <0.001 ~ scales::number(PercESUkill, big.mark = "", accuracy = .0001))) %>%
    dplyr::mutate(PercESUkill = as.character(PercESUkill)) %>%
    dplyr::mutate(PercESUkill = stringr::str_replace_all(PercESUkill, c(
      "0.0000" = "<0.001",
      "0.0001" = "<0.001",
      "0.0002" = "<0.001",
      "0.0003" = "<0.001",
      "0.0004" = "<0.001",
      "0.0005" = "<0.001",
      "0.0006" = "<0.001",
      "0.0007" = "<0.001",
      "0.0008" = "<0.001",
      "0.0009" = "<0.001"))) %>%
    order_table()

  return(perc_ESU_DPS_table)
}
