# *** perc_ESU_DPS ----------
#' @title Percent of ESU/DPS
#'
#' @description Calculate the percent of the ESU or DPS abundance that will be taken or killed by proposed research
#'
#' @details A dataframe of proposed take is merged with a dataframe of abundance to calculate the percentage of abundance the take summary represents. This can be applied to an individual permit summary or a baseline or program-wide take summary as long as the necessary columns (Species, LifeStage, Production, ExpTake, TotalMorts, abundance) are present
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
    group_by(Species) %>%
    replace_na(list(ExpTake = 0, TotalMorts = 0, abundance = 0)) %>%
    summarise(ExpTake = sum(ExpTake), TotalMorts = sum(TotalMorts), abundance = sum(abundance)) %>%
    mutate(PercESUtake = (ExpTake/abundance)*100) %>%
    mutate(PercESUkill = (TotalMorts/abundance)*100)

  df_sp <- df %>%
    filter(Species %in% c("Puget Sound/Georgia Basin DPS bocaccio",
                          "Puget Sound/Georgia Basin DPS yelloweye rockfish",
                          "Southern DPS eulachon")) %>%
    left_join(select(df_by_sp_adults, c(Species, abundance, PercESUtake, PercESUkill)),
              by = "Species")

  #Calculate take sums as perc of ESU/DPS by species and lifestage
  #For species where you can't distinguish hatchery and natural origin adults
  df_by_sp_ls <- df_abund %>%
    group_by(Species, LifeStage) %>%
    replace_na(list(ExpTake = 0, TotalMorts = 0, abundance = 0)) %>%
    summarise(ExpTake = sum(ExpTake), TotalMorts = sum(TotalMorts), abundance = sum(abundance)) %>%
    mutate(PercESUtake = (ExpTake/abundance)*100) %>%
    mutate(PercESUkill = (TotalMorts/abundance)*100) %>%
    filter(!abundance == 0)

  df_ls_list <- unique(abund[abund$Production == "Listed Hatchery and Natural Origin", "Species"])

  df_ls <- df %>%
    filter(Species %in% unlist(df_ls_list)) %>%
    filter(LifeStage == "Adult") %>%
    left_join(select(df_by_sp_ls, c(Species, LifeStage, abundance, PercESUtake, PercESUkill)),
              by = c("Species", "LifeStage"))

  #Calculate take sums as perc of ESU/DPS by species, lifestage, and hatchery origin
  #For species where you can't distinguish clipped and unclipped hatchery fish
  df_by_sp_ls_HOR <- df_abund %>%
    mutate(Production = str_remove(Production, c(" Intact Adipose| Adipose Clip"))) %>%
    group_by(Species, LifeStage, Production) %>%
    replace_na(list(ExpTake = 0, TotalMorts = 0, abundance = 0)) %>%
    summarise(ExpTake = sum(ExpTake), TotalMorts = sum(TotalMorts), abundance = sum(abundance)) %>%
    mutate(PercESUtake = (ExpTake/abundance)*100) %>%
    mutate(PercESUkill = (TotalMorts/abundance)*100) %>%
    filter(!Production == "Natural")

  df_HOR_list <- unique(abund[abund$Production == "Listed Hatchery", "Species"])

  df_HOR <- df %>%
    filter(Species %in% unlist(df_HOR_list)) %>%
    filter(LifeStage == "Adult") %>%
    filter(!Production == "Natural") %>%
    left_join(select(df_by_sp_ls_HOR, c(Species, LifeStage, abundance, PercESUtake, PercESUkill)),
              by = c("Species", "LifeStage"))


  #Calculate take sums as perc of ESU/DPS when you have all component information
  #For salmonid juveniles and most salmonid adults, green sturgeon juvs, subadults, and adults
  df_by_all <- df_abund %>%
    group_by(Species, LifeStage, Production) %>%
    replace_na(list(ExpTake = 0, TotalMorts = 0, abundance = 0)) %>%
    summarise(ExpTake = sum(ExpTake), TotalMorts = sum(TotalMorts), abundance = sum(abundance)) %>%
    mutate(PercESUtake = (ExpTake/abundance)*100) %>%
    mutate(PercESUkill = (TotalMorts/abundance)*100) %>%
    filter(!abundance == 0)
  #filter(!ExpTake == 0)

  df_sturg <- df %>%
    filter(Species == "Southern DPS green sturgeon") %>%
    left_join(select(df_by_all, c(Species, LifeStage, Production, abundance, PercESUtake, PercESUkill)),
              by = c("Species", "LifeStage", "Production"))


  df_NOR <- df %>%
    filter(LifeStage == "Adult" & Production == "Natural") %>%
    filter(!Species %in% c("Southern DPS green sturgeon",
                           "Puget Sound/Georgia Basin DPS bocaccio",
                           "Puget Sound/Georgia Basin DPS yelloweye rockfish",
                           "Southern DPS eulachon")) %>%
    left_join(select(df_by_all, c(Species, LifeStage, Production, abundance, PercESUtake, PercESUkill)),
              by = c("Species", "LifeStage", "Production")) %>%
    filter(!abundance == 0)

  df_juvs <- df %>%
    filter(LifeStage == "Juvenile") %>%
    filter(!Species %in% c("Southern DPS green sturgeon",
                           "Puget Sound/Georgia Basin DPS bocaccio",
                           "Puget Sound/Georgia Basin DPS yelloweye rockfish",
                           "Southern DPS eulachon")) %>%
    left_join(select(df_by_all, c(Species, LifeStage, Production, abundance, PercESUtake, PercESUkill)),
              by = c("Species", "LifeStage", "Production"))

  perc_ESU_DPS_table <- bind_rows(df_ls, df_sp, df_HOR, df_NOR, df_sturg, df_juvs) %>%
    relocate(LifeStage, .after = Species) %>%
    select(!abundance) %>%
    mutate(PercESUtake = case_when(
      PercESUtake == 0 | PercESUtake >=0.001 ~ scales::number(PercESUtake, big.mark = "", accuracy = .001),
      PercESUtake >0 & PercESUtake <0.001 ~ scales::number(PercESUtake, big.mark = "", accuracy = .0001))) %>%
    mutate(PercESUtake = as.character(PercESUtake)) %>%
    mutate(PercESUtake = str_replace_all(PercESUtake, c(
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
    mutate(PercESUkill = case_when(
      PercESUkill == 0 | PercESUkill >=0.001 ~ scales::number(PercESUkill, big.mark = "", accuracy = .001),
      PercESUkill >0 & PercESUkill <0.001 ~ scales::number(PercESUkill, big.mark = "", accuracy = .0001))) %>%
    mutate(PercESUkill = as.character(PercESUkill)) %>%
    mutate(PercESUkill = str_replace_all(PercESUkill, c(
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
