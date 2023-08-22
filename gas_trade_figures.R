library("reshape2")
library("stringr")
library("scales")
library("plyr")
library("readr")
library("tidyr")
library("dplyr")
library("ggplot2")
library("rgcam")
library("jgcricolors")
library("grid")


# SET WORKING DIRECTORY -----------------------------------------------------------------------------
## REQUIRED: Manually set working directory
setwd("C:/gas_trade/metarepo/")

PLOT_FOLDER <- paste(getwd(), "/figures/", sep = "")

# Create the specified output directory inside the current working directory
dir.create(PLOT_FOLDER)


# FUNCTIONS ---------------------------------------------------------------
#Function "is not an element of" (opposite of %in%)
'%!in%' <- function( x, y ) !( '%in%'( x, y ) )

aggregate_rows <- function(df, filter_var, var_name, filter_group, ...) {
  group_var <- quos(...)
  filter_var <- enquo(filter_var)
  filter_var_name <- quo_name(filter_var)
  df %>%
    filter(!!filter_var %in% filter_group) %>%
    group_by(!!!group_var) %>%
    dplyr::summarise(value = sum(value)) %>%
    dplyr::mutate(!!filter_var_name := !!var_name)
}

#returns difference from a scenario
diff_from_scen <- function(df, diff_scenarios, ref_scenario, join_var){
  
  diff_df <- df %>%
    filter(scenario %in% diff_scenarios)
  
  ref_df <- df %>%
    filter(scenario %in% ref_scenario)
  
  output_df <- diff_df %>%
    full_join(ref_df, by = join_var,
              suffix = c(".diff", ".ref")) %>%
    
    mutate(value.diff = if_else(is.na(value.diff),0,value.diff),
           value.ref = if_else(is.na(value.ref),0,value.ref),
           value = value.diff - value.ref)
  
  return(output_df)
}

# DEFINE CONVERSION FACTORS ----------------------------------------------------------
EJ_Tcf <- .981
EJ_Bcfd <- .981 * 1000 / 365
MtC_GtCO2 <- 44 / (12 * 1000)
EJ_Mcf <- EJ_Tcf * 1e+6
EJ_TWh <- 277.7777777
EJ_quad <- 0.95
USD1975_2021 <- 1.18895 / 0.29829
C_CO2 <- 44/12
Tg_Gt <- 1e+12 / (1e+9 * 1e+6)

CONV_MJ_BTU <- 947.777
CONV_BTU_CF <- 1 / 1027
CONV_CF_TONLNG <- 1 / 48700
# DEFINE COLORS, SCENARIOS, GROUPS, LEVELS ------------------------------------------------------------------

pal_16 <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442","#CC79A7","#333333", "#D55E00", "#0072B2",  
            "#333333", "#FFCC00", "#CC6600", "#006600", "#3333CC", "#CC0033", "#0099CC", "#999966")

elec_gen_colors <- c("coal" = "gray20",
                     "coal CCS" = "gray40",
                     "refined liquids" = "#d01c2a",
                     "refined liquids CCS" = "#f7988f",
                     "gas" = "deepskyblue1",
                     "gas CCS" = "darkslategray1",
                     "biomass" = "#00931d",
                     "biomass CCS" = "#88c892",
                     "geothermal" = "#ad440c",
                     "hydrogen" = "peachpuff2",
                     "hydro" = "#3d86f9",
                     "nuclear" = "#ef8e27",
                     "solar" = "#fdfa28",
                     "wind" = "#fdd67b")

res_prod_colors <- c("coal" = "gray20",
                     "natural gas" = "deepskyblue1",
                     "crude oil" = "#d01c2a")

NG_trade_group <- c("traded Afr_MidE pipeline gas",
              "traded EUR pipeline gas",
              "traded LA pipeline gas",
              "traded N.Amer pipeline gas",
              "traded PAC pipeline gas",
              "traded RUS pipeline gas",
              "traded LNG")

pri_ene_fuels <- c("a oil", "b natural gas", "c coal", "d biomass", "e nuclear",
                   "f hydro", "g wind", "h solar", "i geothermal", "j traditional biomass")
pri_ene_CCS_fuels <- c("a oil", "a oil CCS", "b natural gas", "b natural gas CCS",
                       "c coal", "c coal CCS", "d biomass", "d biomass CCS", "e nuclear",
                       "f hydro", "g wind", "h solar", "i geothermal", "j traditional biomass")

#GHG emissions
res_prod_GHG_group <- c("coal", "unconventional oil", "crude oil", "natural gas")
trans_GHG_group <- c("gas pipeline", "gas processing", "refining", "H2 central production", "H2 forecourt production")
aglu_GHG_group <- c("Corn", "FiberCrop", "FodderGrass", "FodderHerb", "MiscCrop", "OilCrop", "OtherGrain", "PalmFruit", "Rice", "RootTuber","SugarCrop", "Wheat", 
                    "biomass", "UnmanagedLand", "regional biomass", "regional biomassOil", "regional corn for ethanol", "regional sugar for ethanol",
                    "Beef", "Dairy", "Pork", "Poultry", "SheepGoat")
urb_GHG_group <- c("desalinated water",  "urban processes")
ind_GHG_group <- c("N fertilizer", "cement", 
                   "industrial energy use", "industrial feedstocks", "industrial processes", "process heat cement")
trn_GHG_group <- c( "trn_aviation_intl","trn_freight","trn_freight_road",
                    "trn_pass","trn_pass_road","trn_pass_road_LDV","trn_pass_road_LDV_4W","trn_shipping_intl")
elec_GHG_group <- c("backup_electricity", "elec_biomass (IGCC CCS)", "elec_biomass (IGCC)", "elec_biomass (conv CCS)", "elec_biomass (conv)",
                    "elec_coal (IGCC CCS)","elec_coal (IGCC)","elec_coal (conv pul CCS)","elec_coal (conv pul)",
                    "elec_gas (CC CCS)","elec_gas (CC)","elec_gas (steam/CT)",
                    "elec_refined liquids (CC CCS)","elec_refined liquids (CC)","elec_refined liquids (steam/CT)",
                    "electricity","electricity_net_ownuse", "csp_backup")
bld_GHG_group <- c("comm cooling", "comm heating", "comm others", "resid cooling", "resid heating", "resid others", "district heat")




trade_sector_levels <- c("LNG", "Afr_MidE", "EUR", "LA", "N.Amer", "PAC", "RUS")
domestic_sector_levels <- c("domestic", "LNG", "Afr_MidE", "EUR", "LA", "N.Amer", "PAC", "RUS")

grouped_region_levels <- c("USA", "Middle East", "Russia", "Europe", "SE.Asia", "Can.+Mex.", "Africa",
                           "C.Asia+E.Eur", "LAC", "China", "Australia+NZ", "S.Asia","India","E.Asia")
flip_grouped_region_levels <- c("E.Asia", "India", "S.Asia", "Australia+NZ", "China", "LAC", "C.Asia+E.Eur",
                                "Africa", "Can.+Mex.", "SE.Asia","Europe", "Russia","Middle East", "USA")

paper_scenarios <- c("Reference",
                     "Transition",
                     "Reference_LLS",
                     "Transition_LLS",
                     "Reference_LT_sshc",
                     "Transition_LT_sshc")

paper_scenario_labels <- c("Reference" = "Reference",
                           "Transition" = "Transition",
                           "Reference_LLS" = "Reference_LLS",
                           "Transition_LLS" = "Transition_LLS",
                           "Reference_LT_sshc" = "Reference_LT",
                           "Transition_LT_sshc" = "Transition_LT")


# READ IN ADDITIONAL DATA, MAPPING FILES ----------------------------------

grouped_region_mapping <- readr::read_csv("./mappings/grouped_region_mapping.csv")
grouped_region_mapping_WEO <- readr::read_csv("./mappings/grouped_region_mapping_WEO.csv")
elec_gen_mapping <- readr::read_csv("./mappings/elec_gen_mapping.csv")
GWP <- readr::read_csv("./mappings/GWP.csv")

#IPCC AR6 data
AR6_DB <- readr::read_csv("./comparison_data/AR6_Scenarios_Database_World_v1.1.csv")

AR6_DB_long <- AR6_DB %>%
  pivot_longer(cols = as.character(seq(1995,2100),by=1)) %>%
  dplyr::rename(year = name) %>%
  mutate(year = as.numeric(year))

#IEA World Energy Outlook data
WEO_world_FFI_CO2_em <- readr::read_csv("./comparison_data/WEO_world_FFI_CO2_em.csv")
WEO_world_gas_cons <- readr::read_csv("./comparison_data/WEO_world_gas_cons.csv")
WEO_world_LNG_cons <- readr::read_csv("./comparison_data/WEO_world_LNG_cons.csv")
WEO_world_pipeline_cons <- readr::read_csv("./comparison_data/WEO_world_pipeline_cons.csv")
WEO_world_traded_gas_cons <- bind_rows(WEO_world_LNG_cons,
                                       WEO_world_pipeline_cons) %>%
  group_by(scenario, year) %>%
  summarise_at(c("Bcm", "Tcf", "EJ"), sum)

WEO_reg_gas_cons <- readr::read_csv("./comparison_data/WEO_reg_gas_cons.csv")
WEO_reg_gas_prod <- readr::read_csv("./comparison_data/WEO_reg_gas_prod.csv")
# SELECT DATA ---------------------------------------------------------

prj <- rgcam::loadProject("gas_trade.dat")
prj_LUC <- rgcam::loadProject("gas_trade_LUC.dat")


# RETRIEVE QUERIES --------------------------------------------------------

reg_gas_tech <- rgcam::getQuery(prj, "regional natural gas by tech (nest)")

tra_gas_tech <- rgcam::getQuery(prj, "traded gas by tech") %>%
  filter(sector %in% NG_trade_group, subsector != "statistical differences") %>%
  select(-region) %>%
  separate(subsector, c("region",NA), sep = " traded ")

pri_ene<- rgcam::getQuery(prj, "primary energy consumption by region (direct equivalent)") 

pri_ene_CCS <- rgcam::getQuery(prj, "primary energy consumption with CCS by region (direct equivalent)") 

tra_gas_tech_vintage <- rgcam::getQuery(prj, "traded gas by tech and vintage") %>%
  filter(sector %in% NG_trade_group, subsector != "statistical differences") %>%
  select(-region) %>%
  separate(subsector, c("region",NA), sep = " traded ") %>%
  separate(technology, c(NA, "vintage"), sep = ",year=")

reg_gas_tech_vintage <- rgcam::getQuery(prj, "regional natural gas by tech and vintage") %>%
  separate(technology, c("technology","vintage"), sep = ",year=") %>%
  select(scenario, region, technology, vintage, year, value, Units)

elec_gen <- rgcam::getQuery(prj, "elec gen by gen tech") %>%
  left_join(elec_gen_mapping, by = c("technology"))

final_ene <- rgcam::getQuery(prj, "final energy consumption by sector and fuel")

res_prod <- rgcam::getQuery(prj, "resource production") 

GHG_sector <- rgcam::getQuery(prj, "nonCO2 emissions by sector") 
GHG_res_prod <- rgcam::getQuery(prj, "nonCO2 emissions by resource production") 
LUC_em <- rgcam::getQuery(prj_LUC, "LUC emissions by region") %>%
  group_by(scenario, region, year) %>%
  dplyr::summarize(value = sum(value)) %>%
  mutate(value = value * 44/12,
         Units = "MtCO2", 
         sector = "LUC",
         GHG = "CO2")

# DATA PROCESSING ---------------------------------------------------------


  # RESOURCE PRODUCTION -----------------------------------------------------
global_res_prod <- res_prod %>%
  group_by(scenario, resource, year) %>%
  dplyr::summarise(value = sum(value))

  # PRODUCTION, TRADE, CONSUMPTION FLOWS --------------------------------

gross_imports_domestic_ng <- reg_gas_tech %>%
  filter(Units =="EJ") %>%
  select(scenario, region, technology, year, value) %>%
  separate(technology, c("trade", "sector"), sep = " ") %>%
  mutate(sector = if_else(trade == "domestic", "domestic", sector))

gross_exports_ng <- tra_gas_tech %>%
  separate(sector, c("trade", "sector"), sep = " ") %>%
  mutate(trade = if_else(trade == "traded", "exported", trade)) %>%
  select(-technology, -output, -Units)

gross_trade_ng <- gross_imports_domestic_ng %>%
  filter(trade == "imported") %>%
  mutate(value = value * -1) %>%
  bind_rows(gross_exports_ng)

net_trade_ng <- gross_trade_ng %>%
  group_by(scenario, region, year) %>%
  dplyr::summarise(value = sum(value))

trade_ng <- gross_trade_ng %>%
  left_join(net_trade_ng, by = c("scenario", "region", "year"), suffix = c("", ".net")) 

gross_production_ng <- gross_imports_domestic_ng %>%
  filter(trade == "domestic") %>%
  bind_rows(gross_exports_ng) 

gross_consumption_ng <- gross_imports_domestic_ng %>%
  mutate(value = value * -1) 

  # AGG REGION PRODUCTION, TRADE, CONSUMPTION FLOWS -------------------------

grouped_gross_trade_ng <- gross_trade_ng %>%
  left_join(grouped_region_mapping, by = "region") %>%
  group_by(scenario, grouped_region, trade, sector, year) %>%
  dplyr::summarise(value = sum(value))

grouped_net_trade_ng <- gross_trade_ng %>%
  left_join(grouped_region_mapping, by = "region") %>%
  group_by(scenario, grouped_region, year) %>%
  dplyr::summarise(value = sum(value))

grouped_trade_ng <- grouped_gross_trade_ng %>%
  left_join(grouped_net_trade_ng, by = c("scenario", "grouped_region", "year"), suffix = c("", ".net")) %>%
  mutate(grouped_region = factor(grouped_region, levels = flip_grouped_region_levels),
         sector = factor(sector, levels = trade_sector_levels))

grouped_production_ng <- gross_production_ng %>%
  left_join(grouped_region_mapping, by = "region") %>%
  group_by(scenario, grouped_region, trade, sector, year) %>%
  dplyr::summarise(value = sum(value)) %>%
  mutate(grouped_region = factor(grouped_region, levels = flip_grouped_region_levels),
         sector = factor(sector, levels = trade_sector_levels))

grouped_consumption_ng <- gross_consumption_ng %>%
  left_join(grouped_region_mapping, by = "region") %>%
  group_by(scenario, grouped_region, trade, sector, year) %>%
  dplyr::summarise(value = sum(value)) %>%
  mutate(grouped_region = factor(grouped_region, levels = flip_grouped_region_levels),
         sector = factor(sector, levels = trade_sector_levels))

  # DIFF AGG REGION PRODUCTION(REFERENCE_LLS AND REFERENCE_LT SCENARIOS) ----------
diff_grouped_production_ng_REF_LLS_sens <- diff_from_scen(df = grouped_production_ng, 
                                                          diff_scenarios = c("Reference_LLS"),
                                                          ref_scenario = "Reference",
                                                          join_var = c("grouped_region", "trade", "sector", "year")) %>%
  mutate(scenario.diff = "Reference_LLS")

diff_grouped_production_ng_REF_LT_sens <- diff_from_scen(df = grouped_production_ng, 
                                                         diff_scenarios = c("Reference_LT_sshc"),
                                                         ref_scenario = "Reference",
                                                         join_var = c("grouped_region", "trade", "sector", "year")) %>%
  mutate(scenario.diff = "Reference_LT_sshc")


diff_grouped_production_ng_REF_sens_net <- bind_rows(diff_grouped_production_ng_REF_LLS_sens,
                                                     diff_grouped_production_ng_REF_LT_sens)%>%
  group_by(scenario.diff, grouped_region, year) %>%
  dplyr::summarise(value = sum(value))

diff_grouped_production_ng_REF_sens_plot <- bind_rows(diff_grouped_production_ng_REF_LLS_sens,
                                                      diff_grouped_production_ng_REF_LT_sens) %>%
  left_join(diff_grouped_production_ng_REF_sens_net, by = c("scenario.diff", "grouped_region", "year"), suffix = c("", ".net"))


  # DIFF AGG REGION PRODUCTION (TRANSITION_LLS AND TRANSITION_LT SCENARIOS) --------

diff_grouped_production_ng_TRANS_LLS_sens <- diff_from_scen(df = grouped_production_ng, 
                                                            diff_scenarios = c("Transition_LLS"),
                                                            ref_scenario = "Transition",
                                                            join_var = c("grouped_region", "trade", "sector", "year")) %>%
  mutate(scenario.diff = "Transition_LLS")

diff_grouped_production_ng_TRANS_LT_sens <- diff_from_scen(df = grouped_production_ng, 
                                                           diff_scenarios = c("Transition_LT_sshc"),
                                                           ref_scenario = "Transition",
                                                           join_var = c("grouped_region", "trade", "sector", "year")) %>%
  mutate(scenario.diff = "Transition_LT_sshc")

diff_grouped_production_ng_TRANS_sens_net <- bind_rows(diff_grouped_production_ng_TRANS_LLS_sens,
                                                       diff_grouped_production_ng_TRANS_LT_sens) %>%
  group_by(scenario.diff, grouped_region, year) %>%
  dplyr::summarise(value = sum(value))

diff_grouped_production_ng_TRANS_sens_plot <- bind_rows(diff_grouped_production_ng_TRANS_LLS_sens,
                                                        diff_grouped_production_ng_TRANS_LT_sens) %>%
  left_join(diff_grouped_production_ng_TRANS_sens_net, by = c("scenario.diff", "grouped_region", "year"), suffix = c("", ".net"))



  # DIFF AGG CONSUMPTION (REFERENCE_LLS AND REFERENCE_LT SCENARIOS) ---------
diff_grouped_consumption_ng_REF_LLS_sens <- diff_from_scen(df = grouped_consumption_ng, 
                                                           diff_scenarios = c("Reference_LLS"),
                                                           ref_scenario = "Reference",
                                                           join_var = c("grouped_region", "trade", "sector", "year")) %>%
  mutate(scenario.diff = "Reference_LLS")

diff_grouped_consumption_ng_REF_LT_sens <- diff_from_scen(df = grouped_consumption_ng, 
                                                          diff_scenarios = c("Reference_LT_sshc"),
                                                          ref_scenario = "Reference",
                                                          join_var = c("grouped_region", "trade", "sector", "year"))%>%
  mutate(scenario.diff = "Reference_LT_sshc")

diff_grouped_consumption_ng_REF_sens_net <- bind_rows(diff_grouped_consumption_ng_REF_LLS_sens,
                                                      diff_grouped_consumption_ng_REF_LT_sens) %>%
  group_by(scenario.diff, grouped_region, year) %>%
  dplyr::summarise(value = sum(value))

diff_grouped_consumption_ng_REF_sens_plot <- bind_rows(diff_grouped_consumption_ng_REF_LLS_sens,
                                                       diff_grouped_consumption_ng_REF_LT_sens) %>%
  left_join(diff_grouped_consumption_ng_REF_sens_net, by = c("scenario.diff", "grouped_region", "year"), suffix = c("", ".net"))


  # DIFF AGG CONSUMPTION (TRANSITION_LLS and TRANSITION_LT SCENARIOS) -------
diff_grouped_consumption_ng_TRANS_LLS_sens <- diff_from_scen(df = grouped_consumption_ng, 
                                                             diff_scenarios = c("Transition_LLS"),
                                                             ref_scenario = "Transition",
                                                             join_var = c("grouped_region", "trade", "sector", "year")) %>%
  mutate(scenario.diff = "Transition_LLS")

diff_grouped_consumption_ng_TRANS_LT_sens <- diff_from_scen(df = grouped_consumption_ng, 
                                                            diff_scenarios = c("Transition_LT_sshc"),
                                                            ref_scenario = "Transition",
                                                            join_var = c("grouped_region", "trade", "sector", "year")) %>%
  mutate(scenario.diff = "Transition_LT_sshc")


diff_grouped_consumption_ng_TRANS_sens_net <- bind_rows(diff_grouped_consumption_ng_TRANS_LLS_sens,
                                                        diff_grouped_consumption_ng_TRANS_LT_sens) %>%
  group_by(scenario.diff, grouped_region, year) %>%
  dplyr::summarise(value = sum(value))

diff_grouped_consumption_ng_TRANS_sens_plot <- bind_rows(diff_grouped_consumption_ng_TRANS_LLS_sens,
                                                         diff_grouped_consumption_ng_TRANS_LT_sens) %>%
  left_join(diff_grouped_consumption_ng_TRANS_sens_net, by = c("scenario.diff", "grouped_region", "year"), suffix = c("", ".net"))

  # PRIMARY ENERGY ----------------------------------------------------------
#primary energy
nat_gas_pri_ene <- pri_ene %>%
  filter(fuel %in% c("traded Afr_MidE pipeline gas", "traded LNG", "traded LA pipeline gas", "traded PAC pipeline gas", "traded N.Amer pipeline gas",
                     "traded RUS pipeline gas", "traded EUR pipeline gas",
                     Units == "EJ")) %>%
  group_by(scenario, region, year) %>%
  dplyr::summarise(value = sum(value)) %>%
  mutate(fuel = "b natural gas")

grouped_pri_ene <- pri_ene %>%
  filter(fuel %!in% c("traded Afr_MidE pipeline gas", "traded LNG", "traded LA pipeline gas", "traded PAC pipeline gas", "traded N.Amer pipeline gas",
                      "traded RUS pipeline gas", "traded EUR pipeline gas"),
         Units == "EJ") %>%
  bind_rows(nat_gas_pri_ene) %>%
  group_by(scenario, region, fuel, year) %>%
  dplyr::summarise(value = sum(value)) %>%
  filter(fuel %in% pri_ene_fuels)

global_pri_ene <- grouped_pri_ene %>%
  group_by(scenario, fuel, year) %>%
  dplyr::summarise(value = sum(value))

  # PRIMARY ENERGY WITH CCS  -------------------------------------------------
#primary energy with CCS
nat_gas_pri_ene_CCS <- pri_ene_CCS %>%
  filter(fuel %in% c("traded Afr_MidE pipeline gas", "traded LNG", "traded LA pipeline gas", "traded PAC pipeline gas", "traded N.Amer pipeline gas",
                     "traded RUS pipeline gas", "traded EUR pipeline gas",
                     Units == "EJ")) %>%
  group_by(scenario, region, year) %>%
  dplyr::summarise(value = sum(value)) %>%
  mutate(fuel = "b natural gas")

nat_gas_CCS_pri_ene_CCS <- pri_ene_CCS %>%
  filter(fuel %in% c("traded Afr_MidE pipeline gas CCS", "traded LNG CCS", "traded LA pipeline gas CCS", "traded PAC pipeline gas CCS", "traded N.Amer pipeline gas CCS",
                     "traded RUS pipeline gas CCS", "traded EUR pipeline gas CCS",
                     Units == "EJ")) %>%
  group_by(scenario, region, year) %>%
  dplyr::summarise(value = sum(value)) %>%
  mutate(fuel = "b natural gas CCS")

grouped_pri_ene_CCS <- pri_ene_CCS %>%
  filter(fuel %!in% c("traded Afr_MidE pipeline gas", "traded LNG", "traded LA pipeline gas", "traded PAC pipeline gas", "traded N.Amer pipeline gas",
                      "traded RUS pipeline gas", "traded EUR pipeline gas",
                      "traded Afr_MidE pipeline gas CCS", "traded LNG CCS", "traded LA pipeline gas CCS", "traded PAC pipeline gas CCS", "traded N.Amer pipeline gas CCS",
                      "traded RUS pipeline gas CCS", "traded EUR pipeline gas CCS"),
         Units == "EJ") %>%
  bind_rows(nat_gas_pri_ene_CCS,
            nat_gas_CCS_pri_ene_CCS) %>%
  group_by(scenario, region, fuel, year) %>%
  dplyr::summarise(value = sum(value)) %>%
  filter(fuel %in% pri_ene_CCS_fuels)

global_pri_ene_CCS <- grouped_pri_ene_CCS %>%
  group_by(scenario, fuel, year) %>%
  dplyr::summarise(value = sum(value))

grouped_region_pri_ene_CCS <- grouped_pri_ene_CCS %>%
  left_join(grouped_region_mapping, by = c("region")) %>%
  group_by(scenario, grouped_region, fuel, year) %>%
  dplyr::summarise(value = sum(value))


  # DIFF PRIMARY ENERGY WITH CCS (LLS and LT SCENARIOS) --------------------------------------------
diff_grouped_region_pri_ene_CCS_REF <- diff_from_scen(df = grouped_region_pri_ene_CCS, 
                                                      diff_scenarios = c("Reference_LLS", "Reference_LT_sshc"),
                                                      ref_scenario = "Reference",
                                                      join_var = c("grouped_region", "fuel", "year"))

diff_grouped_region_pri_ene_CCS_REF_net <- diff_grouped_region_pri_ene_CCS_REF %>%
  group_by(scenario.diff, grouped_region, year) %>%
  dplyr::summarise(value = sum(value))

diff_grouped_region_pri_ene_CCS_REF_plot <- diff_grouped_region_pri_ene_CCS_REF %>%
  left_join(diff_grouped_region_pri_ene_CCS_REF_net, by = c("scenario.diff", "grouped_region", "year"), suffix = c("", ".net"))


diff_grouped_region_pri_ene_CCS_TRANS <- diff_from_scen(df = grouped_region_pri_ene_CCS, 
                                                        diff_scenarios = c("Transition_LLS", "Transition_LT_sshc"),
                                                        ref_scenario = "Transition",
                                                        join_var = c("grouped_region", "fuel", "year"))

diff_grouped_region_pri_ene_CCS_TRANS_net <- diff_grouped_region_pri_ene_CCS_TRANS %>%
  group_by(scenario.diff, grouped_region, year) %>%
  dplyr::summarise(value = sum(value))

diff_grouped_region_pri_ene_CCS_TRANS_plot <- diff_grouped_region_pri_ene_CCS_TRANS %>%
  left_join(diff_grouped_region_pri_ene_CCS_TRANS_net, by = c("scenario.diff", "grouped_region", "year"), suffix = c("", ".net"))

diff_grouped_region_pri_ene_CCS <- bind_rows(diff_grouped_region_pri_ene_CCS_REF_plot,
                                             diff_grouped_region_pri_ene_CCS_TRANS_plot) %>%
  mutate(grouped_region = factor(grouped_region, levels = flip_grouped_region_levels))

  # DIFF PRIMARY ENERGY WITH CCS (TRANSITION) -------------------------------------------------
diff_grouped_region_pri_ene_CCS_TRANS_REF <- diff_from_scen(df = grouped_region_pri_ene_CCS, 
                                                            diff_scenarios = c("Transition"),
                                                            ref_scenario = "Reference",
                                                            join_var = c("grouped_region", "fuel","year"))


diff_grouped_region_pri_ene_CCS_TRANS_REF_net <- diff_grouped_region_pri_ene_CCS_TRANS_REF %>%
  group_by(scenario.diff, grouped_region, year) %>%
  dplyr::summarise(value = sum(value))

diff_grouped_region_pri_ene_CCS_TRANS_REF_plot <- diff_grouped_region_pri_ene_CCS_TRANS_REF %>%
  left_join(diff_grouped_region_pri_ene_CCS_TRANS_REF_net, by = c("scenario.diff", "grouped_region", "year"), suffix = c("", ".net"))


  # ELECTRICITY GENERATION ----------------------------------------------------------------

grouped_region_elec_gen <- elec_gen %>%
  left_join(grouped_region_mapping, by = c("region")) %>%
  group_by(scenario, grouped_region, sector, year) %>%
  dplyr::summarise(value = sum(value))

global_elec_gen <- elec_gen %>%
  group_by(scenario, sector, year) %>%
  dplyr::summarise(value = sum(value))


  # DIFF ELECTRICITY GENERATION (TRANSITION) --------------------------------

diff_grouped_region_elec_gen_TRANS_REF <- diff_from_scen(df = grouped_region_elec_gen, 
                                                         diff_scenarios = c("Transition"),
                                                         ref_scenario = "Reference",
                                                         join_var = c("grouped_region", "sector","year"))


diff_grouped_region_elec_gen_TRANS_REF_net <- diff_grouped_region_elec_gen_TRANS_REF %>%
  group_by(scenario.diff, grouped_region, year) %>%
  dplyr::summarise(value = sum(value))


diff_grouped_region_elec_gen_TRANS_REF_plot <- diff_grouped_region_elec_gen_TRANS_REF %>%
  left_join(diff_grouped_region_elec_gen_TRANS_REF_net, by = c("scenario.diff", "grouped_region", "year"), suffix = c("", ".net"))


  # FINAL ENERGY ------------------------------------------------------------
grouped_region_final_ene <- final_ene %>%
  left_join(grouped_region_mapping, by = c("region")) %>%
  group_by(scenario, grouped_region, input, year) %>%
  dplyr::summarise(value = sum(value))

global_final_ene <- final_ene %>%
  group_by(scenario, input, year) %>%
  dplyr::summarise(value = sum(value))

  # DIFF FINAL ENERGY -------------------------------------------------------
diff_grouped_region_final_ene_TRANS_REF <- diff_from_scen(df = grouped_region_final_ene, 
                                                          diff_scenarios = c("Transition"),
                                                          ref_scenario = "Reference",
                                                          join_var = c("grouped_region", "input","year"))


diff_grouped_region_final_ene_TRANS_REF_net <- diff_grouped_region_final_ene_TRANS_REF %>%
  group_by(scenario.diff, grouped_region, year) %>%
  dplyr::summarise(value = sum(value))


diff_grouped_region_final_ene_TRANS_REF_plot <- diff_grouped_region_final_ene_TRANS_REF %>%
  left_join(diff_grouped_region_final_ene_TRANS_REF_net, by = c("scenario.diff", "grouped_region", "year"), suffix = c("", ".net"))



  # EXPORT INFRASTRUCTURE CAPACITY ------------------------------------------
    # "Gross Additions and Retirements by Vintage" ----------------------------

conv_traded_gas_tech_vintage <- tra_gas_tech_vintage %>%
  mutate(value = value * CONV_MJ_BTU * CONV_BTU_CF * CONV_CF_TONLNG * 10^6,
         Units = "MTPA",
         vintage = as.numeric(vintage))

# total export capacity in MTPA
conv_traded_gas_tech <- conv_traded_gas_tech_vintage %>%
  group_by(Units, scenario, sector, region, output, year) %>%
  dplyr::summarise(value = sum(value))

conv_global_traded_gas_tech <- conv_traded_gas_tech %>%
  group_by(scenario, year) %>%
  dplyr::summarise(value = sum(value))

# cumulative total export capacity in MTPA
cum_conv_traded_gas_tech <- conv_traded_gas_tech %>%
  group_by(Units, scenario, sector, region, output) %>%
  dplyr::mutate(cum_value = cumsum(value))

# Calculate additions by vintage
conv_traded_gas_tech_vintage %>%
  filter(vintage > 2015) %>% 
  mutate(additions = if_else(vintage == year, value, 0)) -> gas_vintage_add

# Calculate retirements by vintage
conv_traded_gas_tech_vintage %>%
  filter(vintage >= 2015) %>% 
  group_by(scenario, region, sector, Units, vintage) %>%
  mutate(prev_year = lag(value, n = 1L)) %>%
  ungroup() %>% 
  mutate(prev_year = if_else(is.na(prev_year), 0, prev_year), 
         retirements = prev_year - value,
         retirements = if_else(retirements < 0, 0, retirements)) %>%
  arrange(vintage, sector, region) -> gas_vintage_ret


    # "Expected Natural Retirements" ------------------------------------------

# Calculate s-curve output fraction
# parameters (from A_ff_TradedTechnology_NG.csv)
half.life <- 23
steepness <- 0.2
lifetime <- 45


conv_traded_gas_tech_vintage %>%
  # for base years only
  filter(vintage == 2015) %>%
  mutate(s_curve_frac = if_else(year > vintage,
                                (1 / (1 + exp( steepness * ((year - vintage) - half.life )))), 
                                1)) %>% 
  # Adjust s-curve output fraction to ensure that all of the capacity is retired at the end of lifetime
  mutate(s_curve_adj = if_else(year - vintage >= lifetime, 0, s_curve_frac),
         s_curve_adj = if_else(is.na(s_curve_adj), 1, s_curve_adj)) %>%
  select(scenario, region, sector,  vintage, Units, year, value, s_curve_adj) -> s_curve_frac_adj

# Expected gas capacity assuming natural shutdowns only
# Create variable reflecting each tech/ vintage generation in year of installment (OG_gas_capacity)
s_curve_frac_adj %>%
  left_join(conv_traded_gas_tech_vintage %>% 
              filter(vintage == year) %>%
              select(-year) %>%
              rename(OG_gas_capacity = value),
            by = c("scenario", "region", "sector", "vintage", "Units")) %>%
  mutate(gas_expect = OG_gas_capacity * s_curve_adj)  %>% 
  # Expected natural retirements
  group_by(scenario, region, sector, Units, vintage) %>%
  mutate(prev_yr_expect = lag(gas_expect, n = 1L), 
         natural_retire = if_else(year > vintage & prev_yr_expect > gas_expect, prev_yr_expect - gas_expect, 0)) %>% 
  ungroup() -> gas_retire_expect


    # "Gross Additions and Retirements by Sector" -------------------------

# Total additions per region/ sector/ year (in EJ)
gas_vintage_add %>% 
  group_by(scenario, region, sector, Units, year) %>%
  summarise(additions = sum(additions)) %>%
  ungroup() -> gas_total_add

# Total retirements per region/ sector/ year (in EJ)
gas_vintage_ret %>% 
  left_join(gas_retire_expect %>% 
              select(scenario, sector, region, vintage, year, output, Units, natural_retire), 
            by = c("Units", "scenario", "sector", "region", "vintage", "output", "year")) %>% 
  # vintages > 2015 have no expected natural retirements - replace NA values with zero
  replace_na(list(natural_retire = 0)) %>% 
  # adjust retirements to account for expected natural retirements
  mutate(retirements = retirements - natural_retire,
         # make sure we don't have any negative values for retirement after adjustment
         retirements = if_else(retirements < 0, 0, retirements)) %>% 
  group_by(scenario, region, sector, Units, year) %>%
  summarise(retirements = sum(retirements)) %>%
  ungroup() -> gas_total_ret


    # "Adjusted Additions and Retirements" -------------------------

# Merge total additions and retirements data tables
gas_total_add %>%
  left_join(gas_total_ret, by = c("scenario", "region", "sector", "Units", "year")) %>%
  mutate(add_adj = if_else(additions > retirements, additions - retirements, 0), 
         ret_adj = if_else(retirements > additions, retirements - additions, 0)) -> gas_add_ret

global_total_add <- gas_add_ret %>%
  group_by(scenario, sector, year) %>%
  dplyr::summarise(additions = sum(add_adj))

    # Cumulative Additions and Retirements ------------------------------------

cum_gas_total_add <- gas_add_ret %>%
  group_by(scenario, region, sector, Units) %>%
  dplyr::mutate(cum_additions = cumsum(add_adj))

cum_gas_total_ret <- gas_add_ret %>%
  group_by(scenario, region, sector, Units) %>%
  dplyr::mutate(cum_retirements = cumsum(ret_adj))

cum_global_total_add <- cum_gas_total_add %>%
  group_by(scenario, sector, year) %>%
  dplyr::summarise(cum_additions = sum(cum_additions)) 

cum_global_total_ret <- cum_gas_total_ret %>%
  group_by(scenario, sector, year) %>%
  dplyr::summarise(cum_retirements = sum(cum_retirements))

cum_agg_region_total_add <- cum_gas_total_add %>%
  left_join(grouped_region_mapping, by = c("region")) %>%
  group_by(scenario, grouped_region, sector, year) %>%
  dplyr::summarise(cum_additions = sum(cum_additions)) 

cum_agg_region_total_ret <- cum_gas_total_ret %>%
  left_join(grouped_region_mapping, by = c("region")) %>%
  group_by(scenario, grouped_region, sector, year) %>%
  dplyr::summarise(cum_retirements = sum(cum_retirements))




  # ANNUAL INFRASTRUCTURE COST ----------------------------------------------

#Convert from MTPA -> $ (via EJ * $/GJ)
global_total_add_LNG_cost <- global_total_add %>%
  filter(sector == "traded LNG") %>%
  dplyr::mutate(cost = additions * 2.02 * 10^9 * (1/(CONV_MJ_BTU * CONV_BTU_CF * CONV_CF_TONLNG * 10^6)) * (1/0.13) * (1/5))

global_total_add_pipeline_cost <- global_total_add %>%
  filter(sector != "traded LNG") %>%
  dplyr::mutate(cost = additions * 0.57 * 10^9 * (1/(CONV_MJ_BTU * CONV_BTU_CF * CONV_CF_TONLNG * 10^6)) * (1/0.13) * (1/5))

global_total_add_ng_cost <- bind_rows(global_total_add_LNG_cost,
                                      global_total_add_pipeline_cost)  
# IMPORT INFRASTRUCTURE CAPACITY ------------------------------------------

    # "Gross Additions and Retirements by Vintage" ----------------------------
#convert to MTPA (million tons per annum)
conv_reg_gas_tech_vintage <- reg_gas_tech_vintage %>%
  mutate(value = value * CONV_MJ_BTU * CONV_BTU_CF * CONV_CF_TONLNG * 10^6,
         Units = "MTPA",
         vintage = as.numeric(vintage))

# Total cumulative capacity
conv_reg_gas_tech <- conv_reg_gas_tech_vintage %>%
  group_by(scenario, region, technology, year) %>%
  dplyr::summarise(value = sum(value))

# Calculate additions by vintage
conv_reg_gas_tech_vintage %>%
  filter(vintage > 2015) %>% 
  mutate(additions = if_else(vintage == year, value, 0)) -> gas_vintage_add_reg

# Calculate retirements by vintage
conv_reg_gas_tech_vintage %>%
  filter(vintage >= 2015) %>% 
  group_by(scenario, region, technology, Units, vintage) %>%
  mutate(prev_year = lag(value, n = 1L)) %>%
  ungroup() %>% 
  mutate(prev_year = if_else(is.na(prev_year), 0, prev_year), 
         retirements = prev_year - value,
         retirements = if_else(retirements < 0, 0, retirements)) %>%
  arrange(vintage, technology, region) -> gas_vintage_ret_reg


    # "Expected Natural Retirements" ------------------------------------------

# Calculate s-curve output fraction
# parameters (from A_ff_TradedTechnology_NG.csv)
half.life <- 23
steepness <- 0.2
lifetime <- 45


conv_reg_gas_tech_vintage %>%
  # for base years only
  filter(vintage == 2015) %>%
  mutate(s_curve_frac = if_else(year > vintage,
                                (1 / (1 + exp( steepness * ((year - vintage) - half.life )))), 
                                1)) %>% 
  # Adjust s-curve output fraction to ensure that all of the capacity is retired at the end of lifetime
  mutate(s_curve_adj = if_else(year - vintage >= lifetime, 0, s_curve_frac),
         s_curve_adj = if_else(is.na(s_curve_adj), 1, s_curve_adj)) %>%
  select(scenario, region, technology,  vintage, Units, year, value, s_curve_adj) -> s_curve_frac_adj_reg

# Expected gas capacity assuming natural shutdowns only
# Create variable reflecting each tech/ vintage generation in year of installment (OG_gas_capacity)
s_curve_frac_adj_reg %>%
  left_join(conv_reg_gas_tech_vintage %>% 
              filter(vintage == year) %>%
              select(-year) %>%
              rename(OG_gas_capacity = value),
            by = c("scenario", "region", "technology", "vintage", "Units")) %>%
  mutate(gas_expect = OG_gas_capacity * s_curve_adj)  %>% 
  # Expected natural retirements
  group_by(scenario, region, technology, Units, vintage) %>%
  mutate(prev_yr_expect = lag(gas_expect, n = 1L), 
         natural_retire = if_else(year > vintage & prev_yr_expect > gas_expect, prev_yr_expect - gas_expect, 0)) %>% 
  ungroup() -> gas_retire_expect_reg


    # "Gross Additions and Retirements by Sector" -------------------------

# Total additions per region/ technology/ year (in EJ)
gas_vintage_add_reg %>% 
  group_by(scenario, region, technology, Units, year) %>%
  summarise(additions = sum(additions)) %>%
  ungroup() -> gas_total_add_reg

# Total retirements per region/ technology/ year (in EJ)
gas_vintage_ret_reg %>% 
  left_join(gas_retire_expect_reg %>% 
              select(scenario, technology, region, vintage, year, Units, natural_retire), 
            by = c("Units", "scenario", "technology", "region", "vintage",  "year")) %>% 
  # vintages > 2015 have no expected natural retirements - replace NA values with zero
  replace_na(list(natural_retire = 0)) %>% 
  # adjust retirements to account for expected natural retirements
  mutate(retirements = retirements - natural_retire,
         # make sure we don't have any negative values for retirement after adjustment
         retirements = if_else(retirements < 0, 0, retirements)) %>% 
  group_by(scenario, region, Units, technology, year) %>%
  summarise(retirements = sum(retirements)) %>%
  ungroup() -> gas_total_ret_reg


    # "Adjusted Additions and Retirements" -------------------------

# Merge total additions and retirements data tables
gas_total_add_reg %>%
  left_join(gas_total_ret_reg, by = c("scenario", "region", "technology", "Units", "year")) %>%
  mutate(add_adj = if_else(additions > retirements, additions - retirements, 0), 
         ret_adj = if_else(retirements > additions, retirements - additions, 0)) -> gas_add_ret_reg



    # Cumulative Additions and Retirements ------------------------------------

cum_gas_total_add_reg <- gas_add_ret_reg %>%
  group_by(scenario, region, technology, Units) %>%
  dplyr::mutate(cum_additions = cumsum(add_adj))

cum_gas_total_ret_reg <- gas_add_ret_reg %>%
  group_by(scenario, region, technology, Units) %>%
  dplyr::mutate(cum_retirements = cumsum(ret_adj))

cum_global_total_add_reg <- cum_gas_total_add_reg %>%
  group_by(scenario, technology, year) %>%
  dplyr::summarise(cum_additions = sum(cum_additions)) 

cum_global_total_ret_reg <- cum_gas_total_ret_reg %>%
  group_by(scenario, technology, year) %>%
  dplyr::summarise(cum_retirements = sum(cum_retirements))

cum_agg_region_total_add_reg <- cum_gas_total_add_reg %>%
  left_join(grouped_region_mapping, by = c("region")) %>%
  group_by(scenario, grouped_region, technology, year) %>%
  dplyr::summarise(cum_additions = sum(cum_additions)) 

cum_agg_region_total_ret_reg <- cum_gas_total_ret_reg %>%
  left_join(grouped_region_mapping, by = c("region")) %>%
  group_by(scenario, grouped_region, technology, year) %>%
  dplyr::summarise(cum_retirements = sum(cum_retirements))


  # PROCESS IPCC AR6 DATA -------------------------------------------------------


#Temperature data
AR6_temp <- AR6_DB_long %>%
  filter(Variable == "AR6 climate diagnostics|Surface Temperature (GSAT)|MAGICCv7.5.3|50.0th Percentile", year == 2100) %>%
  mutate(temp_category = case_when(value < 1.5 ~ "A_below 1.5C",
                                   value >= 1.5 & value < 2.0 ~ "B_1.5-2C",
                                   value >= 2.0 & value < 3.0 ~ "C_2-3C",
                                   value >= 3.0 & value < 4.0 ~ "D_3-4C",
                                   value >= 4.0 ~ "E_above 4C")) %>%
  select(Model, Scenario, temp_category)

AR6_temp$temp_category <- factor(AR6_temp$temp_category,
                                 levels = c("A_below 1.5C",
                                            "B_1.5-2C",
                                            "C_2-3C",
                                            "D_3-4C",
                                            "E_above 4C"))

#Global gas demand
AR6_pri_ene_gas <- AR6_DB_long %>%
  filter(Variable == "Primary Energy|Gas") %>%
  left_join(AR6_temp, by = c("Model", "Scenario")) %>%
  na.omit() %>%
  mutate(category = "AR6") %>%
  rename(Model_Scenario = Scenario,
         scenario = temp_category)

#Global primary energy
AR6_pri_ene <- AR6_DB_long %>%
  filter(Variable == "Primary Energy") %>%
  left_join(AR6_temp, by = c("Model", "Scenario")) %>%
  na.omit()

#Global gas in electricity
AR6_elec_gas <- AR6_DB_long %>%
  filter(Variable == "Secondary Energy|Electricity|Gas") %>%
  left_join(AR6_temp, by = c("Model", "Scenario")) %>%
  na.omit()

#FFI CO2 emissions
AR6_FFI_CO2_em <- AR6_DB_long %>%
  filter(Variable == "Emissions|CO2|Energy and Industrial Processes") %>%
  left_join(AR6_temp, by = c("Model", "Scenario")) %>%
  na.omit()  %>%
  mutate(category = "AR6") %>%
  rename(Model_Scenario = Scenario,
         scenario = temp_category)




  # GHG EMISSIONS -----------------------------------------------------------

GHG <- GHG_res_prod %>%
  select(-resource) %>%
  rename(sector = subresource) %>%
  bind_rows(GHG_sector) %>%
  rename(GHG = ghg) %>%
  left_join(GWP, by = c("GHG" = "ghg")) %>%
  mutate(Units = "MTCO2e", value = value * AR5all, SAR = NULL, AR5 = NULL,
         AR4 = NULL, SARall = NULL, AR5all = NULL, AR4all = NULL) %>%
  na.omit() %>%
  bind_rows(LUC_em)

grouped_region_GHG <- GHG %>%
  left_join(grouped_region_mapping, by = "region") %>%
  group_by(scenario, grouped_region, GHG, sector, year) %>%
  dplyr::summarise(value = sum(value))

global_GHG <- grouped_region_GHG %>%
  group_by(scenario, year) %>%
  dplyr::summarise(value = sum(value))

global_CO2 <- GHG %>%
  filter(GHG == "CO2") %>%
  group_by(scenario, year) %>%
  dplyr::summarise(value = sum(value))

global_nonCO2 <- GHG %>%
  filter(GHG != "CO2") %>%
  group_by(scenario, year) %>%
  dplyr::summarise(value = sum(value))

# GHG EMISSIONS BY AGGREGATED SECTOR --------------------------------------

global_CO2_res_prod <- GHG_res_prod %>%
  group_by(scenario, resource, year) %>%
  rename(GHG = ghg) %>%
  left_join(GWP, by = c("GHG" = "ghg")) %>%
  mutate(Units = "MTCO2e", value = value * AR5all, SAR = NULL, AR5 = NULL,
         AR4 = NULL, SARall = NULL, AR5all = NULL, AR4all = NULL) %>%
  na.omit()  %>%
  filter(GHG == "CO2") %>%
  dplyr::summarise(value = sum(value)) %>%
  rename(sector = resource)

global_CO2_sector <- GHG_sector %>%
  group_by(scenario, sector, year) %>%
  rename(GHG = ghg) %>%
  left_join(GWP, by = c("GHG" = "ghg")) %>%
  mutate(Units = "MTCO2e", value = value * AR5all, SAR = NULL, AR5 = NULL,
         AR4 = NULL, SARall = NULL, AR5all = NULL, AR4all = NULL) %>%
  na.omit()  %>%
  filter(GHG == "CO2") %>%
  dplyr::summarise(value = sum(value)) %>%
  bind_rows(LUC_em)

global_CO2_sector_res_prod <- bind_rows(global_CO2_res_prod,
                                        global_CO2_sector)

global_CO2_agg_sector_res_prod <- bind_rows(aggregate_rows(global_CO2_sector_res_prod, sector, "Buildings", bld_GHG_group, scenario, year),
                                            aggregate_rows(global_CO2_sector_res_prod, sector, "Electricity", elec_GHG_group, scenario, year),
                                            aggregate_rows(global_CO2_sector_res_prod, sector, "Transportation", trn_GHG_group, scenario, year),
                                            aggregate_rows(global_CO2_sector_res_prod, sector, "Industry", ind_GHG_group, scenario, year),
                                            aggregate_rows(global_CO2_sector_res_prod, sector, "Urban", urb_GHG_group, scenario, year),
                                            aggregate_rows(global_CO2_sector_res_prod, sector, "Agriculture and Land Use", c(aglu_GHG_group,"LUC"), scenario, year),
                                            aggregate_rows(global_CO2_sector_res_prod, sector, "Transformation", trans_GHG_group, scenario, year),
                                            aggregate_rows(global_CO2_sector_res_prod, sector, "Resource Production", res_prod_GHG_group, scenario, year))


global_GHG_res_prod <- GHG_res_prod %>%
  group_by(scenario, resource, year) %>%
  rename(GHG = ghg) %>%
  left_join(GWP, by = c("GHG" = "ghg")) %>%
  mutate(Units = "MTCO2e", value = value * AR5all, SAR = NULL, AR5 = NULL,
         AR4 = NULL, SARall = NULL, AR5all = NULL, AR4all = NULL) %>%
  na.omit()  %>%
  dplyr::summarise(value = sum(value)) %>%
  rename(sector = resource)

global_GHG_sector <- GHG_sector %>%
  group_by(scenario, sector, year) %>%
  rename(GHG = ghg) %>%
  left_join(GWP, by = c("GHG" = "ghg")) %>%
  mutate(Units = "MTCO2e", value = value * AR5all, SAR = NULL, AR5 = NULL,
         AR4 = NULL, SARall = NULL, AR5all = NULL, AR4all = NULL) %>%
  na.omit()  %>%
  dplyr::summarise(value = sum(value)) %>%
  bind_rows(LUC_em)

global_GHG_sector_res_prod <- bind_rows(global_GHG_res_prod,
                                        global_GHG_sector)

global_GHG_agg_sector_res_prod <- bind_rows(aggregate_rows(global_GHG_sector_res_prod, sector, "Buildings", bld_GHG_group, scenario, year),
                                            aggregate_rows(global_GHG_sector_res_prod, sector, "Electricity", elec_GHG_group, scenario, year),
                                            aggregate_rows(global_GHG_sector_res_prod, sector, "Transportation", trn_GHG_group, scenario, year),
                                            aggregate_rows(global_GHG_sector_res_prod, sector, "Industry", ind_GHG_group, scenario, year),
                                            aggregate_rows(global_GHG_sector_res_prod, sector, "Urban", urb_GHG_group, scenario, year),
                                            aggregate_rows(global_GHG_sector_res_prod, sector, "Agriculture and Land Use", c(aglu_GHG_group,"LUC"), scenario, year),
                                            aggregate_rows(global_GHG_sector_res_prod, sector, "Transformation", trans_GHG_group, scenario, year),
                                            aggregate_rows(global_GHG_sector_res_prod, sector, "Resource Production", res_prod_GHG_group, scenario, year))


# GAS GHG EMISSIONS BY AGGREGATED SECTOR ----------------------------------

gas_res_prod <- GHG_res_prod %>%
  filter(resource == "natural gas") %>%
  rename(sector = resource,
         subsector = subresource,
         GHG = ghg) %>%
  mutate(technology = subsector)

gas_GHG <- GHG_em_tech %>%
  rename(GHG = ghg) %>%
  filter(subsector %in% c("gas", "gas (CC)", "gas (steam/CT)", "gas pipeline", "gas to liquids") | technology =="NG" ) %>%
  bind_rows(gas_res_prod) %>%
  left_join(GWP, by = c("GHG" = "ghg")) %>%
  mutate(Units = "MTCO2e", value = value * AR5all, SAR = NULL, AR5 = NULL,
         AR4 = NULL, SARall = NULL, AR5all = NULL, AR4all = NULL) %>%
  na.omit() 

global_gas_GHG <- gas_GHG %>%
  group_by(scenario, sector, year) %>%
  dplyr::summarise(value = sum(value))

global_sector_gas_GHG <- bind_rows(aggregate_rows(global_gas_GHG, sector, "Buildings", bld_GHG_group, scenario, year),
                                   aggregate_rows(global_gas_GHG, sector, "Electricity", elec_GHG_group, scenario, year),
                                   aggregate_rows(global_gas_GHG, sector, "Transportation", trn_GHG_group, scenario, year),
                                   aggregate_rows(global_gas_GHG, sector, "Industry", ind_GHG_group, scenario, year),
                                   aggregate_rows(global_gas_GHG, sector, "Urban", urb_GHG_group, scenario, year),
                                   aggregate_rows(global_gas_GHG, sector, "Agriculture and Land Use", aglu_GHG_group, scenario, year),
                                   aggregate_rows(global_gas_GHG, sector, "Transformation", trans_GHG_group, scenario, year),
                                   aggregate_rows(global_gas_GHG, sector, "Resource Production", res_prod_GHG_group, scenario, year))

# FIGURES -----------------------------------------------------------------
  # 2A ------------------------------------------------------------------

total_traded_ng <- tra_gas_tech %>%
  group_by(scenario, region, year) %>%
  dplyr::summarise(value = sum(value))

global_total_traded_ng <- total_traded_ng %>%
  group_by(scenario, year) %>%
  dplyr::summarise(value = sum(value))


ggplot(data = filter(global_total_traded_ng, scenario %in% paper_scenarios, year >= 2015 & year <= 2050),
       aes(x = year, y = value, color = scenario, group = scenario, linetype = scenario))+
  geom_line(size = 1.5)+
  labs(title = "Global traded natural gas", x = "", y = "EJ") +
  scale_y_continuous(limits = c(0,200))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), text = element_text(size = 20))+
  theme(legend.position = "right", text = element_text(size = 20),
        legend.key.width = unit(2, "cm")) +
  scale_color_manual(values = c("Reference" = "#56b4e9",
                                "Reference_LLS" = "#56b4e9",
                                "Reference_LT_sshc" = "#56b4e9",
                                "Transition" = "#e69f00",
                                "Transition_LLS" = "#e69f00",
                                "Transition_LT_sshc" = "#e69f00"),
                     labels = c("Reference" = "Reference",
                                "Reference_LLS" = "Reference_LLS",
                                "Reference_LT_sshc" = "Reference_LT",
                                "Transition" = "Transition",
                                "Transition_LLS" = "Transition_LLS",
                                "Transition_LT_sshc" = "Transition_LT"))+
  scale_linetype_manual(values = c("Reference" = 1,
                                   "Reference_LLS" = 2,
                                   "Reference_LT_sshc" = 3,
                                   "Transition" = 1,
                                   "Transition_LLS" = 2,
                                   "Transition_LT_sshc" = 3),
                        labels = c("Reference" = "Reference",
                                   "Reference_LLS" = "Reference_LLS",
                                   "Reference_LT_sshc" = "Reference_LT",
                                   "Transition" = "Transition",
                                   "Transition_LLS" = "Transition_LLS",
                                   "Transition_LT_sshc" = "Transition_LT"))+
  ggsave(paste0(PLOT_FOLDER,"2A_global_export_NG.pdf", sep = ""),width=11, height=8.5, units="in")



  # 2B ------------------------------------------------------

domestic_ng <- reg_gas_tech %>%
  filter(subsector == "domestic natural gas", Units == "EJ") %>%
  group_by(scenario, region, sector, year) %>%
  dplyr::summarise(value = sum(value))

global_domestic_ng <- domestic_ng %>%
  group_by(scenario, year) %>%
  dplyr::summarise(value = sum(value))

ggplot(data = filter(global_domestic_ng, scenario %in% paper_scenarios, year >= 2015 & year <= 2050),
       aes(x = year, y = value, color = scenario, group = scenario, linetype = scenario))+
  geom_line(size = 1.5)+
  labs(title = "Global domestic natural gas", x = "", y = "EJ") +
  scale_y_continuous(limits = c(0,200))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), text = element_text(size = 20))+
  theme(legend.position = "right", text = element_text(size = 20),
        legend.key.width = unit(2, "cm")) +
  scale_color_manual(values = c("Reference" = "#56b4e9",
                                "Reference_LLS" = "#56b4e9",
                                "Reference_LT_sshc" = "#56b4e9",
                                "Transition" = "#e69f00",
                                "Transition_LLS" = "#e69f00",
                                "Transition_LT_sshc" = "#e69f00"),
                     labels = c("Reference" = "Reference",
                                "Reference_LLS" = "Reference_LLS",
                                "Reference_LT_sshc" = "Reference_LT",
                                "Transition" = "Transition",
                                "Transition_LLS" = "Transition_LLS",
                                "Transition_LT_sshc" = "Transition_LT"))+
  scale_linetype_manual(values = c("Reference" = 1,
                                   "Reference_LLS" = 2,
                                   "Reference_LT_sshc" = 3,
                                   "Transition" = 1,
                                   "Transition_LLS" = 2,
                                   "Transition_LT_sshc" = 3),
                        labels = c("Reference" = "Reference",
                                   "Reference_LLS" = "Reference_LLS",
                                   "Reference_LT_sshc" = "Reference_LT",
                                   "Transition" = "Transition",
                                   "Transition_LLS" = "Transition_LLS",
                                   "Transition_LT_sshc" = "Transition_LT"))+
  ggsave(paste0(PLOT_FOLDER,"2B_global_domestic_NG.pdf", sep = ""),width=11, height=8.5, units="in")


  # 2C -----------------------------------------------

global_traded_pipeline <- tra_gas_tech %>%
  filter(sector %in% c("traded Afr_MidE pipeline gas", "traded LA pipeline gas", "traded PAC pipeline gas", "traded N.Amer pipeline gas",
                       "traded RUS pipeline gas", "traded EUR pipeline gas")) %>%
  group_by(scenario, sector, year) %>%
  dplyr::summarise(value = sum(value))

global_total_traded_pipeline <- global_traded_pipeline %>%
  group_by(scenario, year) %>%
  dplyr::summarise(value = sum(value))

ggplot(data = filter(global_total_traded_pipeline, scenario %in% paper_scenarios, year >= 2015 & year <= 2050),
       aes(x = year, y = value, color = scenario, group = scenario, linetype = scenario))+
  geom_line(size = 1.5)+
  labs(title = "Global traded pipeline gas", x = "", y = "EJ") +
  scale_y_continuous(limits = c(0,70))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), text = element_text(size = 20))+
  theme(legend.position = "right", text = element_text(size = 20),
        legend.key.width = unit(2, "cm")) +
  scale_color_manual(values = c("Reference" = "#56b4e9",
                                "Reference_LLS" = "#56b4e9",
                                "Reference_LT_sshc" = "#56b4e9",
                                "Transition" = "#e69f00",
                                "Transition_LLS" = "#e69f00",
                                "Transition_LT_sshc" = "#e69f00"),
                     labels = c("Reference" = "Reference",
                                "Reference_LLS" = "Reference_LLS",
                                "Reference_LT_sshc" = "Reference_LT",
                                "Transition" = "Transition",
                                "Transition_LLS" = "Transition_LLS",
                                "Transition_LT_sshc" = "Transition_LT"))+
  scale_linetype_manual(values = c("Reference" = 1,
                                   "Reference_LLS" = 2,
                                   "Reference_LT_sshc" = 3,
                                   "Transition" = 1,
                                   "Transition_LLS" = 2,
                                   "Transition_LT_sshc" = 3),
                        labels = c("Reference" = "Reference",
                                   "Reference_LLS" = "Reference_LLS",
                                   "Reference_LT_sshc" = "Reference_LT",
                                   "Transition" = "Transition",
                                   "Transition_LLS" = "Transition_LLS",
                                   "Transition_LT_sshc" = "Transition_LT"))+
  ggsave(paste0(PLOT_FOLDER,"2C_global_traded_pipeline_NG.pdf", sep = ""),width=11, height=8.5, units="in")

  # 2D -------------------------------------------------------

global_traded_LNG <- tra_gas_tech %>%
  filter(sector %in% c("traded LNG")) %>%
  group_by(scenario, sector, year) %>%
  dplyr::summarise(value = sum(value))

ggplot(data = filter(global_traded_LNG, scenario %in% paper_scenarios, year >= 2015 & year <= 2050),
       aes(x = year, y = value, color = scenario, group = scenario, linetype = scenario))+
  geom_line(size = 1.5)+
  labs(title = "Global traded LNG", x = "", y = "EJ") +
  scale_y_continuous(limits = c(0,70))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), text = element_text(size = 20))+
  theme(legend.position = "right", text = element_text(size = 20),
        legend.key.width = unit(2, "cm")) +
  scale_color_manual(values = c("Reference" = "#56b4e9",
                                "Reference_LLS" = "#56b4e9",
                                "Reference_LT_sshc" = "#56b4e9",
                                "Transition" = "#e69f00",
                                "Transition_LLS" = "#e69f00",
                                "Transition_LT_sshc" = "#e69f00"),
                     labels = c("Reference" = "Reference",
                                "Reference_LLS" = "Reference_LLS",
                                "Reference_LT_sshc" = "Reference_LT",
                                "Transition" = "Transition",
                                "Transition_LLS" = "Transition_LLS",
                                "Transition_LT_sshc" = "Transition_LT"))+
  scale_linetype_manual(values = c("Reference" = 1,
                                   "Reference_LLS" = 2,
                                   "Reference_LT_sshc" = 3,
                                   "Transition" = 1,
                                   "Transition_LLS" = 2,
                                   "Transition_LT_sshc" = 3),
                        labels = c("Reference" = "Reference",
                                   "Reference_LLS" = "Reference_LLS",
                                   "Reference_LT_sshc" = "Reference_LT",
                                   "Transition" = "Transition",
                                   "Transition_LLS" = "Transition_LLS",
                                   "Transition_LT_sshc" = "Transition_LT"))+
  ggsave(paste0(PLOT_FOLDER,"2D_global_traded_LNG.pdf", sep = ""),width=11, height=8.5, units="in")




  # 3A ----------------------------------------------------------------------

ggplot(data = filter(grouped_production_ng, (year == 2050 | (year == 2015 & scenario == "Reference")),
                     scenario %in% c("Reference", "Transition")),
       aes(x = grouped_region, y = value, fill = sector, group = sector))+
  geom_bar(position = position_stack(reverse = TRUE), stat = "identity")+
  stat_identity(yintercept=0, geom='hline', inherit.aes=TRUE, size = 1)+
  labs(title = "", x = "", y = "Gas production (EJ)") +
  facet_wrap(~interaction(scenario, year), nrow = 1, scales = "fixed", labeller = as_labeller(c("Reference.2015" = "2015",
                                                                                                "Reference.2050" = "2050 Reference",
                                                                                                "Transition.2050" = "2050 Transition"))) +
  theme_bw()+
  theme(text = element_text(size = 12))+
  theme(legend.position = "right", text = element_text(size = 12)) +
  scale_fill_manual(values = c(
    "domestic" = "gray40",
    "LNG" = "gray70",
    "Afr_MidE" = "#E69F00",
    "EUR" = "#56B4E9",
    "LA" = "#009E73",
    "N.Amer" = "#F0E442",
    "PAC" = "#0072B2",
    "RUS" = "#D55E00"),
    labels = c("domestic" = "domestic",
               "LNG" = "LNG",
               "Afr_MidE" = "Afr_MidE pipeline",
               "EUR" = "EUR pipeline",
               "LA" = "LA pipeline",
               "N.Amer" = "N.Amer pipeline",
               "PAC" = "PAC pipeline",
               "RUS" = "RUS pipeline"))+
  coord_flip()+
  ggsave(paste0(PLOT_FOLDER,"3A_region_gross_production_NG_2050.pdf", sep = ""),width=9, height=3, units="in")

  # 3B ----------------------------------------------------------------------

ggplot(data = filter(grouped_trade_ng, (year == 2050 | (year == 2015 & scenario == "Reference")),
                     scenario %in% c("Reference", "Transition")),
       aes(x = grouped_region, y = value, fill = sector, group = sector))+
  geom_bar(position = position_stack(reverse = TRUE), stat = "identity")+
  stat_identity(yintercept=0, geom='hline', inherit.aes=TRUE, size = 1)+
  geom_errorbar(data = filter(grouped_trade_ng, (year == 2050 | (year == 2015 & scenario == "Reference")),
                              scenario %in% c("Reference","Transition")),
                aes(x = grouped_region, ymin = value.net, ymax = value.net), lty = "32", width = 1, size = 0.4)+
  stat_identity(yintercept=0, geom='hline', inherit.aes=TRUE, size = 1, color = "black")+
  labs(title = "", x = "", y = "Gas trade (EJ)") +
  facet_wrap(~interaction(scenario, year), nrow = 1, scales = "fixed", labeller = as_labeller(c("Reference.2015" = "2015",
                                                                                                "Reference.2050" = "2050 Reference",
                                                                                                "Transition.2050" = "2050 Transition"))) +
  theme_bw()+
  theme(text = element_text(size = 12))+
  theme(legend.position = "right", text = element_text(size = 12)) +
  scale_fill_manual(values = c(
    "LNG" = "gray70",
    "Afr_MidE" = "#E69F00",
    "EUR" = "#56B4E9",
    "LA" = "#009E73",
    "N.Amer" = "#F0E442",
    "PAC" = "#0072B2",
    "RUS" = "#D55E00"),
    labels = c("domestic" = "domestic",
               "LNG" = "LNG",
               "Afr_MidE" = "Afr_MidE pipeline",
               "EUR" = "EUR pipeline",
               "LA" = "LA pipeline",
               "N.Amer" = "N.Amer pipeline",
               "PAC" = "PAC pipeline",
               "RUS" = "RUS pipeline"))+
  scale_y_continuous(limits = c(-30, 30))+
  coord_flip()+
  ggsave(paste0(PLOT_FOLDER,"3B_region_gross_trade_NG_2050.pdf", sep = ""),width=9, height=3, units="in")


  # 3C ----------------------------------------------------------------------

ggplot(data = filter(grouped_consumption_ng, (year == 2050 | (year == 2015 & scenario == "Reference")),
                     scenario %in% c("Reference", "Transition")),
       aes(x = grouped_region, y = value*-1, fill = sector, group = sector))+
  geom_bar(position = position_stack(reverse = TRUE), stat = "identity")+
  stat_identity(yintercept=0, geom='hline', inherit.aes=TRUE, size = 1)+
  labs(title = "", x = "", y = "Gas consumption (EJ)") +
  facet_wrap(~interaction(scenario, year), nrow = 1, scales = "fixed", labeller = as_labeller(c("Reference.2015" = "2015",
                                                                                                "Reference.2050" = "2050 Reference",
                                                                                                "Transition.2050" = "2050 Transition"))) +
  theme_bw()+
  theme(text = element_text(size = 12))+
  theme(legend.position = "right", text = element_text(size = 12)) +
  scale_fill_manual(values = c(
    "domestic" = "gray40",
    "LNG" = "gray70",
    "Afr_MidE" = "#E69F00",
    "EUR" = "#56B4E9",
    "LA" = "#009E73",
    "N.Amer" = "#F0E442",
    "PAC" = "#0072B2",
    "RUS" = "#D55E00"),
    labels = c("domestic" = "domestic",
               "LNG" = "LNG",
               "Afr_MidE" = "Afr_MidE pipeline",
               "EUR" = "EUR pipeline",
               "LA" = "LA pipeline",
               "N.Amer" = "N.Amer pipeline",
               "PAC" = "PAC pipeline",
               "RUS" = "RUS pipeline"))+
  
  coord_flip()+
  ggsave(paste0(PLOT_FOLDER,"3C_region_gross_consumption_NG_2050_multi.pdf", sep = ""),width=9, height=3, units="in")


  # 4A -------------------------------------------------------------------
  
  diff_grouped_production_ng_LLS_plot <- bind_rows(diff_grouped_production_ng_REF_sens_plot,
                                                   diff_grouped_production_ng_TRANS_sens_plot) %>%
    filter(scenario.diff %in% c("Reference_LLS","Transition_LLS"))

ggplot(data = filter(diff_grouped_production_ng_LLS_plot, year == 2050),
       aes(x = grouped_region, y = value, fill = sector, group = sector))+
  geom_bar(position = position_stack(reverse = TRUE), stat = "identity")+
  geom_errorbar(data = filter(diff_grouped_production_ng_LLS_plot, year == 2050),
                aes(x = grouped_region, ymin = value.net, ymax = value.net), lty = "32", width = 1, size = 0.4)+
  stat_identity(yintercept=0, geom='hline', inherit.aes=TRUE, size = 1)+
  scale_y_continuous(limits = c(-2,2))+
  labs(title = "", x = "", y = " Gas production (EJ)") +
  facet_wrap(~scenario.diff, nrow = 1, scales = "fixed", labeller = as_labeller(c("Reference_LLS" = "2050 Reference_LLS - Reference",
                                                                                  "Transition_LLS" = "2050 Transition_LLS - Transition"))) +
  theme_bw()+
  theme(text = element_text(size = 12))+
  theme(legend.position = "right", text = element_text(size = 12)) +
  scale_fill_manual(values = c(
    "domestic" = "gray40",
    "LNG" = "gray70",
    "Afr_MidE" = "#E69F00",
    "EUR" = "#56B4E9",
    "LA" = "#009E73",
    "N.Amer" = "#F0E442",
    "PAC" = "#0072B2",
    "RUS" = "#D55E00"))+
  coord_flip()+
  ggsave(paste0(PLOT_FOLDER,"4A_region_gross_production_NG_2050_diff_LLS.pdf", sep = ""),width=8, height=3.5, units="in")

  
  # 4B ----------------------------------------------------------------------
  
  
  diff_grouped_consumption_ng_LLS_plot <- bind_rows(diff_grouped_consumption_ng_REF_sens_plot,
                                                    diff_grouped_consumption_ng_TRANS_sens_plot) %>%
    filter(scenario.diff %in% c("Reference_LLS","Transition_LLS"))
  
ggplot(data = filter(diff_grouped_consumption_ng_LLS_plot, year == 2050),
       aes(x = grouped_region, y = value*-1, fill = sector, group = sector))+
  geom_bar(position = position_stack(reverse = TRUE), stat = "identity")+
  geom_errorbar(data = filter(diff_grouped_consumption_ng_LLS_plot, year == 2050),
                aes(x = grouped_region, ymin = value.net*-1, ymax = value.net*-1), lty = "32", width = 1, size = 0.4)+
  stat_identity(yintercept=0, geom='hline', inherit.aes=TRUE, size = 1)+
  scale_y_continuous(limits = c(-2,2))+
  labs(title = "", x = "", y = " Gas consumption (EJ)") +
  facet_wrap(~scenario.diff, nrow = 1, scales = "fixed", labeller = as_labeller(c("Reference_LLS" = "2050 Reference_LLS - Reference",
                                                                                  "Transition_LLS" = "2050 Transition_LLS - Transition"))) +
  theme_bw()+
  theme(text = element_text(size = 12))+
  theme(legend.position = "right", text = element_text(size = 12)) +
  scale_fill_manual(values = c(
    "domestic" = "gray40",
    "LNG" = "gray70",
    "Afr_MidE" = "#E69F00",
    "EUR" = "#56B4E9",
    "LA" = "#009E73",
    "N.Amer" = "#F0E442",
    "PAC" = "#0072B2",
    "RUS" = "#D55E00"))+
  coord_flip()+
  ggsave(paste0(PLOT_FOLDER,"4B_region_gross_consumption_NG_2050_diff_LLS.pdf", sep = ""),width=8, height=3.5, units="in")

  
  # 4C ----------------------------------------------------------------------

diff_grouped_region_pri_ene_CCS_LLS <- diff_grouped_region_pri_ene_CCS %>%
  filter(scenario.diff %in% c("Reference_LLS", "Transition_LLS"))

ggplot(data = filter(diff_grouped_region_pri_ene_CCS_LLS, year == 2050, scenario.diff %in% paper_scenarios),
       aes(x = grouped_region, y = value, fill = fuel, group = fuel))+
  geom_bar(position = position_stack(reverse = TRUE), stat = "identity")+
  geom_errorbar(data = filter(diff_grouped_region_pri_ene_CCS_LLS, year == 2050),
                aes(x = grouped_region, ymin = value.net, ymax = value.net), lty = "32", width = 1, size = 0.4)+
  stat_identity(yintercept=0, geom='hline', inherit.aes=TRUE, size = 1, color = "black")+
  labs(title = "", x = "", y = " Primary energy consumption (EJ)") +
  facet_wrap(~scenario.diff, nrow = 1, scales = "fixed", labeller = as_labeller(c("Reference_LLS" = "2050 Reference_LLS - Reference",
                                                                                  "Transition_LLS" = "2050 Transition_LLS - Transition"))) +
  theme_bw()+
  theme(text = element_text(size = 12))+
  theme(legend.position = "right", text = element_text(size = 12)) +
  scale_fill_manual(values=jgcricol()$pal_all[names(jgcricol()$pal_all) %in% pri_ene_CCS_fuels])+
  coord_flip()+
  ggsave(paste0(PLOT_FOLDER,"4C_diff_region_pri_ene_CCS_2050_LLS.pdf", sep = ""),width=8.8, height=3.5, units="in")

  # 4D ----------------------------------------------------------------------
  
  
  diff_grouped_production_ng_LT_plot <- bind_rows(diff_grouped_production_ng_REF_sens_plot,
                                                  diff_grouped_production_ng_TRANS_sens_plot) %>%
    filter(scenario.diff %in% c("Reference_LT_sshc", "Transition_LT_sshc"))
  
ggplot(data = filter(diff_grouped_production_ng_LT_plot, year == 2050),
       aes(x = grouped_region, y = value, fill = sector, group = sector))+
  geom_bar(position = position_stack(reverse = TRUE), stat = "identity")+
  geom_errorbar(data = filter(diff_grouped_production_ng_LT_plot, year == 2050),
                aes(x = grouped_region, ymin = value.net, ymax = value.net), lty = "32", width = 1, size = 0.4)+
  stat_identity(yintercept=0, geom='hline', inherit.aes=TRUE, size = 1)+
  scale_y_continuous(limits = c(-20,20))+
  labs(title = "", x = "", y = " Gas production (EJ)") +
  facet_wrap(~scenario.diff, nrow = 1, scales = "fixed", labeller = as_labeller(c("Reference_LT_sshc" = "2050 Reference_LT - Reference",
                                                                                  "Transition_LT_sshc" = "2050 Transition_LT - Transition"))) +
  theme_bw()+
  theme(text = element_text(size = 12))+
  theme(legend.position = "right", text = element_text(size = 12)) +
  scale_fill_manual(values = c(
    "domestic" = "gray40",
    "LNG" = "gray70",
    "Afr_MidE" = "#E69F00",
    "EUR" = "#56B4E9",
    "LA" = "#009E73",
    "N.Amer" = "#F0E442",
    "PAC" = "#0072B2",
    "RUS" = "#D55E00"))+
  coord_flip()+
  ggsave(paste0(PLOT_FOLDER,"4D_region_gross_production_NG_2050_diff_LT.pdf", sep = ""),width=8, height=3.5, units="in")

  
  # 4E ----------------------------------------------------------------------
  
  
  diff_grouped_consumption_ng_LT_plot <- bind_rows(diff_grouped_consumption_ng_REF_sens_plot,
                                                   diff_grouped_consumption_ng_TRANS_sens_plot) %>%
    filter(scenario.diff %in% c( "Reference_LT_sshc", "Transition_LT_sshc"))
  
ggplot(data = filter(diff_grouped_consumption_ng_LT_plot, year == 2050),
       aes(x = grouped_region, y = value*-1, fill = sector, group = sector))+
  geom_bar(position = position_stack(reverse = TRUE), stat = "identity")+
  geom_errorbar(data = filter(diff_grouped_consumption_ng_LT_plot, year == 2050),
                aes(x = grouped_region, ymin = value.net*-1, ymax = value.net*-1), lty = "32", width = 1, size = 0.4)+
  stat_identity(yintercept=0, geom='hline', inherit.aes=TRUE, size = 1)+
  scale_y_continuous(limits = c(-20,20))+
  labs(title = "", x = "", y = " Gas consumption (EJ)") +
  facet_wrap(~scenario.diff, nrow = 1, scales = "fixed", labeller = as_labeller(c("Reference_LT_sshc" = "2050 Reference_LT - Reference",
                                                                                  "Transition_LT_sshc" = "2050 Transition_LT - Transition"))) +
  theme_bw()+
  theme(text = element_text(size = 12))+
  theme(legend.position = "right", text = element_text(size = 12)) +
  scale_fill_manual(values = c(
    "domestic" = "gray40",
    "LNG" = "gray70",
    "Afr_MidE" = "#E69F00",
    "EUR" = "#56B4E9",
    "LA" = "#009E73",
    "N.Amer" = "#F0E442",
    "PAC" = "#0072B2",
    "RUS" = "#D55E00"))+
  coord_flip()+
  ggsave(paste0(PLOT_FOLDER,"4E_region_gross_consumption_NG_2050_diff_LT.pdf", sep = ""),width=8, height=3.5, units="in")

  
  # 4F ----------------------------------------------------------------------

diff_grouped_region_pri_ene_CCS_LT <- diff_grouped_region_pri_ene_CCS %>%
  filter(scenario.diff %in% c("Reference_LT_sshc", "Transition_LT_sshc"))

ggplot(data = filter(diff_grouped_region_pri_ene_CCS_LT, year == 2050, scenario.diff %in% paper_scenarios),
       aes(x = grouped_region, y = value, fill = fuel, group = fuel))+
  geom_bar(position = position_stack(reverse = TRUE), stat = "identity")+
  geom_errorbar(data = filter(diff_grouped_region_pri_ene_CCS_LT, year == 2050),
                aes(x = grouped_region, ymin = value.net, ymax = value.net), lty = "32", width = 1, size = 0.4)+
  stat_identity(yintercept=0, geom='hline', inherit.aes=TRUE, size = 1, color = "black")+
  labs(title = "", x = "", y = " Primary energy consumption (EJ)") +
  facet_wrap(~scenario.diff, nrow = 1, scales = "fixed", labeller = as_labeller(c("Reference_LT_sshc" = "2050 Reference_LT - Reference",
                                                                                  "Transition_LT_sshc" = "2050 Transition_LT - Transition"))) +
  theme_bw()+
  theme(text = element_text(size = 12))+
  theme(legend.position = "right", text = element_text(size = 12)) +
  scale_fill_manual(values=jgcricol()$pal_all[names(jgcricol()$pal_all) %in% pri_ene_CCS_fuels])+
  coord_flip()+
  ggsave(paste0(PLOT_FOLDER,"4F_diff_region_pri_ene_CCS_2050_LT.pdf", sep = ""),width=8.8, height=3.5, units="in")

  
  # 5A ----------------------------------------------------------------------

cum_global_total_add$sector <- factor(cum_global_total_add$sector,
                                      levels = c("traded LNG",
                                                 "traded Afr_MidE pipeline gas",
                                                 "traded EUR pipeline gas",
                                                 "traded LA pipeline gas",
                                                 "traded N.Amer pipeline gas",
                                                 "traded PAC pipeline gas",
                                                 "traded RUS pipeline gas"))

cum_global_total_add$scenario <- factor(cum_global_total_add$scenario,
                                        levels = c(
                                          "Transition_LT_sshc",
                                          "Transition_LLS",
                                          "Transition",
                                          "Reference_LT_sshc",
                                          "Reference_LLS",
                                          "Reference"))

ggplot(data = filter(cum_global_total_add, scenario %in% paper_scenarios, year == 2050),
       aes(x = scenario, y = cum_additions, fill = sector, group = sector))+
  geom_bar(position = position_stack(reverse = TRUE), stat = "identity")+
  stat_identity(yintercept=0, geom='hline', inherit.aes=TRUE, size = 1)+
  labs(title = "2050 cumulative pipeline and LNG additions", x = "", y = "MTPA") +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), text = element_text(size = 12))+
  theme(legend.position = "right", text = element_text(size = 12)) +
  scale_y_continuous(limits = c(0, 1700))+
  scale_fill_manual(values = c(
    "traded LNG" = "gray70",
    "traded Afr_MidE pipeline gas" = "#E69F00",
    "traded EUR pipeline gas" = "#56B4E9",
    "traded LA pipeline gas" = "#009E73",
    "traded N.Amer pipeline gas" = "#F0E442",
    "traded PAC pipeline gas" = "#0072B2",
    "traded RUS pipeline gas" = "#D55E00"))+
  scale_x_discrete(labels = paper_scenario_labels)+
  coord_flip()+
  ggsave(paste0(PLOT_FOLDER,"5A_cum_global_total_add_facet_export_2050.pdf", sep = ""),width=6, height=3, units="in")



  # 5B ----------------------------------------------------------------------

cum_global_total_ret$sector <- factor(cum_global_total_ret$sector,
                                      levels = c("traded LNG",
                                                 "traded Afr_MidE pipeline gas",
                                                 "traded EUR pipeline gas",
                                                 "traded LA pipeline gas",
                                                 "traded N.Amer pipeline gas",
                                                 "traded PAC pipeline gas",
                                                 "traded RUS pipeline gas"))

cum_global_total_ret$scenario <- factor(cum_global_total_ret$scenario,
                                        levels = c(
                                          "Transition_LT_sshc",
                                          "Transition_LLS",
                                          "Transition",
                                          "Reference_LT_sshc",
                                          "Reference_LLS",
                                          "Reference"))

ggplot(data = filter(cum_global_total_ret, scenario %in% paper_scenarios, year == 2050),
       aes(x = scenario, y = cum_retirements, fill = sector, group = sector))+
  geom_bar(position = position_stack(reverse = TRUE), stat = "identity")+
  stat_identity(yintercept=0, geom='hline', inherit.aes=TRUE, size = 1)+
  labs(title = "2050 cumulative pipeline and LNG underutilization", x = "", y = "MTPA") +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), text = element_text(size = 12))+
  theme(legend.position = "right", text = element_text(size = 12)) +
  scale_y_continuous(limits = c(0,170))+
  scale_fill_manual(values = c(
    "traded LNG" = "gray70",
    "traded Afr_MidE pipeline gas" = "#E69F00",
    "traded EUR pipeline gas" = "#56B4E9",
    "traded LA pipeline gas" = "#009E73",
    "traded N.Amer pipeline gas" = "#F0E442",
    "traded PAC pipeline gas" = "#0072B2",
    "traded RUS pipeline gas" = "#D55E00"))+
  scale_x_discrete(labels = paper_scenario_labels)+
  coord_flip()+
  ggsave(paste0(PLOT_FOLDER,"5B_cum_global_total_ret_facet_export_2050.pdf", sep = ""),width=6, height=3, units="in")


# SI FIGURES --------------------------------------------------------------
  # S2A ----------------------------------------------------------------------
cum_agg_region_total_add$grouped_region <- factor(cum_agg_region_total_add$grouped_region , 
                                                  levels  = grouped_region_levels)


cum_agg_region_total_add$sector <- factor(cum_agg_region_total_add$sector,
                                          levels = c("traded LNG",
                                                     "traded Afr_MidE pipeline gas",
                                                     "traded EUR pipeline gas",
                                                     "traded LA pipeline gas",
                                                     "traded N.Amer pipeline gas",
                                                     "traded PAC pipeline gas",
                                                     "traded RUS pipeline gas"))

ggplot(data = filter(cum_agg_region_total_add, scenario %in% paper_scenarios, year %in% c(2050)),
       aes(x = grouped_region, y = cum_additions, fill = sector, group = sector))+
  geom_bar(position = position_stack(reverse = TRUE), stat = "identity")+
  stat_identity(yintercept=0, geom='hline', inherit.aes=TRUE, size = 1)+
  facet_wrap(~scenario, nrow = 2, scales = "fixed", labeller = as_labeller(paper_scenario_labels))+
  labs(title = "Cumulative pipeline and LNG additions in 2050", x = "Region", y = "MTPA") +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), text = element_text(size = 12))+
  theme(legend.position = "right", text = element_text(size = 12)) +
  scale_x_discrete(labels = paper_scenario_labels)+
  scale_fill_manual(values = c(
    "traded LNG" = "gray70",
    "traded Afr_MidE pipeline gas" = "#E69F00",
    "traded EUR pipeline gas" = "#56B4E9",
    "traded LA pipeline gas" = "#009E73",
    "traded N.Amer pipeline gas" = "#F0E442",
    "traded PAC pipeline gas" = "#0072B2",
    "traded RUS pipeline gas" = "#D55E00"))+
  ggsave(paste0(PLOT_FOLDER,"S2A_cum_region_total_add_facet_2050_export.png", sep = ""),width=11, height=8, units="in")


  # S2B ---------------------------------------------------------------------

cum_agg_region_total_ret$sector <- factor(cum_agg_region_total_ret$sector,
                                          levels = c("traded LNG",
                                                     "traded Afr_MidE pipeline gas",
                                                     "traded EUR pipeline gas",
                                                     "traded LA pipeline gas",
                                                     "traded N.Amer pipeline gas",
                                                     "traded PAC pipeline gas",
                                                     "traded RUS pipeline gas"))


cum_agg_region_total_ret$grouped_region <- factor(cum_agg_region_total_ret$grouped_region , 
                                                  levels  = grouped_region_levels)
ggplot(data = filter(cum_agg_region_total_ret, scenario %in% paper_scenarios, year %in% c(2050)),
       aes(x = grouped_region, y = cum_retirements, fill = sector, group = sector))+
  geom_bar(position = position_stack(reverse = TRUE), stat = "identity")+
  stat_identity(yintercept=0, geom='hline', inherit.aes=TRUE, size = 1)+
  facet_wrap(~scenario, nrow = 2, scales = "fixed", labeller = as_labeller(paper_scenario_labels))+
  labs(title = "Cumulative pipeline and LNG underutilization in 2050", x = "Region", y = "MTPA") +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), text = element_text(size = 12))+
  theme(legend.position = "right", text = element_text(size = 12)) +
  scale_fill_manual(values = c(
    "traded LNG" = "gray70",
    "traded Afr_MidE pipeline gas" = "#E69F00",
    "traded EUR pipeline gas" = "#56B4E9",
    "traded LA pipeline gas" = "#009E73",
    "traded N.Amer pipeline gas" = "#F0E442",
    "traded PAC pipeline gas" = "#0072B2",
    "traded RUS pipeline gas" = "#D55E00"))+
  ggsave(paste0(PLOT_FOLDER,"S2B_cum_region_total_ret_facet_2050_export.png", sep = ""),width=11, height=8, units="in")




  # S3A ----------------------------------------------------------------------

cum_global_total_add_reg$technology <- factor(cum_global_total_add_reg$technology,
                                              levels = c("domestic natural gas",
                                                         "imported LNG",
                                                         "imported Afr_MidE pipeline gas",
                                                         "imported EUR pipeline gas",
                                                         "imported LA pipeline gas",
                                                         "imported N.Amer pipeline gas",
                                                         "imported PAC pipeline gas",
                                                         "imported RUS pipeline gas"))

cum_global_total_add_reg$scenario <- factor(cum_global_total_add_reg$scenario,
                                            levels = c(
                                              "Transition_LT_sshc",
                                              "Transition_LLS",
                                              "Transition",
                                              "Reference_LT_sshc",
                                              "Reference_LLS",
                                              "Reference"))

ggplot(data = filter(cum_global_total_add_reg, scenario %in% paper_scenarios, year == 2050,
                     technology != "domestic natural gas"),
       aes(x = scenario, y = cum_additions, fill = technology, group = technology))+
  geom_bar(position = position_stack(reverse = TRUE), stat = "identity")+
  stat_identity(yintercept=0, geom='hline', inherit.aes=TRUE, size = 1)+
  labs(title = "Cumulative pipeline and LNG additions in 2050", x = "", y = "MTPA") +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), text = element_text(size = 12))+
  theme(legend.position = "right", text = element_text(size = 12)) +
  scale_x_discrete(labels = paper_scenario_labels)+
  scale_fill_manual(values = c(
    "imported LNG" = "gray70",
    "imported Afr_MidE pipeline gas" = "#E69F00",
    "imported EUR pipeline gas" = "#56B4E9",
    "imported LA pipeline gas" = "#009E73",
    "imported N.Amer pipeline gas" = "#F0E442",
    "imported PAC pipeline gas" = "#0072B2",
    "imported RUS pipeline gas" = "#D55E00"))+
  coord_flip()+
  ggsave(paste0(PLOT_FOLDER,"S3A_cum_global_total_add_facet_import_2050.pdf", sep = ""),width=6, height=3, units="in")



  # S3B ---------------------------------------------------------------------

cum_global_total_ret_reg$technology <- factor(cum_global_total_ret_reg$technology,
                                              levels = c("domestic natural gas",
                                                         "imported LNG",
                                                         "imported Afr_MidE pipeline gas",
                                                         "imported EUR pipeline gas",
                                                         "imported LA pipeline gas",
                                                         "imported N.Amer pipeline gas",
                                                         "imported PAC pipeline gas",
                                                         "imported RUS pipeline gas"))

cum_global_total_ret_reg$scenario <- factor(cum_global_total_ret_reg$scenario,
                                            levels = c(
                                              "Transition_LT_sshc",
                                              "Transition_LLS",
                                              "Transition",
                                              "Reference_LT_sshc",
                                              "Reference_LLS",
                                              "Reference"))

ggplot(data = filter(cum_global_total_ret_reg, scenario %in% paper_scenarios, year == 2050,
                     technology != "domestic natural gas"),
       aes(x = scenario, y = cum_retirements, fill = technology, group = technology))+
  geom_bar(position = position_stack(reverse = TRUE), stat = "identity")+
  stat_identity(yintercept=0, geom='hline', inherit.aes=TRUE, size = 1)+
  labs(title = "Cumulative pipeline and LNG underutilization in 2050", x = "", y = "MTPA") +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), text = element_text(size = 12))+
  theme(legend.position = "right", text = element_text(size = 12)) +
  scale_x_discrete(labels = paper_scenario_labels)+
  scale_fill_manual(values = c(
    "imported LNG" = "gray70",
    "imported Afr_MidE pipeline gas" = "#E69F00",
    "imported EUR pipeline gas" = "#56B4E9",
    "imported LA pipeline gas" = "#009E73",
    "imported N.Amer pipeline gas" = "#F0E442",
    "imported PAC pipeline gas" = "#0072B2",
    "imported RUS pipeline gas" = "#D55E00"))+
  coord_flip()+
  ggsave(paste0(PLOT_FOLDER,"S3B_cum_global_total_ret_facet_import_2050.pdf", sep = ""),width=6, height=3, units="in")


  # S4A ---------------------------------------------------------------------

cum_agg_region_total_add_reg$grouped_region <- factor(cum_agg_region_total_add_reg$grouped_region , 
                                                      levels  = grouped_region_levels)


cum_agg_region_total_add_reg$technology <- factor(cum_agg_region_total_add_reg$technology,
                                                  levels = c("domestic natural gas",
                                                             "imported LNG",
                                                             "imported Afr_MidE pipeline gas",
                                                             "imported EUR pipeline gas",
                                                             "imported LA pipeline gas",
                                                             "imported N.Amer pipeline gas",
                                                             "imported PAC pipeline gas",
                                                             "imported RUS pipeline gas"))

ggplot(data = filter(cum_agg_region_total_add_reg, scenario %in% paper_scenarios, year %in% c(2050),
                     technology != "domestic natural gas"),
       aes(x = grouped_region, y = cum_additions, fill = technology, group = technology))+
  geom_bar(position = position_stack(reverse = TRUE), stat = "identity")+
  stat_identity(yintercept=0, geom='hline', inherit.aes=TRUE, size = 1)+
  facet_wrap(~scenario, nrow = 2, scales = "fixed", labeller = as_labeller(paper_scenario_labels))+
  labs(title = "Cumulative pipeline and LNG additions in 2050", x = "Region", y = "MTPA") +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), text = element_text(size = 12))+
  theme(legend.position = "right", text = element_text(size = 12)) +
  scale_fill_manual(values = c(
    "imported LNG" = "gray70",
    "imported Afr_MidE pipeline gas" = "#E69F00",
    "imported EUR pipeline gas" = "#56B4E9",
    "imported LA pipeline gas" = "#009E73",
    "imported N.Amer pipeline gas" = "#F0E442",
    "imported PAC pipeline gas" = "#0072B2",
    "imported RUS pipeline gas" = "#D55E00"))+
  ggsave(paste0(PLOT_FOLDER,"S4A_cum_region_total_add_facet_2050_import.png", sep = ""),width=11, height=8, units="in")


  # S4B ---------------------------------------------------------------------

cum_agg_region_total_ret_reg$technology <- factor(cum_agg_region_total_ret_reg$technology,
                                                  levels = c("domestic natural gas",
                                                             "imported LNG",
                                                             "imported Afr_MidE pipeline gas",
                                                             "imported EUR pipeline gas",
                                                             "imported LA pipeline gas",
                                                             "imported N.Amer pipeline gas",
                                                             "imported PAC pipeline gas",
                                                             "imported RUS pipeline gas"))

cum_agg_region_total_ret_reg$grouped_region <- factor(cum_agg_region_total_ret_reg$grouped_region , 
                                                      levels  = grouped_region_levels)



ggplot(data = filter(cum_agg_region_total_ret_reg, scenario %in% paper_scenarios, year %in% c(2050),
                     technology != "domestic natural gas"),
       aes(x = grouped_region, y = cum_retirements, fill = technology, group = technology))+
  geom_bar(position = position_stack(reverse = TRUE), stat = "identity")+
  stat_identity(yintercept=0, geom='hline', inherit.aes=TRUE, size = 1)+
  facet_wrap(~scenario, nrow = 2, scales = "fixed", labeller = as_labeller(paper_scenario_labels))+
  labs(title = "Cumulative pipeline and LNG retirement in 2050", x = "Region", y = "MTPA") +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), text = element_text(size = 12))+
  theme(legend.position = "right", text = element_text(size = 12)) +
  scale_fill_manual(values = c(
    "imported LNG" = "gray70",
    "imported Afr_MidE pipeline gas" = "#E69F00",
    "imported EUR pipeline gas" = "#56B4E9",
    "imported LA pipeline gas" = "#009E73",
    "imported N.Amer pipeline gas" = "#F0E442",
    "imported PAC pipeline gas" = "#0072B2",
    "imported RUS pipeline gas" = "#D55E00"))+
  ggsave(paste0(PLOT_FOLDER,"S4B_cum_region_total_ret_facet_2050_import.png", sep = ""),width=11, height=8, units="in")




  # S5 ----------------------------------------------------------------------

global_total_add_ng_cost$sector <- factor(global_total_add_ng_cost$sector,
                                          levels = c("traded LNG",
                                                     "traded Afr_MidE pipeline gas",
                                                     "traded EUR pipeline gas",
                                                     "traded LA pipeline gas",
                                                     "traded N.Amer pipeline gas",
                                                     "traded PAC pipeline gas",
                                                     "traded RUS pipeline gas"))

global_total_add_ng_cost$scenario <- factor(global_total_add_ng_cost$scenario,
                                            levels = c("Reference",
                                                       "Reference_LLS",
                                                       "Reference_LT_sshc",
                                                       "Transition",
                                                       "Transition_LLS",
                                                       "Transition_LT_sshc"))

ggplot(data = filter(global_total_add_ng_cost, scenario %in% paper_scenarios, year >= 2015 & year<= 2050),
       aes(x = year, y = cost/(10^9), fill = sector, group = sector))+
  geom_bar(position = position_stack(reverse = TRUE), stat = "identity")+
  stat_identity(yintercept=0, geom='hline', inherit.aes=TRUE, size = 1)+
  facet_wrap(~scenario, nrow = 2, scales = "fixed", labeller = as_labeller(paper_scenario_labels))+
  labs(title = "Annual pipeline and LNG trade infrastructure costs", x = "Year", y = "2020$ (billions)") +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), text = element_text(size = 12))+
  theme(legend.position = "right", text = element_text(size = 12)) +
  scale_fill_manual(values = c(
    "traded LNG" = "gray70",
    "traded Afr_MidE pipeline gas" = "#E69F00",
    "traded EUR pipeline gas" = "#56B4E9",
    "traded LA pipeline gas" = "#009E73",
    "traded N.Amer pipeline gas" = "#F0E442",
    "traded PAC pipeline gas" = "#0072B2",
    "traded RUS pipeline gas" = "#D55E00"))+
  ggsave(paste0(PLOT_FOLDER,"S5_annual_global_gas_trade_costs.png", sep = ""),width=15, height=8.5, units="in")

  # S6A ---------------------------------------------------------------------
ggplot(data = filter(global_pri_ene_CCS, scenario %in% paper_scenarios, year >= 2015 & year <= 2050),
       aes(x = year, y = value, fill = fuel, group = fuel))+
  geom_bar(position = position_stack(reverse = TRUE), stat = "identity")+
  facet_wrap(~scenario, nrow = 2, scales = "free", labeller = as_labeller(paper_scenario_labels)) +
  labs(title = "Global primary energy", x = "", y = "EJ") +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1), text = element_text(size = 12))+
  theme(legend.position = "right", text = element_text(size = 12)) +
  scale_fill_manual(values=jgcricol()$pal_all[names(jgcricol()$pal_all) %in% pri_ene_CCS_fuels])+
  ggsave(paste0(PLOT_FOLDER,"S6A_global_pri_ene_CCS_scen.png", sep = ""),width=15, height=8.5, units="in")



  # S6B ---------------------------------------------------------------------

diff_grouped_region_pri_ene_CCS_TRANS_REF_plot$grouped_region <- factor(diff_grouped_region_pri_ene_CCS_TRANS_REF_plot$grouped_region,
                                                                        levels  = flip_grouped_region_levels)

ggplot(data = filter(diff_grouped_region_pri_ene_CCS_TRANS_REF_plot, year == 2050),
       aes(x = grouped_region, y = value, fill = fuel, group = fuel))+
  geom_bar(position = position_stack(reverse = TRUE), stat = "identity")+
  geom_errorbar(data = filter(diff_grouped_region_pri_ene_CCS_TRANS_REF_plot, year == 2050),
                aes(x = grouped_region, ymin = value.net, ymax = value.net), lty = "32", width = 1, size = 0.4)+
  stat_identity(yintercept=0, geom='hline', inherit.aes=TRUE, size = 1, color = "black")+
  labs(title = "", x = "", y = "Primary energy consumption (EJ)") +
  facet_wrap(~scenario.diff, nrow = 2, scales = "fixed", labeller = as_labeller(c("Transition" = "2050 Transition - Reference"))) +
  theme_bw()+
  theme(text = element_text(size = 12))+
  theme(legend.position = "right", text = element_text(size = 12)) +
  scale_fill_manual(values=jgcricol()$pal_all[names(jgcricol()$pal_all) %in% pri_ene_CCS_fuels])+
  coord_flip()+
  ggsave(paste0(PLOT_FOLDER,"S6B_diff_region_pri_ene_CCS_2050_TRANS_REF.png", sep = ""),width=9, height=7, units="in")




  # S7A ----------------------------------------------------------------------

ggplot(data = filter(global_elec_gen, scenario %in% paper_scenarios, year >= 2015 & year <= 2050),
       aes(x = year, y = value*277.778, fill = sector, group = sector))+
  geom_bar(position = position_stack(reverse = TRUE), stat = "identity")+
  facet_wrap(~scenario, nrow = 2, scales = "free", labeller = as_labeller(paper_scenario_labels)) +
  labs(title = "Global electricity generation", x = "", y = "TWh") +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1), text = element_text(size = 12))+
  theme(legend.position = "right", text = element_text(size = 12)) +
  scale_fill_manual(values=elec_gen_colors)+
  ggsave(paste0(PLOT_FOLDER,"S7A_global_elec_gen_scen.png", sep = ""),width=15, height=8.5, units="in")


  # S7B ---------------------------------------------------------------------
diff_grouped_region_elec_gen_TRANS_REF_plot$grouped_region <- factor(diff_grouped_region_elec_gen_TRANS_REF$grouped_region,
                                                                      levels  = flip_grouped_region_levels)

ggplot(data = filter(diff_grouped_region_elec_gen_TRANS_REF_plot, year == 2050),
       aes(x = grouped_region, y = value*277.778, fill = sector, group = sector))+
  geom_bar(position = position_stack(reverse = TRUE), stat = "identity")+
  geom_errorbar(data = filter(diff_grouped_region_elec_gen_TRANS_REF_plot, year == 2050),
                aes(x = grouped_region, ymin = value.net, ymax = value.net), lty = "32", width = 1, size = 0.4)+
  stat_identity(yintercept=0, geom='hline', inherit.aes=TRUE, size = 1, color = "black")+
  labs(title = "", x = "", y = "Electricity generation (TWh)") +
  facet_wrap(~scenario.diff, nrow = 2, scales = "fixed", labeller = as_labeller(c("Transition" = "2050 Transition - Reference"))) +
  theme_bw()+
  theme(text = element_text(size = 12))+
  theme(legend.position = "right", text = element_text(size = 12)) +
  scale_fill_manual(values=elec_gen_colors)+
  coord_flip()+
  ggsave(paste0(PLOT_FOLDER,"S7B_diff_region_elec_gen_CCS_2050_TRANS_REF.png", sep = ""),width=9, height=7, units="in")


  # S8A ---------------------------------------------------------------------

ggplot(data = filter(global_final_ene, scenario %in% paper_scenarios, year >= 2015 & year <= 2050),
       aes(x = year, y = value, fill = input, group = input))+
  geom_bar(position = position_stack(reverse = TRUE), stat = "identity")+
  facet_wrap(~scenario, nrow = 2, scales = "free", labeller = as_labeller(paper_scenario_labels)) +
  labs(title = "Global final energy", x = "", y = "EJ") +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1), text = element_text(size = 12))+
  theme(legend.position = "right", text = element_text(size = 12)) +
  scale_fill_manual(values=jgcricol()$pal_all[names(jgcricol()$pal_all) %in% unique(final_ene$input)])+
  ggsave(paste0(PLOT_FOLDER,"S8A_global_final_ene_scen.png", sep = ""),width=15, height=8.5, units="in")

  # S8B ---------------------------------------------------------------------
diff_grouped_region_final_ene_TRANS_REF_plot$grouped_region <- factor(diff_grouped_region_final_ene_TRANS_REF$grouped_region,
                                                                      levels  = flip_grouped_region_levels)

ggplot(data = filter(diff_grouped_region_final_ene_TRANS_REF_plot, year == 2050),
       aes(x = grouped_region, y = value, fill = input, group = input))+
  geom_bar(position = position_stack(reverse = TRUE), stat = "identity")+
  geom_errorbar(data = filter(diff_grouped_region_final_ene_TRANS_REF_plot, year == 2050),
                aes(x = grouped_region, ymin = value.net, ymax = value.net), lty = "32", width = 1, size = 0.4)+
  stat_identity(yintercept=0, geom='hline', inherit.aes=TRUE, size = 1, color = "black")+
  labs(title = "", x = "", y = "Final energy (EJ)") +
  facet_wrap(~scenario.diff, nrow = 2, scales = "fixed", labeller = as_labeller(c("Transition" = "2050 Transition - Reference"))) +
  theme_bw()+
  theme(text = element_text(size = 12))+
  theme(legend.position = "right", text = element_text(size = 12)) +
  scale_fill_manual(values=jgcricol()$pal_all[names(jgcricol()$pal_all) %in% unique(final_ene$input)])+
  coord_flip()+
  ggsave(paste0(PLOT_FOLDER,"S8B_diff_region_final_ene_2050_TRANS_REF.png", sep = ""),width=9, height=7, units="in")



  # S9A ----------------------------------------------------------------------

ggplot(data = filter(global_total_traded_ng, scenario %in% paper_scenarios, year >= 2015 & year <= 2050),
       aes(x = year, y = value, color = scenario, group = scenario, linetype = scenario))+
  geom_line(size = 1.5)+
  geom_point(data = WEO_world_traded_gas_cons, aes(x=year, y=EJ, group = scenario, shape=scenario), size = 3, color= "black")+
  labs(title = "Global traded natural gas", x = "", y = "EJ") +
  scale_y_continuous(limits = c(0,200))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), text = element_text(size = 20))+
  theme(legend.position = "right", text = element_text(size = 20),
        legend.key.width = unit(2, "cm")) +
  scale_color_manual(values = c("Reference" = "#56b4e9",
                                "Reference_LLS" = "#56b4e9",
                                "Reference_LT_sshc" = "#56b4e9",
                                "Transition" = "#e69f00",
                                "Transition_LLS" = "#e69f00",
                                "Transition_LT_sshc" = "#e69f00"),
                     labels = c("Reference" = "Reference",
                                "Reference_LLS" = "Reference_LLS",
                                "Reference_LT_sshc" = "Reference_LT",
                                "Transition" = "Transition",
                                "Transition_LLS" = "Transition_LLS",
                                "Transition_LT_sshc" = "Transition_LT"))+
  scale_linetype_manual(values = c("Reference" = 1,
                                   "Reference_LLS" = 2,
                                   "Reference_LT_sshc" = 3,
                                   "Transition" = 1,
                                   "Transition_LLS" = 2,
                                   "Transition_LT_sshc" = 3),
                        labels = c("Reference" = "Reference",
                                   "Reference_LLS" = "Reference_LLS",
                                   "Reference_LT_sshc" = "Reference_LT",
                                   "Transition" = "Transition",
                                   "Transition_LLS" = "Transition_LLS",
                                   "Transition_LT_sshc" = "Transition_LT"))+
  scale_shape_manual(values = c("historical" = 19,
                                "IEA-STEPS" = 15,
                                "IEA-APS" = 17,
                                "IEA-NZE" = 18),
                     labels = c("historical" = "historical",
                                "IEA-STEPS" = "IEA-STEPS",
                                "IEA-APS" = "IEA-APS",
                                "IEA-NZE" = "IEA-NZE",
                                "IEEJ-REF" = "IEEJ-REF",
                                "IEEJ-ATS" = "IEEJ-ATS"))+
  ggsave(paste0(PLOT_FOLDER,"S9A_global_export_NG_literature.png", sep = ""),width=11, height=8.5, units="in")


  # S9B ---------------------------------------------------------------------

ggplot(data = filter(global_traded_LNG, scenario %in% paper_scenarios, year >= 2015 & year <= 2050),
       aes(x = year, y = value, color = scenario, group = scenario, linetype = scenario))+
  geom_line(size = 1.5)+
  geom_point(data = WEO_world_LNG_cons, aes(x=year, y=EJ, group = scenario, shape=scenario), size = 3, color= "black")+
  labs(title = "Global traded LNG", x = "", y = "EJ") +
  scale_y_continuous(limits = c(0,70))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), text = element_text(size = 20))+
  theme(legend.position = "right", text = element_text(size = 20),
        legend.key.width = unit(2, "cm")) +
  scale_color_manual(values = c("Reference" = "#56b4e9",
                                "Reference_LLS" = "#56b4e9",
                                "Reference_LT_sshc" = "#56b4e9",
                                "Transition" = "#e69f00",
                                "Transition_LLS" = "#e69f00",
                                "Transition_LT_sshc" = "#e69f00"),
                     labels = c("Reference" = "Reference",
                                "Reference_LLS" = "Reference_LLS",
                                "Reference_LT_sshc" = "Reference_LT",
                                "Transition" = "Transition",
                                "Transition_LLS" = "Transition_LLS",
                                "Transition_LT_sshc" = "Transition_LT"))+
  scale_linetype_manual(values = c("Reference" = 1,
                                   "Reference_LLS" = 2,
                                   "Reference_LT_sshc" = 3,
                                   "Transition" = 1,
                                   "Transition_LLS" = 2,
                                   "Transition_LT_sshc" = 3),
                        labels = c("Reference" = "Reference",
                                   "Reference_LLS" = "Reference_LLS",
                                   "Reference_LT_sshc" = "Reference_LT",
                                   "Transition" = "Transition",
                                   "Transition_LLS" = "Transition_LLS",
                                   "Transition_LT_sshc" = "Transition_LT"))+
  scale_shape_manual(values = c("historical" = 19,
                                "IEA-STEPS" = 15,
                                "IEA-APS" = 17,
                                "IEA-NZE" = 18),
                     labels = c("historical" = "historical",
                                "IEA-STEPS" = "IEA-STEPS",
                                "IEA-APS" = "IEA-APS",
                                "IEA-NZE" = "IEA-NZE",
                                "IEEJ-REF" = "IEEJ-REF",
                                "IEEJ-ATS" = "IEEJ-ATS"))+
  ggsave(paste0(PLOT_FOLDER,"S9B_global_traded_LNG_literature.png", sep = ""),width=11, height=8.5, units="in")


  # S9C ---------------------------------------------------------------------

ggplot(data = filter(global_total_traded_pipeline, scenario %in% paper_scenarios, year >= 2015 & year <= 2050),
       aes(x = year, y = value, color = scenario, group = scenario, linetype = scenario))+
  geom_line(size = 1.5)+
  geom_point(data = WEO_world_pipeline_cons, aes(x=year, y=EJ, group = scenario, shape=scenario), size = 3, color= "black")+
  labs(title = "Global traded pipeline gas", x = "", y = "EJ") +
  scale_y_continuous(limits = c(0,70))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), text = element_text(size = 20))+
  theme(legend.position = "right", text = element_text(size = 20),
        legend.key.width = unit(2, "cm")) +
  scale_color_manual(values = c("Reference" = "#56b4e9",
                                "Reference_LLS" = "#56b4e9",
                                "Reference_LT_sshc" = "#56b4e9",
                                "Transition" = "#e69f00",
                                "Transition_LLS" = "#e69f00",
                                "Transition_LT_sshc" = "#e69f00"),
                     labels = c("Reference" = "Reference",
                                "Reference_LLS" = "Reference_LLS",
                                "Reference_LT_sshc" = "Reference_LT",
                                "Transition" = "Transition",
                                "Transition_LLS" = "Transition_LLS",
                                "Transition_LT_sshc" = "Transition_LT"))+
  scale_linetype_manual(values = c("Reference" = 1,
                                   "Reference_LLS" = 2,
                                   "Reference_LT_sshc" = 3,
                                   "Transition" = 1,
                                   "Transition_LLS" = 2,
                                   "Transition_LT_sshc" = 3),
                        labels = c("Reference" = "Reference",
                                   "Reference_LLS" = "Reference_LLS",
                                   "Reference_LT_sshc" = "Reference_LT",
                                   "Transition" = "Transition",
                                   "Transition_LLS" = "Transition_LLS",
                                   "Transition_LT_sshc" = "Transition_LT"))+
  scale_shape_manual(values = c("historical" = 19,
                                "IEA-STEPS" = 15,
                                "IEA-APS" = 17,
                                "IEA-NZE" = 18),
                     labels = c("historical" = "historical",
                                "IEA-STEPS" = "IEA-STEPS",
                                "IEA-APS" = "IEA-APS",
                                "IEA-NZE" = "IEA-NZE",
                                "IEEJ-REF" = "IEEJ-REF",
                                "IEEJ-ATS" = "IEEJ-ATS"))+
  ggsave(paste0(PLOT_FOLDER,"S9C_global_traded_pipeline_NG_literature.png", sep = ""),width=11, height=8.5, units="in")


  # S10 ---------------------------------------------------------------------

#group regions to match WEO
grouped_res_prod_WEO <- res_prod %>%
  left_join(grouped_region_mapping_WEO, by = "region") %>%
  group_by(Units, scenario, grouped_region, resource, year) %>%
  dplyr::summarise(value = sum(value))

ggplot()+
  geom_point(data = filter(grouped_res_prod_WEO, resource == "natural gas", scenario %in% paper_scenarios, year >= 2015 & year <= 2050),
             aes(x = year, y = value, color = scenario, group = scenario))+
  geom_line(data = filter(grouped_res_prod_WEO, resource == "natural gas", scenario %in% paper_scenarios, year >= 2015 & year <= 2050),
            aes(x = year, y = value, color = scenario, group = scenario, linetype = scenario))+
  geom_point(data = filter(WEO_reg_gas_prod, year >= 2015), aes(x=year, y=EJ, group = scenario, shape=scenario), color= "black")+
  labs(title = "Regional natural gas production", x = "", y = "EJ") +
  facet_wrap(~grouped_region)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), text = element_text(size = 20))+
  theme(legend.position = "right", text = element_text(size = 20),
        legend.key.width = unit(2, "cm")) +
  scale_color_manual(values = c("Reference" = "#56b4e9",
                                "Reference_LLS" = "#56b4e9",
                                "Reference_LT_sshc" = "#56b4e9",
                                "Transition" = "#e69f00",
                                "Transition_LLS" = "#e69f00",
                                "Transition_LT_sshc" = "#e69f00"),
                     labels = c("Reference" = "Reference",
                                "Reference_LLS" = "Reference_LLS",
                                "Reference_LT_sshc" = "Reference_LT",
                                "Transition" = "Transition",
                                "Transition_LLS" = "Transition_LLS",
                                "Transition_LT_sshc" = "Transition_LT"))+
  scale_linetype_manual(values = c("Reference" = 1,
                                   "Reference_LLS" = 2,
                                   "Reference_LT_sshc" = 3,
                                   "Transition" = 1,
                                   "Transition_LLS" = 2,
                                   "Transition_LT_sshc" = 3),
                        labels = c("Reference" = "Reference",
                                   "Reference_LLS" = "Reference_LLS",
                                   "Reference_LT_sshc" = "Reference_LT",
                                   "Transition" = "Transition",
                                   "Transition_LLS" = "Transition_LLS",
                                   "Transition_LT_sshc" = "Transition_LT"))+
  scale_shape_manual(values = c("historical" = 19,
                                "IEA-STEPS" = 15,
                                "IEA-APS" = 17),
                     labels = c("historical" = "historical",
                                "IEA-STEPS" = "IEA-STEPS",
                                "IEA-APS" = "IEA-APS"))+
  ggsave(paste0(PLOT_FOLDER,"S10_reg_gas_prod_NG_literature.png", sep = ""),width=11, height=8.5, units="in")


  # S11 ---------------------------------------------------------------------

grouped_gas_cons_WEO <- grouped_pri_ene_CCS %>%
  filter(fuel %in% c("b natural gas", "b natural gas CCS")) %>%
  left_join(grouped_region_mapping_WEO, by = c("region")) %>%
  group_by(scenario, grouped_region, year) %>%
  dplyr::summarise(value = sum(value))

ggplot()+
  geom_point(data = filter(grouped_gas_cons_WEO, scenario %in% paper_scenarios, year >= 2015 & year <= 2050),
             aes(x = year, y = value, color = scenario, group = scenario))+
  geom_line(data = filter(grouped_gas_cons_WEO, scenario %in% paper_scenarios, year >= 2015 & year <= 2050),
            aes(x = year, y = value, color = scenario, group = scenario, linetype = scenario))+
  geom_point(data = filter(WEO_reg_gas_cons, year >= 2015), aes(x=year, y=EJ, group = scenario, shape=scenario), color= "black")+
  labs(title = "Regional natural gas consumption", x = "", y = "EJ") +
  facet_wrap(~grouped_region)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), text = element_text(size = 20))+
  theme(legend.position = "right", text = element_text(size = 20),
        legend.key.width = unit(2, "cm")) +
  scale_color_manual(values = c("Reference" = "#56b4e9",
                                "Reference_LLS" = "#56b4e9",
                                "Reference_LT_sshc" = "#56b4e9",
                                "Transition" = "#e69f00",
                                "Transition_LLS" = "#e69f00",
                                "Transition_LT_sshc" = "#e69f00"),
                     labels = c("Reference" = "Reference",
                                "Reference_LLS" = "Reference_LLS",
                                "Reference_LT_sshc" = "Reference_LT",
                                "Transition" = "Transition",
                                "Transition_LLS" = "Transition_LLS",
                                "Transition_LT_sshc" = "Transition_LT"))+
  scale_linetype_manual(values = c("Reference" = 1,
                                   "Reference_LLS" = 2,
                                   "Reference_LT_sshc" = 3,
                                   "Transition" = 1,
                                   "Transition_LLS" = 2,
                                   "Transition_LT_sshc" = 3),
                        labels = c("Reference" = "Reference",
                                   "Reference_LLS" = "Reference_LLS",
                                   "Reference_LT_sshc" = "Reference_LT",
                                   "Transition" = "Transition",
                                   "Transition_LLS" = "Transition_LLS",
                                   "Transition_LT_sshc" = "Transition_LT"))+
  scale_shape_manual(values = c("historical" = 19,
                                "IEA-STEPS" = 15,
                                "IEA-APS" = 17),
                     labels = c("historical" = "historical",
                                "IEA-STEPS" = "IEA-STEPS",
                                "IEA-APS" = "IEA-APS"))+
  ggsave(paste0(PLOT_FOLDER,"S11_reg_gas_cons_NG_literature.png", sep = ""),width=11, height=8.5, units="in")



  # S12 ---------------------------------------------------------------------
ggplot()+
  geom_line(data = filter(AR6_pri_ene_gas, year <= 2050 & year >= 2020),
            aes(x = year, y = value*EJ_Tcf, color = scenario, linetype = scenario, group = interaction(Model, Model_Scenario)), linewidth = 0.5, alpha = 0.05) +
  geom_line(data = filter(global_pri_ene, scenario %in% paper_scenarios, fuel == "b natural gas", year >= 2015 & year <= 2050),
            aes(x = year, y = value, color = scenario, group = scenario, linetype = scenario), linewidth = 1)+
  geom_point(data = WEO_world_gas_cons, aes(x=year, y=EJ, group = scenario, shape=scenario), size = 3, color= "black")+
  labs(title = "Global natural gas consumption", x = "", y = "EJ") +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1), text = element_text(size = 12))+
  theme(legend.position = "right", text = element_text(size = 12),
        legend.key.width = unit(2, "cm")) +
  scale_color_manual(values = c("Reference" = "#56b4e9",
                                "Reference_LLS" = "#56b4e9",
                                "Reference_LT_sshc" = "#56b4e9",
                                "Transition" = "#e69f00",
                                "Transition_LLS" = "#e69f00",
                                "Transition_LT_sshc" = "#e69f00",
                                "A_below 1.5C" = "#c77cff",
                                "B_1.5-2C" = "#7997ff",
                                "C_2-3C" = "#00ba38",
                                "D_3-4C" = "#d39200",
                                "E_above 4C" = "#f8766d"),
                     labels = c("Reference" = "Reference",
                                "Reference_LLS" = "Reference_LLS",
                                "Reference_LT_sshc" = "Reference_LT",
                                "Transition" = "Transition",
                                "Transition_LLS" = "Transition_LLS",
                                "Transition_LT_sshc" = "Transition_LT",
                                "A_below 1.5C" = "below 1.5C",
                                "B_1.5-2C" = "1.5-2C",
                                "C_2-3C" = "2-3C",
                                "D_3-4C" = "3-4C",
                                "E_above 4C" = "above 4C"))+
  scale_shape_manual(values = c("historical" = 19,
                                "IEA-STEPS" = 15,
                                "IEA-APS" = 17,
                                "IEA-NZE" = 18,
                                "IEEJ-REF" = 3,
                                "IEEJ-ATS" = 4),
                     labels = c("historical" = "historical",
                                "IEA-STEPS" = "IEA-STEPS",
                                "IEA-APS" = "IEA-APS",
                                "IEA-NZE" = "IEA-NZE",
                                "IEEJ-REF" = "IEEJ-REF",
                                "IEEJ-ATS" = "IEEJ-ATS"))+
  scale_linetype_manual(values = c("Reference" = 1,
                                   "Reference_LLS" = 2,
                                   "Reference_LT_sshc" = 3,
                                   "Transition" = 1,
                                   "Transition_LLS" = 2,
                                   "Transition_LT_sshc" = 3,
                                   "A_below 1.5C" = 1,
                                   "B_1.5-2C" = 1,
                                   "C_2-3C" = 1,
                                   "D_3-4C" = 1,
                                   "E_above 4C" = 1),
                        labels = c("Reference" = "Reference",
                                   "Reference_LLS" = "Reference_LLS",
                                   "Reference_LT_sshc" = "Reference_LT",
                                   "Transition" = "Transition",
                                   "Transition_LLS" = "Transition_LLS",
                                   "Transition_LT_sshc" = "Transition_LT",
                                   "A_below 1.5C" = "below 1.5C",
                                   "B_1.5-2C" = "1.5-2C",
                                   "C_2-3C" = "2-3C",
                                   "D_3-4C" = "3-4C",
                                   "E_above 4C" = "above 4C"))+
  ggsave(paste0(PLOT_FOLDER,"S12_global_NG_cons_literature.png", sep = ""),width=11, height=8.5, units="in")



  # S13 ---------------------------------------------------------------------

ggplot()+
  geom_line(data = filter(AR6_FFI_CO2_em, year <= 2050 & year >= 2020),
            aes(x = year, y = value/1000, color = scenario, linetype = scenario, group = interaction(Model, Model_Scenario)), linewidth = 0.5, alpha = 0.05) +
  geom_line(data = filter(global_FFI_CO2, scenario %in% paper_scenarios, year >= 2015 & year <= 2050),
            aes(x = year, y = value/1000, color = scenario, group = scenario, linetype = scenario), linewidth = 1)+
  geom_point(data = WEO_world_FFI_CO2_em, aes(x=year, y=value/1000, group = scenario, shape=scenario), size = 3, color= "black")+
  labs(title = "Global FFI CO2 emissions", x = "", y = "GtCO2") +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), text = element_text(size = 20))+
  theme(legend.position = "right", text = element_text(size = 20),
        legend.key.width = unit(1.2, "cm")) +
  scale_color_manual(values = c("Reference" = "#56b4e9",
                                "Reference_LLS" = "#56b4e9",
                                "Reference_LT_sshc" = "#56b4e9",
                                "Transition" = "#e69f00",
                                "Transition_LLS" = "#e69f00",
                                "Transition_LT_sshc" = "#e69f00",
                                "A_below 1.5C" = "#c77cff",
                                "B_1.5-2C" = "#7997ff",
                                "C_2-3C" = "#00ba38",
                                "D_3-4C" = "#d39200",
                                "E_above 4C" = "#f8766d"),
                     labels = c("Reference" = "Reference",
                                "Reference_LLS" = "Reference_LLS",
                                "Reference_LT_sshc" = "Reference_LT",
                                "Transition" = "Transition",
                                "Transition_LLS" = "Transition_LLS",
                                "Transition_LT_sshc" = "Transition_LT",
                                "A_below 1.5C" = "below 1.5C",
                                "B_1.5-2C" = "1.5-2C",
                                "C_2-3C" = "2-3C",
                                "D_3-4C" = "3-4C",
                                "E_above 4C" = "above 4C"))+
  scale_shape_manual(values = c("historical" = 19,
                                "IEA-STEPS" = 15,
                                "IEA-APS" = 17,
                                "IEA-NZE" = 18),
                     labels = c("historical" = "historical",
                                "IEA-STEPS" = "IEA-STEPS",
                                "IEA-APS" = "IEA-APS",
                                "IEA-NZE" = "IEA-NZE"))+
  scale_linetype_manual(values = c("Reference" = 1,
                                   "Reference_LLS" = 2,
                                   "Reference_LT_sshc" = 3,
                                   "Transition" = 1,
                                   "Transition_LLS" = 2,
                                   "Transition_LT_sshc" = 3,
                                   "A_below 1.5C" = 1,
                                   "B_1.5-2C" = 1,
                                   "C_2-3C" = 1,
                                   "D_3-4C" = 1,
                                   "E_above 4C" = 1),
                        labels = c("Reference" = "Reference",
                                   "Reference_LLS" = "Reference_LLS",
                                   "Reference_LT_sshc" = "Reference_LT",
                                   "Transition" = "Transition",
                                   "Transition_LLS" = "Transition_LLS",
                                   "Transition_LT_sshc" = "Transition_LT",
                                   "A_below 1.5C" = "below 1.5C",
                                   "B_1.5-2C" = "1.5-2C",
                                   "C_2-3C" = "2-3C",
                                   "D_3-4C" = "3-4C",
                                   "E_above 4C" = "above 4C"))+
  ggsave(paste0(PLOT_FOLDER,"S13_global_CO2_literature.png", sep = ""),width=11, height=8.5, units="in")


  # S14 ---------------------------------------------------------------------
ggplot(data = filter(global_CO2_agg_sector_res_prod, scenario %in% paper_scenarios, year >= 2015 & year <= 2050),
       aes(x = year, y = value/1000, fill = sector, group = sector))+
  geom_bar(position = position_stack(reverse = TRUE), stat = "identity")+
  facet_wrap(~scenario, nrow = 2, scales = "fixed", labeller = as_labeller(paper_scenario_labels)) +
  labs(title = "Global CO2 emissions by sector", x = "", y = "GtCO2e") +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1), text = element_text(size = 12))+
  theme(legend.position = "right", text = element_text(size = 12)) +
  scale_fill_manual(values=c("Electricity" = "#0099cc",
                             "Industry" = "#cc0033",
                             "Transportation" = "#3333cc",
                             "Buildings" = "#ffcc00",
                             "Resource Production" = "#cc79a7",
                             "Transformation" = "#006600",
                             "Urban" = "#333333",
                             "Agriculture and Land Use" = "#00931d"))+
  ggsave(paste0(PLOT_FOLDER,"global_CO2_sector_res_prod.png", sep = ""),width=15, height=8.5, units="in")


  # S15 ---------------------------------------------------------------------

ggplot(data = filter(global_GHG_agg_sector_res_prod, scenario %in% paper_scenarios, year >= 2015 & year <= 2050),
       aes(x = year, y = value/1000, fill = sector, group = sector))+
  geom_bar(position = position_stack(reverse = TRUE), stat = "identity")+
  facet_wrap(~scenario, nrow = 2, scales = "fixed", labeller = as_labeller(paper_scenario_labels)) +
  labs(title = "Global GHG emissions by sector", x = "", y = "GtCO2e") +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1), text = element_text(size = 12))+
  theme(legend.position = "right", text = element_text(size = 12)) +
  scale_fill_manual(values=c("Electricity" = "#0099cc",
                             "Industry" = "#cc0033",
                             "Transportation" = "#3333cc",
                             "Buildings" = "#ffcc00",
                             "Resource Production" = "#cc79a7",
                             "Transformation" = "#006600",
                             "Urban" = "#333333",
                             "Agriculture and Land Use" = "#00931d"))+
  ggsave(paste0(PLOT_FOLDER,"global_GHG_sector_res_prod.png", sep = ""),width=15, height=8.5, units="in")


  # S16 ---------------------------------------------------------------------

ggplot(data = filter(global_sector_gas_GHG, scenario %in% paper_scenarios, year >= 2015 & year <= 2050),
       aes(x = year, y = value/1000, fill = sector, group = sector))+
  geom_bar(position = position_stack(reverse = TRUE), stat = "identity")+
  facet_wrap(~scenario, nrow = 2, scales = "fixed", labeller = as_labeller(paper_scenario_labels)) +
  labs(title = "Global gas GHG emissions", x = "", y = "GtCO2e") +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1), text = element_text(size = 12))+
  theme(legend.position = "right", text = element_text(size = 12)) +
  scale_fill_manual(values=c("Electricity" = "#0099cc",
                             "Industry" = "#cc0033",
                             "Transportation" = "#3333cc",
                             "Buildings" = "#ffcc00",
                             "Resource Production" = "#cc79a7",
                             "Transformation" = "#006600",
                             "Urban" = "#333333",
                             "Agriculture and Land Use" = "#00931d"))+
  ggsave(paste0(PLOT_FOLDER,"global_gas_GHG_sector.png", sep = ""),width=15, height=8.5, units="in")
