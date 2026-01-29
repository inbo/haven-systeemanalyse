library(tidyverse)
library(lubridate)
library(INBOtheme)
conflicted::conflicts_prefer(dplyr::filter)

# # Content
# #########
# 
# ## Hydrochem_calc_mg_NP(data): berekent mg N/l en mg P/l voor abiotische N- en P-componenten en uniformiseert Component en Eenheid in gedownloade LIMS-data.
# 
# ## Hydrochem_add_SatCalc() berekent de zuurstofsaturatie op basis van O2_veld, temp en Cl-gehalte. Toegevoeds omdat saturatie niet altijd wordt genoteerd in het veld en er wel normen or bestaan.
# 
# ## dit script maakt tabel BKN aan (biologische kwaliteitsnormen)
# 
# ## Hydrochem_plot_component(data, component, watertype): maakt staafdiagram van alle waarnemingen van één component met aanduiding van ecologische kwaliteitsnorm voor gekozen watertype(s) (maximaal 2) met datum-meetpuntcode op x-as en waarde op y-as
#   # data: dataset uit LIMS gedwonload
#   # component: naam component waarvoor normen bestaan, één te kiezen uit 
#       # c("O2_veld", "Saturatie", "COD", "NPOC", "T.N", "NO3", "NH4", "T.P", "PO4", "SO4", "EC.25", "pH.25", "Cl")
#   # watertype: één of twee te kiezen uit waarde in BKN$Watertype (opgelijst in tabel Watertypes)
# 
# 
# # a <- read_csv("../data/DWHLIMS_Sigma2014_2021.csv")
# 
# 
# # Bereken mg N en P van LIMS downloads en pas Eenheid aan
# ###########################################################################
# # NO3, NO2, NH4 en PO4 worden in mg/l gegeven
# Hydrochem_calc_mg_NP <- function(data){
#   data %>% 
#     dplyr::select(LaboCode, MeetpuntCode = FinCode, Monsternamedatum, Component, 
#                   WaardeGeformatteerd, Meetwaarde = NumeriekeWaarde, 
#                   Eenheid, BinnenSpecs, BenedenLOQ, BovenMaxLOQ) %>% 
#     # Omrekening naar mgN/l en mgP/l
#     mutate(Meetwaarde2 =
#              case_when(Component == "NH4" ~ Meetwaarde*14.007/(14.007+3*1.008),
#                        Component == "NO2" ~ Meetwaarde*14.007/(14.007+2*15.999),
#                        Component == "NO3" ~ Meetwaarde*14.007/(14.007+3*15.999),
#                        Component == "PO4" ~ Meetwaarde*30.974/(30.974+4*15.999),
#                        TRUE ~ Meetwaarde),
#            Meetwaarde2 = round(Meetwaarde2, 3),
#            Meetwaarde2 = if_else(BenedenLOQ == FALSE, Meetwaarde2, Meetwaarde2/2),
#            Eenheid2 = 
#              case_when(Component %in% c("NH4", "NO2", "NO3") ~ "mgN/l",
#                        Component == "PO4" ~ "mgP/l",
#                        Eenheid == "MG_P_L" ~ "mg/l",
#                        Eenheid == "MICROS_P_CM"  ~ "µS/cm",
#                        Eenheid == "NONE" ~ NA_character_,
#                        Eenheid == "MEQ_L" ~ "meq/l",
#                        Eenheid == "PERCENT" ~ "%",
#                        Eenheid == "DEGREE_C" ~ "°C",
#                        Eenheid == "µG_L" ~ "µg/l",
#                        Eenheid == "MG_N_P_L" ~ "mgN/l",
#                        Eenheid == "MGO2_P_L" ~ "mgO2/l",
#                        Eenheid == "MG_P_P_L" ~ "mgP/l",
#                        TRUE ~ Eenheid),
#            Component = 
#              case_when(Component == "AL" ~ "Al",
#                        Component == "CA" ~ "Ca",
#                        Component %in% c("EC", "EC_25°C") ~ "EC.25",
#                        Component %in% c("EC25.VELD") ~ "EC_veld",
#                        Component == "FE" ~ "Fe",
#                        Component == "MG" ~ "Mg",
#                        Component == "MN" ~ "Mn",
#                        Component == "O2.VELD" ~ "O2_veld",
#                        Component == "pH(25°C)" ~ "pH.25",
#                        Component == "pH.VELD" ~ "pH_veld",
#                        Component == "SAT.VELD" ~ "Saturatie",
#                        Component == "SI" ~ "Si",
#                        TRUE ~ Component))
#     }
# 
# # Saturatie is niet altijd ingevuld, maar kan redelijk nauwkeurig berekend worden obv TEMP.VELD, O2_veld en Cl
# # We vergelijken hieronder twee methodes
# 
# ## Methode: zie https://edepot.wur.nl/379645
# ############################################
# # T <- 15.4
# # Te <- T/10
# # Cl = 80.4
# # Bar = 101.3
# # Ox0 <- 14.6393 - 4.3174*Te + 1.17217*Te^2 - 0.2565*Te^3 + 0.027825*Te^4
# # Sal  <-  0.001805*Cl + 0.03
# # Delta <- 0.09216 - 0.03316*Te + 0.007089*Te^2 - 0.000759*Te^3 
# # Pwi = 0.638*exp(Te*0.643)
# # Fact <- (Bar-Pwi)/(101.3-Pwi)
# # ox <- (Ox0 - Sal*Delta)*Fact
# ## Methode: zie http://www.math4mbo.nl/files/HA-1025-h-17-1-o-opg5.pdf
# ######################################################################
# # Temp <- seq(0, 25, by = 0.01)
# # Sat <- 498/(34 + Temp)
# # SatDat <- tibble(Temp = Temp, Sat = Sat)
# 
# #Vergelijking van resultaten
# ############################
# # a <- bDWH2021opp %>% 
# #   filter(Component %in% c("O2_veld", "Saturatie", "TEMP.VELD", "Cl")) %>% 
# #   select(MeetpuntCode, Monsternamedatum, Meetwaarde, Component) %>% 
# #   spread(key = Component, value = Meetwaarde) %>% 
# #   mutate(
# #     Te = TEMP.VELD/10,
# #     Ox0 = 14.6393 - 4.3174*Te + 1.17217*Te^2 - 0.2565*Te^3 + 0.027825*Te^4,
# #     Sal = 0.001805*Cl+0.03,
# #     Delta = 0.09216 - 0.03316*Te + 0.007089*Te^2 - 0.000759*Te^3,
# #     Saturatie_WUR = 100*O2_veld/(Ox0 - Sal*Delta),
# #     Saturatie_math4 = 100*O2_veld/(498/(34+TEMP.VELD)))
# # a %>% 
# #   ggplot(aes(x = Saturatie, y = Saturatie_WUR)) +
# #   geom_point()
# # a %>% 
# #   ggplot(aes(x = Saturatie, y = Saturatie_math4)) +
# #   geom_point()
# # a %>% 
# #   ggplot(aes(x = Saturatie_WUR, y = Saturatie_math4)) +
# #   geom_point()
# 
# # Resultaten zijn gelijkaardig, WUR methode is wellicht nauwkeuriger, we gebruiken de WUR-methode. Eronder staat ook een functie met de math4-methode, uitgecommentarieerd.
# 
# Hydrochem_add_SatCalc <- function(data){
#   data %>% 
#     filter(Component %in% c("O2_veld", "TEMP.VELD")) %>% 
#     dplyr::select(LaboCode, MeetpuntCode, Monsternamedatum, Component, 
#                   Meetwaarde) %>%
#     spread(key = Component, value = Meetwaarde) %>% 
#     # WUR methode, voor normale luchtdruk
#     mutate(
#       Te = TEMP.VELD/10,
#       Ox0 = 14.6393 - 4.3174*Te + 1.17217*Te^2 - 0.2565*Te^3 + 0.027825*Te^4,
#       Sal = 0.001805*Cl+0.03,
#       Delta = 0.09216 - 0.03316*Te + 0.007089*Te^2 - 0.000759*Te^3,
#       Meetwaarde2 = 100*O2_veld/(Ox0 - Sal*Delta)) %>% 
#     mutate(WaardeGeformatteerd = NA_character_,
#            Meetwaarde = NA_integer_,
#            Eenheid = NA_character_,
#            Component = "Sat_calc",
#            BinnenSpecs = ifelse(Meetwaarde2 == 200, FALSE, TRUE),
#            Eenheid2 = "%",
#            BenedenLOQ = FALSE,
#            BovenMaxLOQ = ifelse(Meetwaarde2 == 200, TRUE, FALSE)) %>% 
#     dplyr::select(LaboCode, MeetpuntCode, Monsternamedatum, Component, 
#                   WaardeGeformatteerd, Meetwaarde, 
#                   Eenheid, BinnenSpecs, BenedenLOQ, BovenMaxLOQ, Meetwaarde2,
#                   Eenheid2) %>% 
#     bind_rows(data) %>% 
#     mutate(Component = replace(Component, Component == "Saturatie", "Saturatie_ori"),
#            Component = replace(Component, Component == "Sat_calc", "Saturatie"))
# }
# # Functie obv math4 methode
# # Hydrochem_add_SatCalc2 <- function(data){
# #   data %>% 
# #     filter(Component %in% c("O2_veld", "TEMP.VELD")) %>% 
# #     dplyr::select(LaboCode, MeetpuntCode, Monsternamedatum, Component, 
# #                   Meetwaarde) %>%
# #     spread(key = Component, value = Meetwaarde) %>% 
# #     # math4-methode
# #     mutate(Meetwaarde2 = 100*O2_veld/(498/(34+TEMP.VELD))) %>% 
# #     mutate(WaardeGeformatteerd = NA_character_,
# #            Meetwaarde = NA_integer_,
# #            Eenheid = NA_character_,
# #            Component = "Sat_calc",
# #            BinnenSpecs = ifelse(Meetwaarde2 == 200, FALSE, TRUE),
# #            Eenheid2 = "%",
# #            BenedenLOQ = FALSE,
# #            BovenMaxLOQ = ifelse(Meetwaarde2 == 200, TRUE, FALSE)) %>% 
# #     dplyr::select(LaboCode, MeetpuntCode, Monsternamedatum, Component, 
# #                   WaardeGeformatteerd, Meetwaarde, 
# #                   Eenheid, BinnenSpecs, BenedenLOQ, BovenMaxLOQ, Meetwaarde2,
# #                   Eenheid2) %>% 
# #     bind_rows(data)
# # }
# # VOORBEELD
# DWH2021opp <-
#   read_csv("../data/DWHLIMS_Sigma_opp2021.csv", lazy = FALSE)
# bDWH2021opp <-
#   Hydrochem_calc_mg_NP(DWH2021opp)
# cDWH2021opp <-
#   Hydrochem_add_SatCalc(bDWH2021opp)
# 
# 
# # Lees tabel met biologische kwaliteitsnormen en lijst watertypes op
# BKN <- read_csv("C:/R/Projects/Hydrochem/data/BKNforGGplot.csv", lazy = FALSE)
# Watertypes <- BKN %>% 
#   distinct(Watertype)
# 
# 
# # Plot component
# Hydrochem_plot_component <- function(data, component, watertype){
#   # Definieer titel, label y-as en schaal y-as obv component en dataset
#   if(component == "O2_veld"){
#     title = "Zuurstof"
#     laby = "O2 (mg N/l)" 
#     limy = c(0,20)} 
#   
#   else if(component == "Saturatie"){
#     title = "Zuurstof"
#     laby = "% O2" 
#     limy = c(0,201)} 
#   
#   else if(component == "COD"){
#     title = "Chemische zuurstofvraag"
#     laby = "CZV (mg O2/l)" 
#     limy = c(0, plyr::round_any(max(data$Meetwaarde2[data$Component == component]), 10, 
#                                 f = ceiling))}
#   else if(component == "NPOC"){
#     title = "Koolstof totaal (NPOC)"
#     laby = "NPOC (mg C/l)" 
#     limy = c(0, plyr::round_any(max(data$Meetwaarde2[data$Component == component]), 10, 
#                                 f = ceiling))}
#   else if(component == "T.N"){
#     title = "Stikstof totaal"
#     laby = "N (mg/l)" 
#     limy = c(0, plyr::round_any(max(data$Meetwaarde2[data$Component == component]), 1, 
#                                 f = ceiling))}
#   else if(component == "NO3"){
#     title = "Nitraat"
#     laby = "NO3 (mg N/l)" 
#     limy = c(0, plyr::round_any(max(data$Meetwaarde2[data$Component == component]), 1, 
#                                 f = ceiling))}
#   else if(component == "NH4"){
#     title = "Ammonium"
#     laby = "NH4 (mg N/l)" 
#     limy = c(0, plyr::round_any(max(data$Meetwaarde2[data$Component == component]), 1, 
#                                 f = ceiling))}
#   else if(component == "T.P"){
#     title = "Fosfor totaal"
#     laby = "P (mg/l)" 
#     limy = c(0, plyr::round_any(max(data$Meetwaarde2[data$Component == component]), 0.1, 
#                                 f = ceiling))}
#   else if(component == "PO4"){
#       title = "Fosfaat"
#       laby = "PO4 (mg P/l)" 
#       limy = c(0, plyr::round_any(max(data$Meetwaarde2[data$Component == component]), 0.1, 
#                                   f = ceiling))}
#   else if(component == "SO4"){
#     title = "Fosfaat"
#     laby = "SO4 (mg/l)" 
#     limy = c(0, plyr::round_any(max(data$Meetwaarde2[data$Component == component]), 10, 
#                                 f = ceiling))}
#   else if(component == "EC.25"){
#     title = "Elektrische geleidbaarheid"
#     laby = "EC.25 (µS/cm)" 
#     limy = c(0, plyr::round_any(max(data$Meetwaarde2[data$Component == component]), 10, 
#                                 f = ceiling))}
#   else if(component == "pH.25"){
#     title = "Zuurgraad"
#     laby = "pH" 
#     limy = c(0, plyr::round_any(max(data$Meetwaarde2[data$Component == component]), 1, 
#                                 f = ceiling))}
#   else if(component == "Cl"){
#     title = "Chloride"
#     laby = "Cl (mg/l)" 
#     limy = c(0, plyr::round_any(max(data$Meetwaarde2[data$Component == component]), 10, 
#                                 f = ceiling))}
# 
# # Plot
#   # lees BKN bestand in en maak tabel norm aan obv watertype en component 
#   BKN <- 
#     read_csv("C:/R/Projects/Hydrochem/data/BKNforGGplot.csv", lazy = FALSE) %>% 
#     mutate(norm_min = ifelse(Component == "EC.20", norm_min*1.116, norm_min),
#            norm_max = ifelse(Component == "EC.20", norm_max*1.116, norm_max),
#            Component = replace(Component, Component == "EC.20", "EC.25"))
#   norm <- BKN %>%
#     filter(Watertype %in% watertype, Component == component)
#   data <- data %>% 
#     filter(Component %in% component) %>%
#     # labels for x-axis
#     mutate(Staal = str_c(str_sub(as.character(Monsternamedatum), 1L, 7L), "_",
#                          MeetpuntCode))
#   # plot zonder BKN als watertype is afwezig of als geen norm bestaat voor component-watertype
#   if(missing(watertype)|nrow(norm) == 0){
#     data %>% 
#       ggplot(aes(x = Staal, y = Meetwaarde2, fill = MeetpuntCode)) +
#       geom_col() +
#       labs(title = title, y = laby) +
#       scale_y_continuous(limits = limy) +
#       theme(axis.text.x = element_text(angle = 90),
#             axis.title.x = element_blank(),
#             plot.title = element_text(hjust = 0.5, size = 16),
#             legend.text = element_text(color = "black"))
#   }
#   
#   #Polta als watertype is opgegeven
#   else if(!missing(watertype)){
#     # Pas limy aan als norm_max > limy[2]
#     if(max(norm$norm_max) > limy[2]){limy <- c(0, max(norm$norm_max))}
#     
#     if(component == "pH.25"){
#       data %>% 
#         ggplot(aes(x = Staal, y = Meetwaarde2, fill = MeetpuntCode)) +
#         geom_col() +
#         # biologische kwaliteitsnorm(en)
#         geom_hline(aes(yintercept = norm$norm_min[1], 
#                        linetype = norm$Legendelabel[1]), 
#                    color = "red", lwd = 1) +
#         geom_hline(aes(yintercept = norm$norm_max[1], 
#                        linetype = norm$Legendelabel[1]), 
#                    color = "red", lwd = 1) +
#         geom_hline(aes(yintercept = norm$norm_min[2], 
#                        linetype = norm$Legendelabel[2]), 
#                    color = "red", lwd = 1) +
#         geom_hline(aes(yintercept = norm$norm_max[2], 
#                        linetype = norm$Legendelabel[2]), 
#                    color = "red", lwd = 1) +labs(title = title, y = laby) +
#         scale_y_continuous(limits = limy) +
#         scale_linetype_manual(name = "Biol. kwaliteitsnorm", values = c(1,2)) +
#         theme(axis.text.x = element_text(angle = 90),
#               axis.title.x = element_blank(),
#               plot.title = element_text(hjust = 0.5, size = 16),
#               legend.text = element_text(color = "black"))}
#     else if(component == "Cl" & "brakke polderwaterloop" %in% watertype){
#       data %>% 
#         ggplot(aes(x = Staal, y = Meetwaarde2, fill = MeetpuntCode)) +
#         geom_col() +
#         geom_hline(aes(yintercept = norm$norm_max, linetype = norm$Legendelabel), 
#                    color = "red", lwd = 1) +
#         labs(title = title, y = laby) +
#         scale_y_continuous(limits = limy) +
#         scale_linetype_manual(name = "Biol. kwaliteitsnorm", values = c(2)) +
#         theme(axis.text.x = element_text(angle = 90),
#               axis.title.x = element_blank(),
#               plot.title = element_text(hjust = 0.5, size = 16),
#               legend.text = element_text(color = "black"))
#     }
#     else{data %>% 
#       ggplot(aes(x = Staal, y = Meetwaarde2, fill = MeetpuntCode)) +
#       geom_col() +
#       geom_hline(aes(yintercept = norm$norm_min[1], linetype = norm$Legendelabel[1]), 
#                  color = "red", lwd = 1) +
#       geom_hline(aes(yintercept = norm$norm_min[2], linetype = norm$Legendelabel[2]), 
#                    color = "red", lwd = 1) +
#       labs(title = title, y = laby) +
#       scale_y_continuous(limits = limy) +
#       scale_linetype_manual(name = "Biol. kwaliteitsnorm", values = c(1, 2)) +
#       theme(axis.text.x = element_text(angle = 90),
#             axis.title.x = element_blank(),
#             plot.title = element_text(hjust = 0.5, size = 16),
#             legend.text = element_text(color = "black"))}
#      }
# }
# 
# # Voorbeeld
# ###########
# bDWH2021opp %>% 
#   filter(str_sub(MeetpuntCode, 1, 3) %in% c("DUR", "MOB")) %>% 
#   Hydrochem_plot_component(component = "Saturatie", 
#                            watertype = "circumneutraal, sterk gebufferd meer")
# cDWH2021opp %>% 
#   filter(str_sub(MeetpuntCode, 1, 3) %in% c("DUR", "MOB")) %>% 
#   Hydrochem_plot_component(component = "T.P", 
#                            watertype = "circumneutraal, sterk gebufferd meer")
# 
# cDWH2021opp %>% 
#   filter(str_sub(MeetpuntCode, 1, 3) %in% c("SCM")) %>% 
#   Hydrochem_plot_component(component = "T.N", 
#                            watertype = "circumneutraal, sterk gebufferd meer")
#############################################################################################

# Testdata
VAR <-
  read_csv("C:/R/Projects/Hydrochem/data/Hydrochem_2014_2024.csv") %>%
  filter(str_detect(MeetpuntCode, "VARS|VARR"))
VAR_DWH_test <- read_csv("C:/R/Projects/Hydrochem/data/DWHLIMS_Sigma2014_2024.csv")  %>%
  filter(str_detect(FinCode, "VARS|VARR"))

################
# November 2022, januari 2025
###############

# Bovenstaande is vorige versie van de functies.
# Beschrijving en achtergrond van de functies is te vinden in Functie_hydrochem_LIMS.Rmd en Functie_hydrochem_LIMS.html, in dezelfde directory. 

Hydrochem_calc_mg_NP <- function(data){
  #Sanity checks
  #-------------
  assertthat::assert_that(is.data.frame(data), 
                          msg ="data has to be a data frame")
  assertthat::assert_that(!("Meetwaarde2" %in% names(data)),
                          msg = "Meetwaarde2 is column in data. Most probably you ran this function already on this data.")

  if("WaardeNumeriek" %in% names(data)){
    data <- data %>% rename(NumeriekeWaarde = WaardeNumeriek)}
  data %>% 
    dplyr::select(LaboCode, MeetpuntCode = FinCode, Monsternamedatum, Component, 
                  AnalyseNaam, WaardeGeformatteerd, Meetwaarde = NumeriekeWaarde, 
                  Eenheid, BinnenSpecs, BenedenLOQ, BovenMaxLOQ) %>% 
    # Omrekening naar mgN/l en mgP/l
    mutate(Meetwaarde2 =
             case_when(Component == "NH4" ~ Meetwaarde*14.007/(14.007+3*1.008),
                       Component == "NO2" ~ Meetwaarde*14.007/(14.007+2*15.999),
                       Component == "NO3" ~ Meetwaarde*14.007/(14.007+3*15.999),
                       Component == "PO4" ~ Meetwaarde*30.974/(30.974+4*15.999),
                       TRUE ~ Meetwaarde),
           Meetwaarde2 = round(Meetwaarde2, 3),
           # Meetwaarde < DL Meetwaarde2 = DL/2
           Meetwaarde2 = if_else(BenedenLOQ == FALSE, Meetwaarde2, Meetwaarde2/2),
           Eenheid2 = 
             case_when(Component %in% c("NH4", "NO2", "NO3") ~ "mgN/l",
                       Component == "PO4" ~ "mgP/l",
                       Eenheid == "MG_P_L" ~ "mg/l",
                       Eenheid == "MICROS_P_CM"  ~ "µS/cm",
                       Eenheid == "NONE" ~ NA_character_,
                       Eenheid == "MEQ_L" ~ "meq/l",
                       Eenheid == "PERCENT" ~ "%",
                       Eenheid == "DEGREE_C" ~ "°C",
                       Eenheid == "µG_L" ~ "µg/l",
                       Eenheid == "MG_N_P_L" ~ "mgN/l",
                       Eenheid == "MGO2_P_L" ~ "mgO2/l",
                       Eenheid == "MG_P_P_L" ~ "mgP/l",
                       TRUE ~ Eenheid),
           Component = 
             case_when(Component == "AL" ~ "Al",
                       Component == "CA" ~ "Ca",
                       Component == "NA" ~ "Na",
                       Component %in% c("EC", "EC_25°C") ~ "EC.25",
                       Component %in% c("EC25.VELD") ~ "EC_veld",
                       Component == "FE" ~ "Fe",
                       Component == "MG" ~ "Mg",
                       Component == "MN" ~ "Mn",
                       Component == "O2.VELD" ~ "O2_veld",
                       Component %in% c("pH (25°C)", "pH(25°C)") ~ "pH.25",
                       Component == "pH.VELD" ~ "pH_veld",
                       Component == "SAT.VELD" ~ "Saturatie",
                       Component == "SI" ~ "Si",
                       Component == "N_KJEL" ~ "N_Kj",
                       Component == "KJELD.N" ~ "N_Kj",
                       Component == "P_TOT" ~ "T.P",
                       Component == "N_TOT" ~ "T.N",
                       (Component == "Temp" & AnalyseNaam == "VELDMETING") ~ 
                         "Temp_veld",
                       Component == "TEMP.VELD"  ~ "Temp_veld",
                       TRUE ~ Component))
}
# Test
# Hydrochem_calc_mg_NP(VAR_DWH_test) %>% 
#   filter(Component == "PO4")
# (VAR <- Hydrochem_calc_mg_NP(VAR_DWH_test))


# Hydrochem_add_Ntcalc
######################
# calculate N.T_calc if N_Kj, NO3 and NO2 are measured.

Hydrochem_add_Ntcalc <- function(data){
  # Sanity
  assertthat::assert_that(is.data.frame(data), 
                          msg ="data has to be a data frame")
  assertthat::assert_that(!("T.N_calc" %in% data$Component),
                          msg = "T.N_calc is present as Component in data. Most probably you ran this function already on this data.")
  
  data %>% 
    group_by(LaboCode, MeetpuntCode, Monsternamedatum) %>%
    filter(any("N_Kj" %in% Component), 
           any("NO3" %in% Component),        # keep data if N_Kj, NO3 an NO2 are present in analysis,
           any("NO2" %in% Component)) %>%    # drop data if one of those comp is absent
    summarise(Meetwaarde2 = 
                Meetwaarde2[Component == "N_Kj"] + Meetwaarde2[Component == "NO3"] +
                Meetwaarde2[Component == "NO2"],
              BenedenLOQ = !BenedenLOQ[Component == "N_Kj"] * 
                !BenedenLOQ[Component == "NO3"] *
                !BenedenLOQ[Component == "NO2"], # if all 3 components belowLOQ => belowLOQ else above
              .groups = "drop") %>% 
    mutate(WaardeGeformatteerd = NA_character_,
           Meetwaarde = NA_integer_,
           Eenheid = NA_character_,
           Component = "T.N_calc",
           Eenheid2 = "mgN/l",
           BinnenSpecs = BenedenLOQ,
           BovenMaxLOQ = FALSE) %>% # problem is that this parameter is not part of Lims export anymore, but will (almost) never be TRUE
    dplyr::select(LaboCode, MeetpuntCode, Monsternamedatum, Component,
                  WaardeGeformatteerd, Meetwaarde,
                  Eenheid, BinnenSpecs, BenedenLOQ, BovenMaxLOQ, Meetwaarde2,
                  Eenheid2) %>% 
    bind_rows(data)
}

# Tests
# VARNtcalc <- 
#   Hydrochem_add_Ntcalc(VAR)
# Hydrochem_add_Ntcalc(VARNtcalc)
# Hydrochem_add_Ntcalc(VAR)
# VAR_DWH_test %>%
#   Hydrochem_calc_mg_NP() %>%
#   Hydrochem_add_Ntcalc() %>%
#   filter(Component %in% c("T.N", "T.N_calc")) %>%
#   ggplot(aes(x = Monsternamedatum, y = Meetwaarde2,
#              color = MeetpuntCode, shape = Component)) +
#   geom_point()

# Hydrochem_add_SatCalc
#######################
# Calculate O2-concentration and O2-saturtion, based on measured O2-saturtion or O2-concentration and temperature

# Hydrochem_add_SatCalc_old <- function(data){
#    a <- 
#     data %>%
#     filter(Component %in% c("O2_veld", "Temp_veld")) %>%
#     group_by(LaboCode, MeetpuntCode, Monsternamedatum) %>%
#     summarise(
#       O2 = sum((Component == "O2_veld")*Meetwaarde),
#       TEMP = sum((Component == "Temp_veld")*Meetwaarde),
#       .groups = "drop") %>% 
#     mutate(Meetwaarde2 = 100*O2/(498/(34+TEMP)),
#            Meetwaarde2 = if_else(O2 == 0, NA_real_, Meetwaarde2)) %>% # if O2-veld = 0 this is not correct, but I guess this never happens.
#     mutate(WaardeGeformatteerd = NA_character_,
#            Meetwaarde = NA_integer_,
#            Eenheid = NA_character_,
#            Component = "Sat_calc",
#            BinnenSpecs = ifelse(O2 == 20, FALSE, TRUE), # 20 mg/l is detectielimiet
#            Eenheid2 = "%",
#            BenedenLOQ = FALSE,
#            BovenMaxLOQ = ifelse(O2 == 20, TRUE, FALSE)) %>%
#     dplyr::select(LaboCode, MeetpuntCode, Monsternamedatum, Component,
#                   WaardeGeformatteerd, Meetwaarde,
#                   Eenheid, BinnenSpecs, BenedenLOQ, BovenMaxLOQ, Meetwaarde2,
#                   Eenheid2)
# 
#   b <-
#     data %>%
#     filter(Component %in% c("Saturatie", "Temp_veld")) %>%
#     group_by(LaboCode, MeetpuntCode, Monsternamedatum) %>%
#     summarise(
#       Sat = sum((Component == "Saturatie")*Meetwaarde),
#       TEMP = sum((Component == "Temp_veld")*Meetwaarde),
#       .groups = "drop") %>%
#     mutate(Meetwaarde2 = (498/(34+TEMP))*Sat/100,
#            Meetwaarde2 = if_else(Sat == 0, NA_real_, Meetwaarde2)) %>%
#     mutate(WaardeGeformatteerd = NA_character_,
#            Meetwaarde = NA_integer_,
#            Eenheid = NA_character_,
#            Component = "O2_calc",
#            BinnenSpecs = ifelse(Sat == 200, FALSE, TRUE), # 20 mg/l is detectielimiet
#            Eenheid2 = "mg/l",
#            BenedenLOQ = FALSE,
#            BovenMaxLOQ = ifelse(Sat == 200, TRUE, FALSE)) %>%
#     dplyr::select(LaboCode, MeetpuntCode, Monsternamedatum, Component,
#                   WaardeGeformatteerd, Meetwaarde,
#                   Eenheid, BinnenSpecs, BenedenLOQ, BovenMaxLOQ, Meetwaarde2,
#                   Eenheid2)
# 
#   bind_rows(a, b, data)
# }

Hydrochem_add_SatCalc <- function(data){
  # Sanity
  assertthat::assert_that(is.data.frame(data), 
                          msg ="data has to be a data frame")
  assertthat::assert_that(!("Sat_calc" %in% data$Component |
                              "O2_calc" %in% data$Component),
                          msg = "Sat_calc or O2_calc is present as Component in data. Most probably you ran this function already on this data.")
  # calculate O2-saturation
  a <- 
    data %>%
    group_by(LaboCode, MeetpuntCode, Monsternamedatum) %>% 
    filter(any("O2_veld" %in% Component), # if O2-veld and TEMP.VELD are present we can calculate O2-sat
           any("Temp_veld" %in% Component)) %>% 
    summarise(O2 = Meetwaarde2[Component == "O2_veld"],
              Meetwaarde2 =
                100*Meetwaarde2[Component == "O2_veld"]/
                (498/(34+Meetwaarde2[Component == "Temp_veld"])),
              .groups = "drop") %>% 
    mutate(WaardeGeformatteerd = NA_character_,
           Meetwaarde = NA_integer_,
           Eenheid = NA_character_,
           Component = "Sat_calc",
           BinnenSpecs = ifelse(O2 == 20, FALSE, TRUE), # 20 mg/l is detectielimiet
           Eenheid2 = "%",
           BenedenLOQ = FALSE,
           BovenMaxLOQ = ifelse(O2 == 20, TRUE, FALSE)) %>%
    dplyr::select(LaboCode, MeetpuntCode, Monsternamedatum, Component,
                  WaardeGeformatteerd, Meetwaarde,
                  Eenheid, BinnenSpecs, BenedenLOQ, BovenMaxLOQ, Meetwaarde2,
                  Eenheid2)

  
  b <-
    data %>%
    group_by(LaboCode, MeetpuntCode, Monsternamedatum) %>% 
    filter(any("Saturatie" %in% Component), # if OSaturatie and TEMP.VELD are present we can calculate O2-calc
           any("Temp_veld" %in% Component)) %>% 
    summarise(Sat = Meetwaarde2[Component == "Saturatie"],
              Meetwaarde2 = (498/(34+Meetwaarde2[Component == "Temp_veld"]))*
                Meetwaarde2[Component == "Saturatie"]/100,
              .groups = "drop") %>%
    mutate(WaardeGeformatteerd = NA_character_,
           Meetwaarde = NA_integer_,
           Eenheid = NA_character_,
           Component = "O2_calc",
           BinnenSpecs = ifelse(Sat == 200, FALSE, TRUE), # 20 mg/l is detectielimiet
           Eenheid2 = "mg/l",
           BenedenLOQ = FALSE,
           BovenMaxLOQ = ifelse(Sat == 200, TRUE, FALSE)) %>%
    dplyr::select(LaboCode, MeetpuntCode, Monsternamedatum, Component,
                  WaardeGeformatteerd, Meetwaarde,
                  Eenheid, BinnenSpecs, BenedenLOQ, BovenMaxLOQ, Meetwaarde2,
                  Eenheid2)

  return(bind_rows(data, a, b))
}

# Tests
# Hydrochem_add_SatCalc(VAR) %>%
#   filter(Component %in% c("O2_veld", "O2_calc")) %>%
#   ggplot(aes(x = Monsternamedatum, y = Meetwaarde2,
#              col = MeetpuntCode, shape = Component)) +
#   geom_point()
# 
# VAR_DWH_test %>%
#   Hydrochem_calc_mg_NP() %>% 
#   Hydrochem_add_SatCalc() %>%
#   filter(Component %in% c("Saturatie", "Sat_calc")) %>%
#   ggplot(aes(x = Monsternamedatum, y = Meetwaarde2,
#              col = MeetpuntCode, shape = Component)) +
#   geom_point()
# filter O2_veld if present, otherwise filter O2_calc
# VAR %>%
#   group_by(LaboCode) %>%
#   filter(case_when("O2_veld" %in% Component ~ Component == "O2_veld",
#                    .default = Component == "O2_calc")) %>%
#   ggplot(aes(x = Monsternamedatum, y = Meetwaarde2,
#              col = MeetpuntCode, shape = Component)) +
#   geom_point()
# 
# test <- function(data){
#   data %>% 
#     group_by(LaboCode) %>%
#     filter(case_when("O2_veld" %in% Component ~ Component == "O2_veld",
#                      .default = Component == "O2_calc")) %>% 
#     ungroup()
# }
# test(VAR_DWH)

# BKN voor geleidbaarheid is EC.20, labo rapporteert EC.25
Hydrochem_add_EC20 <- function(data){ 
  data %>% 
    filter(Component == "EC.25") %>% 
    mutate(Component = "EC.20",
           Meetwaarde = Meetwaarde/1.116) %>% 
    bind_rows(data)
    }

# BKN <- read_csv("C:/R/Projects/Hydrochem/data/BKNforGGplot.csv", lazy = FALSE)

#----------------------------------------------------------------------------------

# Plot component

# Hydrochem_plot_titlelabs is used in other functions, returns a list with title, label y en limits y

Hydrochem_plot_titlelabs <- function(data, component){
  # Definieer titel, label y-as en schaal y-as obv component en dataset
  # fysisch
  if(component == "EC.25"){
    title = "Elektrische geleidbaarheid"
    laby = "EC.25 (µS/cm)" 
    limy = c(0, plyr::round_any(max(data$Meetwaarde2[data$Component == component]), 10, 
                                f = ceiling))}
  if(component == "EC.20"){
    title = "Elektrische geleidbaarheid"
    laby = "EC.20 (µS/cm)" 
    limy = c(0, plyr::round_any(max(data$Meetwaarde2[data$Component == component]), 10,
                                      f = ceiling))}
  else if(component == "pH.25"){
    title = "Zuurgraad"
    laby = "pH" 
    limy = c(0, plyr::round_any(max(data$Meetwaarde2[data$Component == component]), 1, 
                                f = ceiling))}
  else if(component == "SD.VELD"){
    title = "Secchi diepte"
    laby = "Secchi diepte (m)" 
    limy = c(0, plyr::round_any(max(data$Meetwaarde2[data$Component == component]), 0.1, 
                                f = ceiling))}
  # chemisch - zuurstof
  else if(component == "O2_veld"){
    title = "Zuurstof"
    laby = "O2 (mg N/l)" 
    limy = c(0,20)} 
  else if(component == "Saturatie"){
    title = "Zuurstof"
    laby = "% O2" 
    limy = c(0,201)} 
  else if(component == "Sat_calc"){
    title = "Zuurstof"
    laby = "% O2" 
    limy = c(0,201)} 
  else if(component == "COD"){
    title = "Chemische zuurstofvraag"
    laby = "CZV (mg O2/l)" 
    limy = c(0, plyr::round_any(max(data$Meetwaarde2[data$Component == component]), 10, 
                                f = ceiling))}
  # chemisch C
  else if(component == "NPOC"){
    title = "Koolstof totaal (NPOC)"
    laby = "NPOC (mg C/l)" 
    limy = c(0, plyr::round_any(max(data$Meetwaarde2[data$Component == component]), 10, 
                                f = ceiling))}
  # chemisch nutriënten
  else if(component == "T.N"){
    title = "Stikstof totaal"
    laby = "N (mg/l)" 
    limy = c(0, plyr::round_any(max(data$Meetwaarde2[data$Component == component]), 1, 
                                f = ceiling))}
  else if(component == "N_Kj"){
    title = "Stikstof Kjeldahl"
    laby = "N (mg/l)" 
    limy = c(0, plyr::round_any(max(data$Meetwaarde2[data$Component == component]), 1, 
                                f = ceiling))}
  else if(component == "NO3"){
    title = "Nitraat"
    laby = "NO3 (mg N/l)" 
    limy = c(0, plyr::round_any(max(data$Meetwaarde2[data$Component == component]), 1, 
                                f = ceiling))}
  else if(component == "NO2"){
    title = "Nitriet"
    laby = "NO2 (mg N/l)" 
    limy = c(0, plyr::round_any(max(data$Meetwaarde2[data$Component == component]), 1, 
                                f = ceiling))}
  else if(component == "NH4"){
    title = "Ammonium"
    laby = "NH4 (mg N/l)" 
    limy = c(0, plyr::round_any(max(data$Meetwaarde2[data$Component == component]), 1, 
                                f = ceiling))}
  else if(component == "T.P"){
    title = "Fosfor totaal"
    laby = "P (mg/l)" 
    limy = c(0, plyr::round_any(max(data$Meetwaarde2[data$Component == component]), 0.1, 
                                f = ceiling))}
  else if(component == "PO4"){
    title = "ortho-Fosfaat"
    laby = "PO4 (mg P/l)" 
    limy = c(0, plyr::round_any(max(data$Meetwaarde2[data$Component == component]), 0.1, 
                                f = ceiling))}
  # Biologisch
  else if(component == "CHL.A"){
    title = "Chlorofyl a"
    laby = "Chl a (µg/l)" 
    limy = c(0, plyr::round_any(max(data$Meetwaarde2[data$Component == component]), 0.1, 
                                f = ceiling))}
  else if(component == "Faeofytine"){
    title = "Faeofytine"
    laby = "Faeofytine (µg/l)" 
    limy = c(0, plyr::round_any(max(data$Meetwaarde2[data$Component == component]), 0.1, 
                                f = ceiling))}
  # chemisch anionen
  else if(component == "Cl"){
    title = "Chloride"
    laby = "Cl (mg/l)" 
    limy = c(0, plyr::round_any(max(data$Meetwaarde2[data$Component == component]), 10, 
                                f = ceiling))}
  else if(component == "SO4"){
    title = "Sulfaat"
    laby = "SO4 (mg/l)" 
    limy = c(0, plyr::round_any(max(data$Meetwaarde2[data$Component == component]), 10, 
                                f = ceiling))}
  else if(component == "HCO3"){
    title = "Bicarbonaat"
    laby = "HCO3 (mg/l)" 
    limy = c(0, plyr::round_any(max(data$Meetwaarde2[data$Component == component]), 10, 
                                f = ceiling))}
  #chemisch kationen
  else if(component == "Na"){
    title = "Natrium"
    laby = "Na (mg/l)" 
    limy = c(0, plyr::round_any(max(data$Meetwaarde2[data$Component == component]), 10, 
                                f = ceiling))}
  else if(component == "K"){
    title = "Kalium"
    laby = "K (mg/l)" 
    limy = c(0, plyr::round_any(max(data$Meetwaarde2[data$Component == component]), 10, 
                                f = ceiling))}
  else if(component == "Ca"){
    title = "Calcium"
    laby = "Ca (mg/l)" 
    limy = c(0, plyr::round_any(max(data$Meetwaarde2[data$Component == component]), 10, 
                                f = ceiling))}
  else if(component == "Mg"){
    title = "Magnesium"
    laby = "Mg (mg/l)" 
    limy = c(0, plyr::round_any(max(data$Meetwaarde2[data$Component == component]), 5, 
                                f = ceiling))}
  # metalen en andere
  else if(component == "Fe"){
    title = "IJzer"
    laby = "Fe (mg/l)" 
    limy = c(0, plyr::round_any(max(data$Meetwaarde2[data$Component == component]), 0.5, 
                                f = ceiling))}
  else if(component == "Al"){
    title = "Aluminium"
    laby = "Al (mg/l)" 
    limy = c(0, plyr::round_any(max(data$Meetwaarde2[data$Component == component]), 0.5, 
                                f = ceiling))}
  else if(component == "Mn"){
    title = "Mangaan"
    laby = "Mn (mg/l)" 
    limy = c(0, plyr::round_any(max(data$Meetwaarde2[data$Component == component]), 0.5, 
                                f = ceiling))}
  else if(component == "Si"){
    title = "Silicium"
    laby = "Si (mg/l)" 
    limy = c(0, plyr::round_any(max(data$Meetwaarde2[data$Component == component]), 10, 
                                f = ceiling))}
  return(list(title, laby, limy))
}


# Hydrochem_plot_titlelabsload is used in other functions wich plot loads (vuilvracht), returns a list with title, label y en limits y

Hydrochem_plot_titlelabsload <- function(data, component){
  # Definieer titel, label y-as en schaal y-as obv component en dataset
  # fysisch
  if(component == "EC.25"){
    title = "Elektrische geleidbaarheid"
    laby = "EC.25 (µS/cm)" 
    limy = c(0, plyr::round_any(max(data$Meetwaarde2[data$Component == component]), 10, 
                                f = ceiling))}
  if(component == "EC.20"){
    title = "Elektrische geleidbaarheid"
    laby = "EC.20 (µS/cm)" 
    limy = c(0, plyr::round_any(max(data$Meetwaarde2[data$Component == component]), 10,
                                f = ceiling))}
  else if(component == "pH.25"){
    title = "Zuurgraad"
    laby = "pH" 
    limy = c(0, plyr::round_any(max(data$Meetwaarde2[data$Component == component]), 1, 
                                f = ceiling))}
  else if(component == "SD.VELD"){
    title = "Secchi diepte"
    laby = "Secchi diepte (m)" 
    limy = c(0, plyr::round_any(max(data$Meetwaarde2[data$Component == component]), 0.1, 
                                f = ceiling))}
  # chemisch - zuurstof
  else if(component == "O2_veld"){
    title = "Zuurstof"
    laby = "O2 (mg N/l)" 
    limy = c(0,20)} 
  else if(component == "Saturatie"){
    title = "Zuurstof"
    laby = "% O2" 
    limy = c(0,201)} 
  else if(component == "Sat_calc"){
    title = "Zuurstof"
    laby = "% O2" 
    limy = c(0,201)} 
  else if(component == "COD"){
    title = "Chemische zuurstofvraag"
    laby = "CZV (kg O2/dag)" 
    limy = c(0, plyr::round_any(max(data$Meetwaarde2[data$Component == component]), 10, 
                                f = ceiling))}
  # chemisch C
  else if(component == "NPOC"){
    title = "Koolstof totaal (NPOC)"
    laby = "NPOC (kg C/dag)" 
    limy = c(0, plyr::round_any(max(data$Meetwaarde2[data$Component == component]), 10, 
                                f = ceiling))}
  # chemisch nutriënten
  else if(component == "T.N"){
    title = "Stikstof totaal"
    laby = "N (kg N/dag)" 
    limy = c(0, plyr::round_any(max(data$Meetwaarde2[data$Component == component]), 1, 
                                f = ceiling))}
  else if(component == "N_Kj"){
    title = "Stikstof Kjeldahl"
    laby = "N (kg N/dag)" 
    limy = c(0, plyr::round_any(max(data$Meetwaarde2[data$Component == component]), 1, 
                                f = ceiling))}
  else if(component == "NO3"){
    title = "Nitraat"
    laby = "NO3 (kg N/dag)" 
    limy = c(0, plyr::round_any(max(data$Meetwaarde2[data$Component == component]), 1, 
                                f = ceiling))}
  else if(component == "NO2"){
    title = "Nitriet"
    laby = "NO2 (kg N/dag)" 
    limy = c(0, plyr::round_any(max(data$Meetwaarde2[data$Component == component]), 1, 
                                f = ceiling))}
  else if(component == "NH4"){
    title = "Ammonium"
    laby = "NH4 (kg N/dag)" 
    limy = c(0, plyr::round_any(max(data$Meetwaarde2[data$Component == component]), 1, 
                                f = ceiling))}
  else if(component == "T.P"){
    title = "Fosfor totaal"
    laby = "P (kg P/dag)" 
    limy = c(0, plyr::round_any(max(data$Meetwaarde2[data$Component == component]), 0.1, 
                                f = ceiling))}
  else if(component == "PO4"){
    title = "ortho-Fosfaat"
    laby = "PO4 (kg P/dag)" 
    limy = c(0, plyr::round_any(max(data$Meetwaarde2[data$Component == component]), 0.1, 
                                f = ceiling))}
  # Biologisch
  else if(component == "CHL.A"){
    title = "Chlorofyl a"
    laby = "Chl a (g/dag)" 
    limy = c(0, plyr::round_any(max(data$Meetwaarde2[data$Component == component]), 0.1, 
                                f = ceiling))}
  else if(component == "Faeofytine"){
    title = "Faeofytine"
    laby = "Faeofytine (g/dag)" 
    limy = c(0, plyr::round_any(max(data$Meetwaarde2[data$Component == component]), 0.1, 
                                f = ceiling))}
  # chemisch anionen
  else if(component == "Cl"){
    title = "Chloride"
    laby = "Cl (kg/dag)" 
    limy = c(0, plyr::round_any(max(data$Meetwaarde2[data$Component == component]), 10, 
                                f = ceiling))}
  else if(component == "SO4"){
    title = "Sulfaat"
    laby = "SO4 (kg/dag)" 
    limy = c(0, plyr::round_any(max(data$Meetwaarde2[data$Component == component]), 10, 
                                f = ceiling))}
  else if(component == "HCO3"){
    title = "Bicarbonaat"
    laby = "HCO3 (kg/dag)" 
    limy = c(0, plyr::round_any(max(data$Meetwaarde2[data$Component == component]), 10, 
                                f = ceiling))}
  #chemisch kationen
  else if(component == "Na"){
    title = "Natrium"
    laby = "Na (kg/dag)" 
    limy = c(0, plyr::round_any(max(data$Meetwaarde2[data$Component == component]), 10, 
                                f = ceiling))}
  else if(component == "K"){
    title = "Kalium"
    laby = "K (kg/dag)" 
    limy = c(0, plyr::round_any(max(data$Meetwaarde2[data$Component == component]), 10, 
                                f = ceiling))}
  else if(component == "Ca"){
    title = "Calcium"
    laby = "Ca (kg/dag)" 
    limy = c(0, plyr::round_any(max(data$Meetwaarde2[data$Component == component]), 10, 
                                f = ceiling))}
  else if(component == "Mg"){
    title = "Magnesium"
    laby = "Mg (kg/dag)" 
    limy = c(0, plyr::round_any(max(data$Meetwaarde2[data$Component == component]), 5, 
                                f = ceiling))}
  # metalen en andere
  else if(component == "Fe"){
    title = "IJzer"
    laby = "Fe (kg/dag)" 
    limy = c(0, plyr::round_any(max(data$Meetwaarde2[data$Component == component]), 0.5, 
                                f = ceiling))}
  else if(component == "Al"){
    title = "Aluminium"
    laby = "Al (mg/l)" 
    limy = c(0, plyr::round_any(max(data$Meetwaarde2[data$Component == component]), 0.5, 
                                f = ceiling))}
  else if(component == "Mn"){
    title = "Mangaan"
    laby = "Mn (kg/dag)" 
    limy = c(0, plyr::round_any(max(data$Meetwaarde2[data$Component == component]), 0.5, 
                                f = ceiling))}
  else if(component == "Si"){
    title = "Silicium"
    laby = "Si (kg/dag)" 
    limy = c(0, plyr::round_any(max(data$Meetwaarde2[data$Component == component]), 10, 
                                f = ceiling))}
  return(list(title, laby, limy))
}

#----------------------------------------------------------------------------------------

# Hydrochem_plot_component() makes a bar chart of the concentration (Meetwaarde) of one component for each combinantion of date and MeetpuntCode in a data frame with LIMS data. The norm (VlaremII) is shown as a red line.
# - data: data frame with at least columns Component, MeetpuntCode, Monsternamedatum, Meetwaarde2. Made for the resulta of Hydrochem_calc_mgNP() and Hydrochem_add_SatCalc()

# - component: one component out of c("O2_veld", "Saturatie", "COD", "EC.25", "Cl", "SO4", "pH.25", "T.N", "N_Kj", "NO3", "T.P", "PO4", "N_anorg")

# - watertype:  a vector with one, two or three types of water body from VlaremII bijlage 2.3.1
#               default: NULL => plot is shown without norm.  
#               possible values: "kleine beek", "kleine beek Kempen", "grote beek", "grote beek Kempen", "kleine rivier", "grote rivier", "zeer grote rivier", "zoete polderwaterloop", "brakke polderwaterloop", "zoet mesotidaal estuarium", "zwak brak macrotidaal estuarium", "brak, macrotidaal estuarium", "zout, mesotidaal estuarium", "circumneutraal, sterk gebufferd meer", "matig ionenrijk, alkalisch meer", "groot diep eutroof alkalisch meer", "groot diep oligotroof tot mesotroof alaklisch meer", "Ionenrijk alkalisch meer", "alkalisch duinwater", "zeer licht brak meer", "Circumneutraal zwak gebufferd meer", "Circumneutraal ijzerrijk meer", "sterk zuur meer", "matig zuur meer", "sterk brak meer".

Hydrochem_bar_component <- 
  function(data, component, watertype = NULL, title = TRUE){
  # lees BKN bestand in en maak tabel norm aan obv watertype en component 
  p <- "C:/R/Projects/Hydrochem/"
  BKN <- 
    read_csv(str_c(p, "data/BKNforGGplot.csv"), 
             lazy = FALSE, show_col_types = FALSE) %>% 
    # reken norm EC.20 om naar EC.25
    mutate(norm_min = ifelse(Component == "EC.20", norm_min*1.116, norm_min),
           norm_max = ifelse(Component == "EC.20", norm_max*1.116, norm_max),
           Component = replace(Component, Component == "EC.20", "EC.25"))
  # voeg norm voor Sat_calc toe (is zelfde als saturatie)
  BKN <- 
    BKN %>%
    filter(Component == "Saturatie") %>% 
    mutate(Component = "Sat_calc") %>% 
    bind_rows(BKN)
  norm <- BKN %>%
    filter(Watertype %in% watertype, Component == component)
  data <- data %>% 
    filter(Component %in% component) %>%
    # labels for x-axis
    mutate(Staal = str_c(str_sub(as.character(Monsternamedatum), 1L, 7L), "_",
                         MeetpuntCode))
  
  # make vectors of possible values for parameter (= Component) and norm (= Watertype)
  cde <- 
    read_csv("C:/R/Projects/Hydrochem/data/cde_Parameter_CodeVMM.csv", 
             lazy = FALSE, show_col_types = FALSE) %>% 
    rename(CodeVMM = Code) %>% 
    mutate(Component = replace(Component, Component == "EC.20", "EC.25"))
  Component <- unique(cde$Component)
  norms <- unique(BKN$Watertype)
 
  #Sanity checks
  #-------------
  ## data: moet dataframe zijn
  if(!is.data.frame(data)){stop("data must be data.frame")}
  
  ## norm: moet uit Watertype
  if(length(watertype) > 3){stop("Maximal 3 values for watertype allowed!")}
  normstr <-
    str_c("watertype is wrong \n", "must be one of \"", 
          str_c(norms, collapse = "\", \""), "\".")
  if(as.logical(sum(!watertype %in% norms))){stop(normstr)}
  
  ## component: moet uit vector Component komen
  Componentstr <-
    str_c("component must be one of \"",
          str_c(Component, collapse = "\", \""), "\".")
  if(!component %in% Component){stop(Componentstr)}
  
  ## Parameter: moet aanwezig zijn in data
  parameterstr <- 
    str_c("No data for ", component, " found in data source")
  if(!component %in% data$Component){stop(parameterstr)}
  # Waarschuwing dat geen norm bestaat voor deze component
  if(nrow(norm) == 0 & !missing(watertype)){
    print(str_c("Er bestaat geen norm voor ", component))}
  # Waarschuwing dat norm niet geplot wordt omdat geen watertype is opgegeven
  if(missing(watertype)){
    print("Er wordt geen kwaliteitsnorm geplot omdat er geen watertype is opgegeven")
  }
  
  # some norms exist for streaming waters and not for standing waters (e.g. NO3) and vice versa (e.g. T.N)
  # therefor it is possible that nrow(norm) differs from length(watertype); in this case a plot should be produced with one or two norms less and a message should be given that no nrom exist for that watertype. 
  if(nrow(norm)!=length(watertype)){
    d <- tibble(Watertype = watertype) %>% 
      filter(!(Watertype %in% norm$Watertype)) # watertypes without norm 
    if(nrow(d) == 1){
      print(str_c("OPGELET: Er bestaat geen norm voor '", component, "' in '",
                  d$Watertype, "'"))}
    else{
      print(str_c("OPGELET: Er bestaan geen normen voor '", component, "' in '",
                  str_flatten(d$Watertype, collapse = "' en '"), "'"))
    }
  }
  
  
  # Plot
  ######
  # Definieer titel, label y-as en schaal y-as obv component en dataset
  list <- Hydrochem_plot_titlelabs(data, component)
  if(title == TRUE){title <- list[[1]]}else{title <- NULL}
  laby <- list[[2]]
  limy <- list[[3]]
  
  # filter data
  if(component == "O2_veld"){
    data <- 
      data %>% 
      group_by(LaboCode) %>%
      filter(case_when("O2_veld" %in% Component ~ Component == "O2_veld",
                       .default = Component == "O2_calc")) %>% 
      ungroup()}
  else if(component == "Saturatie"){
    data <- 
      data %>% 
      group_by(LaboCode) %>%
      filter(case_when("Saturatie" %in% Component ~ Component == "Saturatie",
                       .default = Component == "Sat_calc")) %>% 
      ungroup()}
  else if(component == "T.N"){
    data <- 
      data %>% 
      group_by(LaboCode) %>%
      filter(case_when("T.N" %in% Component ~ Component == "T.N",
                       .default = Component == "T.N_calc")) %>% 
      ungroup()}
  else{data <- data %>% 
    filter(Component %in% component)}
  
  data <- 
    data %>% 
    mutate(Staal = str_c(str_sub(as.character(Monsternamedatum), 1L, 7L),
                         MeetpuntCode), sep = "_")
  # basic graph
  p <- data %>%
    ggplot(aes(x = Staal, y = Meetwaarde2, fill = MeetpuntCode)) +
    geom_col() +
    labs(title = title, y = laby) +
    theme(axis.text.x = element_text(angle = 90),
          axis.title.x = element_blank(),
          plot.title = element_text(hjust = 0.5, size = 16),
          legend.text = element_text(color = "black"))
    
  ## plot zonder BKN als watertype afwezig is of als geen norm bestaat voor component-watertype
  if(missing(watertype)|nrow(norm) == 0){ 
    p <- 
      p  +
      scale_y_continuous(limits = limy) }
  # Plot als watertype is opgegeven
  else{
    # Pas limy aan als norm_max > limy[2]
    if(max(norm$norm_max) > limy[2]){limy <- c(0, max(norm$norm_max))}
    
    if(component == "Cl" & "brakke polderwaterloop" %in% watertype){
      p <- 
        p +
        geom_hline(aes(yintercept = norm$norm_max, 
                       linetype = norm$Legendelabel),
                   color = "red", lwd = 1) +
        scale_linetype_manual(name = "Biol. kwaliteitsnorm", values = c(2)) +
        scale_y_continuous(limits = limy) }
    # pH has min and max => special treatment
     else if(component == "pH.25" & nrow(norm) == 1){
      p <- 
        p +
        # biologische kwaliteitsnorm(en)
        geom_hline(aes(yintercept = norm$norm_min[1],
                       linetype = norm$Legendelabel[1]),
                   color = "red", lwd = 1) +
        geom_hline(aes(yintercept = norm$norm_max[1],
                       linetype = norm$Legendelabel[1]),
                   color = "red", lwd = 1) +
        scale_linetype_manual(name = "Biol. kwaliteitsnorm", values = c(1,2)) +
        scale_y_continuous(limits = limy) }
    else if(component == "pH.25" & nrow(norm) == 2){
      p <- 
        p +
        # biologische kwaliteitsnorm(en)
        geom_hline(aes(yintercept = norm$norm_min[1],
                       linetype = norm$Legendelabel[1]),
                   color = "red", lwd = 1) +
        geom_hline(aes(yintercept = norm$norm_max[1],
                       linetype = norm$Legendelabel[1]),
                   color = "red", lwd = 1) +
        geom_hline(aes(yintercept = norm$norm_min[2],
                       linetype = norm$Legendelabel[2]),
                   color = "red", lwd = 1) +
        geom_hline(aes(yintercept = norm$norm_max[2],
                       linetype = norm$Legendelabel[2]),
                   color = "red", lwd = 1) +
        scale_linetype_manual(name = "Biol. kwaliteitsnorm", values = c(1,2)) +
        scale_y_continuous(limits = limy) }
    else if(component == "pH.25" & nrow(norm) == 3){
      p <- 
        p +
        # biologische kwaliteitsnorm(en)
        geom_hline(aes(yintercept = norm$norm_min[1],
                       linetype = norm$Legendelabel[1]),
                   color = "red", lwd = 1) +
        geom_hline(aes(yintercept = norm$norm_max[1],
                       linetype = norm$Legendelabel[1]),
                   color = "red", lwd = 1) +
        geom_hline(aes(yintercept = norm$norm_min[2],
                       linetype = norm$Legendelabel[2]),
                   color = "red", lwd = 1) +
        geom_hline(aes(yintercept = norm$norm_max[2],
                       linetype = norm$Legendelabel[2]),
                   color = "red", lwd = 1) +
        geom_hline(aes(yintercept = norm$norm_min[3],
                       linetype = norm$Legendelabel[3]),
                   color = "red", lwd = 1) +
        geom_hline(aes(yintercept = norm$norm_max[3],
                       linetype = norm$Legendelabel[3]),
                   color = "red", lwd = 1) +
        scale_linetype_manual(name = "Biol. kwaliteitsnorm", values = c(1,2,3)) +
        scale_y_continuous(limits = limy) }
    # all other parameters, up to tree watertypes
    else if(nrow(norm) == 1){
      p <- 
        p +
        geom_hline(aes(yintercept = norm$norm_min[1], 
                       linetype = norm$Legendelabel[1]),
                   color = "red", lwd = 1) +
        scale_linetype_manual(name = "Biol. kwaliteitsnorm", 
                              values = c(1))  +
        scale_y_continuous(limits = limy) }
    else if(nrow(norm) == 2){
      p <- 
        p +
        geom_hline(aes(yintercept = norm$norm_min[1], 
                       linetype = norm$Legendelabel[1]),
                   color = "red", lwd = 1) +
        geom_hline(aes(yintercept = norm$norm_min[2], 
                       linetype = norm$Legendelabel[2]),
                   color = "red", lwd = 1) +
        scale_linetype_manual(name = "Biol. kwaliteitsnorm", values = c(1, 2)) +
        scale_y_continuous(limits = limy)}
    else{
      p <- 
        p +
        geom_hline(aes(yintercept = norm$norm_min[1], 
                       linetype = norm$Legendelabel[1]),
                   color = "red", lwd = 1) +
        geom_hline(aes(yintercept = norm$norm_min[2], 
                       linetype = norm$Legendelabel[2]),
                   color = "red", lwd = 1) +
        geom_hline(aes(yintercept = norm$norm_min[3], 
                       linetype = norm$Legendelabel[3]),
                   color = "red", lwd = 1) +
        scale_linetype_manual(name = "Biol. kwaliteitsnorm", 
                              values = c(1, 2, 3)) +
        scale_y_continuous(limits = limy) }
  }
  return(p)
}

# Tests
# Hydrochem_bar_component(data = VAR, component = "pH.25",
#                         watertype = c("kleine beek Kempen", "grote beek",
#                                       "circumneutraal, sterk gebufferd meer"))
# Hydrochem_bar_component(data = VAR, component = "NO3",
#                         watertype = "kleine beek Kempen")
# Hydrochem_bar_component(data = VAR, component = "NO3",
#                         watertype = c("kleine beek Kempen",
#                                       "circumneutraal, sterk gebufferd meer"))
Hydrochem_plot_component <- 
  function(...){
    print("Hydrochem_plot_component() is replaced by Hydrochem_bar_component(). Please change code")
  }

#-----------------------------------------------------------------------------------------
# Tests
# test <- function(data, component){
#   return( component)
# }
# VAR_DWH <- 
#   read_csv("../data/hydrochem_2014_2024.csv") %>% 
#   filter(str_detect(MeetpuntCode, "VAR"))
# VAR_DWH %>%
#   filter(str_starts(MeetpuntCode, "VARR|VARS")) %>% 
#   Hydrochem_plot_component(component = "NO3")
# VAR_DWH %>%
#   filter(str_starts(MeetpuntCode, "VARR|VARS")) %>% 
#   Hydrochem_bar_component(component = "NO3")
# VAR_DWH %>%
#   filter(str_starts(MeetpuntCode, "VARR|VARS")) %>% 
#   Hydrochem_bar_component(component = "NO3", title = FALSE)
# VAR_DWH %>%
#   filter(str_starts(MeetpuntCode, "VARR|VARS")) %>% 
#   Hydrochem_bar_component(component = "NO3", 
#                           watertype = c("kleine beek Kempen"))

# REDUNDANT: does the same as Hydrochem_plot_component_norm without norm given
# Plot component
# Hydrochem_plot_component <- function(data, component){
#   # Sanity checks
#   #-------------
#   ## data: moet dataframe zijn
#   if(!is.data.frame(data)){stop("data must be data.frame")}
#   
#   ## component: moet aanwezig zijn in data
#   parameterstr <- 
#     str_c("No data for ", component, " found in data source")
#   if(!component %in% data$Component){stop(parameterstr)}
#   
#   # Definieer titel, label y-as en schaal y-as obv component en dataset
#   # fysisch
#   list <- Hydrochem_plot_titlelabs(data, component)
#   title <- list[[1]]
#   laby <- list[[2]]
#   limy <- list[[3]]
#   
#   #Plot 
#   data <- data %>% 
#     filter(Component %in% component) %>%
#     # labels for x-axis
#     mutate(Staal = str_c(str_sub(as.character(Monsternamedatum), 1L, 7L), "_",
#                          MeetpuntCode))
#   if(component == "pH.25"){
#     data %>% 
#       ggplot(aes(x = Staal, y = Meetwaarde2, fill = MeetpuntCode)) +
#       geom_col() +
#       scale_y_continuous(limits = limy) +
#       theme(axis.text.x = element_text(angle = 90),
#             axis.title.x = element_blank(),
#             plot.title = element_text(hjust = 0.5, size = 16),
#             legend.text = element_text(color = "black"))}
#   else{data %>% 
#       ggplot(aes(x = Staal, y = Meetwaarde2, fill = MeetpuntCode)) +
#       geom_col() +
#       labs(title = title, y = laby) +
#       scale_y_continuous(limits = limy) +
#       theme(axis.text.x = element_text(angle = 90),
#             axis.title.x = element_blank(),
#             plot.title = element_text(hjust = 0.5, size = 16),
#             legend.text = element_text(color = "black"))}
# }

# Hydrochem_plot_component(POL, "SO4")
# Hydrochem_plot_component(POL, "Cl")
# Hydrochem_plot_component(POL, "HCO3")

# VAR_DWH %>%
#   filter(str_starts(MeetpuntCode, "VARR|VARS")) %>% 
#   Hydrochem_plot_component(component = "NO3")

#----------------------------------------------------------------------------------------

# Hydrochem_plot_component() makes a bar chart of the concentration (Meetwaarde) of one component for each combinantion of date and MeetpuntCode in a data frame with LIMS data. The norm (VlaremII) is shown as a red line.
# - data: data frame with at least columns Component, MeetpuntCode, Monsternamedatum, Meetwaarde2. Made for the resulta of Hydrochem_calc_mgNP() and Hydrochem_add_SatCalc()

# - component: one component out of c("O2_veld", "Saturatie", "COD", "EC.25", "Cl", "SO4", "pH.25", "T.N", "N_Kj", "NO3", "T.P", "PO4", "N_anorg")

# - watertype:  a vector with one, two or three types of water body from VlaremII bijlage 2.3.1
#               default: NULL => plot is shown without norm.  
#               possible values: "kleine beek", "kleine beek Kempen", "grote beek", "grote beek Kempen", "kleine rivier", "grote rivier", "zeer grote rivier", "zoete polderwaterloop", "brakke polderwaterloop", "zoet mesotidaal estuarium", "zwak brak macrotidaal estuarium", "brak, macrotidaal estuarium", "zout, mesotidaal estuarium", "circumneutraal, sterk gebufferd meer", "matig ionenrijk, alkalisch meer", "groot diep eutroof alkalisch meer", "groot diep oligotroof tot mesotroof alaklisch meer", "Ionenrijk alkalisch meer", "alkalisch duinwater", "zeer licht brak meer", "Circumneutraal zwak gebufferd meer", "Circumneutraal ijzerrijk meer", "sterk zuur meer", "matig zuur meer", "sterk brak meer".

# - title: TRUE or FALSE, default = TRUE

Hydrochem_point_component <- 
  function(data, component, watertype = NULL, title = TRUE){
  # lees BKN bestand in en maak tabel norm aan obv watertype en component 
  p <- "C:/R/Projects/Hydrochem/"
  BKN <- 
    read_csv(str_c(p, "data/BKNforGGplot.csv"), 
             lazy = FALSE, show_col_types = FALSE)
  # add norm for EC.25
  BKN <-
    BKN %>% 
    filter(Component == "EC.20") %>% 
    # reken norm EC.20 om naar EC.25
    mutate(norm_min = norm_min*1.116,
           norm_max = norm_max*1.116,
           Component = "EC.25") %>% 
    bind_rows(BKN)
  
  # voeg norm voor Sat_calc toe (is zelfde als saturatie)
  BKN <- 
    BKN %>%
    filter(Component == "Saturatie") %>% 
    mutate(Component = "Sat_calc") %>% 
    bind_rows(BKN)
  
  norm <- BKN %>%
    filter(Watertype %in% watertype, Component == component)
  
  
  # filter data
  # Use O2_veld if present, else use O2_calc
  if(component == "O2_veld"){
    data <- 
      data %>% 
      group_by(LaboCode) %>%
      filter(case_when("O2_veld" %in% Component ~ Component == "O2_veld",
                       .default = Component == "O2_calc")) %>% 
      ungroup()}
  # Use Sturatie if present, else use Sat_calc
  else if(component == "Saturatie"){
    data <- 
      data %>% 
      group_by(LaboCode) %>%
      filter(case_when("Saturatie" %in% Component ~ Component == "Saturatie",
                       .default = Component == "Sat_calc")) %>% 
      ungroup()}
  # Use T.N if present, else use T.N_calc
  else if(component == "T.N"){
    data <- 
      data %>% 
      group_by(LaboCode) %>%
      filter(case_when("T.N" %in% Component ~ Component == "T.N",
                       .default = Component == "T.N_calc")) %>% 
      ungroup()}
  else{data <- data %>% 
    filter(Component %in% component)}
  
  # make vectors of possible values for parameter (= Component) and norm (= Watertype)
  cde <- 
    read_csv("C:/R/Projects/Hydrochem/data/cde_Parameter_CodeVMM.csv", 
             lazy = FALSE, show_col_types = FALSE) %>% 
    rename(CodeVMM = Code) %>% 
    mutate(Component = replace(Component, Component == "EC.20", "EC.25"))
  Component <- unique(cde$Component)
  norms <- unique(BKN$Watertype)
  
  # Sanity checks
  #-------------
  ## data: moet dataframe zijn
  if(!is.data.frame(data)){stop("data must be data.frame")}
  
  ## norm: moet uit Watertype
  if(length(watertype) > 3){stop("Maximal 3 values for watertype allowed!")}
  normstr <-
    str_c("watertype is wrong \n", "must be one of \"", 
          str_c(norms, collapse = "\", \""), "\".")
  if(as.logical(sum(!watertype %in% norms))){stop(normstr)}
  
  # ## component: moet uit vector Component komen
  # Componentstr <- 
  #   str_c("component must be one of \"", 
  #         str_c(Component, collapse = "\", \""), "\".")
  # if(!component %in% Component){stop(Componentstr)}
  # 
  ## Parameter: moet aanwezig zijn in data
  parameterstr <- 
    str_c("No data for ", component, " found in data source")
  if(!component %in% data$Component){stop(parameterstr)}
  # Waarschuwing dat geen norm bestaat voor deze component
  if(!missing(watertype) & nrow(norm) == 0){
    print(str_c("OPGELET: Er bestaat geen norm voor ", component))}
  
  # some norms exist for streaming waters and not for standing waters (e.g. NO3) and vice versa (e.g. T.N)
  # therefor it is possible that nrow(norm) differs from length(watertype); in this case a plot should be produced with one or two norms less and a message should be given that no nrom exist for that watertype. 
   if(nrow(norm)!=length(watertype)){
    d <- tibble(Watertype = watertype) %>% 
      filter(!(Watertype %in% norm$Watertype)) # watertypes without norm 
  if(nrow(d) == 1){
    print(str_c("OPGELET: Er bestaat geen norm voor '", component, "' in '",
                d$Watertype, "'"))}
    else{
      print(str_c("OPGELET: Er bestaan geen normen voor '", component, "' in '",
                  str_flatten(d$Watertype, collapse = "' en '"), "'"))
    }
  }
  
  # Plot
  ######
  # Definieer titel, label y-as en schaal y-as obv component en dataset
  list <- Hydrochem_plot_titlelabs(data, component)
  if(title == TRUE){title <- list[[1]]}else{title <- NULL}
  laby <- list[[2]]
  limy <- list[[3]]
  
  # basic plot
  if(length(unique(data$Component)) > 1){ # is this possible??? script does not work with length(Component) > 1
    p <- 
      data %>%
      ggplot(aes(x = Monsternamedatum, y = Meetwaarde2, 
               col = MeetpuntCode, shape = Component))}
  else{
    p <- 
      data %>%
      ggplot(aes(x = Monsternamedatum, y = Meetwaarde2, 
                 col = MeetpuntCode))}
  p <-
    p +
    geom_point(size = 2) +
    labs(title = title, y = laby) +
    scale_x_datetime(date_labels = "%b-%Y") +
    theme(axis.text.x = element_text(angle = 90),
          axis.title.x = element_blank(),
          plot.title = element_text(hjust = 0.5, size = 16),
          legend.text = element_text(color = "black"))
  ## plot zonder BKN als watertype afwezig is of als geen norm bestaat voor component-watertype
  if(missing(watertype)|nrow(norm) == 0){ 
    p <- 
      p  +
      scale_y_continuous(limits = limy) }
  # Plot als watertype is opgegeven
  else{
    # Pas limy aan als norm_max > limy[2]
    if(max(norm$norm_max) > limy[2]){limy <- c(0, max(norm$norm_max))}
    
    if(component == "Cl" & "brakke polderwaterloop" %in% watertype){
      p <- 
        p +
        geom_hline(aes(yintercept = norm$norm_max, 
                       linetype = norm$Legendelabel),
                   color = "red", lwd = 0.5) +
        scale_linetype_manual(name = "Biol. kwaliteitsnorm", values = c(2)) +
        scale_y_continuous(limits = limy) }
    # pH has min and max => special treatment
    else if(component == "pH.25" & nrow(norm) == 1){
      p <- 
        p +
        # biologische kwaliteitsnorm(en)
        geom_hline(aes(yintercept = norm$norm_min[1],
                       linetype = norm$Legendelabel[1]),
                   color = "red", lwd = 0.5) +
        geom_hline(aes(yintercept = norm$norm_max[1],
                       linetype = norm$Legendelabel[1]),
                   color = "red", lwd = 0.5) +
        scale_linetype_manual(name = "Biol. kwaliteitsnorm", values = c(1,2)) +
        scale_y_continuous(limits = limy) }
    else if(component == "pH.25" & nrow(norm) == 2){
      p <- 
        p +
        # biologische kwaliteitsnorm(en)
        geom_hline(aes(yintercept = norm$norm_min[1],
                       linetype = norm$Legendelabel[1]),
                   color = "red", lwd = 0.5) +
        geom_hline(aes(yintercept = norm$norm_max[1],
                       linetype = norm$Legendelabel[1]),
                   color = "red", lwd = 0.5) +
        geom_hline(aes(yintercept = norm$norm_min[2],
                       linetype = norm$Legendelabel[2]),
                   color = "red", lwd = 0.5) +
        geom_hline(aes(yintercept = norm$norm_max[2],
                       linetype = norm$Legendelabel[2]),
                   color = "red", lwd = 0.5) +
        scale_linetype_manual(name = "Biol. kwaliteitsnorm", values = c(1,2)) +
        scale_y_continuous(limits = limy) }
    else if(component == "pH.25" & nrow(norm) == 3){
      p <- 
        p +
        # biologische kwaliteitsnorm(en)
        geom_hline(aes(yintercept = norm$norm_min[1],
                       linetype = norm$Legendelabel[1]),
                   color = "red", lwd = 0.5) +
        geom_hline(aes(yintercept = norm$norm_max[1],
                       linetype = norm$Legendelabel[1]),
                   color = "red", lwd = 0.5) +
        geom_hline(aes(yintercept = norm$norm_min[2],
                       linetype = norm$Legendelabel[2]),
                   color = "red", lwd = 0.5) +
        geom_hline(aes(yintercept = norm$norm_max[2],
                       linetype = norm$Legendelabel[2]),
                   color = "red", lwd = 0.5) +
        geom_hline(aes(yintercept = norm$norm_min[3],
                       linetype = norm$Legendelabel[3]),
                   color = "red", lwd = 0.5) +
        geom_hline(aes(yintercept = norm$norm_max[3],
                       linetype = norm$Legendelabel[3]),
                   color = "red", lwd = 0.5) +
        scale_linetype_manual(name = "Biol. kwaliteitsnorm", values = c(1,2,3)) +
        scale_y_continuous(limits = limy) }
    # all other parameters, up to tree watertypes
    else if(nrow(norm) == 1){
      p <- 
        p +
        geom_hline(aes(yintercept = norm$norm_min[1], 
                       linetype = norm$Legendelabel[1]),
                   color = "red", lwd = 0.5) +
        scale_linetype_manual(name = "Biol. kwaliteitsnorm", 
                              values = c(1))  +
        scale_y_continuous(limits = limy) }
    else if(nrow(norm) == 2){
      p <- 
        p +
        geom_hline(aes(yintercept = norm$norm_min[1], 
                       linetype = norm$Legendelabel[1]),
                   color = "red", lwd = 0.5) +
        geom_hline(aes(yintercept = norm$norm_min[2], 
                       linetype = norm$Legendelabel[2]),
                   color = "red", lwd = 0.5) +
        scale_linetype_manual(name = "Biol. kwaliteitsnorm", values = c(1, 2)) +
        scale_y_continuous(limits = limy)}
    else{
      p <- 
        p +
        geom_hline(aes(yintercept = norm$norm_min[1], 
                       linetype = norm$Legendelabel[1]),
                   color = "red", lwd = 0.5) +
        geom_hline(aes(yintercept = norm$norm_min[2], 
                       linetype = norm$Legendelabel[2]),
                   color = "red", lwd = 0.5) +
        geom_hline(aes(yintercept = norm$norm_min[3], 
                       linetype = norm$Legendelabel[3]),
                   color = "red", lwd = 0.5) +
        scale_linetype_manual(name = "Biol. kwaliteitsnorm", 
                              values = c(1, 2, 3)) +
        scale_y_continuous(limits = limy) }
  }
  return(p)
}


# Checks
VAR %>%
  Hydrochem_point_component(component = "Cl", watertype = "kleine beek")
VAR %>%
  Hydrochem_point_component(
    component = "NO3", 
    watertype = c("kleine beek", "circumneutraal, sterk gebufferd meer"))
VAR %>%
  Hydrochem_point_component(
    component = "NO3", 
    watertype = c("kleine beek", "circumneutraal, sterk gebufferd meer",
                  "matig ionenrijk, alkalisch meer"))

# (p <- VAR_DWH %>%
#   filter(str_starts(MeetpuntCode, "VARR|VARS")) %>%
#   Hydrochem_point_component(component = "pH.25",
#                                 watertype = "kleine beek Kempen"))
# plotly::ggplotly(p, dynamicTicks = TRUE)
# (p <- VAR_DWH %>%
#     filter(str_starts(MeetpuntCode, "VARR|VARS")) %>%
#     Hydrochem_point_component(component = "Saturatie",
#                               watertype = "kleine beek Kempen"))
# plotly::ggplotly(p, dynamicTicks = TRUE)
# 
# VAR_DWH %>% 
#   filter(str_starts(MeetpuntCode, "VARR|VARS")) %>%
#   Hydrochem_point_component(component = "PO4", watertype = "kleine beek Kempen")
# 
# (p <- 
#   VAR_DWH %>% 
#   filter(str_starts(MeetpuntCode, "VARR|VARS")) %>%
#   Hydrochem_point_component(component = "T.P", watertype = "kleine beek Kempen"))
# plotly::ggplotly(p, dynamicTicks = TRUE)

#----------------------------------------------------------------------------------------------























# Plot twee componenten met visuele aanduiding normoverschreidingen

hydrochem_plot2component <- function(data, component1, component2, watertype){
  #filter data
  plotdata <- 
    data %>% 
    filter(Component %in% c(component1, component2)) %>% 
    select(Monsternamedatum, MeetpuntCode, Component, Doelstelling, Sigma_cluster,
           Watertype, GebiedNaam, Meetwaarde2) %>%
    spread(key = Component, value = Meetwaarde2) %>% 
    filter(!is.na(.data[[component1]]), !is.na(.data[[component2]]))

  BKN <- 
    read_csv("C:/R/Projects/Hydrochem/data/BKNforGGplot.csv", lazy = FALSE, 
             show_col_types = FALSE) %>% 
    mutate(norm_min = ifelse(Component == "EC.20", norm_min*1.116, norm_min),
           norm_max = ifelse(Component == "EC.20", norm_max*1.116, norm_max),
           Component = replace(Component, Component == "EC.20", "EC.25"),
           Parameter = replace(Parameter, Parameter == "EGV (20°C)", "EGV (25°C)"))
  # Add Sat_calc en O2_calc
  a <- 
    BKN %>%  
    filter(Component == "Saturatie") %>% 
    mutate(Component = "Sat_calc")
  
  b <- 
    BKN%>% 
    filter(Component == "O2_veld") %>% 
    mutate(Component = "O2_calc") 
  
  BKN <- bind_rows(BKN, a, b)
  
  # selecteer normen
  BKNx_min <-
    BKN[which(BKN$Watertype_kort == watertype & BKN$Component == component1),]$norm_min
  BKNx_max <-
    BKN[which(BKN$Watertype_kort == watertype & BKN$Component == component1),]$norm_max
  BKNy_min <-
    BKN[which(BKN$Watertype_kort == watertype & BKN$Component == component2),]$norm_min
  BKNy_max <-
    BKN[which(BKN$Watertype_kort == watertype & BKN$Component == component2),]$norm_max
  
  # selecteer maximale waarden in dataset
  min_x <-  min(data[which(data$Component == component1),]$Meetwaarde2, na.rm = TRUE)
  max_x <-  max(data[which(data$Component == component1),]$Meetwaarde2, na.rm = TRUE)
  min_y <-  min(data[which(data$Component == component2),]$Meetwaarde2, na.rm = TRUE)
  max_y <-  max(data[which(data$Component == component2),]$Meetwaarde2, na.rm = TRUE)
  
  if(component1 %in% c("O2_veld", "O2_calc")){
    # grenzen rect1
    xmin1 = 0
    xmax1 = BKNx_min
    ymin1 = 0
    ymax1 = 1.02*max_y
    # grenzen rect2
    xmin2 = 0
    xmax2 = 1.02*max_x
    ymin2 = BKNy_min
    ymax2 = 1.02*max_y
    # grenzen rect3
    xmin3 = BKNx_min
    xmax3 = 1.02*max_x
    ymin3 = 0
    ymax3 = ymin2
    
  }
  else if(component2 %in% c("O2_veld", "O2_calc")){
    # grenzen rect1
    xmin1 = 0
    xmax1 = 1.02*max_x
    ymin1 = 0
    ymax1 = BKNy_min
    # grenzen rect2
    xmin2 = BKNx_min
    xmax2 = 1.02*max_x
    ymin2 = 0
    ymax2 = 1.02*max_y
    # grenzen rect3
    xmin3 = 0
    xmax3 = BKNx_min
    ymin3 = BKNy_min
    ymax3 = 1.02*max_y
  }
  else if(component1 == "pH.25"){
    # grenzen rect1
    xmin1 = 0.98*min_x
    xmax1 = BKNx_min
    ymin1 = 0
    ymax1 = 1.02*max_y
    # grenzen rect2
    xmin2 = BKNx_max
    xmax2 = 1.02*max_x
    ymin2 = 0
    ymax2 = 1.02*max_y
    # grenzen rect3
    xmin3 = 0.98*min_x
    xmax3 = 1.02*max_x
    ymin3 = BKNy_min
    ymax3 = 1.02*max_y
    # grenzen rect4
    xmin4 = BKNx_min
    xmax4 = BKNx_max
    ymin4 = 0
    ymax4 = BKNy_min
  }
  # else if(component2 == "pH.25"){
  #     
  # }
  else{
    xmin1 = BKNx_min
    xmax1 = 1.02*max_x
    ymin1 = 0
    ymax1 = 1.02*max_y
    # grenzen rect2
    xmin2 = 0
    xmax2 = 1.02*max_x
    ymin2 = BKNy_min
    ymax2 = 1.02*max_y
    # grenzen rect3
    xmin3 = 0
    xmax3 = xmin1
    ymin3 = 0
    ymax3 = ymin2
  }
  
  # axis titles
  BKNx_comp <- first(BKN[which(BKN$Component == component1),]$Parameter)
  BKNx_eenh <- first(BKN[which(BKN$Component == component1),]$Eenheid)
  BKNy_comp <- first(BKN[which(BKN$Component == component2),]$Parameter)
  BKNy_eenh <- first(BKN[which(BKN$Component == component2),]$Eenheid)
  
  xlab <- paste0(BKNx_comp, " (", BKNx_eenh, ")")
  ylab <- paste0(BKNy_comp, " (", BKNy_eenh, ")")

  
  if(component1 == "pH.25"){
    ggplot(data = plotdata, aes(label = MeetpuntCode)) +
      geom_rect(data = data.frame(x = 0, y = 0, MeetpuntCode =""),
                aes(xmin = xmin1, xmax = xmax1, ymin = ymin1, ymax = ymax1),
                fill = "red", alpha = 0.5) +
      geom_rect(data = data.frame(x = 0, y = 0, MeetpuntCode =""),
                aes(xmin = xmin2, xmax = xmax2, ymin = ymin2, ymax = ymax2),
                fill = "red", alpha = 0.5) +
      geom_rect(data = data.frame(x = 0, y = 0, MeetpuntCode =""),
                aes(xmin = xmin3, xmax = xmax3, ymin = ymin3, ymax = ymax3),
                fill = "red", alpha = 0.5) +
      geom_rect(data = data.frame(x = 0, y = 0, MeetpuntCode =""),
                aes(xmin = xmin4, xmax = xmax4, ymin = ymin4, ymax = ymax4),
                fill = "deepskyblue", alpha = 0.75, MeetpuntCode ="") +
      geom_point(data = plotdata, aes(x = .data[[component1]], y = .data[[component2]])) +
      labs(x = xlab, y = ylab, title = watertype)} 
  else {
    ggplot(data = plotdata, aes(label = MeetpuntCode)) +
      geom_rect(data = data.frame(x = 0, y = 0, MeetpuntCode =""),
                aes(xmin = xmin1, xmax = xmax1, ymin = ymin1, ymax = ymax1),
                fill = "red", alpha = 0.5) +
      geom_rect(data = data.frame(x = 0, y = 0, MeetpuntCode =""),
                aes(xmin = xmin2, xmax = xmax2, ymin = ymin2, ymax = ymax2),
                fill = "red", alpha = 0.5) +
      geom_rect(data = data.frame(x = 0, y = 0, MeetpuntCode =""),
                aes(xmin = xmin3, xmax = xmax3, ymin = ymin3, ymax = ymax3),
                fill = "deepskyblue", alpha = 0.75) +
      geom_point(data = plotdata, aes(x = .data[[component1]], y = .data[[component2]])) +
      labs(x = xlab, y = ylab, title = watertype)
  }
}


#########################################
hydrochem_plot2component2 <- function(data, component1, component2, watertype){
  #filter data
  plotdata <- 
    data %>% 
    filter(Component %in% c(component1, component2)) %>% 
    select(MeetpuntCode, Jaar, Watertype_kort, Component, Value, Sigma_cluster) %>% 
    spread(key = Component, value = Value) %>% 
    filter(!is.na(.data[[component1]]), !is.na(.data[[component2]]))
  
  BKN <- 
    read_csv("C:/R/Projects/Hydrochem/data/BKNforGGplot.csv", lazy = FALSE, 
             show_col_types = FALSE) %>% 
    mutate(norm_min = ifelse(Component == "EC.20", norm_min*1.116, norm_min),
           norm_max = ifelse(Component == "EC.20", norm_max*1.116, norm_max),
           Component = replace(Component, Component == "EC.20", "EC.25"),
           Parameter = replace(Parameter, Parameter == "EGV (20°C)", "EGV (25°C)"))
  # Add Sat_calc en O2_calc
  a <- 
    BKN %>%  
    filter(Component == "Saturatie") %>% 
    mutate(Component = "Sat_calc")
  
  b <- 
    BKN %>% 
    filter(Component == "O2_veld") %>% 
    mutate(Component = "O2_calc") 
  
  BKN <- bind_rows(BKN, a, b)
  
  # selecteer normen
  BKNx_min <-
    BKN[which(BKN$Watertype_kort == watertype & BKN$Component == component1),]$norm_min
  BKNx_max <-
    BKN[which(BKN$Watertype_kort == watertype & BKN$Component == component1),]$norm_max
  BKNy_min <-
    BKN[which(BKN$Watertype_kort == watertype & BKN$Component == component2),]$norm_min
  BKNy_max <-
    BKN[which(BKN$Watertype_kort == watertype & BKN$Component == component2),]$norm_max
  
  # selecteer maximale waarden in dataset
  min_x <-  min(data[which(data$Component == component1),]$Value, na.rm = TRUE)
  max_x <-  max(data[which(data$Component == component1),]$Value, na.rm = TRUE)
  min_y <-  min(data[which(data$Component == component2),]$Value, na.rm = TRUE)
  max_y <-  max(data[which(data$Component == component2),]$Value, na.rm = TRUE)
  
  if(component1 %in% c("O2_veld", "O2_calc")){
    # grenzen rect1
    xmin1 = 0
    xmax1 = BKNx_min
    ymin1 = 0
    ymax1 = 1.02*max_y
    # grenzen rect2
    xmin2 = 0
    xmax2 = 1.02*max_x
    ymin2 = BKNy_min
    ymax2 = 1.02*max_y
    # grenzen rect3
    xmin3 = BKNx_min
    xmax3 = 1.02*max_x
    ymin3 = 0
    ymax3 = ymin2
    
  }
  else if(component2 %in% c("O2_veld", "O2_calc")){
    # grenzen rect1
    xmin1 = 0
    xmax1 = 1.02*max_x
    ymin1 = 0
    ymax1 = BKNy_min
    # grenzen rect2
    xmin2 = BKNx_min
    xmax2 = 1.02*max_x
    ymin2 = 0
    ymax2 = 1.02*max_y
    # grenzen rect3
    xmin3 = 0
    xmax3 = BKNx_min
    ymin3 = BKNy_min
    ymax3 = 1.02*max_y
  }
  else if(component1 == "pH.25"){
    # grenzen rect1
    xmin1 = 0.98*min_x
    xmax1 = BKNx_min
    ymin1 = 0
    ymax1 = 1.02*max_y
    # grenzen rect2
    xmin2 = BKNx_max
    xmax2 = 1.02*max_x
    ymin2 = 0
    ymax2 = 1.02*max_y
    # grenzen rect3
    xmin3 = 0.98*min_x
    xmax3 = 1.02*max_x
    ymin3 = BKNy_min
    ymax3 = 1.02*max_y
    # grenzen rect4
    xmin4 = BKNx_min
    xmax4 = BKNx_max
    ymin4 = 0
    ymax4 = BKNy_min
  }
  # else if(component2 == "pH.25"){
  #     
  # }
  else{
    xmin1 = BKNx_min
    xmax1 = 1.02*max_x
    ymin1 = 0
    ymax1 = 1.02*max_y
    # grenzen rect2
    xmin2 = 0
    xmax2 = 1.02*max_x
    ymin2 = BKNy_min
    ymax2 = 1.02*max_y
    # grenzen rect3
    xmin3 = 0
    xmax3 = xmin1
    ymin3 = 0
    ymax3 = ymin2
  }
  
  # axis titles
  BKNx_comp <- first(BKN[which(BKN$Component == component1),]$Parameter)
  BKNx_eenh <- first(BKN[which(BKN$Component == component1),]$Eenheid)
  BKNy_comp <- first(BKN[which(BKN$Component == component2),]$Parameter)
  BKNy_eenh <- first(BKN[which(BKN$Component == component2),]$Eenheid)
  
  xlab <- paste0(BKNx_comp, " (", BKNx_eenh, ")")
  ylab <- paste0(BKNy_comp, " (", BKNy_eenh, ")")
  
  
  if(component1 == "pH.25"){
    ggplot(data = plotdata, aes(label = MeetpuntCode)) +
      geom_rect(data = data.frame(x = 0, y = 0, MeetpuntCode =""),
                aes(xmin = xmin1, xmax = xmax1, ymin = ymin1, ymax = ymax1),
                fill = "red", alpha = 0.5) +
      geom_rect(data = data.frame(x = 0, y = 0, MeetpuntCode =""),
                aes(xmin = xmin2, xmax = xmax2, ymin = ymin2, ymax = ymax2),
                fill = "red", alpha = 0.5) +
      geom_rect(data = data.frame(x = 0, y = 0, MeetpuntCode =""),
                aes(xmin = xmin3, xmax = xmax3, ymin = ymin3, ymax = ymax3),
                fill = "red", alpha = 0.5) +
      geom_rect(data = data.frame(x = 0, y = 0, MeetpuntCode =""),
                aes(xmin = xmin4, xmax = xmax4, ymin = ymin4, ymax = ymax4),
                fill = "deepskyblue", alpha = 0.75, MeetpuntCode ="") +
      geom_point(data = plotdata, aes(x = .data[[component1]], y = .data[[component2]])) +
      labs(x = xlab, y = ylab, title = watertype)} 
  else {
    ggplot(data = plotdata, aes(label = MeetpuntCode)) +
      geom_rect(data = data.frame(x = 0, y = 0, MeetpuntCode =""),
                aes(xmin = xmin1, xmax = xmax1, ymin = ymin1, ymax = ymax1),
                fill = "red", alpha = 0.5) +
      geom_rect(data = data.frame(x = 0, y = 0, MeetpuntCode =""),
                aes(xmin = xmin2, xmax = xmax2, ymin = ymin2, ymax = ymax2),
                fill = "red", alpha = 0.5) +
      geom_rect(data = data.frame(x = 0, y = 0, MeetpuntCode =""),
                aes(xmin = xmin3, xmax = xmax3, ymin = ymin3, ymax = ymax3),
                fill = "deepskyblue", alpha = 0.75) +
      geom_point(data = plotdata, aes(x = .data[[component1]], y = .data[[component2]])) +
      labs(x = xlab, y = ylab, title = watertype)
 
  }
}
#############################################
# hydrochem_plot2component_leg
##############################
# Met deze functie kan aan de punten een legende toegekend worden op basis van legende. legende kan volgende waarden hebben:
# => Seizoen (onderscheid tussen zomer en wintermetingen)
# => Watertype
# => Doelstelling
# => Sigma_cluster

hydrochem_plot2component_leg <- function(data, component1, component2, watertype, legende){
  #filter data
  plotdata <- 
    data %>% 
    filter(Component %in% c(component1, component2)) %>% 
    mutate( Seizoen = ifelse(month(Monsternamedatum) %in% c(4, 5, 6, 7, 8, 9), "zomer", "winter")) %>% 
    select(Monsternamedatum, MeetpuntCode, GebiedCode, Component, Meetwaarde2, legende) %>% 
    spread(key = Component, value = Meetwaarde2) %>% 
    filter(!is.na(.data[[component1]]), !is.na(.data[[component2]]))
  BKN <- 
    read_csv("C:/R/Projects/Hydrochem/data/BKNforGGplot.csv", lazy = FALSE, 
             show_col_types = FALSE) %>% 
    mutate(norm_min = ifelse(Component == "EC.20", norm_min*1.116, norm_min),
           norm_max = ifelse(Component == "EC.20", norm_max*1.116, norm_max),
           Component = replace(Component, Component == "EC.20", "EC.25"),
           Parameter = replace(Parameter, Parameter == "EGV (20°C)", "EGV (25°C)"))
  # Add Sat_calc en O2_calc
  a <- 
    BKN %>%  
    filter(Component == "Saturatie") %>% 
    mutate(Component = "Sat_calc")
  
  b <- 
    BKN%>% 
    filter(Component == "O2_veld") %>% 
    mutate(Component = "O2_calc") 
  
  BKN <- bind_rows(BKN, a, b)
  
  # selecteer normen
  BKNx_min <-
    BKN[which(BKN$Watertype_kort == watertype & BKN$Component == component1),]$norm_min
  BKNx_max <-
    BKN[which(BKN$Watertype_kort == watertype & BKN$Component == component1),]$norm_max
  BKNy_min <-
    BKN[which(BKN$Watertype_kort == watertype & BKN$Component == component2),]$norm_min
  BKNy_max <-
    BKN[which(BKN$Watertype_kort == watertype & BKN$Component == component2),]$norm_max
  
  # selecteer maximale waarden in dataset
  min_x <-  min(data[which(data$Component == component1),]$Meetwaarde2, na.rm = TRUE)
  max_x <-  max(data[which(data$Component == component1),]$Meetwaarde2, na.rm = TRUE)
  min_y <-  min(data[which(data$Component == component2),]$Meetwaarde2, na.rm = TRUE)
  max_y <-  max(data[which(data$Component == component2),]$Meetwaarde2, na.rm = TRUE)
  
  if(component1 == "O2_veld"){
    # grenzen rect1
    xmin1 = 0
    xmax1 = BKNx_min
    ymin1 = 0
    ymax1 = 1.02*max_y
    # grenzen rect2
    xmin2 = 0
    xmax2 = 1.02*max_x
    ymin2 = BKNy_min
    ymax2 = 1.02*max_y
    # grenzen rect3
    xmin3 = BKNx_min
    xmax3 = 1.02*max_x
    ymin3 = 0
    ymax3 = ymin2
    
  }
  else if(component2 == "O2_veld"){
    # grenzen rect1
    xmin1 = 0
    xmax1 = 1.02*max_x
    ymin1 = 0
    ymax1 = BKNy_min
    # grenzen rect2
    xmin2 = BKNx_min
    xmax2 = 1.02*max_x
    ymin2 = 0
    ymax2 = 1.02*max_y
    # grenzen rect3
    xmin3 = 0
    xmax3 = BKNx_min
    ymin3 = BKNy_min
    ymax3 = 1.02*max_y
  }
  else if(component1 == "pH.25"){
    # grenzen rect1
    xmin1 = 0.98*min_x
    xmax1 = BKNx_min
    ymin1 = 0
    ymax1 = 1.02*max_y
    # grenzen rect2
    xmin2 = BKNx_max
    xmax2 = 1.02*max_x
    ymin2 = 0
    ymax2 = 1.02*max_y
    # grenzen rect3
    xmin3 = 0.98*min_x
    xmax3 = 1.02*max_x
    ymin3 = BKNy_min
    ymax3 = 1.02*max_y
    # grenzen rect4
    xmin4 = BKNx_min
    xmax4 = BKNx_max
    ymin4 = 0
    ymax4 = BKNy_min
  }
  # else if(component2 == "pH.25"){
  #     
  # }
  else{
    xmin1 = BKNx_min
    xmax1 = 1.02*max_x
    ymin1 = 0
    ymax1 = 1.02*max_y
    # grenzen rect2
    xmin2 = 0
    xmax2 = 1.02*max_x
    ymin2 = BKNy_min
    ymax2 = 1.02*max_y
    # grenzen rect3
    xmin3 = 0
    xmax3 = xmin1
    ymin3 = 0
    ymax3 = ymin2
  }
  
  # axis titles
  BKNx_comp <- first(BKN[which(BKN$Component == component1),]$Parameter)
  BKNx_eenh <- first(BKN[which(BKN$Component == component1),]$Eenheid)
  BKNy_comp <- first(BKN[which(BKN$Component == component2),]$Parameter)
  BKNy_eenh <- first(BKN[which(BKN$Component == component2),]$Eenheid)
  
  xlab <- paste0(BKNx_comp, " (", BKNx_eenh, ")")
  ylab <- paste0(BKNy_comp, " (", BKNy_eenh, ")")
  
  
  if(component1 == "pH.25"){
    p <- 
      ggplot(data = plotdata, aes(label = MeetpuntCode)) +
      geom_rect(data = data.frame(x = 0, y = 0, MeetpuntCode = ""),
                aes(xmin = xmin1, xmax = xmax1, ymin = ymin1, ymax = ymax1),
                fill = "red", alpha = 0.5) +
      geom_rect(data = data.frame(x = 0, y = 0, MeetpuntCode = ""),
                aes(xmin = xmin2, xmax = xmax2, ymin = ymin2, ymax = ymax2),
                fill = "red", alpha = 0.5) +
      geom_rect(data = data.frame(x = 0, y = 0, MeetpuntCode = ""),
                aes(xmin = xmin3, xmax = xmax3, ymin = ymin3, ymax = ymax3),
                fill = "red", alpha = 0.5) +
      geom_rect(data = data.frame(x = 0, y = 0, MeetpuntCode = ""),
                aes(xmin = xmin4, xmax = xmax4, ymin = ymin4, ymax = ymax4),
                fill = "deepskyblue", alpha = 0.5) } 
  else {
    p <- 
      ggplot(data = plotdata, aes(label = MeetpuntCode)) +
      geom_rect(data = data.frame(x = 0, y = 0, MeetpuntCode = ""),
                aes(xmin = xmin1, xmax = xmax1, ymin = ymin1, ymax = ymax1),
                fill = "red", alpha = 0.5) +
      geom_rect(data = data.frame(x = 0, y = 0, MeetpuntCode = ""),
                aes(xmin = xmin2, xmax = xmax2, ymin = ymin2, ymax = ymax2),
                fill = "red", alpha = 0.5) +
      geom_rect(data = data.frame(x = 0, y = 0, MeetpuntCode = ""),
                aes(xmin = xmin3, xmax = xmax3, ymin = ymin3, ymax = ymax3),
                fill = "deepskyblue", alpha = 0.5) 
  }
  
  # palettes voor punten
  pal_type <- inbo_palette()
    # c("darkblue", "turquoise4", "blue", "purple")
  pal_doelstelling = c("black", "turquoise3", "green", "darkblue")
  pal_cluster = inbo_palette()
  if(legende == "Watertype"){
    pal <- pal_type
  }
  else if(legende == "Doelstelling"){
    pal <- pal_doelstelling
  }
  else if(legende == "Sigma_cluster"){
    pal <- pal_cluster
  }

  if(legende == "Seizoen"){
    p  +
      geom_point(aes(x = .data[[component1]], y = .data[[component2]], shape = .data[[legende]]),
                 col = "black", stroke = 1, alpha = 1, size = 2) +
      labs(x = xlab, y = ylab, title = watertype) +
      scale_shape(name = "Seizoen staalname")
    }
  else{
    p  +
    geom_point(aes(x = .data[[component1]], y = .data[[component2]], fill = .data[[legende]]),
               size = 2, shape = 21, col = "black", stroke = 1, alpha = 1) +
    labs(x = xlab, y = ylab, title = watertype) +
    scale_fill_manual(values = pal)
  }
}

# data_pw %>% 
#   hydrochem_plot2component_leg("T.N", "T.P", "Ionenrijk alkalisch meer", legende = "Seizoen")
# data_opp %>% 
#   hydrochem_plot2component_leg("T.N", "T.P", "Ionenrijk alkalisch meer", legende = "Sigma_cluster")




#---------------------------------------------------------------------------------
# Groundwater
#############

# # read and restructure AchtergrondwaardenGrondwater.xlsx
# p <- "G:/Gedeelde drives/PRJ_SCHELDE_SIGMA/MonitoringSigmaplan/Data/Abiotiek/Normen/AchtergrondwaardenGrondwater.xlsx"
# # read first row and make names of it
# (cnames <- readxl::read_excel(p, sheet = "Achtergrondwaarden", n_max = 0) %>%
#     names())
# # read rows 3  tot 46 dan add names
# GRW <- readxl::read_excel(p, sheet = "Achtergrondwaarden", range = "a3:ab46",
#                           col_types = c("text", rep("numeric", 27)))
# names(GRW) <- cnames
# # read row 2 as Eenheden
# Eenh <-
#   readxl::read_excel(p, sheet = "Achtergrondwaarden", range = "a1:ab2" ) %>%
#   pivot_longer(cols = !parameter, names_to = "Component",
#                values_to = "Eenheid") %>%
#   rename(Grondwaterlichaam = parameter) %>%
#   mutate(Eenheid = ifelse(Eenheid == "Sörensen", NA_character_, Eenheid)) %>%
#   select(-Grondwaterlichaam)
# # add Eenheden and make long format
# GRW_l <-
#   GRW %>%
#   filter(parameter != "Gemiddelde") %>%
#   pivot_longer(!parameter, names_to = "Component",
#                values_to = "Richtwaarde") %>%
#   rename(Grondwaterlichaam = parameter) %>%
#   left_join(Eenh, by = c("Component")) %>% 
#   mutate(Component2 = 
#            case_when(Component == "pH min" ~ "pH",
#                      Component == "pH max" ~ "pH",
#                      Component == "Ca2+" ~ "Ca",
#                      Component == "Cl-" ~ "Cl",
#                      Component == "Ec" ~ "EC.20",
#                      Component == "Fe2+/3+" ~ "Fe",
#                      Component == "K+" ~ "K",
#                      Component == "Mg2+" ~ "Mg",
#                      Component == "Mn2+/3+/4+/7+" ~ "Mn",
#                      Component == "Na+" ~ "Na",
#                      Component == "NH4+" ~ "NH4",
#                      Component == "NO2-" ~ "NO2",
#                      Component == "NO3-" ~ "NO3",
#                      Component == "PO4-/ 2-/3-" ~ "PO4",
#                      Component == "SO42-" ~ "SO4",
#                      .default = Component
#                      ))
# 
# GRW_l <- 
#   GRW_l %>% 
#   arrange(Component) %>% 
#   group_by(Grondwaterlichaam, Component2) %>% 
#   summarise(Richtwaarde_min = last(Richtwaarde),
#             Richtwaarde_max = first(Richtwaarde),
#             .groups = "drop") %>% 
#   rename(Component = Component2)
# 
# GRW_l %>% write_csv(file = "data/BKNGWforggplot.csv" )



# puntgrafiek
Hydrochem_point_component_gw <- 
  function(data, component, gw_lichaam = "Algemene Norm - richtwaarde", 
           title = TRUE){
    # lees GRW (grondwater richtwaarde) bestand in en maak tabel norm aan obv waterlichaam en component 
    p <- "C:/R/Projects/Hydrochem/"
    GRW <- 
      read_csv(str_c(p, "data/BKNGWforggplot.csv"), show_col_types = FALSE)
    # add richwaarde for EC.25
    GRW <-
      GRW %>%
      filter(Component == "EC.20") %>%
      # reken norm EC.20 om naar EC.25
      mutate(Richtwaarde_min = Richtwaarde_min*1.116,
             Richtwaarde_max = Richtwaarde_max*1.116,
             Component = "EC.25") %>%
      bind_rows(GRW)

    norm <- GRW %>%
      filter(Grondwaterlichaam %in% gw_lichaam, Component == component)
    
    # filter data
    data <- data %>%
      filter(Component %in% component)
    # 
    # make vectors of possible values for parameter (= Component) and norm (= Watertype)
    cde <-
      read_csv("C:/R/Projects/Hydrochem/data/cde_Parameter_CodeVMM.csv",
               lazy = FALSE, show_col_types = FALSE) %>%
      rename(CodeVMM = Code) %>%
      mutate(Component = case_when(Component == "EC.20" ~ "EC.25",
                                   .default = Component))
    Component <- unique(cde$Component)
    norms <- unique(GRW$Grondwaterlichaam)

    # Sanity checks
    #-------------
    # data: moet dataframe zijn
    if(!is.data.frame(data)){stop("data must be data.frame")}

    ## gw_lichaam: moet uit Grondwaterlichaam komen
    if(length(gw_lichaam) > 1){stop("Choose one value for gw_lichaam!")}
    normstr <-
      str_c("gw_lichaam is wrong \n", "must be one of \"",
            str_c(norms, collapse = "\", \""), "\".")
    if(as.logical(sum(!gw_lichaam %in% norms))){stop(normstr)}

    ## component: moet uit vector Component komen
    Componentstr <-
      str_c("component must be one of \"",
            str_c(Component, collapse = "\", \""), "\".")
    if(!component %in% Component){stop(Componentstr)}

    ## Parameter: moet aanwezig zijn in data
    parameterstr <-
      str_c("No data for ", component, " found in data source")
    if(!component %in% data$Component){stop(parameterstr)}
    # Waarschuwing dat geen norm bestaat voor deze component
    # if(is.na(norm$Richtwaarde_min)){
    #   print(str_c("OPGELET: Er bestaat geen richtwaarde voor ", 
    #               component, " in dit grondwaterlichaam"))}
    
    # If a specific grondwaterlichaam is chosen => Algemene norm + specific norm is shown
    # therefor add "Algemene Norm - richtwaarde" and filter norm anew
    if(gw_lichaam != "Algemene Norm - richtwaarde"){
      gw_lichaam <- c("Algemene Norm - richtwaarde", gw_lichaam)
    }
    
    norm <- GRW %>%
      filter(Grondwaterlichaam %in% gw_lichaam, Component == component)
    
    # # Plot
    # ######
    # # Definieer titel, label y-as en schaal y-as obv component en dataset
    # list <- Hydrochem_plot_titlelabs(data, component)
    # if(title == TRUE){title <- list[[1]]}else{title <- NULL}
    # laby <- list[[2]]
    # limy <- list[[3]]
    # 
    # # basic plot
    # if(length(unique(data$Component)) > 1){
    #   p <- 
    #     data %>%
    #     ggplot(aes(x = Monsternamedatum, y = Meetwaarde2, 
    #                col = MeetpuntCode, shape = Component))}
    # else{
    #   p <- 
    #     data %>%
    #     ggplot(aes(x = Monsternamedatum, y = Meetwaarde2, 
    #                col = MeetpuntCode))}
    # p <-
    #   p +
    #   geom_point(size = 2) +
    #   labs(title = title, y = laby) +
    #   scale_x_datetime(date_labels = "%b-%Y") +
    #   theme(axis.text.x = element_text(angle = 90),
    #         axis.title.x = element_blank(),
    #         plot.title = element_text(hjust = 0.5, size = 16),
    #         legend.text = element_text(color = "black"))
    # ## plot zonder BKN als watertype afwezig is of als geen norm bestaat voor component-watertype
    # if(missing(watertype)|nrow(norm) == 0){ 
    #   p <- 
    #     p  +
    #     scale_y_continuous(limits = limy) }
    # # Plot als watertype is opgegeven
    # else{
    #   # Pas limy aan als norm_max > limy[2]
    #   if(max(norm$norm_max) > limy[2]){limy <- c(0, max(norm$norm_max))}
    #   
    #   if(component == "Cl" & "brakke polderwaterloop" %in% watertype){
    #     p <- 
    #       p +
    #       geom_hline(aes(yintercept = norm$norm_max, 
    #                      linetype = norm$Legendelabel),
    #                  color = "red", lwd = 0.5) +
    #       scale_linetype_manual(name = "Biol. kwaliteitsnorm", values = c(2)) +
    #       scale_y_continuous(limits = limy) }
    #   # pH has min and max => special treatment
    #   else if(component == "pH.25" & length(watertype) == 1){
    #     p <- 
    #       p +
    #       # biologische kwaliteitsnorm(en)
    #       geom_hline(aes(yintercept = norm$norm_min[1],
    #                      linetype = norm$Legendelabel[1]),
    #                  color = "red", lwd = 0.5) +
    #       geom_hline(aes(yintercept = norm$norm_max[1],
    #                      linetype = norm$Legendelabel[1]),
    #                  color = "red", lwd = 0.5) +
    #       scale_linetype_manual(name = "Biol. kwaliteitsnorm", values = c(1,2)) +
    #       scale_y_continuous(limits = limy) }
    #   else if(component == "pH.25" & length(watertype) == 2){
    #     p <- 
    #       p +
    #       # biologische kwaliteitsnorm(en)
    #       geom_hline(aes(yintercept = norm$norm_min[1],
    #                      linetype = norm$Legendelabel[1]),
    #                  color = "red", lwd = 0.5) +
    #       geom_hline(aes(yintercept = norm$norm_max[1],
    #                      linetype = norm$Legendelabel[1]),
    #                  color = "red", lwd = 0.5) +
    #       geom_hline(aes(yintercept = norm$norm_min[2],
    #                      linetype = norm$Legendelabel[2]),
    #                  color = "red", lwd = 0.5) +
    #       geom_hline(aes(yintercept = norm$norm_max[2],
    #                      linetype = norm$Legendelabel[2]),
    #                  color = "red", lwd = 0.5) +
    #       scale_linetype_manual(name = "Biol. kwaliteitsnorm", values = c(1,2)) +
    #       scale_y_continuous(limits = limy) }
    #   else if(component == "pH.25" & length(watertype) == 3){
    #     p <- 
    #       p +
    #       # biologische kwaliteitsnorm(en)
    #       geom_hline(aes(yintercept = norm$norm_min[1],
    #                      linetype = norm$Legendelabel[1]),
    #                  color = "red", lwd = 0.5) +
    #       geom_hline(aes(yintercept = norm$norm_max[1],
    #                      linetype = norm$Legendelabel[1]),
    #                  color = "red", lwd = 0.5) +
    #       geom_hline(aes(yintercept = norm$norm_min[2],
    #                      linetype = norm$Legendelabel[2]),
    #                  color = "red", lwd = 0.5) +
    #       geom_hline(aes(yintercept = norm$norm_max[2],
    #                      linetype = norm$Legendelabel[2]),
    #                  color = "red", lwd = 0.5) +
    #       geom_hline(aes(yintercept = norm$norm_min[3],
    #                      linetype = norm$Legendelabel[3]),
    #                  color = "red", lwd = 0.5) +
    #       geom_hline(aes(yintercept = norm$norm_max[3],
    #                      linetype = norm$Legendelabel[3]),
    #                  color = "red", lwd = 0.5) +
    #       scale_linetype_manual(name = "Biol. kwaliteitsnorm", values = c(1,2,3)) +
    #       scale_y_continuous(limits = limy) }
    #   # all other parameters, up to tree watertypes
    #   else if(length(watertype) == 1){
    #     p <- 
    #       p +
    #       geom_hline(aes(yintercept = norm$norm_min[1], 
    #                      linetype = norm$Legendelabel[1]),
    #                  color = "red", lwd = 0.5) +
    #       scale_linetype_manual(name = "Biol. kwaliteitsnorm", 
    #                             values = c(1))  +
    #       scale_y_continuous(limits = limy) }
    #   else if(length(watertype) == 2){
    #     p <- 
    #       p +
    #       geom_hline(aes(yintercept = norm$norm_min[1], 
    #                      linetype = norm$Legendelabel[1]),
    #                  color = "red", lwd = 0.5) +
    #       geom_hline(aes(yintercept = norm$norm_min[2], 
    #                      linetype = norm$Legendelabel[2]),
    #                  color = "red", lwd = 0.5) +
    #       scale_linetype_manual(name = "Biol. kwaliteitsnorm", values = c(1, 2)) +
    #       scale_y_continuous(limits = limy)}
    #   else{
    #     p <- 
    #       p +
    #       geom_hline(aes(yintercept = norm$norm_min[1], 
    #                      linetype = norm$Legendelabel[1]),
    #                  color = "red", lwd = 0.5) +
    #       geom_hline(aes(yintercept = norm$norm_min[2], 
    #                      linetype = norm$Legendelabel[2]),
    #                  color = "red", lwd = 0.5) +
    #       geom_hline(aes(yintercept = norm$norm_min[3], 
    #                      linetype = norm$Legendelabel[3]),
    #                  color = "red", lwd = 0.5) +
    #       scale_linetype_manual(name = "Biol. kwaliteitsnorm", 
    #                             values = c(1, 2, 3)) +
    #       scale_y_continuous(limits = limy) }
    # }
    return(norm)
  }

# Tests

# VAR_DWH %>% 
#   filter(str_detect(MeetpuntCode, "VARP")) %>% 
#   Hydrochem_point_component_gw(component = "pH.25", gw_lichaam = "CKS_0200_GWL_1")


