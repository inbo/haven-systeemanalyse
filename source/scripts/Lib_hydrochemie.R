
# Atoommassa's, moleculaire massa's en valenties --------------------------------------------

# Atoommass's
M.H <- 	1.0079
M.C <- 12.011
M.N <- 14.0067
M.O <- 15.999
M.Na <- 22.990
M.K <- 39.098
M.Ca <- 40.078
M.Mg <- 24.305
M.Fe <- 55.845
M.P <- 30.974
M.S <- 32.06
M.Cl <- 35.453
M.Si <- 28.086
M.Al <- 26.9815
M.Mn <- 54.938

M.HCO3 <- M.H + M.C + 3*M.O
M.H2CO3 <- 2*M.H + M.C + 3*M.O
M.CO2 <- M.C + 2*M.O
M.H2O <- M.H + 2*M.O
M.OH <- M.H + M.O
M.SO4 <- M.S + 4*M.O
M.PO4 <- M.P + 4*M.O
M.NO3 <- M.N + 3*M.O
M.NO2 <- M.N + 2*M.O
M.NH4 <- M.N + 4*M.H
M.CO3 <- M.C + 3*M.O

# Valenties
z.H <- 1
z.OH <- -1
z.Na <- 1
z.K <- 1
z.NH4 <- 1
z.Ca <- 2
z.Mg <- 2
z.Fe <- 2
z.Mn <- 2
z.Al <- 3
z.Si <- 4
## ANIONEN, enkel bij STIFF dient dit negatief te zijn, dit wordt in het algortime van stiff aangepast.
z.Cl <- 1
z.HCO3 <- 1
z.SO4 <- 2
z.PO4 <- 3
z.NO3 <- 1
z.NO2 <- 1
z.CO3 <- 1




# Data LIMS 2014-2022 ------------------------------------------------------

## Lees de data in en standardiseer dit naar een unifom formaat.
lims_connect <- lims_connect()
lims_AHR <- read_lims_data(connection = lims_connect, project = c("I-14025-01","I-14025-02",
                                                                  "I-15027-01","I-15027-02",
                                                                  "I-16023-03", "I-16023-04",
                                                                  "I-17W007-01", "I-17W007-02",
                                                                  "I-18W007-01", "I-18W007-02",
                                                                  "I-19W007-01", 
                                                                  "I-20W007-01",
                                                                  "I-21W007-01",
                                                                  "I-22W007-01",
                                                                  "I-23W007-01"
                                                                  ),
                           sql_template = "default",
                           show_query = F) %>% 
  mutate(Haven = "AHR")

lims_WAH <- read_lims_data(connection = lims_connect, 
                           project = c("I-14026-01","I-14026-02", "I-14026-04", "I-14026-05",
                                       "I-15020-01","I-15020-02", "I-15020-03",
                                       "I-16023-01", "I-16023-02",
                                       "I-17W006-01",
                                       "I-18W006-01",
                                       "I-19W006-01",
                                       "I-20W006-01",
                                       "I-21W006-01",
                                       "I-22W006-01",
                                       "I-23W006-01",
                                       "I-24W006-01"),
                           sql_template = "default", 
                           show_query = F) %>% 
  mutate(Haven = "WAH")



lims_havens_ruw <- bind_rows(lims_AHR, lims_WAH)


StandardizeDataLabo <- function(data_labo, data_lims = lims_havens_ruw) {
  # Ruim de dataframe correct op voor de data van 2014 - ...
  
  #
  data_lims_datums <- data_lims %>% 
    mutate(Monsternamedatum = as.Date(Monsternamedatum)) %>% 
    dplyr::select(Project, LaboCode, OrigineelStaal, 
                  "Monsternamedatum_Lims" =  Monsternamedatum, 
                  "Monsternemer_Lims" = Monsternemer) %>% 
    distinct()

  data_stddz <- data_labo %>% 
    tibble() %>% 
    rename("Project" = "LaboProject") %>% 
    mutate(Datum =  as_date(Monsternamedatum)) %>% 
    left_join(., data_lims_datums, by = c("Project", "LaboCode", "StaalNummer" = "OrigineelStaal")) %>% 
    mutate(
      Monsternamedatum = if_else(is.na(Monsternamedatum), Monsternamedatum_Lims, Monsternamedatum),
      Monsternemer = if_else(is.na(Monsternemer), Monsternemer_Lims, Monsternemer )
    ) %>% 
    # Fout in Project I-20W006-01 Update Jaar
    mutate(Monsternamedatum = case_when(
      Project == "I-20W006-01" & !is.na(Monsternamedatum) ~ Monsternamedatum %>% 
        update(year = 2020),
      TRUE ~ Monsternamedatum
    )) %>% 
    mutate(Jaar = year(Monsternamedatum),
           Datum = as.Date(Monsternamedatum),
           # BenedenLOQ
           BenedenDetectielimiet = case_when(
             # Pre 2020
             str_detect(WaardeGeformatteerd, "<|>") ~ TRUE,
             # Post 2020
             str_detect(LimietSymbool, "<|>") ~ TRUE,
             TRUE ~ FALSE)
           ) %>% 
    dplyr::select(-c(Monsternamedatum_Lims, Monsternemer_Lims)) %>% 
    mutate(
      WaardeRuw_old = WaardeRuw, 
      WaardeRuw = WaardeRuw %>% 
        str_replace_all(",", ".") %>%   # Convert decimal comma to dot
        as.double(.)) %>%
    mutate(WaardeNumeriek = NumeriekeWaarde) %>% 
    # ANALYSENAAM
    mutate(AnalyseNaam = case_when(
      Component %in% c("NH4") ~ "ICAS/CFA",
      AnalyseNaam %in% c("METALEN_TOT", "MINERALEN_TOT", "ICCS") ~ "MINMET_TOT",  
      # voor jaren waarin zowel ICCS als MINMET_TOT Geen overlap van elementen. Wel  bij I-19W007-01 
      AnalyseNaam %in% c("CFA", "ICAS", "TOT_P") ~ "ICAS/CFA",
      TRUE ~ AnalyseNaam
    )) %>% 
    # Verwijder de observaties van MINMET met NA als WaardeNumeriek
    dplyr::filter((AnalyseNaam == "MINMET_TOT" & !is.na(WaardeNumeriek)) | AnalyseNaam != "MINMET_TOT") %>% 
    # DUPLICATEN
    group_by(Project, ExternSampleID, OrigineelStaal, AnalyseNaam, Component) %>% 
    slice_min(WaardeRuw, with_ties = FALSE) %>% 
    ungroup() %>% 
    # # CL en CL_CFA dubbel maar nummerieke waarde verschil in 3de cijfer na de komma. CL_CFA is meest nauwkeurig
    # dplyr::filter((Project %in% c("I-17W007-02", "I-18W006-01", "I-19W006-01") & Component != "CL") | 
    #                 !Project %in% c("I-17W007-02", "I-18W006-01", "I-19W006-01")) %>% 
    # COMPONENT
    mutate(Component = case_when(
      # QC
      Component == "Totaal Kationen" ~ "T.KATION",
      Component == "Totaal Anionen" ~ "T.ANION",
      Component == "Verschil Procentueel" ~ "VERSCHIL.IC.PROC",
      Component == "EC gemeten" ~ "EC.MEAS",
      Component == "EC berekend" ~ "EC.CALC",
      Component == "Verschil %" ~ "VERSCHIL.EC.PROC",
      Component == "Verschil Absoluut" ~ "VERSCHIL.IC.ABS",
      Component == "Verschil_ABS" ~ "VERSCHIL.EC.ABS",
      Component == "Zwev. stof 105°C" ~ "ZS.150",
      str_detect(Component, "pH") & !str_detect(Component, regex("Veld", ignore_case = TRUE)) ~ "pH",
      # Krijg elementen van MINERALEN en METALEN met Eerste Characters als hoofletter en de daaropvolgende als kleine letetrs.
      AnalyseNaam == "MINMET_TOT" & str_length(Component) > 1 ~  str_c(str_sub(Component, 1, 1) %>% str_to_upper(), str_sub(Component, -1) %>% str_to_lower()),
      # Maak alle componenten van Geleidbaarheid EC
      AnalyseNaam %in% c("GELEIDBAARHEID", "EC") ~ "EC25",
      AnalyseNaam == "SPECTROFOTOMETRIE" & Component == "CHL.A" ~ "Chlorofyl a",
      Component == "CHL.A" ~ "Chlorofyl a",
      AnalyseNaam == "ICAS/CFA" & str_detect(Component, "CL") ~ "Cl",  # DUBBELE METINGEN verschil is er maar is kleiner als 0.1
      str_detect(Component, "KJEL") ~ "Kjel.N",
      AnalyseNaam == "VELDMETING" & Component == "EC_veld" | Component == "EC_veld" ~ "EC25.VELD",
      
      AnalyseNaam == "VELDMETING" & Component == "pH_veld" | Component == "pH_veld" ~ "pH.VELD",
      AnalyseNaam == "ICAS/CFA" & str_detect(Component, "P_TOT") ~ "P.TOT",
      Component %in% c("T.P", "P_TOT") ~ "P.TOT",
      Component %in% c("T.S") ~ "S.TOT",
      Component %in% c("T.N") ~ "N.TOT",
      # IONEN,
      Component %in% c("CA", "NA", "MG", "FE", "MN", "AL", "CL", "SI") ~ str_to_title(Component),
      TRUE ~ Component
    )) %>% 
    mutate(Eenheid = case_when(
      Component %in% c("N.TOT", "Kjel.N") ~ "mg N/l",
      Component == "P.TOT" ~ "mg P/l",
      Component == "S.TOT" ~ "mg S/l",
      Component %in% c("EC.CALC", "EC.MEAS", "VERSCHIL.EC.ABS", "EC25", "EC25.VELD") ~ "µS/cm",
      AnalyseNaam %in% c("MINMET_TOT") ~  "mg/l",
      AnalyseNaam %in% c("ICAS/CFA") & !Component %in% c("KJEL.N", "P.TOT", "N.TOT") ~ "mg/l",
      AnalyseNaam == "ZUURTEGRAAD" & str_detect(Component, "pH") ~ "pH",
      AnalyseNaam == "VELDMETING" & Component == "EC25.VELD" ~ "µS/cm",
      Component %in% c("pH.VELD", "pH") ~ "pH", 
      AnalyseNaam %in% c("BOD", "COD") ~ "mg O2/l",
      AnalyseNaam %in% c("Chlorofyl a", "Faeofytine") ~ "µg/l",
      Component == "HCO3" ~ "mg/l",
      str_detect(Eenheid, "mg/L") ~ "mg/l",
      TRUE ~ Eenheid
    ))

  return(data_stddz)
}



StandardizeData <- function(data) {
  # Ruim de dataframe correct op voor de data van 2014 - ...
  
  #

  data_XD <- data %>% 
    # filter duplicaten weg
    dplyr::filter(!str_starts(string = LaboCode, "D")) %>% 
    # Indien meerdere resultaatreplicaten: resultaatreplicaat 2 is de correcte.
    group_by(Project, ExternSampleID, OrigineelStaal, AnalyseNaam, Component) %>%
    dplyr::filter(ResultaatReplicaat == max(ResultaatReplicaat %>% as.integer())) %>%
    ungroup() %>%
    # # Gemiddelde datum
    # group_by(Project) %>% 
    # mutate(Datum =  mean(as_date(Monsternamedatum), na.rm = TRUE)) %>% 
    # ungroup() %>% 
    mutate(Datum =  as_date(Monsternamedatum), na.rm = TRUE) %>% 
    # Add jaar
    mutate(Jaar = year(Datum)) %>% 
    mutate(Jaar = case_when(
      is.na(Jaar) ~ str_c(20, str_sub(ContractID, 1, 2)) %>% 
        as.double(),
      TRUE ~ Jaar),
      Datum = case_when(
        is.na(Datum) & Project == "I-20W007-01" & !is.na(ExternSampleID) ~ as.Date("01-05-2020", format = "%d-%m-%Y", tz = ""),
        TRUE ~ Datum
      )) %>% 
    mutate("Detectielimiet" = BenedenLOQ) %>% 
    tibble()
  
  data_aanpassingen <- data_XD %>% 
    mutate(WaardeRuw = as.double(WaardeRuw)) %>% 
    # ANALYSENAAM
    mutate(AnalyseNaam = case_when(
      Component %in% c("NH4") ~ "ICAS/CFA",
      AnalyseNaam %in% c("METALEN_TOT", "MINERALEN_TOT", "ICCS") ~ "MINMET_TOT",  
      # voor jaren waarin zowel ICCS als MINMET_TOT Geen overlap van elementen. Wel  bij I-19W007-01 
      AnalyseNaam %in% c("CFA", "ICAS", "TOT_P") ~ "ICAS/CFA",
      TRUE ~ AnalyseNaam
    )) %>% 
    # Verwijder de observaties van MINMET met NA als WaardeNumeriek
    dplyr::filter((AnalyseNaam == "MINMET_TOT" & !is.na(WaardeNumeriek)) | AnalyseNaam != "MINMET_TOT") %>% 
    # CL en CL_CFA dubbel maar nummerieke waarde verschil in 3de cijfer na de komma. CL_CFA is meest nauwkeurig
    dplyr::filter((Project %in% c("I-17W007-02", "I-18W006-01", "I-19W006-01") & Component != "CL") | 
             !Project %in% c("I-17W007-02", "I-18W006-01", "I-19W006-01")) %>% 
    # COMPONENT
    mutate(Component = case_when(
      # QC
      Component == "Totaal Kationen" ~ "T.KATION",
      Component == "Totaal Anionen" ~ "T.ANION",
      Component == "Verschil Procentueel" ~ "VERSCHIL.IC.PROC",
      Component == "EC gemeten" ~ "EC.MEAS",
      Component == "EC berekend" ~ "EC.CALC",
      Component == "Verschil %" ~ "VERSCHIL.EC.PROC",
      Component == "Verschil Absoluut" ~ "VERSCHIL.IC.ABS",
      Component == "Verschil_ABS" ~ "VERSCHIL.EC.ABS",
      Component == "Zwev. stof 105°C" ~ "ZS.150",
      str_detect(Component, "pH") & !str_detect(Component, regex("Veld", ignore_case = TRUE)) ~ "pH",
      # Krijg elementen van MINERALEN en METALEN met Eerste Characters als hoofletter en de daaropvolgende als kleine letetrs.
      AnalyseNaam == "MINMET_TOT" & str_length(Component) > 1 ~  str_c(str_sub(Component, 1, 1) %>% str_to_upper(), str_sub(Component, -1) %>% str_to_lower()),
      # Maak alle componenten van Geleidbaarheid EC
      AnalyseNaam == "GELEIDBAARHEID" ~ "EC25",
      AnalyseNaam == "SPECTROFOTOMETRIE" & Component == "CHL.A" ~ "Chlorofyl a",
      AnalyseNaam == "ICAS/CFA" & str_detect(Component, "CL") ~ "Cl",  # DUBBELE METINGEN verschil is er maar is kleiner als 0.1
      AnalyseNaam == "ICAS/CFA" & str_detect(Component, "KJEL") ~ "Kjel.N",
      AnalyseNaam == "VELDMETING" & Component == "EC_veld" ~ "EC25.VELD",
      AnalyseNaam == "VELDMETING" & Component == "pH_veld" ~ "pH.VELD",
      AnalyseNaam == "ICAS/CFA" & str_detect(Component, "P_TOT") ~ "P.TOT",
      Component %in% c("T.P") ~ "P.TOT",
      Component %in% c("T.S") ~ "S.TOT",
      Component %in% c("T.N") ~ "N.TOT",
      TRUE ~ Component
    )) %>% 
    mutate(Eenheid = case_when(
      Component %in% c("N.Totaal", "KJEL.N") ~ "mg N/l",
      Component == "P.TOT" ~ "mg P/l",
      Component == "S.TOT" ~ "mg S/l",
      Component %in% c("EC.CALC", "EC.MEAS", "VERSCHIL.EC.ABS") ~ "µS/cm",
      AnalyseNaam %in% c("MINMET_TOT") ~  "mg/l",
      AnalyseNaam %in% c("ICAS/CFA") & !Component %in% c("KJEL.N", "P.TOT", "N.TOT") ~ "mg/l",
      AnalyseNaam == "ZUURTEGRAAD" & str_detect(Component, "pH") ~ "pH",
      AnalyseNaam == "VELDMETING" & Component == "EC25.VELD" ~ "µS/cm",
      AnalyseNaam == "VELDMETING" & Component == "pH.VELD" ~ "pH", 
      AnalyseNaam %in% c("BOD", "COD") ~ "mg O2/l",
      AnalyseNaam %in% c("Chlorofyl a", "Faeofytine") ~ "µg/l",
      Component == "HCO3" ~ "mg/l",
      TRUE ~ Eenheid
    ))
  
  
  return(data_aanpassingen)
}

# Standardizeer, enkel geldig voor 2014-2022.
lims_havens_data <- lims_havens_ruw %>% 
  StandardizeData() %>% 
  mutate(ExternSampleID = str_to_upper(ExternSampleID),
         ExternSampleID = str_replace(ExternSampleID, "[X]$", ""),
         ExternSampleID = str_replace(ExternSampleID, " ", ""),
         ExternSampleID = str_replace(ExternSampleID, "M", "H")) 

# staal <- lims_havens_data %>%
#   filter(LaboCode ==  "15-002443" & ExternSampleID  == "AHRS006")

# # Dit zou in principe leeg moeten zijn bij correcte aanpassing
# lims_havens_data %>%
#   arrange(Project, AnalyseNaam) %>%
#   select(- c("LimsAnalyseNaam", "Instrument", "Batch")) %>%
#   distinct() %>%
#   group_by(., Project, ExternSampleID, LaboCode, Monsternamedatum,  AnalyseNaam, Component, Eenheid) %>%
#   summarise(Aantal = n(), .groups = "drop") %>%
#   filter(Aantal != 1) %>%
#   print(n = 210)


# Controle------------------------------------------------
# Controle------------------------------------------------
# Stalen worden op 2 manieren gecontroleerd of ze bruikbaar zijn:
## 1. Veld & labo pH & EC != X% verschillen.
## 2. Elektronen-neutraliteit lager als X%

ExpressieControle <- function(x, y, procent) {
  # Controles of the measurement dont differs more than X procent between x & y.
  # Meer gedetailleerd:
  # Het absolute verschil moet KLEINER zijn dan 10% van het gemiddelde van de veld-meting en labo-meting.
  
  verschil <-  abs(x - y)
  gem <- (x + y) / 2
  gem_procent <- (gem/100) * procent
  
  if (verschil <= gem_procent) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}



controle_waarde_veldlab <- function(staal, component, procent, NoValuePass = FALSE) {

  veld <- staal %>%
    filter(str_detect(.data$Component, component)) %>%
    filter(str_detect(.data$Component, regex("veld", ignore_case = TRUE))) %>% 
    filter(!is.na(WaardeRuw))

  veld %>% 
    distinct(Component, WaardeRuw)
  
  labo <- staal %>%
    filter(.data$Component == component) %>% 
    filter(!is.na(WaardeRuw))
  
  if (nrow(labo) == 0 & component == "EC25") {
    labo <- staal %>% 
      filter(str_detect(.data$Component, "EC.MEAS"))
  }
  
  if (nrow(labo) == 0) {
    print(str_c("Geen waarde voor component ", component, " voor OrigineelStaalnummer ", 
                staal %>% 
                  distinct(OrigineelStaal) %>% 
                  pull(OrigineelStaal),
                " met SampleID ",
                staal %>% 
                  distinct(ExternSampleID ) %>% 
                  pull(ExternSampleID )
                ,
                " van project " , 
                staal %>% 
                  distinct(Project) %>% 
                  pull(Project)
                ))
  }
  
  # If value is missing, PASS this test
  if ((nrow(labo) == 0 | nrow(veld) == 0) & NoValuePass) {
    return(TRUE)
  }
  
  
  
  # Controleren of veldwaarde beschikbaar is
  if (nrow(veld) != 0 && !is.na(veld %>% pull(WaardeRuw))) {
    # Controleren of labowaarde beschikbaar is
    if (nrow(labo) == 0) {
      return(FALSE)
    } else {
      # Waarden ophalen
      labo_waarde <- labo %>%
        mutate(WaardeRuw = as.double(WaardeRuw)) %>%
        pull(WaardeRuw)

      veld_waarde <- veld %>%
        mutate(WaardeRuw = as.double(WaardeRuw)) %>%
        pull(WaardeRuw)

      # ExpressieControle toepassen
      return(ExpressieControle(labo_waarde, veld_waarde, procent = procent))
    }
  }
  return(TRUE)
}


ControleParametersVeldLab <- function(staal, procent, PassNoValue =  FALSE) {
  # Deze functie controleerd of de staalname meegenomen mag worden in de tijdlijn.
  # Namelijk, Zowel de veldmeting van EC en pH mogen niet meer als 10 % verschillen in grootte van de labo-meting.
  
  # Returns boolean expression
  # EC controle
  boolean_EC <- controle_waarde_veldlab(staal, "EC25", procent, NoValuePass = PassNoValue)
  
  # pH controle
  boolean_pH <- controle_waarde_veldlab(staal, "pH", procent, NoValuePass = PassNoValue)
  
  return(boolean_EC & boolean_pH)
}


ConcentratieIon <- function(staal_ion, HelftDetLim = FALSE, MissingNA = FALSE) {
  # Geeft de concentratie van een ion weer, rekening houdend met de detectielimiet.
  # Indien ion niet aanwezig is, zal dit waarde 0 krijgen.
  # Indien HelftDetLim == FALSE (default), dan zal de waarde bij LOQ gelijk gesteld worden aan 0.
  # Indien HelftDetLim == TRUE, dan zal de waarde bij LOQ gedeeld worden door 2.
  # MissingNA, return NA or 0 when missing. FALSE 0, TRUE NA
  
  # Ion niet aanwezig dus waarde dient 0 te worden.
  if (nrow(staal_ion) == 0) {
    if (MissingNA) {
      return(NA_real_)
    } else {
      return(0)
    }
  }
  
  if (is.na(staal_ion$WaardeRuw)) {
    if (MissingNA) {
      return(NA_real_)
    } else {
      return(0)
    }
  }
  
  # Is er een detectielimiet?
  if(is.na(staal_ion$BenedenDetectielimiet)) {
    return(staal_ion$WaardeRuw)
  }  
  
  if (!staal_ion$BenedenDetectielimiet) {
    return(staal_ion$WaardeRuw)
    # Onder detectielimiet
    # Indien onder, waarde 0.
  } else if (!HelftDetLim) {
    return(0)
    # Indien onder waarde/2
  } else {
    return(staal_ion$WaardeRuw/2)
  }
}

## Deze wordt niet gebruikt. Het lab gebruikt niet hetzelfde aantal anionen en kationen.
# EN_procent <- function(Staal) {
#   # DEZE NIET gebruiken
#   # Kationen gebruikt  Meq.Ca + Meq.K + Meq.Na + Meq.Mg
#   # ANIONEN Meq.Cl + Meq.HCO3 + Meq.SO4
#   Kation <- Staal %>%  dplyr::filter(.data$Component == "T.KATION") %>%  pull(WaardeNumeriek)
#   Anion <- Staal %>%  dplyr::filter(.data$Component == "T.ANION") %>%  pull(WaardeNumeriek)
#   
#   EN <- ((Kation - Anion) / (Kation + Anion)) * 100
#   return(EN)
# }

# T kationen -< 2.3434       Meq.Ca + Meq.K + Meq.Na + Meq.Mg
# T.ANIONEN -> 2.1657       Meq.Cl + Meq.HCO3 + Meq.SO4


Ionen <- c("Ca", "Na", "K", "Mg", "Fe", "NH4", "Mn", "Al", "SO4", "HCO3", "Cl", "NO2", "NO3", "PO4")

BerekenEN <- function(staal, HelftDetLim = FALSE, Fe = FALSE) {
  # Elektroneneutraliteitspercentage
  # De concentraties worden omgerekend naar mili-equivalent per liter.
  # Onder kationen worden volgende zaken meegenomen, Si wordt standaard door het labo op 0 gezet dus niet mee te pakken;
  ## Ca, Na, K, Mg, NH4, Fe, Mn, Al
  # Onder anionen worden volgende zaken meegenomen:
  ## SO4, HCO3, Cl, NO2, NO3, PO4
  # Als er veel IJzer (Fe) in het water zit, dan wordt dit niet steeds mee opgenomen in de analyse.
  ## Boolean ingebouwd waarbij Fe 0 kan worden.
  C.Ca <- subset(staal, Component == "Ca") %>% 
    ConcentratieIon(., HelftDetLim = HelftDetLim)
  C.Na <- subset(staal, Component == "Na") %>% 
    ConcentratieIon(., HelftDetLim = HelftDetLim)
  C.K <- subset(staal, Component == "K") %>% 
    ConcentratieIon(.,  HelftDetLim = HelftDetLim)
  C.Mg <- subset(staal,Component == "Mg") %>% 
    ConcentratieIon(.,  HelftDetLim = HelftDetLim)
  C.Fe <- subset(staal, Component == "Fe") %>% 
    ConcentratieIon(.,  HelftDetLim = HelftDetLim)
  if (!Fe) {
    C.Fe <- 0
  }
  
  C.NH4 <- subset(staal, Component == "NH4") %>% 
    ConcentratieIon(.,  HelftDetLim = HelftDetLim)
  # Toegevoegd
  C.Mn <- subset(staal, Component == "Mn") %>%
    ConcentratieIon(.,  HelftDetLim = HelftDetLim)
  C.Al <-  subset(staal, Component == "Al") %>%
    ConcentratieIon(.,  HelftDetLim = HelftDetLim)
  
  C.SO4 <- subset(staal, Component == "SO4") %>% 
    ConcentratieIon(.,  HelftDetLim = HelftDetLim)
  C.HCO3 <- subset(staal, Component == "HCO3") %>% 
    ConcentratieIon(.,  HelftDetLim = HelftDetLim)
  C.Cl <- subset(staal, Component == "Cl") %>% 
    ConcentratieIon(.,  HelftDetLim = HelftDetLim)
  C.NO2 <- subset(staal, Component == "NO2") %>% 
    slice_head(n = 1) %>%   # Slice_head want er zitten blijkbaar bij NO2 2 verschillende detectielimiete
    
    ConcentratieIon(.,  HelftDetLim = HelftDetLim)
  C.NO3 <- subset(staal, Component == "NO3") %>% 
    ConcentratieIon(.,  HelftDetLim = HelftDetLim)
  C.PO4 <- subset(staal, Component == "PO4") %>% 
    ConcentratieIon(.,  HelftDetLim = HelftDetLim)
  
  Meq.Ca <- (C.Ca / M.Ca) * z.Ca
  Meq.Na <- (C.Na / M.Na) * z.Na
  Meq.K <- (C.K / M.K) * z.K
  Meq.Mg <- (C.Mg / M.Mg) * z.Mg
  Meq.NH4 <- (C.NH4 / M.NH4) * z.NH4
  Meq.Fe <- (C.Fe / M.Fe) * z.Fe
  # # # Toegevoegd
  Meq.Mn <- (C.Mn / M.Mn) * z.Mn
  Meq.Al <- (C.Al / M.Al) * z.Al
  
  Meq.SO4 <- (C.SO4 / M.SO4) * z.SO4
  Meq.HCO3 <- (C.HCO3 / M.HCO3) * z.HCO3
  Meq.Cl <- (C.Cl / M.Cl) * z.Cl
  Meq.NO2 <- (C.NO2 / M.NO2) * z.NO2
  Meq.NO3 <- (C.NO3 / M.NO3) * z.NO3
  Meq.PO4 <- (C.PO4 / M.PO4) * z.PO4
  
  Meq.Anion <- Meq.SO4 + Meq.HCO3 + Meq.Cl + Meq.PO4 + Meq.NO2 + Meq.NO3
  Meq.Kation <- Meq.Ca + Meq.Na + Meq.K + Meq.Mg + Meq.NH4 + Meq.Fe + Meq.Mn + Meq.Al
  
  EN <- ((Meq.Kation - abs(Meq.Anion)) / (Meq.Kation + abs(Meq.Anion))) * 100
  return(EN)
}


ControleEN <- function(staal, procent = 10, HelftDetLim = FALSE, Fe = FALSE) {
  
  Controle <- procent > abs(BerekenEN(staal, HelftDetLim = HelftDetLim, Fe = Fe))
  
  # if (!Controle) {
  #   Controle <- procent > abs(BerekenEN(staal, HelftDetLim = HelftDetLim, Fe = FALSE))
  # }
  return(Controle)
}

ControleStaal <- function(staal, procent_EN = 10, procent_laboveld = 10, HelftDetLim = FALSE, 
                          NoValuePassVeldLabo = FALSE, 
                          Fe = FALSE) {
  # Combines the control for each sample.
  # If a percent is NULL, this control will be skipped.
  
  if (!is.null(procent_EN)) {
    EN <- ControleEN(staal, procent = procent_EN, HelftDetLim = HelftDetLim, Fe = Fe)
    label_EN <- ""
  } else {
    EN <- TRUE
    label_EN <- 'EN niet gecontroleerd'
  }
  
  if (!is.null(procent_laboveld)) {
    Parameters_veldlabo <- ControleParametersVeldLab(staal, procent = procent_laboveld, PassNoValue = NoValuePassVeldLabo)
    label_veldlabo <- ""
  } else {
    Parameters_veldlabo <- TRUE
    label_veldlabo <- "Veldlabo niet gecontroleerd"
  }
  
  if (!EN) {
    label_EN <- "EN"
  }
  if (!Parameters_veldlabo) {
    label_veldlabo <- "veldlabo"
  }
  
  return(list(boolean = EN & Parameters_veldlabo, label_EN = label_EN, label_veldlabo = label_veldlabo))
}


ControleerStalen <- function(data_stalen, procent_EN = 10, procent_laboveld = 10, HelftDetLim = FALSE, 
                             IncludeEmptyVeldLabo = FALSE, Fe = FALSE) {
  
  # Controleerd meerdere stalen & behoudt enkel degene waarvan de voorwaarde voldoet. 
  stalen_gecontroleerd <- data.frame()
  stalen_NOK <- data.frame()
  
  for (code in unique(data_stalen$OrigineelStaal)) {
    
    staal <- subset(data_stalen, OrigineelStaal == code)
    
    controle_staal <- ControleStaal(staal, 
                                    procent_EN = procent_EN, procent_laboveld = procent_laboveld,
                                    HelftDetLim = HelftDetLim,
                                    Fe = Fe, NoValuePassVeldLabo = IncludeEmptyVeldLabo)
    
    if (controle_staal$boolean) {
      stalen_gecontroleerd <- bind_rows(stalen_gecontroleerd, staal %>% 
                                          mutate(Label = str_c(controle_staal$label_EN, 
                                                               controle_staal$label_veldlabo, sep = "|")))
    } else {
      stalen_NOK <- bind_rows(stalen_NOK, 
                              staal %>% 
                                mutate(Label = str_c(controle_staal$label_EN, 
                                                     controle_staal$label_veldlabo, sep = "|")))
    }
  }
  return(list(ok = stalen_gecontroleerd,
              nok = stalen_NOK))
}


# Bereken N en P ------------------------------------------------------------------

CombineerNOx <- function(Staal, HelftDetLim = FALSE) {
  # Maak een component NO3+NO2 aan.
  # Return de staal terug waarbij dit component is toegevoegd.
  
  NOx <- subset(Staal, Component == "NO3+NO2") %>% 
    ConcentratieIon(., HelftDetLim = HelftDetLim, MissingNA = TRUE)
  
  NO3 <- subset(Staal, Component == "NO3") %>% 
    ConcentratieIon(., HelftDetLim = HelftDetLim, MissingNA = TRUE)
  
  NO2 <- subset(Staal, Component == "NO2") %>% 
    ConcentratieIon(., HelftDetLim = HelftDetLim, MissingNA = TRUE)
  
  if (!is.na(NOx)) {
    return(Staal)
  } else {
    NOx <- sum(NO3, NO2)
    Staal_N <- Staal %>% 
      bind_rows(., Staal %>% 
                  slice_head(n = 1) %>% 
                  mutate(LimsAnalyseNaam= "Berekend",
                         Techniek = "Berekend",
                         Opmerkingen = "Info",
                         Component = "NO3+NO2",
                         Eenheid = "mg/L",
                         WaardeRuw = NOx,
                         Detectielimiet = NA,
                         BenedenLOQ = NA,
                         LimietSymbool = NA)) %>% 
      arrange(Component)
    
    return(Staal_N)
    
  }
}


BerekenOrganischN <- function(Staal, HelftDetLim = FALSE)  {
  
  # Eerst omzetten naar mg N/L
  # Norganisch stikstof kan op verschillende manieren berekend worden:
  ## 1. Organisch = N.kjel - N.NH4.
  ## 2. Organisch = Totaal - NH4 - NO2 - NO3.
  ## 3. Eerst NH4 berekenen door; NH4 = Totaal - Kjel
  ##   Organisch <- kjel - nh4.
  
  C.NO2 <- subset(Staal, Component == "NO2") %>% 
    ConcentratieIon(., HelftDetLim = HelftDetLim, MissingNA = TRUE)
  C.NO3 <- subset(Staal, Component == "NO3") %>% 
    ConcentratieIon(., HelftDetLim = HelftDetLim, MissingNA = TRUE)
  C.NH4 <- subset(Staal,Component == "NH4") %>% 
    ConcentratieIon(., HelftDetLim = HelftDetLim, MissingNA = TRUE)
  
  # mgN/L
  N.NO2 <- (C.NO2 * M.N) / M.NO2
  N.NO3 <- (C.NO3 * M.N) / M.NO3
  N.NH4 <- (C.NH4 * M.N) / M.NH4
  
  KJEL.N <- subset(Staal, Component == "Kjel.N") %>% 
    ConcentratieIon(., HelftDetLim = HelftDetLim, MissingNA = TRUE)
  T.N <- subset(Staal, Component == "N.TOT") %>% 
    ConcentratieIon(., HelftDetLim = HelftDetLim, MissingNA = TRUE)
  
  # Methode 1
  N.ORG <- KJEL.N - N.NH4
  
  # Methode 2
  if (is.na(N.ORG)) {
    N.ORG <- T.N - N.NH4 - N.NO2 - N.NO3
  } 
  
  # Methode 3
  if (is.na(N.ORG)) {
    N.NH4 <- T.N - KJEL.N
    N.ORG <- KJEL.N - N.NH4 
  }
  
  
  Staal.N.org <- Staal %>% 
    bind_rows(., Staal %>% 
                slice_head(n = 1) %>% 
                mutate(LimsAnalyseNaam= "Berekend",
                       Techniek = "Berekend",
                       Opmerkingen = "Info",
                       Component = "N.ORG",
                       Eenheid = "mg/L",
                       WaardeRuw = N.ORG,
                       Detectielimiet = NA,
                       BenedenLOQ = NA,
                       LimietSymbool = NA)) %>% 
    arrange(Component)

  
  return(Staal.N.org)
}

BerekenTotaalN <- function(Staal, HelftDetLim = FALSE) {
  #in mgN/L
  
  # N.TOT bestaat al
  if ("N.TOT" %in% Staal$Component) {
    return(Staal)
    
  # N.TOT te berekenen
  } else {
    # Als een component ontbreekt zal dit NA worden.
    
    C.NO2 <- subset(Staal, Component == "NO2") %>% 
      ConcentratieIon(., HelftDetLim = HelftDetLim, MissingNA = TRUE)
    C.NO3 <- subset(Staal, Component == "NO3") %>% 
      ConcentratieIon(., HelftDetLim = HelftDetLim, MissingNA = TRUE)
    # mgN/L
    N.NO2 <- (C.NO2 * M.N) / M.NO2
    N.NO3 <- (C.NO3 * M.N) / M.NO3
    KJEL.N <- subset(Staal, Component == "Kjel.N") %>% 
      ConcentratieIon(., HelftDetLim = HelftDetLim, MissingNA = TRUE)
    # T.N
    T.N <- KJEL.N + N.NO2 + N.NO3
  }
  
  Staal.N.Tot <- Staal %>% 
    bind_rows(., Staal %>% 
                slice_head(n = 1) %>% 
                mutate(LimsAnalyseNaam= "Berekend",
                       Techniek = "Berekend",
                       Opmerkingen = "Info",
                       Component = "N.TOT",
                       Eenheid = "mg/L",
                       WaardeRuw = T.N,
                       Detectielimiet = NA,
                       BenedenLOQ = NA,
                       LimietSymbool = NA)) %>% 
    arrange(Component)
  
  return(Staal.N.Tot)
}



BerekenOrganischP <- function(Staal, HelftDetLim = FALSE) {
  # Organische P is simpelweg Totale P - Fosfaat (PO4)
  
  C.PO4 <- subset(Staal, Component == "PO4") %>% 
    ConcentratieIon(., HelftDetLim = HelftDetLim, MissingNA = TRUE)
  
  P.PO4 <- (C.PO4 * M.P) / M.PO4
  
  T.P <- subset(Staal, Component == "P.TOT") %>% 
    ConcentratieIon(., HelftDetLim = HelftDetLim, MissingNA = TRUE)
  
  P.ORG <- T.P - P.PO4
  
  # Add to data.
  Staal.P.Tot <- Staal %>% 
    bind_rows(., Staal %>% 
                slice_head(n = 1) %>% 
                mutate(LimsAnalyseNaam= "Berekend",
                       Techniek = "Berekend",
                       Opmerkingen = "Info",
                       Component = "P.ORG",
                       Eenheid = "mg/L",
                       WaardeRuw = P.ORG,
                       Detectielimiet = NA,
                       BenedenLOQ = NA,
                       LimietSymbool = NA)) %>% 
    arrange(Component)
  
  return(Staal.P.Tot)
}


AddRow <- function(Staal, element, waarde) {
  # Only add it when needed.
  
  element_adjust <- if_else(str_detect(element, "N"), str_c(element, ".N"), str_c(element, ".P"))
  
  eenheid <- if_else(str_detect(element, "N"), "mg N/L", "mg P/L")
  
  subset_staal <- subset(Staal, Component == element_adjust)
  
  if (element_adjust %in% Staal$Component) {
    row <- data.frame()
  } else {
    row <- Staal %>% 
      slice_head(n = 1) %>% 
      mutate(LimsAnalyseNaam= "Berekend",
             Techniek = "Berekend",
             Opmerkingen = "Info",
             Component = element_adjust,
             Eenheid = eenheid,
             WaardeRuw = waarde,
             WaardeGeformatteerd = as.character(waarde),
             Detectielimiet = NA,
             BenedenLOQ = NA, 
             LimietSymbool = NA) 
  }
  return(row)
}


ConvertEquivalentNP <- function(Staal, HelftDetLim = FALSE) {
  # Zet Stikstof en Phosphor om in equivalent N of P. 
  
  Staal_controle <- Staal %>% 
    filter(!is.na(WaardeRuw))
  
  rows_bind <- data.frame()
  
  for (element in c("NH4", "NO2", "NO3", "PO4")) {
    
    C <- Staal %>% 
      subset(Component == element) %>% 
      ConcentratieIon(., HelftDetLim = HelftDetLim, MissingNA = TRUE)
    
    if (element == "NH4") {
      
        Waarde <- C * M.N / M.NH4
    }
    
    if (element == "NO3") { 

        Waarde <- C * M.N / M.NO3
    }
    
    if (element == "NO2") { 
        Waarde <- C * M.N / M.NO2
    }
    if (element == "PO4") {
      
        Waarde <- C * M.P / M.PO4
    }
    
    rows_bind <- bind_rows(rows_bind, 
                           AddRow(Staal, element, Waarde))

  }
  
  Staal.equivalent <- Staal %>% 
    bind_rows(., rows_bind)
  
  return(Staal.equivalent)
}



# AddTOrgNP <- function(Stalen, HelftDetLim = FALSE) {
#   
#   Stalen_NP <- Stalen %>% 
#     group_by(ContractID, Klant, Project, ExternSampleID, LaboCode, Datum) %>% 
#     do(
#       Org.N = Bereken_N_organisch(., HelftDetLim = HelftDetLim),
#       T.N = Bereken_N_totaal(.,  HelftDetLim = HelftDetLim),
#       Org.P = Bereken_P_organisch(.,  HelftDetLim = HelftDetLim)
#     ) %>% 
#     unnest(cols= c(Org.N, T.N, Org.P)) %>% 
#     group_by(ContractID, Klant, Project, ExternSampleID, LaboCode, Datum) %>% 
#     pivot_longer(., cols = c(Org.N:Org.P), names_to = "Component", values_to = "WaardeNumeriek") %>% 
#     mutate(Eenheid = case_when(
#       str_detect(Component, "N") ~ "mg N/l",
#       str_detect(Component, "P") ~ "mg P/l"
#     )) %>% 
#     mutate(Instrument = "Berekend",
#            BenedenLOQ = FALSE) %>%
#     # Verwijder de T.N indioen deze al bestaat. Klopt.
#     bind_rows(Stalen %>% 
#                 filter(Component != "T.N"), .) %>%
#     arrange(ExternSampleID, LaboCode, Datum)
#   
#   
#   return(Stalen_NP)
# }

# Ondersteunende functies grafieken --------------------

Bereken_IR <- function(Staal, HelftDetLim = FALSE) {
  C.Ca <- subset(Staal, Component == "Ca") %>% 
    Concentratie_ion(., HelftDetLim)
  C.Cl <- subset(Staal,Component == "Cl") %>% 
    Concentratie_ion(., HelftDetLim)
  Meq.Ca <- C.Ca / M.Ca * z.Ca
  Meq.Cl <- C.Cl / M.Cl * abs(z.Cl)
  
  IR <- (Meq.Ca / (Meq.Ca + Meq.Cl)) * 100
  return(IR)
}


DataTijdreeksComponent <- function(Stalen, MeetpuntCode, Component_, HelftDetLim = FALSE) {
  
  code_component <-  Stalen %>% 
    filter(.data$ExternSampleID == MeetpuntCode & .data$Component %in% Component_)
  
  if (nrow(code_component) != 0) {
    code_component <- code_component %>% 
      mutate(Waarde = case_when(
        HelftDetLim & BenedenLOQ == TRUE ~  as.double(WaardeNumeriek)/2,
        !HelftDetLim & BenedenLOQ == TRUE ~  0,
        TRUE ~ as.double(WaardeNumeriek)
      )) %>% 
      arrange(Datum)
    
  } else {
    code_component <- NULL
    
  }
  return(code_component)
}


# Bruikbare stalen-------------------------
# Standardizeer, enkel geldig voor 2014-2022.
# lims_havens_data_controle <- lims_havens_ruw %>% 
#   StandardizeData() %>% 
#   mutate(ExternSampleID = str_to_upper(ExternSampleID),
#          ExternSampleID = str_replace(ExternSampleID, "[X]$", ""),
#          ExternSampleID = str_replace(ExternSampleID, " ", ""),
#          ExternSampleID = str_replace(ExternSampleID, "M", "H")) %>% 
#   ControleerStalen(., HelftDetLim = HelftDetLim) %>% 
#   AddTOrgNP(., HelftDetLim)


TijdreeksComponenten <- function(Stalen, MeetpuntCode, Component_, HelftDetLim = FALSE) {
  
  code_component <- DataTijdreeksComponent(Stalen = Stalen, MeetpuntCode = MeetpuntCode, Component_ = Component_, HelftDetLim = HelftDetLim)
  
  
  # n <- code_component %>% 
  #   summarise(mean(Waarde, na.rm = TRUE)) %>% 
  #   pull() %>% 
  #   as.character() %>% 
  #   str_locate(., "[1-9]")
  # 
  # # nth digit that is not 0
  # n <- s
  # 

  G <- NULL
  
  if (!is.null(code_component)) {
    
    unit <- code_component %>% 
      distinct(Eenheid) %>% 
      pull()
    
    # if (str_detect(unit, "O2")) {
    #   
    #   unit <- expression("mg O"[2]*"/l") %>% 
    #     as.character()
    # }

    if (length(unit) != 1) {
      return("Componenten niet in dezelfde eenheid")
    }
    
    G <- ggplot(data = code_component, aes(x = Datum, y = Waarde, col = Component)) +
      geom_point() +
      geom_line()
    
    
    G <- G +
      labs(x = "", y = unit, parse = TRUE) +
      theme(
        axis.text = element_text(colour = inbo_steun_donkerroos), 
        legend.text = element_text(color = inbo_steun_donkerroos),
        axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.5),
        panel.background = element_rect(fill = NA,
                                        colour = NA,
                                        linewidth = 0.25, linetype = "solid"),
        panel.grid.major = element_line(linewidth  = 0.5, linetype = 'solid',
                                        colour = "gray80"),
        panel.grid.minor = element_line(linewidth  = 0.00001, linetype = 'solid',
                                        colour = "gray90")) +
      scale_x_date(date_labels = "%b %Y", date_breaks = "6 months") +
      scale_y_continuous(limits = c(RoundAny(min(code_component$Waarde, na.rm = TRUE), 1, f = floor), 
                                    RoundAny(max(code_component$Waarde, na.rm = TRUE), 1, f = ceiling)))
  }
  
  Onderschrift <- str_c("Tijdreeks van ",str_c(Component_,  collapse = " en "), " voor ", code, ".")
  
  return(list(Graf = G, Bijschrift = Onderschrift))
}









# Grafieken --------------------------





Kleur1 <- rgb(53,97,150,maxColorValue = 255)
Stiff <- function(Staal, HelftDetLim = FALSE, MetECpH = TRUE, MetDatum = FALSE, MetMeetpunt = FALSE,
                  SchaalXVast = FALSE, XBereik = 10, AsX = TRUE, AsY = FALSE, MidLijn = TRUE, DikteMidLijn = 0.5, 
                  KleurMidLijn = Kleur1, LijnKleur = Kleur1, LijnDikte = 1, LijnType = "solid", Vulling = 0.5, 
                  VulKleur = Kleur1, LabelPlaatsFactor=1.1, LabelPlaatsSchuif = 0, AsLabelGrootte = 11, AsLabelKleur = inbo_steun_donkerroos, 
                  AsTitelGrootte = 11, AsTitelKleur = "black", StiffLabelGrootte = 4, StiffLabelKleur = "black", 
                  StiffLabelFontface = "plain", Grid = FALSE, PanelKleur = "white", AsLijnKleur = "black", 
                  AsLijnDikte = 0.5, AsLijnType = "solid", LabelFactorECpH = 1.2, PijlHoogteFactorECpH = 1.2, 
                  PijlBreedteFactorECpH = 0.04, ECpHExtraMarge = 0.3,
                  Combine = FALSE) {
  
  C.Ca <- subset(Staal, Component == "Ca") %>% 
    Concentratie_ion(., HelftDetLim = HelftDetLim)
  C.Na <- subset(Staal, Component == "Na") %>% 
    Concentratie_ion(., HelftDetLim = HelftDetLim)
  C.K <- subset(Staal,Component == "K") %>% 
    Concentratie_ion(., HelftDetLim = HelftDetLim)
  C.Mg <- subset(Staal,Component == "Mg") %>% 
    Concentratie_ion(., HelftDetLim = HelftDetLim)
  C.SO4 <- subset(Staal,Component == "SO4") %>% 
    Concentratie_ion(., HelftDetLim = HelftDetLim)
  C.HCO3 <- subset(Staal,Component == "HCO3") %>% 
    Concentratie_ion(., HelftDetLim = HelftDetLim)
  C.Cl <- subset(Staal,Component == "Cl") %>% 
    Concentratie_ion(., HelftDetLim = HelftDetLim)
 
  Meq.Ca <- C.Ca / M.Ca * z.Ca
  Meq.Na <- C.Na / M.Na * z.Na
  Meq.K <- C.K / M.K * z.K
  Meq.Mg <- C.Mg / M.Mg * z.Mg
  
  Meq.SO4 <- C.SO4 / M.SO4 * -z.SO4
  Meq.HCO3 <- C.HCO3 / M.HCO3 * -z.HCO3
  Meq.Cl <- C.Cl / M.Cl * -z.Cl
  
  X <- c(Meq.Ca,Meq.Mg,Meq.Na + Meq.K,Meq.Cl,Meq.SO4,Meq.HCO3,Meq.Ca)
  Y <- c(2,1,0,0,1,2,2)
  
  AsXBereik <- round(max(max(X),max(-X)),digits = 0)
  if (Combine) {
    return(AsXBereik)
  }
  
  if (SchaalXVast) {AsXBereik <- XBereik}
  
  GegVoorPlot <- data.frame(X = X,Y = Y)
  G <- ggplot(data = GegVoorPlot, aes(x = X,y = Y))
  G <- G + geom_polygon(colour = LijnKleur,linewidth = LijnDikte, linetype = LijnType, fill = VulKleur, alpha = Vulling)
  G <- G + scale_x_continuous(limits = c(-AsXBereik*LabelPlaatsFactor,AsXBereik*LabelPlaatsFactor),
                              breaks = c(-AsXBereik,-AsXBereik/2,0,AsXBereik/2,AsXBereik))
  G <- G + scale_y_continuous(breaks = c(0,1,2))
  G <- G + theme(axis.text.x = element_text(size = AsLabelGrootte, colour = AsLabelKleur))
  G <- G + theme(axis.title.y = element_blank())
  G <- G + theme(axis.ticks.y = element_blank())
  G <- G + theme(axis.text.y = element_blank())
  G <- G + theme(panel.grid.minor = element_blank())
  G <- G + xlab("Concentratie (meq/l)")
  G <- G + theme(axis.title.x = element_text(size = AsTitelGrootte, colour = AsTitelKleur))

  G <- G + geom_text(label = expression("HCO"["3"]^"-"), x = -AsXBereik*(LabelPlaatsFactor + LabelPlaatsSchuif), y = 2, hjust = 0,size = StiffLabelGrootte,colour = StiffLabelKleur,fontface = StiffLabelFontface)
  G <- G + geom_text(label = expression("SO"["4"]^"2-"), x = -AsXBereik*(LabelPlaatsFactor + LabelPlaatsSchuif), y = 1, hjust = 0,size = StiffLabelGrootte,colour = StiffLabelKleur,fontface = StiffLabelFontface)
  G <- G + geom_text(label = expression("Cl"^"-"), x = -AsXBereik*(LabelPlaatsFactor + LabelPlaatsSchuif), y = 0, hjust = 0,size = StiffLabelGrootte,colour = StiffLabelKleur,fontface = StiffLabelFontface)
  G <- G + geom_text(label = expression(paste("Na"^"+"," + ","K"^"+")),x = AsXBereik*(LabelPlaatsFactor + LabelPlaatsSchuif), y = 0,hjust = 1,size = StiffLabelGrootte,colour = StiffLabelKleur,fontface = StiffLabelFontface)
  G <- G + geom_text(label = expression("Mg"^"2+"),x = AsXBereik*(LabelPlaatsFactor + LabelPlaatsSchuif), y = 1, hjust = 1,size = StiffLabelGrootte,colour = StiffLabelKleur,fontface = StiffLabelFontface)
  G <- G + geom_text(label = expression("Ca"^"2+"),x = AsXBereik*(LabelPlaatsFactor + LabelPlaatsSchuif), y = 2, hjust = 1,size = StiffLabelGrootte,colour = StiffLabelKleur,fontface = StiffLabelFontface)
  
  G <- G + theme(panel.background = element_rect(fill = PanelKleur))
  
  if (!Grid) {
    G <- G + theme(panel.grid.major = element_blank())
  }
  
  if (AsY) {
    G <- G + geom_vline(xintercept = 0,colour = AsLijnKleur,size = AsLijnDikte,linetype = AsLijnType)
  }
  
  if (AsX) {
    G <- G + theme(axis.line = element_line(colour = AsLijnKleur,size = AsLijnDikte,linetype = AsLijnType))
    G <- G + theme(axis.line.y = element_blank())
  }
  
  if (MidLijn) {
    G <- G + annotate("line", x = c(0,0), y = c(0,2), colour = KleurMidLijn, size = DikteMidLijn)
  }
  
  if (MetECpH) {
    TickLengte = 0.07
    PijlBreedte = PijlBreedteFactorECpH
    
    G <- G + annotate("line", x = c(-AsXBereik,AsXBereik), y = c(2.69, 2.69))
    G <- G + annotate("line", x = c(-AsXBereik,AsXBereik), y = c(2.75, 2.75))
    
    G <- G + annotate("line", x = c(-AsXBereik,-AsXBereik), y = c(2.75 , 2.75 + TickLengte))
    G <- G + annotate("line", x = c(AsXBereik,AsXBereik), y = c(2.75, 2.75 + TickLengte))
    
    G <- G + annotate("line", x = c(-AsXBereik,-AsXBereik), y = c(2.69 - TickLengte, 2.69))
    
    pHMin = 3
    pHMax = 11
    IntervalLengte.pH <- 2 * AsXBereik / (pHMax - pHMin)
    for (Loper in 1:((pHMax - pHMin) - 1)) {
      G <- G + annotate("line", x = c(-AsXBereik + Loper * IntervalLengte.pH,-AsXBereik + Loper * IntervalLengte.pH), y = c(2.75, 2.75 + 2/3*TickLengte))
    }
    G <- G + annotate("line", x = c(0,0), y = c(2.75, 2.75 + TickLengte))
    G <- G + annotate("line", x = c(-AsXBereik/2,-AsXBereik/2), y = c(2.75, 2.75 + TickLengte))
    G <- G + annotate("line", x = c(AsXBereik/2,AsXBereik/2), y = c(2.75, 2.75 + TickLengte))
    
    pHLoc <- (subset(Staal, Component == "pH.25")$WaardeNumeriek  - pHMin)*IntervalLengte.pH - AsXBereik
    G <- G + annotate("polygon", x = c(pHLoc,pHLoc - PijlBreedte*AsXBereik,pHLoc + PijlBreedte*AsXBereik), 
                      y = c(2.75,2.75 + PijlHoogteFactorECpH*TickLengte, 2.75 + PijlHoogteFactorECpH*TickLengte), fill = "black")
    
    # Nodig voor afstand tov grafiekbreedte !
    ECMin <- log10(0.1)
    ECMax <- log10(20)
    IntervalLengte.EC <- 2 * AsXBereik / (ECMax - ECMin)
    G <- G + annotate("line", x = c(-AsXBereik + 1*IntervalLengte.EC,-AsXBereik + 1*IntervalLengte.EC), y = c(2.69, 2.69 - TickLengte))
    G <- G + annotate("line", x = c(-AsXBereik + 2*IntervalLengte.EC,-AsXBereik + 2*IntervalLengte.EC), y = c(2.69, 2.69 - TickLengte))
    
    for (Loper in 2:9) {
      G <- G + annotate("line", x = c(-AsXBereik + log10(Loper)*IntervalLengte.EC,-AsXBereik + log10(Loper)*IntervalLengte.EC), y = c(2.69, 2.69 - 2/3*TickLengte))
    }
    for (Loper in 2:9) {
      G <- G + annotate("line", x = c(-AsXBereik + (1 + log10(Loper))*IntervalLengte.EC,-AsXBereik + (1 + log10(Loper))*IntervalLengte.EC), y = c(2.69, 2.69 - 2/3*TickLengte))
    }
    G <- G + annotate("line", x = c(-AsXBereik + (2 + log10(2))*IntervalLengte.EC,-AsXBereik + (2 + log10(2))*IntervalLengte.EC), y = c(2.69, 2.69 - 2/3*TickLengte))
    
    ECLoc <- (log10(subset(Staal, Component %in% c("EC.MEAS", "EC"))$WaardeNumeriek[1]) - log10(100))*IntervalLengte.EC - AsXBereik
    G <- G + annotate("polygon", x = c(ECLoc,ECLoc - PijlBreedte*AsXBereik,ECLoc + PijlBreedte*AsXBereik), 
                      y = c(2.69,2.69 - PijlHoogteFactorECpH*TickLengte, 2.69 - PijlHoogteFactorECpH*TickLengte), fill = "black")
    
    G <- G + annotate("text",x = 0, y = 2.75 + 4.5*TickLengte, label = "pH")
    G <- G + annotate("text",x = -AsXBereik, y = 2.75 + (2 + ECpHExtraMarge)*TickLengte, label = "3")
    G <- G + annotate("text",x = 0, y = 2.75 + (2 + ECpHExtraMarge)*TickLengte, label = "7")
    G <- G + annotate("text",x = AsXBereik, y = 2.75 + (2 + ECpHExtraMarge)*TickLengte, label = "11")
    G <- G + annotate("text",x = -AsXBereik/2, y = 2.75 + (2 + ECpHExtraMarge)*TickLengte, label = "5")
    G <- G + annotate("text",x = AsXBereik/2, y = 2.75 + (2 + ECpHExtraMarge)*TickLengte, label = "9")
    
    G <- G + annotate("text",x = 0, y = 2.69 - (3.9 + ECpHExtraMarge)*TickLengte, label = "EC")
    G <- G + annotate("text",x = -AsXBereik, y = 2.69 - (1.8 + ECpHExtraMarge)*TickLengte, label = "100")
    G <- G + annotate("text",x = -AsXBereik + 1*IntervalLengte.EC, y = 2.69 - (1.8 + ECpHExtraMarge)*TickLengte, label = "1000")
    G <- G + annotate("text",x = -AsXBereik + 2*IntervalLengte.EC, y = 2.69 - (1.8 + ECpHExtraMarge)*TickLengte, label = "10000")
    
  }
  
  Titel = NULL
  if (MetMeetpunt) {
    Titel <- paste(Titel,Staal$ExternSampleID[1])
  }
  if (MetDatum) {
    Titel <- paste(Titel,Staal$Datum[1])
  }
  if (length(Titel) > 0) {
    G <- G + ggtitle(Titel) + theme(plot.title = element_text(hjust = 0.5))
  }
  
  G <- G + theme(
    axis.text = element_text(colour = inbo_steun_donkerroos), 
    legend.text = element_text(color = inbo_steun_donkerroos))
  
  
  
  Onderschrift <- str_c("Stiff-diagram voor ",Staal$ExternSampleID[1]," op ",Staal$Datum[1],".")
  
  
  return(list(Graf = G, Bijschrift = Onderschrift))
}











IREC <- function(Stalen, HelftDetLim = FALSE, MetDatum = FALSE) {
  # Meerdere stalen kunnen ingvoerd worden!
  
  Stalen_IREC <- Stalen %>% 
    distinct() %>% 
    group_by(Project, ExternSampleID , OrigineelStaal, Datum) %>% 
    # Summarise expects single value, not a dataframe.
    do(IR = Bereken_IR(., HelftDetLim = HelftDetLim),
       EC = subset(., Component %in% c("EC.MEAS", "EC"))$WaardeNumeriek[1]) %>% 
    unnest(cols= c(IR, EC)) %>% 
    arrange(ExternSampleID, Datum) %>% 
    filter(!is.na(OrigineelStaal))

  ijkpunten <- data.frame(
    Type = c("Lithoclien", "Atmoclien", "Thalassoclien"),
    Afk  = c("Li", "At", "Th"),
    EC25 = c(65.8, 4.27, 4782),
    IR = c(94.9, 20.0, 3.7),
    # x_label = c(65.8, 3.27, 5900),
    # y_label = c(98,20.0, 1.59)
    x_label = c(65.8, 4.27, 4782),
    y_label = c(94.9, 20.0, 3.7)
  )
  
  if (MetDatum) {
    Stalen_IREC <- Stalen_IREC %>% 
      mutate(Label = str_c(ExternSampleID, " ", Datum))
  } else {
    Stalen_IREC <- Stalen_IREC %>% 
      mutate(Label = ExternSampleID)
  }
  

  G <- ggplot() + 
    geom_point(data = ijkpunten, aes(x = EC25, y = IR), pch = 21, size = 10,col = "black") +
    # geom_polygon(data = ijkpunten, aes(x = EC25, y = IR), alpha = 0) +
    geom_text(data = ijkpunten, aes(x = x_label, y = y_label, label = Afk), col = "black") +
    geom_point(data = Stalen_IREC, aes(x = EC, y = IR, colour = Label)) +
    scale_color_brewer(palette="BrBG") +
    
    
    scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x)(c(1, 1e5)), 
                  limits = c(1, 1e5), 
                  labels = trans_format("log10", math_format(10^.x))) +
    labs(x = "EGV (µS/cm)", y = "IR (%)", col = "Meetpunt") +
    theme(
      axis.text = element_text(colour = inbo_steun_donkerroos), 
      legend.text = element_text(color = inbo_steun_donkerroos),
      # axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.5),
      panel.background = element_rect(fill = NA,
                                      colour = NA,
                                      size = 0.25, linetype = "solid"),
      panel.grid.major = element_line(size  = 0.5, linetype = 'solid',
                                      colour = "gray80"),
      panel.grid.minor = element_line(size  = 0.00001, linetype = 'solid',
                                      colour = "gray90"))
  
  
  codes <- Stalen %>% 
    distinct(ExternSampleID) %>% 
    pull()
  
  Onderschrift <- str_c("IREC-diagram voor ",str_c(codes, collapse = ", "), "." )
  
  
    return(list(Graf = G, Bijschrift = Onderschrift))
}




CombineStiff <- function(chemie_code,  MetDatum = TRUE) {
  # Voegt vanaf 1e nieuwe rij gg plots toe. etc
  
  code_chemie <- chemie_code %>% 
    filter(!is.na(Monsternamedatum)) %>% 
    distinct(LaboCode, Monsternamedatum) %>% 
    arrange(Monsternamedatum)
  
  code <- chemie_code %>% 
    distinct(ExternSampleID) %>% 
    pull()
  
  # get max x-value
  c_stiff <- c()
  for (i in seq_len(nrow(code_chemie))) {
    ### STIFF ---------------------------------------
    
    staal <- chemie_code %>% 
      filter(LaboCode == code_chemie[i, "LaboCode"]) %>%
      distinct()
    
    stiff_staal <- Stiff(staal, MetDatum = MetDatum, Combine = TRUE)
    
    c_stiff <- c_stiff %>% append(., stiff_staal)
  }
  
  g <- ggplot() +
    theme_void()
  bereik <- c_stiff %>% max()
  sequentie <- seq_len(nrow(code_chemie))
  stiff_final <- NULL
  
  for (i in sequentie) {
    ### STIFF ---------------------------------------
    staal <- chemie_code %>% 
      filter(LaboCode == code_chemie[i, "LaboCode"]) %>% 
      distinct()
    
    stiff_staal <- Stiff(staal, MetDatum = MetDatum, SchaalXVast = TRUE, XBereik = bereik)$Graf
    
    if ((i - 1) %% 3 == 0) {
      stiff_row <- stiff_staal
    } else {
      stiff_row <- stiff_row + stiff_staal
    }
    
    if (i %% 3 == 0 & i != tail(sequentie, n = 1)) {
      stiff_final <- stiff_final / stiff_row
    }
    
    if (i == tail(sequentie, n = 1)) {
      
      if (!is.null(stiff_final)) {
        
        if (i %% 3 == 1) {
          stiff_row <- stiff_row + g + g
        } else if (i %% 3 == 2) {
          stiff_row <- stiff_row + g
        } 
        stiff_final <- stiff_final / stiff_row
        
      } else {
        stiff_final <- stiff_row
      }
    }
  }
  
  onderschrift <- str_c("Stiff-diagram(men) voor ", code, ".")
  
  return(list(Graf = stiff_final, Bijschrift = onderschrift))
  
}








Bereken_CO3 <- function(Staal) {
  
  ## Berekening van de [CO3]
  # De evenwichtsconstante liggen rond een bepaalde waarde bij 25°C en een ionsterkte van 0,1 mol/L:
  # 
  # De eerste evenwichtsconstante is K1 : [H2CO3] <-> [HCO3-] + [H+], waarbij
  # K1 + 1x7 . 10^-3.
  # 
  # De tweede evenwichtsconstante is [H2CO3] <-> [HCO3-] + [H+]; K2 = 4.3x10^-7.
  # 
  # Weergegeven [HCO3] is combinatie van CO3 + HCO3
  # R = [CO3-] + [HCO3]
  # 
  # [H+] <-> 10^(-pH)
  #
  # K2 omrekenen:
  #   K2 =  ( [CO3^2-]*[H+] ) / [HCO3-]
  #   K2 * [HCO3-] = (R-[HCO3]) * [H]
  #   K2 * [HCO3-] = R * [H] - [HCO3]*[H]
  #   K2* [HCO3] + [HCO3] * [H] = R * [H]
  #  Divide by (K2 + [H])
  #   [HCO3] = (R*[H]) / (K2 + [H])
   
  # CO2 is H2CO3, splitingsreactie in zuur milieu in water. 
  pH <- Staal %>% 
    filter(str_detect(Component, "pH.25")) %>% 
    pull(WaardeNumeriek)

  C.H <- 10^(-pH)
  
  K2 <- 4.3 * 10^-7
  
  # HCO3 waarde is combinatie van HCO3 en CO3
  R <- Staal %>% 
    filter(str_detect(Component, "HCO3")) %>% 
    pull(WaardeNumeriek)

  C.HCO3 <- (R*C.H) / (K2 + C.H)
  
  C.CO3 <- R - C.HCO3
    
    return(list(HCO3 = C.HCO3, CO3 = C.CO3))
}





Maucha <- function(Staal, HelftDetLim = FALSE, MetDatum = FALSE, MetMeetpunt = FALSE, 
                   hexadecagon = TRUE, labels = TRUE, 
                   radial = TRUE, # Radiaal gaat zichtbaar zijn gezien dit onderdeel is van polygoon, maar maakt wel mogelijk om dit een aandere kleur te geven als polygoon.
                   col = NULL, polygon.fill = TRUE, 
                   polygon.alpha = 1, polygon.border = "black", 
                   col.radial = "black", col.hexadecagon = "black",
                   labels.col = "black") {
  # Niet toaal alle kationen en anionen ! Enkel degene opgenomen (H ipv MAAR H en CO3 niet beschikbaar !!!)
  # Verbetering tov andere code gezien hexadecagon hier correct geplaatst wordt
  
  
  # Kationen
  C.Ca <- subset(Staal, Component == "Ca") %>%
    Concentratie_ion(., HelftDetLim = HelftDetLim)
  C.Na <- subset(Staal, Component == "Na") %>%
    Concentratie_ion(., HelftDetLim = HelftDetLim)
  C.K <- subset(Staal,Component == "K") %>%
    Concentratie_ion(., HelftDetLim = HelftDetLim)
  C.Mg <- subset(Staal,Component == "Mg") %>%
    Concentratie_ion(., HelftDetLim = HelftDetLim)
  
  Meq.Ca <- C.Ca / M.Ca * z.Ca
  Meq.Na <- C.Na / M.Na * z.Na
  Meq.K <- C.K / M.K * z.K
  Meq.Mg <- C.Mg / M.Mg * z.Mg
  
  # Anionen
  C.SO4 <- subset(Staal,Component == "SO4") %>%
    Concentratie_ion(., HelftDetLim = HelftDetLim)
  C.Cl <- subset(Staal,Component == "Cl") %>%
    Concentratie_ion(., HelftDetLim = HelftDetLim)
  
  #Controleer of CO3 en HCO3 zelf berekend dienten te worden.
  if (!c("CO3") %in% c(pull(Staal, Component))) {
    
    carbonaat <- Bereken_CO3(Staal)
    C.CO3 <- carbonaat$CO3
    C.HCO3 <- carbonaat$HCO3

  } else {
    C.HCO3 <- subset(Staal,Component == "HCO3") %>%
      Concentratie_ion(., HelftDetLim = HelftDetLim)
    C.CO3 <- subset(Staal, Component == "CO3") %>%
      Concentratie_ion(., HelftDetLim = HelftDetLim)
  }
    
  Meq.SO4 <- C.SO4 / M.SO4 * z.SO4
  Meq.HCO3 <- C.HCO3 / M.HCO3 * z.HCO3
  Meq.Cl <- C.Cl / M.Cl * z.Cl
  Meq.CO3 <- C.CO3 / M.CO3 * z.CO3

  Ionen <- data.frame(
    Ion = c("K", "Na", "Ca", "Mg", "SO4", "Cl", "HCO3", "CO3"),
    Type = c(rep("Kation", 4), rep("Anion", 4)),
    Meq = c(Meq.K, Meq.Na, Meq.Ca, Meq.Mg, Meq.SO4, Meq.Cl, Meq.HCO3,  Meq.CO3)
  ) %>% 
    group_by(Type) %>%
    # Equivalent percent (Peq)
    mutate(Peq = Meq / sum(Meq) *100) %>% 
    ungroup()
    
  
  # EC
  EC <- subset(Staal, Component %in% c("EC.MEAS", "EC"))$WaardeNumeriek[1]
  
  
  # Surface circle
  A <- sum(Ionen$Peq) * EC/20000
  # radius
  R <- sqrt(((A / 16) * 2 / sin(22.5 * pi / 180)))
  # Straal afhankelijk van equivalent per cent
  a <- Ionen %>% 
    mutate(a = Peq/ (R * sin(22.5 * pi / 180))) %>% 
    pull()
  
  
  if(is.null(col)) {
    col <- c("#843860", "#FFCD34", "#C63526", "gray",
             "#282A72", "#019966", "darkturquoise", "#8f8e94")
  } else if (length(col) != 8){
    print('Needs 8 colors as a vector with order for: "K", "Na", "Ca", "Mg", "SO4", "Cl", "HCO3", "H"')
  }
  
  # Creeer coordinaten voor polygonen
  coord.x <- matrix(NA, nrow = 5, ncol = 8)
  coord.y <- matrix(NA, nrow = 5, ncol = 8)
  coord.x[1, ] <- coord.x[5, ] <- rep(0, 8)
  coord.y[1, ] <- coord.y[5, ] <- rep(0, 8)
  coord.x[2, ] <- cos(seq(90, -225, -45) * pi / 180) * R
  coord.y[2, ] <- sin(seq(90, -225, -45) * pi / 180) * R
  coord.x[3, ] <- cos(seq(67.5, -247.5, -45) * pi / 180) * a
  coord.y[3, ] <- sin(seq(67.5, -247.5, -45) * pi / 180) * a
  coord.x[4, ] <- cos(seq(45, -270, -45) * pi / 180) * R
  coord.y[4, ] <- sin(seq(45, -270, -45) * pi / 180) * R
  
  xlim <- max(abs(c(min(coord.x[3, ]), max(coord.x[3, ]))))
  ylim <- max(abs(c(min(coord.y[3, ]), max(coord.y[3, ]))))
  alllim <- max(xlim , ylim)
  

  # Niet correct in originele code ! Wordt niet volledig rondgemaakt.
  if (hexadecagon) {
    coord.hexadecagon.x  <- matrix(NA, nrow = 3, ncol = 8)
    coord.hexadecagon.y  <- matrix(NA, nrow = 3, ncol = 8)
    coord.hexadecagon.x[1, ] <- cos(seq(90, -225, -45) * pi / 180) * R
    coord.hexadecagon.y[1, ] <- sin(seq(90, -225, -45) * pi / 180) * R
    coord.hexadecagon.x[2, ] <-  cos(seq(67.5, -247.5, -45) * pi / 180) * R
    coord.hexadecagon.y[2, ] <- sin(seq(67.5, -247.5, -45) * pi / 180) * R
    coord.hexadecagon.x[3, ] <- cos(seq(45, -270, -45) * pi / 180) * R
    coord.hexadecagon.y[3, ] <- sin(seq(45, -270, -45) * pi / 180) * R
  } 
  

  if (labels) {
    lab <- c(as.character(expression("K"^"+")), as.character(expression("Na"^"+")), as.character(expression("Ca"^"2+")), as.character(expression("Mg"^"2+")), 
             as.character(expression("SO"["4"]^"2-")), as.character(expression("Cl"^"-")), as.character(expression("HCO"["3"]^"-")), 
             as.character(expression("CO"["3"]^"2-")))
    lab.pos.x <- coord.x[3, ]
    lab.pos.y <- coord.y[3, ]
    lab.pos.x[a < R] <- coord.x[3, a < R]
    lab.pos.y[a < R] <- coord.y[3, a < R]
    lab.pos.x <- lab.pos.x + cos(seq(67.5, -247.5, -45) * pi / 180) * 2
    lab.pos.y <- lab.pos.y + sin(seq(67.5, -247.5, -45) * pi / 180) * 2
    
    xlim <- max(abs(c(min(lab.pos.x), max(lab.pos.x))))
    ylim <- max(abs(c(min(lab.pos.y), max(lab.pos.y))))
    alllim <- max(xlim , ylim)
  }


  # Grafiek
  G <- ggplot() +
    scale_x_continuous(limits = c(-xlim, xlim)) +
    scale_y_continuous(limits = c(-ylim, ylim)) +
    coord_equal() +
    theme(
      axis.text = element_text(colour = inbo_steun_donkerroos), 
      legend.text = element_text(color = inbo_steun_donkerroos),
      axis.ticks = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      plot.subtitle = element_text(hjust = 0.5)
    ) +
    labs(subtitle = str_c("Conductiviteit: ", EC, " µS/cm"))

  if (!polygon.fill) {
    col <- NA
  }
  
  G <- G +
    geom_polygon(aes(x = coord.x[, 1], y = coord.y[, 1]), 
                 fill = col[1], color = polygon.border, 
                 alpha = polygon.alpha) + 
    geom_polygon(aes(x = coord.x[, 2], y = coord.y[, 2]), 
                 fill = col[2], color = polygon.border, 
                 alpha = polygon.alpha) + 
    geom_polygon(aes(x = coord.x[, 3], y = coord.y[, 3]), 
                 fill = col[3], color = polygon.border, 
                 alpha = polygon.alpha) + 
    geom_polygon(aes(x = coord.x[, 4], y = coord.y[, 4]), 
                 fill = col[4], color = polygon.border, 
                 alpha = polygon.alpha) + 
    geom_polygon(aes(x = coord.x[, 5], y = coord.y[, 5]), 
                 fill = col[5], color = polygon.border, 
                 alpha = polygon.alpha) + 
    geom_polygon(aes(x = coord.x[, 6], y = coord.y[, 6]), 
                 fill = col[6], color = polygon.border, 
                 alpha = polygon.alpha) + 
    geom_polygon(aes(x = coord.x[, 7], y = coord.y[, 7]), 
                 fill = col[7], color = polygon.border, 
                 alpha = polygon.alpha) + 
    geom_polygon(aes(x = coord.x[, 8], y = coord.y[, 8]), 
                 fill = col[8], color = polygon.border, 
                 alpha = polygon.alpha)
    
  if (radial) {
    G <- G + 
      geom_segment(aes(x = 0, y = 0, xend = cos(pi/4)*R, yend = sin(pi/4)*R), col = col.radial) +
      geom_segment(aes(x = 0, y = 0, xend = cos(5*pi/4)*R, yend = sin(5*pi/4)*R),  col = col.radial)  +
      geom_segment(aes(x = 0, y = 0, xend = cos(0)*R, yend = sin(0)*R),  col = col.radial) +
      geom_segment(aes(x = 0, y = 0, xend = cos(pi)*R, yend = sin(pi)*R), col = col.radial) +
      geom_segment(aes(x = 0, y = 0, xend = cos(pi/2)*R, yend = sin(pi/2)*R), col = col.radial) +
      geom_segment(aes(x = 0, y = 0, xend = cos(3*pi/2)*R, yend = sin(3*pi/2)*R), col = col.radial) +
      geom_segment(aes(x = 0, y = 0, xend = cos(7*pi/4)*R, yend = sin(7*pi/4)*R), col = col.radial) +
      geom_segment(aes(x = 0, y = 0, xend = cos(3*pi/4)*R, yend = sin(3*pi/4)*R), col = col.radial)
  }
  
  if (hexadecagon) {
    G <- G + 
      geom_line(aes(x = coord.hexadecagon.x[, 1], y = coord.hexadecagon.y[, 1]), linetype = "solid", col = col.hexadecagon) +
      geom_line(aes(x = coord.hexadecagon.x[, 2], y = coord.hexadecagon.y[, 2]), linetype = "solid", col = col.hexadecagon) + 
      geom_line(aes(x = coord.hexadecagon.x[, 3], y = coord.hexadecagon.y[, 3]), linetype = "solid", col = col.hexadecagon) +
      geom_line(aes(x = coord.hexadecagon.x[, 4], y = coord.hexadecagon.y[, 4]),linetype = "solid", col = col.hexadecagon) +
      geom_line(aes(x = coord.hexadecagon.x[, 5], y = coord.hexadecagon.y[, 5]), linetype = "solid", col = col.hexadecagon) + 
      geom_line(aes(x = coord.hexadecagon.x[, 6], y = coord.hexadecagon.y[, 6]), linetype = "solid", col = col.hexadecagon) + 
      geom_line(aes(x = coord.hexadecagon.x[, 7], y = coord.hexadecagon.y[, 7]), linetype = "solid", col = col.hexadecagon) + 
      geom_line(aes(x = coord.hexadecagon.x[, 8], y = coord.hexadecagon.y[, 8]),  linetype = "solid", col = col.hexadecagon)
  }
  
  if (labels) {
    G <- G + 
      geom_text(aes(x = lab.pos.x, y = lab.pos.y, label = lab), 
                color = labels.col, parse = TRUE)
  }
  
  Titel = NULL
  if (MetMeetpunt) {
    Titel <- paste(Titel,Staal$ExternSampleID[1])
  }
  if (MetDatum) {
    Titel <- paste(Titel,Staal$Datum[1])
  }
  if (length(Titel) > 0) {
    G <- G + labs(title = Titel) + theme(plot.title = element_text(hjust = 0.5))
  }
  return(G)
}

