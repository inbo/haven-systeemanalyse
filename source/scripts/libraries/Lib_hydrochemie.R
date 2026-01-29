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
# KATIONEN
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


# Standardise ----------


StandardiseContentLims <- function(hydrochemische_data, remove_duplicates = TRUE) {
  
  if (remove_duplicates) {
    hydrochemische_data <- hydrochemische_data %>% 
      dplyr::filter(!str_starts(string = LaboCode, "D"))
  }
  
  hydrochemische_data_sddz <- hydrochemische_data %>% 
    # Structuur
    mutate(
      WaardeRuw = WaardeNumeriek
    ) %>% 
    filter(!Component %in% c("Opmerking")) %>% 
    mutate( across(.cols = c("Component", "Eenheid"), ~ str_trim(.x, side = c("both")))) %>% 
    mutate(
      Component = case_when(
        Component %in% c("MG", "MN", "NA", "SI", "AL",
                         "CA", "FE", "CL") ~ str_to_title(Component),
        Component == "Totaal Kationen" ~ "T.KATION",
        Component == "Totaal Anionen" ~ "T.ANION",
        Component == "Verschil Procentueel" ~ "VERSCHIL.IC.PROC",
        Component == "EC gemeten" ~ "EC.MEAS",
        Component == "EC berekend" ~ "EC.CALC",
        Component == "Verschil %" ~ "VERSCHIL.EC.PROC",
        Component == "Verschil Absoluut" ~ "VERSCHIL.IC.ABS",
        Component == "Verschil_ABS" ~ "VERSCHIL.EC.ABS",
        Component == "Zwev. stof 105°C" ~ "ZS.150",
        Component == "Zwev. stof 550°C" ~ "ZS.550",
        str_detect(Component, "pH") & !str_detect(Component, "VELD|veld") ~ "pH25",
        str_detect(Component, "pH") & str_detect(Component, "VELD|veld") ~ "pH.VELD",
        str_detect(Component, "EC") & str_detect(Component, "veld") ~ "EC25.VELD",
        str_detect(Component, "O2") & str_detect(Component, "veld") ~ "O2.VELD",
        Component == "Saturatie" ~ "SAT.VELD",
        Component == "Saliniteit" ~ "SAL.VELD", 
        Component %in%  c("TEMP.VELD", "Temp") ~ "Temp.VELD",
        Component %in% c("EC.25", "EC_25°C", "EC") ~ "EC25",
        Component == "CHL.A" ~ "Chlorofyl a",
        Component %in% c("T.P", "P_TOT") ~ "P.TOT",
        Component %in% c("T.S") ~ "S.TOT",
        Component %in% c("T.N") ~ "N.TOT",
        str_detect(Component, "KJEL") ~ "Kjel.N",
        TRUE ~ Component
      ),
      Eenheid = case_when(
        Component %in% c("pH25", "pH.VELD") ~ "pH",
        Component %in% c("EC25.VELD", "EC.MEAS", "EC.CALC", "VERSCHIL.EC.ABS") ~ "µS/cm",
        Component == "Kjel.N" ~ "mg N/L",
        str_detect(Component, ".TOT$") ~ str_c("mg ", str_sub(Component, 1, 1), "/L"),
        str_detect(Eenheid, "/l") ~ str_replace(Eenheid, "/l", "/L"),
        Component %in% c("COD", "BOD") ~ "mg O2/L",
        Component %in% c("P.TOT", "O2.VELD") ~ "mg/L",
        Component == "SAL.VELD" ~ "g/kg",
        TRUE ~ Eenheid),
      Matrix = str_to_title(Matrix),
      SapCode = case_when(
        Component %in% c("Chlorofyl a", "Faeofytine") ~ "SAP-158B",
        TRUE ~ str_replace(SapCode, "_.*", "") %>% 
          str_replace(., "(?<=\\p{L})\\d$", "")
      ))
  
  return(hydrochemische_data_sddz)
}



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

AddMissingEC <- function(Staal) {
  # Sommige stalen geen EC25 Maar is identiek aan EC.MEAS. Deze overnemen
  Staal_EC <- Staal %>% 
    filter(Component == "EC25")
  
  Staal_ECMEAS <- Staal %>% 
    filter(Component == "EC.MEAS")
  
  
  if (nrow(Staal_EC) == 0 & nrow(Staal_ECMEAS) == 1) {
    Staal_adj <- Staal %>% 
      bind_rows(., Staal_ECMEAS %>% 
                  mutate(
                    LimsAnalyseNaam= "Overgenomen van EC.MEAS", 
                    Component = "EC25"))
    return(Staal_adj)
  } else {
    return(Staal)
  }
}





ControleLaboVeld <- function(staal, procent, PassNoValue = FALSE) {

  # PassNoValue: if component is missing, accept it or not?
  
  # pH 
  pH_labo <- subset(staal, Component == "pH25") %>% 
    ConcentratieIon(., MissingNA = TRUE)
  pH_veld <- subset(staal, Component == "pH25.VELD") %>% 
    ConcentratieIon(., MissingNA = TRUE)

  if(any(c(is.na(pH_labo), is.na(pH_veld)) & PassNoValue)) {
    ph_controle <- TRUE
  } else if ( any(c(is.na(pH_labo), is.na(pH_veld)) & !PassNoValue)) {
    ph_controle <- FALSE
  } else {
    ph_controle <- ExpressieControle(pH_labo, pH_veld, procent = procent)
  }
  
  # EC
  EC_labo <- subset(staal, Component == "EC25") %>% 
    ConcentratieIon(., MissingNA = TRUE)
  EC_veld <- subset(staal, Component == "EC25.VELD") %>% 
    ConcentratieIon(., MissingNA = TRUE)
  if(any(c(is.na(EC_labo), is.na(EC_veld)) & PassNoValue)) {
    EC_controle <- TRUE
  } else if ( any(c(is.na(EC_labo), is.na(EC_veld)) & !PassNoValue)) {
    EC_controle <- FALSE
  } else {
    EC_controle <- ExpressieControle(EC_labo, EC_veld, procent = procent)
  }
  
  laboveld_controle <- ph_controle & EC_controle
  
  return(laboveld_controle)
}



ConcentratieIon <- function(staal_ion, HelftDetLim = FALSE, MissingNA = FALSE) {
  # Geeft de concentratie van een ion weer, rekening houdend met de detectielimiet.
  # Indien ion niet aanwezig is, zal dit waarde 0 krijgen.
  # Indien HelftDetLim == FALSE (default), dan zal de waarde bij LOQ gelijk gesteld worden aan 0.
  # Indien HelftDetLim == TRUE, dan zal de waarde bij LOQ gedeeld worden door 2.
  # MissingNA, return NA or 0 when missing. FALSE 0, TRUE NA
  
  # Ion niet aanwezig dus waarde dient NA te worden.
  if (nrow(staal_ion) == 0) {
    if (MissingNA) {
      return(NA_real_)
    } else {
      return(0)
    }
  }
  
  # Is er een detectielimiet?
  if(is.na(staal_ion$Bepaalbaarheidsgrens)) {
    return(staal_ion$WaardeRuw)
  }  
  
  if (!staal_ion$BenedenLOQ) {
    return(staal_ion$WaardeRuw)
    # Onder detectielimiet
    # Indien onder, waarde 0.
  } else if (!HelftDetLim) {
    return(0)
    # Indien onder waarde/2
  } else {
    return(staal_ion$Bepaalbaarheidsgrens/2)
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

BerekenEN <- function(staal, HelftDetLim = FALSE, MissingNA = TRUE) {
  # Elektroneneutraliteitspercentage
  # De concentraties worden omgerekend naar mili-equivalent per liter.
  # Onder kationen worden volgende zaken meegenomen, Si wordt standaard door het labo op 0 gezet dus niet mee te pakken;
  ## Ca, Na, K, Mg, NH4, Fe, Mn, Al
  ## Mn & Al niet steeds opgenomen. Ook relatief klein aandeel.
  # Onder anionen worden volgende zaken meegenomen:
  ## SO4, HCO3, Cl, NO2, NO3, PO4
  # Als er veel IJzer (Fe) in het water zit, dan wordt dit niet steeds mee opgenomen in de analyse.
  ## Boolean ingebouwd waarbij Fe 0 kan worden.
  
  C.Ca <- subset(staal, Component == "Ca") %>% 
    ConcentratieIon(.,  HelftDetLim = HelftDetLim, MissingNA = MissingNA)
  C.Na <- subset(staal, Component == "Na") %>% 
    ConcentratieIon(.,  HelftDetLim = HelftDetLim, MissingNA = MissingNA)
  C.K <- subset(staal, Component == "K") %>% 
    ConcentratieIon(.,  HelftDetLim = HelftDetLim, MissingNA = MissingNA)
  C.Mg <- subset(staal,Component == "Mg") %>% 
    ConcentratieIon(.,  HelftDetLim = HelftDetLim, MissingNA = MissingNA)
  C.Fe <- subset(staal, Component == "Fe") %>% 
    ConcentratieIon(.,  HelftDetLim = HelftDetLim, MissingNA = MissingNA)
  # Verwaarlossbaar ??
  C.NH4 <- subset(staal, Component == "NH4") %>% 
    ConcentratieIon(.,  HelftDetLim = HelftDetLim, MissingNA = MissingNA)
  
  # Toegevoegd - verwaarloosbaar.
  C.Mn <- subset(staal, Component == "Mn") %>%
    ConcentratieIon(.,  HelftDetLim = HelftDetLim, MissingNA = FALSE)
  C.Al <-  subset(staal, Component == "Al") %>%
    ConcentratieIon(.,  HelftDetLim = HelftDetLim, MissingNA = FALSE)
  C.SO4 <- subset(staal, Component == "SO4") %>% 
    ConcentratieIon(.,  HelftDetLim = HelftDetLim, MissingNA = MissingNA)
  C.HCO3 <- subset(staal, Component == "HCO3") %>% 
    ConcentratieIon(.,  HelftDetLim = HelftDetLim, MissingNA = MissingNA)
  C.Cl <- subset(staal, Component == "Cl") %>% 
    ConcentratieIon(.,  HelftDetLim = HelftDetLim, MissingNA = MissingNA)
  ## Verwaarloosbaar ?
  C.NO2 <- subset(staal, Component == "NO2") %>% 
    ConcentratieIon(.,  HelftDetLim = HelftDetLim, MissingNA = MissingNA)
  C.NO3 <- subset(staal, Component == "NO3") %>% 
    ConcentratieIon(.,  HelftDetLim = HelftDetLim, MissingNA = MissingNA)
  C.PO4 <- subset(staal, Component == "PO4") %>% 
    ConcentratieIon(.,  HelftDetLim = HelftDetLim, MissingNA = MissingNA)
  
 
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
  
  
  Meq.Anion <- sum(Meq.SO4, Meq.HCO3, Meq.Cl,  Meq.PO4, Meq.NO2, Meq.NO3)
  Meq.Kation <- sum(Meq.Ca, Meq.Na, Meq.K, Meq.Mg, Meq.NH4, Meq.Fe, Meq.Mn, Meq.Al)
  
  EN <- ((Meq.Kation - abs(Meq.Anion)) / (Meq.Kation + abs(Meq.Anion))) * 100
  return(EN)
}


ControleEN <- function(staal, procent = 10, HelftDetLim = FALSE,
                       MissingNA = TRUE
                       # , Fe = FALSE
                       ) {
  
  Controle <- procent > abs(BerekenEN(staal, HelftDetLim = HelftDetLim, MissingNA = MissingNA))
  
  return(Controle)
}

ControleStaal <- function(staal, procent_EN = 10, procent_laboveld = 10, HelftDetLim = FALSE, 
                          NoValuePassVeldLabo = FALSE, 
                          Fe = FALSE) {
  # Combines the control for each sample.
  # If a percent is NULL, this control will be skipped.
  
  if (!is.null(procent_EN)) {
    EN <- ControleEN(staal, procent = procent_EN, HelftDetLim = HelftDetLim
                     # , Fe = Fe
                     )
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

ConvertNutrient <- function(Staal, HelftDetLim = FALSE, MissingNA = TRUE) {
  
  # MissingNA zou altijd TRUE moeten zijn, want anders zal het ervan uitgaan dat het wel gemeten is maar 0 indien niet aanwezig.
  # Enkel voor waterstalen! Dus eenheid steeds mg/L
  
  
  Equivalenten <- tibble(
    origineel = c("NO2", "NO3", "NH4", "PO4", "SO4"),
    nieuw = c("NO2.N", "NO3.N", "NH4.N", "PO4.P", "SO4.S"),
    molaire_massa = c(M.NO2, M.NO3, M.NH4, M.PO4, M.SO4),
    mol_comp = c(rep(M.N, 3), M.P, M.S),
    eenheid = (c(rep("mg N/L", 3), "mg P/L", "mg S/L"))
  )
  
  staal_componenten <- Staal %>% 
    filter(Eenheid == "mg/L") %>% 
    pull(Component)
  
  Staal_adj <- Equivalenten %>% 
    filter(!(nieuw %in% staal_componenten))
  
  if (nrow(Staal_adj) != 0) {
    
    Staal_adj <- Staal_adj %>% 
      rowwise() %>% 
      mutate(
        waarde = {
          concentratie <- Staal %>%
            filter(Component == origineel) %>%
            ConcentratieIon(HelftDetLim = HelftDetLim, MissingNA = MissingNA)
          
          (concentratie / molaire_massa) * mol_comp
          },
        data = list(
          Staal %>%
            slice_head(n = 1) %>%
            mutate(
              LimsAnalyseNaam = "Berekend",
              Instrument = "Berekend",
              SapCode = "Berekend",
              Component = nieuw,
              Eenheid = eenheid,
              WaardeRuw = waarde,
              Opmerking = "Berekend",
              Bepaalbaarheidsgrens = NA,
              BenedenLOQ = NA
            )
        )) %>% 
      ungroup() %>% 
      pull(data) %>% 
      list_rbind()
  } else {
    Staal_adj <- tibble()
  }
  
  Staal_return <- Staal %>% 
    bind_rows(., Staal_adj) %>% 
    arrange(Component)
  
  return(Staal_return)
}

CalculateTotalN <- function(Staal) {
  #in mgN/L - HElftdetectimiliet dient bepaald te worden in stap COnvert Equivalent
  
  # N.TOT bestaat al
  if ("N.TOT" %in% Staal$Component) {
    return(Staal)
    } else if ("(NO3+NO2).N" %in% Staal$Component) { 
      
      N.NO2.NO3 <- subset(Staal, Component == "(NO3+NO2).N") %>% 
        ConcentratieIon(., HelftDetLim = FALSE, MissingNA = TRUE)
      N.Kjel <- subset(Staal, Component == "Kjel.N") %>% 
        ConcentratieIon(., HelftDetLim = FALSE, MissingNA = TRUE)
      
      N.TOT <- N.Kjel + N.NO2.NO3
      
      } else {
        # Als een component ontbreekt zal dit NA worden.
        N.NO2 <- subset(Staal, Component == "NO2.N") %>% 
          ConcentratieIon(., HelftDetLim = FALSE, MissingNA = TRUE)
        N.NO3 <- subset(Staal, Component == "NO3.N") %>% 
          ConcentratieIon(., HelftDetLim = FALSE, MissingNA = TRUE)
        N.Kjel <- subset(Staal, Component == "Kjel.N") %>% 
          ConcentratieIon(., HelftDetLim = FALSE, MissingNA = TRUE)
        
        N.TOT <- N.Kjel + N.NO2 + N.NO3
      }
  
  Staal_adj <- Staal %>% 
    bind_rows(., Staal %>% 
                slice_head(n = 1) %>% 
                mutate(
                  LimsAnalyseNaam= "Berekend",
                  Instrument = "Berekend",
                  Component = "N.TOT",
                  Eenheid = "mg N/L",
                  WaardeRuw = N.TOT,
                  Opmerking = "Berekend",
                  Bepaalbaarheidsgrens = NA,
                  BenedenLOQ = NA)
              ) %>% 
    arrange(Component)
  
  return(Staal_adj)
}



CalculateOrganicConcentration <- function(Staal) {
  # HelftDetLim is standard FALSE since in this step you cant change it, you cange it in 
  # ConvertEquivalent
  
  # Nitrogen
  N.NO2 <- subset(Staal, Component == "NO2.N") %>% 
    ConcentratieIon(., HelftDetLim = FALSE, MissingNA = TRUE)
  N.NO3 <- subset(Staal, Component == "NO3.N") %>% 
    ConcentratieIon(., HelftDetLim = FALSE, MissingNA = TRUE)
  N.NH4 <- subset(Staal, Component == "NH4.N") %>% 
    ConcentratieIon(., HelftDetLim = FALSE, MissingNA = TRUE)
  N.Kjel <- subset(Staal, Component == "Kjel.N") %>% 
    ConcentratieIon(., HelftDetLim = FALSE, MissingNA = TRUE)
  N.Tot <- subset(Staal, Component == "N.TOT") %>% 
    ConcentratieIon(., HelftDetLim = FALSE, MissingNA = TRUE)
  N.NO2.NO3 <- subset(Staal, Component == "(NO3+NO2).N") %>% 
    ConcentratieIon(., HelftDetLim = FALSE, MissingNA = TRUE)

  # Methode 1
  N.ORG <- N.Kjel - N.NH4
  # Methode 2
  if (is.na(N.ORG) | N.ORG < 0) {
    
    if (!is.na(N.NO2.NO3)) {
      N.ORG <- N.Tot - N.NH4 - N.NO2.NO3
    } else {
      N.ORG <- N.Tot - N.NH4 - N.NO2 - N.NO3
    }
    
  }  
  
  if (!is.na(N.ORG)) {
    if (N.ORG < 0) {N.ORG <- 0}
  }
    
  #P hosphor
  P.PO4 <- subset(Staal, Component == "PO4.P") %>% 
    ConcentratieIon(., HelftDetLim = FALSE, MissingNA = TRUE)
  P.Tot <- subset(Staal, Component == "P.TOT") %>% 
    ConcentratieIon(., HelftDetLim = FALSE, MissingNA = TRUE)
  
  P.ORG <- P.Tot - P.PO4
  
  if (!is.na(P.ORG)) {
    if (P.ORG < 0) {P.ORG <- 0}
  }
  
  Staal_adj <- Staal %>%
    # bind two new rows for N.ORG and P.ORG
    bind_rows(
      map2_dfr(
        # the two component‐names
        c("N.ORG", "P.ORG"),
        # the two corresponding values
        c(N.ORG, P.ORG),
        # for each pair, take that one base‐row and mutate it
        ~ slice_head(Staal, n = 1) %>%
          mutate(
            LimsAnalyseNaam = "Berekend",
            Instrument = "Berekend",
            Component       = .x,
            Eenheid         = str_c("mg ", str_sub(.x, 1, 1), "/L"),
            WaardeRuw       = .y,
            Opmerking = "Berekend",
            Bepaalbaarheidsgrens  = NA_real_,
            BenedenLOQ      = NA
            )
        )
      ) %>% 
    arrange(Component)
  

  return(Staal_adj)
}

CalculateEC20 <- function(Staal) {
  # HelftDetLim is standard FALSE since in this step you cant change it, you cange it in 
  # ConvertEquivalent
  
  # Nitrogen
  EC25 <- subset(Staal, Component == "EC25") %>% 
    pull(WaardeRuw)
  EC20 <- EC25 / 1.116

  Staal_adj <- Staal %>% 
    bind_rows(., Staal %>% 
                slice_head(n = 1) %>% 
                mutate(
                  LimsAnalyseNaam= "Berekend",
                  Instrument = "Berekend",
                  Component = "EC20",
                  Eenheid = "µS/cm",
                  WaardeRuw = EC20,
                  Opmerking = "Berekend",
                  Bepaalbaarheidsgrens = NA,
                  BenedenLOQ = NA)
    ) %>% 
    arrange(Component)
  
  return(Staal_adj)
}

CalculateSaturation <- function(Staal) {
  # HelftDetLim is standard FALSE since in this step you cant change it, you cange it in 
  # ConvertEquivalent
  
  SAT.VELD  <- subset(Staal, Component == "SAT.VELD") %>% 
    { if (nrow(.) != 0) .$WaardeRuw else NULL }
  
  # Already calculated
  if(!is.null(SAT.VELD)) {
    return(Staal)
  }
  
  Temp.VELD  <- subset(Staal, Component == "Temp.VELD") %>% 
    { if (nrow(.) != 0) .$WaardeRuw else NULL }
  
  O2.VELD <- subset(Staal, Component == "O2.VELD") %>% 
    { if (nrow(.) != 0) .$WaardeRuw else NULL }
  
  # Cant be calculated
  if (is.null(Temp.VELD) | is.null(O2.VELD)) {
    return(Staal)
  }
  
  Saturation_calculation <-  100*O2.VELD/ (498/(34+Temp.VELD))

  Staal_adj <- Staal %>% 
    bind_rows(., Staal %>% 
                slice_head(n = 1) %>% 
                mutate(
                  LimsAnalyseNaam= "Berekend",
                  Instrument = "Berekend",
                  Component = "SAT.VELD",
                  Eenheid = "%",
                  WaardeRuw = Saturation_calculation,
                  Opmerking = "Berekend",
                  Bepaalbaarheidsgrens = 20,
                  BenedenLOQ = if_else(Saturation_calculation > 20, FALSE, TRUE))
    ) %>% 
    arrange(Component)
  
  return(Staal_adj)
}


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


colours_components <- c(
  # EC & pH (pairing for conductivity and acidity)
  "EC25" = "#356196",  # Kleur1: Strong blue for EC (electrical conductivity) - Represents water's ability to conduct electricity.
  "pH25" = "#C77A44",  # Kleur10: Reddish-orange for pH (acidity/basicity) - Represents the pH level of water.
  
  # Chlorophyll a & Faeofytine
  "Chlorofyl a" = "#BFD6B6",  # Kleur13: Soft greenish for Chlorophyll a - Represents photosynthesis and plant life.
  "Faeofytine" = "#4D574A",   # Kleur6: Muted greenish-brown for Faeofytine - Represents chlorophyll degradation.
  
  # Ions Grouping (Cl & SO4, Na & K, Ca, Mg, HCO3)
  "Cl" = "#2F3332",   # Kleur15: Dark greyish-green for Chloride - Chloride is a common ion in seawater and industrial processes.
  "SO4" = "#ABB8B3",  # Kleur14: Soft grey-green for Sulfates - Often found in natural waters.
  "Na" = "#FA5255",   # Kleur9: Bright red for Sodium - Sodium is essential in water balance and salinity.
  "K" = "#7A282A",    # Kleur7: Rich red for Potassium - Important for plant growth and cellular processes.
  "Ca" = "#7A4A2A",   # Kleur5: Deep brown for Calcium - Affects water hardness and is crucial for biological systems.
  "Mg" = "#282B2A",   # Kleur2: Dark earthy green for Magnesium - Essential in plant metabolism and water chemistry.
  "HCO3" = "#D9D5D4", # Kleur4: Soft neutral tone for Bicarbonate - Buffers the pH of water.
  
  # BOD & COD (Both related to oxygen demand in water)
  "BOD" = "#FA5255",  # Kleur3: Tomato red for BOD - Biochemical Oxygen Demand related to organic decomposition.
  "COD" = "#541C1D",  # Kleur12: Deep red for COD - Chemical Oxygen Demand reflecting oxidation of organic material.
  
  # T.P & T.N (Nutrients contributing to eutrophication)
  
  "NO2.N" = "#BFD6B6",
  "NO3.N" = "#ABB8B3",
  "NH4.N" = "#FA5255",
  "PO4.P" = "#C77A44",
  "P.TOT" = "#2D332B",  # Kleur16: Dark brownish-green for Total Phosphorus - Key nutrient contributing to eutrophication.
  "N.TOT" = "#7A4A2A"   # Kleur5: Deep brown for Total Nitrogen - Another key nutrient in eutrophication processes.
)



labels_components <- c(
  # EC & pH (pairing for conductivity and acidity)
  "EC25" = "Geleidbaarheid",  
  "pH25" = "Zuurtegraad",
  
  # Chlorophyll a & Faeofytine
  "Chlorofyl a" = "Chlorofyl a", 
  "Faeofytine" = "Faeofytine", 
  # Ions Grouping (Cl & SO4, Na & K, Ca, Mg, HCO3)
  "Cl" = expression("Cl"^"-"),  
  "SO4" = expression("SO"["4"]^"2-"),  
  "Na" = expression("Na"^"+"),   
  "K" = expression("K"^"+"),  
  "Ca" = expression("Ca"^"2+"),  
  "Mg" = expression("Mg"^"2+"),   
  "HCO3" = expression("HCO"["3"]^"-"), 
  
  # BOD & COD (Both related to oxygen demand in water)
  "BOD" = "BOD", 
  "COD" = "COD",
  
  # T.P & T.N (Nutrients contributing to eutrophication)
  "NO3.N" = expression("NO"["3"]^"-"),
  "NO2.N" = expression("NO"["2"]^"-"),
  "NH4.N" = expression("NH"["4"]^"+"),
  "PO4.P" = expression("PO"["4"]^"3-"),
  "P.TOT" = "Totaal Fosfor (P)",
  "N.TOT" = "Totaal Stikstof (N)"   
)


labels_facet_components <- c(
  HCO3 = "HCO[3]^{'-'}",
  SO4 = "SO[4]^{'2-'}",
  Na = "Na^{'+'}",
  K = "K^{'+'}",
  Ca = "Ca^{'2+'}",
  Mg = "Mg^{'2+'}",
  Cl = "Cl^{'-'}",
  EC25 = "Geleidbaarheid",
  pH25 = "Zuurtegraad",
  "Chlorofyl a" = "Chlorofyl~a",
  Faeofytine = "Faeofytine",
  BOD = "BOD",
  COD = "COD",
  
  NO3.N = "NO[3]^{'-'}",
  NO2.N = "NO[2]^{'-'}",
  NH4.N = "NH[4]^{'+'}",
  PO4.P = "PO[4]^{'3-'}",
  
  P.TOT = "Totaal~Fosfor~(P)",
  N.TOT = "Totaal~Stikstof~(N)"
)









DataTijdreeksComponent <- function(Stalen, MeetpuntCode, Componenten, HelftDetLim = FALSE) {
  
  code_component <-  Stalen %>% 
    filter(.data$ExternSampleID %in% MeetpuntCode & .data$Component %in% Componenten)
  
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


# watertype_meetpunt <- function() {
#   
#   test <- st_read(dsn = "S:/Vlaanderen/Water/Watervlakken/Watervlakken.shp")
#   
#   test %>% 
#     distinct(KRWTYPE)
# }



AddBKNTijdreeks <- function(grafiek, Component_analyse, 
                            n_breaks = 8, BKN_watertype = "brakke polderwaterloop", data_BKN = BKN_df,
                            color_norm_min = "#FA5255", color_norm_max = "#7A282A") {

  
  BKN_component <- data_BKN %>% 
    filter(Watertype == BKN_watertype) %>% 
    filter(Component == Component_analyse)
    
    
  if (nrow(BKN_component) != 0) {
      
    max_y <- max( ggplot_build(grafiek)$layout$panel_params[[1]]$y$breaks, na.rm = TRUE)
    norm_min <- BKN_component$norm_min
    norm_max <- BKN_component$norm_max
    
    ymax <- max(max_y, norm_max)
    # # Take closest - but not needed since when it is off, probably wrong watertype
    # dmax <- abs(BKN_component$norm_max - max_y)
    # dmin <- abs(BKN_component$norm_min - max_y)
    # 
    # ymax <- if (dmax < dmin & norm_max > max_y) {
    #   norm_max 
    #   } else if (dmax < dmin & norm_max < max_y) {
    #     max_y
    #     } else if (dmax > dmin & norm_min > max_y) {
    #       norm_min
    #       } else if (dmax > dmin & norm_min < max_y) {
    #         max_y
    #         }
    
    legende <- str_c(BKN_component$Toetswijze, BKN_component$Watertype, sep = "\n")
    
    min_y <- add_min_y_axis(element = Component_analyse)
    
    grafiek <- 
      grafiek +
        scale_y_continuous(limits = c(min_y, ymax), breaks = pretty_breaks(n = n_breaks)) +
        
      geom_hline(aes(yintercept = norm_min,
                     linetype = legende), 
                 color = color_norm_min, lwd = 0.5) +
      geom_hline(aes(yintercept = norm_max,
                     linetype = legende), 
                color = color_norm_max, lwd = 0.5) +
      scale_linetype_manual(name = "Basismilieu- \nkwaliteitsnorm", values = c(1,2))
  }
  
  return(grafiek)
}

# Grafieken ------------------
TijdreeksComponenten <- function(Stalen, MeetpuntCode, Componenten, HelftDetLim = FALSE,
                                 peilbuizen_info__diepte = NULL,
                                 jaren = NULL,
                                 facet = TRUE,
                                 peilpuntcode = FALSE,
                                 color_legend = NULL,
                                 line_width = 0.75,
                                 point_size = 1,
                                 color_ondiep = Kleur1,
                                 color_diep = Kleur9,
                                 color_midden = Kleur15,
                                 components_labels = labels_components,
                                 components_facet_labels = labels_facet_components){
  
  code_component <- DataTijdreeksComponent(Stalen = Stalen, 
                                           MeetpuntCode = MeetpuntCode, Componenten = Componenten, 
                                           HelftDetLim = HelftDetLim)

  if (is.null(code_component)) {
    return(NULL)
  }
  
  code_component <- code_component %>% 
    mutate(Datum = as.Date(Datum)) %>% 
    filter(!is.na(WaardeRuw))

  G <- NULL

  if (nrow(code_component) != 0) {
    
    unit <- code_component %>% 
      distinct(Eenheid) %>% 
      pull()

    if (length(unit) != 1) {
      return("Componenten niet in dezelfde eenheid")
    }
    
    
    if (!is.null(jaren)) {
      code_component <- code_component %>% 
        filter(Jaar %in% jaren)
    }
    
    if (is.null(peilbuizen_info__diepte)) {
      
      G <- ggplot(data = code_component, aes(x = Datum, y = WaardeRuw, col = Component)) +
        geom_point(size = 1.5) +
        geom_line(linewidth = 1.5) +
        labs(x = "", y = unit, parse = TRUE)
      
      
      if (!is.null(color_legend)) {
        
        G <- G +
          scale_color_manual(values = color_legend, labels = components_labels)
      } else {
        G <- G + 
          scale_color_discrete(labels = components_labels)
      }
       
    } else {
      
      peilbuizen_info__diepte <- peilbuizen_info__diepte %>% 
        mutate(Kleur = case_when(
          Diepte == min(Diepte) ~ color_ondiep,
          Diepte == max(Diepte) ~ color_diep,
          TRUE ~ color_midden
        )) %>% 
        arrange(MeetpuntCode)
      
      variables <- peilbuizen_info__diepte %>% 
        pull(MeetpuntCode)
      
      colors_variables <- peilbuizen_info__diepte %>% 
        pull(Kleur)
      
      
      G <- ggplot(data = code_component, aes(x = Datum, y = WaardeRuw, col = ExternSampleID)) +
        geom_point(size = point_size) +
        geom_line(linewidth = line_width) +
        scale_color_manual(values = colors_variables, breaks = variables, labels = variables) +
        labs(x = "", y = unit, colour = "Meetpunt") +
        guides(colour = guide_legend(reverse = TRUE))  
    }
    
    
    if (facet) {
      
      G <- G +
        facet_wrap(~ Component, scales  = "free_y", 
                   labeller = labeller(Component = as_labeller(components_facet_labels, label_parsed))) +
        scale_x_date(date_labels = "%b %Y", date_breaks = "1 year")
      
      if (is.null(peilbuizen_info__diepte)) {
        G <- G +
          guides(color = "none") 
        }
    } else {
      G <- G +
        scale_x_date(date_labels = "%b %Y", date_breaks = "6 months")
      
        # scale_y_continuous(limits = c(RoundAny(min(code_component$Waarde, na.rm = TRUE), 1, f = floor), 
        #                               RoundAny(max(code_component$Waarde, na.rm = TRUE), 1, f = ceiling)))
    }
  }
  
  
  if (peilpuntcode) {
    G <- G +
      labs(subtitle = str_c(MeetpuntCode, collapse = "-"))
  }
  
  Onderschrift <- str_c("Tijdreeks van ",str_c(Componenten,  collapse = " en "), 
                           " voor ", str_c(MeetpuntCode, collapse = " en "), ".")

  return(list(Graf = G, Bijschrift = Onderschrift))
}

add_min_y_axis <- function(element, min_y = 0, min_pH = 6) {
  # Used ins cale_y_axis() & AddBKNTijdreeks()
  
  y_min <- if_else(str_detect(element, "pH"), min_pH, min_y)
  
  return(y_min)
}


scale_y_axis <- function(figuur, n_breaks = 8, componenten, meetpunten, hydrochemische_data) {
  
  
  minmax_value <- hydrochemische_data %>% 
    filter(ExternSampleID %in% meetpunten & Component %in% componenten) %>% 
    summarise(
      min_y = min(WaardeRuw, na.rm = TRUE),
      max_y = max(WaardeRuw, na.rm = TRUE)
      ) %>% 
    mutate(
      # smallest = nchar(min_y),
      # base = 10 ^ (smallest - 1),
      magnitude = 10 ^ floor(log10(min_y))) %>% 
    mutate(
      min_y_round = RoundAny(min_y, magnitude, f = floor),
      max_y_round = RoundAny(max_y, magnitude, f = ceiling),
    ) %>% 
    mutate(
      min_y_round = if_else(is.nan(min_y_round), min_y, min_y_round),
      max_y_round = if_else(is.nan(max_y_round), max_y, max_y_round))
  
  y_min <- add_min_y_axis(element = componenten)

  
  figuur <- figuur  +
    scale_y_continuous(limits = c(
      # minmax_value$min_y_round
      y_min
      , minmax_value$max_y_round),
                       breaks = pretty_breaks(n = n_breaks))
  
  return(figuur)
}




# Grafieken --------------------------
Kleur_Stiff <- rgb(53,97,150,maxColorValue = 255)
Stiff <- function(Staal, HelftDetLim = FALSE, MetECpH = TRUE, MetDatum = FALSE, MetMeetpunt = FALSE,
                  XMaxBereik = NULL, AsX = TRUE, AsY = FALSE, MidLijn = TRUE, DikteMidLijn = 0.5, 
                  KleurMidLijn = Kleur_Stiff, LijnKleur = Kleur_Stiff, LijnDikte = 1, LijnType = "solid", Vulling = 0.5, 
                  VulKleur = Kleur_Stiff, LabelPlaatsFactor = 1.5, LabelPlaatsSchuif = 0, AsLabelGrootte = 11, 
                  AsLabelKleur = Kleur16, 
                  AsTitelGrootte = 11, AsTitelKleur = "black", StiffLabelGrootte = 3, StiffLabelKleur = "black", 
                  StiffLabelFontface = "plain", Grid = FALSE, PanelKleur = "white", AsLijnKleur = "black", 
                  TitleGrootte = 12,
                  AsLijnDikte = 0.5, AsLijnType = "solid", LabelFactorECpH = 1.2, PijlHoogteFactorECpH = 1.2, 
                  PijlBreedteFactorECpH = 0.04, ECpHExtraMarge = 0.3,
                  GetMaxX = FALSE) {
  # GetMaxX -> Get the maximu value for the x-value
  # If XMAxBereik is Null, get the defauflt, otherwise the value you put in (max from difetrent GetMaxX = TRUE)
  
  
  
  C.Ca <- subset(Staal, Component == "Ca") %>% 
    ConcentratieIon(., HelftDetLim = HelftDetLim)
  C.Na <- subset(Staal, Component == "Na") %>% 
    ConcentratieIon(., HelftDetLim = HelftDetLim)
  C.K <- subset(Staal,Component == "K") %>% 
    ConcentratieIon(., HelftDetLim = HelftDetLim)
  C.Mg <- subset(Staal,Component == "Mg") %>% 
    ConcentratieIon(., HelftDetLim = HelftDetLim)
  C.SO4 <- subset(Staal,Component == "SO4") %>% 
    ConcentratieIon(., HelftDetLim = HelftDetLim)
  C.HCO3 <- subset(Staal,Component == "HCO3") %>% 
    ConcentratieIon(., HelftDetLim = HelftDetLim)
  C.Cl <- subset(Staal,Component == "Cl") %>% 
    ConcentratieIon(., HelftDetLim = HelftDetLim)
 
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
  if (GetMaxX) {
    return(AsXBereik)
  }
  
  if (!is.null(XMaxBereik)) {AsXBereik <- XMaxBereik}
  
  G <- ggplot(data = data.frame(X = X,Y = Y), aes(x = X,y = Y)) +
    geom_polygon(colour = LijnKleur,linewidth = LijnDikte, linetype = LijnType, fill = VulKleur, alpha = Vulling)+ 
    scale_x_continuous(limits = c(-AsXBereik*LabelPlaatsFactor,AsXBereik*LabelPlaatsFactor),
                       breaks = c(-AsXBereik,-AsXBereik/2,0,AsXBereik/2,AsXBereik),
                       labels = abs) + 
    scale_y_continuous(breaks = c(0,1,2)) + 
    xlab("Concentratie (meq/l)") +
    geom_text(label = expression("HCO"["3"]^"-"), x = -AsXBereik*(LabelPlaatsFactor + LabelPlaatsSchuif), y = 2, 
              hjust = 0,size = StiffLabelGrootte,colour = StiffLabelKleur,fontface = StiffLabelFontface) +
    geom_text(label = expression("SO"["4"]^"2-"), x = -AsXBereik*(LabelPlaatsFactor + LabelPlaatsSchuif), y = 1, 
              hjust = 0,size = StiffLabelGrootte,colour = StiffLabelKleur,fontface = StiffLabelFontface) +
    geom_text(label = expression("Cl"^"-"), x = -AsXBereik*(LabelPlaatsFactor + LabelPlaatsSchuif), y = 0, 
              hjust = 0,size = StiffLabelGrootte,colour = StiffLabelKleur,fontface = StiffLabelFontface) +
    geom_text(label = expression(paste("Na"^"+"," + ","K"^"+")),x = AsXBereik*(LabelPlaatsFactor + LabelPlaatsSchuif), y = 0,
              hjust = 1,size = StiffLabelGrootte,colour = StiffLabelKleur,fontface = StiffLabelFontface) +
    geom_text(label = expression("Mg"^"2+"),x = AsXBereik*(LabelPlaatsFactor + LabelPlaatsSchuif), y = 1, 
              hjust = 1,size = StiffLabelGrootte,colour = StiffLabelKleur,fontface = StiffLabelFontface) +
    geom_text(label = expression("Ca"^"2+"),x = AsXBereik*(LabelPlaatsFactor + LabelPlaatsSchuif), y = 2, 
              hjust = 1,size = StiffLabelGrootte,  colour = StiffLabelKleur, fontface = StiffLabelFontface) +
    theme(axis.text.x = element_text(size = AsLabelGrootte, colour = AsLabelKleur),
          axis.title.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.y = element_blank(),
          panel.grid.minor = element_blank(),
          axis.title.x = element_text(size = AsTitelGrootte, colour = AsTitelKleur),
          panel.background = element_rect(fill = PanelKleur)
    )
  
  if (!Grid) {
    G <- G + 
      theme(panel.grid.major = element_blank())
  }
  
  if (AsY) {
    G <- G + 
      geom_vline(xintercept = 0,colour = AsLijnKleur,size = AsLijnDikte,linetype = AsLijnType)
  }
  
  if (AsX) {
    G <- G + 
      theme(axis.line = element_line(colour = AsLijnKleur,size = AsLijnDikte,linetype = AsLijnType),
            axis.line.y = element_blank())
  }
  
  if (MidLijn) {
    G <- G + 
      annotate("line", x = c(0,0), y = c(0,2), colour = KleurMidLijn, size = DikteMidLijn)
  }
  
  if (MetECpH) {
    TickLengte = 0.07
    PijlBreedte = PijlBreedteFactorECpH
    
    G <- G + 
      annotate("line", x = c(-AsXBereik,AsXBereik), y = c(2.69, 2.69)) +
      annotate("line", x = c(-AsXBereik,AsXBereik), y = c(2.75, 2.75)) +
      annotate("line", x = c(-AsXBereik,-AsXBereik), y = c(2.75 , 2.75 + TickLengte)) +
      annotate("line", x = c(AsXBereik,AsXBereik), y = c(2.75, 2.75 + TickLengte)) +
      annotate("line", x = c(-AsXBereik,-AsXBereik), y = c(2.69 - TickLengte, 2.69))
    
    pHMin = 3
    pHMax = 11
    IntervalLengte.pH <- 2 * AsXBereik / (pHMax - pHMin)
    for (Loper in 1:((pHMax - pHMin) - 1)) {
      G <- G + 
        annotate("line", x = c(-AsXBereik + Loper * IntervalLengte.pH,-AsXBereik + Loper * IntervalLengte.pH), y = c(2.75, 2.75 + 2/3*TickLengte))
    }
    
    pHLoc <- (subset(Staal, Component == "pH25")$WaardeRuw - pHMin)*IntervalLengte.pH - AsXBereik
    G <- G + 
      annotate("line", x = c(0,0), y = c(2.75, 2.75 + TickLengte)) +
      annotate("line", x = c(-AsXBereik/2,-AsXBereik/2), y = c(2.75, 2.75 + TickLengte)) +
      annotate("line", x = c(AsXBereik/2,AsXBereik/2), y = c(2.75, 2.75 + TickLengte)) +
      annotate("polygon", x = c(pHLoc, pHLoc - PijlBreedte*AsXBereik,pHLoc + PijlBreedte*AsXBereik), 
                      y = c(2.75,2.75 + PijlHoogteFactorECpH*TickLengte, 2.75 + PijlHoogteFactorECpH*TickLengte), fill = "black")
    
    # Nodig voor afstand tov grafiekbreedte !
    ECMin <- log10(0.1)
    ECMax <- log10(20)
    IntervalLengte.EC <- 2 * AsXBereik / (ECMax - ECMin)
    G <- G + 
      annotate("line", x = c(-AsXBereik + 1*IntervalLengte.EC,-AsXBereik + 1*IntervalLengte.EC), y = c(2.69, 2.69 - TickLengte)) +
      annotate("line", x = c(-AsXBereik + 2*IntervalLengte.EC,-AsXBereik + 2*IntervalLengte.EC), y = c(2.69, 2.69 - TickLengte))
    
    for (Loper in 2:9) {
      G <- G + 
        annotate("line", x = c(-AsXBereik + log10(Loper)*IntervalLengte.EC,-AsXBereik + log10(Loper)*IntervalLengte.EC), y = c(2.69, 2.69 - 2/3*TickLengte))
    }
    for (Loper in 2:9) {
      G <- G + 
        annotate("line", x = c(-AsXBereik + (1 + log10(Loper))*IntervalLengte.EC,-AsXBereik + (1 + log10(Loper))*IntervalLengte.EC), 
                 y = c(2.69, 2.69 - 2/3*TickLengte))
    }
    G <- G + 
      annotate("line", x = c(-AsXBereik + (2 + log10(2))*IntervalLengte.EC,-AsXBereik + (2 + log10(2))*IntervalLengte.EC), y = c(2.69, 2.69 - 2/3*TickLengte))
    
    ECLoc <- (log10(subset(Staal, Component %in% c("EC25"))$WaardeRuw) - log10(100))*IntervalLengte.EC - AsXBereik
    
    
    G <- G + 
      annotate("polygon", x = c(ECLoc,ECLoc - PijlBreedte*AsXBereik,ECLoc + PijlBreedte*AsXBereik), 
                      y = c(2.69,2.69 - PijlHoogteFactorECpH*TickLengte, 2.69 - PijlHoogteFactorECpH*TickLengte), fill = "black") + 
      annotate("text",x = 0, y = 2.75 + 4.5*TickLengte, label = "pH") + 
      annotate("text",x = -AsXBereik, y = 2.75 + (2 + ECpHExtraMarge)*TickLengte, label = "3") +
      annotate("text",x = 0, y = 2.75 + (2 + ECpHExtraMarge)*TickLengte, label = "7") +
      annotate("text",x = AsXBereik, y = 2.75 + (2 + ECpHExtraMarge)*TickLengte, label = "11") +
      annotate("text",x = -AsXBereik/2, y = 2.75 + (2 + ECpHExtraMarge)*TickLengte, label = "5") +
      annotate("text",x = AsXBereik/2, y = 2.75 + (2 + ECpHExtraMarge)*TickLengte, label = "9") +
      annotate("text",x = 0, y = 2.69 - (3.9 + ECpHExtraMarge)*TickLengte, label = "EC") + 
      annotate("text",x = -AsXBereik, y = 2.69 - (1.8 + ECpHExtraMarge)*TickLengte, label = "100") +
      annotate("text",x = -AsXBereik + 1*IntervalLengte.EC, y = 2.69 - (1.8 + ECpHExtraMarge)*TickLengte, label = "1000") +
      annotate("text",x = -AsXBereik + 2*IntervalLengte.EC, y = 2.69 - (1.8 + ECpHExtraMarge)*TickLengte, label = "10000")
  }
  
  Titel = NULL
  if (MetMeetpunt) {
    Titel <- paste(Titel,Staal$ExternSampleID[1])
  }
  if (MetDatum) {
    Titel <- paste(Titel,format(Staal$Datum[1], "%d-%m-%Y"))
  }
  if (length(Titel) > 0) {
    G <- G + 
      ggtitle(Titel) + 
      theme(plot.title = element_text(hjust = 0.5, size = TitleGrootte))
  }
  
  G <- G + 
    theme(
    axis.text = element_text(colour = inbo_steun_donkerroos), 
    legend.text = element_text(color = inbo_steun_donkerroos))
  
  
  
  Onderschrift <- str_c("Stiff-diagram voor ",Staal$ExternSampleID[1]," op ",Staal$Datum[1],".")
  
  
  return(list(Graf = G, Bijschrift = Onderschrift))
}



# IREC <- function(Stalen, HelftDetLim = FALSE, MetDatum = FALSE) {
#   # Meerdere stalen kunnen ingvoerd worden!
#   
#   Stalen_IREC <- Stalen %>% 
#     distinct() %>% 
#     group_by(Project, ExternSampleID , OrigineelStaal, Datum) %>% 
#     # Summarise expects single value, not a dataframe.
#     do(IR = Bereken_IR(., HelftDetLim = HelftDetLim),
#        EC = subset(., Component %in% c("EC.MEAS", "EC"))$WaardeNumeriek[1]) %>% 
#     unnest(cols= c(IR, EC)) %>% 
#     arrange(ExternSampleID, Datum) %>% 
#     filter(!is.na(OrigineelStaal))
# 
#   ijkpunten <- data.frame(
#     Type = c("Lithoclien", "Atmoclien", "Thalassoclien"),
#     Afk  = c("Li", "At", "Th"),
#     EC25 = c(65.8, 4.27, 4782),
#     IR = c(94.9, 20.0, 3.7),
#     # x_label = c(65.8, 3.27, 5900),
#     # y_label = c(98,20.0, 1.59)
#     x_label = c(65.8, 4.27, 4782),
#     y_label = c(94.9, 20.0, 3.7)
#   )
#   
#   if (MetDatum) {
#     Stalen_IREC <- Stalen_IREC %>% 
#       mutate(Label = str_c(ExternSampleID, " ", Datum))
#   } else {
#     Stalen_IREC <- Stalen_IREC %>% 
#       mutate(Label = ExternSampleID)
#   }
#   
# 
#   G <- ggplot() + 
#     geom_point(data = ijkpunten, aes(x = EC25, y = IR), pch = 21, size = 10,col = "black") +
#     # geom_polygon(data = ijkpunten, aes(x = EC25, y = IR), alpha = 0) +
#     geom_text(data = ijkpunten, aes(x = x_label, y = y_label, label = Afk), col = "black") +
#     geom_point(data = Stalen_IREC, aes(x = EC, y = IR, colour = Label)) +
#     scale_color_brewer(palette="BrBG") +
#     
#     
#     scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x)(c(1, 1e5)), 
#                   limits = c(1, 1e5), 
#                   labels = trans_format("log10", math_format(10^.x))) +
#     labs(x = "EGV (µS/cm)", y = "IR (%)", col = "Meetpunt") +
#     theme(
#       axis.text = element_text(colour = inbo_steun_donkerroos), 
#       legend.text = element_text(color = inbo_steun_donkerroos),
#       # axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.5),
#       panel.background = element_rect(fill = NA,
#                                       colour = NA,
#                                       size = 0.25, linetype = "solid"),
#       panel.grid.major = element_line(size  = 0.5, linetype = 'solid',
#                                       colour = "gray80"),
#       panel.grid.minor = element_line(size  = 0.00001, linetype = 'solid',
#                                       colour = "gray90"))
#   
#   
#   codes <- Stalen %>% 
#     distinct(ExternSampleID) %>% 
#     pull()
#   
#   Onderschrift <- str_c("IREC-diagram voor ",str_c(codes, collapse = ", "), "." )
#   
#   
#     return(list(Graf = G, Bijschrift = Onderschrift))
# }
# 
# 
# 
# 
# CombineStiff <- function(chemie_code,  MetDatum = TRUE) {
#   # Voegt vanaf 1e nieuwe rij gg plots toe. etc
#   
#   code_chemie <- chemie_code %>% 
#     distinct(LaboCode, Datum) %>% 
#     arrange(Datum)
#   
#   code <- chemie_code %>% 
#     distinct(ExternSampleID) %>% 
#     pull()
#   
#   # get max x-value
#   c_stiff <- c()
#   for (i in seq_len(nrow(code_chemie))) {
#     ### STIFF ---------------------------------------
#     
#     staal <- chemie_code %>% 
#       filter(LaboCode == code_chemie[i, "LaboCode"])
#     
#     stiff_staal <- Stiff(staal, MetDatum = MetDatum, Combine = TRUE)
#     
#     c_stiff <- c_stiff %>% append(., stiff_staal)
#   }
#   
#   g <- ggplot() +
#     theme_void()
#   bereik <- c_stiff %>% max()
#   sequentie <- seq_len(nrow(code_chemie))
#   stiff_final <- NULL
#   
#   for (i in sequentie) {
#     ### STIFF ---------------------------------------
#     staal <- chemie_code %>% 
#       filter(LaboCode == code_chemie[i, "LaboCode"]) %>% 
#       distinct()
#     
#     stiff_staal <- Stiff(staal, MetDatum = MetDatum, SchaalXVast = TRUE, XBereik = bereik)$Graf
#     
#     if ((i - 1) %% 3 == 0) {
#       stiff_row <- stiff_staal
#     } else {
#       stiff_row <- stiff_row + stiff_staal
#     }
#     
#     if (i %% 3 == 0 & i != tail(sequentie, n = 1)) {
#       stiff_final <- stiff_final / stiff_row
#     }
#     
#     if (i == tail(sequentie, n = 1)) {
#       
#       if (!is.null(stiff_final)) {
#         
#         if (i %% 3 == 1) {
#           stiff_row <- stiff_row + g + g
#         } else if (i %% 3 == 2) {
#           stiff_row <- stiff_row + g
#         } 
#         stiff_final <- stiff_final / stiff_row
#         
#       } else {
#         stiff_final <- stiff_row
#       }
#     }
#   }
#   
#   onderschrift <- str_c("Stiff-diagram(men) voor ", code, ".")
#   
#   return(list(Graf = stiff_final, Bijschrift = onderschrift))
#   
# }
# 


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

