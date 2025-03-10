# Data-ophalende functies ------------------------------------------------------------

GetWatinaMeetpunt <- function(dbase_connection, gebied){
  dbGetQuery(dbase_connection, paste0(
    "SELECT
    M.MeetpuntCode,
    M.MeetpuntStatus,
    M.MeetpuntType,
    M.X,
    M.Y,
    M.GebiedCode,
    M.GebiedNaam
  FROM report.vw_Meetpunt M
  WHERE M.GebiedCode IN (", paste0("\'", gebied, "\'", collapse = ", "), ")"))
}


GetWatinaPeilpunten <- function(dbase_connection, gebied){
  # Querry to get data from wationa for Meetpunten
  dbGetQuery(dbase_connection, paste0(
    "SELECT
      P.MeetpuntCode,
      P.PeilpuntCode,
      P.PeilpuntVersie,
      P.PeilpuntStatus,
      P.PeilpuntToestand,
      P.PeilpuntType,
      P.PeilpuntLengteBuis,
      P.PeilpuntLengteFilter,
      P.PeilpuntTAWNulpunt,
      P.PeilpuntHoogteBovenMaaiveld,
      P.PeilpuntTAWMaaiveld,
      P.X,
      P.Y,
      P.GebiedCode,
      P.GebiedNaam,
      P.PeilpuntStartdatum,
      P.PeilpuntEinddatum
    FROM report.vw_Peilpunt P
    WHERE P.GebiedCode IN (", paste0("\'", gebied, "\'", collapse = ", "), ")"))
}


GetWatinaTijdreeks <- function(dbase_connection, Peilpunt){
  if (length(Peilpunt) < 8) {Peilpunt <- str_c(Peilpunt,"%")}
  dbGetQuery(dbase_connection, glue_sql(
    "SELECT
       PM.MeetpuntCode,
       PM.PeilpuntCode,
       PM.PeilmetingDatum,
       PM.PeilmetingDateTime,
       PM.PeilmetingType,
       PM.PeilmetingStatus,
       PM.PeilmetingCategorie,
       PM.PeilmetingMeetwaarde,
       PM.PeilmetingTAW,
       PM.PeilmetingMaaiveld,
       PM.PeilmetingCommentaar
     FROM report.vw_PeilMeting PM
     WHERE PeilpuntCode LIKE {Peilpunt}",
    Peilpunt = Peilpunt,
    .con = dbase_connection))
}


# GetDruksondeRelatie <- function(dbase_connection, meetpunt) {
#   meetpunt <- str_c(meetpunt,"%")
#   dbGetQuery(dbase_connection, glue_sql(
#     "SELECT
#       P.Hoofdtype,
#       P.LaatstActiefin,
#       P.GestartOp,
#       P.StartdatumGekend,
#       P.ActiefTot,
#       P.EinddatumGekend,
#       P.EersteMeting,
#       P.LaatsteMeting
#     FROM report.vw_Druksonde P
#     WHERE LaatstActiefin LIKE {meetpunt}",
#     meetpunt = meetpunt, 
#     .con = dbase_connection))
# }



# Parameters & Data inlezen ----------------

watina <- connect_inbo_dbase(database_name = "D0025_00_Watina")  # Open data-set
watina_havens_peilpuntinfo <- GetWatinaPeilpunten(watina, c("AHR", "WAH"))
dbDisconnect(watina)  # close dataset

neerslagoverschot_gegevens_df <- readxl::read_xlsx("Data/Gegevens/NeerslagOverschot.xlsx", na = "") %>% 
  #  Warning that there is missing value in the data. Silence the warning.
  suppressWarnings()

# kwelpunten <- readxl::read_xlsx("Data/Gegevens/Kwelpunten.xlsx", na = "")


Schothoogte <- data.frame(
  Gebied = c("Doelpolder Noord", "Putten Weide", "Putten West"),
  Schothoogte = c(2.75, 1.20, 1.80))


## Status meetpunt naast gevalideerd ook ingegeven? (default = FALSE):
ingegeven <- TRUE

## Controle
RP_dagen <- 30  # Representatieve periode max 30 dagen
percent_decimal <- 0.8  # Hoeveel percent van de stalen dienen aanwezig te zijn tussen een bepaalde peridoe
min_aantal_meting <- 20 # Voor hydrologisch jaar, min aantal metingen (is ook default 20.)

## tijdspanne tussen metingen
tijdspanne <- 14  # GH3 en GL3
tijdspanne_GV3 <- 12  # GV3
tijdvenster <- 21  # tijdvenster tussen grootste spreiding.


# Ondersteunende functies ------------------------------------------------------

RoundAny = function(x, accuracy, f=round){
  # Rond een getal af op een in te geven cijfer.
  # x is het getal, accuracy de cijfer waarnaar het dient af te ronden
  # f de formule voor ceiling (dichtbijzijnde naar boven afgerond) of floor (dichtsbijzijnde naar onder afgerond).
  
  f(x/ accuracy) * accuracy
}


AdaptTijdreeks <- function(tijdreeks, ingegeven = FALSE) {
  # Filter de tijdreeks zodat kalibratiemetingen verwijdert worden, en voeg een variabele toe met het hydrologisch jaar en het gewone jaar.
  # Past de metingen aan met Categroie "geen water in peilbuis" naar NA.
  # Filter ook of data 'ingegeven' dient meegenomen te worden of niet.
  
  tijdreeks_ <- tijdreeks %>% 
    filter(., PeilmetingType != "Kalibratiemeting",
           PeilmetingStatus != "Verwijderd") %>% 
    filter(., !PeilmetingCategorie  %in% c("originele kalibratiemeting in het veld")) %>% 
    # Bereken hydrologisch jaar voor observatie. Bv. voor 2017: 01/04/'17-31/03/'18.
    # Indien datum voor 04/01 dan is het hydrologisch jaar van die datum het jaar - 1.
    dplyr::mutate(HydrologischJaar = if_else(PeilmetingDatum >= as_date(str_c(year(PeilmetingDatum), "-04-01"), format = "%Y-%m-%d"), 
                                             year(PeilmetingDatum), 
                                             as.double(year(PeilmetingDatum) - 1)),
                  Jaar = year(PeilmetingDatum)) %>% 
    arrange(HydrologischJaar, PeilmetingDateTime) %>% 
    distinct() %>%  
    #  Bodempeil, indien categorie = geen water in peilbuis, waarde moet NA zijn voor die kolommen. 
    dplyr::mutate(across(.cols = c(PeilmetingMeetwaarde, PeilmetingTAW, PeilmetingMaaiveld), 
                         ~if_else(PeilmetingCategorie == "geen water in peilbuis", 
                                  NA_real_, .x, 
                                  missing = .x)))
  
  if (ingegeven) {
    tijdreeks_adapt <- tijdreeks_ %>% 
      filter(PeilmetingStatus %in% c("Gevalideerd", "Ingegeven")) 
  } else {
    tijdreeks_adapt <- 
      tijdreeks_ %>% filter(PeilmetingStatus %in% c("Gevalideerd"))
  }
  
  return(tijdreeks_adapt)
}



KiesMeetMethode <- function(tijdreeks, TAW = TRUE, PeilpuntTAWMaaiveld = NULL) {
  # Keuze welke waarde gebruikt wordt. Bij default is dit TAW (TRUE). 
  # Gezien alle functies gebasseerd zijn op de waarde in kolom "PeilmetingTAW'
  # wordt indien er met het maaiveld gerekend te worden deze kolom overschreven:
  # Indien maaiveld leeg blijft (NULL - default), dan wordt het bestaande maaiveld gebruikt
  # (kolom wordt hernoemd naar 'PeilmetingTAW")
  # Indien maaiveld zelf bepaald dient te worden : ingegeven maaiveld - PeilmetingTAW
  if (!TAW) {
    if (!is.null(PeilpuntTAWMaaiveld)) {
      tijdreeks_ <- tijdreeks %>% 
        mutate(
          PeilmetingTAW_echt = PeilmetingTAW,
               # PeilmetingMaaiveld = PeilmetingTAW - PeilpuntTAWnulpunt - PeilpuntHoogteBovenMaaiveld 
               # of PeilmetingMaaiveld = PeilmetingTAW - PeilpuntTAWmaaiveld
               # PeilpuntTAWmaaiveld = PeilpuntTAWNulpunt - PeilpuntHoogteBovenMaaiveld
          PeilmetingTAW = PeilmetingTAW - PeilpuntTAWMaaiveld)
      
    } else {
      tijdreeks_ <- tijdreeks %>%
        mutate(PeilmetingTAW_echt = PeilmetingTAW,
               PeilmetingTAW = PeilmetingMaaiveld)
    }
  } else {
    tijdreeks_ <- tijdreeks
    }
  return(tijdreeks_)
}



AddBodempeil <- function(tijdreeks, watina_havens_peilpuntinfo) {
  # Bereken het bodempeil per PEILmeetpunt.
  # Houd rekening dat er meerdere unieke peilpunten per meetpunt kan zijn.
  # Indien geen bodempeil berekend kan worden zal dit NA zijn.
  # Bodempeil wordt berkend door PeilpuntTAWnulpunt - Lengte Peilbuis
    ##   # watina_havens_bodempeil is de dataframe over de peilpunten met extra informatie
  
  watina_havens_bodempeil_ <- watina_havens_peilpuntinfo %>% 
    mutate(Bodempeil = PeilpuntTAWNulpunt - PeilpuntLengteBuis) %>% 
    select(MeetpuntCode, PeilpuntCode, PeilpuntTAWNulpunt, PeilpuntLengteBuis, Bodempeil) %>% 
    arrange(MeetpuntCode)
  
  tijdreeks_bodempeil <- left_join(tijdreeks, watina_havens_bodempeil_, by = c("MeetpuntCode", "PeilpuntCode")) 
  
  return(tijdreeks_bodempeil)
}




## GET RP
GetRP <- function(tijdreeks, aangepaste_periode = FALSE, periode_begin = NULL, periode_einde = NULL, ingegeven = FALSE) {
  # Berekend de representatieve periode tussen de observaties. 
  # RP voor een meting wordt gedefinieerd als de periode van halfweg de vorige meting tot halfweg de volgende meting (uitgedrukt in dagen).
  # Voor het berekenen van de RP voor de eerste en laatste meting  wordt de afstand tot de voorgaande/volgende meting standaard gerekend voor:
  ##  Aangepaste periode; periode begin en en periode einde
  ##  Hydrologisch jaar : 1 april en 31 maart.
  
  # PARAMETERS
  ## aangepaste periode = FALSE (default). Indien FALSE wordt er met het hydroloigsch jaar gewerkt. 
  ##Periode begin en periode einde: enkel bij aangepaste periode.
  ## Ingegeven = FALSE (default). Paramter voor AdaptTijdreeks().

  
  # Adapt time-series to correct format and data.
  tijdreeks_ <- tijdreeks %>% 
    AdaptTijdreeks(., ingegeven = ingegeven) %>% 
    add_column(RP = NA)
  
  tijdreeks_rp <- data.frame()
  # Maak compatibel voor broedseizoen en hyrologisch jaar.
  if (!aangepaste_periode) {
    periode_begin <- "01-04"  #  Voor hydrojaar dient dit 01-04 te zijn
    periode_einde <- "31-03" # Voor hydrojaar dient dit 31-03 te zijn.
    jaren <- unique(tijdreeks_$HydrologischJaar)
  } else {
    jaren <- unique(tijdreeks_$Jaar)
  }
  
  for (jaar in jaren) {
    # Filter time-series to correct time-scale.
    if (aangepaste_periode) {
      tijdreeks_jaar <- subset(tijdreeks_, Jaar == jaar) %>% 
        arrange(PeilmetingDatum) %>% 
        filter(PeilmetingDatum %within% 
                 interval(as_date(str_c(periode_begin, "-", jaar), format = "%d-%m-%Y"),
                          as_date(str_c(periode_einde, "-", jaar), format = "%d-%m-%Y")))
    } else {
      tijdreeks_jaar <- subset(tijdreeks_, HydrologischJaar == jaar) %>% 
        arrange(PeilmetingDatum)
    }
    
    
    # eerste periode
    for (i in seq_len(nrow(tijdreeks_jaar))) {
      # Starts counting from starting date ! 
      if (i == 1) {
        P1 <- as_date(str_c(periode_begin,"-", jaar), format = "%d-%m-%Y")
        DP2 <- tijdreeks_jaar$PeilmetingDatum[2] - tijdreeks_jaar$PeilmetingDatum[1]
        P2 <- tijdreeks_jaar$PeilmetingDatum[1] + DP2/2
          
        # Count to last date for last observation
        } else if (i == nrow(tijdreeks_jaar)) {
          DP1 <-  tijdreeks_jaar$PeilmetingDatum[i] - tijdreeks_jaar$PeilmetingDatum[i - 1]
          P1 <- tijdreeks_jaar$PeilmetingDatum[i] - DP1/2
          
          if (!aangepaste_periode) {
            P2 <- as_date(str_c(periode_einde, "-",jaar + 1), format = "%d-%m-%Y")
          } else {
            P2 <- as_date(str_c(periode_einde, "-" , jaar), format = "%d-%m-%Y")
          }
          
        # All other observations
        } else {
          DP1 <- tijdreeks_jaar$PeilmetingDatum[i] - tijdreeks_jaar$PeilmetingDatum[i - 1]
          P1 <- tijdreeks_jaar$PeilmetingDatum[i] - DP1/2
          
          DP2 <- tijdreeks_jaar$PeilmetingDatum[i + 1] - tijdreeks_jaar$PeilmetingDatum[i]
          P2 <- tijdreeks_jaar$PeilmetingDatum[i] + DP2/2
        }
        
        RP <- as.numeric(P2) - as.numeric(P1)
        if (!is.na(RP) & RP == 0) {RP <- 1}  # Indien er slechts 1 datum is, dan kan de volgende datum niet berekend worden
        tijdreeks_jaar[i, "RP"] <- RP
        }

    
    tijdreeks_rp <- bind_rows(tijdreeks_rp, tijdreeks_jaar)
  }
  
  if (nrow(tijdreeks_) == 0) {
    return(tijdreeks_rp)
  } else {
    return(tijdreeks_rp %>% 
             mutate(RP = as.numeric(RP)))
  }
}

# Gewogen gemiddelde
GewogenGemiddelde <- function(subset_tijdreeks, jaar, aangepaste_periode, periode_begin = NULL, periode_einde = NULL, ingegeven = FALSE) {
  
  # The weighted mean is the mean combined with the RP. it is calculated as follows: sum(value*RP) / sum(RP)
  # This is realyy meant to work under the functiosn Statistieken since it does not control the quality of the data.
  
  # Indien subset 0 is.
  if (nrow(subset_tijdreeks) == 0) {
    
    if(aangepaste_periode) {
      return(data.frame(Jaar = jaar,
                        GG_gewogen = NA))
    } else {
      return(data.frame(HydrologischJaar = jaar,
                        GG_gewogen = NA))
    }
    return(subset_tijdreeks)
  }
  
  # Krijg RP
  tijdreeks_ <- subset_tijdreeks %>%
    GetRP(aangepaste_periode, periode_begin, periode_einde, ingegeven)
  
  if (aangepaste_periode) {
    GG_g <- tijdreeks_ %>%
      mutate(X = PeilmetingTAW * as.numeric(RP)) %>%
      group_by(Jaar) %>%
      summarise(X = sum(X, na.rm = TRUE),
                Y = sum(as.numeric(RP)),
                .groups = "drop") %>%
      mutate(GG = X/Y) %>% 
      rename(GG_gewogen = GG) %>% 
      dplyr::select(Jaar, GG_gewogen)
  } else {
    GG_g <- tijdreeks_ %>%
      mutate(X = PeilmetingTAW * as.numeric(RP)) %>%
      group_by(HydrologischJaar) %>%
      summarise(X = sum(X, na.rm = TRUE),
                Y = sum(as.numeric(RP)),
                .groups = "drop") %>%
      mutate(GG = X/Y) %>%
      rename(GG_gewogen = GG) %>% 
      dplyr::select(HydrologischJaar, GG_gewogen)
  }
  return(GG_g)
}


# VerwijderTrailingJAren

VerwijderLagLeading <- function(tabel) {
  # Bij het verkrijgen van een tabel met statistieken kan het zijn dat er voor sommige (hydro)jaren er geen statistiek berekend kon worden.
  # Deze functie verwijdert al de trailing jaren (vanvoor als vanachter) zodat enkel detabel overblijft beginnend en eindigend met een statistiek.
  # Het verwijdert niet de NA die ertussen voorkomt.
  
  # Verwijder alle beginnende en eindigende jaren met NA's
  while (is.na(tabel[1, "Gemiddelde"]) & nrow(tabel) != 0) {
    tabel <- tabel %>% filter(!row_number() == 1)
  }
  if (nrow(tabel) != 0) {
    while (is.na(tabel[nrow(tabel), "Gemiddelde"]) & nrow(tabel) != 0) {
      tabel <- tabel %>% filter(!row_number() == nrow(tabel))
    }
  }

  return(tabel)
}



# Controle-functies------ 

## GetMissingTijdreeks
GetMissingTijdreeks <- function(type, gebieden = c("AHR", "WAH")) {
  # De meetpunten of peilpunten die een lege tijdreeks hebben.
  #  type: Filter op Meetpunt of Peilpunt.
  # Returns een dataframe van de meetpunten of peilpunten zonder tijdreeks.
  missing.df <- data.frame()
  if (type == "Meetpunt") {
    watina_havens_df <- GetWatinaMeetpunt(watina, gebieden) %>% 
      filter(MeetpuntStatus  == "Gevalideerd")
    
    for (loper in unique(watina_havens_df$MeetpuntCode)) {
      check.df <- GetWatinaTijdreeks(watina, loper)
      if (nrow(check.df) == 0) {
        add <- watina_havens_df %>% filter(MeetpuntCode == loper)
        missing.df <- bind_rows(missing.df, add)
      }
    }
  } else if (type == "Peilpunt") {
    
    watina_havens_df <- GetWatinaPeilpunten(watina, gebieden) %>% 
      filter(PeilpuntStatus  == "Gevalideerd") %>% 
      # onbekend -> peilschaal
      mutate(PeilpuntType = case_when(
        PeilpuntType == "onbekend" ~ "peilschaal", 
        TRUE ~ PeilpuntType))
    for (loper in unique(watina_havens_df$PeilpuntCode)) {
      check.df <- GetWatinaTijdreeks(watina, loper)
      if (nrow(check.df) == 0) {
        add <- watina_havens_df %>% filter(PeilpuntCode == loper)
        missing.df <- bind_rows(missing.df, add)
      }
    }
  } else {
    print(str_c("Geen geldige punt-type ingevoerd. Voer 'Meetpunt' of 'Peilpunt' toe."))
  }
  return(missing.df)
}


## ControleKwantiteit
ControleKwantiteit <- function(subset_tijdreeks, methode, percent_decimal) {  
  # Controle of er voldoende metingen aanwezig zijn om de GH3 of GL3 te berekenen
  # Het percentage van aanwezige data wordt meegegeven als decimaal getal bij de 
  # parameter "percent_decimal".
  # Argumenten:
  #   subset: subset van de tijdreeks voor het hydrologisch jaar, aangemaakt in de functie CalculateGH3/GL3.
  #   methode: GH3 voor de wintermaanden van 1 januari t.e.m. 31 maart. GL3 voor de zomermaanden van 15 juli tot en met 30 november.
  #   percent_decimal: percentage van de metingen dat aantwzzig moet zijn om een betrouwbaar gemiddelde te berekenen.
  # Returns: Een booleaanse expressie dat duid of de voorwaarden voor dat hydrologisch jaar voldaan zijn.
  
  ## Bepaal parameters om controle uit te voeren.
  jaar <- subset_tijdreeks %>% 
    distinct(HydrologischJaar) %>% 
    pull()
  
  if (methode == "GH3") {
    subset_controle <- subset_tijdreeks %>% 
      # Jaar + 1 want er wordt gerekend met hydrologisch jaar!
      filter(PeilmetingDatum %within% interval(as_date(str_c(as.numeric(jaar) + 1, "-01-01"), format = "%Y-%m-%d"),
                                               as_date(str_c(as.numeric(jaar) + 1, "-03-31"), format = "%Y-%m-%d")))
    
    jaar_controle <- as.character(as.numeric(jaar) + 1)
    periode <- "wintermaanden"
    n_hand <- 6  # 3 maanden * 2
    n_sonde <- if_else(as.numeric(jaar_controle) %% 4 == 0, 91, 90)  # Schrikeljaar of niet
    
  } else if (methode == "GL3") {
    subset_controle <- subset_tijdreeks %>% 
      filter(PeilmetingDatum %within% interval(as_date(str_c(as.numeric(jaar), "-07-15"), format = "%Y-%m-%d"),
                                               as_date(str_c(as.numeric(jaar), "-11-30"), format = "%Y-%m-%d")))
    jaar_controle <- jaar
    periode <- "zomermaanden"
    n_hand <- 9  # 4.5 maanden * 2
    n_sonde <- interval(as_date(str_c(jaar, "-07-15"), format = "%Y- %m-%d"), as_date(str_c(jaar, "-11-30"), format = "%Y- %m-%d")) / days(1)
    
  } else {
    print("Geen geldige periode ingegeven.")
  }
  
  ## Voer controle uit.
  if (nrow(subset_controle) != 0) {
    if (length(unique(subset_controle$PeilmetingType)) == 1) {
      
      if (unique(subset_controle$PeilmetingType) == "Handmatige meting") {
        if (nrow(subset_controle) < (n_hand * percent_decimal)) {
          controle = FALSE
          label = str_c("#<", percent_decimal*100, "%")
        } else {
          controle = TRUE
          label = ""
        }
        
      } else if (unique(subset_controle$PeilmetingType) == "Sondemeting") {
        if (nrow(subset_tijdreeks) < (n_sonde * percent_decimal)) {
          controle = FALSE
          label = str_c("#<", percent_decimal*100, "%")
        } else {
          controle = TRUE
          label = ""
        }
      }
    } else {  # Zowel handmetingen als sondemetingen
      # Subsets controleren handmatige metingen en sondemetingen    
      subset_controle_hand <- subset_controle %>% 
        filter(PeilmetingType == "Handmatige meting") %>% 
        arrange(PeilmetingDateTime)
      
      subset_controle_sonde <- subset_controle %>% 
        filter(PeilmetingType == "Sondemeting") %>% 
        arrange(PeilmetingDateTime)
      
      # Eerste handmeting
      eerste_handmeting <- subset_controle_hand %>% 
        slice_head(n = 1)
      
      eerste_sondemeting_1 <- subset_controle_sonde %>% 
        slice_head(n = 1)
      
      # Laatste sondemeting VOOR eerste handmeting
      laatste_sondemeting_1 <-  subset_controle_sonde %>%
        filter(PeilmetingDateTime <= eerste_handmeting$PeilmetingDateTime) %>% 
        arrange(desc(PeilmetingDateTime)) %>% 
        slice_head(n = 1)
      
      # Eerste sondemeting NA eerste handmeting
      eerste_sondemeting_2 <- subset_controle_sonde %>%
        filter(PeilmetingDateTime > eerste_handmeting$PeilmetingDateTime) %>% 
        arrange(PeilmetingDateTime) %>% 
        slice_head(n = 1)
      
      # Indien handmeting niet tussen sondemeting valt. Dus eerst handmatige metingen en dan overgestapt naar sondemetingen.
      # periode handmatige metingen XXXXXX
      if (eerste_sondemeting_1$PeilmetingDatum  == eerste_sondemeting_2$PeilmetingDatum) {
        if (methode == "GH3") {
          periode_hand <- eerste_sondemeting_1$PeilmetingDatum - as_date(str_c("01-01-", jaar_controle), format = "%m-%d-%Y")
        } else {  # GL3
          periode_hand <- eerste_sondemeting_1$PeilmetingDatum - as_date(str_c("07-15-", jaar_controle), format = "%m-%d-%Y")
        }
      } else {  # Tussen periode van sondemetingen ook handmatige metingen.
        periode_hand <- eerste_sondemeting_2$PeilmetingDatum - laatste_sondemeting_1$PeilmetingDatum
      }
      
      # Voor handmatige combinatie, versoepel voorwaarde. Aantal observaties * 14 (2 weken) moet gelijk of 
      # groter zijn als het tijdsverschil dat dichts bij lager veelvoud van 14 ligt;
      # bV 1 meting en 21 dagen verschil ? 1 * 14 >= 14 -> TRUE ; 
      # 1 meting en 28 dagen verschil? 1 * 14 >= 28 -> FALSE
      periode_hand <- as.numeric(periode_hand)
      if (periode_hand %% 14 <= 7) {
        afgerond_veelvoud <- floor(periode_hand/14) * 14
      } else {
        afgerond_veelvoud <- ceiling(periode_hand/14) * 14
      }
      
      controle_hand <- nrow(subset_controle_hand) * 14  >= afgerond_veelvoud
      controle_sonde <- nrow(subset_controle_sonde) >= (n_sonde - periode_hand) * percent_decimal
      
      if (controle_hand & controle_sonde) {
        controle = TRUE
        label = ""
      } else {
        controle = FALSE
        label = str_c("#<", percent_decimal*100, "%")
      }
    } 
  } else {
    # print(str_c("Geen metingen voor ", subset %>% distinct(MeetpuntCode) %>% pull() ,
    #             " gedurende het hydrologisch jaar ", jaar, " tijdens de ", periode, "."))
    controle  = F
    label = "X"
  }
  return(list(Controle = controle, Label = label))
}

## RepresentatievePeriode
ReprensentatievePeriode <- function(subset_tijdreeks, methode, RP_dagen) {
  # Berekend of een subset voor een hydrologisch jaar voldoet aan de representatieve periode.
  # Returns: booleaanse expressie
  subset_tijdreeks <- subset_tijdreeks %>%  
    arrange(PeilmetingDatum)
  jaar <- subset_tijdreeks %>% distinct(HydrologischJaar) %>%  pull()
  RP <- TRUE
  i <- 1
  RP_dagen <- days(RP_dagen)
  
  if (methode == "GH3") {
    subset_controle <- subset_tijdreeks %>% 
      # Jaar + 1 want er wordt gerekend met hydrologisch jaar!
      filter(PeilmetingDatum %within% interval(as_date(str_c(as.numeric(jaar) + 1, "-01-01"), format = "%Y-%m-%d"),
                                               as_date(str_c(as.numeric(jaar) + 1, "-03-31"), format = "%Y-%m-%d"))) %>% 
      
      arrange(PeilmetingDatum)
  } else if (methode == "GL3") {
    subset_controle <- subset_tijdreeks %>% 
      filter(PeilmetingDatum %within% interval(as_date(str_c(as.numeric(jaar), "-07-15"), format = "%Y-%m-%d"),
                                               as_date(str_c(as.numeric(jaar), "-11-30"), format = "%Y-%m-%d"))) %>% 
      arrange(PeilmetingDatum)
  }
  
  while (RP & i != nrow(subset_controle) +1) {
    # Eerste periode
    if (i == 1) {
      if (methode == "GH3") {
        P1 <- as_date(str_c(jaar +1, "-01-01"), format = "%Y-%m-%d")
      } else {
        P1 <- as_date(str_c(jaar, "-07-15"), format = "%Y-%m-%d")
      }
      DP2 <- subset_controle$PeilmetingDatum[2] - subset_controle$PeilmetingDatum[1]
      P2 <- subset_controle$PeilmetingDatum[1] + DP2/2
      
      # Laatste periode
    } else if (i == nrow(subset_controle)) {
      
      DP1 <-  subset_controle$PeilmetingDatum[i] - subset_controle$PeilmetingDatum[i - 1]
      P1 <- subset_controle$PeilmetingDatum[i] - DP1/2
      
      if (methode == "GH3") {
        P2 <- as_date(str_c(jaar +1, "-03-31"), format = "%Y-%m-%d")
      } else {
        P2 <- as_date(str_c(jaar, "-11-30"), format = "%Y-%m-%d")
      }
      
      # Niet eerste of laatste meting voor een periode.
    } else {
      DP1 <- subset_controle$PeilmetingDatum[i] - subset_controle$PeilmetingDatum[i - 1]
      P1 <- subset_controle$PeilmetingDatum[i] - DP1/2
      
      DP2 <- subset_controle$PeilmetingDatum[i + 1] - subset_controle$PeilmetingDatum[i]
      P2 <- subset_controle$PeilmetingDatum[i] + DP2/2
    }
    
    DP <- P2 - P1
    RP <- DP <= RP_dagen
    i <- i + 1
  } 
  # Missende jaren
  if (is.na(RP)) {RP <- FALSE}
  if (RP) { label <- ""} else {label <- "RP"}
  
  return(list(Controle = RP, Label = label))
}


## ControlePeriode
ControlePeriode <- function(subset_tijdreeks, 
                            periode_begin, 
                            periode_einde, 
                            percent_decimal, 
                            aangepaste_periode = aangepaste_periode,
                            RP_dagen = NULL, ingegeven = FALSE) {
  #  Controlleer of er voldoende metingen zijn binnen een ingegeven periode.
  
  tijdreeks_ <- subset_tijdreeks %>% 
    GetRP(., aangepaste_periode = aangepaste_periode, periode_begin = periode_begin, periode_einde = periode_einde, ingegeven = ingegeven)
  
  if (nrow(tijdreeks_) < 2) {
    return(list(Controle = FALSE, Label = str_c("<", percent_decimal*100,"% & RP")))
  }
  
  jaar <- unique(tijdreeks_$Jaar)
  
  # Aantal dagen tussen eerste en laatste meting.
  periode <- (as_date(str_c(periode_einde, "-", jaar), format = "%d- %m-%Y") - 
                as_date(str_c(periode_begin, "-", jaar), format = "%d- %m-%Y")) %>% 
    as.numeric()
  
  #  KWANTITEIT
  if (length(unique(tijdreeks_$PeilmetingType)) == 1) {
    
    if (unique(tijdreeks_$PeilmetingType) == "Handmatige meting") {
      # 1 om de 14 dagen
      n_hand <-  periode/ 14
      n_controle <- nrow(tijdreeks_) >=  (percent_decimal * n_hand)
    } else {
      n_sonde <- periode
      n_controle <- nrow(tijdreeks_) >=  (percent_decimal * n_sonde)
    }
  } else {
    # Subsets controleren handmatige metingen en sondemetingen    
    subset_controle_hand <- tijdreeks_ %>% 
      filter(PeilmetingType == "Handmatige meting") %>% 
      arrange(PeilmetingDateTime)
    
    subset_controle_sonde <- tijdreeks_ %>% 
      filter(PeilmetingType == "Sondemeting") %>% 
      arrange(PeilmetingDateTime)
    
    # Eerste handmeting
    eerste_handmeting <- subset_controle_hand %>% 
      dplyr::slice_head(., n = 1)
    
    eerste_sondemeting_1 <- subset_controle_sonde %>% 
      dplyr::slice_head(., n = 1)
    
    # Laatste sondemeting VOOR eerste handmeting
    laatste_sondemeting_1 <-  subset_controle_sonde %>%
      dplyr::filter(PeilmetingDateTime <= eerste_handmeting$PeilmetingDateTime) %>% 
      arrange(desc(PeilmetingDateTime)) %>% 
      dplyr::slice_head(., n = 1)
    
    # Eerste sondemeting NA eerste handmeting
    eerste_sondemeting_2 <- subset_controle_sonde %>%
      dplyr::filter(PeilmetingDateTime > eerste_handmeting$PeilmetingDateTime) %>% 
      arrange(PeilmetingDateTime) %>% 
      dplyr::slice_head(., n = 1)
    
    # Indien handmeting niet tussen sondemeting valt. Dus eerst handmatige metingen en dan overgestapt naar sondemetingen.
    # periode handmatige metingen 
    if (eerste_sondemeting_1$PeilmetingDatum  == eerste_sondemeting_2$PeilmetingDatum) {
      periode_hand <- eerste_sondemeting_1$PeilmetingDatum - as_date(str_c(periode_begin, "-",jaar), format = "%d-%m-%Y")
    } else {  # Tussen periode van sondemetingen ook handmatige metingen.
      periode_hand <- eerste_sondemeting_2$PeilmetingDatum - laatste_sondemeting_1$PeilmetingDatum
    }
    
    periode_sonde <- periode - periode_hand
    
    # Voor handmatige combinatie, versoepel voorwaarde. Aantal observaties * 14 (2 weken) moet gelijk of 
    # groter zijn als het tijdsverschil dat dichts bij lager veelvoud van 14 ligt;
    # bV 1 meting en 22 dagen verschil ? 1 * 14 >= 14 -> TRUE ; 
    # 1 meting en 28 dagen verschil? 1 * 14 >= 28 -> FALSE
    controle_hand <- nrow(subset_controle_hand) * 14  >= periode_hand - (as.numeric(periode_hand) %% 14) 
    controle_sonde <- nrow(subset_controle_sonde) >= (periode_sonde) * percent_decimal
    
    if (controle_hand & controle_sonde) {n_controle = TRUE} else {n_controle = FALSE}
  }
  
  # RP
  if (any(tijdreeks_$RP >= RP_dagen)) {RP_controle <- FALSE} else {RP_controle <- TRUE}
  
  # LABEL 
  controle <- FALSE
  if (n_controle & RP_controle) {
    label = ""
    controle <- TRUE
  } else if (!n_controle & !RP_controle) {
    label = str_c("<", percent_decimal*100,"% & RP")
  } else if (!n_controle) {
    label = str_c("<", percent_decimal*100,"%")
  } else {
    label = "RP"
  }
  
  return(list(Controle = controle, Label = label))
}

## Controle Hydrologisch jaar
ControleHydro <- function(subset_tijdreeks, min_aantal_meting = 20, RP_dagen = 30, ingegeven = FALSE) {

  tijdreeks_ <- subset_tijdreeks %>% 
    GetRP(., aangepaste_periode = FALSE, ingegeven = ingegeven)
  
  n_controle <- nrow(tijdreeks_) >= min_aantal_meting
  RP_controle <- !any(tijdreeks_$RP > RP_dagen)
  
  controle <- FALSE
  if (n_controle & RP_controle) {
    label = ""
    controle <- TRUE
  } else if (!n_controle & !RP_controle) {
    label = str_c("<", min_aantal_meting," & RP")
  } else if (!n_controle) {
    label = str_c("<", min_aantal_meting)
  } else {
    label = "RP"
  }
  return(list(Controle = controle, Label = label))
}

ControleTijdreeks <- function(tijdreeks, 
                              percent_decimal = 0.8,
                              RP_dagen = 30, 
                              min_aantal_metingen = 20,  # Hydrologisch jaar
                              aangepaste_periode = FALSE, 
                              periode_begin, periode_einde,
                              ingegeven = FALSE) {
  # Behoud enkel de (hydro)jaren van de tijdreeks die voldaan zijn aan de controle-voorwaarden.
  
  tijdreeks_ <- tijdreeks %>% 
    AdaptTijdreeks(., ingegeven = ingegeven)
  
  tijdreeks_controle <- data.frame()
  
  if (aangepaste_periode) {  # Aangepaste periode
    tijdreeks_ <- tijdreeks_ %>% 
      group_by(Jaar) %>% 
      filter(.data$PeilmetingDatum %within% interval(str_c(periode_begin, "-",year(.data$PeilmetingDatum)) %>%  dmy(),
                                                     str_c(periode_einde, "-",year(.data$PeilmetingDatum)) %>%  dmy())) %>% 
      ungroup()
    
    for (jaar in unique(tijdreeks_$Jaar)) {
      
      tijdreeks_jaar <- tijdreeks_ %>%
        filter(.data$Jaar == jaar)
      
      Controle <- ControlePeriode(tijdreeks_jaar, periode_begin = periode_begin, periode_einde = periode_einde,
                                  percent_decimal = percent_decimal,
                                  RP_dagen = RP_dagen, 
                                  aangepaste_periode = aangepaste_periode, ingegeven = ingegeven)
      
      if (Controle$Controle) {
        tijdreeks_controle <- bind_rows(tijdreeks_controle, tijdreeks_jaar)}
    }
  } else {  # Hydrojaar
    
    for (jaar in unique(tijdreeks_$HydrologischJaar)) {
      
      tijdreeks_jaar <- tijdreeks_ %>%
        filter(.data$HydrologischJaar == jaar)
      
      Controle <- ControleHydro(tijdreeks_jaar, min_aantal_meting = min_aantal_metingen,
                                RP_dagen = RP_dagen, 
                                ingegeven = ingegeven)
      if (Controle$Controle) {
        tijdreeks_controle <- bind_rows(tijdreeks_controle, tijdreeks_jaar)}
    }
  }
  return(tijdreeks_controle)
}  

# Uitvoerende functies -------------

## GH3
CalculateGH3 <- function(tijdreeks, tijdspanne, ingegeven = FALSE, percent_decimal, RP_dagen) {
  # Bereken de 3 hoogste maxima per hydrologisch jaar waarbij er een bepaalde minimale tijdpanne tussen deze metingen dient te zitten.
  # Deze functie houdt rekening dat meerdere observaties hetzelfde maxima kan hebben. Indien dit het geval is, dan wordt
  # het maximum behouden op basis van het grootste daaropvolgend maximum.
  # Argumenten
  # tijdreeks: dataframe van de tijdreeks voor elk peilpunt
  # tijdspanne: het aantal dagen dat er minstens tussen het aantal maxima mag liggen.
  # Ingegegeven: boolean die bij default op FALSE staat om naast gevalideerde gegevens ook ingegeven gegevens gebruikt.
  # percent_decimal = hoeveel procent (in decimaal) van de stalen minstens aanwezig dienen te zijn gedurende de winterperiode.
  # Returns: een list met 4 dataframes, namelijk "GHG": gemiddelde van de 3 hoogste maxima.GHG_ID : de gebruikte observaties om dit te berekenen.
  # en een "overzicht" waarin per jaar de combinaties staan van verschillende 'hoogste metingen". De tijdreeks is de gefilterede tijdreeks met
  # hydrologisch jaar, nodig om de correctie van de GH3 toe te passen voor manuele metingen.
  
  GH3 <- data.frame()
  overzicht <- data.frame()
  methode <- "GH3"
  
  dd <- ddays(tijdspanne - 1)  # Aantal dagen (in seconden weergegeven).
  tijdreeks <- tijdreeks %>% 
    AdaptTijdreeks(ingegeven)
  
  # (1) Maak voor elk hydrologisch jaar alle mogelijke combinaties aan indien er meerdere gelijke maxima's zijn.
  for (jaar in unique(tijdreeks$HydrologischJaar)) {
    
    teller <- 1  # Voor alle mogelijke combinaties uit elkaar te houden. Elke combinatie krijgt een afzonderlijke ID toegekend.
    
    # Creëer subset.
    subset <- subset(tijdreeks, HydrologischJaar == jaar)
    
    # Controleer of percent_decimal voldoende is.
    controle <- ControleKwantiteit(subset, methode, percent_decimal)
    RP <- ReprensentatievePeriode(subset, methode, RP_dagen)
    
    if (controle$Controle & RP$Controle) {
      max1 <- subset %>% slice_max(order_by = PeilmetingTAW, with_ties = TRUE)
      
      if (nrow(max1) != 0) {
        for (i in 1:nrow(max1)) {
          
          max1.final <- max1[i, ] %>%  mutate(ID = teller)
          subset2 <- subset %>% filter(!PeilmetingDatum %within% interval(max1[i, ]$PeilmetingDatum - dd, max1[i, ]$PeilmetingDatum + dd))
          max2 <- subset2 %>% slice_max(order_by = PeilmetingTAW, with_ties = TRUE)
          
          if (nrow(max2) != 0) {
            for (j in 1:nrow(max2)) {
              
              if (j != 1) {  # Meer als 2 2de max voor dezelfde 1e maximum dus 1e max dient nieuwe ID te krijgen.
                max1.final <- max1[i, ] %>%  mutate(ID = teller)
              }
              max2.final <- max2[j, ] %>%  mutate(ID = teller)
              subset3 <- subset2 %>% filter(!PeilmetingDatum %within% interval(max2[j, ]$PeilmetingDatum - dd, max2[j, ]$PeilmetingDatum + dd))
              max3 <- subset3 %>% slice_max(order_by = PeilmetingTAW, with_ties = TRUE)
              
              if (nrow(max3) != 0) {
                for (k in 1:nrow(max3)) {
                  
                  if (k != 1) {  # Meer als twee 3de maxen voor dezelfde 2e maximum.
                    max1.final <- max1[i, ] %>%  mutate(ID = teller)
                    max2.final <- max2[j, ] %>%  mutate(ID = teller)
                  }
                  
                  max3.final <- max3[k, ] %>%  mutate(ID = teller)
                  
                  add_overzicht <- bind_rows(max1.final, max2.final, max3.final) %>% 
                    mutate(Label = controle$Label)
                  overzicht <- bind_rows(overzicht, add_overzicht)
                  teller <- teller + 1
                }
              } else {
                add_overzicht <- bind_rows(max1.final, max2.final)
                overzicht <- bind_rows(overzicht, add_overzicht)
                teller <- teller + 1
              }
            }
          } else {  # Geen data dus max2 en max3 
            add_overzicht <- max1.final
            overzicht <- bind_rows(overzicht, add_overzicht)
            teller <- teller + 1
          }
        }
      } 
    } else {
      print(str_c("Niet voldoende metingen voor de periode 01/01/",
                  as.numeric(jaar) + 1, " - 31/03/", as.numeric(jaar) + 1,
                  " om de GH3 correct in te schatten voor het hydrologisch jaar ", jaar, "."))
      max1.final <- tijdreeks[1, ]
      max1.final[1, 3:ncol(max1.final)] <- NA
      max1.final <- max1.final %>% mutate(HydrologischJaar = jaar, ID = teller)
      
      # Geef correcte label waarom niet meegenomen, ofwel RP of kwantiteit
      controle_label <- case_when(
        !controle$Controle & RP$Controle ~ controle$Label,
        !RP$Controle & controle$Controle ~ RP$Label,
        !controle$Controle & !controle$Controle ~ 'RP&80%',
        TRUE ~ ""
      )
      
      add_overzicht <- max1.final %>% 
        mutate(Label = controle_label)
      
      overzicht <- bind_rows(overzicht, add_overzicht) %>% 
        arrange(HydrologischJaar, ID, PeilmetingDateTime)
    }
  }
  
  # (2) Kies de ID met hoogste waardes
  GH3_ID <- data.frame()
  
  if (nrow(overzicht) != 0) {
    
    for (jaar in unique(overzicht$HydrologischJaar)) {
      subset <- overzicht %>% filter(HydrologischJaar == jaar)
      for (id in unique(subset$ID)) {
        
        if (id == 1) {
          subsetA <- subset %>% filter(ID == id)
          
        } else {
          subsetB <- subset %>% filter(ID == id)
          
          # Subset B heeft een 2de meting extra.
          if (is.na(subsetA[2, "PeilmetingTAW"]) & !is.na(subsetB[2, "PeilmetingTAW"])) {
            subsetA <- subsetB
            # Beide hebben een tweede meting.
          } else if (!is.na(subsetA[2, "PeilmetingTAW"]) & !is.na(subsetB[2, "PeilmetingTAW"])) {
            
            if (subsetA[2, "PeilmetingTAW"] < subsetB[2, "PeilmetingTAW"]) {
              subsetA <- subsetB
              
              # Gelijke tweede maxima
            } else if (subsetA[2, "PeilmetingTAW"] == subsetB[2, "PeilmetingTAW"]) {
              
              # Subset B heeft 3de een meting extra
              if (is.na(subsetA[3, "PeilmetingTAW"]) & !is.na(subsetB[3, "PeilmetingTAW"])) {
                subsetA <- subsetB
                
                # Beide hebben een derde meting
              } else if (!is.na(subsetA[3, "PeilmetingTAW"]) & !is.na(subsetB[3, "PeilmetingTAW"])) {
                if (subsetA[3, "PeilmetingTAW"] < subsetB[3, "PeilmetingTAW"]) {
                  subsetA <- subsetB
                }
              } 
            }
          }
        }
      }
      GH3_ID <- bind_rows(GH3_ID, subsetA)
    }
    
    # (3) Add uitzondering handmatige metingen
    GH3_ID_correctie <- data.frame()
    for (jaar in unique(GH3_ID$HydrologischJaar)) {
      
      ID_subset <- GH3_ID %>% 
        filter(HydrologischJaar == jaar)
      
      if (nrow(ID_subset) != 3) {
        subset_tijdreeks <- subset(tijdreeks, HydrologischJaar == jaar)
        
        if (nrow(subset_tijdreeks) != 0) { # Indien jaar bestaat
          
          if (length(unique(subset_tijdreeks$PeilmetingType)) == 1) {  # Indien slechts 1 type van meting
            if (unique(subset_tijdreeks$PeilmetingType) == "Handmatige meting") {  # Indien dit type een handmatige emting is.
              controle <- ControleKwantiteit(subset_tijdreeks, methode = 'GH3', percent_decimal)
              RP <- ReprensentatievePeriode(subset_tijdreeks,  methode = 'GH3', RP_dagen)
              if (controle$Controle & RP$Controle) {
                ID_subset <- subset_tijdreeks %>% 
                  arrange(desc(PeilmetingTAW)) %>% 
                  slice(1:3) %>% 
                  mutate(ID = 1, Label = "*") # Pak de drie hoogste
                print(str_c("Maxima voor de handmatige metingen aangepast voor het hydrologisch jaar ", jaar))
              }
            }
          }
        }
      }
      GH3_ID_correctie <- bind_rows(GH3_ID_correctie, ID_subset)
    }
    
    # Bereken het gemiddelde
    GH3 <- GH3_ID_correctie %>% 
      group_by(HydrologischJaar, Label) %>% 
      summarise(Gemiddelde = mean(PeilmetingTAW), 
                Aantal = n(), 
                .groups = "drop") %>% 
      # Corrigeer voor missende (delen van het) hydrologisch jaar
      mutate(Aantal = if_else(is.na(Gemiddelde), 0, as.double(Aantal))) %>% 
      # Indien toch ergens twee metingen
      mutate(Gemiddelde = if_else(Aantal == 3, Gemiddelde, NA_real_)) %>% 
      arrange(HydrologischJaar) %>% 
      select(HydrologischJaar, Gemiddelde, Aantal, Label) %>% 
      complete(., HydrologischJaar = min(tijdreeks$HydrologischJaar):max(tijdreeks$HydrologischJaar), fill = list(Gemiddelde = NA_real_, Aantal = 0, Label = "XXX"))
    
    
  } else {
    print("Geen data beschikbaar. Controleer of data gevalideerd is.")
  }
  
  
  return(list(GH3 = GH3, GH3_ID = GH3_ID, Algemeen = overzicht))
}


## GL3
CalculateGL3 <- function(tijdreeks, tijdspanne, ingegeven = FALSE, percent_decimal, RP_dagen) {
  
  # Bereken de 3 laagste minima per hydrologisch jaar waarbij er een bepaalde minimale tijdpanne tussen deze metingen dient te zitten.
  # Deze functie houdt rekening dat meerdere observaties hetzelfde minima kan hebben. Indien dit het geval is, dan wordt
  # het minima behouden op basis van het laagste daaropvolgende minima.
  # Argumenten
  # tijdreeks: dataframe van de tijdreeks voor elk peilpunt
  # tijdspanne: het aantal dagen dat er minstens tussen het aantal minima mag liggen.
  # Ingegegeven: boolean die bij default op FALSE staat om naast gevalideerde gegevens ook ingegeven gegevens gebruikt.
  # percent_decimal = hoeveel procent (in decimaal) van de stalen minstens aanwezig dienen te zijn gedurende de zomerperiode
  # Returns: een list met 3 dataframes, namelijk "GLG": gemiddelde van de 3 laagste minima. GLG_ID : de gebruikte observaties om dit te berekenen.
  # en een "overzicht" waarin per jaar de combinaties staan van verschillende "laagste metingen". De tijdreeks is de gefilterede tijdreeks met
  # hydrologisch jaar, nodig om de correctie van de GL3 toe te passen voor manuele metingen.
  
  GL3 <- data.frame()
  overzicht <- data.frame()
  methode <- "GL3"
  dd <- ddays(tijdspanne - 1)  # Aantal dagen (in seconden weergegeven).
  
  tijdreeks <- tijdreeks %>% 
    AdaptTijdreeks(ingegeven)
  
  # (1) Maak voor elk hydrologisch jaar alle mogelijke combinaties aan indien er meerdere gelijke minima's zijn.
  for (jaar in unique(tijdreeks$HydrologischJaar)) {
    
    teller <- 1  # elke keer als deze door een loop gaat en een overzicht creërt dient deze 1 hoger te zijn.
    
    # Creëer subset.
    subset <- subset(tijdreeks, HydrologischJaar == jaar) %>% 
      #  Als er geen water in peilbuis zit wordt de PeilmetingTAW aangepast naar NA.
      mutate(PeilmetingTAW = case_when(
        str_detect(.data$PeilmetingCategorie, "geen water") ~ NA_real_,
        TRUE ~ PeilmetingTAW
      ))
    
    # Controles
    controle <- ControleKwantiteit(subset, methode, percent_decimal)
    RP <- ReprensentatievePeriode(subset, methode, RP_dagen)
    
    # Controle bodempeil. Indien er NA is wordt er vanuitgegaan dat dit komt door Bodempeil onder waterpeil zit. 
    
    
    
    
    if (any(is.na(subset$PeilmetingTAW))) {
      bodempeil <- TRUE
    } else {bodempeil <- FALSE
    }
    
    if (controle$Controle & RP$Controle) {
      
      if (bodempeil) {
        add_overzicht <- subset %>% 
          filter(is.na(PeilmetingTAW)) %>% 
          mutate(
            ID = teller,
            Label = "Droogval")
        
        overzicht <- bind_rows(overzicht, add_overzicht)
        
      } else {
        min1 <- subset %>% slice_min(order_by = PeilmetingTAW, with_ties = TRUE)
        
        if (nrow(min1) != 0) {
          for (i in 1:nrow(min1)) {
            
            min1.final <- min1[i, ] %>% mutate(ID = teller)
            subset2 <- subset %>% filter(!PeilmetingDatum %within% interval(min1[i, ]$PeilmetingDatum - dd, min1[i, ]$PeilmetingDatum + dd))
            min2 <- subset2 %>% slice_min(order_by = PeilmetingTAW, with_ties = TRUE)
            
            if (nrow(min2) != 0) {
              for (j in 1:nrow(min2)) {
                if (j != 1) {  # Meer als 2 gelijke 2de minima voor zelfde 1e minima dus 1e minima dient ook nieuwe ID te krijgen.
                  min1.final <- min1[i, ] %>%  mutate(ID = teller)
                }
                
                min2.final <- min2[j, ] %>% mutate(ID = teller)
                subset3 <- subset2 %>% filter(!PeilmetingDatum %within% interval(min2[j, ]$PeilmetingDatum - dd, min2[j, ]$PeilmetingDatum + dd))
                min3 <- subset3 %>% slice_min(order_by = PeilmetingTAW, with_ties = TRUE)
                
                if (nrow(min3) != 0) {
                  for (k in 1:nrow(min3)) {
                    
                    if (k != 1) {  # Meer als 2 gelijke 3de minima voor dezelfde 2e minima. ID eerste en tweede minima hebben dus een gelijkaarid ID nodig als nieuwe meting.
                      min1.final <- min1[i, ] %>% mutate(ID = teller)
                      min2.final <- min2[j, ] %>% mutate(ID = teller)
                    }
                    
                    min3.final <- min3[k, ] %>% mutate(ID = teller)
                    
                    
                    add_overzicht <- bind_rows(min1.final, min2.final, min3.final) %>% 
                      mutate(Label = "")
                    
                    overzicht <- bind_rows(overzicht, add_overzicht)
                    teller <- teller + 1
                    
                  }
                } else {
                  
                  add_overzicht <- bind_rows(min1.final, min2.final) %>% 
                    mutate(Label = "")
                  overzicht <- bind_rows(overzicht, add_overzicht)
                  
                  teller <- teller + 1
                }
              }
              
            } else {  # Geen data dus min2 en min3 
              
              add_overzicht <- min1.final %>%
                mutate(Label = "")
              overzicht <- bind_rows(overzicht, add_overzicht)
              
              teller <- teller + 1
            }
          }
          
        } 
      }
    } else {
      
      print(str_c("Niet voldoende of geen metingen voor de periode 15/07/",
                  jaar, "- 30/11/", jaar,
                  " om de GL3 correct in te schatten voor het hydrologisch jaar ", jaar, "."))
      min1.final <- tijdreeks[1, ]
      min1.final[1, 3:ncol(min1.final)] <- NA
      min1.final <- min1.final %>% mutate(HydrologischJaar = jaar, ID = teller)
      
      # Geef correcte label waarom niet meegenomen, ofwel RP of kwantiteit
      controle_label <- case_when(
        !controle$Controle & RP$Controle ~ controle$Label,
        !RP$Controle & controle$Controle ~ RP$Label,
        !controle$Controle & !controle$Controle ~ 'RP&80%',
        TRUE ~ "")
      
      add_overzicht <- min1.final %>% 
        mutate(Label = controle_label)
      
      overzicht <- bind_rows(overzicht, add_overzicht)
    }
  }
  
  # (2) Kies ID met 3 laagste waardes
  GL3_ID <- data.frame()
  
  if (nrow(overzicht) != 0) {
    overzicht <- overzicht %>% 
      arrange(HydrologischJaar, ID, PeilmetingDateTime)
    
    for (jaar in unique(overzicht$HydrologischJaar)) {
      subset <- overzicht %>% filter(HydrologischJaar == jaar)
      
      for (id in unique(subset$ID)) {
        if (id == 1) {
          subsetA <- subset %>% filter(ID == id)
          
        } else {
          subsetB <- subset %>% filter(ID == id)
          
          if (is.na(subsetA[2, "PeilmetingTAW"]) & !is.na(subsetB[2, "PeilmetingTAW"])) {
            subsetA <- subsetB  # Subset B heeft een meting extra.
          } else if (!is.na(subsetA[2, "PeilmetingTAW"]) & !is.na(subsetB[2, "PeilmetingTAW"])) {
            # Beide hebben een tweede meting.
            
            if (subsetA[2, "PeilmetingTAW"] > subsetB[2, "PeilmetingTAW"]) {
              subsetA <- subsetB
              
            } else if (subsetA[2, "PeilmetingTAW"] == subsetB[2, "PeilmetingTAW"]) {
              
              if (is.na(subsetA[3, "PeilmetingTAW"]) & !is.na(subsetB[3, "PeilmetingTAW"])) {
                subsetA <- subsetB  # Subset B heeft een meting extra.
                
              } else if (!is.na(subsetA[3, "PeilmetingTAW"]) & !is.na(subsetB[3, "PeilmetingTAW"])) {
                # Beide hebben een derde meting
                
                if (subsetA[3, "PeilmetingTAW"] > subsetB[3, "PeilmetingTAW"]) {
                  subsetA <- subsetB
                }
              } 
            }
          }
        }
      }
      
      GL3_ID <- bind_rows(GL3_ID, subsetA)
    }
    
    # (3) Add uitzondering handmatige metingen
    GL3_ID_correctie <- data.frame()
    for (jaar in unique(GL3_ID$HydrologischJaar)) {
      ID_subset <- GL3_ID %>%  filter(HydrologischJaar == jaar)
      
      if (any(is.na(ID_subset$PeilmetingTAW))) {bodempeil <- TRUE} else {bodempeil <- FALSE}
      
      if (!bodempeil) {
        
        if (nrow(ID_subset) != 3) {
          subset_tijdreeks <- subset(tijdreeks, HydrologischJaar == jaar)
          
          if (nrow(subset_tijdreeks) != 0) { # Indien jaar bestaat
            
            if (length(unique(subset_tijdreeks$PeilmetingType)) == 1) {
              
              if (unique(subset_tijdreeks$PeilmetingType) == "Handmatige meting") {
                controle <- ControleKwantiteit(subset_tijdreeks, methode, percent_decimal)
                RP <- ReprensentatievePeriode(subset_tijdreeks,  methode, RP_dagen)
                if (controle$Controle & RP$Controle) {
                  ID_subset <- subset_tijdreeks %>% 
                    arrange(PeilmetingTAW) %>% 
                    slice(1:3) %>% 
                    mutate(ID = 1, Label = "*") # Pak de drie laagste
                  
                  print(str_c("Minima voor de handmatige metingen aangepast voor het hydrologisch jaar ", jaar))
                }
              }
            }
          }
        }
      }
      GL3_ID_correctie <- bind_rows(GL3_ID_correctie, ID_subset)
    }
    
    
    
    # (4) Maak datapunten lager als bodempeil NA
    GL3_ID_bodempeil <- GL3_ID_correctie %>% 
      AddBodempeil(., watina_havens_peilpuntinfo) %>% 
      # Is na nodig om controle-labels te behouden.
      mutate(Label = if_else(PeilmetingTAW <= Bodempeil & !is.na(PeilmetingTAW), "Droogval", Label),
             Label = if_else(is.na(Label), "", Label))
    
    # Indien er voor een hydrologisch jaar met observatie <= bodempeil, maak voor al de observaties in dat jaar het label: < bodempeil. 
    bodempeil_jaren <- GL3_ID_bodempeil %>% 
      select(HydrologischJaar, Label) %>% 
      filter(Label == "Droogval") %>% 
      distinct() %>% 
      pull(HydrologischJaar)
    
    # Creëer tekst indien bodempeil niet berkend kan worden voor een jaar.
    Bodem_label <- GL3_ID_bodempeil %>% distinct(Bodempeil) %>% pull()
    
    if (any(is.na(Bodem_label))) {
      print(str_c("Het bodempeil kan niet berekend worden voor één of meerdere peilpuntcodes."))
    }
    
    
    # (5) Bereken het gemiddelde
    GL3 <- GL3_ID_bodempeil %>% 
      mutate(Label = if_else(HydrologischJaar %in% bodempeil_jaren, "Droogval", Label)) %>% 
      group_by(HydrologischJaar, Label) %>%
      summarise(Gemiddelde = mean(PeilmetingTAW), 
                Aantal = n(),
                .groups = "drop") %>% 
      mutate(Aantal = if_else(is.na(Gemiddelde), 0, as.double(Aantal))) %>% 
      select(HydrologischJaar, Gemiddelde, Aantal, Label) %>% 
      complete(., HydrologischJaar = min(tijdreeks$HydrologischJaar):max(tijdreeks$HydrologischJaar), 
               fill = list(Gemiddelde = NA_real_, Aantal = 0, Label = "XXX"))
    
  } else {
    print("Geen data beschikbaar. Mogelijks enkel data met PeilmetingStatus == Ingegeven")
  }
  
  return(list(GL3 = GL3, GL3_ID = GL3_ID, Algemeen = overzicht))
}

VerschilGx3 <- function(tijdreeks, tijdspanne, ingegeven = FALSE, percent_decimal, RP_dagen){
  GH3 <- tijdreeks %>% 
    CalculateGH3(., tijdspanne, ingegeven = FALSE, percent_decimal, RP_dagen)
  
  GH3_ <- GH3$GH3 %>% 
    rename(LabelGH3 = Label,
           AantalGH3 = Aantal,
           GH3 = Gemiddelde)
  
  
  GL3 <- tijdreeks %>% 
    CalculateGL3(., tijdspanne, ingegeven = FALSE, percent_decimal, RP_dagen)
  
  GL3_ <- GL3$GL3 %>% 
    rename(LabelGL3 = Label,
           AantalGL3 = Aantal,
           GL3 = Gemiddelde)
  
  Gx3 <- left_join(GH3_, GL3_, by = "HydrologischJaar") %>% 
    mutate(Gx3 = GH3 - GL3)
  
  return(Gx3)
}


## GV3
CalculateGV3 <- function(tijdreeks, tijdspanne = 12, ingegeven = FALSE) {
  # Berekend de gemiddelde grondwaterstand (op basis van 3 metingen) dichts bij 1 april, met een tijdspanne tussen 1 maart en 31 mei. 
  # Er is dus een langere periode beschikbaar dna 1 april dan ervoor (30 dagen vs 2 maand).
  # Dit houdt geen rekening met het hydrologisch jaar !
  # Watina gebruikt voor tijdspanne 12 dagen om zo voldoende meetpunten rond 1 april te hebben indien dit handmatig gemeten is.
  # Watina controleerd enkel ofdat er 3 metingen gebeurd zijn, maar houd geen rekening met de representatieve periode. 
  # Returns: lijst met gemiddelde en overzicht van de gebruikte observaties.
  
  overzicht <- data.frame()
  
  tijdreeks <- tijdreeks %>% 
    AdaptTijdreeks(., ingegeven)
  
  dd <- ddays(tijdspanne - 1)  # Aantal dagen (in seconden weergegeven).
  
  for (jaar in unique(tijdreeks$Jaar)) {
    
    teller <- 1
    
    # Creeër subset.
    subset <- subset(tijdreeks, Jaar == jaar) %>%
      ##Filter tijdreeks_jaar op correcte periode.
      filter(PeilmetingDateTime %within%
               interval(as_date(str_c(year(PeilmetingDateTime), "-03-01 00:00:00"), format = "%Y-%m-%d %H:%M:%S"),
                        as_date(str_c(year(PeilmetingDateTime), "-05-31 23:59:59"), format = "%Y-%m-%d %H:%M:%S"))) %>% 
      mutate(Verschil_tijd = abs(PeilmetingDateTime - as_datetime(str_c(year(PeilmetingDateTime), "-04-01 00:00:00"), format = "%Y-%m-%d %H:%M:%S")),
             Verschil_dagen = abs(PeilmetingDatum  - as_date(str_c(year(PeilmetingDatum ), "-04-01"), format = "%Y-%m-%d")))
    
    
    controle <- FALSE
    if (nrow(subset) >= 3) {
      controle <- TRUE
    } else {
      controle <- FALSE
      label <- "#<3"
    }
    
    if (controle)  {
      # eerste mean. Dit kan twee metingen bevatten als deze evenveel van tijd verschillen met 01-04.
      mean1 <- subset %>%  
        slice_min(order_by = Verschil_tijd, with_ties = TRUE)
      
      if (nrow(mean1) != 0) {
        
        for (i in 1:nrow(mean1)) {  # Bepaal al de mogelijkheden indien er twee metingen zijn met gelijk tijdverschil.
          
          mean1.final <- mean1[i, ] %>% 
            mutate(ID = teller)
          
          subset2 <- subset %>% 
            filter(!PeilmetingDateTime %within% interval(mean1[i, ]$PeilmetingDateTime - dd, mean1[i, ]$PeilmetingDateTime + dd))
          
          mean2 <- subset2 %>% slice_min(order_by = Verschil_tijd, with_ties = TRUE)
          
          if (nrow(mean2) != 0) {
            # Indien mean2 meerdere waarnemingen heeft betekend dit dat beide opgenomen dienen te worden gezien 
            # dit sowieso een meting x-aantal dagen voor en x-aantal dagen na de eerste meting is dicht bij 01-04 ! mean3 wordt dan sws de tweede mean2.
            mean2.final <- mean2[1, ] %>% 
              mutate(ID = teller)
            
            subset3 <- subset2 %>% 
              filter(!PeilmetingDateTime %within% interval(mean2[1, ]$PeilmetingDateTime - dd, mean2[1, ]$PeilmetingDatum + dd))
            
            mean3 <- subset3 %>% 
              slice_min(order_by = Verschil_tijd, with_ties = TRUE)  # Kan dus maar slechts max. 1 observatie bevatten.
            
            if (nrow(mean3) != 0) {
              
              mean3.final <- mean3 %>%
                arrange(desc(PeilmetingTAW)) %>% 
                mutate(ID = teller) %>% 
                slice_head(n = 1)
              
              overzicht_add <- bind_rows(mean1.final, mean2.final, mean3.final) %>% 
                mutate(Label = "")
              overzicht <- bind_rows(overzicht, overzicht_add)
              teller <- teller + 1
              
            } else {
              print(str_c("Geen derde meting voor het jaar ", jaar, " en ID ",teller))
              overzicht_add <- bind_rows(mean1.final, mean2.final)
              overzicht <- bind_rows(overzicht, overzicht_add)
              teller = teller + 1  # Pas hier teller + 1 gezien 
            }
            
          } else {
            overzicht <- bind_rows(overzicht, mean1.final)
            teller <- teller + 1
            print(str_c("Geen tweede meting voor het jaar ", jaar))
          }
        }  
      } else {
        print(str_c("Geen metingen voor het jaar ", jaar))
        
        add_missend <- tijdreeks[1, ]
        add_missend[1, 3:ncol(tijdreeks)-1] <- NA
        add_missend <- add_missend %>% 
          mutate(ID = 1,
                 Label = "X")
        
        overzicht <- bind_rows(overzicht, add_missend)
        
      }
      
    } else {
      print(str_c("Niet voldoende metingen voor het jaar ", jaar))
      add_missend <- tijdreeks[1, ]
      add_missend[1, 3:ncol(tijdreeks)-1] <- NA
      add_missend <- add_missend %>% 
        mutate(
          Jaar = jaar,
          ID = 1,
          Label = label)
      
      overzicht <- bind_rows(overzicht, add_missend)
    }
  }
  
  ## Kies ID met meeste observaties of minste tijdverschil
  GV3_ID <- data.frame()
  
  if (nrow(overzicht) != 0) {
    for (jaar in unique(overzicht$Jaar)) {
      
      subset <- overzicht %>% filter(Jaar == jaar)
      
      for (id in unique(subset$ID)) {
        if (id == 1) {
          subsetA <- subset %>% filter(ID == id)
        } else {
          subsetB <- subset %>% filter(ID == id)
          
          if (nrow(subsetA) < nrow(subsetB)) {  # Meer observaties
            subsetA <- subsetB
            
          } else if(subsetA$Verschil_tijd[2] > subsetB$Verschil_tijd[2]) {  # Subset B heeft recentere 2de observatie
            subsetA <- subsetB
          } else if (subsetA$Verschil_tijd[2] == subsetB$Verschil_tijd[2]) {  # Gelijke 2de observatie
            if (subsetA$Verschil_tijd[3] > subsetB$Verschil_tijd[3]) {  # subset B heeft recentere 3de observatie als 2de observatie gelijk is.
              subsetA <- subsetB
            }
          } 
        }
      }
      GV3_ID <- bind_rows(GV3_ID, subsetA)
    }
    
    # Bereken het gemiddelde
    GV3 <- GV3_ID %>% 
      arrange(Jaar) %>% 
      group_by(Jaar, Label) %>% 
      summarise(Gemiddelde = mean(PeilmetingTAW), 
                Aantal = n(), .groups = "drop") %>% 
      mutate(Aantal = if_else(is.na(Gemiddelde), 0, as.double(Aantal))) %>% 
      mutate(Gemiddelde = if_else(Aantal != 3, NA_real_, Gemiddelde)) %>% 
      mutate(Label = if_else(Aantal != 3 & is.na(Label), "X", Label)) %>% 
      select(Jaar, Gemiddelde, Aantal, Label) %>% 
      complete(., Jaar = min(tijdreeks$Jaar):max(tijdreeks$Jaar), fill = list(Gemiddelde = NA_real_, Aantal = 0, Label = "XXX"))
    
    
    # # Remove NA begin en eind
    # while (is.na(GV3[1, "Gemiddelde"])) {
    #   GV3 <- GV3 %>% filter(!row_number() == 1)
    # }
    # while (is.na(GV3[nrow(GV3), "Gemiddelde"])) {
    #   GV3 <- GV3 %>% filter(!row_number() == nrow(GV3))
    # }
  }
  
  return(list(GV3 = GV3, GV3_ID = GV3_ID, Overzicht = overzicht))
}



CalculateGZ3 <- function(tijdreeks, datum, tijdspanne, ingegeven = FALSE) {
  # Berekend de gemiddelde grondwaterstand (op basis van 3 metingen) dichts bij een ingegeven datum, met een max tijdspannen van 31 dagen ervoor en erna.
  # Dit houdt geen rekening met het hydrologisch jaar !
  # Watina controleerd enkel ofdat er 3 metingen gebeurd zijn, maar houd geen rekening met de representatieve periode. 
  # Returns: lijst met gemiddelde en overzicht van de gebruikte observaties.
  
  overzicht <- data.frame()
  
  tijdreeks <- tijdreeks %>% 
    AdaptTijdreeks(., ingegeven)
  
  dd <- ddays(tijdspanne - 1)  # Aantal dagen (in seconden weergegeven).
  
  datum_eng <- str_c(str_sub(datum, 4, 5), "-", str_sub(datum, 1, 2))
  
  for (jaar in unique(tijdreeks$Jaar)) {
    teller <- 1
    
    # Creeër subset.
    subset <- subset(tijdreeks, Jaar == jaar) %>% 
      #  Filter only dates within range of 31 days of the given 'center" date.
      filter(PeilmetingDateTime %within%
               interval(as_date(str_c(year(PeilmetingDateTime), "-", datum_eng, " 00:00:00"), format = "%Y-%m-%d %H:%M:%S") - 31,
                        as_date(str_c(year(PeilmetingDateTime), "-", datum_eng, "23:59:59"), format = "%Y-%m-%d %H:%M:%S") + 31)) %>% 
      # Bereken verschil in tijd in dagen en seconden. Ga uit van seconden om de 3 hoogste waarden te berekenen.
      mutate(Verschil_tijd = abs(PeilmetingDateTime - as_datetime(str_c(year(PeilmetingDateTime), "-", datum_eng, " 00:00:00"), format = "%Y-%m-%d %H:%M:%S")),
             Verschil_dagen = abs(PeilmetingDatum  - as_date(str_c(year(PeilmetingDatum), "-", datum_eng), format = "%Y-%m-%d")))
    
    
    
    controle <- FALSE
    if (nrow(subset) >= 3) {
      controle <- TRUE
    } else {
      controle <- FALSE
      label <- "#<3"
    }
    
    
    if (controle)  {
      
      # eerste mean. Dit kan twee metingen bevatten als deze evenveel van tijd verschillen met 01-04.
      mean1 <- subset %>%  
        slice_min(order_by = Verschil_tijd, with_ties = TRUE)
      
      
      if (nrow(mean1) != 0) {
        for (i in 1:nrow(mean1)) {
          
          mean1.final <- mean1[i, ] %>% 
            mutate(ID = teller)
          
          subset2 <- subset %>% 
            filter(!PeilmetingDateTime %within% interval(mean1[i, ]$PeilmetingDateTime - dd, mean1[i, ]$PeilmetingDateTime + dd))
          
          mean2 <- subset2 %>% slice_min(order_by = Verschil_tijd, with_ties = TRUE)
          
          if (nrow(mean2) != 0) {
            # Indien mean2 meerdere waarnemingen heeft betekend dit dat beide opgenomen dienen te worden gezien 
            # dit sowieso een meting x-aantal dagen voor en x-aantal dagen na de eerste meting is dicht bij 01-04 ! mean3 is dan sws de tweede mean2.
            mean2.final <- mean2[1, ] %>% 
              mutate(ID = teller)
            
            subset3 <- subset2 %>% 
              filter(!PeilmetingDateTime %within% interval(mean2[1, ]$PeilmetingDateTime - dd, mean2[1, ]$PeilmetingDatum + dd))
            
            mean3 <- subset3 %>% 
              slice_min(order_by = Verschil_tijd, with_ties = TRUE)  # Kan dus maar slechts max. 1 observatie bevatten.
            
            if (nrow(mean3) != 0) {
              mean3.final <- mean3 %>% mutate(ID = teller)
              
              overzicht_add <- bind_rows(mean1.final, mean2.final, mean3.final) %>% 
                mutate(Label = "")
              overzicht <- bind_rows(overzicht, overzicht_add)
              teller <- teller + 1
              
            } else {
              print(str_c("Geen derde meting voor het jaar ", jaar, " en ID ",teller))
              overzicht_add <- bind_rows(mean1.final, mean2.final)
              overzicht <- bind_rows(overzicht, overzicht_add)
              teller = teller + 1  # Pas hier teller + 1 gezien 
            }
            
          } else {
            overzicht <- bind_rows(overzicht, mean1.final)
            teller <- teller + 1
            print(str_c("Geen tweede meting voor het jaar ", jaar))
          }
        }  
      } else {
        print(str_c("Geen metingen voor het jaar ", jaar))
        
        add_missend <- tijdreeks[1, ]
        add_missend[1, 3:ncol(tijdreeks)-1] <- NA
        add_missend <- add_missend %>% 
          mutate(ID = 1,
                 Label = "X")
        
        overzicht <- bind_rows(overzicht, add_missend)
      }
      
    } else {
      print(str_c("Niet voldoende metingen voor het jaar ", jaar))
      add_missend <- tijdreeks[1, ]
      add_missend[1, 3:ncol(tijdreeks)-1] <- NA
      add_missend <- add_missend %>% 
        mutate(
          Jaar = jaar,
          ID = 1,
          Label = label)
      
      overzicht <- bind_rows(overzicht, add_missend)
    }
  }
  
  ## Kies ID met meeste observaties of minste tijdverschil
  GV3_ID <- data.frame()
  
  if (nrow(overzicht) != 0) {
    for (jaar in unique(overzicht$Jaar)) {
      
      subset <- overzicht %>% filter(Jaar == jaar)
      
      for (id in unique(subset$ID)) {
        if (id == 1) {
          subsetA <- subset %>% filter(ID == id)
        } else {
          subsetB <- subset %>% filter(ID == id)
          
          if (nrow(subsetA) < nrow(subsetB)) {  # Meer observaties
            subsetA <- subsetB
            
          } else if(subsetA$Verschil_tijd[2] > subsetB$Verschil_tijd[2]) {  # Subset B heeft recentere 2de observatie
            subsetA <- subsetB
          } else if (subsetA$Verschil_tijd[2] == subsetB$Verschil_tijd[2]) {  # Gelijke 2de observatie
            if (subsetA$Verschil_tijd[3] > subsetB$Verschil_tijd[3]) {  # subset B heeft recentere 3de observatie als 2de observatie gelijk is.
              subsetA <- subsetB
            }
          } 
        }
      }
      GV3_ID <- bind_rows(GV3_ID, subsetA)
    }
    
    # Bereken het gemiddelde
    GV3 <- GV3_ID %>% 
      arrange(Jaar) %>% 
      group_by(Jaar, Label) %>% 
      summarise(Gemiddelde = mean(PeilmetingTAW), 
                Aantal = n(), .groups = "drop") %>% 
      mutate(Aantal = if_else(is.na(Gemiddelde), 0, as.double(Aantal))) %>% 
      mutate(Gemiddelde = if_else(Aantal != 3, NA_real_, Gemiddelde)) %>% 
      mutate(Label = if_else(Aantal != 3 & is.na(Label), "X", Label)) %>% 
      select(Jaar, Gemiddelde, Aantal, Label) %>% 
      complete(., Jaar = min(tijdreeks$Jaar):max(tijdreeks$Jaar), fill = list(Gemiddelde = NA_real_, Aantal = 0, Label = "XXX"))
  }
  
  return(list(GV3 = GV3, GV3_ID = GV3_ID, Overzicht = overzicht))
}



## PeriodeBodempeil

PeriodeBodempeil <- function(tijdreeks, watina_havens_peilpuntinfo, ingegeven = FALSE, hydrojaar = NULL) {
  # Geeft voor een tijdreeks weer gedurende welke periode dat het water onder bodempeil zat.
  # Om het resultaat onder elkaar te krijgen dient de return nog met cat() uitgevoerd te worden.
  # Standaard berekend het de periode onder bodempeil voor alle jaren, maar er is ook de mogelijkheid om dit per hyrojaar in te voeren. 
  
  tijdreeks_bodempeil <- tijdreeks %>% 
    AdaptTijdreeks(ingegeven = ingegeven) %>% 
    AddBodempeil(., watina_havens_peilpuntinfo) %>% 
    mutate(PeilmetingCategorie  = case_when(
      # Controlleerd of er INGEVULDE waarde onder het bodempeil zit.
      !is.na(Bodempeil) & !is.na(PeilmetingTAW) & (PeilmetingTAW <= Bodempeil) ~ "< Bodempeil",
      TRUE ~ PeilmetingCategorie))
  
  
  if (!is.null(hydrojaar)) {
    
    if (!hydrojaar %in% unique(tijdreeks_bodempeil$HydrologischJaar)) { 
      print(str_c("Geen data in hydrologisch jaar ", hydrojaar, "."))
    } else {
      tijdreeks_bodempeil <- tijdreeks_bodempeil %>% 
        filter(HydrologischJaar == hydrojaar)
    }
  }
  
  begin <- TRUE
  periode <- ""
  
  if (nrow(tijdreeks_bodempeil) != 0) {
    for (i in seq_len(nrow(tijdreeks_bodempeil))) {
      
      if (tijdreeks_bodempeil[i, "PeilmetingCategorie"] %in% c("geen water in peilbuis", "< Bodempeil") & begin) {
        
        start <- tijdreeks_bodempeil[i, "PeilmetingDateTime"]
        # peilbuis_start <- tijdreeks_bodempeil[i, "PeilpuntCode"]
        einde <- tijdreeks_bodempeil[i, "PeilmetingDateTime"]
        begin <- FALSE
        
        
      } else if (tijdreeks_bodempeil[i, "PeilmetingCategorie"] %in% c("geen water in peilbuis", "< Bodempeil") & !begin) {
        
        einde <- tijdreeks_bodempeil[i, "PeilmetingDateTime"]
        # peilbuis_einde <- tijdreeks_bodempeil[i, "PeilpuntCode"]
        
      } else if (is.na(tijdreeks_bodempeil[i, "PeilmetingCategorie"]) & !begin) {
        # Droogte gedaan maar periode ervoor wel droog !
        begin <- TRUE
        
        if (start == einde) {
          add <- str_c("Op ", format(start, "%d-%m-%Y %H:%M:%S"), " was het water onder het bodempeil.")
        } else {
          add <- str_c("Tussen ", format(start, "%d-%m-%Y %H:%M:%S"), 
                       " en ", format(einde, "%d-%m-%Y %H:%M:%S"), 
                       " was het water onder het bodempeil.")
        }
        
        
        periode <- str_c(periode, "\n", add) 
        # periode <- append(periode, add)
        
      } else {
        # Geen droogte en periode ervoor ook niet.
        # Kan waarschijnlijk verwijdert worden deze else {}
      }
    }
  }
  return(periode)
}

## Statistieken

StatistiekenPeriode <- function(tijdreeks,
                                periode_begin, periode_einde, 
                                percent_decimal = 0.8, RP_dagen = 30, 
                                ingegeven = FALSE) {
  # Get the common statistics for an adapted period in a year.
  # The function itselfs controls of the quality is fine. 
  ## Parameters:
  ## tijdreeks: time series
  ## periode_begin: start of period
  ## periode_einde; end of period
  ## percent_decimal = quantity control
  ## RP_dagen: RP control
  ## ingegeven: Default FALSE. Include status "ingegeven". 
  # Returns: table with the statistics: min, max, mean, weighted mean, and amplitude (max - min)
  
  
  tijdreeks_ <- tijdreeks %>% 
    AdaptTijdreeks(ingegeven = ingegeven)
  
  tijdreeks_broedseizoen <- data.frame()
  tijdreeks_GG_gem <- data.frame()
  for (jaar in unique(tijdreeks_$Jaar)) {
    tijdreeks_periode <- tijdreeks_ %>% 
      filter(Jaar == jaar) %>% 
      filter(PeilmetingDatum %within% 
               interval(as_date(str_c(periode_begin, "-", jaar), format = "%d-%m-%Y"),
                        as_date(str_c(periode_einde, "-", jaar), format = "%d-%m-%Y")))
    
    controle <- ControlePeriode(tijdreeks_periode, 
                                periode_begin = periode_begin, periode_einde = periode_einde, 
                                aangepaste_periode = TRUE, 
                                percent_decimal = percent_decimal, RP_dagen = RP_dagen, ingegeven = ingegeven)
    
    gg_gem <- GewogenGemiddelde(tijdreeks_periode, jaar, aangepaste_periode = TRUE, 
                                periode_begin = periode_begin, periode_einde = periode_einde, 
                                ingegeven = ingegeven)
    tijdreeks_GG_gem <- bind_rows(tijdreeks_GG_gem, gg_gem)
    
    tijdreeks_periode_controle <- tijdreeks_periode %>% 
      mutate(Controle = controle$Controle,
             Label = controle$Label)
    
    tijdreeks_broedseizoen <- bind_rows(tijdreeks_broedseizoen, tijdreeks_periode_controle)
  }
  
  if (nrow(tijdreeks_broedseizoen) == 0) {return(NULL)}
  
  statistiek_ <- tijdreeks_broedseizoen %>% 
    group_by(Jaar, Label, Controle) %>% 
    summarise(., across(.cols = PeilmetingTAW,
                        .fn = list(
                          GG_rekenkundig = ~mean(.x),
                          min = ~ min(.x),
                          max = ~ max(.x, na.rm = TRUE),
                          Aantal = ~ n()),
                        .names = "{.fn}"),
              .groups = "drop") %>% 
    left_join(., tijdreeks_GG_gem, by = "Jaar") %>%
    # Maak NA indien controle FALSE
    mutate(across(.cols = c(GG_rekenkundig, GG_gewogen), ~ if_else(!Controle, NA_real_, .x))) %>% 
    mutate(GG_gewogen = if_else(is.na(GG_rekenkundig), NA_real_, GG_gewogen)) %>% 
    mutate(Label = if_else(Controle == TRUE & is.na(GG_rekenkundig), "< Bodempeil", Label)) %>% 
    mutate(Amplitude = max - min) %>% 
    dplyr::select(Jaar, GG_rekenkundig, GG_gewogen, min, max, Aantal, Label) %>% 
    complete(., Jaar = min(tijdreeks_broedseizoen$Jaar):max(tijdreeks_broedseizoen$Jaar), 
             fill = list(GG_rekenkundig = NA, GG_gewogen = NA, min = NA, max = NA, amp = NA, Aantal = 0, Label = "XXX"))
  
  return(statistiek_)
}


StatistiekenHydro <- function(tijdreeks,
                              min_aantal_meting = 20, RP_dagen = 30, 
                              ingegeven = FALSE) {
  
  # Get the common statistics for an adapted period in a year.
  # The function itselfs controls of the quality is fine. 
  ## Parameters:
  ## tijdreeks: time series
  ## min_aantal_meting = minimum of 20 observations
  ## RP_dagen: RP control
  ## ingegeven: Default FALSE. Include status "ingegeven". 
  # Returns: table with the statistics: min, max, mean, weighted mean, and amplitude (max - min)
  
  tijdreeks_ <- tijdreeks %>% 
    AdaptTijdreeks(ingegeven)
  
  tijdreeks_hydro <- data.frame()
  tijdreeks_GG_gem <- data.frame()
  for (jaar in unique(tijdreeks_$HydrologischJaar)) {
    
    tijdreeks_jaar <- tijdreeks_ %>% 
      filter(HydrologischJaar == jaar)
    
    controle <- ControleHydro(tijdreeks_jaar, min_aantal_meting, ingegeven = ingegeven)
    
    gg_gem <- GewogenGemiddelde(tijdreeks_jaar, jaar, aangepaste_periode = FALSE, ingegeven = ingegeven)
    tijdreeks_GG_gem <- bind_rows(tijdreeks_GG_gem, gg_gem)
    
    tijdreeks_periode_controle <- tijdreeks_jaar %>% 
      mutate(Controle = controle$Controle,
             Label = controle$Label)
    
    
    tijdreeks_hydro <- bind_rows(tijdreeks_hydro, tijdreeks_periode_controle)
  }
  
  if (nrow(tijdreeks_hydro) == 0) {return(NULL)}
  
  statistiek_ <- tijdreeks_hydro %>% 
    group_by(HydrologischJaar, Controle, Label) %>% 
    summarise(., across(.cols = PeilmetingTAW,
                        .fn = list(
                          GG_rekenkundig = ~mean(.x),
                          min = ~ min(.x),
                          max = ~ max(.x, na.rm = TRUE),
                          Aantal = ~ n()),
                        .names = "{.fn}"),
              .groups = "drop") %>%
    left_join(., tijdreeks_GG_gem, by = "HydrologischJaar") %>% 
    mutate(across(.cols = c(GG_rekenkundig, GG_gewogen), ~ if_else(!Controle, NA_real_, .x))) %>% 
    mutate(GG_gewogen = if_else(is.na(GG_rekenkundig), NA_real_, GG_gewogen)) %>% 
    mutate(Label = if_else(Controle == TRUE & is.na(GG_rekenkundig), "< Bodempeil", Label)) %>% 
    mutate(Amplitude = max- min) %>% 
    dplyr::select(HydrologischJaar, GG_rekenkundig, GG_gewogen, min, max, Amplitude, Aantal, Label) %>% 
    complete(., HydrologischJaar = min(tijdreeks_$HydrologischJaar):max(tijdreeks_$HydrologischJaar), 
             fill = list(GG_rekenkundig = NA, GG_gewogen= NA, min = NA, max = NA, amp = NA, Aantal  = 0)) 
  
  return(statistiek_)
}


## Spreiding

GrootsteSpreiding <- function(tijdreeks, periode_begin, periode_einde, tijdvenster,percent_decimal = 0.8, RP_dagen = 30, ingegeven = FALSE) {
  # Verkrijg de grootste spreiding in een bepaalde periode.
  # Controleerd of er voldoende datapunten zijn voor dat periode en/of de Representatieve periode klein genoeg is.
  # De tijdvenster is hoeveel dagen er tussen de metingen dienen te zitten.
  # Nieuwe label: !Tijdvenster; voldoende metingen maar  geen meting in die periode met 30 dagen ertussen.
  ##Parameters : 
  ##tijdvenster: aantal dagen tussen de datums om de grootste spreiding te berekenen
  #
  
  tijdreeks_ <- tijdreeks %>% 
    AdaptTijdreeks(ingegeven)
  
  spreiding <- data.frame()
  overzicht <- data.frame()
  dd <- ddays(tijdvenster)
  
  for (jaar in unique(tijdreeks_$Jaar)) {
    
    tijdreeks_jaar <- subset(tijdreeks_, Jaar == jaar)
    
    tijdreeks_periode <- tijdreeks_jaar %>% # Filter op broedseizoen voor dat jaar
      filter(PeilmetingDatum %within% 
             interval(as_date(str_c(periode_begin,  "-", jaar), format = "%d-%m-%Y"),
                      as_date(str_c(periode_einde,  "-", jaar), format = "%d-%m-%Y")))
    
    gegevens <- data.frame()
    delta_boolean <- FALSE # Conroleerd of delta effectief veranderd is.
    delta <- 0 # Voor eerste meting
    
    if (nrow(tijdreeks_periode) != 0) {
      controle <- ControlePeriode(tijdreeks_periode, 
                                  periode_begin = periode_begin, 
                                  periode_einde = periode_einde, 
                                  percent_decimal = percent_decimal,
                                  RP_dagen = RP_dagen, ingegeven = ingegeven)
      
      if (controle$Controle) {
        
        for (i in 1:nrow(tijdreeks_periode)) {
          n1 <- tijdreeks_periode[i, ]
          
          subset_n2 <- tijdreeks_periode %>%  
            filter(
              PeilmetingDatum == tijdreeks_periode[i, ]$PeilmetingDatum - dd |
                PeilmetingDatum == tijdreeks_periode[i, ]$PeilmetingDatum + dd) 
          
          if (nrow(subset_n2) != 0) {  # Indien er meerdere waarden zijn, bv # dagen ervoor en erna.
            
            for (j in 1:nrow(subset_n2)) {
              n2 <- subset_n2[j, ]
              
              if (n1[, "PeilmetingTAW"] >= n2[, "PeilmetingTAW"]) {
                verschil <- n1[, "PeilmetingTAW"] - n2[, "PeilmetingTAW"]
                delta_boolean <- TRUE
              } else {  # Indien een waarde negatief is.
                verschil <- n2[, "PeilmetingTAW"] - n1[, "PeilmetingTAW"]
                delta_boolean <- TRUE
              }
              
              if (verschil > delta) {
                delta <- verschil
                id = 1
                n1 <- n1 %>% mutate(ID= id)
                n2 <- n2 %>% mutate(ID= id)
                gegevens <- bind_rows(n1, n2)  # Indien nieuwe spreiding groter, wordt de oude overschreven.
                
              } else if (verschil == delta & nrow(gegevens) != 0) { # nrow(gegevens) != 0 voorkomt dat, als eerste meting toevallig ook 0 is, deze niet opgenomen wordt

                # Voorkomt dat er dubeble gegevens zijn (n1-n2 &  n2-n1)
                if(!n1$PeilmetingDateTime %in% unique(gegevens$PeilmetingDateTime) |
                   !n2$PeilmetingDateTime %in% unique(gegevens$PeilmetingDateTime)) {
                  id <- gegevens[nrow(gegevens), "ID"] + 1
                  n1 <- n1 %>% mutate(ID = id)
                  n2 <- n2 %>% mutate(ID = id)
                  gegevens <- bind_rows(gegevens, n1, n2)
 
                }

              }  # anders kleiner dus niet belangrijk.
            }
          }
        } 

        if (delta_boolean) {
          add <- data.frame(
            Jaar = jaar,
            delta = delta,
            Label = controle$Label)
          
          spreiding <- bind_rows(spreiding, add)
          overzicht <- bind_rows(overzicht,gegevens)  
        } else {  # Er is voldoende data maar er zijn geen metingen met het aantal dagen van het tijdvenster ertussen.
          add <- data.frame(
            Jaar = jaar,
            delta = NA,
            Label = "!Tijdvenster")
          
          spreiding <- bind_rows(spreiding, add)
        }
      } else {
        add <- data.frame(
          Jaar = jaar,
          delta = NA,
          Label = controle$Label)
        spreiding <- bind_rows(spreiding, add)

      }
    } else {  # Net voldoende data voor deze periode
      add <- data.frame(
        Jaar = jaar,
        delta = NA,
        Label = "XXX")
      spreiding <- bind_rows(spreiding, add)
    }
  }
  return(list(Spreiding = spreiding, Overzicht = overzicht))
}

# Data voor analyses -------------------------------------------------------------
GetPeilVerloop <- function(tijdreeks, aangepaste_periode = FALSE,  
                           periode_begin = "15-03",  
                           periode_einde = "15-07",
                           jaar = NULL, 
                           max_tijdspanne = 60, 
                           ingegeven = FALSE, 
                           label_droogval = "x") {
  # Get the time-series in correct format for visualisation;
    # If more than max_tijdspanne" between two observation, the line will be disrupted
    # Get a dot-label when the water-table is below ground-table (dry).
    # Periode for facet_wrap. This will be used to make nice plots (see GrafiekPeilverloop whereby all facets starts with the desired month! 
    # Sounds easy but it wasnt to figure it out :)
 # Returns: adapted time-series

  # Geeft dag weer niet uur !
  tijdreeks_dagen <- tijdreeks %>% 
    AdaptTijdreeks(ingegeven) %>% 
    mutate(Verschil_dag = difftime(PeilmetingDateTime, lag(PeilmetingDateTime), units = "days") %>% 
             as.numeric(.) %>% 
             if_else(is.na(.), 0, .),
           Label = if_else(PeilmetingCategorie == "geen water in peilbuis",
                           label_droogval, "")) %>% 
    as.data.frame()
  
  #Identificeer de rijen waar het verschil tussen deze en vorige meting meer is als het maximum toegelaten tijdspanne.
  index_max <-  tijdreeks_dagen %>% 
    rownames_to_column(., var = "index") %>% 
    filter(.data$Verschil_dag > max_tijdspanne) %>% 
    dplyr::select(index) %>% 
    mutate(index = as.numeric(index)) %>% 
    pull()
  
  j <- 0 # Eerste index. Index verspringt als er een rij wordt toegevoegd dus steeds een aantal toegevoegde rij optellen bij de index.
  for (index in index_max ) {
    tijdreeks_dagen <- tijdreeks_dagen %>% 
      add_row(MeetpuntCode = tijdreeks_dagen[index + j, "MeetpuntCode"],
              PeilpuntCode = tijdreeks_dagen[index + j, "PeilpuntCode"],
              PeilmetingDatum = tijdreeks_dagen[index + j , "PeilmetingDatum"] - 1, 
              HydrologischJaar = tijdreeks_dagen[index + j, "HydrologischJaar"],
              Jaar = tijdreeks_dagen[index + j, "Jaar"],
              .before = index + j)
    j <- j + 1
  }
  
  # Periode voor facet_wrap!!!
  if (aangepaste_periode) {
    
    tijdreeks_periode <- tijdreeks_dagen %>%
      group_by(Jaar) %>% 
      filter(PeilmetingDatum %within% interval(str_c(periode_begin, "-",year(PeilmetingDatum)) %>%  dmy(),
                                               str_c(periode_einde, "-",year(PeilmetingDatum)) %>%  dmy())) %>% 
      ungroup() %>%
      mutate(Periode = as_date(str_c("2020-", month(PeilmetingDatum), "-", day(PeilmetingDatum), tz = NULL)))
    
    if (!is.null(jaar)) {
      tijdreeks_periode <- tijdreeks_periode %>% 
        filter(.data$Jaar %in% jaar)
    }
    
  } else {
    tijdreeks_periode <- tijdreeks_dagen %>% 
      mutate(Periode = if_else(PeilmetingDatum >= as_date(str_c(year(PeilmetingDatum), "-04-01"), format = "%Y-%m-%d"),
                               as_date(str_c("2019-", month(PeilmetingDatum), "-", day(PeilmetingDatum)), tz = NULL),
                               as_date(str_c("2020-", month(PeilmetingDatum), "-", day(PeilmetingDatum)), tz = NULL)))
    
    if (!is.null(jaar)) {
      tijdreeks_periode <- tijdreeks_periode %>% 
        filter(.data$HydrologischJaar %in% jaar)
    }
  }
  return(tijdreeks_periode)
}




# Nazoeken (bvb Putte Weiden) welke jaren heftige regenval in de zomer. Deze jaren eruit halen. 
# Kreekpeil in periode die je gebruikt in de periode die niet boven schothoogte komt. In DPN: 2m75 mTAW, 
# in Putten Weiden op 1.20 mTAW , Putten west is 1m80 mTAW
# Groot Rietveld -> Geen rekening mee houden.


# Indien droogval ook weglaten.

NeerslagOverschot <- function(tijdreeks, 
                              schothoogte = NULL,  # mTAW. Mag er niet over komen. Kan gebeuren door hevige regenval in de zomer. Indien dit het geval, dit jaar niet meenemen.
                              periode_zomermin = "01-08", #  minimum waarde dichts bij deze datum 
                              periode_begin = "01-04",  # April als beginperiode zodat zeker over stuwhoogte.
                              periode_einde = "31-12",  # Periode zo laat gezien meetpunt dichts bij 1 augustus.
                              ingegeven = FALSE,
                              neerslagoverschot = neerslagoverschot_gegevens_df) {
  # Krijg het grootste verschil tussen twee data in peilhoogte. Er moeten metingen voor juni liggen.
  # Data wordt niet gecontroleert of het voldoet aan de kwaliteitsvoorwaarden.
  # Calculate the greatest difference between two dates. The first needs to be after 1/04 (afer winter weir height).
  # Secondmin-value needs to be as close as possible around the date of periode_zomermin.
  #When tis is calculated, get the precipication surplus for this period from another data-file "neerslagoverschot".
  # Water coould never be above "schothoogte" otherwise water is leaving the system.
  # So when water above this height or below water-table -> NA

  
  neerslagoverschot <- neerslagoverschot %>% 
    mutate(Datum = ymd(Datum))
  
  tijdreeks_ <- tijdreeks %>% 
    AdaptTijdreeks(., ingegeven = ingegeven)
  
  delta_neerslagoverschot <- data.frame()
  
  for (jaar in unique(tijdreeks_$Jaar)) {
    
    tijdreeks_jaar <- tijdreeks_ %>% 
      dplyr::filter(.data$PeilmetingDatum %within% 
                      interval(as_date(str_c(periode_begin, "-", jaar), format = "%d-%m-%Y"),
                               as_date(str_c(periode_einde, "-", jaar), format = "%d-%m-%Y")))
    
    controle_droogval <- any(is.na(tijdreeks_jaar$PeilmetingTAW))
    if(controle_droogval) {
      print(str_c("Droogval ", jaar))
    }
    
    if (nrow(subset(tijdreeks_jaar, month(PeilmetingDatum) < 6)) != 0 & !controle_droogval) {  #Er zijn metingen voor juni en er is geen droogval
      # if (Controle_jaar$Controle) {
      
      coldate <- tijdreeks_jaar %>% 
        pull(PeilmetingDatum)
      
      x <- as_date(str_c(periode_zomermin, "-", jaar), format = "%d-%m-%Y")
      
      no <- which(abs(coldate-x) == min(abs(coldate-x)))[1]
      
      
      Min <- slice(tijdreeks_jaar, no) %>% 
        pull(PeilmetingTAW)
      
      
      WinterMax <- max(subset(tijdreeks_jaar, month(PeilmetingDatum) < 6)$PeilmetingTAW, na.rm = TRUE)  # Min moet vOOr de maand mei zijn.
      
      Min.Datum <- slice(tijdreeks_jaar, no) %>% 
        pull(PeilmetingDatum)
      
      WinterMax.Datum <- subset(tijdreeks_jaar, PeilmetingTAW == WinterMax) %>% 
        arrange(PeilmetingDatum) %>% 
        slice(., 1) %>% 
        pull(PeilmetingDatum)
      
      Delta <- WinterMax - Min
      
      controle <- FALSE
      
      if (!is.null(schothoogte)) {
        tijdreeks_schothoogte <- tijdreeks_ %>% 
          dplyr::filter(.data$PeilmetingDatum %within% 
                          interval(WinterMax.Datum, Min.Datum))
        tijdreeks_schothoogte <- tijdreeks_schothoogte %>% 
          filter(!is.na(PeilmetingTAW))
        
        if (nrow(tijdreeks_schothoogte) != 0) {
          controle <- any(tijdreeks_schothoogte$PeilmetingTAW >= schothoogte)
        }
      }
      
      if (!controle & !is.na(Min)) {  # Niet Lager als schothoogte OF droogval (NA)
        N.O <- subset(neerslagoverschot, Datum  %within% 
                        interval(as_date(Min.Datum, format = "%d-%m-%Y"),
                                 as_date(WinterMax.Datum, format = "%d-%m-%Y")))
        
        N.O <- sum(N.O$Neerslagoverschot)
        
        Gegevens <- bind_cols("Jaar" = jaar, "Min.Datum" = Min.Datum, "WinterMax.Datum" = WinterMax.Datum,
                              "Min" = Min, "WinterMax" = WinterMax, "Delta" = Delta, "Neerslagoverschot" = N.O)
        
        delta_neerslagoverschot <- bind_rows(delta_neerslagoverschot, Gegevens)
      }
    }
  }
  
  if (nrow(delta_neerslagoverschot) == 0) {
    delta_neerslagoverschot_finale <- delta_neerslagoverschot
  } else {
    delta_neerslagoverschot_finale <- delta_neerslagoverschot %>%
      tidyr::complete(., Jaar = min(tijdreeks_$Jaar):max(tijdreeks_$Jaar), fill = list(Delta  = NA_real_))
  }

  return(delta_neerslagoverschot_finale)
}




CommonYearsKwel <- function(data_kwel, aangepaste_periode) {
  # get the common years that there is at least data for two codes the time-series.
  
  data_kwel <- data_kwel %>%  dplyr::mutate(HydrologischJaar = if_else(PeilmetingDatum >= as_date(str_c(year(PeilmetingDatum), "-04-01"), format = "%Y-%m-%d"), 
                                           year(PeilmetingDatum), 
                                           as.double(year(PeilmetingDatum) - 1)),
                Jaar = year(PeilmetingDatum)) 
  
  if (aangepaste_periode) {
    data_kwel_jaren <- data_kwel %>%
      group_by(Jaar) %>% 
      filter(n_distinct(MeetpuntCode) >= 2) %>% 
      ungroup() %>% 
      arrange(MeetpuntCode, Jaar,PeilmetingDateTime)
  } else {
    data_kwel_jaren <- data_kwel %>%
      group_by(HydrologischJaar) %>% 
      filter(n_distinct(MeetpuntCode) >= 2) %>% 
      ungroup() %>% 
      arrange(MeetpuntCode, HydrologischJaar,PeilmetingDateTime)
  }
  
  if (nrow(data_kwel_jaren) == 0) {
    data_kwel_jaren <- NULL
  } 
  
  return(data_kwel_jaren)
}


DataKwelpunt <- function(kwelpunten,  # Vector voor de meetpunten die vergeleken dienen te worden.
                         maaiveld = TRUE, # Als TRUE in maaiveld hoogte, anders in mTAW
                         PeilpuntTAWMaaiveld = NULL,
                         ingegeven = FALSE, # TRUE ook ingegeven, anders enkel gevalideerd
                         aangepaste_periode = FALSE,
                         periode_begin = "01-01",  
                         periode_einde = "01-08",
                         controle = FALSE, # conteroleer op kwaliteit:
                         percent_decimal = 0.8, # kwantieti van de metingen voor aangepaste periode
                         min_aantal_metingen = 20, # kwantiteit voor hydrologisch jaar
                         RP_dagen = 30, # Representatieve periode
                         common_year = TRUE # Selecteer enkel de (hydro)jaren/jarren waar er voor beide meetpunten data is.
                         ) {
  
  
  # Get the time-series for seepage-points after controling the quality and get it for the good period.
  # PeilpuntTAWMaaiveld is MEAN van al de PeilpuntTAWMaaiveld !
  meetpunten_diepte <- watina_havens_peilpuntinfo %>% 
    dplyr::filter(.data$MeetpuntCode %in% kwelpunten) %>% 
    mutate(Diepte_filter_mTAW = PeilpuntHoogteBovenMaaiveld  - PeilpuntLengteBuis,
           Diepte_filter_top_mTAW =  PeilpuntHoogteBovenMaaiveld  - PeilpuntLengteBuis + PeilpuntLengteFilter) %>% 
    select(MeetpuntCode, Diepte_filter_mTAW, Diepte_filter_top_mTAW) %>% 
    # Als meerdere peilpunten aanwezig voor zelfde meetpunt, neem het minimum.
    group_by(MeetpuntCode) %>% 
    mutate(across(c(Diepte_filter_mTAW, Diepte_filter_top_mTAW), ~min(.x, na.rm = TRUE))) %>% 
    ungroup() %>% 
    distinct() %>% 
    # Nummer peilbuizen volgens diepte
    arrange(desc(Diepte_filter_mTAW)) %>% 
    mutate(Diepte = row_number()) 

  #Get Peilverloop + in Maaiveldhoogte
  gegevens_kwelpunt <- NULL
  
  watina <- connect_inbo_dbase(database_name = "D0025_00_Watina")  # Open data-set
  for (code in unique(meetpunten_diepte$MeetpuntCode)) {
    
    Peilverloop_kwel <- GetWatinaTijdreeks(watina, code)
    
    if (controle) {
      
      Peilverloop_kwel <- Peilverloop_kwel %>% 
      ControleTijdreeks(., percent_decimal = percent_decimal, 
                        min_aantal_metingen  = min_aantal_metingen ,
                        RP_dagen = RP_dagen, 
                        aangepaste_periode = aangepaste_periode,
                        periode_begin = periode_begin,
                        periode_einde = periode_einde,
                        ingegeven  = ingegeven)
      
      # Indien er bij controle een peilreeks geen data bevat.
      if (nrow(Peilverloop_kwel) == 0) {
        return(Peilverloop_kwel)
      }
      
    }
    gegevens_kwelpunt <- bind_rows(gegevens_kwelpunt, Peilverloop_kwel)
  }
  dbDisconnect(watina)  # close dataset
  
  gegevens_kwelpunt <- gegevens_kwelpunt %>% 
    left_join(., meetpunten_diepte, by = c("MeetpuntCode"))
  
  
  if (aangepaste_periode) {
    gegevens_kwelpunt <- gegevens_kwelpunt %>%
      mutate(Jaar = year(PeilmetingDatum)) %>% 
      group_by(Jaar) %>% 
      filter(PeilmetingDatum %within% interval(str_c(periode_begin, "-",year(PeilmetingDatum)) %>%  dmy(),
                                               str_c(periode_einde, "-",year(PeilmetingDatum)) %>%  dmy())) %>% 
      ungroup() 
  }
  

  if (maaiveld) {
    
    peilpunt_info <- watina_havens_peilpuntinfo %>% 
      filter(MeetpuntCode %in% kwelpunten)
    
     if (is.null(PeilpuntTAWMaaiveld)){
       PeilpuntTAWMaaiveld <- peilpunt_info %>% 
         summarise(PeilpuntTAWMaaiveld = mean(PeilpuntTAWMaaiveld, na.rm = TRUE)) %>% 
         pull()
     }
    
    
    if (is.na(PeilpuntTAWMaaiveld) & is.null(PeilpuntTAWMaaiveld)) {
      
      # Uitgaand dat PeilpuntTAWMaaiveld gelijk is.
      peilpunt_info_missend <- peilpunt_info %>% 
        group_by(MeetpuntCode, PeilpuntCode) %>% 
        reframe(PeilpuntTAWMaaiveld = PeilpuntTAWMaaiveld)
      
      return(peilpunt_info_missend)
    }
    gegevens_kwelpunt <- gegevens_kwelpunt %>% 
      KiesMeetMethode(TAW = !maaiveld, PeilpuntTAWMaaiveld = PeilpuntTAWMaaiveld)
    
  }
  
  if (common_year) {
    
    gegevens_kwelpunt <- gegevens_kwelpunt %>% 
      CommonYearsKwel(., aangepaste_periode = aangepaste_periode)
  }

  return(gegevens_kwelpunt)
}








DataOpbolling <- function(PeilbuisCode, PeilSchaalCode, 
                          aangepaste_periode = FALSE, 
                          periode_begin = "01-01", 
                          periode_einde = "01-08",
                          maaiveld = TRUE, 
                          jaar = NULL, 
                          ingegeven = FALSE, 
                          max_tijdspanne = 60,
                          controle = TRUE,
                          percent_decimal = 0.8,
                          min_aantal_metingen = 20,
                          RP_dagen = 30,
                          common_year = TRUE,
                          watina_havens_peilpuntinfo_ = watina_havens_peilpuntinfo) {
  
  #Get the time-serie for peilschaal en peilbuis in 1 tabel. De peilbuis mTAW wordt berkend op basis van PeilpuntTAWMaaiveld van de peilbuis
  
  # Krijg ineens de aangepaste en correcte data voor de opbolling.
  PeilpuntTAWMaaiveld <- watina_havens_peilpuntinfo_ %>% 
    filter(.data$MeetpuntCode == PeilbuisCode  & !is.na(.data$PeilpuntTAWMaaiveld)) %>%
    summarise(PeilpuntTAWMaaiveld = mean(PeilpuntTAWMaaiveld, na.rm = TRUE)) %>% 
    pull(PeilpuntTAWMaaiveld)
  
  
  codes <- c(PeilbuisCode, PeilSchaalCode)
  gegevens_opbolling <- NULL
  
  watina <- connect_inbo_dbase(database_name = "D0025_00_Watina")  # Open data-set

  for (code in codes) {
    
    tijdreeks_code <- GetWatinaTijdreeks(watina, code) %>% 
      AdaptTijdreeks(., ingegeven = ingegeven) %>% 
      KiesMeetMethode(., TAW = !maaiveld, PeilpuntTAWMaaiveld = PeilpuntTAWMaaiveld)
    
    if (controle) {
      
      tijdreeks_code <- tijdreeks_code %>% 
        ControleTijdreeks(., percent_decimal = percent_decimal, 
                          min_aantal_metingen  = min_aantal_metingen ,
                          RP_dagen = RP_dagen, 
                          aangepaste_periode = aangepaste_periode,
                          periode_begin = periode_begin,
                          periode_einde = periode_einde,
                          ingegeven  = ingegeven)
      
    }
    
    gegevens_opbolling <- bind_rows(gegevens_opbolling, tijdreeks_code)
  }
  dbDisconnect(watina)  # close dataset
  
  
  if (aangepaste_periode) {
    
    gegevens_opbolling <- gegevens_opbolling %>%
      group_by(Jaar) %>% 
      filter(PeilmetingDatum %within% interval(str_c(periode_begin, "-",year(PeilmetingDatum)) %>%  dmy(),
                                               str_c(periode_einde, "-",year(PeilmetingDatum)) %>%  dmy())) %>% 
      ungroup() 
  }

  
  
  # Filter gegevens op het correcte jaar.
  if (!is.null(jaar)) {
    if (!aangepaste_periode) {
      gegevens_opbolling <- dplyr::filter(gegevens_opbolling, .data$HydrologischJaar %in% jaar)
    } else {
      gegevens_opbolling <- dplyr::filter(gegevens_opbolling, .data$Jaar %in% jaar)
    }
  } 

  if (common_year) {
    gegevens_opbolling <- gegevens_opbolling %>% 
      CommonYearsKwel(., aangepaste_periode = aangepaste_periode)
    if (is.null(gegevens_opbolling)) {
      # Geen gemeenschappelijke jaren voor ingegeven periode.
      gegevens_opbolling <- data.frame()
    }
    
  }
  
  
  
  return(list(data = gegevens_opbolling, maaiveld = str_c("Maaiveld: ", PeilpuntTAWMaaiveld, " mTAW")))
}


GemiddeldZomerpeil <- function(tijdreeks, periode_begin = "15-07", periode_einde = "31-08", 
                               ingegeven = FALSE) {
  # Get mean of the summer
  gemiddelde <- tijdreeks %>%
    AdaptTijdreeks(., ingegeven = ingegeven) %>% 
    dplyr::filter(.data$PeilmetingDatum %within% 
                                  interval(as_date(str_c(periode_begin, "-", Jaar), format = "%d-%m-%Y"),
                                           as_date(str_c(periode_einde, "-", Jaar), format = "%d-%m-%Y"))) %>% 
    summarise(gemiddelde = mean(PeilmetingTAW, na.rm = TRUE)) %>% 
    pull()
  
 return(gemiddelde)   
}


# Grafieken ------
GrafiekPeilverloopSimple <- function(Peilreeks,
                                     maaiveld_min = NULL,
                                     maaiveld_nul = TRUE,
                                     periode_begin = "01-01",
                                     periode_einde = "31-12",
                                     y_tekst = "",
                                     breaks_x_scale = 10) {
  
  
  if (!is.null(maaiveld_min)) {
    Peilreeks_maaiveld <- Peilreeks %>% 
      mutate('PeilpuntTAWMaaiveld' = maaiveld_min)
  }
  
  # if (!is.null(info_maaiveld)) {
  #   Peilreeks_maaiveld <- Peilreeks %>% 
  #     left_join(., info_maaiveld %>%  
  #                 select(MeetpuntCode, PeilpuntTAWMaaiveld), by = c("MeetpuntCode"))
  # } else {
  #   Peilreeks_maaiveld <- Peilreeks %>% 
  #     mutate('PeilpuntTAWMaaiveld' = NA_real_)
  # }
  
  G <- ggplot(data = Peilreeks_maaiveld) +
    geom_line(aes(x = PeilmetingDatum, y = PeilmetingTAW,  col  = MeetpuntCode), linewidth = 1) +
    #Add minimul maaiveld if it isnt NULL
    {if(!is.null(maaiveld_min)) {
      geom_line(aes(x = PeilmetingDatum, y = 0), col = "black", linewidth = 1.2)
    }
    } +
    geom_line(aes(x = PeilmetingDatum, y = PeilpuntTAWMaaiveld), col = "brown", linewidth = 1.2) +
    {if(maaiveld) {
      geom_line(aes(x = PeilmetingDatum, y = 0), col = "black", linewidth = 1.2)
    }} +
    scale_x_date(date_labels = "%m-%Y", 
                 limits = c(as.Date(min(Peilreeks_maaiveld$PeilmetingDatum)),
                            as.Date(max(Peilreeks_maaiveld$PeilmetingDatum))),
                 breaks = breaks_pretty(n = breaks_x_scale),
                 date_minor_breaks =  "1 month") +
    scale_y_continuous(
      limits = c(
        min(c(
          RoundAny(Peilreeks_maaiveld$PeilmetingTAW, 0.1, floor),
          unique(Peilreeks_maaiveld$PeilpuntTAWMaaiveld)), na.rm = TRUE),
        max(c(
          RoundAny(Peilreeks_maaiveld$PeilmetingTAW, 0.1, ceiling),
          unique(Peilreeks_maaiveld$PeilpuntTAWMaaiveld)), na.rm = TRUE)),
      
      breaks = seq(
        min(c(
          RoundAny(Peilreeks_maaiveld$PeilmetingTAW, 0.1, floor),
          unique(Peilreeks_maaiveld$PeilpuntTAWMaaiveld)), na.rm = TRUE),
        max(c(
          RoundAny(Peilreeks_maaiveld$PeilmetingTAW, 0.1, ceiling),
          unique(Peilreeks_maaiveld$PeilpuntTAWMaaiveld)), na.rm = TRUE),
        by = 0.2),
      minor_breaks = waiver()) +
    geom_label(aes(label = Label, x = PeilmetingDatum, y = min(PeilmetingTAW, na.rm = TRUE) + 0.01),
               fill = 'transparent', color = inbo_bruinrood, show.legend = FALSE, label.size = 0) +
    labs(y = y_tekst, x = "", col = "Peil")
  
  
  schrift <- str_c("Tijdreeks voor peilpunt ", code, " van ", 
                   Peilreeks %>% 
                     slice_min(., PeilmetingDatum, with_ties = FALSE) %>% 
                     pull(PeilmetingDatum) %>% 
                     format(., "%d-%m-%Y")," tot ", Peilreeks %>% 
                     slice_max(., PeilmetingDatum, with_ties = FALSE) %>% 
                     pull(PeilmetingDatum) %>% 
                     format(., "%d-%m-%Y"),". Het maaiveld ligt op ", info_code %>% 
                     distinct(PeilpuntTAWMaaiveld) %>% 
                     pull(PeilpuntTAWMaaiveld)," mTAW. De grenswaarde staat op ", 
                   maaiveld_min," meter onder het maaiveld.", sep = "")
  
  
  
  
  
  return(list(Graf = G, Bijschrift = schrift))
}


GrafiekPeilverloopSimpleTAW <- function(Peilreeks,
                                        y_tekst = "",
                                        breaks_x_scale = 10) {
  
  

  
  
  G <- ggplot(data = Peilreeks) +
    geom_line(aes(x = PeilmetingDatum, y = PeilmetingTAW,  col  = MeetpuntCode), linewidth = 1) +
    scale_x_date(date_labels = "%m-%Y", 
                 limits = c(as.Date(min(Peilreeks$PeilmetingDatum)),
                            as.Date(max(Peilreeks$PeilmetingDatum))),
                 breaks = breaks_pretty(n = breaks_x_scale),
                 date_minor_breaks =  "1 month") +
    scale_y_continuous(
      limits = c(
        min(RoundAny(Peilreeks$PeilmetingTAW, 0.1, floor), na.rm = TRUE),
        max(RoundAny(Peilreeks$PeilmetingTAW, 0.1, ceiling), na.rm = TRUE)),
      breaks = seq(min(RoundAny(Peilreeks$PeilmetingTAW, 0.1, floor), na.rm = TRUE),
                   max(RoundAny(Peilreeks$PeilmetingTAW, 0.1, ceiling), na.rm = TRUE),
                   by = 0.2),
      minor_breaks = waiver()) +
    geom_label(aes(label = Label, x = PeilmetingDatum, y = min(PeilmetingTAW, na.rm = TRUE) + 0.01),
               fill = 'transparent', color = inbo_bruinrood, show.legend = FALSE, label.size = 0) +
    labs(y = y_tekst, x = "", col = "Peil")
  
  
  label <- Peilreeks %>% 
    distinct(Label) %>% 
    filter(!is.na(Label)) %>% 
    pull()
  
  
  schrift <- str_c("Tijdreeks voor peilpunt ", code, " van ", 
                   Peilreeks %>% 
                     slice_min(., PeilmetingDatum, with_ties = FALSE) %>% 
                     pull(PeilmetingDatum) %>% 
                     format(., "%d-%m-%Y")," tot ", Peilreeks %>% 
                     slice_max(., PeilmetingDatum, with_ties = FALSE) %>% 
                     pull(PeilmetingDatum) %>% 
                     format(., "%d-%m-%Y"),". Periode van droogval zijn aangeduid met ", label, ".", sep = "")
  
  
  
  
  
  return(list(Graf = G, Bijschrift = schrift))
}



