library(ggplot2)

RuimteRondGrafiek <- 0.5 #cm
LetterGrootte <- 9

Opmaak.Graf <- function(Graf, Grid = TRUE, X.Jaar = FALSE) {
  Graf <- Graf + 
    theme(panel.background = element_rect(fill = "white"))
  if (Grid) {
    Graf <- Graf + 
      theme(panel.grid = element_line(color = "grey90"))
  } else {
    Graf <- Graf + 
      theme(panel.grid = element_blank()) 
  }
  Graf <- Graf + 
    theme(axis.line = element_line(color = Kleur2)) + 
    theme(axis.ticks = element_line(color = Kleur2)) + 
    theme(axis.text = element_text(color = Kleur2, size = LetterGrootte)) +
    theme(axis.title.y = element_text(color = Kleur2, size = LetterGrootte)) +
    theme(axis.title = element_text(color = Kleur2))
  
  if (X.Jaar) {
    Graf <- Graf + 
      scale_x_continuous(breaks = c(2003,2005,2007,2009,2011,2013,2015,2017,2019))
  }
  Graf
}


klimaatsamenvattingPerDagISOWEEK <- function(data_weer, datum_start, datum_einde, NA_rm = TRUE) {
  
  
  # Neem de volledige week mee waarin het start- als einddatum invalt.
  # Dit zorgt ervoor dat de aantal halve maanden afwisselt per jaar. 
  # Bereken Tmin, Tmax, Tgem, Wgem, Ntot per dag voor een opgegeven periode
  
  start_datum <- datum_start %>% 
    as.Date(., format = "%d/%m/%Y")
  
  einde_datum <- datum_einde %>% 
    as.Date(., format = "%d/%m/%Y")
  
  # Add weeks
  data_weer_week <- data_weer %>% 
    mutate(Week = isoweek(DatumTijd),
           Datum = as_date(DatumTijd)) %>% 
    filter(
      (Jaar ==  year(start_datum) & Week == isoweek(start_datum)) | 
        (Jaar ==  year(einde_datum) & Week == isoweek(einde_datum))
    ) %>% 
    distinct(DatumTijd, Datum, Dag, Maand, Jaar)
  
  
  # Get the first date of the week wherein the startdate falls
  datum_start_week <- data_weer_week %>% 
    slice_min(DatumTijd, with_ties = FALSE) %>% 
    mutate(DatumTijd = as.POSIXlt(str_c(str_c(Dag, Maand, Jaar, sep = "/"),"00:00:00"), 
                                  format = "%d/%m/%Y %H:%M:%S", 
                                  origin = "01/01/1970 00:00:00")) %>% 
    pull(DatumTijd)
  
  # Get the first date of the week wherein the einddate falls
  datum_einde_week <- data_weer_week %>% 
    slice_max(DatumTijd, with_ties = FALSE) %>% 
    mutate(DatumTijd = as.POSIXlt(str_c(str_c(Dag, Maand, Jaar, sep = "/"),"23:59:59"), 
                                  format = "%d/%m/%Y %H:%M:%S", 
                                  origin = "01/01/1970 00:00:00")) %>% 
    pull(DatumTijd)
  
  
  data_weer_subset <- data_weer %>% 
    mutate(
      DatumTijd = as.POSIXlt(DatumTijd, format = "%d/%m/%Y %H:%M", origin = "01/01/1970 00:00"),
      Datum = as_date(DatumTijd),
      Week = isoweek(DatumTijd)) %>%
    filter((DatumTijd >= datum_start_week) & (DatumTijd <= datum_einde_week)) %>% 
    group_by(Datum) %>% 
    summarise(
      Tmin = min(Temperatuur, na.rm = NA_rm),
      Tmax = max(Temperatuur, na.rm = NA_rm),
      Tgem = mean(Temperatuur, na.rm = NA_rm),
      WSgem = mean(Windsnelheid, na.rm = NA_rm),
      NStot = sum(Neerslag, na.rm = NA_rm),
      .groups = "drop") %>% 
    mutate(
      Periode = as.POSIXct(Datum),
      Jaar = year(Periode),
      Maand = month(Periode),
      Dag = mday(Periode),
      # maandag tot zondag
      Week = isoweek(Periode)
      ) %>% 
    group_by(Jaar, Week) %>% 
    mutate(Periode_week = cur_group_id()) %>% 
    ungroup()
  
  return(data_weer_subset)
}


OpdelingHalveMaand <- function(data_weer_subset, opdeling_halvemaand = TRUE, min_dagen_blok = 7) {
  # Deelt de datums op in halve maand (TRUE) of voor volledie maand (FALSE).
  # BELANGRIJK: maanden worden opgedeeld in blokken van 2. 1 blok 1-15e & 16- einde maand!
  # Als een (Halve) maand blok minder als 7 (min_dagen_blok) dagen bevat, dan zal dit toegevoegd worden aan de vorige groep, of aan de volgende groep als dit aan het begin van de periode is! Concreet: 15/03 --> Zal worden toegevoegd aan tweede deel maand, 15/07 aan eerste deel maand. 01/03 aan tweede deel februari. Zo onstaan er geen blokken met slechts enkele dagen waarop een statistiek op wordt berekend!
  
  
  if (opdeling_halvemaand) {
    data_opdeling <- data_weer_subset %>% 
      mutate(
        Jaar = year(Datum),
        Maand = month(Datum),
        Block = ifelse(day(Datum) <= 15, 1, 2)
      ) %>% 
      arrange(Datum) %>% 
      mutate(Block_num = dense_rank(str_c(Jaar, Maand, Block))) %>% 
      # Adjust block_num. When a group of halve maand is kleiner als 7 dagen, voeg dit toe bij de vorige groep, of volgende grep als dit de eerste week van de periode is.
      group_by(Jaar, Maand, Block)
    
  } else {
    data_opdeling <- data_weer_subset %>% 
      mutate(
        Jaar = year(Datum),
        Maand = month(Datum)
      ) %>% 
      arrange(Datum) %>% 
      mutate(Block_num = dense_rank(str_c(Jaar, Maand))) %>% 
      # Adjust block_num. When a group of halve maand is kleiner als 7 dagen, voeg dit toe bij de vorige groep, of volgende grep als dit de eerste week van de periode is.
      group_by(Jaar, Maand)
  }
  
  data_opdeling <- data_opdeling %>% 
    mutate(
      Block_num = case_when(
        n() < min_dagen_blok & Block_num != 1 ~ Block_num - 1,
        n() < min_dagen_blok & Block_num == 1 ~ Block_num + 1,
        TRUE ~ Block_num
      )) %>% 
    ungroup() %>% 
    mutate(Periode = dense_rank(Block_num)) %>% 
    dplyr::select(-c(Block_num))
  
  return(data_opdeling)
}


klimaatsamenvattingPerDag <- function(data_weer, datum_start, datum_einde, NA_rm = TRUE,
                                      min_dagen_blok = 7) {
  
  # Bereken Tmin, Tmax, Tgem, Wgem, Ntot per dag voor een opgegeven periode
  datum_start_CET <- as.POSIXlt(paste0(datum_start,"00:00:00"), 
                            format = "%d/%m/%Y %H:%M:%S", 
                            origin = "01/01/1970 00:00:00")
  
  datum_einde_CET <- as.POSIXlt(paste0(datum_einde,"23:59:59"), 
                            format = "%d/%m/%Y %H:%M:%S", 
                            origin = "01/01/1970 00:00:00")
  
  data_weer_subset <- data_weer %>% 
    mutate(
      DatumTijd = as.POSIXlt(DatumTijd, format = "%d/%m/%Y %H:%M", origin = "01/01/1970 00:00"),
      Datum = as_date(DatumTijd)
      ) %>% 
    subset(., (DatumTijd >= datum_start_CET) & (DatumTijd <= datum_einde_CET)) %>% 
    group_by(Datum) %>% 
    summarise(
      Tmin = min(Temperatuur, na.rm = NA_rm),
      Tmax = max(Temperatuur, na.rm = NA_rm),
      Tgem = mean(Temperatuur, na.rm = NA_rm),
      WSgem = mean(Windsnelheid, na.rm = NA_rm),
      NStot = sum(Neerslag, na.rm = NA_rm),
      .groups = "drop")
  
  return(data_weer_subset)
}


TijdreeksWeerBroedseizoen <- function(jaarkeuze, weertabel, 
                                      parameter = "Tgem", lijnkleur = Kleur3,
                                      lijndikte = 1, ruimte = RuimteRondGrafiek) {
  
  datum_start <- paste0("15/03/",jaarkeuze)
  datum_einde <- paste0("15/07/",jaarkeuze)
  
  gegevens <- klimaatsamenvattingPerDag(weertabel, datum_start, datum_einde) %>% 
    mutate("Periode" = as.POSIXct(Datum)) %>% 
    pivot_longer(., cols = !c(Datum, Periode), 
                 names_to = "Parameter", values_to = "Waarde") %>% 
    filter(Parameter == parameter) %>% 
    arrange(Periode)
  
  breaks <- c(
    as.POSIXct(datum_start, format = "%d/%m/%Y"),
    as.POSIXct(str_c("01/04/",jaarkeuze), format = "%d/%m/%Y"),
    as.POSIXct(str_c("01/05/",jaarkeuze), format = "%d/%m/%Y"),
    as.POSIXct(str_c("01/06/",jaarkeuze), format = "%d/%m/%Y"),
    as.POSIXct(str_c("01/07/",jaarkeuze), format = "%d/%m/%Y"),
    as.POSIXct(datum_einde, format = "%d/%m/%Y")
    )
  
  labels <- c("15 maart" ,"1 april","1 mei","1 juni","1 juli", "15 juli")
  
  if (parameter == "Tgem") {
    text_plot <- str_c("Gemiddelde temperatuur ", "(\u00B0C)")
  }
  if (parameter == "Tmin") {
    text_plot <- str_c("Minimum temperatuur ", "(\u00B0C)")
  }
  if (parameter == "Tmax") {
    text_plot <- str_c("Maximum temperatuur ", "(\u00B0C)")
  }
  if (parameter == "WSgem") {
    text_plot <- "Gemiddelde windsnelheid (m/s)"
  }
  if (parameter == "NStot") {
    text_plot <- "Neerslag (mm)"
  }
  
  # Scale_y_continuous
  ymin <- gegevens %>% 
    slice_min(Waarde, with_ties = FALSE) %>% 
    pull(Waarde)
  
  ymax <- gegevens %>% 
    slice_max(Waarde, with_ties = FALSE) %>% 
    pull(Waarde)
  
  multiplier <- 5
  
  # Altijd van 0 beginnen, behalve als kleiner als 0
  ymin_breaks <- if_else(round(ymin / multiplier) * multiplier > 0, 0,  round(ymin / multiplier) * multiplier)
  ymax_breaks <- round(ymax / multiplier) * multiplier

  grafiek <- ggplot(data = gegevens) + 
    geom_line(aes(x = Periode, y = Waarde), color = lijnkleur, lwd = lijndikte) + 
    scale_x_datetime(breaks = breaks, labels = labels, minor_breaks = "day") +
    scale_y_continuous(breaks = seq(ymin_breaks, ymax_breaks, by = multiplier), 
                       limits = c(min(ymin, ymin_breaks), max(ymax, ymax_breaks))) +
    labs(y = text_plot, x = "") + 
    theme(
      axis.title.x = element_blank(), 
      plot.margin = margin(ruimte, ruimte, ruimte, ruimte,"cm"),
      axis.title.y = element_text(margin = margin(0,0.2,0,0,"cm")))
  
  return(grafiek)
}


GrafiekSamenvattingWeerBroedseizoen <- function(jaarkeuze = werk_jaar, weertabel = data_weer, 
                                              parameter = "Tgem", 
                                              opdeling_halvemaand = TRUE,   
                                              kleur = Kleur3, vulkleur = Kleur3, 
                                              lijndikte = 1, alpha = 0.5, puntgrootte = 3, 
                                              NA_rm = TRUE, include_totaal = FALSE, 
                                              ruimte = RuimteRondGrafiek) {
  weertabelsubset <- weertabel %>%
    filter(Jaar <= jaarkeuze) %>% 
    arrange(DatumTijd)
  
  # Klimaatsamenvatting per dag
  data_totaal <- weertabelsubset %>% 
    group_split(Jaar) %>% 
    map2(., .y = unique(weertabelsubset$Jaar),  
         ~ klimaatsamenvattingPerDag(data_weer = .x,
                                     datum_start = str_c("15/03/", .y), 
                                     datum_einde = str_c("15/07/", .y), 
                                     NA_rm = NA_rm)) %>% 
    map(., ~ OpdelingHalveMaand(., opdeling_halvemaand = opdeling_halvemaand)) %>% 
    reduce(., .f = bind_rows) 
    
  
  # Statistiek per parameter Jaar en halvemaandperiode
  data_totaal_statistiek <- data_totaal %>%
    pivot_longer(., cols = c(Tmin:NStot), names_to = "Parameter", values_to = "Waarde") %>% 
    filter(Parameter == parameter) %>% 
    group_by(Jaar, Parameter, Periode) %>% 
    # IF NStot take sum, otherwise mean
    summarise(across(.cols = c(Waarde), 
                     ~if_else(parameter %in% c("NStot"), sum(.x, na.rm = NA_rm), mean(.x, na.rm = NA_rm))),
              .groups = "drop")
  
  # Data voor de lijngrafiek voor jaarkeuze
  data_graf_lijn <- data_totaal_statistiek %>% 
    filter(Jaar == jaarkeuze)
    
  # Data voor spreiding per jaar
  data_graf_bar <- data_totaal_statistiek %>% 
    group_by(Parameter, Periode) %>% 
    summarise(
      Min = min(Waarde, na.rm = NA_rm),
      Max  = max(Waarde, na.rm = NA_rm),
              .groups = "drop")

  
  if (include_totaal) {
    
    data_graf_bar <- data_graf_bar %>% 
      bind_rows(., 
                # Krijg het min en max van de jaarstatstiek van alle jaren.
                data_totaal %>%
                  pivot_longer(., cols = c(Tmin:NStot), names_to = "Parameter", values_to = "Waarde") %>% 
                  filter(Parameter == parameter) %>% 
                  group_by(Jaar, Parameter) %>% 
                  # IF NStot take sum, otherwise mean
                  summarise(across(.cols = c(Waarde),
                                   ~if_else(parameter %in% c("NStot"), sum(.x, na.rm = NA_rm), mean(.x, na.rm = NA_rm))),
                            .groups = "drop") %>% 
                  group_by(Parameter) %>% 
                  summarise(
                    Min = min(Waarde, na.rm = NA_rm),
                    Max  = max(Waarde, na.rm = NA_rm),
                    .groups = "drop") %>% 
                  mutate(Periode = 10)
      )
    
    # Data voor totaalpunt (group enkel Jaar)
    data_graf_bar_punt <- data_totaal %>%
      pivot_longer(., cols = c(Tmin:NStot), names_to = "Parameter", values_to = "Waarde") %>% 
      filter(Parameter == parameter) %>%
      filter(Jaar == jaarkeuze) %>% 
      group_by(Jaar, Parameter) %>% 
      # IF NStot take sum, otherwise mean
      summarise(across(.cols = c(Waarde), 
                       ~if_else(parameter %in% c("NStot"), sum(.x, na.rm = NA_rm), mean(.x, na.rm = NA_rm))),
                .groups = "drop") %>% 
      mutate(Periode = 10)
  }
  
  if (parameter == "Tgem") {
    text_plot <- str_c("Gemiddelde temperatuur ", "(\u00B0C)")
  }
  if (parameter == "Tmin") {
    text_plot <- str_c("Minimum temperatuur ", "(\u00B0C)")
  }
  if (parameter == "Tmax") {
    text_plot <- str_c("Maximum temperatuur ", "(\u00B0C)")
  }
  if (parameter == "WSgem") {
    text_plot <- "Gemiddelde windsnelheid (m/s)"
  }
  if (parameter == "NStot") {
    text_plot <- "Totale neerslag (mm)"
  }
  
  
  # Scale_y_continuous
  ymin <- data_graf_bar %>% 
    slice_min(Min, with_ties = FALSE) %>% 
    pull(Min)
  
  ymax <- data_graf_bar %>% 
    slice_max(Max, with_ties = FALSE) %>% 
    pull(Max)
  
  multiplier <- if_else(parameter == "NStot", 100, 5)
  
  y_breaks <- round(ymax / multiplier) * multiplier
  
  # Altijd van 0 beginnen, behalve als kleiner als 0
  ymin_breaks <- if_else(round(ymin / multiplier) * multiplier > 0, 0,  round(ymin / multiplier) * multiplier)
  ymax_breaks <- round(ymax / multiplier) * multiplier

  grafiek <- ggplot() +
    geom_linerange(data = data_graf_bar, aes(x = Periode, ymin = Min, ymax = Max, col = Parameter),
                   alpha = alpha, linewidth = 10) +
    geom_line(data = data_graf_lijn, aes(x = Periode, y = Waarde), 
              color = kleur, linewidth = lijndikte) +
    scale_color_manual(values = c(vulkleur,NA)) +
    labs( y = text_plot,  x = "") +
    scale_y_continuous(breaks = seq(ymin_breaks, ymax_breaks, by = multiplier), 
                       limits = c(min(ymin, ymin_breaks), max(ymax, ymax_breaks))) +
    theme(
      legend.position = "none",
      panel.grid.minor.x = element_blank(),
      plot.margin = margin(ruimte,ruimte,ruimte,ruimte,"cm"),
      axis.title.y = element_text(margin = margin(0,0.2,0,0,"cm"))
      ) 
  
  if (include_totaal) {
    grafiek <- grafiek +
      geom_point(data = data_graf_bar_punt, aes(x = Periode, y = Waarde), col = kleur, size = puntgrootte) +
      scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,10), labels = c("mar","apr","apr","mei","mei","jun","jun","jul","Totaal"))
      } else {
        grafiek <- grafiek +
          scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8), labels = c("mar","apr","apr","mei","mei","jun","jun","jul"))
      }
      
  return(grafiek)
}


TijdreeksWeerWinter <- function(jaarkeuze, weertabel, 
                              parameter = "Tgem", lijnkleur = Kleur3,
                              lijndikte = 1, ruimte = RuimteRondGrafiek, labels_x2 = TRUE) {
  
  # Jaar van winter is het broedseizoen! Was in de vorige versie van het werkjaar - werkjaar + 1. Klopt volgens mij niet.
  
  datum_start <- str_c("01/10/",jaarkeuze - 1)
  datum_einde <- str_c("31/03/",jaarkeuze)
  
  gegevens <- klimaatsamenvattingPerDag(weertabel, datum_start, datum_einde) %>% 
    mutate("Periode" = as.POSIXct(Datum)) %>% 
    pivot_longer(., cols = !c(Datum, Periode), 
                 names_to = "Parameter", values_to = "Waarde") %>% 
    filter(Parameter == parameter) %>% 
    arrange(Periode)
  
  breaks <- c(as.POSIXct(str_c("01/10/",jaarkeuze - 1), format = "%d/%m/%Y"),
              as.POSIXct(str_c("01/11/",jaarkeuze - 1), format = "%d/%m/%Y"),
              as.POSIXct(str_c("01/12/",jaarkeuze - 1), format = "%d/%m/%Y"),
              as.POSIXct(str_c("01/01/",jaarkeuze), format = "%d/%m/%Y"),
              as.POSIXct(str_c("01/02/",jaarkeuze), format = "%d/%m/%Y"),
              as.POSIXct(str_c("01/03/",jaarkeuze), format = "%d/%m/%Y"),
              as.POSIXct(str_c("01/04/",jaarkeuze), format = "%d/%m/%Y"))
  
  if (labels_x2) {
    labels <- c("1 oktober","","1 december","","1 februari","", "1 april")
  } else {
    labels <- c("1 oktober","1 november","1 december","1 januari","1 februari","1 maart", "1 april")
  }
  

  if (parameter == "Tgem") {
    text_plot <- str_c("Gemiddelde temperatuur ", "(\u00B0C)")
  }
  if (parameter == "Tmin") {
    text_plot <- str_c("Minimum temperatuur ", "(\u00B0C)")
  }
  if (parameter == "Tmax") {
    text_plot <- str_c("Maximum temperatuur ", "(\u00B0C)")
  }
  if (parameter == "WSgem") {
    text_plot <- "Gemiddelde windsnelheid (m/s)"
  }
  if (parameter == "NStot") {
    text_plot <- "Neerslag (mm)"
  }
  
  # Scale_y_continuous
  ymin <- gegevens %>% 
    slice_min(Waarde, with_ties = FALSE) %>% 
    pull(Waarde)
  
  ymax <- gegevens %>% 
    slice_max(Waarde, with_ties = FALSE) %>% 
    pull(Waarde)
  
  multiplier <- 5
  
  # Altijd van 0 beginnen, behalve als kleiner als 0
  ymin_breaks <- if_else(round(ymin / multiplier) * multiplier > 0, 0,  round(ymin / multiplier) * multiplier)
  ymax_breaks <- round(ymax / multiplier) * multiplier
  
  grafiek <- ggplot(data = gegevens) + 
    geom_line(aes(x = Periode, y = Waarde), color = lijnkleur, lwd = lijndikte) + 
    scale_x_datetime(breaks = breaks, labels = labels, minor_breaks = "day") +
    scale_y_continuous(breaks = seq(ymin_breaks, ymax_breaks, by = multiplier), 
                       limits = c(min(ymin, ymin_breaks), max(ymax, ymax_breaks))) +
    ylab(text_plot) + 
    theme(
      axis.title.x = element_blank(), 
      plot.margin = margin(ruimte, ruimte, ruimte, ruimte,"cm"),
      axis.title.y = element_text(margin = margin(0,0.2,0,0,"cm")))
  
  return(grafiek)
}


GrafiekSamenvattingWeerWinter <- function(jaarkeuze = werk_jaar, weertabel = data_weer, 
                                   parameter = "Tgem",
                                   opdeling_halvemaand = FALSE,
                                   kleur = Kleur3, vulkleur = Kleur3, 
                                   lijndikte = 1, alpha = 0.5, puntgrootte = 3, 
                                   NA_rm = TRUE, include_totaal = FALSE, 
                                   ruimte = RuimteRondGrafiek,
                                   labels_x2 = TRUE) {
  
  weertabelsubset <- weertabel %>%
    # Jaar wordt hydrologisch jaar (vanaf april !)
    mutate(Hydrojaar = if_else(Maand >= 4, Jaar + 1, Jaar)) %>% 
    # Verwijder ook het eerste &laatste hydrojaar die incompleet zijn.
    filter(Hydrojaar <= jaarkeuze & Hydrojaar > min(weertabel$Jaar, na.rm = TRUE)) %>% 
    arrange(DatumTijd)

  # Klimaatsamenvatting per dag
  data_totaal <- weertabelsubset %>%
    group_split(Hydrojaar) %>% 
    map2(., .y = unique(weertabelsubset$Hydrojaar),  
         ~ klimaatsamenvattingPerDag(data_weer = .x,
                                     datum_start = str_c("01/10/", .y - 1), 
                                     datum_einde = str_c("31/03/", .y), 
                                     NA_rm = NA_rm)) %>% 
    map(., ~ OpdelingHalveMaand(., opdeling_halvemaand = opdeling_halvemaand)) %>% 
    reduce(., .f = bind_rows) %>% 
    # Hydrologisch jaar
    mutate(Jaar = if_else(Maand >= 4, Jaar + 1, Jaar))
  
  # Statistiek per parameter Jaar en halvemaandperiode
  data_totaal_statistiek <- data_totaal %>%
    pivot_longer(., cols = c(Tmin:NStot), names_to = "Parameter", values_to = "Waarde") %>% 
    filter(Parameter == parameter) %>% 
    group_by(Jaar, Parameter, Periode) %>% 
    # IF NStot take sum, otherwise mean
    summarise(across(.cols = c(Waarde), 
                     ~if_else(parameter %in% c("NStot"), sum(.x, na.rm = NA_rm), mean(.x, na.rm = NA_rm))),
              .groups = "drop")
  
  # Data voor de lijngrafiek voor jaarkeuze
  data_graf_lijn <- data_totaal_statistiek %>% 
    filter(Jaar == jaarkeuze)
  
  # Data voor spreiding per jaar
  data_graf_bar <- data_totaal_statistiek %>% 
    group_by(Parameter, Periode) %>% 
    summarise(
      Min = min(Waarde, na.rm = NA_rm),
      Max  = max(Waarde, na.rm = NA_rm),
      .groups = "drop")
  
  if (include_totaal) {
    
    data_graf_bar <- data_graf_bar %>% 
      bind_rows(., 
                # Krijg het min en max van de jaarstatstiek van alle jaren.
                data_totaal %>%
                  pivot_longer(., cols = c(Tmin:NStot), names_to = "Parameter", values_to = "Waarde") %>% 
                  filter(Parameter == parameter) %>% 
                  group_by(Jaar, Parameter) %>% 
                  # IF NStot take sum, otherwise mean
                  summarise(across(.cols = c(Waarde),
                                   ~if_else(parameter %in% c("NStot"), sum(.x, na.rm = NA_rm), mean(.x, na.rm = NA_rm))),
                            .groups = "drop") %>% 
                  group_by(Parameter) %>% 
                  summarise(
                    Min = min(Waarde, na.rm = NA_rm),
                    Max  = max(Waarde, na.rm = NA_rm),
                    .groups = "drop") %>% 
                  mutate(Periode = max(data_graf_bar$Periode) + 2)
                )
    
    # Data voor totaalpunt (group enkel Jaar)
    data_graf_bar_punt <- data_totaal %>%
      pivot_longer(., cols = c(Tmin:NStot), names_to = "Parameter", values_to = "Waarde") %>% 
      filter(Parameter == parameter) %>%
      filter(Jaar == jaarkeuze) %>% 
      group_by(Jaar, Parameter) %>% 
      # IF NStot take sum, otherwise mean
      summarise(across(.cols = c(Waarde), 
                       ~if_else(parameter %in% c("NStot"), sum(.x, na.rm = NA_rm), mean(.x, na.rm = NA_rm))),
                .groups = "drop") %>% 
      # Niet + 2 want is al gebeurd in vorige stap.
      mutate(Periode = max(data_graf_bar$Periode))
  }
  
  
  if (parameter == "Tgem") {
    text_plot <- str_c("Gemiddelde temperatuur ", "(\u00B0C)")
  }
  if (parameter == "Tmin") {
    text_plot <- str_c("Minimum temperatuur ", "(\u00B0C)")
  }
  if (parameter == "Tmax") {
    text_plot <- str_c("Maximum temperatuur ", "(\u00B0C)")
  }
  if (parameter == "WSgem") {
    text_plot <- "Gemiddelde windsnelheid (m/s)"
  }
  if (parameter == "NStot") {
    text_plot <- "Totale neerslag (mm)"
  }
  
  
  # Scale_y_continuous
  ymin <- data_graf_bar %>% 
    slice_min(Min, with_ties = FALSE) %>% 
    pull(Min)
  
  ymax <- data_graf_bar %>% 
    slice_max(Max, with_ties = FALSE) %>% 
    pull(Max)
  
  multiplier <- if_else(parameter == "NStot", 100, 5)
  
  y_breaks <- round(ymax / multiplier) * multiplier
  
  # Altijd van 0 beginnen, behalve als kleiner als 0
  ymin_breaks <- if_else(round(ymin / multiplier) * multiplier > 0, 0,  round(ymin / multiplier) * multiplier)
  ymax_breaks <- round(ymax / multiplier) * multiplier
  
  grafiek <- ggplot() +
    geom_linerange(data = data_graf_bar, aes(x = Periode, ymin = Min, ymax = Max, col = Parameter),
                   alpha = alpha, linewidth = 10) +
    geom_line(data = data_graf_lijn, aes(x = Periode, y = Waarde), 
              color = kleur, linewidth = lijndikte) +
    scale_color_manual(values = c(vulkleur,NA)) +
    labs( y = text_plot,  x = "") +
    scale_y_continuous(breaks = seq(ymin_breaks, ymax_breaks, by = multiplier), 
                       limits = c(min(ymin, ymin_breaks), max(ymax, ymax_breaks))) +
    theme(
      legend.position = "none",
      panel.grid.minor.x = element_blank(),
      plot.margin = margin(ruimte,ruimte,ruimte,ruimte,"cm"),
      axis.title.y = element_text(margin = margin(0,0.2,0,0,"cm"))
    ) 
  
  if (labels_x2) {
    
    if (!opdeling_halvemaand) {
      labels <- c("oktober","","december","","februari","")
    } else {
      
      labels <- c("oktober", "", "november", "", "december","",
                  "januari", "", "februari","")
    }
    
  } else {
    rep_label <- if_else(opdeling_halvemaand, 2, 1)
    labels <- c(rep("oktober", rep_label), rep("november", rep_label),
                rep("december", rep_label), rep("januari", rep_label),
                rep("februari", rep_label), rep("maart", rep_label))
  }
  
  max_break <- length(labels)
  if (include_totaal) {
    grafiek <- grafiek +
      geom_point(data = data_graf_bar_punt, aes(x = Periode, y = Waarde), col = kleur, size = puntgrootte) +
      scale_x_continuous(breaks = c(1:max_break, max_break + 2), labels = c(labels,"Totaal"))
  } else {
    grafiek <- grafiek +
      scale_x_continuous(breaks = c(1:max_break), labels = c(labels))
  }
  return(grafiek)
}
