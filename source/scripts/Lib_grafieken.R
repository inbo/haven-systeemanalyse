# Deze library bevat functies uit verschillende library van de hydrologie/chemie of bodem.

OpmaakGrafiek <- function(grafiek, enkel_meetpunt = TRUE, waterpeil = TRUE, 
                          facet = FALSE, boor = FALSE,
                          axis.text.size  = 6, axis.title.size = 7) {
  
  if (boor) {
  
    grafiek <- grafiek +
      theme(
        axis.text = element_text(colour = inbo_steun_donkerroos, size = axis.text.size), 
        axis.title = element_text(size = axis.title.size,face = "bold"),
        # top right bottom left
        plot.margin = unit(c(0,0, 0, 0), "cm"))
    
    return(grafiek)
  }
  
  
  if (!enkel_meetpunt) {
    grafiek <- grafiek +
      theme(legend.position = "right",  # Adjust the legend position
            legend.key.size = unit(0.4, "cm"),  # Size legend-labels
            legend.title = element_text(size = axis.title.size),  # size legend titles
            legend.spacing = unit(0.2, "cm"),  # sapce between different legend
            legend.text = element_text(size = axis.text.size), # legend text size
            # legend.margin = margin(0, 0, 0, 0, unit = "cm"),  # legend bordered above and below legend.
            axis.text = element_text(size = axis.text.size)
      )
  } else {
    grafiek <- grafiek +
      theme(
        # top right bottom left
        # plot.margin = unit(c(0, 0, 0, 0), "cm"),
        axis.text = element_text(size = axis.text.size), 
        axis.title = element_text(size = axis.title.size,face = "bold"))
  }
  
  if (waterpeil) {
    grafiek <- grafiek +
      labs(x = "")
  }
  
  if (facet) {
    grafiek <- grafiek +
      theme(strip.text = element_text(size = axis.title.size, colour= "black"),
            strip.background = element_rect(colour = "black", fill = NA))
  }
  
  grafiek <- grafiek + theme(
    axis.text = element_text(colour = inbo_steun_donkerroos), 
    legend.text = element_text(color = inbo_steun_donkerroos),
    axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.6),
    panel.background = element_rect(fill = NA,
                                    colour = NA,
                                    linewidth = 0.25, linetype = "solid"),
    panel.grid.major = element_line(linewidth  = 0.5, linetype = 'solid',
                                    colour = "gray80"),
    panel.grid.minor = element_line(linewidth  = 0.1, linetype = 'solid',
                                    colour = "gray80"))
    # plot.margin = unit(c(0, 0, 0, 0), "cm"))
  
  return(grafiek)
}




OpmaakKaart <- function(kaart, expand = TRUE) {
  
  kaart <- kaart + 
    theme(legend.position = "right",  # Adjust the legend position
          legend.key.size = unit(0.4, "cm"),  # Size legend-labels
          legend.title = element_text(size = 7),  # size legend titles
          legend.spacing = unit(0.2, "cm"),  # sapce between different legend
          legend.text = element_text(size = 6), # legend text size
          legend.margin = margin(0, 0, 0, 0, unit = "cm"),  # legend bordered above and below legend.
          # top right bottom left
          plot.margin = unit(c(0, 0, 0, 0), "cm")
          )
  
  if (!expand) {
    kaart <- kaart +
      coord_sf(expand = FALSE)
  }
  return(kaart)
}



# Inlezen data ----------------------------------------------------------------------
label_kleur <- inbo_bruinrood
label_kleur_tekst <- "bruine"



watina <- connect_inbo_dbase(database_name = "D0025_00_Watina")  # Open data-set
watina_havens_meetpunt <- GetWatinaMeetpunt(watina, c("AHR", "WAH"))  # Nodig voor locaties
dbDisconnect(watina)  # close dataset


watina_havens_meetpunt_sf <- watina_havens_meetpunt %>% 
  filter(!is.na(.data$X) & !is.na(.data$Y)) %>% 
  st_as_sf(., coords = c("X", "Y"), crs = 31370) %>% 
  mutate(
    Longitude = unlist(map(.$geometry,1)),
    Latitude = unlist(map(.$geometry,2)))

gebieden_sf_totaal <- gebieden_sf %>% 
  group_by(Gebied) %>% 
  dplyr::summarise() %>% 
  ungroup()

st_agr(watina_havens_meetpunt_sf) = "constant"
st_agr(gebieden_sf_totaal) = "constant"

watina_gebied_codes <- st_intersection(watina_havens_meetpunt_sf, gebieden_sf_totaal)

# Wordt ook ingelezen in lib hydrologie
watina <- connect_inbo_dbase(database_name = "D0025_00_Watina")  # Open data-set
watina_havens_peilpuntinfo <- GetWatinaPeilpunten(watina, c("AHR", "WAH"))
dbDisconnect(watina)  # close dataset


# Kaarten locatie peilbuizen ---------------------

MeetpuntenGebied <- function(gebied, selectie_meetpunten = NULL) {
  
  # Geeft een overzicht van de locatie van de peilbuizen.
  watina_gebied <- watina_gebied_codes %>% 
    filter(., .data$Gebied == gebied & str_detect(.data$MeetpuntCode, "P|S")) %>% 
    mutate(MeetpuntType = case_when(
      str_detect(MeetpuntType, "Peilbuis") ~ "Grondwater",
      str_detect(MeetpuntType, "Peilschaal") ~ "Oppervlakte water",
      TRUE ~ MeetpuntType
    )) %>% 
    st_drop_geometry()
  
  if (!is.null(selectie_meetpunten)) {
    watina_gebied <- watina_gebied %>% 
      filter(., MeetpuntCode %in% selectie_meetpunten)
  }
  
  return(watina_gebied)
}



# LocatiePeilbuizen_oud <- function(gebied, gebieden_buffer = 0, 
#                               selectie_meetpunten = NULL, 
#                               interval_m = 1,
#                               min_interval = NULL,
#                               max_interval = NULL) {
#   # Geeft een overzicht van de locatie van de peilbuizen.
#   watina_gebied <- watina_gebied_codes %>% 
#     filter(., .data$Gebied == gebied & str_detect(.data$MeetpuntCode, "P|S")) %>% 
#     mutate(MeetpuntType = case_when(
#       str_detect(MeetpuntType, "Peilbuis") ~ "Grondwater",
#       str_detect(MeetpuntType, "Peilschaal") ~ "Oppervlakte water",
#       TRUE ~ MeetpuntType
#     ))
#     
#   if (!is.null(selectie_meetpunten)) {
#     watina_gebied <- watina_gebied %>% 
#       filter(., MeetpuntCode %in% selectie_meetpunten)
#   }
#   
#   
#   G <- KaartGebiedOverzicht(gebied, gebieden_buffer = 0, 
#                             interval_m = interval_m,
#                             min_interval = min_interval,
#                             max_interval = max_interval,
#                             schelde_dokken = FALSE)$Kaart +
#     new_scale_fill() + 
#     geom_sf(data = watina_gebied, aes(fill = MeetpuntType), color = "black", pch = 21, size = 3) +
#     scale_fill_manual(values = c("Grondwater" = inbo_donkerblauw, "Oppervlakte water" = inbo_rood)) +
#     geom_sf_label_repel(data = watina_gebied, aes(label = MeetpuntCode), alpha = 0.9, col = 1) +
#     labs(x = "", y = "") +
#     guides(fill = guide_legend(order = 2))
#   
#   Onderschrift <- str_c("Locatie van de meetpunten in ", gebied, ".")
#   
#   return(list(Graf = G, Bijschrift = Onderschrift))
# }


LocatiePeilbuizen <- function(gebied, kaart, 
                              selectie_meetpunten = NULL) {
  
  # Geeft een overzicht van de locatie van de peilbuizen.
  watina_gebied <- watina_gebied_codes %>% 
    filter(., .data$Gebied == gebied & str_detect(.data$MeetpuntCode, "P|S")) %>% 
    mutate(MeetpuntType = case_when(
      str_detect(MeetpuntType, "Peilbuis") ~ "Grondwater",
      str_detect(MeetpuntType, "Peilschaal") ~ "Oppervlakte water",
      TRUE ~ MeetpuntType
    )) %>% 
    mutate(MeetpuntCode = str_replace(MeetpuntCode, "WAH", ""))
  
  if (!is.null(selectie_meetpunten)) {
    watina_gebied <- watina_gebied %>% 
      filter(., MeetpuntCode %in% selectie_meetpunten) %>% 
      mutate(MeetpuntCode = str_replace(MeetpuntCode, "WAH", ""))
  }
  
  
  G <- kaart +
    new_scale_fill() + 
    geom_sf(data = watina_gebied, aes(fill = MeetpuntType), color = "black", pch = 21, size = 2.5,  alpha = 0.8) +
    scale_fill_manual(values = c("Grondwater" = inbo_lichtblauw, "Oppervlakte water" = inbo_felrood)) +
    geom_sf_label_repel(data = watina_gebied, aes(label = MeetpuntCode), alpha = 0.9, col = 1, size = 2) +
    labs(x = "", y = "") +
    guides(fill = guide_legend(order = 2))
  
  
  Onderschrift <- str_c("Locatie van de meetpunten in ", gebied, ".")
  
  return(list(Kaart = G, Bijschrift = Onderschrift))
}





GroepenMeetpunten <- function(gebied, buffer_m = 2, selectie_meetpunten = NULL) {
  # Creër Groepen voor de meetpunten per type. Enkel Peilschaal en Peilbuizen.
  # De groepscodes hebben opeenvolgende nummering.
  # Groepen worden aangemaakt per meetpunt. Als een meetpunt van een groep een extra meetpunt heeft die niet opgenomen is door het eerste meetpunt, dan wordt de extra meetpunt in die groep gestoken. 
  # Punten die uitmalej  vna een andere intersectie, die op hun beurt puntne hebben die niet zitten in eerste intersectie dienen ook opgenomen te worden.
  
  watina_gebied <- watina_gebied_codes %>% 
    filter(., .data$Gebied == gebied & str_detect(.data$MeetpuntCode, "P|S")) %>% 
    st_buffer(., dist = buffer_m) %>% 
    mutate(Groep = NA)
  
  if (!is.null(selectie_meetpunten)) {
    watina_gebied <- watina_gebied %>% 
      filter(.data$MeetpuntCode %in% selectie_meetpunten)
  }
  
  groep <- 1
  gebied_groepscode <- data.frame()
  
  for (type in unique(watina_gebied$MeetpuntType)) {
    subset_code <- subset(watina_gebied, MeetpuntType == type)
    
    for (i in seq_len(nrow(subset_code))) {
      code <- subset_code[i, ] %>% 
        select(MeetpuntCode, geometry)
      
      st_agr(subset_code) = "constant"
      st_agr(code) = "constant"
      intersected_polygons <- st_intersection(subset_code, code)
      

      # Als er al een andere groep van bestaat, voeg ze allemaal samen
      if (!all(is.na(intersected_polygons$Groep))) {
        groep <- intersected_polygons %>% 
          filter(!is.na(Groep)) %>% 
          distinct(Groep) %>% 
          pull()
      }
      
      subset_code <- subset_code %>% 
        mutate(Groep = case_when(
          MeetpuntCode %in% pull(intersected_polygons, MeetpuntCode) & is.na(Groep) ~ groep,
          TRUE ~ Groep
        ))
      
      # Fix opeenvolgende nummering.
      max_groep <- subset_code %>% 
        st_drop_geometry() %>% 
        summarise(max = max(Groep, na.rm = TRUE)) %>% 
        pull()
      
      groep <- max_groep + 1
    }
    subset_code <- subset_code %>% 
      st_drop_geometry()
    
    gebied_groepscode <- bind_rows(gebied_groepscode, subset_code)
  }
  
  gebied_groepscode <- gebied_groepscode %>% 
    st_as_sf(., coords = c("Longitude", "Latitude"), crs = 31370) %>% 
    arrange(Groep) %>% 
    dplyr::select(MeetpuntCode, MeetpuntType, Groep)
  
  return(gebied_groepscode)
}


MeetpuntDiepteFilter <- function(Meetpunten) {
  
  meetpunten_diepte <- watina_havens_peilpuntinfo %>% 
    filter(MeetpuntCode %in% Meetpunten) %>% 
    # Make infinitive NA.
    mutate(Filterbodem_mTAW = PeilpuntHoogteBovenMaaiveld  - PeilpuntLengteBuis,
           Filtertop_mTAW =  PeilpuntHoogteBovenMaaiveld  - PeilpuntLengteBuis + PeilpuntLengteFilter) %>% 
    select(MeetpuntCode, Filterbodem_mTAW, Filtertop_mTAW) %>% 
    # Als meerdere peilpunten aanwezig voor zelfde meetpunt, neem het minimum.
    group_by(MeetpuntCode) %>% 
    mutate(across(c(Filterbodem_mTAW, Filtertop_mTAW), ~min(.x, na.rm = TRUE))) %>% 
    ungroup() %>% 
    distinct() %>% 
    # Nummer peilbuizen volgens diepte
    arrange(desc(Filterbodem_mTAW)) %>% 
    mutate(Diepte = row_number())
  
  return(meetpunten_diepte)
}



LocatiePeilbuizenKaartTabel <- function(gebied, kaart,
                                        buffer_m = 2,
                                        selectie_meetpunten = NULL, 
                                        info_meetpunten = TRUE,
                                        code = TRUE) {
  # Maakt een kaart & tabel van de gegroepeerde meetpunten.
  # De codes worden gegroepeerd op basis van Type en afstand.
  # Mogelijkheid om enkel op voorhand een selectie door te geven van de gewesnte meetpunten.
  # Optie om gedetailleerde info over de meetpunten op te nemen in tabel.
  # Plot de Meetpuntcode van de ondiepe buis (code = TRUE) of gebruik de groepscode (centroid van degroep).
  #
  # Voor verschillende versies van een peilbuis -> De gemiddelde van de meetpuntpeilbuizen is genomen voor Hoogte maaiveld
  
  # Groepsnummering
  Meetpunten_groepen <- GroepenMeetpunten(gebied = gebied, buffer_m = buffer_m, selectie_meetpunten = selectie_meetpunten)
  
  # Voeg dieptenummering en filterdiepte toe per groep
  meetpunten_diepte <- data.frame()
  for (groep in unique(Meetpunten_groepen$Groep)) {
    
    df_Meetpunten <- Meetpunten_groepen %>% 
      filter(Groep == groep) %>% 
      pull(MeetpuntCode) %>% 
      MeetpuntDiepteFilter(.)
    
    meetpunten_diepte <- bind_rows(meetpunten_diepte, df_Meetpunten)
  }
  
  Meetpunten_groepen <- Meetpunten_groepen %>% 
    left_join(., meetpunten_diepte, by = c("MeetpuntCode")) %>% 
    # Make Infinitive NA
  mutate(across(.cols = c(Filtertop_mTAW, Filterbodem_mTAW), ~ if_else( is.infinite(.x), NA, .x)))
  
  # Plot code of groepscode
  if (code) {
    groep_meetpunten_code <- Meetpunten_groepen %>% 
      filter(Diepte == 1) %>% 
      mutate(MeetpuntType = case_when(
        str_detect(MeetpuntType, "Peilbuis") ~ "Grondwater",
        str_detect(MeetpuntType, "Peilschaal") ~ "Oppervlakte water",
        TRUE ~ MeetpuntType)) %>% 
      mutate(MeetpuntCode = str_replace(MeetpuntCode, "WAH", ""))
  } else {
    groep_meetpunten_code <- Meetpunten_groepen %>% 
      group_by(MeetpuntType , Groep) %>% 
      summarise(geometry = st_union(geometry), .groups = "keep") %>% 
      summarise(geometry = st_centroid(geometry),.groups = "drop") %>% 
      mutate(MeetpuntType = case_when(
        str_detect(MeetpuntType, "Peilbuis") ~ "Grondwater",
        str_detect(MeetpuntType, "Peilschaal") ~ "Oppervlakte water",
        TRUE ~ MeetpuntType
      )) %>% 
      rename("MeetpuntCode" = Groep)
  }
  
  G <- kaart +
    new_scale_fill() + 
    geom_sf(data = groep_meetpunten_code, aes(fill = MeetpuntType), color = "black", pch = 21, size = 2.5) +
    scale_fill_manual(values = c("Grondwater" = inbo_lichtblauw, "Oppervlakte water" = inbo_felrood)) +
    ggsflabel::geom_sf_label_repel(data = groep_meetpunten_code, aes(label = MeetpuntCode), alpha = 0.8, col = 1, size = 2, nudge_x = 0, nudge_y = 0) + 
    labs(x = "", y = "") +
    guides(fill = guide_legend(order = 2))

  
  tabel <- watina_havens_peilpuntinfo  %>% 
    rename(MeetpuntType = PeilpuntType) %>% 
    mutate(MeetpuntType = case_when(
      str_detect(MeetpuntType, "piëzometer|peilbuis") ~ "Peilbuis/Piëzometer",
      str_detect(MeetpuntType, "opp. water") ~ "Oppervlakte water",
      str_detect(MeetpuntType, "peilschaal") ~ "Peilschaal",
      TRUE ~ MeetpuntType
    )) %>% 
    filter(., watina_havens_peilpuntinfo$MeetpuntCode %in% pull(Meetpunten_groepen, MeetpuntCode)) %>% 
    left_join(., Meetpunten_groepen %>%  st_drop_geometry(), by = c("MeetpuntCode", "MeetpuntType")) %>% 
    # Groepeer voor kwelgroep zodat ondiepe eerst staat.
    group_by(Groep) %>% 
    arrange(Groep, Diepte) %>% 
    select(Groep, MeetpuntCode, MeetpuntType, PeilpuntTAWMaaiveld, Filterbodem_mTAW, Filtertop_mTAW) %>% 
    # Fill missende waarde van TAW maaiveld in 
    group_by(MeetpuntCode) %>% 
    fill(PeilpuntTAWMaaiveld, .direction = "downup") %>% 
    mutate(PeilpuntTAWMaaiveld = mean(PeilpuntTAWMaaiveld)) %>% 
    ungroup() %>% 
    distinct() %>% 
    rename("Hoogte maaiveld (mTAW)" = PeilpuntTAWMaaiveld,
           "Filtertop (mTAW)" = Filtertop_mTAW,
           "Filterbodem (mTAW)" = Filterbodem_mTAW) %>% 
    mutate(MeetpuntType = case_when(
      str_detect(MeetpuntType, "Peilbuis") ~ "Grondwater",
      str_detect(MeetpuntType, "Peilschaal") ~ "Oppervlakte water",
      TRUE ~ MeetpuntType
    ))
    

  # Not info needed, only smaller selection
  if (!info_meetpunten) { tabel <- tabel %>% select(Groep, MeetpuntCode, MeetpuntType)} 
  
  tabel <- tabel %>% 
    flextable() %>% 
    flextable::align(., align = "center", part = "body") %>% 
    merge_v(j = c("Groep")) %>% 
    set_table_properties(., layout = "autofit") %>% 
    hline(border = fp_border(width = 0.5)) 
  
  # Kwel aanwezig voor onderschrift.
  aantal_kwel <-
    Meetpunten_groepen %>% 
    st_drop_geometry() %>% 
    group_by(Groep) %>% 
    summarise(Aantal = n(), .groups = "drop") 
  
  kwel <- any(aantal_kwel$Aantal > 1)
  
  
  bijschrift_add <- ""
  if (kwel & code) { bijschrift_add <- " Per kwelgroep is enkel het ondiepe meetpunt weergegeven."}
  
  Onderschrift_kaart <- str_c("Locatie van de meetpunten in ", gebied, " per groep.", bijschrift_add)
  
  Onderschrift_tabel <- str_c("Overzicht van de de meetpunten in ", gebied, " per groep.")
  
  
  return(list(Kaart = G, Tabel = tabel, Bijschrift_Kaart = Onderschrift_kaart, Bijschrift_Tabel = Onderschrift_tabel))
}


GetDatum <- function(periode) {
  
  dag <- periode %>% 
    str_sub(., 1, 2) %>% 
    str_replace(., "^[0]", "")
  
  maand <- periode %>% 
    str_sub(., 4, 5) %>% 
    str_replace(., "^[0]", "")
  
  maand_voluit <- case_when(maand == "1" ~ "januari",
                            maand == "2" ~ "februari",
                            maand == "3" ~ "maart",
                            maand == "4" ~  "april",
                            maand == "5" ~ "mei",
                            maand == "6" ~ "juni",
                            maand == "7" ~ "juli",
                            maand == "8" ~ "augustus",
                            maand == "9" ~ "september",
                            maand == "10" ~ "oktober",
                            maand == "11" ~ "november",
                            maand == "12" ~ "december")
  
  return(str_c(dag, " ", maand_voluit))
}


OnderschriftPeil <- function(meetpunt, droogval, periode_begin, periode_einde, aangepaste_periode, titel = "Peil") {
  
  if (str_detect(meetpunt, 'S')) {
    type <- titel
  } else if (str_detect(meetpunt, 'P')) {
    type <- "Waterpeil"
  }
  
  if (aangepaste_periode) {
    periode <- str_c(GetDatum(periode_begin), " - ", GetDatum(periode_einde))
  } else {
    periode <- str_c("het hydrologisch jaar")
  }
  
  Onderschrift <- str_c(type, " voor ", meetpunt, " voor ", periode,".")
  if (droogval) {
    
    Onderschrift <- str_c(Onderschrift, " De bruine kruisen zijn datums met droogval.")
    
  }
  
  return(Onderschrift)
}



OnderschriftOpbolling <- function(maaiveldhoogte ,Peilbuis, Peilschaal, maaiveld, droogval, boorprofiel, aangepaste_periode) {
  
  if (maaiveld) {
    onder_m <- ' m t.o.v. maaiveld'
  } else {
    onder_m <- ' mTAW'
  }
  if (boorprofiel) {
    onder_b <- str_c(" De bodembeschrijving is een boring ter hoogte van ",  Peilbuis, ".")
  } else {
    onder_b <- ""
  }
  
  if (droogval) {
    onder_dv <- str_c(" De bruine kruisen zijn datums met droogval.")
  } else {
    onder_dv <- "" 
  }
  
  if (aangepaste_periode) {

    periode <- str_c(GetDatum(periode_begin), " - ", GetDatum(periode_einde), ".")
  } else {
    periode <- str_c("het hydrologisch jaar.")

  }
  
  
  Onderschrift <- str_c("Opbolling van het bodempeil ",Peilbuis,  
                        " tegenover het oppervlakte-peil  ", Peilschaal, " in", onder_m, 
                        " voor ", periode , " ",maaiveldhoogte ,". ",  onder_dv, onder_b)
  return(Onderschrift)
}



#  Grafieken peilen ------
GrafiekBodemProfiel <- function(peilpuntcodes, max_diepte = NULL) {
  
  # Geeft de bodembeschrijving weer op basis van de boringen voor ingegeven peilpunten.
  # Dit is steeds uitgedrukt in Diepte tegenover maaiveld.
  # De peilbuizen worden geörderend van diepst (rechts) naar ondiepst (links) tegenover de bodembeschrijving.
  #  Houdt er rekening mee dat het waterpeil lager kan liggen als het bodemprofiel;
  
  # Krijg informatie over 
  info_filter <- watina_havens_peilpuntinfo %>% 
    filter(MeetpuntCode %in% peilpuntcodes) %>% 
    filter(!is.na(PeilpuntLengteBuis)) %>% 
    mutate(
      filter_bodem = PeilpuntHoogteBovenMaaiveld - PeilpuntLengteBuis,
      filter_top = PeilpuntHoogteBovenMaaiveld - PeilpuntLengteBuis + PeilpuntLengteFilter) %>% 
    arrange(filter_bodem)
  
  
  GB_data <- boringen_df %>% 
    filter(peilpunt %in% peilpuntcodes)
  
  meetpunten <- info_filter %>% 
    distinct(MeetpuntCode) %>%  
    pull()
  
  aantal <- info_filter %>% 
    distinct(MeetpuntCode) %>%  
    pull() %>% 
    length()
  
  
  info_filter <- info_filter %>% 
    filter(MeetpuntCode %in% meetpunten)
  
  
  if (is.null(max_diepte)) {
    max_diepte <- RoundAny(info_filter[1, "filter_bodem"], 0.1, floor)
  } else {
    max_diepte <- max_diepte
  }
  
  if (aantal == 1) {
    
    G <- Boorprofiel.PB1(GB_data, filter.top = info_filter[1, "filter_top"], filter.bodem = info_filter[1, "filter_bodem"], MaxDiepte = max_diepte)
    
  } else if (aantal == 2) {
    
    G <- Boorprofiel.PB2(GB_data, 
                         filter1.top = info_filter[1, "filter_top"], filter1.bodem = info_filter[1, "filter_bodem"],
                         filter2.top = info_filter[2, "filter_top"], filter2.bodem = info_filter[2, "filter_bodem"], 
                         MaxDiepte = max_diepte)
    
    
    
  } else if (aantal == 3) {
    
    G <- Boorprofiel.PB3(GB_data, 
                         filter1.top = info_filter[1, "filter_top"], filter1.bodem = info_filter[1, "filter_bodem"],
                         filter2.top = info_filter[2, "filter_top"], filter2.bodem = info_filter[2, "filter_bodem"],
                         filter3.top = info_filter[3, "filter_top"], filter3.bodem = info_filter[3, "filter_bodem"],
                         MaxDiepte = max_diepte)
  }
  
  
  return(list(G = G, Data = GB_data, MaxDiepte = max_diepte))
}


GrafiekPeilverloop <- function(tijdreeks_peilverloop, 
                               aangepaste_periode = FALSE,
                               periode_begin = "01-01", periode_einde = "31-12",
                               jaar = NULL, facet = TRUE,
                               maaiveld = FALSE, label = TRUE,
                               titel = "Peil") {
  # get the timeseries in graphs for the differenct years. This function only creates the graphs. The data is from getPeilverloop() !
  # Uses facet_wrap. It always statrts from the first month for each year so all the scales the x-axes are starting from te same (was quitte hard to figure this out).
  # aangepaste_periode is FALSE
  # periode_begin en einde enkel voor aangepaste periode zodat scale en label enkel die maanden weergeeft.
  # maaiveld, only needed to get correct y-axis label.
  # Jaar dient opgenomen te worden in GetPeilverloop() !
  # label = TRUE, if water was below watertable for a certain observation, this observation gets a point-label in the graph.
  
  # if all years
  jaar_label <- "6 month"
  # Periode opbolling
  if (!is.null(jaar)) {
    jaar_label <- "2 month"
    if (!aangepaste_periode) {
      tijdreeks_peilverloop <- dplyr::filter(tijdreeks_peilverloop, .data$HydrologischJaar %in% jaar)
    } else {
      tijdreeks_peilverloop <- dplyr::filter(tijdreeks_peilverloop, .data$Jaar %in% jaar)
    }
  }

  if (facet) {
    
    if (aangepaste_periode) {
      
      G <- ggplot(data = tijdreeks_peilverloop) +
        geom_line(aes(x = Periode, y = PeilmetingTAW,  col  = MeetpuntCode)) +
        facet_row(~ Jaar, scales = "free_x") +
        scale_x_date(
          date_labels = "%b",
          breaks = seq(as.Date(str_c("2020", "-" , periode_begin), format = "%Y-%d-%m"),
                       as.Date(str_c("2021", "-", periode_einde), format = "%Y-%d-%m"),
                       by = "2 months"),
          limits = as.Date(c(
            str_c("2020", "-", periode_begin), 
            str_c("2020", "-", periode_einde)), format = "%Y-%d-%m"),
          date_minor_breaks = "1 month")
      
    } else {
      # Hydrologisch jaar
      ## Hier moet periode ook van 2019 en 2020 zijn zoals berekend in GetPeilverloop().
      ## Dit omdat de labels niet deftig geplaats kunnen worden gezien als je 1 jaar neemt (bvb 2020), gaat R automatisch uit van een logische volgorde.
      ## Dus maart na april lukt niet gezien onlogisch. De stalen van april tot decemebr zijn in 2019 (geen schrikeljaar), 
      ## en januari tot maart in 2020 (wel schrikeljaar zodat een observatie op 29/02 wel mogelijk is.)
      
      G <- ggplot(data = tijdreeks_peilverloop) +
        geom_line(aes(x = Periode, y = PeilmetingTAW, col = MeetpuntCode)) +
        facet_row(~ HydrologischJaar, scales = "free_x") +
        scale_x_date(
          date_labels = "%b",
          breaks = seq(as.Date(str_c("2019", "-" , "01-04"), format = "%Y-%d-%m"),
                       as.Date(str_c("2020", "-", "31-12"), format = "%Y-%d-%m"),
                       by = "2 month"),
          limits = c(as.Date(str_c(2019, "-", "04", "-01")), as.Date(str_c(2020, "-", "03-31"))),
                     date_minor_breaks = "1 month")
    }
    
    # Add point label if observation is NA due below watertable.
    if (label) {
      G <- G +
        geom_label(aes(label = Label, x = Periode, y = min(PeilmetingTAW, na.rm = TRUE)),
                   fill = 'transparent', color = label_kleur, show.legend = FALSE, label.size = 0)
    }
    
  } else {
    
    G <- ggplot(data = tijdreeks_peilverloop) +
      geom_line(aes(x = PeilmetingDatum, y = PeilmetingTAW, col = MeetpuntCode))
    
    if (aangepaste_periode) {
      G <- G +
        scale_x_date(date_labels = "%m-%Y", 
                     limits = c(as.Date(str_c(min(tijdreeks_peilverloop$Jaar), "-" , periode_begin), format = "%Y-%d-%m"),
                                as.Date(str_c(max(tijdreeks_peilverloop$Jaar) + 1, "-", periode_begin), format = "%Y-%d-%m")),
                     breaks = seq(as.Date(str_c(min(tijdreeks_peilverloop$Jaar), "-" , periode_begin), format = "%Y-%d-%m"),
                                  as.Date(str_c(max(tijdreeks_peilverloop$Jaar) + 1, "-", periode_begin), format = "%Y-%d-%m"),
                                  by = jaar_label),
                     date_minor_breaks =  "1 month")
    } else {
      G <- G +
        scale_x_date(date_labels = "%m-%Y", 
                     limits = c(as.Date(str_c(min(tijdreeks_peilverloop$HydrologischJaar), "-" , "01-04"), format = "%Y-%d-%m"),
                                as.Date(str_c(max(tijdreeks_peilverloop$HydrologischJaar) + 1, "-", "31-03"), format = "%Y-%d-%m")),
                     breaks = seq(as.Date(str_c(min(tijdreeks_peilverloop$HydrologischJaar), "-" , "01-04"), format = "%Y-%d-%m"),
                                  as.Date(str_c(max(tijdreeks_peilverloop$HydrologischJaar) + 1, "-", "31-12"), format = "%Y-%d-%m"),
                                  by = jaar_label),
                     date_minor_breaks =  "1 month")
    }

    if (label) {
      G <- G +
        geom_label(aes(label = Label, x = PeilmetingDatum, y = min(PeilmetingTAW, na.rm = TRUE) + 0.01),
                   fill = 'transparent', color = label_kleur, show.legend = FALSE, label.size = 0)
    }
  }

  
  if (length(unique(tijdreeks_peilverloop$MeetpuntCode)) == 1) {
    G <- G + 
      theme(legend.position = "none")
  }
  
  G <- G +
    labs(x = "") +
    scale_y_continuous(limits = c(min(RoundAny(tijdreeks_peilverloop$PeilmetingTAW, 0.1, floor), na.rm = TRUE),
                                  max(RoundAny(tijdreeks_peilverloop$PeilmetingTAW, 0.1, ceiling), na.rm = TRUE)),
                       breaks = seq(min(RoundAny(tijdreeks_peilverloop$PeilmetingTAW, 0.1, floor), na.rm = TRUE),
                                    max(RoundAny(tijdreeks_peilverloop$PeilmetingTAW, 0.1, ceiling), na.rm = TRUE),
                                    by = 0.2),
                       minor_breaks = waiver())

  # Add correct Y-axis label  
  if (maaiveld) {
    G <- G + 
      labs(y = "peil (m t.o.v. maaiveld)", x = "") 
  } else {
    G <- G + 
      labs(y = "Peil (mTAW)", x = "") 
    
  }
  
  meetpunt <-  tijdreeks_peilverloop %>% distinct(MeetpuntCode) %>% pull()
  droogval <- any(!is.na(tijdreeks_peilverloop$Label) & tijdreeks_peilverloop$Label == "x")
  
  
  Onderschrift <- OnderschriftPeil(meetpunt, aangepaste_periode = aangepaste_periode,
                                   periode_begin = periode_begin, periode_einde = periode_einde,
                                   droogval, titel = titel)
  
  
  return(list(Graf = G, Bijschrift = Onderschrift))
}

# Kreekpeil in periode die je gebruikt in de periode die niet boven schothoogte komt. In DPN: 2m75 mTAW, 
# in Putten Weiden op 1.20 mTAW , Putten west is 1m80 mTAW
# Groot Rietveld -> Geen rekening mee houden.


GrafiekNeerslagoverschot <- function(neerslagoverschot_df, periode_begin = "01-04", periode_zomermin = "01-08") {
  
  # Behoud enkel neerslagoverschot kleiner of gelijk aan 0.
  data_subset <- subset(neerslagoverschot_df, Neerslagoverschot  <= 0)                              
  
  G <- ggplot()
  
  if (nrow(data_subset) != 0) {
    
    G <- ggplot(data = data_subset, aes(Neerslagoverschot , Delta)) +
      geom_point(size = 3, color = "blue") + 
      geom_smooth(method = "lm", formula = "y ~ x") +
      labs(x = "Neerslag overschot (mm)", y = "Peildaling (m)") +
      scale_x_continuous(breaks = seq(RoundAny(min(data_subset$Neerslagoverschot), 10, floor),
                                      RoundAny(max(data_subset$Neerslagoverschot), 10, ceiling),
                                      by = 20)) +
      scale_y_continuous(breaks = seq(RoundAny(min(data_subset$Delta), 0.1, floor),
                                      RoundAny(max(data_subset$Delta), 0.1, ceiling),
                                      by = 0.1))
  }

  Onderschrift <- str_c("Neerslagoverschot in functie van peildaling. De peildaling is het verschil tussen de de maximale waarde tussen ", 
                        GetDatum(periode_begin), " en 1 juni, en de minimale waarde rond ", GetDatum(periode_zomermin), 
                        ". Jaren met droogval werden niet opgenomen.")
  
  
  return(list(Graf = G, Bijschrift = Onderschrift))
}
  

# Kwel -----------------------

OnderschriftKwel <- function(gegevens_diepte, maaiveld, droogval, boorprofiel, aangepaste_periode, periode_begin, periode_einde, controle_boring) {
  # Subfunctie van GrafiekKwel()
  if (maaiveld) {
    onder_m <- ' m t.o.v. maaiveld'
  } else {
    onder_m <- ' mTAW'
  }
  if (boorprofiel &  controle_boring) {
    onder_b <- str_c(" De bodembeschrijving is een boring ter hoogte van de kwelpunten.")
  } else if (boorprofiel & !controle_boring) {
    onder_b <- str_c(" Geen bodembeschrijving beschikbaar.")
  } else {
    onder_b <- ""
  }
  
  if (droogval) {
    onder_dv <- str_c(" De bruine kruisen zijn datums met droogval.")
  } else {
    onder_dv <- "" 
  }
  
  if (aangepaste_periode) {
    periode <- str_c(GetDatum(periode_begin), " - ", GetDatum(periode_einde), ".")
  } else {
    periode <- str_c("het hydrologisch jaar.")
  }
 
  gegevens_diepte <- gegevens_diepte %>% 
    mutate(
      tekst = case_when(
        Diepte == 1 ~ "ondiepe filter",
        Diepte == 2 & nrow(gegevens_diepte) == 2 ~ "diepe filter",
        Diepte == 2 & nrow(gegevens_diepte) != 2 ~ "middelste filter",
        Diepte == 3 ~ "diepe filter",
        TRUE ~ "filter te benoemen."),
      tekst_kleur = case_when(
        tekst == "ondiepe filter" ~ "blauw",
        tekst == "diepe filter" ~ "rood",
        tekst == "middelste filter" ~ "oranje",
        TRUE ~ "te benoemen kleur")
      )
  
  tekst_meetpunten <- ""
  for (i in seq_len(nrow(gegevens_diepte))) {
    
    if (i != nrow(gegevens_diepte)) {
      tekst_meetpunten <- str_c(tekst_meetpunten, str_c(gegevens_diepte[i, "tekst"], " (", gegevens_diepte[i, "MeetpuntCode"],"-", gegevens_diepte[i, "tekst_kleur"] ,"), "))
    } else {
      tekst_meetpunten <- str_c(tekst_meetpunten, str_c(gegevens_diepte[i, "tekst"], " (", gegevens_diepte[i, "MeetpuntCode"],"-", gegevens_diepte[i, "tekst_kleur"] ,")."))
    }

  }
  
  Onderschrift <- str_c("Het grondwaterpeil van de kwelpunten in", onder_m, 
                        " voor ", periode , " Legende: " ,tekst_meetpunten, onder_dv, onder_b)
  return(Onderschrift)
}




GrafiekKwelpunt <- function(kwelpunt_data, # result van DataKwelpunt()
                            aangepaste_periode, # Booleaanse parameter. Indien False: hydrologisch jaar. Indien TRUE, periode begin en einde in te geven (zelfde jaar.)
                            facet = TRUE, # Jaren in facet of niet.
                            periode_begin = "01-01",  # Zie aangepaste periode
                            periode_einde = "01-08", # Zie aangepaste periode
                            jaar = NULL, # vector met jaren. jaren worden hier wel gefilterd. Ook een rescaling van de X-axis. 
                            # Als er op jaar gefilterd dient te worden, in functie DataKwelpunt()
                            maaiveld = TRUE,  # Booleaans. Indien FALSE -> mTAW (label)
                            boorprofiel = TRUE,  # boorprofiel ook in returnlijst als er een boorprofiel is voor de ingegeven meetpunten
                            max_tijdspanne = 60, # meer als x dagen geen meting, dan wordt de lijn van de tijdreeks onderbroken
                            ingegeven = FALSE, # Booleaans. Als TRUE zowel gevalideerde als ingegeven data. Anders enkel de gevalideerde data
                            label = TRUE, # Plot label van droogval als er droogval is.
                            legende = FALSE # Plot de legende met meetpuntcodes
                            ){
  # Geeft het peilerloop weer. Onderbreekt de lijn als er meer als 60 dagen tussen zit.
  
  gegevens <- kwelpunt_data %>% 
    arrange(MeetpuntCode)
  
  # Voeg NA toe tussen de observaties van de tijdreeksen als er meer tijd tussenzit als de ingegeven max_tijdspanne zodat de lijn onderbroken wordt. Dit gebeurt in de functie GetPeilverloop
  gegevens_peilverloop <- data.frame()
  for (code in unique(gegevens$MeetpuntCode)) {
    subet_tijdreeks <- gegevens %>% 
      filter(.data$MeetpuntCode == code) %>% 
      GetPeilVerloop(., aangepaste_periode = aangepaste_periode,  
                     periode_begin = periode_begin,  
                     periode_einde = periode_einde,
                     jaar = jaar, 
                     max_tijdspanne = max_tijdspanne, 
                     ingegeven = ingegeven) 
    
    gegevens_peilverloop <- bind_rows(gegevens_peilverloop, subet_tijdreeks)
  }
  

  # Meetpunten juiste kleur geven afhankelijk van diepte.
  gegevens_diepte <- gegevens_peilverloop %>% 
    select(MeetpuntCode, Diepte_filter_mTAW, Diepte) %>% 
    distinct() %>%
    # Door toevoeging NA tussen observaties met meer als 60 dagen verschil is er NA. Deze wegfilteren
    filter(!is.na(.data$Diepte)) %>% 
    mutate(Kleur = case_when(
      Diepte == min(Diepte) ~ inbo_donkerblauw,
      Diepte == max(Diepte) ~ inbo_rood,
      TRUE ~ inbo_oranje
    )) %>% 
    arrange(MeetpuntCode)
  
  # Hier worden de kleuren toegekend
  variables <- gegevens_diepte %>% 
    pull(MeetpuntCode)
  colors_variables <- gegevens_diepte %>% 
    pull(Kleur)
  
  GB_data <- NULL
  GB_data$G <- NULL
  # Controle of er boorbeschrijving aanwezig is.
  controle_boring <- ControleBoring(variables)  # Komt er een van de meetpunten voor in de boorbeschrijvingen
  
  if (boorprofiel & controle_boring) {
    # Krijg min en max waarde voor bodemprofiel van relevante meetpunten. HIER.
    GB_data <- GrafiekBodemProfiel(pull(distinct(gegevens_diepte, MeetpuntCode)))
    
    # Diepste punt is bodemfilter
    max_diepte  <- GB_data$MaxDiepte
    
    max_mTAW <- gegevens_peilverloop %>% 
      summarise(min = min(PeilmetingTAW, na.rm = TRUE)) %>% 
      pull() %>% 
      RoundAny(., 0.1, floor)
    
    if (max_mTAW < max_diepte) {
      max_diepte <- max_mTAW 
      
      GB_data <- GrafiekBodemProfiel(pull(distinct(gegevens_peilverloop, MeetpuntCode)), max_diepte = max_diepte)
    }
    
    Data_GB <- GB_data$Data %>% 
      mutate(van = - van,
             tot = -tot)
    
  }
  
  # if all years
  jaar_label <- "6 month"
  # Periode kwelpunten
  if (!is.null(jaar)) {
    jaar_label = "2 month"
    # if (!aangepaste_periode) {
    #   gegevens <- dplyr::filter(gegevens_peilverloop, .data$HydrologischJaar %in% jaar)
    # } else {
    #   gegevens <- dplyr::filter(gegevens_peilverloop, .data$Jaar %in% jaar)
    # }
  }
  
  if (facet) {
    
    if (aangepaste_periode) {
      G <- ggplot(data = gegevens_peilverloop) +
        geom_line(aes(x = Periode, y = PeilmetingTAW, col = MeetpuntCode)) +
        scale_color_manual(values = colors_variables, breaks = variables, labels = variables) + 
        facet_wrap(~ Jaar, scales = "free_x") +
        scale_x_date(
          date_labels = "%b",
          breaks = seq(as.Date(str_c("2020", "-" , periode_begin), format = "%Y-%d-%m"),
                       as.Date(str_c("2021", "-", periode_einde), format = "%Y-%d-%m"),
                       by = "2 months"),
          limits = as.Date(c(
            str_c("2020", "-", periode_begin), 
            str_c("2020", "-", periode_einde)), format = "%Y-%d-%m"),
          date_minor_breaks = "1 month")
      
    } else {
      G <- ggplot(data = gegevens_peilverloop) +
        geom_line(aes(x = Periode, y = PeilmetingTAW, col = MeetpuntCode)) +
        scale_color_manual(values = colors_variables, breaks = variables, labels = variables) + 
        facet_wrap(~ HydrologischJaar, scales = "free_x") +
        scale_x_date(date_labels = "%b",
                     breaks = seq(as.Date(str_c("2019", "-" , "01-04"), format = "%Y-%d-%m"),
                                  as.Date(str_c("2020", "-", "31-12"), format = "%Y-%d-%m"),
                                  by = "2 month"),
                     limits = c(as.Date(str_c(2019, "-", "04", "-01")), as.Date(str_c(2020, "-", "03-31"))),
                     date_minor_breaks = "1 month")
    }
    
    if (label) {
      G <- G +
        geom_label(aes(label = Label, x = Periode, y = min(PeilmetingTAW, na.rm = TRUE) - 0.01),
                   fill = 'transparent', color = label_kleur, show.legend = FALSE, label.size = 0)
    }
    
  } else {

    G <- ggplot(data = gegevens_peilverloop) +
      geom_line(aes(x = PeilmetingDatum, y = PeilmetingTAW, col = MeetpuntCode)) +
      scale_color_manual(values = colors_variables, breaks = variables, labels = variables)
    
    
    if (aangepaste_periode) {
      G <- G +
        scale_x_date(date_labels = "%m-%Y", 
                     limits = c(as.Date(str_c(min(gegevens_peilverloop$Jaar), "-" , periode_begin), format = "%Y-%d-%m"),
                                as.Date(str_c(max(gegevens_peilverloop$Jaar), "-", periode_einde), format = "%Y-%d-%m")),
                     breaks = seq(as.Date(str_c(min(gegevens_peilverloop$Jaar), "-" , periode_begin), format = "%Y-%d-%m"),
                                  as.Date(str_c(max(gegevens_peilverloop$Jaar), "-", periode_einde), format = "%Y-%d-%m"),
                                  by = jaar_label),
                     date_minor_breaks =  "1 month")
      
      
    } else {
      G <- G +
        scale_x_date(date_labels = "%m-%Y", 
                     limits = c(as.Date(str_c(min(gegevens_peilverloop$HydrologischJaar), "-" , "01-04"), format = "%Y-%d-%m"),
                                as.Date(str_c(max(gegevens_peilverloop$HydrologischJaar) + 1, "-", "31-03"), format = "%Y-%d-%m")),
                     breaks = seq(as.Date(str_c(min(gegevens_peilverloop$HydrologischJaar), "-" , "01-04"), format = "%Y-%d-%m"),
                                  as.Date(str_c(max(gegevens_peilverloop$HydrologischJaar) + 1, "-", "31-12"), format = "%Y-%d-%m"),
                                  by = jaar_label),
                     date_minor_breaks =  "1 month")
    }
    
    
    if (label) {
      G <- G +
        geom_label(aes(label = Label, x = PeilmetingDatum, y = min(PeilmetingTAW, na.rm = TRUE) - 0.01),
                   fill = 'transparent', color = label_kleur, show.legend = FALSE, label.size = 0)
    }
  }
  
  if (!legende) {
    G <- G + 
      theme(legend.position = "none")
  }
  
  G <- G +
    labs(x = "")
  
  #Toevoeging correcte label  
  if (maaiveld) { 
    G <- G + labs(y = "Peil (m t.o.v. maaiveld)", x = "")
    
  } else {G <- G + labs(y = "Peil (mTAW)", x = "")}
  
  # Aanpassen xy naar bodem + aanpassen bodem
  if (boorprofiel & controle_boring) {
    G <- G +
      scale_y_continuous(limits = c(max_diepte,
                                    -0.03*max_diepte),
                         breaks = seq(RoundAny(max_diepte, 0.5, floor),
                                      RoundAny(max(Data_GB$van,  na.rm = TRUE), 0.5, ceiling),
                                      by = 0.5),
                         minor_breaks = waiver())
    if (maaiveld) {
      G <- G + 
        geom_hline(yintercept = 0, colour = NA) +
        annotate("curve",
                 x = min(gegevens_peilverloop$PeilmetingDatum),xend = max(gegevens_peilverloop$PeilmetingDatum),
                 y = -0.01*max_diepte, yend = -0.01*max_diepte, curvature = 0, linewidth = 1, colour = NA) + 
        annotate("curve", x = min(gegevens_peilverloop$PeilmetingDatum),xend = max(gegevens_peilverloop$PeilmetingDatum),
                 y = -0.02*max_diepte, yend = -0.02*max_diepte, curvature = 0, linewidth = 1, colour = NA) + 
        annotate("curve", x = min(gegevens_peilverloop$PeilmetingDatum),xend = max(gegevens_peilverloop$PeilmetingDatum),
                 y = -0.03*max_diepte, yend = -0.03*max_diepte, curvature = 0, linewidth = 1, colour = NA)
    }
    
    # Geen boorbeschrijving.
  } else {
    G <- G +
      scale_y_continuous(limits = c(min(RoundAny(gegevens_peilverloop$PeilmetingTAW, 0.1, floor), na.rm = TRUE),
                                    max(RoundAny(gegevens_peilverloop$PeilmetingTAW, 0.1, ceiling), na.rm = TRUE)),
                         breaks = seq(min(RoundAny(gegevens_peilverloop$PeilmetingTAW, 0.1, floor), na.rm = TRUE),
                                      max(RoundAny(gegevens_peilverloop$PeilmetingTAW, 0.1, ceiling), na.rm = TRUE),
                                      by = 0.2),
                         minor_breaks = waiver())
    
  }
  
  droogval <- any(!is.na(gegevens_peilverloop$Label))
  
  Onderschrift <- OnderschriftKwel(gegevens_diepte = gegevens_diepte, maaiveld = maaiveld, droogval = droogval, boorprofiel = boorprofiel, 
                                   aangepaste_periode = aangepaste_periode, 
                                   periode_begin = periode_begin,  
                                   periode_einde = periode_einde, 
                                   controle_boring = controle_boring)
  
  
  return(list(Graf = G, Boor = GB_data$G, Bijschrift = Onderschrift))
}

# Opbolling ---------
OnderschriftOpbolling <- function(maaiveldhoogte ,Peilbuis, Peilschaal, maaiveld, droogval, boorprofiel, controle_boring, aangepaste_periode, periode_begin, periode_einde) {
  
  if (maaiveld) {
    onder_m <- ' m t.o.v. maaiveld'
  } else {
    onder_m <- ' mTAW'
  }
  if (boorprofiel &  controle_boring) {
    onder_b <- str_c(" De bodembeschrijving is een boring ter hoogte van de peilbuis.")
  } else if (boorprofiel & !controle_boring) {
    onder_b <- str_c(" Geen bodembeschrijving beschikbaar.")
  } else {
    onder_b <- ""
  }
  
  if (droogval) {
    onder_dv <- str_c(" De bruine kruisen zijn datums met droogval.")
  } else {
    onder_dv <- "" 
  }
  if (aangepaste_periode) {
    periode <- str_c(GetDatum(periode_begin), " - ", GetDatum(periode_einde), ".")
  } else {
    periode <- str_c("het hydrologisch jaar.")
  }
  Onderschrift <- str_c("Opbolling van het grondwaterpeil ",Peilbuis,  
                        " (blauw) tegenover het oppervlaktewaterpeil ", Peilschaal, " (rood) in", onder_m, 
                        " voor ", periode , " ",maaiveldhoogte ,".",  onder_dv, onder_b)
  return(Onderschrift)
}


GrafiekOpbolling <- function(opbolling_data, # Data van OpbollingData()
                              aangepaste_periode, # Booleaans. Indien FALSE hydroloigsch jaar. TRUE aangepaste data binnen hetzelfde jaa
                              periode_begin = "01-01", # Enkel als aangeapst TRUE. 
                              periode_einde = "01-08", 
                              facet = TRUE,
                              jaar = NULL, # rescaling x-axis labels
                              maaiveld = TRUE, # Als fout mTAW
                              boorprofiel = TRUE,
                              max_tijdspanne = 60,
                              ingegeven = FALSE,  # Booleaans. Als False: enkel gevalideerd, anders ook ingegeven
                              label = TRUE, # label plotten als er droogval is
                             legende = FALSE) { 
  # GEeft het peilerloop weer. Onderbreekt de lijn als er meer als 60 dagen tussen zit.
  
  # gegevens <- opbolling_data$data %>% 
  #   arrange(MeetpuntCode)
  
  PeilbuisCode <- opbolling_data$data %>% 
    filter(str_detect(MeetpuntCode, "P")) %>% 
    distinct(MeetpuntCode) %>% 
    pull()
  
  PeilschaalCode <- opbolling_data$data %>% 
    filter(str_detect(MeetpuntCode, "S")) %>% 
    distinct(MeetpuntCode) %>% 
    pull()
  
  # Voeg NA toe tussen de observaties van de tijdreeksen als er meer tijd tussenzit als de ingegeven max_tijdspanne zodat de lijn onderbroken wordt.
  gegevens_peilverloop <- data.frame()
  for (code in unique(opbolling_data$data$MeetpuntCode)) {
    
    subet_tijdreeks <- opbolling_data$data %>% 
      filter(.data$MeetpuntCode == code) %>% 
      GetPeilVerloop(., aangepaste_periode = aangepaste_periode,  
                     periode_begin = periode_begin,  
                     periode_einde = periode_einde,
                     jaar = jaar, 
                     max_tijdspanne = max_tijdspanne, 
                     ingegeven = ingegeven)
    
    gegevens_peilverloop <- bind_rows(gegevens_peilverloop, subet_tijdreeks)
  }

  if (nrow(gegevens_peilverloop) == 0) {
    G <- ggplot()
    return(list(Graf = G,   Bijschrift = str_c("Geen data beschikbaar voor ",  PeilbuisCode, " en ",  PeilschaalCode, " voor ingegeven periode.")))
  }
  
  
  # if all years
  jaar_label <- "6 month"
  # Periode opbolling
  if (!is.null(jaar)) {
    jaar_label <- "2 month"
    # if (!aangepaste_periode) {
    #   gegevens_peilverloop <- dplyr::filter(gegevens_peilverloop, .data$HydrologischJaar %in% jaar)
    # } else {
    #   gegevens_peilverloop <- dplyr::filter(gegevens_peilverloop, .data$Jaar %in% jaar)
    # }
  }
  GB_data <- NULL
  GB_data$G <- NULL
  controle_boring <- ControleBoring(c(PeilbuisCode, PeilschaalCode)) 
  
  # Krijg min en max waarde voor bodemprofiel van relevante meetpunten.
  if (boorprofiel & controle_boring) {
    
    GB_data <- GrafiekBodemProfiel(pull(distinct(gegevens_peilverloop, MeetpuntCode)))
    max_diepte  <- GB_data$MaxDiepte
    
    max_mTAW <- gegevens_peilverloop %>% 
      summarise(min = min(PeilmetingTAW, na.rm = TRUE)) %>% 
      pull() %>% 
      RoundAny(., 0.1, floor)
    
    if (max_mTAW < max_diepte) {
      max_diepte <- max_mTAW 
      
      GB_data <- GrafiekBodemProfiel(pull(distinct(gegevens_peilverloop, MeetpuntCode)), max_diepte = max_diepte)
    }
    
    Data_GB <- GB_data$Data %>% 
      mutate(van = - van,
             tot = -tot)
  }
  
  # Kleur de codes correct.
  Peilbuis <- gegevens_peilverloop %>% 
    filter(str_detect(MeetpuntCode, "P")) %>% 
    distinct(MeetpuntCode) %>% 
    pull()
  
  Peilschaal <- gegevens_peilverloop %>% 
    filter(str_detect(MeetpuntCode, "S")) %>% 
    distinct(MeetpuntCode) %>% 
    pull()
  
  variables <- c(Peilbuis, Peilschaal)
  colors_variables <- c(inbo_donkerblauw,inbo_rood)
  
  if (facet) {
    
    if (aangepaste_periode) {
      G <- ggplot(data = gegevens_peilverloop) +
        geom_line(aes(x = Periode, y = PeilmetingTAW, col = MeetpuntCode)) +
        scale_color_manual(values = colors_variables, breaks = variables, labels = variables) + 
        facet_wrap(~ Jaar, scales = "free_x") +
        scale_x_date(
          date_labels = "%b",
          breaks = seq(as.Date(str_c("2020", "-" , periode_begin), format = "%Y-%d-%m"),
                       as.Date(str_c("2020", "-", periode_einde), format = "%Y-%d-%m"),
                       by = "2 months"),
          limits = as.Date(c(
            str_c("2020", "-", periode_begin), 
            str_c("2020", "-", periode_einde)), format = "%Y-%d-%m"))
      
      if (label) {
        G <- G +
          geom_label(aes(label = Label, x = Periode, y = min(PeilmetingTAW, na.rm = TRUE) - 0.01),
                     fill = 'transparent', color = label_kleur, show.legend = FALSE, label.size = 0)
      }
    } else {
      G <- ggplot(data = gegevens_peilverloop) +
        geom_line(aes(x = Periode, y = PeilmetingTAW, col = MeetpuntCode)) +
        scale_color_manual(values = colors_variables, breaks = variables, labels = variables) + 
        facet_wrap(~ HydrologischJaar, scales = "free_x") +
        scale_x_date(date_labels = "%b",
                     breaks = seq(as.Date(str_c("2019", "-" , "01-04"), format = "%Y-%d-%m"),
                                  as.Date(str_c("2020", "-", "31-12"), format = "%Y-%d-%m"),
                                  by = "2 month"),
                     limits = c(as.Date(str_c(2019, "-", "04", "-01")), as.Date(str_c(2020, "-", "03-31"))))
      
    }
  } else {
    G <- ggplot(data = gegevens_peilverloop) +
      geom_line(aes(x = PeilmetingDatum, y = PeilmetingTAW, col = MeetpuntCode)) +
      scale_color_manual(values = colors_variables, breaks = variables, labels = variables)
    
    if (aangepaste_periode) {
      G <- G +
        scale_x_date(date_labels = "%m-%Y", 
                     limits = c(as.Date(str_c(min(gegevens_peilverloop$Jaar), "-" , periode_begin), format = "%Y-%d-%m"),
                                as.Date(str_c(max(gegevens_peilverloop$Jaar), "-", periode_einde), format = "%Y-%d-%m")),
                     breaks = seq(as.Date(str_c(min(gegevens_peilverloop$Jaar), "-" , periode_begin), format = "%Y-%d-%m"),
                                  as.Date(str_c(max(gegevens_peilverloop$Jaar), "-", periode_einde), format = "%Y-%d-%m"),
                                  by = jaar_label),
                     date_minor_breaks =  "1 month")
    } else {
      G <- G +
        scale_x_date(date_labels = "%m-%Y", 
                     limits = c(as.Date(str_c(min(gegevens_peilverloop$HydrologischJaar), "-" , "01-04"), format = "%Y-%d-%m"),
                                as.Date(str_c(max(gegevens_peilverloop$HydrologischJaar) + 1, "-", "31-03"), format = "%Y-%d-%m")),
                     breaks = seq(as.Date(str_c(min(gegevens_peilverloop$HydrologischJaar), "-" , "01-04"), format = "%Y-%d-%m"),
                                  as.Date(str_c(max(gegevens_peilverloop$HydrologischJaar) + 1, "-", "31-12"), format = "%Y-%d-%m"),
                                  by = jaar_label),
                     date_minor_breaks =  "1 month")
    }
    
    if (label) {
      G <- G +
        geom_label(aes(label = Label, x = PeilmetingDatum, y = min(PeilmetingTAW, na.rm = TRUE) - 0.01),
                   fill = 'transparent', color = label_kleur, show.legend = FALSE, label.size = 0)
    }
    
  }
  
  G <- G +
    labs(x = "")
  
  if (!legende) {
    G <- G + 
      theme(legend.position = "none")
  }
  
  #Toevoeging correcte label  
  if (maaiveld) { G <- G + labs(y = "Peil (m t.o.v. maaiveld)", x = "")
  } else {G <- G + labs(y = "Peil (mTAW)", x = "")}
  
  
  # Aanpassen xy naar bodem + aanpassen bodem
  if (boorprofiel & controle_boring) {
    G <- G +
      scale_y_continuous(limits = c(max_diepte,
                                    -0.03*max_diepte),
                         breaks = seq(RoundAny(max_diepte, 0.5, floor),
                                      RoundAny(max(Data_GB$van,  na.rm = TRUE), 0.5, ceiling),
                                      by = 0.1),
                         minor_breaks = waiver())
    if (maaiveld) {
      G <- G + 
        geom_hline(yintercept = 0, colour = NA) +
        annotate("curve",
                 x = min(gegevens_peilverloop$PeilmetingDatum),xend = max(gegevens_peilverloop$PeilmetingDatum),
                 y = -0.01*max_diepte, yend = -0.01*max_diepte, curvature = 0, linewidth = 1, colour = NA) + 
        annotate("curve", x = min(gegevens_peilverloop$PeilmetingDatum),xend = max(gegevens_peilverloop$PeilmetingDatum),
                 y = -0.02*max_diepte, yend = -0.02*max_diepte, curvature = 0, linewidth = 1, colour = NA) + 
        annotate("curve", x = min(gegevens_peilverloop$PeilmetingDatum),xend = max(gegevens_peilverloop$PeilmetingDatum),
                 y = -0.03*max_diepte, yend = -0.03*max_diepte, curvature = 0, linewidth = 1, colour = NA)
    }
  } else {
    G <- G +
      scale_y_continuous(limits = c(min(RoundAny(gegevens_peilverloop$PeilmetingTAW, 0.1, floor), na.rm = TRUE),
                                    max(RoundAny(gegevens_peilverloop$PeilmetingTAW, 0.1, ceiling), na.rm = TRUE)),
                         breaks = seq(min(RoundAny(gegevens_peilverloop$PeilmetingTAW, 0.1, floor), na.rm = TRUE),
                                      max(RoundAny(gegevens_peilverloop$PeilmetingTAW, 0.1, ceiling), na.rm = TRUE),
                                      by = 0.2),
                         minor_breaks = waiver())
  }
  
  droogval <- any(!is.na(gegevens_peilverloop$Label))
  
  
  Onderschrift <- OnderschriftOpbolling(maaiveldhoogte = opbolling_data$maaiveld, Peilbuis = Peilbuis, Peilschaal = Peilschaal,
                                        maaiveld = maaiveld, droogval = droogval, boorprofiel = boorprofiel,
                                        aangepaste_periode = aangepaste_periode, periode_begin = periode_begin,  
                                        periode_einde = periode_einde,  controle_boring = controle_boring)
  
  
  return(list(Graf = G, Boor = GB_data$G, Bijschrift = Onderschrift))
}


# Grafieken Statistieken ----------------

OnderschriffTabel <- function(Gx3tabel, label_kwaliteit = "") {
  
  label_tekst <- ""
  if (label_kwaliteit !=  "") {
    if (any(Gx3tabel$Gemiddelde == label_kwaliteit)) {
      label_tekst <- str_c(label_kwaliteit, ": voldoet niet aan de kwaliteit-voorwaarden. ")
    } 
  } else {
    if (any(str_detect(Gx3tabel$Gemiddelde, "[RP<%X]"))) {
      if (any(str_detect(Gx3tabel$Gemiddelde, "RP"))) {
        label_tekst <- str_c(label_tekst, "RP: niet voldaan aan Representatieve Periode. ")
      }
      
      if (any(str_detect(Gx3tabel$Gemiddelde, "%"))) {
        label_tekst <- str_c(label_tekst, "<", percent_decimal, "%: niet voldaan aan kwantiteit. ")
      }
      
      if (any(str_detect(Gx3tabel$Gemiddelde, "X"))) {
        label_tekst <- str_c(label_tekst, "X: Geen of niet voldoende data beschikbaar. ")
      }
      
      if (any(str_detect(Gx3tabel$Label, "Droogval"))) { 
        label_tekst <- str_c(label_tekst, "Droogval: geen water. ")
      }
    } 
  }
  
  return(label_tekst)
}




GrafiekTabelGhl3 <- function(tijdreeks, tijdspanne = 14, 
                             percent_decimal, RP_dagen,
                             ingegeven = FALSE, label_kwaliteit = "" ,
                             GH3 = TRUE, GL3 = TRUE, trim = TRUE,
                             seq_by = 0.01, seq_round = 0.05) {
  
  #Label kan aangeapst worden naar wens.
  GH3_data <- data.frame()
  GL3_data <- data.frame()
  GH3_onderschrift <- NULL
  GL3_onderschrift <- NULL
  
  if (GH3) {
    GH3_data <- CalculateGH3(tijdreeks, tijdspanne = tijdspanne, ingegeven = ingegeven, percent_decimal, RP_dagen)$GH3 %>% 
      mutate(Methode = "GH3")
    GH3_onderschrift <- "gemiddelde hoogste grondwaterstand (GH3)"
    
    if (trim) { 
      GH3_data <- GH3_data %>% 
        VerwijderLagLeading() 
    }
  }

  if (GL3) {
    GL3_data <- CalculateGL3(tijdreeks, tijdspanne = tijdspanne, ingegeven = ingegeven, percent_decimal, RP_dagen)$GL3 %>% 
      mutate(Methode = "GL3")
    GL3_onderschrift <- "gemiddelde laagste grondwaterstand (GL3)"
    
    if (trim) { 
      GL3_data <- GL3_data %>% 
        VerwijderLagLeading() }
  }
  
  Gx3 <- bind_rows(GH3_data, GL3_data) %>% 
    mutate(HydrologischJaar = as.character(HydrologischJaar)) %>% 
    mutate(Label2 = if_else(Label == "Droogval",
                           "X", ""))
  
  
  
  if (all(is.na(Gx3$Gemiddelde))) {
    G <- ggplot()
  } else {
    G <- Gx3 %>% 
      ggplot(aes(x = HydrologischJaar, y = Gemiddelde, color = Methode, group = Methode)) +
      geom_line() +
      geom_point(size = 2) + 
      scale_colour_manual(values = c("GH3" = inbo_donkerblauw, "GL3" = inbo_oranje)) +
      labs(y = "mTAW", x = "", col = "Grondwaterpeil") +
      scale_y_continuous(limits = c(RoundAny(min(Gx3$Gemiddelde, na.rm = TRUE), seq_round, f = floor), 
                                    RoundAny(max(Gx3$Gemiddelde, na.rm = TRUE), seq_round, f = ceiling)), 
                         breaks = seq(RoundAny(min(Gx3$Gemiddelde, na.rm = TRUE), seq_round, f = floor), 
                                      RoundAny(max(Gx3$Gemiddelde, na.rm = TRUE), seq_round, f = ceiling), 
                                      by = seq_by))
  }
  
  Gx3tabel <- Gx3 %>% 
    mutate(Gemiddelde = round(Gemiddelde, 3),
           Gemiddelde = as.character(Gemiddelde)) %>% 
    mutate(Gemiddelde = case_when(
      is.na(Gemiddelde) ~ Label,
      TRUE ~ Gemiddelde
    )) %>% 
    select(Methode, "Hydrologisch Jaar" = HydrologischJaar, Gemiddelde) %>% 
    mutate(Gemiddelde = case_when(
      label_kwaliteit != "" & str_detect(Gemiddelde, "[RP%X]") ~ label_kwaliteit,
      TRUE ~ Gemiddelde
    )) 
  
  
  label_tekst <- OnderschriffTabel(Gx3tabel, label_kwaliteit = label_kwaliteit)
  
  Gx3tabel <- Gx3tabel %>%
    pivot_wider(., names_from = Methode, values_from = Gemiddelde) %>%
    flextable() %>%
    border_outer() %>%
    flextable::align(align = "center", part = "all") %>%
    set_table_properties(., layout = "autofit")

  code <- tijdreeks %>% 
    distinct(MeetpuntCode) %>% 
    pull()
  
  if (is.null(GH3_onderschrift) | is.null(GL3_onderschrift)) {
    text <- NULL
  } else {
    text <- " en "
  }
  
  Onderschrift_Graf <- str_c("De ", GH3_onderschrift, text, GL3_onderschrift, " voor ", code,  ".")
  Onderschrift_Tabel <- str_c("De ", GH3_onderschrift, text, GL3_onderschrift, " voor ", code,  ". ", label_tekst)
  
  
  return(list(Graf = G, Tabel = Gx3tabel, Bijschrift_G = Onderschrift_Graf, Bijschrift_Tabel = Onderschrift_Tabel))
}





GrafiekTabelGvz3 <- function(tijdreeks, tijdspanne = 12, ingegeven = FALSE, trim = TRUE) {
  
  
  GV3 <- CalculateGV3( tijdreeks, tijdspanne = tijdspanne, ingegeven = ingegeven)$GV3 %>% 
    mutate(Methode = "GV3")
  
  
  if (trim) {
    GV3 <- GV3 %>% 
      VerwijderLagLeading()
  }
  
  GB3_01 <- CalculateGZ3(tijdreeks, "01-05", tijdspanne = tijdspanne, ingegeven = ingegeven)$GV3 %>% 
    mutate(Methode = "GB3_01-05")
  
  if (trim) {
    GB3_01 <- GB3_01 %>% 
      VerwijderLagLeading()
  }
  
  GB3_15 <- CalculateGZ3(tijdreeks, "15-05", tijdspanne = tijdspanne, ingegeven = ingegeven)$GV3 %>% 
    mutate(Methode = "GB3_15-05")
  if (trim) {
    GB3_15 <- GB3_15 %>% 
      VerwijderLagLeading()
  }

  Gx3 <- bind_rows(GV3, GB3_01, GB3_15) %>% 
    mutate(Jaar = as.character(Jaar))
  
  if (all(is.na(Gx3$Gemiddelde))) {
    G <- ggplot()
  } else {
    G <- Gx3 %>% 
      ggplot(aes(x = Jaar, y = Gemiddelde, color = Methode, group = Methode)) +
      geom_line() +
      geom_point(size = 2) + 
      scale_colour_manual(values = c("GV3" = inbo_donkergroen, "GB3_01-05" = inbo_oranjerood, "GB3_15-05" = inbo_geelgr)) +
      labs(y = "mTAW", x = "", col = "Grondwaterpeilen") +
      scale_y_continuous(limits = c(RoundAny(min(Gx3$Gemiddelde, na.rm = TRUE), 0.5, f = floor), 
                                    RoundAny(max(Gx3$Gemiddelde, na.rm = TRUE), 0.5, f = ceiling)), 
                         breaks = seq(RoundAny(min(Gx3$Gemiddelde, na.rm = TRUE), 0.5, f = floor), 
                                      RoundAny(max(Gx3$Gemiddelde, na.rm = TRUE), 0.5, f = ceiling), 
                                      by = 0.05))
    
  }
  
  Gx3tabel <- Gx3 %>% 
    mutate(Gemiddelde = round(Gemiddelde, 3),
           Gemiddelde = as.character(Gemiddelde)) %>% 
    mutate(Gemiddelde = case_when(
      is.na(Gemiddelde) ~ Label,
      TRUE ~ Gemiddelde
    )) %>% 
    select(Methode, Jaar, Gemiddelde) %>% 
    pivot_wider(., names_from = Methode, values_from = Gemiddelde) %>% 
    flextable() %>% 
    border_outer() %>% 
    flextable::align(align = "center", part = "all") %>% 
    set_table_properties(., layout = "autofit")
  
  
  
  code <- tijdreeks %>% 
    distinct(MeetpuntCode) %>% 
    pull()
  
  Onderschrift_Graf <- str_c("De gemiddelde voorjaarsgrondwaterstand (GV3) en broedwaterstand gedurende ", GetDatum("01-05"), " en ", GetDatum("15-05"), " voor ", code,  ".")
  Onderschrift_Tabel <- str_c("De gemiddelde voorjaarsgrondwaterstand (GV3) en broedwaterstand gedurende ", GetDatum("01-05"), " en ", GetDatum("15-05"), " voor ", code,  ". RP: noet voldaan aan Representatieve periode. #<3: Niet voldaan aan kwantiteit. Droogval: geen water door droogte. X: Geen of niet voldoende data beschikbaar gedurende kernperiode.")
  
  
  return(list(Graf = G, Tabel = Gx3tabel, Bijschrift_G = Onderschrift_Graf, Bijschrift_Tabel = Onderschrift_Tabel))
}







SelectieJaren <- function(data, PeilbuisCode, PeilSchaalCode,  periode_begin, periode_einde) {
  
  Jaren_peilbuis <- data %>% 
    dplyr::filter(
    .data$MeetpuntCode == PeilbuisCode &
      .data$PeilmetingDatum %within% 
      interval(as_date(str_c(periode_begin, "-", year(.data$PeilmetingDatum)), format = "%d-%m-%Y"),
               as_date(str_c(periode_einde, "-", year(.data$PeilmetingDatum)), format = "%d-%m-%Y"))) %>% 
    mutate(Jaar = year(.data$PeilmetingDatum)) %>% 
    distinct(Jaar) %>% 
    arrange(Jaar) %>% 
    pull()
  
  Jaren_peilschaal <- data %>% 
    dplyr::filter(
      .data$MeetpuntCode == PeilSchaalCode &
        .data$PeilmetingDatum %within% 
        interval(as_date(str_c(periode_begin, "-", year(.data$PeilmetingDatum)), format = "%d-%m-%Y"),
                 as_date(str_c(periode_einde, "-", year(.data$PeilmetingDatum)), format = "%d-%m-%Y"))) %>% 
    mutate(Jaar = year(.data$PeilmetingDatum)) %>% 
    distinct(Jaar) %>% 
    arrange(Jaar) %>% 
    pull()
  
  Jaren <- base::intersect(Jaren_peilbuis, Jaren_peilschaal)
  
  return(Jaren)
}


                   

# Kaart hydrochemie
LayerHydrochemie <- function(kaart, label_meetpunt = TRUE,
                             meetpunten = NULL, parameter.gradient = NULL, parameter.size = NULL,
                             jaren = NULL,  maanden = NULL, datum = NULL, metdatum = FALSE,  buffer_m = 2,
                             hydrochemie.df = lims_havens_gecontroleerd
                             ) {
  
  # If meetpunten is NULL, need for all distinct meetpunten.
  if (is.null(meetpunten)) {
    meetpunten <- hydrochemie.df %>% 
      distinct(ExternSampleID) %>% 
      pull()
  }
  

  
  if (metdatum) {
    lims_data <- hydrochemie.df %>% 
      filter(Datum  == datum &  ExternSampleID %in% meetpunten) %>% 
      filter(Component %in% c(parameter.gradient, parameter.size)) 
  } else {
    
    # Dataset
    lims_data <- hydrochemie.df %>% 
      filter(month(Datum) %in% maanden & year(Datum) %in% jaren & ExternSampleID %in% meetpunten) %>% 
      filter(Component %in% c(parameter.gradient, parameter.size)) 
  }
  
  lims_data <- lims_data %>% 
    filter(!is.na(X) |!is.na(Y)) %>% 
    select(Datum, ExternSampleID, Component, WaardeNumeriek, X, Y) %>% 
    group_by(ExternSampleID) %>% 
    mutate(X = mean(X),
           Y = mean(Y)) %>% 
    ungroup() %>% 
    distinct() %>% 
    st_as_sf(., coords = c("X", "Y"), crs = 31370)
  
  if (nrow(lims_data) == 0) {
    return(str_c("Geen beschikbare hydrochemische informatie voor de ingegeven meetpunten op ", datum, "."))
  }
  
  
  controle <- lims_data %>% 
    group_by(Datum, ExternSampleID, Component) %>% 
    summarise(n = n(), .groups = "drop") %>%
    distinct(n) %>% 
    pull(n) 
  
  if (length(controle) != 1) {
    
    te <- lims_data %>% 
      group_by(Datum, ExternSampleID, Component) %>% 
      summarise(n = n(), .groups = "drop") %>% 
      filter(n != 1)
    
    te_datum <- te$Datum
    te_meetpunt <- te$ExternSampleID
    te_comp <- te$Component
    
    return(str_c("Dubbele waarde aanwezig voor ", te_comp, " voor meetpunt ", te_meetpunt, " op ", te_datum, "."))
    
  }
  
  

  lims_data.wider <- lims_data %>%
    filter(Component %in% c(parameter.gradient, parameter.size)) %>%
    select(Datum, ExternSampleID, Component, WaardeNumeriek) %>%
    # distinct() %>%
    pivot_wider(., names_from = "Component", values_from = "WaardeNumeriek") %>% 
    rename("Size" = parameter.size, "Gradient" = parameter.gradient) %>% 
    mutate(Longitude = unlist(map(.$geometry,1)),
           Latitude = unlist(map(.$geometry,2)))
  
  
  # digits = lims_data.wider %>%
  #   st_drop_geometry() %>%
  #   select(Gradient) %>%
  #   slice_min(Gradient, with_ties = FALSE) %>%
  #   pull() %>%
  #   nchar()
  # if (digits == 1 || digits == 2) {
  #   digits <- 10
  # } else {
  #   digits <- 10^(digits-2)
  # }
  # # 
  # # 
  # vector <-  seq(min(RoundAny(lims_data.wider$Gradient, digits ,floor)), max(RoundAny(lims_data.wider$Gradient, digits, ceiling)), length.out = 5)
  # labels <-  str_c("[", vector[-length(vector)] , "-", vector[-1], ")")
  # # 
  # # 
  # # # BREAKS NOT IN THE GOOD AMOUNT OF GROUPS!!!!!! 
  # lims_data.wider %>%
  #   mutate(gradient_break = cut(Gradient,
  #                               breaks = 0,
  #                               include.lowest = TRUE)) %>%
  #   distinct(gradient_break)
  # # 
  # 

  
  if (!is.null(parameter.gradient) & !is.null(parameter.size)) {
    Graf <- kaart +
      new_scale_color() + 
      ggspatial::geom_sf(data = lims_data.wider, aes(col = Gradient, size = Size), pch = 19) +
      scale_color_gradient2(midpoint = mean(lims_data.wider$Gradient, na.rm = TRUE), 
                            low = "#B589D6", high = "#552586") +
      scale_size(range = c(2.5, 5) ) +
      guides(size = guide_legend(title = parameter.size, reverse = TRUE, order = 1),
             color = guide_legend(title = parameter.gradient, reverse = TRUE, order = 2))

    bijschrift <- str_c("concentraties van ", parameter.gradient, " en ", parameter.size)
    
  } else if (!is.null(parameter.gradient)) {
    Graf <- kaart +
      new_scale_color() + 
      ggspatial::geom_sf(data = lims_data.wider, aes(color = Gradient), pch = 19, fill = "black", size= 2.5) +
      scale_color_gradient2(midpoint = mean(lims_data.wider$Gradient, na.rm = TRUE), 
                            low = "#B589D6", high = "#552586") +
      guides(
             color = guide_legend(title = parameter.gradient, reverse = TRUE, order = 2))
    bijschrift <- str_c("concentraties van ", parameter.gradient)
  } else {
    Graf <- kaart +
      new_scale_color() + 
      ggspatial::geom_sf(data = lims_data.wider, aes(size = Size), pch = 19, col = "#552586") +
      scale_size(range = c(2.5, 5) ) +
      guides(
        color = guide_legend(title = parameter.gradient, reverse = TRUE, order = 2))
    
    bijschrift <- str_c("concentraties van ", parameter.size)
  }
  
  
  if (label_meetpunt) {
    Meetpunten_groepen <- GroepenMeetpunten(gebied = gebied, buffer_m = buffer_m, selectie_meetpunten = meetpunten)
    
    # Voeg dieptenummering en filterdiepte toe per groep
    meetpunten_diepte <- data.frame()
    for (groep in unique(Meetpunten_groepen$Groep)) {
      
      df_Meetpunten <- Meetpunten_groepen %>% 
        filter(Groep == groep) %>% 
        pull(MeetpuntCode) %>% 
        MeetpuntDiepteFilter(.)
      
      meetpunten_diepte <- bind_rows(meetpunten_diepte, df_Meetpunten)
    }
    
    lims_data_meetpunten <- meetpunten_diepte %>% 
      filter(.data$Diepte == 1) %>% 
      left_join(., Meetpunten_groepen, by = c("MeetpuntCode")) %>% 
      filter(MeetpuntCode %in% unique(lims_data$ExternSampleID )) %>% 
      mutate(MeetpuntCode   = str_replace(MeetpuntCode , "WAH", ""))
    
    
    Graf <- Graf +
      ggsflabel::geom_sf_label_repel(data = lims_data_meetpunten, aes(label = MeetpuntCode ), alpha = 0.9, col = 1, size = 2, nudge_x = 0, nudge_y = 0) +
      labs(x = "", y = "")
  }
  
  Datums <- lims_data.wider %>% 
    distinct(Datum) %>% 
    mutate(Datum_ = str_c(day(Datum), "-", month(Datum), "-", year(Datum))) %>% 
    pull(Datum_)
 
  
  
  Bijschrift <- str_c("Overzicht van de ",  bijschrift, " per meetpunt op ", Datums, ".")
 
    
  return(list(Graf = Graf, Bijschrift = Bijschrift))
}


NullVariable <- function(variable) {
  # Make NA variable NULL
  if (is.na(variable)) {
    variable <- NULL
  }
  
  return(variable)
}

