# Basisdata --------------------------------------------------------------------
# BELANGRIJK. Veel functies baseren zich op op data-tabelen die NIET opgenomen zijn in de functie zelf, maar dienen te staan in de Environment.
# Zeker belangrijk de Kaarten, gebieden is sf-file met al de gebieden erin. Wordt binnen de functie gefilterd


# O:\Projects\PRJ_Schelde\Haven\Gebieden Haven
gebieden_sf <- st_read("Data/Shapefiles/Gebieden.shp", crs = 31370) %>% 
  rename(Gebied = Naam) %>% 
  mutate(Gebied = str_to_title(Gebied))


# Geografische informatie overzicht van de Schelde en dokken.
schelde_dokken_sf <- st_read("Data/Shapefiles/Schelde en dokken.shp", crs = 31370)

# Geografische informatie over de afwateringen.
afwatering_sf <- st_read("S:/Vlaanderen/Water/VHA/Wlas.shp") %>% 
  st_transform(crs = 31370) %>% 
  # Filter op Stroombekken Schelde en de categoriën.
  dplyr::filter(str_detect(.data$LBLCATC, "categorie") & STRMGEB == "Schelde") %>% 
  dplyr::select(Name = NAAM, LBLCATC)

# Excel data met de boorbeschrijvingen
boringen_df <- read_xlsx("Data/Gegevens/boringen.xlsx")

# Sommige gebieden hebben een aparte raster-file waarin de toplaag afgegraven is Deze dienen gebruikt te worden
# voor de visuele weergave van het reliëf.
gebieden_afgraving <- c("Doelpolder Noord", "Drijdijck")


# Based on the colors of 'bodemkaart 2015".
colours_bodemkaart <- c("Vochtig zandleem" = "#C2B342", "Antropogeen" = "#FFFFFF",
                        "Natte klei" =  "#6D8532", "Natte zware klei" = "#206375",
                        "Nat zandleem" = "#A69537", "Vochtige zware klei" = "#42927B",
                        "Vochtig zand" =  "#6C8CAD", "Vochtig zand antropogeen" = "#D0AA98",
                        "Nat zand" = "#446589",  
                        "Droog zand antropogeen" = "#EAD0C9",
                        "Droog zand" = "#94B2D1", "Nat zand antropogeen" =  "#B68367", 
                        "Vochtige klei" = "#85A444", "Veen" = "#645D37",  "Landduin" = "#EEDFB9")


## Colors for dtm.
# colours_dtm <- c(
#   "#052667", # Donkerblauw
#   "#1D8F92", # cyan
#   "#07DB00",
#   "#F8FC01",
#   "#EEA114",   # Orange
#   "#C35539")
# 
# 
# colours_dtm <- c(
#   # "#3C71A3",
#   "#68AAA6",
#   "#9DD1A0",
#   # "#BFE4B1",
#   # "#DCF2C3",
#   "#F4F7CC",
#   "#E8C583", 
#   # "#DD835E", 
#   "#BF5440",
#   # "#A63338", 
#   "#911538")

colours_zomerpeil <- c(
  "#052667", # Donkerblauw
  "#408080") # cyan

colours_dtm <- c(
  "#098000",
  "#07DB00",
  "#F8FC01", 
  "#EEA114",   # Orange
  "#C35539", 
  "#911538")



# colours_dtm <- c(
#   "#0A6200", 
#   # "#368B00", 
#   # "#77AF14", 
#   # "#C9DE35", 
#   "#F0F943",
#   # "#E1C538",
#   # "#C0941F",
#   # "#991A1F", 
#   "#811301")


OpmaakKaart <- function(kaart, expand = TRUE) {
  
  kaart <- kaart + 
    labs(x = "", y = "") +
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

# IDENTIFY COLORS
# Based on colors of maps of PP. 
# Screenshot image -> Paint -> identity color (oogdruppel-tool -> edit colors) -> get rgb-value -> 
# set in converter for color chart (https://www.rapidtables.com/web/color/RGB_Color.html)


# Ondersteunende functies ---------------------------------------------------------

# Global-functies die gebruikt kunnen worden in de andere functies.

ColorPalette <- function(colours, intervals) {
  # Create your own color palette by choosing own colour-gradient and amount of intervals needed.
  colfunc <- colorRampPalette(colours)
  created_palette <- colfunc(intervals)
  return(created_palette)
}


ClipShapefile <- function(raster_gebied, shapefile) {
  # Clip external Shapefiles to the corrrect size of the raster so they can be plotted in same figure.
  st_agr(shapefile) = "constant"  # Avoid warning that it is assumed spatially constant
  cropped_sf <- sf::st_crop(shapefile, raster_gebied) %>% 
    dplyr::select(Name)
  return(cropped_sf)
}

RoundAny = function(x, accuracy, f=round){
  # Rond een getal af op een in te geven cijfer.
  # x is het getal, accuracy de cijfer waarnaar het dient af te ronden
  # f de formule voor ceiling (dichtbijzijnde naar boven afgerond) of floor (dichtsbijzijnde naar onder afgerond).
  f(x / accuracy) * accuracy
}


ControleBoring <- function(Meetpunten) {
  # Controlleer of er van de peilbuis een boring beschikbaar is.
  controle <- FALSE
  if (any(Meetpunten %in% unique(boringen_df$peilpunt))) {
    controle <- TRUE
  } 
  return(controle)
}



GebiedenDTMMask <- function(gebieden_buffer = 0, 
                              gebieden = gebieden_sf,
                              DTM_Jaar = 2017, 
                              Locatie_DTM = "S:/Vlaanderen/Hoogte/DHMVII/DHMVIIDTMRAS1m.tif") {
  # Create shape-files voor hoogte-profiel voor enkel het gebied.
  # Boolean to control if map exist.
  
  Create <- FALSE
  # Maak map aan om dtm's in op te slagen
  OutputLocationName <- str_c("DTM gebieden MASK ", DTM_Jaar)
  if (!file.exists(str_c("Data/", OutputLocationName))) {
    dir.create(str_c("Data/", OutputLocationName))
  }
  OutputLocation <- str_c("Data",OutputLocationName,sep = "/")
  
  # Create a submap for the specific buffer if it dont exist. 
  OutputLocationName <- str_c(OutputLocationName, "/buffer_", gebieden_buffer)
  
  if (!file.exists(str_c("Data/", OutputLocationName))) {
    dir.create(str_c("Data/", OutputLocationName))
    Create <- TRUE
  }
  OutputLocation <- str_c("Data",OutputLocationName,sep = "/")
  
  if (Create) {
    # Read dtm Vlaanderen.
    Vlaanderen_dtm <- terra::rast(Locatie_DTM) %>% 
      `crs<-`("EPSG:31370")
    
    # Make from the geometry with same group-name one group and add buffer.
    gebieden_tot_sf <- gebieden %>% 
      group_by(Gebied) %>% 
      summarise(., .groups = "drop")
    
    for (gebied in gebieden_tot_sf$Gebied) {
      
      gebied_dtm <- subset(gebieden_tot_sf, Gebied == gebied) %>% 
        terra::vect()
      
      # get MASKED dtm
      gebied_dtm <- Vlaanderen_dtm %>% 
        `names<-`("Hoogte") %>% 
        terra::crop(., gebied_dtm, mask = TRUE) %>% 
        `crs<-`(str_c("EPSG:31370"))

      writeRaster(x = gebied_dtm, filename = str_c(OutputLocation, "/","dtm_", gebied, "_", gebieden_buffer, ".tif"))
    }
  }
  return(list(OutputLocation, gebieden_buffer))
}

GebiedenDTMMaskGeneral <- function(gebied_sf, 
                                   gebieden_buffer = 0, 
                                   DTM_Jaar = 2017, 
                                   Locatie_DTM = "S:/Vlaanderen/Hoogte/DHMVII/DHMVIIDTMRAS1m.tif") {
  # Create shape-files voor hoogte-profiel voor enkel het gebied.
  # Boolean to control if map exist.
  
  
  gebied <- gebied_sf %>% 
    group_by(Gebied) %>% 
    summarise(., .groups = "drop") %>% 
    pull(Gebied) %>% 
    str_c(collapse = "_")
  
  
  # Maak map aan om dtm's in op te slagen
  OutputLocationName <- str_c("DTM gebieden MASK ", DTM_Jaar)
  if (!file.exists(str_c("Data", OutputLocationName, sep = "/"))) {
    dir.create(str_c("Data", OutputLocationName, sep = "/"))
  }
  OutputLocation <- str_c("Data",OutputLocationName,sep = "/")
  
  # Create a submap for the specific buffer if it dont exist. 
  OutputLocationName <- str_c(OutputLocationName, "/buffer_", gebieden_buffer)
  
  if (!file.exists(str_c("Data", OutputLocationName, sep = "/"))) {
    dir.create(str_c("Data", OutputLocationName, sep = "/"))
  }
  OutputLocation <- str_c("Data",OutputLocationName,sep = "/")
  
  Name_file <- str_c(OutputLocation, "/", "dtm_", gebied, "_", gebieden_buffer, ".tif")
  
  if (!file.exists(Name_file)) {
    # Read dtm Vlaanderen.
    Vlaanderen_dtm <- terra::rast(Locatie_DTM) %>% 
      `crs<-`("EPSG:31370")
    
    # Make from the geometry with same group-name one group and add buffer.
    gebied_dtm <- gebied_sf %>% 
      terra::vect()
    
    gebied_dtm <- Vlaanderen_dtm %>% 
      `names<-`("Hoogte") %>% 
      terra::crop(., gebied_dtm, mask = TRUE) %>% 
      `crs<-`(str_c("EPSG:31370"))
    
    writeRaster(x = gebied_dtm, filename = str_c(OutputLocation, "/","dtm_", gebied, "_", 
                                                 gebieden_buffer, ".tif"))
  }
  
  return(list(OutputLocation, gebied, gebieden_buffer))
}






MinMaxGebied <- function(gebied, interval_m = 1, gebieden = gebieden_sf) {
  # Get the rounded value for the min and max height in the area.
  # The min or max get rounded up depending on the interval.

  locatie <- GebiedenDTMMask(gebieden_buffer = 0, gebieden = gebieden_sf_)
  gebied_raster <- rast(str_c(locatie[1],'/', "dtm", "_" , gebied, "_", locatie[2],".tif"))
  
  gebied_minmax <- gebied_raster %>% 
    minmax()
    
  minmax <-   list( Min = RoundAny(min(gebied_minmax), interval_m, floor), 
                   Max = RoundAny(max(gebied_minmax), interval_m, ceiling))
  return(minmax)
}


MinMaxGebiedAfgraving <- function(gebied, interval_m = 1, 
                                  gebieden = gebieden_sf,
                                  locatie = "Data/DTM gebieden afgraving/") {
  # Get minmax value of the raster of the excavated raster of strictly the area !
  gebied_spv <- gebieden_sf %>% 
    filter(Gebied == gebied) %>% 
    summarise() %>% 
    vect()
  
  gebied_raster <- rast(str_c(locatie,gebied, "_afgraving.tif")) %>% 
    `crs<-` ("EPSG:31370") %>% 
    `names<-` ("Hoogte")
  
  gebied_raster_narrow <- gebied_raster %>% 
    terra::crop(., gebied_spv, mask = TRUE) %>% 
    `crs<-`(str_c("EPSG:31370"))
  
  gebied_minmax <- gebied_raster_narrow %>% 
    minmax()
  
  minmax <-   list( Min = RoundAny(min(gebied_minmax), interval_m, floor), 
                    Max = RoundAny(max(gebied_minmax), interval_m, ceiling))
  return(minmax)
}


HistogramDTM <- function(gebied, interval_m = 0.5) {
  # Enkel voor het gebied op zich.
  
  locatie_mask <- GebiedenDTMMask()
  gebied_raster_mask <- rast(str_c(locatie_mask[1], "/dtm_", gebied, "_" ,locatie_mask[2],".tif"))
  
  gebied_mask_df <- gebied_raster_mask %>% 
    as.data.frame(xy = TRUE) %>% 
    mutate(Hoogte = RoundAny(Hoogte, interval_m, f = round))
  
  
  G <- ggplot(data = gebied_mask_df) +
    geom_histogram(aes(x = Hoogte, y = after_stat(count / sum(count)))) +
    scale_x_continuous(breaks = seq(RoundAny(min(gebied_mask_df$Hoogte), interval_m, f = floor), 
                                    RoundAny(max(gebied_mask_df$Hoogte), interval_m, f = ceiling),
                                    interval_m)) +
    labs(y = "Relatieve frequentie") +
    theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.5)) +
    scale_y_continuous(labels = scales::percent)
  
  return(G)
}


HistogramAfgravingDTM <- function(gebied, interval_m = 0.5,
                                  gebieden = gebieden_sf,
                                  locatie = "Data/DTM gebieden afgraving/") {
  # Enkel voor het gebied op zich.
  
  gebied_spv <- gebieden_sf %>% 
    filter(Gebied == gebied) %>% 
    summarise() %>% 
    vect()
  
  gebied_raster <- rast(str_c(locatie,gebied, "_afgraving.tif")) %>% 
    `crs<-` ("EPSG:31370") %>% 
    `names<-` ("Hoogte")
  
  gebied_raster_narrow <- gebied_raster %>% 
    terra::crop(., gebied_spv, mask = TRUE) %>% 
    `crs<-`(str_c("EPSG:31370"))
  
  gebied_mask_df <- gebied_raster_narrow %>% 
    as.data.frame(xy = TRUE) %>% 
    mutate(Hoogte = RoundAny(Hoogte, interval_m, f = round))
  
  
  G <- ggplot(data = gebied_mask_df) +
    geom_histogram(aes(x = Hoogte, y = after_stat(count / sum(count)))) +
    scale_x_continuous(breaks = seq(RoundAny(min(gebied_mask_df$Hoogte), interval_m, f = floor), 
                                    RoundAny(max(gebied_mask_df$Hoogte), interval_m, f = ceiling),
                                    interval_m)) +
    labs(y = "Relatieve frequentie") +
    theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.5))
  
  return(G)
}


StatistiekDTM <- function(gebied, CI = 0.95, decimalen = 2,  gebieden = gebieden_sf) {
  
  locatie <- GebiedenDTMMask(gebieden_buffer = 0, gebieden = gebieden_sf_)
  gebied_raster <- rast(str_c(locatie[1],'/', "dtm", "_" , gebied, "_", locatie[2],".tif"))
  
  statistiek <- gebied_raster %>% 
    as.data.frame(xy = FALSE) %>% 
    summarise(Mean = mean(Hoogte) %>%  round(., decimalen),
              CI_l = RoundAny(t.test(Hoogte, conf.level = CI)$conf.int[1], decimalen/(10^decimalen), floor),
              CI_h = RoundAny(t.test(Hoogte, conf.level = CI)$conf.int[2], decimalen/(10^decimalen) , ceiling),
              Mean_CI = str_c(Mean ," (", CI_l ,"-", CI_h,")")) %>% 
    as.list()

    return(statistiek)
}







# Make raster-files ---------------------------------------------------------------------------------

## De analyse-tijd om een raster aan te maken + specifieke buffer is intensief.
## Als een raster van een gebied met specifieke buffer nog niet bestaat wordt dit door deze functie aangemaakt en 
## opgeslagen in een direction.
## Deze functies zijn opgenomen in de visualizerende-functies van de kaarten.
## Als een buffer-waarde wordt ingevoerd waarvan de raster nog niet gemaakt is, wordt deze eerst aangemaakt en opgeslagen.
## Wordt ineens uitgevoerd voor ALLE gebieden in de gebiedenlijst.
## Erna wordt de correcte raster geplot.

GebiedenDTM <- function(gebieden_buffer, 
                        gebieden = gebieden_sf,
                        DTM_Jaar = 2017, 
                        Locatie_DTM = "S:/Vlaanderen/Hoogte/DHMVII/DHMVIIDTMRAS1m.tif") {
  # Geeft de outputlocatie weer indien dit bestaat. 
  # Default om te maken staat op FALSE, als er een nieuwe buffer-versie gemaakt dient te worden dient de parameter Create op TRUE gezet te worden.
  # In principe maar 1x te maken. En enkel up te daten als er een nieuw dtm van Vlaanderen beschikbaar is.
  
  # Boolean to control if map exist.
  Create <- FALSE
  # Maak map aan om dtm's in op te slagen
  OutputLocationName <- str_c("DTM gebieden ", DTM_Jaar)
  if (!file.exists(str_c("Data/", OutputLocationName))) {
    dir.create(str_c("Data/", OutputLocationName))
  }
  OutputLocation <- str_c("Data",OutputLocationName,sep = "/")
  
  # Create a submap for the specific buffer if it dont exist. 
  OutputLocationName <- str_c(OutputLocationName, "/buffer_", gebieden_buffer)
  
  if (!file.exists(str_c("Data/", OutputLocationName))) {
    dir.create(str_c("Data/", OutputLocationName))
    Create <- TRUE
  }
  OutputLocation <- str_c("Data",OutputLocationName,sep = "/")
  
  if (Create) {
    # Read dtm Vlaanderen.
    Vlaanderen_dtm <- terra::rast(Locatie_DTM) %>% 
      `crs<-`("EPSG:31370")
    
    # Make from the geometry with same group-name one group and add buffer.
    gebieden_tot_sf <- gebieden %>% 
      group_by(Gebied) %>% 
      summarise(., .groups = "drop") %>% 
      st_buffer(., dist = gebieden_buffer)
    
    for (gebied in gebieden_tot_sf$Gebied) {
      
      gebied_vect <- subset(gebieden_tot_sf, Gebied == gebied) %>% 
        terra::vect()
      
      # Crop to keep square-format for mapping.
      gebied_dtm <- Vlaanderen_dtm %>% 
        terra::crop(., gebied_vect) %>% 
        `names<-`("Hoogte") %>% 
        `crs<-`(str_c("EPSG:31370"))
      
      writeRaster(x = gebied_dtm, filename = str_c(OutputLocation, "/","dtm_", gebied, "_", gebieden_buffer, ".tif"), overwrite = TRUE)
    }
  }
  return(list(OutputLocation, gebieden_buffer))
}

GebiedenDTMGeneral <- function(gebied_sf, gebieden_buffer, 
                        DTM_Jaar = 2017, 
                        Locatie_DTM = "S:/Vlaanderen/Hoogte/DHMVII/DHMVIIDTMRAS1m.tif") {
  # Geeft de outputlocatie weer indien dit bestaat. 
  # Default om te maken staat op FALSE, als er een nieuwe buffer-versie gemaakt dient te worden dient de parameter Create op TRUE gezet te worden.
  # In principe maar 1x te maken. En enkel up te daten als er een nieuw dtm van Vlaanderen beschikbaar is.

  gebied <- gebied_sf %>% 
    group_by(Gebied) %>% 
    summarise(., .groups = "drop") %>% 
    pull(Gebied) %>% 
    str_c(collapse = "_")
  
  # Maak map aan om dtm's in op te slagen
  OutputLocationName <- str_c("DTM gebieden ", DTM_Jaar)
  if (!file.exists(str_c("Data/", OutputLocationName))) {
    dir.create(str_c("Data/", OutputLocationName))
  }
  OutputLocation <- str_c("Data",OutputLocationName,sep = "/")
  
  # Create a submap for the specific buffer if it dont exist. 
  OutputLocationName <- str_c(OutputLocationName, "/buffer_", gebieden_buffer)
  
  if (!file.exists(str_c("Data/", OutputLocationName))) {
    dir.create(str_c("Data/", OutputLocationName))
  }
  OutputLocation <- str_c("Data", OutputLocationName,sep = "/")
  
  Name_file <- str_c(OutputLocation, "/dtm_" ,gebied, "_", gebieden_buffer, ".tif")
  
  if (!file.exists(Name_file)) {
    # Read dtm Vlaanderen.
    Vlaanderen_dtm <- terra::rast(Locatie_DTM) %>% 
      `crs<-`("EPSG:31370")
    
    gebied_vect <- gebied_sf %>% 
      terra::vect()
    
    gebied_dtm <- Vlaanderen_dtm %>% 
      terra::crop(., gebied_vect) %>% 
      `names<-`("Hoogte") %>% 
      `crs<-`(str_c("EPSG:31370"))
    
    writeRaster(x = gebied_dtm, filename = str_c(OutputLocation, "/","dtm_", gebied, "_", gebieden_buffer, 
                                                 ".tif"), overwrite = TRUE)

  }
  return(list(OutputLocation, gebied, gebieden_buffer))
}




GebiedenBodemkaart <- function(gebieden_buffer,
                               gebieden = gebieden_sf,
                               bodemkaart_jaar = 2015,
                               Locatie = "O:/Projects/PRJ_Schelde/Haven/Bodemkaart Havens") {
  
  Create <- FALSE # Assumption is that it already exist.
  
  # Bodemkaart from the area of Antwerp harbor. Is manually made from the big-file since otherwise it is too big to read it.
  # Apperently there is also a raster-file from the Bodemkaart. -> Possibility to replace it with this.
  locatie_bodemkaart <- str_c(Locatie, "/Bodemkaart_havengebieden_", bodemkaart_jaar, ".shp")
  
  # Maak map aan om gebieden in op te slagen
  OutputLocationName <- str_c("Bodemkaart gebieden ", bodemkaart_jaar)
  if (!file.exists(str_c("Data/", OutputLocationName))) {
    dir.create(str_c("Data/", OutputLocationName))
  }
  OutputLocation <- str_c("Data",OutputLocationName,sep = "/")
  
  # Maak submap voor eventuele verschillende buffers
  OutputLocationName <- str_c(OutputLocationName, "/buffer_", gebieden_buffer)
  
  if (!file.exists(str_c("Data/", OutputLocationName))) {
    dir.create(str_c("Data/", OutputLocationName))
    Create <- TRUE   # Kaarten bestaan nog niet.
  }
  OutputLocation <- str_c("Data",OutputLocationName,sep = "/")
  
  if (Create) {
    # Lees bodemkaart
    bodemkaart_havengebieden <- sf::st_read(locatie_bodemkaart) %>% 
      mutate(Generalisatie_voluit = case_when(
        Gegenerali == "Natte ZwKlei" ~ "Natte zware klei",
        Gegenerali == "Vochtige ZwKlei" ~ "Vochtige zware klei",
        Gegenerali == "Vochtig zand ant" ~ "Vochtig zand antropogeen",
        Gegenerali == "Droog zand antr" ~ "Droog zand antropogeen",
        Gegenerali == "Nat zand antr" ~ "Nat zand antropogeen",
        TRUE ~ Gegenerali
      ))
    st_agr(bodemkaart_havengebieden) = "constant"  # Avoid warning messages
    
    # Maak gebieden met zelfde naam als 1 polygoon, en verander NA in XXX
    gebieden_tot_sf <- gebieden %>% 
      dplyr::group_by(Gebied) %>% 
      dplyr::summarise(., .groups = "drop") %>% 
      st_buffer(., dist = gebieden_buffer)
  
    
    for (gebied in gebieden_tot_sf$Gebied) {
      
      gebied_sf <- subset(gebieden_tot_sf, Gebied == gebied)
      # Create box around polygon
      gebied_box <- st_bbox(gebied_sf) %>% st_as_sfc(., crs = 31370)

      bodemkaart_gebied <- bodemkaart_havengebieden %>% 
        st_intersection(., gebied_box)
      
      st_write(obj = bodemkaart_gebied, dsn = str_c(OutputLocation, "/","bodemkaart_", gebied, "_", gebieden_buffer, ".shp"))
    }
  }
  return(list(OutputLocation, gebieden_buffer))
}





GebiedenBodemkaartGeneral <- function(gebied_sf,
                                      gebieden_buffer,
                                      bodemkaart_jaar = 2015,
                                      Locatie = "O:/Projects/PRJ_Schelde/Haven/Bodemkaart Havens") {
  

  gebied <- gebied_sf %>% 
    group_by(Gebied) %>% 
    summarise(., .groups = "drop") %>% 
    pull(Gebied) %>% 
    str_c(., collapse = "_")
  
  # Bodemkaart from the area of Antwerp harbor. Is manually made from the big-file since otherwise it is too big to read it.
  # Apperently there is also a raster-file from the Bodemkaart. -> Possibility to replace it with this.
  locatie_bodemkaart <- str_c(Locatie, "/Bodemkaart_havengebieden_", bodemkaart_jaar, ".shp")
  
  # Maak map aan om gebieden in op te slagen
  OutputLocationName <- str_c("Bodemkaart gebieden ", bodemkaart_jaar)
  if (!file.exists(str_c("Data/", OutputLocationName))) {
    dir.create(str_c("Data/", OutputLocationName))
  }
  OutputLocation <- str_c("Data",OutputLocationName,sep = "/")
  
  # Maak submap voor eventuele verschillende buffers
  OutputLocationName <- str_c(OutputLocationName, "/buffer_", gebieden_buffer)
  
  if (!file.exists(str_c("Data/", OutputLocationName))) {
    dir.create(str_c("Data/", OutputLocationName))
  }
  OutputLocation <- str_c("Data",OutputLocationName,sep = "/")
  
  # Maak enkel kaart als deze nog niet bestaat.
  if (!file.exists(file.path(str_c(OutputLocation, "/", "Bodemkaart_", gebied, "_", gebieden_buffer, ".shp")))) {
    # Lees bodemkaart
    bodemkaart_havengebieden <- sf::st_read(locatie_bodemkaart) %>% 
      mutate(Generalisatie_voluit = case_when(
        Gegenerali == "Natte ZwKlei" ~ "Natte zware klei",
        Gegenerali == "Vochtige ZwKlei" ~ "Vochtige zware klei",
        Gegenerali == "Vochtig zand ant" ~ "Vochtig zand antropogeen",
        Gegenerali == "Droog zand antr" ~ "Droog zand antropogeen",
        Gegenerali == "Nat zand antr" ~ "Nat zand antropogeen",
        TRUE ~ Gegenerali
      ))
    st_agr(bodemkaart_havengebieden) = "constant"  # Avoid warning messages
    
    # Box of the coordinates of the area with buffer
    gebied_box <- gebied_sf %>% 
      st_buffer(., dist = gebieden_buffer) %>% 
      st_bbox(gebied_sf_buffer) %>% 
      st_as_sfc(., crs = 31370)
    
    bodemkaart_gebied <- bodemkaart_havengebieden %>% 
      st_intersection(., gebied_box)

    st_write(obj = bodemkaart_gebied, 
             dsn = str_c(OutputLocation, "/","Bodemkaart_", 
                         gebied, "_", gebieden_buffer, ".shp"))
  }
  return(list(OutputLocation, gebied, gebieden_buffer))
}






# Kaarten --------------------------------------------------------------------

AnnotationKaart <- function(kaart, referentie) {
  
  kaart <- kaart +
    annotation_scale(data = referentie,
                     location = "bl",
                     height = unit(0.25, "cm"),
                     pad_x = unit(0.45, "cm"),
                     pad_y = unit(0.0, "cm")) +
    annotation_north_arrow(data = referentie,
                           location = "bl",
                           style = north_arrow_fancy_orienteering(line_width = .5, text_size = 5),
                           pad_x = unit(0.0, "cm"),
                           pad_y = unit(0.0, "cm"),
                           height = unit(0.5, "cm"),
                           width = unit(0.45, "cm"))
  return(kaart)
}


# KaartGebiedOverzicht <- function(gebied, gebieden_buffer,
#                                       x_min = NULL, x_max = NULL, y_min = NULL, y_max = NULL,
#                                  interval_m = 1,
#                                  min_interval = NULL,
#                                  max_interval = NULL,
#                                  gebieden_sf_ = gebieden_sf,
#                                  schelde_dokken = TRUE,
#                                  afwateringen = TRUE,
#                                  legende_gebied = FALSE, annotation = TRUE,
#                                  colours_dtm = colours_dtm) {
#   # Make the overall dtm for an area with a certain buffer.
#   # PARAMETERS:
#   # gebied :  Name of the area.
#   # gebied_buffer: buffer area in meter.
#   # interval_m: Size of the intervalls in meter. Default is 1m. 
#   # min_interval: The mininum height from where the interval-size will start. Collects all the height below this number in one color.
#   # max_interval: The maximum height from where the interval-size will stop Collects all the height above this number in one color.
#   # gebieden_sf = spatial data with the area names and geometry.
#   # Schelde_dokken: add shapefiles (1) Schelde and (2) dokken to the map (Default = TRUE).
#   # Afwateringen: add shapefile with the drainage-creeks (Default = TRUE).
#   # legende_gebied: add the name of the "gebied" to the legend (Default = FALSE).
#   # annotation = TRUE: add scalebar and north-arrow (default = TRUE)
#   # RETURN: a quitte beautifull map with the height-intervals :) .
#   
#   
#   # SHAPEFILE
#   gebied_sf <- gebieden_sf_ %>% 
#     dplyr::filter(.data$Gebied == gebied) %>% 
#     summarise() %>% 
#     mutate(Gebied = gebied)
#   
#   # RASTER Gebied met buffer
#   locatie <- GebiedenDTM(gebieden_buffer = gebieden_buffer, gebieden = gebieden_sf_)
#   gebied_raster <- rast(str_c(locatie[1],'/', "dtm", "_" , gebied, "_", locatie[2],".tif"))
#   
#   Bijschrift_buffer <- ""
#   # Extend needs to be adapted !
#   if (any(!is.null(c(x_min, x_max, y_min, y_max)))) {
#     # Originele coordinaten (xmin, max etc)
#     locatie_mask <- GebiedenDTMMask()
#     gebied_raster_mask <- rast(str_c(locatie_mask[1], "/dtm_", gebied, "_" ,locatie_mask[2],".tif"))
#     
#     
#     # Replace NULL to gebied_buffer
#     list_coordinaten <- lst(x_min, x_max, y_min, y_max) %>% 
#       map(., ~ replace(.x, is.null(.x), gebieden_buffer))
#     
#     # make xlim and ylim negative
#     if (list_coordinaten$x_min > 0) {list_coordinaten$x_min <- -list_coordinaten$x_min}
#     if (list_coordinaten$y_min > 0) {list_coordinaten$y_min <- -list_coordinaten$y_min}
#     
#     
#     # New frame
#     if (any(c(abs(list_coordinaten$x_min), abs(list_coordinaten$x_max), abs(list_coordinaten$y_min), abs(list_coordinaten$y_max)) > gebieden_buffer)) {
#       return(str("INVALID: buffer dient groter te zijn als grootste absolute waarde van alternatieve buffer limieten!"))
#     }
#     
#     coordinaten_df <- data.frame(longitude = c(gebied_raster_mask %>% xmin() + list_coordinaten$x_min, gebied_raster_mask %>% xmax() + list_coordinaten$x_max),
#                                  latitude = c(gebied_raster_mask %>% ymin() + list_coordinaten$y_min, gebied_raster_mask %>% ymax() + list_coordinaten$y_max))
#     
#     
#     coordinaten_spv <- st_as_sf(coordinaten_df, coords = c("longitude", "latitude"), crs = 31370) %>% 
#       vect()
#     
#     gebied_raster <- crop(gebied_raster, coordinaten_spv)
#     
#     if (!is.null(y_max)) {Bijschrift_buffer <- str_c(Bijschrift_buffer, ", ", y_max , " m omgeving ten Noorden,")}
#     if (!is.null(x_max)) {Bijschrift_buffer <- str_c(Bijschrift_buffer, ", ", x_max , " m omgeving ten Oosten,") }
#     if (!is.null(y_min)) {Bijschrift_buffer <- str_c(Bijschrift_buffer, ", ", y_min , " m omgeving ten Zuiden,") }
#     if (!is.null(x_min)) {Bijschrift_buffer <- str_c(Bijschrift_buffer, ", ", x_min , " m omgeving ten Westen,") }
# 
#   }
#   
#   # Minmax van gebied
#   gebied_minmax <- gebied_raster %>% 
#     minmax()
#   
# 
#   # Indien ingegeven min-/max-interval uit range ligt van min en max gebied worden deze sws niet opgenomen!
#   rcl <-  matrix(c(seq(RoundAny(min(gebied_minmax), interval_m, floor),
#                        RoundAny(max(gebied_minmax), interval_m, ceiling),
#                        by = interval_m)))
#   # Intervallen die buiten range liggen worden 0 gezien dit sws niet wordt opgenomen bij de visualisatie van de intervallen (is gecontroleerd!)
#   if (!is.null(min_interval)) {if (!between(min_interval, min(gebied_minmax), max(gebied_minmax))) {min_interval <- NULL}}
#   if (!is.null(max_interval)) {if (!between(max_interval, min(gebied_minmax), max(gebied_minmax))) {max_interval <- NULL}}
#   # Determine the intervals
#   ## One of min/max interval not NULL.
#   if (!is.null(min_interval) || !is.null(max_interval)) {
#     ## Both not NULL
#     if (!is.null(min_interval) && !is.null(max_interval)) {
#       rcl <- matrix(c(RoundAny(min(gebied_minmax), interval_m, floor), 
#                       seq(RoundAny(min(min_interval), interval_m, floor), 
#                           RoundAny(max(max_interval), interval_m, ceiling),
#                           by = interval_m),
#                       RoundAny(max(gebied_minmax), interval_m, ceiling)))
#       ## Only min_interval is not NULL
#     } else if (!is.null(min_interval) && is.null(max_interval)) {
#       rcl <- matrix(c(RoundAny(min(gebied_minmax), interval_m, floor), 
#                       seq(RoundAny(min(min_interval), interval_m, floor), 
#                           RoundAny(max(gebied_minmax), interval_m, ceiling),
#                           by = interval_m)))
#       ## Only max_interval is not NULL
#     } else { 
#       rcl <- matrix(c(seq(RoundAny(min(gebied_minmax), interval_m, floor), 
#                           RoundAny(max(max_interval), interval_m, ceiling),
#                           by = interval_m),
#                       RoundAny(max(gebied_minmax), interval_m, ceiling)))
#     }
#   }
#   
#   # Classify the raster into new intervals.
#   gebied_raster_classify <- gebied_raster %>% 
#     classify(., rcl, include.lowest = TRUE, right = TRUE) 
#   
#   # Create palette
#   interval_kleuren <- length(rcl)
#   colors <- ColorPalette(colours_dtm, interval_kleuren)
#   
#   # tidyterra creert veel sneller grafiek !
#   G <- ggplot() +
#     tidyterra::geom_spatraster(data = gebied_raster_classify, aes(fill = Hoogte)) +
#     scale_fill_manual(values = colors) +
#     guides(fill = guide_legend(reverse=TRUE)) +
#     geom_sf(data = gebied_sf, alpha = 0, col = "black", lwd = 1, linetype = "solid") +
#     theme(axis.text = element_text(colour = inbo_steun_donkerroos), 
#           legend.text = element_text(color = inbo_steun_donkerroos),
#           panel.background = element_blank(),
#           axis.text.x = element_blank(),
#           axis.ticks.x = element_blank(),
#           axis.text.y = element_blank(),
#           axis.ticks.y = element_blank(),
#           legend.margin = margin(t = 0, b = 0, unit = "cm"),
#           plot.margin = margin(0, 0, 0, 0)) +
#     labs(fill = "Hoogte (m)") +
#     guides(fill = guide_legend(order = 1, reverse = TRUE))
#   
#   # Add legende van gebied
#   if (legende_gebied) {
#     G <- G + 
#       geom_sf(data = gebied_sf, aes(col = Gebied), alpha = 0, linetype = "solid") +
#       scale_color_manual(values = c("black")) +
#       guides(fill = guide_legend(order = 1, reverse = TRUE),
#              color = guide_legend(order = 2))
#   }
#   
#   # Add schelde en dokken
#   if (schelde_dokken) {
#     
#     # Clip the shapefile to same size as the raster.
#     clip_schelde_sf <- ClipShapefile(gebied_raster_classify, schelde_dokken_sf)
#     # Take apart Schelde and the other naems.
#     clip_schelde_dokken_sf <- clip_schelde_sf %>% 
#       mutate(Name = case_when(Name != "Schelde" ~ "Dokken",
#                               TRUE ~ Name)) %>% 
#       group_by(Name) %>% 
#       summarize()
#     
#     G <- G + 
#       new_scale_fill() +
#       geom_sf(data = clip_schelde_dokken_sf, aes(fill = Name), color = NA) +
#       scale_fill_manual(values = c("Schelde" = "dodgerblue3", "Dokken" = inbo_grijs)) +
#       labs(fill = "")
#   }
#   
#   # Add afwateringen
#   if (afwateringen) {
#     
#     clip_afwateringen_sf <- ClipShapefile(gebied_raster_classify, afwatering_sf) %>% 
#       mutate(Name = "Afwateringen") %>%
#       group_by(Name) %>% 
#       summarize()
#     
#     G <- G +
#       new_scale_color() +
#       geom_sf(data = clip_afwateringen_sf, aes(col = Name) , linewidth = 0.8
#               ) +
#       scale_color_manual(values = c("Afwateringen" = "blue1")) +
#       labs(col = "")
#   }
#   
#   # Add annotation
#   if (annotation) {
#     
#     G <- G %>% 
#       AnnotationKaart(., referentie = gebied_raster_classify)
#   }
#   
#   Onderschrift <- str_c("Reliëf van ", gebied, " en ", gebieden_buffer," m omgeving",
#                         Bijschrift_buffer ," met een hoogte-interval van ", interval_m, " m.")
#   
#   
#   
#   return(list(Kaart = G, Bijschrift = Onderschrift))
# }




KaartGebiedOverzicht <- function(gebied, gebieden_buffer,
                                 x_min = NULL, x_max = NULL, y_min = NULL, y_max = NULL,
                                 interval_m = 1,
                                 min_interval = NULL,
                                 max_interval = NULL,
                                 gebieden_sf_ = gebieden_sf,
                                 schelde_dokken = TRUE,
                                 afwateringen = TRUE,
                                 legende_gebied = FALSE, annotation = TRUE,
                                 colours_dtm_ = colours_dtm,
                                 zomerpeil = NULL, 
                                 colours_zomerpeil_ = colours_zomerpeil) {
  # Make the overall dtm for an area with a certain buffer.
  # PARAMETERS:
  # gebied :  Name of the area.
  # gebied_buffer: buffer area in meter.
  # interval_m: Size of the intervalls in meter. Default is 1m. 
  # min_interval: The mininum height from where the interval-size will start. Collects all the height below this number in one color.
  # max_interval: The maximum height from where the interval-size will stop Collects all the height above this number in one color.
  # gebieden_sf = spatial data with the area names and geometry.
  # Schelde_dokken: add shapefiles (1) Schelde and (2) dokken to the map (Default = TRUE).
  # Afwateringen: add shapefile with the drainage-creeks (Default = TRUE).
  # legende_gebied: add the name of the "gebied" to the legend (Default = FALSE).
  # annotation = TRUE: add scalebar and north-arrow (default = TRUE)
  # RETURN: a quitte beautifull map with the height-intervals :) .
  
  # If zomerpeil ! geom_spatrasters RESAMPLES ! -> Low frequency values will go away! Check manually and adapt min_interval
  
  # SHAPEFILE
  gebied_sf <- gebieden_sf_ %>% 
    dplyr::filter(.data$Gebied == gebied) %>% 
    summarise() %>% 
    mutate(Gebied = gebied)
  
  # RASTER Gebied met buffer
  locatie <- GebiedenDTM(gebieden_buffer = gebieden_buffer, gebieden = gebieden_sf_)
  gebied_raster <- rast(str_c(locatie[1],'/', "dtm", "_" , gebied, "_", locatie[2],".tif"))
  
  Bijschrift_buffer <- ""
  # Extend needs to be adapted !
  if (any(!is.null(c(x_min, x_max, y_min, y_max)))) {
    # Originele coordinaten (xmin, max etc)
    locatie_mask <- GebiedenDTMMask()
    gebied_raster_mask <- rast(str_c(locatie_mask[1], "/dtm_", gebied, "_" ,locatie_mask[2],".tif"))
    
    
    # Replace NULL to gebied_buffer
    list_coordinaten <- lst(x_min, x_max, y_min, y_max) %>% 
      map(., ~ replace(.x, is.null(.x), gebieden_buffer))
    
    # make xlim and ylim negative
    if (list_coordinaten$x_min > 0) {list_coordinaten$x_min <- -list_coordinaten$x_min}
    if (list_coordinaten$y_min > 0) {list_coordinaten$y_min <- -list_coordinaten$y_min}
    
    
    # New frame
    if (any(c(abs(list_coordinaten$x_min), abs(list_coordinaten$x_max), abs(list_coordinaten$y_min), abs(list_coordinaten$y_max)) > gebieden_buffer)) {
      return(str("INVALID: buffer dient groter te zijn als grootste absolute waarde van alternatieve buffer limieten!"))
    }
    
    coordinaten_df <- data.frame(longitude = c(gebied_raster_mask %>% xmin() + list_coordinaten$x_min, gebied_raster_mask %>% xmax() + list_coordinaten$x_max),
                                 latitude = c(gebied_raster_mask %>% ymin() + list_coordinaten$y_min, gebied_raster_mask %>% ymax() + list_coordinaten$y_max))
    
    
    coordinaten_spv <- st_as_sf(coordinaten_df, coords = c("longitude", "latitude"), crs = 31370) %>% 
      vect()
    
    gebied_raster <- crop(gebied_raster, coordinaten_spv)
    
    if (!is.null(y_max)) {Bijschrift_buffer <- str_c(Bijschrift_buffer, ", ", y_max , " m omgeving ten Noorden,")}
    if (!is.null(x_max)) {Bijschrift_buffer <- str_c(Bijschrift_buffer, ", ", x_max , " m omgeving ten Oosten,") }
    if (!is.null(y_min)) {Bijschrift_buffer <- str_c(Bijschrift_buffer, ", ", y_min , " m omgeving ten Zuiden,") }
    if (!is.null(x_min)) {Bijschrift_buffer <- str_c(Bijschrift_buffer, ", ", x_min , " m omgeving ten Westen,") }
    
  }
  
  # Minmax van gebied
  gebied_minmax <- gebied_raster %>% 
    minmax()
  
  # Indien ingegeven min-/max-interval uit range ligt van min en max gebied worden deze sws niet opgenomen!
  rcl <-  matrix(c(seq(RoundAny(min(gebied_minmax), interval_m, floor),
                       RoundAny(max(gebied_minmax), interval_m, ceiling),
                       by = interval_m)))
  # Intervallen die buiten range liggen worden 0 gezien dit sws niet wordt opgenomen bij de visualisatie van de intervallen (is gecontroleerd!)
  if (!is.null(min_interval)) {
    if (!between(min_interval, min(gebied_minmax), max(gebied_minmax))) {
      min_interval <- NULL}
    }
  if (!is.null(max_interval)) {
    if (!between(max_interval, min(gebied_minmax), max(gebied_minmax))) {max_interval <- NULL}
    }
  # Determine the intervals
  ## One of min/max interval not NULL.
  if (!is.null(min_interval) || !is.null(max_interval)) {
    ## Both not NULL
    if (!is.null(min_interval) && !is.null(max_interval)) {
      rcl <- matrix(c(RoundAny(min(gebied_minmax), interval_m, floor), 
                      seq(RoundAny(min(min_interval), interval_m, floor), 
                          RoundAny(max(max_interval), interval_m, ceiling),
                          by = interval_m),
                      RoundAny(max(gebied_minmax), interval_m, ceiling)))
      ## Only min_interval is not NULL
    } else if (!is.null(min_interval) && is.null(max_interval)) {
      rcl <- matrix(c(RoundAny(min(gebied_minmax), interval_m, floor), 
                      seq(RoundAny(min(min_interval), interval_m, floor), 
                          RoundAny(max(gebied_minmax), interval_m, ceiling),
                          by = interval_m)))
      ## Only max_interval is not NULL
    } else { 
      rcl <- matrix(c(seq(RoundAny(min(gebied_minmax), interval_m, floor), 
                          RoundAny(max(max_interval), interval_m, ceiling),
                          by = interval_m),
                      RoundAny(max(gebied_minmax), interval_m, ceiling)))
    }
  }
  
  # Classify the raster into new intervals.
  gebied_raster_classify <- gebied_raster %>% 
    classify(., rcl, include.lowest = TRUE, right = TRUE) 
  
  # If zomerpeil, get all values under it in other color.
  if (!is.null(zomerpeil)) {
    interval_zomperpeil <- gebied_raster_classify %>% 
      as.data.frame() %>% 
      distinct(Hoogte) %>% 
      mutate(
        Hoogte = str_sub(Hoogte, 2, -2) %>% 
          str_replace(., "[0-9.-]*$", "") %>% 
          str_sub(., 1, -2) %>% 
          as.numeric()
      ) %>% 
      filter(Hoogte <= zomerpeil)
    
    interval_kleuren_zomerpeil <- nrow(interval_zomperpeil) - 1
    colors_zomerpeil <- ColorPalette(colours_zomerpeil_, interval_kleuren_zomerpeil)
    
    interval_kleuren <- length(rcl) -  nrow(interval_zomperpeil)
    colors <- ColorPalette(colours_dtm_, interval_kleuren)
    colors <- c(colors_zomerpeil, colors)
  } else {
    # Create palette
    interval_kleuren <- length(rcl)
    colors <- ColorPalette(colours_dtm_, interval_kleuren)
  }
 
  # tidyterra creert veel sneller grafiek !
  G <- ggplot() +
    tidyterra::geom_spatraster(data = gebied_raster_classify, aes(fill = Hoogte)) +
    scale_fill_manual(values = colors) +
    guides(fill = guide_legend(reverse = TRUE)) +
    geom_sf(data = gebied_sf, alpha = 0, col = "black", lwd = 1, linetype = "solid") +
    coord_sf(crs = st_crs(31370), expand = FALSE) + 
    theme(axis.text = element_text(colour = inbo_steun_donkerroos), 
          legend.text = element_text(color = inbo_steun_donkerroos),
          panel.background = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          legend.margin = margin(t = 0, b = 0, unit = "cm"),
          plot.margin = margin(0, 0, 0, 0)) +
    labs(fill = "Hoogte (m)") +
    guides(fill = guide_legend(ncol = 2, order = 1, reverse = TRUE))

  
  # Add legende van gebied
  if (legende_gebied) {
    G <- G + 
      geom_sf(data = gebied_sf, aes(col = Gebied), alpha = 0, linetype = "solid") +
      scale_color_manual(values = c("black")) +
      guides(fill = guide_legend(order = 1, reverse = TRUE),
             color = guide_legend(order = 2))
  }
  
  # Add schelde en dokken
  if (schelde_dokken) {
    
    # Clip the shapefile to same size as the raster.
    clip_schelde_sf <- ClipShapefile(gebied_raster_classify, schelde_dokken_sf)
    # Take apart Schelde and the other naems.
    clip_schelde_dokken_sf <- clip_schelde_sf %>% 
      mutate(Name = case_when(Name != "Schelde" ~ "Dokken",
                              TRUE ~ Name)) %>% 
      group_by(Name) %>% 
      summarize()
    
    G <- G + 
      new_scale_fill() +
      geom_sf(data = clip_schelde_dokken_sf, aes(fill = Name), color = NA) +
      scale_fill_manual(values = c("Schelde" = "dodgerblue3", "Dokken" = inbo_grijs)) +
      labs(fill = "")
  }
  
  # Add afwateringen
  if (afwateringen) {
    
    clip_afwateringen_sf <- ClipShapefile(gebied_raster_classify, afwatering_sf) %>% 
      mutate(Name = "Afwateringen") %>%
      group_by(Name) %>% 
      summarize()
    
    G <- G +
      new_scale_color() +
      geom_sf(data = clip_afwateringen_sf, aes(col = Name) , linewidth = 0.8
      ) +
      scale_color_manual(values = c("Afwateringen" = "blue1")) +
      labs(col = "")
  }
  
  # Add annotation
  if (annotation) {
    
    G <- G %>% 
      AnnotationKaart(., referentie = gebied_raster_classify)
  }
  
  Onderschrift <- str_c("Reliëf van ", gebied, " en ", gebieden_buffer," m omgeving",
                        Bijschrift_buffer ," met een hoogte-interval van ", interval_m, " m.")
  
  if (!is.null(zomerpeil)) {
    Onderschrift <- str_c(Onderschrift, " Het zomerpeil is gemiddeld ", zomerpeil, " mTAW.")
  }
  
  
  return(list(Kaart = G, Bijschrift = Onderschrift))
}


KaartGebiedOverzichtGeneral <- function(gebied_sf, gebieden_buffer,
                                 x_min = NULL, x_max = NULL, y_min = NULL, y_max = NULL,
                                 interval_m = 1,
                                 min_interval = NULL,
                                 max_interval = NULL,
                                 schelde_dokken = TRUE,
                                 afwateringen = TRUE,
                                 legende_gebied = FALSE, annotation = TRUE,
                                 dtm_colours = colours_dtm,
                                 zomerpeil = NULL, 
                                 zomerpeil_colours = colours_zomerpeil) {
  # Make the overall dtm for an area with a certain buffer.
  # PARAMETERS:
  # gebied :  Name of the area.
  # gebied_buffer: buffer area in meter.
  # interval_m: Size of the intervalls in meter. Default is 1m. 
  # min_interval: The mininum height from where the interval-size will start. Collects all the height below this number in one color.
  # max_interval: The maximum height from where the interval-size will stop Collects all the height above this number in one color.
  # gebieden_sf = spatial data with the area names and geometry.
  # Schelde_dokken: add shapefiles (1) Schelde and (2) dokken to the map (Default = TRUE).
  # Afwateringen: add shapefile with the drainage-creeks (Default = TRUE).
  # legende_gebied: add the name of the "gebied" to the legend (Default = FALSE).
  # annotation = TRUE: add scalebar and north-arrow (default = TRUE)
  # RETURN: a quitte beautifull map with the height-intervals :) .
  
  # If zomerpeil ! geom_spatrasters RESAMPLES ! -> Low frequency values will go away! Check manually and adapt min_interval
  # SHAPEFILE

  # RASTER Gebied met buffer
  locatie <- GebiedenDTMGeneral(gebied_sf, gebieden_buffer)
  
  gebied_raster <- rast(str_c(locatie[1],'/', "dtm", "_" , locatie[2], "_", locatie[3],".tif"))
  
  Bijschrift_buffer <- ""
  # Extend needs to be adapted !
  if (any(!is.null(c(x_min, x_max, y_min, y_max)))) {
    # Originele coordinaten (xmin, max etc)
    locatie_mask <- GebiedenDTMMaskGeneral(gebied_sf)
    gebied_raster_mask <- rast(str_c(locatie_mask[1], "/dtm_", locatie_mask[2], "_" ,locatie_mask[3],".tif"))
    
    # Replace NULL to gebied_buffer
    list_coordinaten <- lst(x_min, x_max, y_min, y_max) %>% 
      map(., ~ replace(.x, is.null(.x), gebieden_buffer))
    
    # make xlim and ylim negative
    if (list_coordinaten$x_min > 0) {list_coordinaten$x_min <- -list_coordinaten$x_min}
    if (list_coordinaten$y_min > 0) {list_coordinaten$y_min <- -list_coordinaten$y_min}
    
    
    # New frame
    if (any(c(abs(list_coordinaten$x_min), abs(list_coordinaten$x_max), abs(list_coordinaten$y_min), abs(list_coordinaten$y_max)) > gebieden_buffer)) {
      return(str("INVALID: buffer dient groter te zijn als grootste absolute waarde van alternatieve buffer limieten!"))
    }
    
    coordinaten_df <- data.frame(longitude = c(gebied_raster_mask %>% xmin() + list_coordinaten$x_min,
                                               gebied_raster_mask %>% xmax() + list_coordinaten$x_max),
                                 latitude = c(gebied_raster_mask %>% ymin() + list_coordinaten$y_min,
                                              gebied_raster_mask %>% ymax() + list_coordinaten$y_max))
    
    
    coordinaten_spv <- st_as_sf(coordinaten_df, coords = c("longitude", "latitude"), crs = 31370) %>% 
      vect()
    
    gebied_raster <- crop(gebied_raster, coordinaten_spv)
    
    if (!is.null(y_max)) {Bijschrift_buffer <- str_c(Bijschrift_buffer, ", ", y_max , " m omgeving ten Noorden,")}
    if (!is.null(x_max)) {Bijschrift_buffer <- str_c(Bijschrift_buffer, ", ", x_max , " m omgeving ten Oosten,") }
    if (!is.null(y_min)) {Bijschrift_buffer <- str_c(Bijschrift_buffer, ", ", y_min , " m omgeving ten Zuiden,") }
    if (!is.null(x_min)) {Bijschrift_buffer <- str_c(Bijschrift_buffer, ", ", x_min , " m omgeving ten Westen,") }
    
  }
  
  # Minmax van gebied
  gebied_minmax <- gebied_raster %>% 
    minmax()
  
  # Indien ingegeven min-/max-interval uit range ligt van min en max gebied worden deze sws niet opgenomen!
  rcl <-  matrix(c(seq(RoundAny(min(gebied_minmax), interval_m, floor),
                       RoundAny(max(gebied_minmax), interval_m, ceiling),
                       by = interval_m)))
  # Intervallen die buiten range liggen worden 0 gezien dit sws niet wordt opgenomen bij de visualisatie van de intervallen (is gecontroleerd!)
  if (!is.null(min_interval)) {
    if (!between(min_interval, min(gebied_minmax), max(gebied_minmax))) {
      min_interval <- NULL}
  }
  if (!is.null(max_interval)) {
    if (!between(max_interval, min(gebied_minmax), max(gebied_minmax))) {max_interval <- NULL}
  }
  # Determine the intervals
  ## One of min/max interval not NULL.
  if (!is.null(min_interval) || !is.null(max_interval)) {
    ## Both not NULL
    if (!is.null(min_interval) && !is.null(max_interval)) {
      rcl <- matrix(c(RoundAny(min(gebied_minmax), interval_m, floor), 
                      seq(RoundAny(min(min_interval), interval_m, floor), 
                          RoundAny(max(max_interval), interval_m, ceiling),
                          by = interval_m),
                      RoundAny(max(gebied_minmax), interval_m, ceiling)))
      ## Only min_interval is not NULL
    } else if (!is.null(min_interval) && is.null(max_interval)) {
      rcl <- matrix(c(RoundAny(min(gebied_minmax), interval_m, floor), 
                      seq(RoundAny(min(min_interval), interval_m, floor), 
                          RoundAny(max(gebied_minmax), interval_m, ceiling),
                          by = interval_m)))
      ## Only max_interval is not NULL
    } else { 
      rcl <- matrix(c(seq(RoundAny(min(gebied_minmax), interval_m, floor), 
                          RoundAny(max(max_interval), interval_m, ceiling),
                          by = interval_m),
                      RoundAny(max(gebied_minmax), interval_m, ceiling)))
    }
  }
  
  # Classify the raster into new intervals.
  gebied_raster_classify <- gebied_raster %>% 
    classify(., rcl, include.lowest = TRUE, right = TRUE) 
  
  # If zomerpeil, get all values under it in other color.
  if (!is.null(zomerpeil)) {
    interval_zomperpeil <- gebied_raster_classify %>% 
      as.data.frame() %>% 
      distinct(Hoogte) %>% 
      mutate(
        Hoogte = str_sub(Hoogte, 2, -2) %>% 
          str_replace(., "[0-9.-]*$", "") %>% 
          str_sub(., 1, -2) %>% 
          as.numeric()
      ) %>% 
      filter(Hoogte <= zomerpeil)
    
    interval_kleuren_zomerpeil <- nrow(interval_zomperpeil) - 1
    zomerpeil_colors <- ColorPalette(zomerpeil_colours, interval_kleuren_zomerpeil)
    
    interval_kleuren <- length(rcl) -  nrow(interval_zomperpeil)
    colors <- ColorPalette(dtm_colours, interval_kleuren)
    colors <- c(colors_zomerpeil, colors)
  } else {
    # Create palette
    interval_kleuren <- length(rcl)
    colors <- ColorPalette(dtm_colours, interval_kleuren)
  }
  
  # tidyterra creert veel sneller grafiek !
  G <- ggplot() +
    tidyterra::geom_spatraster(data = gebied_raster_classify, aes(fill = Hoogte)) +
    scale_fill_manual(values = colors) +
    guides(fill = guide_legend(reverse = TRUE)) +
    geom_sf(data = gebied_sf, alpha = 0, col = "black", lwd = 1, linetype = "solid") +
    coord_sf(crs = st_crs(31370), expand = FALSE) + 
    theme(axis.text = element_text(colour = inbo_steun_donkerroos), 
          legend.text = element_text(color = inbo_steun_donkerroos),
          panel.background = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          legend.margin = margin(t = 0, b = 0, unit = "cm"),
          plot.margin = margin(0, 0, 0, 0)) +
    labs(fill = "Hoogte (m)") +
    guides(fill = guide_legend(order = 1, reverse = TRUE))
  
  # Add legende van gebied
  if (legende_gebied) {
    G <- G + 
      geom_sf(data = gebied_sf, aes(col = Gebied), alpha = 0, linetype = "solid") +
      scale_color_manual(values = c("black")) +
      guides(fill = guide_legend(order = 1, reverse = TRUE),
             color = guide_legend(order = 2))
  }
  
  # Add schelde en dokken
  if (schelde_dokken) {
    
    # Clip the shapefile to same size as the raster.
    clip_schelde_sf <- ClipShapefile(gebied_raster_classify, schelde_dokken_sf)
    # Take apart Schelde and the other naems.
    clip_schelde_dokken_sf <- clip_schelde_sf %>% 
      mutate(Name = case_when(Name != "Schelde" ~ "Dokken",
                              TRUE ~ Name)) %>% 
      group_by(Name) %>% 
      summarize()
    
    G <- G + 
      new_scale_fill() +
      geom_sf(data = clip_schelde_dokken_sf, aes(fill = Name), color = NA) +
      scale_fill_manual(values = c("Schelde" = "dodgerblue3", "Dokken" = "dodgerblue3")) +
      labs(fill = "")
  }
  
  # Add afwateringen
  if (afwateringen) {
    
    clip_afwateringen_sf <- ClipShapefile(gebied_raster_classify, afwatering_sf) %>% 
      mutate(Name = "Afwateringen") %>%
      group_by(Name) %>% 
      summarize()
    
    G <- G +
      new_scale_color() +
      geom_sf(data = clip_afwateringen_sf, aes(col = Name) , linewidth = 0.8
      ) +
      scale_color_manual(values = c("Afwateringen" = "blue1")) +
      labs(col = "")
  }
  
  # Add annotation
  if (annotation) {
    
    G <- G %>% 
      AnnotationKaart(., referentie = gebied_raster_classify)
  }
  
  gebied <- locatie[2] %>% 
    str_replace(., "_", ", ")
    
  Onderschrift <- str_c("Reliëf van ", gebied, " en ", gebieden_buffer, "m omgeving",
                        Bijschrift_buffer ," met een hoogte-interval van ", interval_m, "m.")
  
  if (!is.null(zomerpeil)) {
    Onderschrift <- str_c(Onderschrift, " Het zomerpeil is gemiddeld ", zomerpeil, " mTAW.")
  }
  
  
  return(list(Kaart = G, Bijschrift = Onderschrift))
}



KaartGebiedDetailDTM <- function(gebied,
                                 interval_m = 0.2,
                                 min_interval = NULL,
                                 max_interval = NULL,
                                 gebieden_sf_ = gebieden_sf, 
                                 locatie = "Data/DTM gebieden afgraving/",
                                 legende_gebied = FALSE, annotation = TRUE, afwateringen = TRUE,
                                 schelde_dokken = TRUE,
                                 colours_dtm_ = colours_dtm,
                                 zomerpeil = NULL, 
                                 colours_zomerpeil_ = colours_zomerpeil) { 
  # Make the detailed dtm for an area if there is an raster for it.
  # Only for Doelpolder Noord and Drijdijk
  # PARAMETERS:
  # gebied :  Name of the area.
  # interval_m: Size of the intervalls in meter. Default is 0.2m. 
  # min_interval: The mininum height from where the interval-size will start. Collects all the height below this number in one color.
  # max_interval: The maximum height from where the interval-size will stop Collects all the height above this number in one color.
  # gebieden_sf = spatial data with the area names and geometry.
  # Schelde_dokken: add shapefiles (1) Schelde and (2) dokken to the map (Default = TRUE).
  # Afwateringen: add shapefile with the drainage-creeks (Default = TRUE).
  # legende_gebied: add the name of the "gebied" to the legend (Default = FALSE).
  # annotation = TRUE: add scalebar and north-arrow (default = TRUE)
  # RETURN: a quitte beautifull map with the height-intervals :) 
  
  

  # Shapfile gebied
  gebied_sf <- gebieden_sf_ %>% 
    dplyr::filter(Gebied == gebied) %>% 
    summarise() %>% 
    mutate(Gebied = gebied)
  
  # raster detail gebied
  gebied_detail_rast <-  rast(str_c(locatie,gebied, "_afgraving.tif")) %>% 
    `crs<-` ("EPSG:31370") %>% 
    `names<-` ("Hoogte")

  # Bepaal interval grootte
  gebied_minmax <- gebied_detail_rast %>% 
    minmax()
  
  # Ga hiervan uit. Indien ingegeven interval uit range ligt van min en max gebied worden deze sws niet opgenomen!
  # (Gecontrolleerd!)
  rcl <-  matrix(c(seq(RoundAny(min(gebied_minmax), interval_m, floor),
                       RoundAny(max(gebied_minmax), interval_m, ceiling),
                       by = interval_m)))
  # Intervallen die buiten range liggen worden 0. 
  if (!is.null(min_interval)) {if (!between(min_interval, min(gebied_minmax), max(gebied_minmax))) {min_interval <- NULL}}
  if (!is.null(max_interval)) {if (!between(max_interval, min(gebied_minmax), max(gebied_minmax))) {max_interval <- NULL}}
  # Intervallen die buiten range van gebied liggen worden sws niet meegenomen !
  if (!is.null(min_interval) || !is.null(max_interval)) {
    if (!is.null(min_interval) && !is.null(max_interval)) {
       rcl <- matrix(c(RoundAny(min(gebied_minmax), interval_m, floor), 
                       seq(RoundAny(min(min_interval), interval_m, floor), 
                           RoundAny(max(max_interval), interval_m, ceiling),
                           by = interval_m),
                       RoundAny(max(gebied_minmax), interval_m, ceiling)))
    } else if (!is.null(min_interval) && is.null(max_interval)) {
      rcl <- matrix(c(RoundAny(min(gebied_minmax), interval_m, floor), 
                      seq(RoundAny(min(min_interval), interval_m, floor), 
                          RoundAny(max(gebied_minmax), interval_m, ceiling),
                          by = interval_m)))
    } else { 
      rcl <- matrix(c(seq(RoundAny(min(gebied_minmax), interval_m, floor), 
                          RoundAny(max(max_interval), interval_m, ceiling),
                          by = interval_m),
                      RoundAny(max(gebied_minmax), interval_m, ceiling)))
      }
    }
    
  gebied_raster_classify <- gebied_detail_rast %>% 
    classify(., rcl, include.lowest = TRUE, right = TRUE) 
  
  # If zomerpeil, get all values under it in other color.
  if (!is.null(zomerpeil)) {
    interval_zomperpeil <- gebied_raster_classify %>% 
      as.data.frame() %>% 
      distinct(Hoogte) %>% 
      mutate(
        Hoogte = str_sub(Hoogte, 2, -2) %>% 
          str_replace(., "[0-9.-]*$", "") %>% 
          str_sub(., 1, -2) %>% 
          as.numeric()
      ) %>% 
      filter(Hoogte < zomerpeil)
    
    interval_kleuren_zomerpeil <- nrow(interval_zomperpeil) - 1
    colors_zomerpeil <- ColorPalette(colours_zomerpeil_, interval_kleuren_zomerpeil)
    
    interval_kleuren <- length(rcl) -  nrow(interval_zomperpeil)
    colors <- ColorPalette(colours_dtm_, interval_kleuren)
    colors <- c(colors_zomerpeil, colors)
  } else {
    # Create palette
    interval_kleuren <- length(rcl)
    colors <- ColorPalette(colours_dtm_, interval_kleuren)
  }

  G <- ggplot() +
    tidyterra::geom_spatraster(data = gebied_raster_classify, aes(fill = Hoogte)) +
    scale_fill_manual(values = colors) +
    guides(fill = guide_legend(reverse=TRUE)) +
    geom_sf(data = gebied_sf, alpha = 0, linetype = "solid", lwd = 1, col = "black") +
    labs(fill = "Hoogte (m)") +
    theme(axis.text = element_text(colour = inbo_steun_donkerroos), 
          legend.text = element_text(color = inbo_steun_donkerroos),
          panel.background = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank()) +
    guides(fill = guide_legend(order = 1, reverse = TRUE))
  
  if (legende_gebied) {
    G <- G + 
      geom_sf(data = gebied_sf, aes(color = Gebied), alpha = 0, linetype = "solid") +
      scale_color_manual(values = c("black")) +
      guides(fill = guide_legend(order = 1, reverse = TRUE),
             color = guide_legend(order = 2))
  }
  if (annotation) {
    G <- G %>% 
      AnnotationKaart(., referentie = gebied_raster_classify)
  }
  
  if (schelde_dokken) {
    
    # Clip the shapefile to same size as the raster.
    clip_schelde_sf <- ClipShapefile(gebied_raster_classify, schelde_dokken_sf)
    # Take apart Schelde and the other naems.
    clip_schelde_dokken_sf <- clip_schelde_sf %>% 
      mutate(Name = case_when(Name != "Schelde" ~ "Dokken",
                              TRUE ~ Name)) %>% 
      group_by(Name) %>% 
      summarize()
    
    G <- G + 
      new_scale_fill() +
      geom_sf(data = clip_schelde_dokken_sf, aes(fill = Name), color = NA) +
      scale_fill_manual(values = c("Schelde" = "dodgerblue3", "Dokken" = inbo_grijs)) +
      labs(fill = "")
  }
  
  # Add afwateringen
  if (afwateringen) {
    
    clip_afwateringen_sf <- ClipShapefile(gebied_raster_classify, afwatering_sf) %>% 
      mutate(Name = "Afwateringen") %>%
      group_by(Name) %>% 
      summarize()
    
    G <- G +
      new_scale_color() +
      geom_sf(data = clip_afwateringen_sf, aes(col = Name), linewidth = 0.8) +
      scale_color_manual(values = c("Afwateringen" = "blue1")) +
      labs(col = "")
  }
  
  
  Onderschrift <- str_c("Reliëf van afgegraven ", gebied, " met een hoogte-interval van ", interval_m, " m.")
  
  if (!is.null(zomerpeil)) {
    Onderschrift <- str_c(Onderschrift, " Het zomerpeil is gemiddeld ", zomerpeil, " mTAW.")
  }
  
  return(list(Kaart = G, Bijschrift = Onderschrift))
}


KaartBodemsamenstelling <- function(gebied, gebieden_buffer, 
                                    gebieden_sf_ = gebieden_sf,
                                    legende_gebied = F, annotation = T) {
  
  locatie <- GebiedenBodemkaart(gebieden_buffer = gebieden_buffer, gebieden = gebieden_sf_)
  
  Gebied_bodemkaart <- st_read(dsn = str_c(locatie[1], "/bodemkaart_", gebied, "_", locatie[2],".shp")) %>% 
    group_by(Gnrlst_) %>% 
    summarise(., .groups = "drop") 
  
  gebied_sf <- gebieden_sf_ %>% 
    dplyr::filter(Gebied == gebied) %>% 
    summarise() %>% 
    mutate(Gebied = gebied)

  G <- ggplot() +
    geom_sf(data = Gebied_bodemkaart, aes(fill = Gnrlst_)) +
    scale_fill_manual(values = colours_bodemkaart) +
    guides(fill = guide_legend(reverse=FALSE)) +
    geom_sf(data = gebied_sf, alpha = 0, color = "black", linetype = "solid", lwd = 1,) +
    theme(axis.text = element_text(colour = inbo_steun_donkerroos), 
          legend.text = element_text(color = inbo_steun_donkerroos),
          panel.background = element_blank(), 
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank()) +
    labs(fill = "Bodemtype", col = "Gebied")
  
  
  if (legende_gebied) {
    G <- G + 
      geom_sf(data = gebied_sf, aes(color = Gebied), alpha = 0, linetype = "dashed") +
      scale_color_manual(values = c("black")) +
      guides(fill = guide_legend(order = 1, reverse = TRUE),
             color = guide_legend(order = 2))
  }
  
  if (annotation) { 
    G <- G %>% 
      AnnotationKaart(., referentie = Gebied_bodemkaart)
    
    }
  
  Onderschrift <- str_c("Bodemkaart van ", gebied, " en ", gebieden_buffer, " m omgeving.")
  return(list(Kaart = G, Bijschrift = Onderschrift))
}



KaartBodemsamenstellingGeneral <- function(gebied_sf, 
                                           gebieden_buffer, 
                                           legende_gebied = F, 
                                           annotation = T) {
  
  locatie <- GebiedenBodemkaartGeneral(gebied_sf, gebieden_buffer)
  
  Gebied_bodemkaart <- st_read(dsn = str_c(locatie[1], "/bodemkaart_", locatie[2], "_", locatie[3],".shp")) %>% 
    group_by(Gnrlst_) %>% 
    summarise(., .groups = "drop") 
  
  gebied_sf_summarize <- gebied_sf %>% 
    group_by(Gebied) %>% 
    summarise(., .groups = "drop")
  
  G <- ggplot() +
    geom_sf(data = Gebied_bodemkaart, aes(fill = Gnrlst_)) +
    scale_fill_manual(values = colours_bodemkaart) +
    guides(fill = guide_legend(reverse=FALSE)) +
    geom_sf(data = gebied_sf_summarize, alpha = 0, color = "black", linetype = "solid", lwd = 1,) +
    theme(axis.text = element_text(colour = inbo_steun_donkerroos), 
          legend.text = element_text(color = inbo_steun_donkerroos),
          panel.background = element_blank(), 
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank()) +
    labs(fill = "Bodemtype", col = "Gebied")
  
  
  if (legende_gebied) {
    G <- G + 
      geom_sf(data = gebied_sf_summarize, color = "black", alpha = 0, linetype = "dashed") +
      guides(fill = guide_legend(order = 1, reverse = TRUE),
             color = guide_legend(order = 2))
  }
  
  if (annotation) { 
    G <- G %>% 
      AnnotationKaart(., referentie = Gebied_bodemkaart)
    
  }
  
  gebied <- locatie[2] %>% 
    str_replace(., "_", ", ")
  
  
  Onderschrift <- str_c("Bodemkaart van ", gebied, " en ", gebieden_buffer, "m omgeving.")
  return(list(Kaart = G, Bijschrift = Onderschrift))
}




KaartWaterpeil <- function(gebied, gebieden_buffer = 0, Peil, titel = "Oppervlakte") {
  # Get the dtm of an area classified to an given water table. Everything under this waterlevel is blue,
  # between 0 and 20cm is lichtgreen,; betwen 25 and 40 is darkgreen, and everything above is beige.
  # For the areas with detailed topographic raster, this raster will be used.
    # Parameters:
    # gebied: area
    # gebied_buffer: standard 0, only for areas without detailed raster.
    # Peil: Reference waterstand
  
  
  if (gebied %in% gebieden_afgraving) {
    
    gebied_raster <-  rast(str_c("Data/DTM gebieden afgraving/", gebied, "_afgraving.tif")) %>% 
      `crs<-` ("EPSG:31370") %>%
      `names<-` ("Hoogte")
    
  } else {
    locatie <- GebiedenDTM(gebieden_buffer = gebieden_buffer, gebieden = gebieden_sf_)
    gebied_raster <- rast(str_c(locatie[1],'/', "dtm", "_" , gebied, "_", locatie[2],".tif"))
    
  }
  
  # Polygon of area
  gebied_sf <- gebieden_sf %>% 
    filter(Gebied == gebied) %>% 
    summarize() 
  
  # Calculate the referene height to a given waterlevel.
  gebied_raster_peil <-  gebied_raster - Peil
  #Get min and max value
  gebied_minmax <- gebied_raster_peil %>% 
    minmax()
  # Classify in nice rounded min and  max, and everything in between (0, 0.25 and 40)
  rcl <-  matrix(c(RoundAny(min(gebied_minmax), 0.5, floor), 0, 0.25, 0.40, RoundAny(max(gebied_minmax), 0.5, ceiling)))
  
  gebied_raster_classify <- gebied_raster_peil %>% 
    classify(., rcl, include.lowest = TRUE, right = TRUE) 
  
  
  # Kleuren, zo aan te passen zodat ze in de grafiek correct owrden overgenomen als er bepaalde klasses niet bestaan.
  variables <- unique(gebied_raster_classify$Hoogte) %>% 
    mutate(Kleur = case_when(
      str_detect(Hoogte, "0]") & str_sub(Hoogte, 1, 1) == "[" ~ "#0373F9",  # Voorkom als maximale hoogte ook eindigt mmet 0 dit blauw is.
      str_detect(Hoogte, "0.25]") ~ "#9FFF73",
      str_detect(Hoogte, "0.4]") ~ "#4DE600",
      str_detect(Hoogte, "0.4") ~ "#FFEBBE"))  

  # colors <- c("#0373F9", "#9FFF73", "#4DE600", "#FFEBBE")
  
  titel <- str_c(titel, "peil")

  G <- ggplot() +
    tidyterra::geom_spatraster(data = gebied_raster_classify, aes(fill = Hoogte)) +
    
    scale_fill_manual(values = variables$Kleur, labels = variables$Hoogte, breaks = variables$Hoogte) +

    guides(fill = guide_legend(reverse=TRUE)) +
    geom_sf(data = gebied_sf, alpha = 0, col = "black", lwd = 1, linetype = "solid") +
    theme(axis.text = element_text(colour = inbo_steun_donkerroos), 
          legend.text = element_text(color = inbo_steun_donkerroos),
          panel.background = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank()) +
    labs(fill = "m t.o.v. maaiveld", subtitle = str_c(titel, ": ", Peil, " mTAW")) +
    guides(fill = guide_legend(order = 1, reverse = TRUE))
  
  Onderschrift <- str_c("Hoogteligging van ", gebied, " tegenover het maaiveld bij een waterpeil van ", Peil, " mTAW.")
  
  return(list(Kaart = G, Bijschrift = Onderschrift))
}


# Boorprofielen ----------------------------------------------------------------

Boorprofiel <- function(Beschrijving, Balkbreedte = 0.4, MaxDiepte = NULL, tekstgrootte = 2, legendecolor = inbo_steun_donkerroos) {
  # Beschrijving is de beschrijving voor een specieke staal.
  # Dit is een ondersteunende functie voor de Boorprofielen.
  
  Beschrijving$Kleur <- NA
  BodemTypes <- unique(Beschrijving$bodemtype)
  if ("zand" %in% BodemTypes) {
    Beschrijving[Beschrijving$bodemtype == "zand",]$Kleur <- "#FFFDB2"
  }
  if ("leem" %in% BodemTypes) {
    Beschrijving[Beschrijving$bodemtype == "leem",]$Kleur <- "#DB3A4F"
  }
  if ("zandig leem" %in% BodemTypes) {
    Beschrijving[Beschrijving$bodemtype == "zandig leem",]$Kleur <- "#FA8D5A"
  }
  if ("lemig zand" %in% BodemTypes) {
    Beschrijving[Beschrijving$bodemtype == "lemig zand",]$Kleur <- "#FDE183"
  }
  if ("klei" %in% BodemTypes) {
    Beschrijving[Beschrijving$bodemtype == "klei",]$Kleur <- "dodgerblue4"
  }
  if ("zandige klei" %in% BodemTypes) {
    Beschrijving[Beschrijving$bodemtype == "zandige klei",]$Kleur <- "steelblue3"
  }
  if ("kleiig zand" %in% BodemTypes) {
    Beschrijving[Beschrijving$bodemtype == "kleiig zand",]$Kleur <- "lightskyblue1"
  }
  if ("veen" %in% BodemTypes) {
    Beschrijving[Beschrijving$bodemtype == "veen",]$Kleur <- "palegreen4"
  }
  
  if (is.null( MaxDiepte)) {
    MaxDiepte <- -max(Beschrijving$tot)
  }
  
  Graf <- ggplot(data = data.frame(x=c(0,1),y=c(0,MaxDiepte))) + geom_line(aes(x,y), color = "white") +
    theme(panel.background = element_rect(color = "white", fill = "white"))
  
  Beschrijving <- subset(Beschrijving, van <= -MaxDiepte)
  if (Beschrijving[nrow(Beschrijving),]$tot >= -MaxDiepte) {
    Beschrijving[nrow(Beschrijving),]$tot <- -MaxDiepte
  }
  
  for (Lijn in 1:nrow(Beschrijving)) {
    Lijn.Kleur <- Beschrijving$Kleur[Lijn]
    Lijn.van.y <- -Beschrijving$van[Lijn]
    Lijn.tot.y <- -Beschrijving$tot[Lijn]
    Lijn.van.x <- 0
    Lijn.tot.x <- Balkbreedte
    Graf <- Graf + annotate("rect", xmin = Lijn.van.x, xmax = Lijn.tot.x, ymin = Lijn.tot.y, ymax = Lijn.van.y, fill = Lijn.Kleur)
  }
  
  Graf <- Graf + 
    theme(axis.ticks = element_blank()) + 
    theme(axis.title.x = element_blank()) + 
    theme(axis.text.x = element_blank()) + 
    ylab("Diepte (m t.o.v. maaiveld)") +
    theme(axis.title.y = element_text(margin = margin(0.5,0.5,0.5,0.5,"cm"))) +
    scale_y_continuous(breaks = c(0,-Beschrijving$tot))
  
  Item = 1
  Graf <- Graf + annotate("rect", xmin = 0.6, xmax = 0.65, ymax = (0.05 + 0.1*(Item - 1))*MaxDiepte, ymin = (0.1 + 0.1*(Item - 1))*MaxDiepte, fill = "#FFFDB2")
  Graf <- Graf + annotate("text", x = 0.7, y = (0.07 + 0.1*(Item - 1))*MaxDiepte, label = "zand", hjust = 0, size = tekstgrootte, colour = legendecolor)
  Item = 2
  Graf <- Graf + annotate("rect", xmin = 0.6, xmax = 0.65, ymax = (0.05 + 0.1*(Item - 1))*MaxDiepte, ymin = (0.1 + 0.1*(Item - 1))*MaxDiepte, fill = "dodgerblue4")
  Graf <- Graf + annotate("text", x = 0.7, y = (0.07 + 0.1*(Item - 1))*MaxDiepte, label = "klei", hjust = 0, size = tekstgrootte, colour = legendecolor)
  Item = 3
  Graf <- Graf + annotate("rect", xmin = 0.6, xmax = 0.65, ymax = (0.05 + 0.1*(Item - 1))*MaxDiepte, ymin = (0.1 + 0.1*(Item - 1))*MaxDiepte, fill = "steelblue3")
  Graf <- Graf + annotate("text", x = 0.7, y = (0.07 + 0.1*(Item - 1))*MaxDiepte, label = "zandige klei", hjust = 0, size = tekstgrootte, colour = legendecolor)
  Item = 4
  Graf <- Graf + annotate("rect", xmin = 0.6, xmax = 0.65, ymax = (0.05 + 0.1*(Item - 1))*MaxDiepte, ymin = (0.1 + 0.1*(Item - 1))*MaxDiepte, fill = "lightskyblue1")
  Graf <- Graf + annotate("text", x = 0.7, y = (0.07 + 0.1*(Item - 1))*MaxDiepte, label = "kleiig zand", hjust = 0, size = tekstgrootte, colour = legendecolor)
  Graf
  Item = 5
  Graf <- Graf + annotate("rect", xmin = 0.6, xmax = 0.65, ymax = (0.05 + 0.1*(Item - 1))*MaxDiepte, ymin = (0.1 + 0.1*(Item - 1))*MaxDiepte, fill = "#DB3A4F")
  Graf <- Graf + annotate("text", x = 0.7, y = (0.07 + 0.1*(Item - 1))*MaxDiepte, label = "leem", hjust = 0, size = tekstgrootte, colour = legendecolor)
  Item = 6
  Graf <- Graf + annotate("rect", xmin = 0.6, xmax = 0.65, ymax = (0.05 + 0.1*(Item - 1))*MaxDiepte, ymin = (0.1 + 0.1*(Item - 1))*MaxDiepte, fill = "#FA8D5A")
  Graf <- Graf + annotate("text", x = 0.7, y = (0.07 + 0.1*(Item - 1))*MaxDiepte, label = "zandig leem", hjust = 0, size = tekstgrootte, colour = legendecolor)
  Item = 7
  Graf <- Graf + annotate("rect", xmin = 0.6, xmax = 0.65, ymax = (0.05 + 0.1*(Item - 1))*MaxDiepte, ymin = (0.1 + 0.1*(Item - 1))*MaxDiepte, fill = "#FDE183")
  Graf <- Graf + annotate("text", x = 0.7, y = (0.07 + 0.1*(Item - 1))*MaxDiepte, label = "lemig zand", hjust = 0, size = tekstgrootte, colour = legendecolor)
  Item = 8
  Graf <- Graf + annotate("rect", xmin = 0.6, xmax = 0.65, ymax = (0.05 + 0.1*(Item - 1))*MaxDiepte, ymin = (0.1 + 0.1*(Item - 1))*MaxDiepte, fill = "palegreen4")
  Graf <- Graf + annotate("text", x = 0.7, y = (0.07 + 0.1*(Item - 1))*MaxDiepte, label = "veen", hjust = 0, size = tekstgrootte, colour = legendecolor)
  Graf <- Graf + geom_hline(yintercept = 0)
  Graf <- Graf + annotate("curve",x = 0.89,xend = 0.9,y = -0.01*MaxDiepte, yend = -0.01*MaxDiepte, curvature = 0, linewidth = 1)
  Graf <- Graf + annotate("curve",x = 0.88,xend = 0.91,y = -0.02*MaxDiepte, yend = -0.02*MaxDiepte, curvature = 0, linewidth = 1)
  Graf <- Graf + annotate("curve",x = 0.87,xend = 0.92,y = -0.03*MaxDiepte, yend = -0.03*MaxDiepte, curvature = 0, linewidth = 1)
  Graf
}




Boorprofiel2 <- function(Beschrijving, Balkbreedte = 0.4, MaxDiepte = NULL, tekstgrootte = 4) {
  # Beschrijving is de beschrijving voor een specieke staal.
  # Dit is een ondersteunende functie voor de Boorprofielen.
  
  Beschrijving$Kleur <- NA
  BodemTypes <- unique(Beschrijving$bodemtype)
  if ("zand" %in% BodemTypes) {
    Beschrijving[Beschrijving$bodemtype == "zand",]$Kleur <- "lightgoldenrodyellow"
  }
  if ("leem" %in% BodemTypes) {
    Beschrijving[Beschrijving$bodemtype == "leem",]$Kleur <- "red3"
  }
  if ("zandig leem" %in% BodemTypes) {
    Beschrijving[Beschrijving$bodemtype == "zandig leem",]$Kleur <- "hotpink1"
  }
  if ("lemig zand" %in% BodemTypes) {
    Beschrijving[Beschrijving$bodemtype == "lemig zand",]$Kleur <- "rosybrown1"
  }
  if ("klei" %in% BodemTypes) {
    Beschrijving[Beschrijving$bodemtype == "klei",]$Kleur <- "dodgerblue4"
  }
  if ("zandige klei" %in% BodemTypes) {
    Beschrijving[Beschrijving$bodemtype == "zandige klei",]$Kleur <- "steelblue3"
  }
  if ("kleiig zand" %in% BodemTypes) {
    Beschrijving[Beschrijving$bodemtype == "kleiig zand",]$Kleur <- "lightskyblue1"
  }
  if ("veen" %in% BodemTypes) {
    Beschrijving[Beschrijving$bodemtype == "veen",]$Kleur <- "palegreen4"
  }
  
  if (is.null( MaxDiepte)) {
    MaxDiepte <- -max(Beschrijving$tot)
  }
  
  Graf <- ggplot(data = data.frame(x=c(0,1),y=c(0,MaxDiepte))) + geom_line(aes(x,y), color = "white") +
    theme(panel.background = element_rect(color = "white", fill = "white"))
  
  Beschrijving <- subset(Beschrijving, van <= -MaxDiepte)
  if (Beschrijving[nrow(Beschrijving),]$tot >= -MaxDiepte) {
    Beschrijving[nrow(Beschrijving),]$tot <- -MaxDiepte
  }
  
  for (Lijn in 1:nrow(Beschrijving)) {
    Lijn.Kleur <- Beschrijving$Kleur[Lijn]
    Lijn.van.y <- -Beschrijving$van[Lijn]
    Lijn.tot.y <- -Beschrijving$tot[Lijn]
    Lijn.van.x <- 0
    Lijn.tot.x <- Balkbreedte
    Graf <- Graf + annotate("rect", xmin = Lijn.van.x, xmax = Lijn.tot.x, ymin = Lijn.tot.y, ymax = Lijn.van.y, fill = Lijn.Kleur)
  }
  
  Graf <- Graf + 
    theme(axis.ticks = element_blank()) + 
    theme(axis.title.x = element_blank()) + 
    theme(axis.text.x = element_blank()) + 
    ylab("Diepte (m t.o.v. maaiveld)") +
    theme(axis.title.y = element_text(margin = margin(0.5,0.5,0.5,0.5,"cm"))) +
    scale_y_continuous(breaks = c(0,-Beschrijving$tot))
  
  
  
  Item = 1
  Graf <- Graf + annotate("rect", xmin = 0.6, xmax = 0.65, ymax = (0.05 + 0.1*(Item - 1))*MaxDiepte, ymin = (0.1 + 0.1*(Item - 1))*MaxDiepte, fill = "lightgoldenrodyellow")
  Graf <- Graf + annotate("text", x = 0.7, y = (0.07 + 0.1*(Item - 1))*MaxDiepte, label = "zand", hjust = 0, size = tekstgrootte)
  Item = 2
  Graf <- Graf + annotate("rect", xmin = 0.6, xmax = 0.65, ymax = (0.05 + 0.1*(Item - 1))*MaxDiepte, ymin = (0.1 + 0.1*(Item - 1))*MaxDiepte, fill = "dodgerblue4")
  Graf <- Graf + annotate("text", x = 0.7, y = (0.07 + 0.1*(Item - 1))*MaxDiepte, label = "klei", hjust = 0, size = tekstgrootte)
  Item = 3
  Graf <- Graf + annotate("rect", xmin = 0.6, xmax = 0.65, ymax = (0.05 + 0.1*(Item - 1))*MaxDiepte, ymin = (0.1 + 0.1*(Item - 1))*MaxDiepte, fill = "steelblue3")
  Graf <- Graf + annotate("text", x = 0.7, y = (0.07 + 0.1*(Item - 1))*MaxDiepte, label = "zandige klei", hjust = 0, size = tekstgrootte)
  Item = 4
  Graf <- Graf + annotate("rect", xmin = 0.6, xmax = 0.65, ymax = (0.05 + 0.1*(Item - 1))*MaxDiepte, ymin = (0.1 + 0.1*(Item - 1))*MaxDiepte, fill = "lightskyblue1")
  Graf <- Graf + annotate("text", x = 0.7, y = (0.07 + 0.1*(Item - 1))*MaxDiepte, label = "kleiig zand", hjust = 0, size = tekstgrootte)
  Graf
  Item = 5
  Graf <- Graf + annotate("rect", xmin = 0.6, xmax = 0.65, ymax = (0.05 + 0.1*(Item - 1))*MaxDiepte, ymin = (0.1 + 0.1*(Item - 1))*MaxDiepte, fill = "red3")
  Graf <- Graf + annotate("text", x = 0.7, y = (0.07 + 0.1*(Item - 1))*MaxDiepte, label = "leem", hjust = 0, size = tekstgrootte)
  Item = 6
  Graf <- Graf + annotate("rect", xmin = 0.6, xmax = 0.65, ymax = (0.05 + 0.1*(Item - 1))*MaxDiepte, ymin = (0.1 + 0.1*(Item - 1))*MaxDiepte, fill = "hotpink1")
  Graf <- Graf + annotate("text", x = 0.7, y = (0.07 + 0.1*(Item - 1))*MaxDiepte, label = "zandig leem", hjust = 0, size = tekstgrootte)
  Item = 7
  Graf <- Graf + annotate("rect", xmin = 0.6, xmax = 0.65, ymax = (0.05 + 0.1*(Item - 1))*MaxDiepte, ymin = (0.1 + 0.1*(Item - 1))*MaxDiepte, fill = "rosybrown1")
  Graf <- Graf + annotate("text", x = 0.7, y = (0.07 + 0.1*(Item - 1))*MaxDiepte, label = "lemig zand", hjust = 0, size = tekstgrootte)
  Item = 8
  Graf <- Graf + annotate("rect", xmin = 0.6, xmax = 0.65, ymax = (0.05 + 0.1*(Item - 1))*MaxDiepte, ymin = (0.1 + 0.1*(Item - 1))*MaxDiepte, fill = "palegreen4")
  Graf <- Graf + annotate("text", x = 0.7, y = (0.07 + 0.1*(Item - 1))*MaxDiepte, label = "veen", hjust = 0, size = tekstgrootte)
  Graf <- Graf + geom_hline(yintercept = 0)
  Graf <- Graf + annotate("curve",x = 0.89,xend = 0.9,y = -0.01*MaxDiepte, yend = -0.01*MaxDiepte, curvature = 0, linewidth = 1)
  Graf <- Graf + annotate("curve",x = 0.88,xend = 0.91,y = -0.02*MaxDiepte, yend = -0.02*MaxDiepte, curvature = 0, linewidth = 1)
  Graf <- Graf + annotate("curve",x = 0.87,xend = 0.92,y = -0.03*MaxDiepte, yend = -0.03*MaxDiepte, curvature = 0, linewidth = 1)
  Graf
}






Boorprofiel.PB1 <- function(Beschrijving, filter.top, filter.bodem, MaxDiepte = 2.9, BalkBreedte = 0.3, Buisbreedte = 0.04) {
  Graf <- Boorprofiel(Beschrijving, Balkbreedte = 0.2, MaxDiepte)
  Graf <- Graf + annotate("rect",xmin = 0.3, xmax = 0.3 + Buisbreedte, ymin = filter.bodem, ymax = filter.top, fill = "grey95", color = "black", linetype = "dashed")
  Graf <- Graf + annotate("rect",xmin = 0.3, xmax = 0.3 + Buisbreedte, ymin = filter.top, ymax = 0, fill = "white", color = "black", linetype = "solid")
  Graf
}

Boorprofiel.PB2 <- function(Beschrijving, filter1.top, filter1.bodem, filter2.top, filter2.bodem, MaxDiepte = NULL, BalkBreedte = 0.3, Buisbreedte = 0.04) {
  Graf <- Boorprofiel(Beschrijving, Balkbreedte = 0.2, MaxDiepte)
  
  Graf <- Graf + annotate("rect",xmin = 0.24, xmax = 0.24 + Buisbreedte, ymin = filter1.bodem, ymax = filter1.top, fill = "grey95", color = "black", linetype = "dashed")
  Graf <- Graf + annotate("rect",xmin = 0.24, xmax = 0.24 + Buisbreedte, ymin = filter1.top, ymax = 0, fill = "white", color = "black", linetype = "solid")
  
  Graf <- Graf + annotate("rect",xmin = 0.33, xmax = 0.33 + Buisbreedte, ymin = filter2.bodem, ymax = filter2.top, fill = "grey95", color = "black", linetype = "dashed")
  Graf <- Graf + annotate("rect",xmin = 0.33, xmax = 0.33 + Buisbreedte, ymin = filter2.top, ymax = 0, fill = "white", color = "black", linetype = "solid")
  
  Graf
}




Boorprofiel.PB3 <- function(Beschrijving, filter1.top, filter1.bodem, filter2.top, filter2.bodem, filter3.top, filter3.bodem, MaxDiepte = NULL, BalkBreedte = 0.3, Buisbreedte = 0.04) {
  Graf <- Boorprofiel(Beschrijving, Balkbreedte = 0.2, MaxDiepte)
  
  Graf <- Graf + annotate("rect",xmin = 0.24, xmax = 0.24 + Buisbreedte, ymin = filter1.bodem, ymax = filter1.top, fill = "grey95", color = "black", linetype = "dashed")
  Graf <- Graf + annotate("rect",xmin = 0.24, xmax = 0.24 + Buisbreedte, ymin = filter1.top, ymax = 0, fill = "white", color = "black", linetype = "solid")
  
  Graf <- Graf + annotate("rect",xmin = 0.33, xmax = 0.33 + Buisbreedte, ymin = filter2.bodem, ymax = filter2.top, fill = "grey95", color = "black", linetype = "dashed")
  Graf <- Graf + annotate("rect",xmin = 0.33, xmax = 0.33 + Buisbreedte, ymin = filter2.top, ymax = 0, fill = "white", color = "black", linetype = "solid")
  
  Graf <- Graf + annotate("rect",xmin = 0.42, xmax = 0.42 + Buisbreedte, ymin = filter3.bodem, ymax = filter3.top, fill = "grey95", color = "black", linetype = "dashed")
  Graf <- Graf + annotate("rect",xmin = 0.42, xmax = 0.42 + Buisbreedte, ymin = filter3.top, ymax = 0, fill = "white", color = "black", linetype = "solid")
  
  Graf
}


# Ortho --------

# Het probleem bij ortho is dat de originele laag veel te groot is om in te lezen dus het best intersect per gebied via GIS en deze kaart inlezen. Dan nog kan het lang duren.

KaartOrtho <- function(gebied_sf,
                       buffer_gebied,
                       fact = 3,
                       locatie = "O:/Projects/PRJ_Schelde/Haven/Gebieden Haven/Ortho/Buffer_") {
  
  # Aggregatiefactor (fact) bepaalt hoe de resolutie van de raster wordt verlaagd door een bepaald aantal pixels samen te voegen tot één enkele pixel.
  gebied <- gebied_sf %>%
    st_drop_geometry() %>% 
    pull(Gebied) %>% 
    str_c(collapse = "_")
  
  ortholayer <- str_c(locatie, buffer_gebied, "/", gebied, "_", buffer_gebied, ".tif")

  
  
  if (file.exists(file.path(ortholayer))) {
    
    
    ortho <- rast(x = ortholayer)

    
    orthofoto_lowres <- aggregate(ortho, fact = fact)
    
    ortho.df <- as.data.frame(orthofoto_lowres, xy = TRUE) %>% 
      rename("red" = 3, "green" = 4, "blue" = 5)
    
    ortho_layer <- ggplot() +
      geom_raster(data = ortho.df, aes(x = x, y = y, fill = rgb(red = red,
                                                                green = green,
                                                                blue = blue,
                                                                maxColorValue = 255))) +
      scale_fill_identity() +
      coord_sf(crs = st_crs(31370), expand = FALSE) + 
      theme(
        plot.margin = margin(0, 0, 0, 0),  # Remove plot margins
        panel.spacing = unit(0, "lines"),
        panel.grid.major = element_blank(),  # Remove major grid lines
        panel.grid.minor = element_blank(),  # Remove minor grid lines
        axis.text = element_blank(),  # Remove axis text
        axis.ticks = element_blank()
      ) +
      annotation_scale(location = "bl") +
      annotation_north_arrow(location = "bl", which_north = "true", 
                             height = unit(1, "cm"),
                             width = unit(1, "cm"),
                             pad_x = unit(0.005, "cm"),
                             pad_y = unit(0.5, "cm"),
                             style = north_arrow_fancy_orienteering(line_width = .5, text_size = 5))
    
  } else {
    print("Ortho dient aangemaakt te worden!")
    ortho_layer <- ggplot()
  }
  
  return(ortho_layer)
  
}



