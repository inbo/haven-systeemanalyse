# Basisdata --------------------------------------------------------------------
# BELANGRIJK. Veel functies baseren zich op op data-tabelen die NIET opgenomen zijn in de functie zelf, maar dienen te staan in de Environment.
# Zeker belangrijk de Kaarten, gebieden is sf-file met al de gebieden erin. Wordt binnen de functie gefilterd


# # O:\Projects\PRJ_Schelde\Haven\Gebieden Haven
# gebieden_sf <- st_read("Data/Shapefiles/Gebieden.shp", crs = 31370) %>% 
#   rename(Gebied = Naam) %>% 
#   mutate(Gebied = str_to_title(Gebied))
# 
# 
# # Geografische informatie overzicht van de Schelde en dokken.
# schelde_dokken_sf <- st_read("Data/Shapefiles/Schelde en dokken.shp", crs = 31370)
# 
# # Geografische informatie over de afwateringen.
# afwatering_sf <- st_read("S:/Vlaanderen/Water/VHA/Wlas.shp") %>% 
#   st_transform(crs = 31370) %>% 
#   # Filter op Stroombekken Schelde en de categoriën.
#   dplyr::filter(str_detect(.data$LBLCATC, "categorie") & STRMGEB == "Schelde") %>% 
#   dplyr::select(Name = NAAM, LBLCATC)
# 
# # Excel data met de boorbeschrijvingen
# boringen_df <- read_xlsx("Data/Gegevens/boringen.xlsx")

# Sommige gebieden hebben een aparte raster-file waarin de toplaag afgegraven is Deze dienen gebruikt te worden
# voor de visuele weergave van het reliëf.
gebieden_afgraving <- c("Doelpolder Noord", "Drijdijck")


# Based on the colors of 'bodemkaart 2015".
colours_bodemkaart <- c("Vochtig zandleem" = "#C2B342", 
                        "Antropogeen" = "#FFFFFF",
                        "Natte klei" =  
                          "#6D8532", 
                        "Natte zware klei" = "#206375",
                        "Nat zandleem" = "#A69537", 
                        "Vochtige zware klei" = "#42927B",
                        "Vochtig zand" =  "#6C8CAD", 
                        "Vochtig zand antropogeen" = "#D0AA98",
                        "Nat zand" = "#446589",  
                        "Droog zand antropogeen" = "#EAD0C9",
                        "Droog zand" = "#94B2D1", 
                        "Nat zand antropogeen" =  "#B68367", 
                        "Vochtige klei" = "#85A444", 
                        "Veen" = "#645D37",  
                        "Landduin" = "#EEDFB9")

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


OpmaakKaartAnnotation <- function(kaart, location_annotation = "bl") {
  
  # Add schaal en pijl.
  # best toevoegen direct na ortho, dtm, en dan pas verder grafiek uitbouwen.
  
  kaart_annotation <- kaart + 
    annotation_scale(location = location_annotation) +
    annotation_north_arrow(location = location_annotation, which_north = "true", 
                           height = unit(1, "cm"),
                           width = unit(1, "cm"),
                           pad_x = unit(0.005, "cm"),
                           pad_y = unit(0.5, "cm"),
                           style = north_arrow_fancy_orienteering(line_width = .5, text_size = 5))
  
  return(kaart_annotation)
}



AdjustPlotExtend <- function(plot_extend, factor = 1/2) {
  
  coords <- plot_extend %>% 
    st_bbox()
  
  # 137821.2 216044.9 138363.2 217248.9 
  
  x <- coords$xmax - coords$xmin
  y <- coords$ymax - coords$ymin 
  
  if (x > y) {
    
    diff <- (x - y) / 2
    
    xmin <- coords$xmin
    xmax <- coords$xmax
    ymin <- coords$ymin - (diff * factor)
    ymax <- coords$ymax + (diff * factor)
    
  } else if (x < y) {
    
    
    diff <- (y - x) / 2 
    
    xmin <- coords$xmin - (diff * factor)
    xmax <- coords$xmax + (diff * factor)
    ymin <- coords$ymin
    ymax <- coords$ymax
    
  } else {
    return(plot_extend)
    
  }
  
  plot_extend_adj <- tibble(
    longitude = c(xmin, xmax),
    latitude = c(ymin, ymax)) %>% 
    st_as_sf(., coords = c("longitude", "latitude"), crs = 31370) %>% 
    st_bbox() %>% 
    st_as_sfc()
  
  return(plot_extend_adj)
}

# IDENTIFY COLORS
# Based on colors of maps of PP. 
# Screenshot image -> Paint -> identity color (oogdruppel-tool -> edit colors) -> get rgb-value -> 
# set in converter for color chart (https://www.rapidtables.com/web/color/RGB_Color.html)




# Kleurenpallet

colours_gebieden <- c(
  "C59" = "grey70",
  "Doelpolder Noord" = "#FF010A",
  "Drijdijck" = "#FFC501",
  "Gedempt deel doeldok" = "#C7C055",
  "Groenknolorchiszone" = "#0DFFCF",
  "Groot Rietveld" = "#C69745",
  "Grote Geule" = "#BA89CE",
  "Haasop" = "#25D159",
  "Kreken van Saleghem" = "#CD00FF",
  "MIDAS" = "#8FC08A",
  "Putten Weiden" = "#79F31B",
  "Putten West" = "#36B5EC",
  "R2 driehoek" = "#F7FF00",
  "Rietveld Kallo" = "#80470F",
  "Spaans Fort" = "#FF007F",
  "Steenlandpolder" = "#B72D17",
  "Verrebroekse Plassen" = "#3421DF",
  "Vlakte van Zwijndrecht" =  "#17AAAF",
  "Prosperpolder Zuid fase 1" = "#EF972C"
)


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


# ControleBoring <- function(Meetpunten, boringen_df = boringen_df) {
#   # Controlleer of er van de peilbuis een boring beschikbaar is.
#   controle <- FALSE
#   if (any(Meetpunten %in% unique(boringen_df$peilpunt))) {
#     controle <- TRUE
#   } 
#   return(controle)
# }

GebiedDTMMask <- function(gebied_sf,
                          gebied_buffer = 0,
                          output_path = "Q:/Projects/PRJ_Schelde/Haven/studiegebied_DTM",
                          DTM_Jaar = 2017, 
                          Locatie_DTM = "S:/Vlaanderen/Hoogte/DHMVII/DHMVIIDTMRAS1m.tif"
                          ) {
  # Create shape-files voor hoogte-profiel voor enkel het gebied.
  # Boolean to control if map exist.
  
  Create <- FALSE
  # Maak map aan om dtm's in op te slagen
  outputlocationname <- str_c(output_path, DTM_Jaar, sep = "/")
  if (!file.exists(outputlocationname)) {
    dir.create(outputlocationname)
  }
  
  # Create a submap for the specific buffer if it dont exist. 
  outputlocationnamebuffer <- str_c(outputlocationname, "/mask_", gebied_buffer)
  
  if (!file.exists(outputlocationnamebuffer)) {
    dir.create(outputlocationnamebuffer)
  }
  
  gebied <- gebied_sf %>% 
    st_drop_geometry() %>% 
    pull() %>% 
    str_c(., collapse = "_")
  
  outputlocationfile <- str_c(outputlocationnamebuffer, 
                              str_c("dtm_mask_", gebied, "_", gebied_buffer, ".tif"), sep = "/")
  
  if (!file.exists(outputlocationfile)) {
    Create <- TRUE
  }
  
  if (Create) {
    # Read dtm Vlaanderen.
    Vlaanderen_dtm <- terra::rast(Locatie_DTM) %>% 
      `crs<-`("EPSG:31370")
    
    
    gebied_dtm <- Vlaanderen_dtm %>% 
      `names<-`("Hoogte") %>% 
      terra::crop(., gebied_sf, mask = TRUE) %>% 
      `crs<-`(str_c("EPSG:31370"))
    
    writeRaster(x = gebied_dtm, 
                filename = outputlocationfile)
    
  }
  return(outputlocationfile)
}

# GebiedenDTMMask <- function(gebieden_buffer = 0, 
#                               gebieden = gebieden_sf,
#                               DTM_Jaar = 2017, 
#                               Locatie_DTM = "S:/Vlaanderen/Hoogte/DHMVII/DHMVIIDTMRAS1m.tif") {
#   # Create shape-files voor hoogte-profiel voor enkel het gebied.
#   # Boolean to control if map exist.
#   
#   Create <- FALSE
#   # Maak map aan om dtm's in op te slagen
#   OutputLocationName <- str_c("DTM gebieden MASK ", DTM_Jaar)
#   if (!file.exists(str_c("Data/", OutputLocationName))) {
#     dir.create(str_c("Data/", OutputLocationName))
#   }
#   OutputLocation <- str_c("Data",OutputLocationName,sep = "/")
#   
#   # Create a submap for the specific buffer if it dont exist. 
#   OutputLocationName <- str_c(OutputLocationName, "/buffer_", gebieden_buffer)
#   
#   if (!file.exists(str_c("Data/", OutputLocationName))) {
#     dir.create(str_c("Data/", OutputLocationName))
#     Create <- TRUE
#   }
#   OutputLocation <- str_c("Data",OutputLocationName,sep = "/")
#   
#   if (Create) {
#     # Read dtm Vlaanderen.
#     Vlaanderen_dtm <- terra::rast(Locatie_DTM) %>% 
#       `crs<-`("EPSG:31370")
#     
#     # Make from the geometry with same group-name one group and add buffer.
#     gebieden_tot_sf <- gebieden %>% 
#       group_by(Gebied) %>% 
#       summarise(., .groups = "drop")
#     
#     for (gebied in gebieden_tot_sf$Gebied) {
#       
#       gebied_dtm <- subset(gebieden_tot_sf, Gebied == gebied) %>% 
#         terra::vect()
#       
#       # get MASKED dtm
#       gebied_dtm <- Vlaanderen_dtm %>% 
#         `names<-`("Hoogte") %>% 
#         terra::crop(., gebied_dtm, mask = TRUE) %>% 
#         `crs<-`(str_c("EPSG:31370"))
# 
#       writeRaster(x = gebied_dtm, filename = str_c(OutputLocation, "/","dtm_", gebied, "_", gebieden_buffer, ".tif"))
#     }
#   }
#   return(list(OutputLocation, gebieden_buffer))
# }

MinMaxGebied <- function(gebied, interval_m = 1, gebieden = gebieden_sf) {
  # Get the rounded value for the min and max height in the area.
  # The min or max get rounded up depending on the interval.

  locatie <- GebiedenDTMMask(gebieden_buffer = 0, gebieden = gebieden_sf_)
  gebied_raster <- rast(str_c(locatie[1],'/', "dtm", "_" , gebied, "_", locatie[2],".tif"))
  
  gebied_minmax <- gebied_raster %>% 
    minmax()
    
  minmax <-   list(Min = RoundAny(min(gebied_minmax), interval_m, floor), 
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
  
  locatie <- GebiedenDTMMask(gebied_buffer = 0, gebieden = gebieden_sf_)
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


GebiedDTM <- function(gebied_sf,
                      gebied_buffer = 50,
                      factor_extend = 1/2,
                      output_path = "Q:/Projects/PRJ_Schelde/Haven/studiegebied_DTM",
                      DTM_Jaar = 2017, 
                      Locatie_DTM = "S:/Vlaanderen/Hoogte/DHMVII/DHMVIIDTMRAS1m.tif") {
  # Geeft de outputlocatie weer indien dit bestaat. 
  # Default om te maken staat op FALSE, als er een nieuwe buffer-versie gemaakt dient te worden dient de parameter Create op TRUE gezet te worden.
  # In principe maar 1x te maken. En enkel up te daten als er een nieuw dtm van Vlaanderen beschikbaar is.
  
  # Boolean to control if map exist.
  Create <- FALSE
  # Maak map aan om dtm's in op te slagen
  OutputLocationName <- str_c(output_path, DTM_Jaar, sep = "/")
  if (!file.exists(OutputLocationName)) {
    dir.create(OutputLocationName)
  }
  # Create a submap for the specific buffer if it dont exist. 
  OutputLocationNameBuffer <- str_c(OutputLocationName, "/buffer_", gebied_buffer)
  
  if (!file.exists(OutputLocationNameBuffer)) {
    dir.create(OutputLocationNameBuffer)
  }
  # outputlocation <- OutputLocationNameBuffer
  
  OutputLocationNameBufferExtend <- str_c(OutputLocationNameBuffer, "/buffer_", gebied_buffer, "_extend_", factor_extend)
  
  if (!file.exists(OutputLocationNameBufferExtend)) {
    dir.create(OutputLocationNameBufferExtend)
  }
  outputlocation <- OutputLocationNameBufferExtend
  
  gebied <- gebied_sf %>% 
    st_drop_geometry() %>% 
    pull() %>% 
    str_c(., collapse = "_")
  
  outputlocationname <- str_c(outputlocation, 
                              str_c("dtm_", gebied, "_", gebied_buffer, ".tif"), sep = "/")
  
  if (!file.exists(outputlocationname)) {
    Create <- TRUE
  }

  if (Create) {
    # Read dtm Vlaanderen.
    Vlaanderen_dtm <- terra::rast(Locatie_DTM) %>% 
      `crs<-`("EPSG:31370")
    
    # Make from the geometry with same group-name one group and add buffer.
    gebied_buffer_vect <- gebied_sf %>% 
      st_buffer(., dist = gebied_buffer) %>% 
      AdjustPlotExtend(., factor = factor_extend) %>% 
      terra::vect()
    
    # Crop to keep square-format for mapping.
    gebied_dtm <- Vlaanderen_dtm %>% 
      terra::crop(., gebied_buffer_vect) %>% 
      `names<-`("Hoogte") %>% 
      `crs<-`(str_c("EPSG:31370"))
    
    writeRaster(x = gebied_dtm, filename = outputlocationname, overwrite = TRUE)
    
  }
  return(outputlocationname)
}

GebiedenBodemkaart <- function(gebied_sf,
                               gebied_buffer,
                               factor_extend = 1/2,
                               bodemkaart_jaar = 2015,
                               locatie = "Q:/Projects/PRJ_Schelde/Haven/studiegebied_bodemkaart/") {
  
  
  gebied <- gebied_sf %>%
    st_drop_geometry() %>% 
    pull(.) %>% 
    str_c(., collapse = "_")
  
  # Bodemkaart from the area of Antwerp harbor. Is manually made from the big-file since otherwise it is too big to read it.
  # Apperently there is also a raster-file from the Bodemkaart. -> Possibility to replace it with this.
  locatie_bodemkaart <- str_c(str_c(locatie, bodemkaart_jaar), 
                              "/Bodemkaart_havengebieden_", bodemkaart_jaar, ".shp")
  outputbodemkaart <- str_c(locatie, bodemkaart_jaar)
  
  
  # Maak map aan om gebieden in op te slagen
  if (!file.exists(outputbodemkaart)) {
    dir.create(outputbodemkaart)
  }
  
  # Maak submap voor eventuele verschillende buffers
  outputlocatie_buffer <- str_c(outputbodemkaart, "/buffer_", gebied_buffer)
  
  if (!file.exists(outputlocatie_buffer)) {
    dir.create(outputlocatie_buffer)
  }
  
  OutputLocationNameBufferExtend <- str_c(outputlocatie_buffer, "/buffer_", gebied_buffer, "_extend_", factor_extend)
  
  if (!file.exists(OutputLocationNameBufferExtend)) {
    dir.create(OutputLocationNameBufferExtend)
  }
  outputlocation <- OutputLocationNameBufferExtend
  
  outputlocationfile <- str_c(outputlocation, 
                              str_c(str_c("Bodemkaart", gebied, gebied_buffer,  sep = "_"), 
                                    "shp", sep = "."), sep = "/")
  
  # Maak enkel kaart als deze nog niet bestaat.
  if (!file.exists(outputlocationfile)) {
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
      st_buffer(., dist = gebied_buffer) %>% 
      st_bbox(.) %>% 
      st_as_sfc(., crs = 31370)
    
    bodemkaart_gebied <- bodemkaart_havengebieden %>% 
      st_intersection(., gebied_box)
    
    st_write(obj = bodemkaart_gebied, 
             dsn = outputlocationfile)
  }
  return(outputlocationfile)
}

# Kaarten --------------------------------------------------------------------

# AnnotationKaart <- function(kaart, referentie) {
#   
#   kaart <- kaart +
#     annotation_scale(data = referentie,
#                      location = "bl",
#                      height = unit(0.25, "cm"),
#                      pad_x = unit(0.45, "cm"),
#                      pad_y = unit(0.0, "cm")) +
#     annotation_north_arrow(data = referentie,
#                            location = "bl",
#                            style = north_arrow_fancy_orienteering(line_width = .5, text_size = 5),
#                            pad_x = unit(0.0, "cm"),
#                            pad_y = unit(0.0, "cm"),
#                            height = unit(0.5, "cm"),
#                            width = unit(0.45, "cm"))
#   return(kaart)
# }





KaartGebiedRelief <- function(gebied_sf, gebied_buffer,
                              x_min = NULL, x_max = NULL, y_min = NULL, y_max = NULL,
                              interval_m = 1,
                              minmax_mask = TRUE, 
                              expand_intervals = 2,
                              min_interval = NULL, max_interval = NULL,
                              factor_extend = 0.5, 
                              schelde_dokken = TRUE, 
                              schelde_dokken_sf_f = schelde_dokken_sf,
                              afwateringen = TRUE,
                              afwatering_sf_f = afwatering_sf,
                              legende_gebied = FALSE, 
                              # annotation = TRUE,
                              dtm_colours = colours_dtm,
                              zomerpeil = NULL, 
                              zomerpeil_colours = colours_zomerpeil) {
  # Make the overall dtm for an area with a certain buffer.
  # PARAMETERS:
  # gebied :  Name of the area.
  # gebied_buffer: buffer area in meter.
  # X / y minmax: hw much buffer in a certain direction. 
  # interval_m: Size of the intervals in meter. Default is 1m. 
  # minmax_mask = TRUE/FALSE. If TRUE (default), get the minmax of only the study-area. If False, get the inmax of whole the raster (square-box + buffer arround the study-area)
  # expand_intervals: how many itnervalls on top of minmax. Only when minmax_mask = TRUE
  # min_interval: The mininum height from where the interval-size will start. Collects all the height below this number in one color.
  # max_interval: The maximum height from where the interval-size will stop Collects all the height above this number in one color.
  # Schelde_dokken: add shapefiles (1) Schelde and (2) dokken to the map (Default = TRUE). This needs to be a part since the minmax is calculated on the whole raster with an inverse mask.
  # Schelde_dokken_sf, add the corect shapefile
  # Afwateringen: add shapefile with the drainage-creeks (Default = TRUE).
  # legende_gebied: add the name of the "gebied" to the legend (Default = FALSE).
  # annotation = TRUE: add scalebar and north-arrow (default = TRUE)
  # RETURN: a quitte beautifull map with the height-intervals :) .
  
  # If zomerpeil ! geom_spatrasters RESAMPLES ! -> Low frequency values will go away! Check manually and adapt min_interval
  
  # RASTER Gebied met buffer
  gebied <- gebied_sf %>% 
    st_drop_geometry() %>% 
    pull() %>% 
    str_c(., collapse = '_')
  
  locatie_raster <- GebiedDTM(gebied_sf = gebied_sf, gebied_buffer = gebied_buffer, factor_extend = factor_extend)
  
  gebied_raster <- rast(locatie_raster)

  # Hoogteverschil gebied & Originele coordinaten (xmin, max etc)
  locatie_mask <- GebiedDTMMask(gebied_sf)
  gebied_raster_mask <- rast(locatie_mask)
  
  Bijschrift_buffer <- ""
  
  # Extend needs to be adapted !
  if (any(!is.null(c(x_min, x_max, y_min, y_max)))) {
    # Originele coordinaten (xmin, max etc)

    # Replace NULL to gebied_buffer
    list_coordinaten <- lst(x_min, x_max, y_min, y_max) %>% 
      map(., ~ replace(.x, is.null(.x), gebied_buffer))
    
    # make xlim and ylim negative
    if (list_coordinaten$x_min > 0) {
      list_coordinaten$x_min <- -list_coordinaten$x_min
      }
    if (list_coordinaten$y_min > 0) {
      list_coordinaten$y_min <- -list_coordinaten$y_min}
    
    # New frame
    if (any(c(abs(list_coordinaten$x_min), abs(list_coordinaten$x_max), 
              abs(list_coordinaten$y_min), abs(list_coordinaten$y_max)) > gebied_buffer)) {
      return(str("INVALID: originel buffer dient groter te zijn als grootste absolute waarde van alternatieve buffer limieten!"))
    }
    
    coordinaten_df <- data.frame(longitude = c(gebied_raster_mask %>% 
                                                 xmin() + list_coordinaten$x_min,
                                               gebied_raster_mask %>% 
                                                 xmax() + list_coordinaten$x_max),
                                 latitude = c(gebied_raster_mask %>% 
                                                ymin() + list_coordinaten$y_min,
                                              gebied_raster_mask %>% 
                                                ymax() + list_coordinaten$y_max))
    
    
    coordinaten_spv <- st_as_sf(coordinaten_df, coords = c("longitude", "latitude"), crs = 31370) %>% 
      vect()
    
    gebied_raster <- crop(gebied_raster, coordinaten_spv)
    
    if (!is.null(y_max)) {
      Bijschrift_buffer <- str_c(Bijschrift_buffer, ", ", y_max , " m omgeving ten Noorden,")
      }
    if (!is.null(x_max)) {
      Bijschrift_buffer <- str_c(Bijschrift_buffer, ", ", x_max , " m omgeving ten Oosten,")
      }
    if (!is.null(y_min)) {
      Bijschrift_buffer <- str_c(Bijschrift_buffer, ", ", y_min , " m omgeving ten Zuiden,")
      }
    if (!is.null(x_min)) {
      Bijschrift_buffer <- str_c(Bijschrift_buffer, ", ", x_min , " m omgeving ten Westen,")
      }
  }
  
  
  # Intervals ---
  
  # Get the min-max of the whole area
  buffer_minmax <- gebied_raster %>% 
    minmax()
    
  if (!is.null(min_interval)) {
    if (!between(min_interval, min(gebied_minmax), max(gebied_minmax))) {
      min_interval <- NULL}
  }
  if (!is.null(max_interval)) {
    if (!between(max_interval, min(gebied_minmax), max(gebied_minmax))) {max_interval <- NULL}
  }
  
  if (minmax_mask) {
    gebied_minmax = gebied_raster_mask %>% 
      minmax()
  } else {
    gebied_minmax <- buffer_minmax
  }
  
  start_interval <- ifelse(!is.null(min_interval),
                       min_interval, 
                       gebied_minmax %>% 
                       min()) %>% 
    RoundAny(., interval_m, floor)
  
  end_interval <- ifelse(!is.null(max_interval),
                          max_interval, 
                          gebied_minmax %>% 
                            max()) %>% 
    RoundAny(., interval_m, ceiling)
  
  
  if (minmax_mask) {
    rcl <- matrix(seq(start_interval - expand_intervals*interval_m,
                     end_interval + expand_intervals*interval_m, 
                     by = interval_m))
  } else {
    rcl <- matrix(seq(start_interval,
                      end_interval,
                      by = interval_m))
  }
  
  
  #Add lower or higher if needed.
  minbuffer <- RoundAny(min(buffer_minmax), interval_m, floor)
  maxbuffer <- RoundAny(max(buffer_minmax), interval_m, ceiling)
  
  if (minbuffer < min(rcl)) {
    rcl <- c(minbuffer, rcl) %>% 
      matrix()
  }
  if (maxbuffer > max(rcl)) {
    rcl <- c(rcl, maxbuffer) %>% 
      matrix()
  }
  
  
  if (!is.null(zomerpeil)) {
    
    if (!zomerpeil %in% rcl) {
      
      rcl <- c(rcl, zomerpeil) %>% 
        sort() %>% 
        matrix()
    }
  }
  
  
  # Classify the raster into new intervals.
  gebied_raster_classify <- gebied_raster %>% 
    classify(., rcl, include.lowest = TRUE, right = TRUE)
  
  # Kleuren
  # Alles onder zomerpeil wordt blauw
  if (!is.null(zomerpeil)) { 
    
    interval_zomperpeil <- rcl[rcl[, 1] < zomerpeil] %>% 
      length()
    
    zomerpeil_colors <- ColorPalette(zomerpeil_colours, interval_zomperpeil)
    
    interval_kleuren <- length(rcl) -  interval_zomperpeil
    colors <- ColorPalette(dtm_colours, interval_kleuren)
    colors <- c(zomerpeil_colors, colors)

  } else {
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
    theme(
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
    clip_schelde_sf <- ClipShapefile(gebied_raster, schelde_dokken_sf_f)
    
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
    
    clip_afwateringen_sf <- ClipShapefile(gebied_raster_classify, afwatering_sf_f) %>% 
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
  
  # # Add annotation
  # if (annotation) {
  #   
  #   G <- G %>% 
  #     AnnotationKaart(., referentie = gebied_raster_classify)
  # }
  # 
  
  Onderschrift <- str_c("Reliëf van ", gebied, " en ", gebied_buffer, "m omgeving",
                        Bijschrift_buffer ," met een hoogte-interval van ", interval_m, "m.")
  
  if (!is.null(zomerpeil)) {
    Onderschrift <- str_c(Onderschrift, " Het zomerpeil is gemiddeld ", zomerpeil, " mTAW.")
  }
  
  
  return(list(Kaart = G, Bijschrift = Onderschrift))
}


KaartGebiedDetailReliëf <- function(gebied_sf,
                                    gebied_buffer,
                                    interval_m = 0.2,
                                    min_interval = NULL,
                                    max_interval = NULL,
                                    minmax_mask = TRUE,
                                    expand_intervals = 2,
                                    locatie = "Q:/Projects/PRJ_Schelde/Haven/Studiegebied_DTM/DTM_gebieden_afgraving/",
                                    legende_gebied = FALSE, 
                                    #annotation = TRUE, 
                                    schelde_dokken = TRUE,
                                    schelde_dokken_sf_f = schelde_dokken_sf,
                                    afwateringen = TRUE,
                                    afwatering_sf_f = afwateringen_sf,
                                    dtm_colours = colours_dtm,
                                    zomerpeil = NULL, 
                                    zomerpeil_colours = colours_zomerpeil) {
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

  gebied <- gebied_sf %>% 
    st_drop_geometry() %>% 
    pull() %>% 
    str_c(., collapse = "_")
  
  
  # raster detail gebied + buffer
  gebied_raster <-  rast(str_c(locatie, gebied, "_afgraving.tif")) %>% 
    `crs<-` ("EPSG:31370") %>% 
    `names<-` ("Hoogte") %>% 
    crop(., y = gebied_sf %>% 
           st_buffer(., dist = gebied_buffer))
  
  
  # Hoogteverschil gebied & Originele coordinaten (xmin, max etc)
  locatie_mask <- GebiedDTMMask(gebied_sf)
  gebied_raster_mask <- rast(locatie_mask)
  
  Bijschrift_buffer <- ""
  # Extend needs to be adapted ! Laten staan voor het geval dit wel nodig is. Dan simpelweg nieuwe parameters in de functie steken.
  # if (any(!is.null(c(x_min, x_max, y_min, y_max)))) {
  #   # Originele coordinaten (xmin, max etc)
  #   
  #   # Replace NULL to gebied_buffer
  #   list_coordinaten <- lst(x_min, x_max, y_min, y_max) %>% 
  #     map(., ~ replace(.x, is.null(.x), gebied_buffer))
  #   
  #   # make xlim and ylim negative
  #   if (list_coordinaten$x_min > 0) {list_coordinaten$x_min <- -list_coordinaten$x_min}
  #   if (list_coordinaten$y_min > 0) {list_coordinaten$y_min <- -list_coordinaten$y_min}
  #   
  #   
  #   # New frame
  #   if (any(c(abs(list_coordinaten$x_min), abs(list_coordinaten$x_max), abs(list_coordinaten$y_min), abs(list_coordinaten$y_max)) > gebied_buffer)) {
  #     return(str("INVALID: buffer dient groter te zijn als grootste absolute waarde van alternatieve buffer limieten!"))
  #   }
  #   
  #   coordinaten_df <- data.frame(longitude = c(gebied_raster_mask %>% 
  #                                                xmin() + list_coordinaten$x_min,
  #                                              gebied_raster_mask %>% 
  #                                                xmax() + list_coordinaten$x_max),
  #                                latitude = c(gebied_raster_mask %>% 
  #                                               ymin() + list_coordinaten$y_min,
  #                                             gebied_raster_mask %>% 
  #                                               ymax() + list_coordinaten$y_max))
  #   
  #   
  #   coordinaten_spv <- st_as_sf(coordinaten_df, coords = c("longitude", "latitude"), crs = 31370) %>% 
  #     vect()
  #   
  #   gebied_raster <- crop(gebied_raster, coordinaten_spv)
  #   
  #   if (!is.null(y_max)) {Bijschrift_buffer <- str_c(Bijschrift_buffer, ", ", y_max , " m omgeving ten Noorden,")}
  #   if (!is.null(x_max)) {Bijschrift_buffer <- str_c(Bijschrift_buffer, ", ", x_max , " m omgeving ten Oosten,") }
  #   if (!is.null(y_min)) {Bijschrift_buffer <- str_c(Bijschrift_buffer, ", ", y_min , " m omgeving ten Zuiden,") }
  #   if (!is.null(x_min)) {Bijschrift_buffer <- str_c(Bijschrift_buffer, ", ", x_min , " m omgeving ten Westen,") }
  #   
  # }
  
  # Intervals ---
  # Get the min-max of the whole area
  buffer_minmax <- gebied_raster %>% 
    minmax()
  
  if (!is.null(min_interval)) {
    if (!between(min_interval, min(gebied_minmax), max(gebied_minmax))) {
      min_interval <- NULL}
  }
  if (!is.null(max_interval)) {
    if (!between(max_interval, min(gebied_minmax), max(gebied_minmax))) {max_interval <- NULL}
  }
  
  if (minmax_mask) {
    gebied_minmax = gebied_raster_mask %>% 
      minmax()
  } else {
    gebied_minmax <- buffer_minmax
  }
  
  start_interval <- ifelse(!is.null(min_interval),
                           min_interval, 
                           gebied_minmax %>% 
                             min()) %>% 
    RoundAny(., interval_m, floor)
  
  end_interval <- ifelse(!is.null(max_interval),
                         max_interval, 
                         gebied_minmax %>% 
                           max()) %>% 
    RoundAny(., interval_m, ceiling)
  
  
  if (minmax_mask) {
    rcl <- matrix(seq(start_interval - expand_intervals*interval_m,
                      end_interval + expand_intervals*interval_m, 
                      by = interval_m))
  } else {
    rcl <- matrix(seq(start_interval,
                      end_interval,
                      by = interval_m))
  }
  
  
  #Add lower or higher if needed.
  minbuffer <- RoundAny(min(buffer_minmax), interval_m, floor)
  maxbuffer <- RoundAny(max(buffer_minmax), interval_m, ceiling)
  
  if (minbuffer < min(rcl)) {
    rcl <- c(minbuffer, rcl) %>% 
      matrix()
  }
  if (maxbuffer > max(rcl)) {
    rcl <- c(rcl, maxbuffer) %>% 
      matrix()
  }
  
  
  if (!is.null(zomerpeil)) {
    
    if (!zomerpeil %in% rcl) {
      
      rcl <- c(rcl, zomerpeil) %>% 
        sort() %>% 
        matrix()
    }
  }
  
  
  # Classify the raster into new intervals.
  gebied_raster_classify <- gebied_raster %>% 
    classify(., rcl, include.lowest = TRUE, right = TRUE)
  
  # Kleuren
  # Alles onder zomerpeil wordt blauw
  if (!is.null(zomerpeil)) { 
    
    interval_zomperpeil <- rcl[rcl[, 1] < zomerpeil] %>% 
      length()
    
    zomerpeil_colors <- ColorPalette(zomerpeil_colours, interval_zomperpeil)
    
    interval_kleuren <- length(rcl) -  interval_zomperpeil
    colors <- ColorPalette(dtm_colours, interval_kleuren)
    colors <- c(zomerpeil_colors, colors)
    
  } else {
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
    theme(
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
    clip_schelde_sf <- ClipShapefile(gebied_raster, schelde_dokken_sf_f)
    
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
    
    clip_afwateringen_sf <- ClipShapefile(gebied_raster_classify, afwatering_sf_f) %>% 
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
  
  # # Add annotation
  # if (annotation) {
  #   
  #   G <- G %>% 
  #     AnnotationKaart(., referentie = gebied_raster_classify)
  # }
  # 
  
  Onderschrift <- str_c("Reliëf van afgegraven ", gebied, " met een hoogte-interval van ", interval_m, " m.")

  if (!is.null(zomerpeil)) {
    Onderschrift <- str_c(Onderschrift, " Het zomerpeil is gemiddeld ", zomerpeil, " mTAW.")
  }
  
  
  return(list(Kaart = G, Bijschrift = Onderschrift))
}


KaartBodemsamenstelling <- function(gebied_sf, 
                                    gebied_buffer, 
                                    legende_gebied = FALSE) {
  
  locatie <- GebiedenBodemkaart(gebied_sf, gebied_buffer)
  
  Gebied_bodemkaart <- st_read(dsn = locatie) %>% 
    group_by(Gnrlst_) %>% 
    summarise(., .groups = "drop")
  

  G <- ggplot() +
    geom_sf(data = Gebied_bodemkaart, aes(fill = Gnrlst_)) +
    scale_fill_manual(values = colours_bodemkaart) +
    guides(fill = guide_legend(reverse = FALSE)) +
    geom_sf(data = gebied_sf, alpha = 0, color = "black", linetype = "solid", lwd = 1,) +
    theme(
          panel.background = element_blank(), 
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank()) +
    labs(fill = "Bodemtype", col = "Gebied")
  
  
  if (legende_gebied) {
    G <- G + 
      geom_sf(data = gebied_sf, color = "black", alpha = 0, linetype = "dashed") +
      guides(fill = guide_legend(order = 1, reverse = TRUE),
             color = guide_legend(order = 2))
  }
  
 
  gebied <- gebied_sf %>% 
    st_drop_geometry() %>% 
    pull() %>% 
    str_c(., collapse = "_")
  
  
  Onderschrift <- str_c("Bodemkaart van ", gebied, " en ", gebied_buffer, "m omgeving.")
  return(list(Kaart = G, Bijschrift = Onderschrift))
}




KaartWaterpeil <- function(gebied_sf, gebied_buffer = 0, 
                           locatie = "Q:/Projects/PRJ_Schelde/Haven/Studiegebied_DTM",
                           locatie_jaar = "2017",
                           peil, titel = "Oppervlakte") {
  # Get the dtm of an area classified to an given water table. Everything under this waterlevel is blue,
  # between 0 and 20cm is lichtgreen,; betwen 25 and 40 is darkgreen, and everything above is beige.
  # For the areas with detailed topographic raster, this raster will be used.
    # Parameters:
    # gebied: area
    # gebied_buffer: standard 0, only for areas without detailed raster.
    # Peil: Reference waterstand
  
  gebied <- gebied_sf %>% 
    st_drop_geometry() %>% 
    pull()
  
  if (gebied %in% gebieden_afgraving) {
    
    box_gebied_buffer <- gebied_sf %>% 
      st_buffer(., dist = gebied_buffer) %>% 
      st_bbox() %>% 
      st_as_sfc() %>% 
      vect()
    
    gebied_raster <-  rast(str_c(locatie, "/DTM_gebieden_afgraving/", gebied, "_afgraving.tif")) %>% 
      `crs<-` ("EPSG:31370") %>%
      `names<-` ("Hoogte") %>% 
      crop(., y = gebied_sf %>% 
             st_buffer(., dist = gebied_buffer))
    
  } else {
    
    gebied_raster <- GebiedDTM(gebied_sf,
                         gebied_buffer = gebied_buffer,
                         output_path = "Q:/Projects/PRJ_Schelde/Haven/studiegebied_DTM",
                         DTM_Jaar = locatie_jaar, 
                         Locatie_DTM = "S:/Vlaanderen/Hoogte/DHMVII/DHMVIIDTMRAS1m.tif") %>% 
      rast(.)
  }
  

  # Calculate the referene height to a given waterlevel.
  gebied_raster_peil <-  gebied_raster - peil
  #Get min and max value
  gebied_minmax <- gebied_raster_peil %>% 
    minmax()
  # Classify in nice rounded min and  max, and everything in between (0, 0.25 and 40)
  rcl <-  matrix(c(RoundAny(min(gebied_minmax), 0.5, floor), 0, 0.25, 0.40, RoundAny(max(gebied_minmax), 0.5, ceiling)))
  
  gebied_raster_classify <- gebied_raster_peil %>% 
    classify(., rcl, include.lowest = TRUE, right = TRUE)
  
  # Kleuren, zo aan te passen zodat ze in de grafiek correct owrden overgenomen als er bepaalde klasses niet bestaan.
  variables <- unique(gebied_raster_classify$Hoogte) %>% 
    mutate(
      Kleur = case_when(
        str_detect(Hoogte, "0]") & 
          str_sub(Hoogte, 1, 1) == "[" ~ "#0373F9",  # Voorkom als maximale hoogte ook eindigt met 0 dit blauw is.
        str_detect(Hoogte, "0.25]") ~ "#4DE600",
        str_detect(Hoogte, "0.4]") ~ "#9FFF73",
        str_detect(Hoogte, "0.4") ~ "#FFEBBE"))

  # colors <- c("#0373F9", "#9FFF73", "#4DE600", "#FFEBBE") "#9FFF73"
  titel <- str_c(titel, "peil")

  G <- ggplot() +
    tidyterra::geom_spatraster(data = gebied_raster_classify, aes(fill = Hoogte)) +
    
    scale_fill_manual(values = variables$Kleur, labels = variables$Hoogte, breaks = variables$Hoogte) +

    guides(fill = guide_legend(reverse=TRUE)) +
    geom_sf(data = gebied_sf, alpha = 0, col = "black", lwd = 1, linetype = "solid") +
    theme(
          panel.background = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank()) +
    labs(fill = "m t.o.v. maaiveld", subtitle = str_c(titel, ": ", peil, " mTAW")) +
    guides(fill = guide_legend(order = 1, reverse = TRUE))
  
  Onderschrift <- str_c("Hoogteligging van ", gebied, " tegenover het maaiveld bij een waterpeil van ", peil, " mTAW.")
  
  return(list(Kaart = G, Bijschrift = Onderschrift))
}

# kleur_zand <- "#FFFDB2"
# kleur_leem <- "#DB3A4F"
# kleur_zandigleem <- "#FA8D5A"
# kleur_lemigzand <- "#FDE183"
# kleur_klei <- "dodgerblue4"
# kleur_zandigklei <- "steelblue3"
# kleur_kleiigzand <- "lightskyblue1"
# kleur_veen <- "palegreen4"
# Boorprofielen ----------------------------------------------------------------
Boorprofiel <- function(Beschrijving, Balkbreedte = 0.4, MaxDiepte = NULL, tekstgrootte = 3, legendecolor = colour_INBO) {
  # Beschrijving is de beschrijving voor een specieke staal.
  # Dit is een ondersteunende functie voor de Boorprofielen.
  
  kleur_zand <- "#EEDFB9"
  kleur_leem <- "#EAD0C9"
  kleur_zandigleem <- "#C2B342"
  kleur_lemigzand <- "#A69537"
  kleur_klei <- "#206375"
  kleur_zandigklei <- "#6D8532"
  kleur_kleiigzand <- "#85A444"
  kleur_veen <- "#645D37"
  
  Beschrijving <- Beschrijving %>%
    mutate(
      across(c(van, tot), ~ -abs(.x)),
      Kleur = case_when(
        bodemtype == "zand" ~ kleur_zand,
        bodemtype == "leem" ~ kleur_leem,
        bodemtype == "zandig leem" ~  kleur_zandigleem,
        bodemtype == "lemig zand" ~ kleur_lemigzand,
        bodemtype == "klei" ~ kleur_klei,
        bodemtype == "zandige klei"~ kleur_zandigklei,
        bodemtype == "kleiig zand"~ kleur_kleiigzand,
        bodemtype == "veen"~ kleur_veen
    ))
  
  if (is.null( MaxDiepte)) {
    MaxDiepte <- -max(Beschrijving$tot)
  } 
  
  Graf <- ggplot(data = data.frame(x = c(0,1),y=c(0,MaxDiepte))) + geom_line(aes(x,y), color = "white") +
    theme(panel.background = element_rect(color = "white", fill = "white"))
  

  Beschrijving <- subset(Beschrijving, van >= MaxDiepte)
  
  if (Beschrijving %>% 
      summarise(., min = min(tot)) %>% 
      pull() >= -MaxDiepte) {
    Beschrijving[nrow(Beschrijving),]$tot <- MaxDiepte
  }
  
  
  for (Lijn in 1:nrow(Beschrijving)) {
    Lijn.Kleur <- Beschrijving$Kleur[Lijn]
    Lijn.van.y <- Beschrijving$van[Lijn]
    Lijn.tot.y <- Beschrijving$tot[Lijn]
    Lijn.van.x <- 0
    Lijn.tot.x <- Balkbreedte
    Graf <- Graf + annotate("rect", xmin = Lijn.van.x, xmax = Lijn.tot.x, ymin = Lijn.tot.y, ymax = Lijn.van.y, fill = Lijn.Kleur)
  }
  
  # Plot the colors and depth
  Graf <- Graf + 
    theme(axis.ticks = element_blank()) + 
    theme(axis.title.x = element_blank()) + 
    theme(axis.text.x = element_blank()) + 
    ylab("Diepte (m t.o.v. maaiveld)") +
    theme(axis.title.y = element_text(margin = margin(0.5,0.5,0.5,0.5,"cm"))) +
    # scale_y_continuous(breaks = c(0,-Beschrijving$tot)) +
    scale_y_continuous(breaks = c(0,Beschrijving$tot))
  

  # Legende
  Item = 1
  Graf <- Graf + 
    annotate("rect", xmin = 0.6, xmax = 0.65, ymax = (0.05 + 0.1*(Item - 1))*MaxDiepte, 
                          ymin = (0.1 + 0.1*(Item - 1))*MaxDiepte, fill = kleur_zand) +
    annotate("text", x = 0.7, y = (0.07 + 0.1*(Item - 1))*MaxDiepte, label = "zand", hjust = 0, size = tekstgrootte, 
             colour = legendecolor)
  
  Item = 2
  Graf <- Graf + 
    annotate("rect", xmin = 0.6, xmax = 0.65, ymax = (0.05 + 0.1*(Item - 1))*MaxDiepte, 
             ymin = (0.1 + 0.1*(Item - 1))*MaxDiepte, fill = kleur_klei) + 
    annotate("text", x = 0.7, y = (0.07 + 0.1*(Item - 1))*MaxDiepte, label = "klei", hjust = 0, size = tekstgrootte, 
             colour = legendecolor)
  
  Item = 3
  Graf <- Graf + 
    annotate("rect", xmin = 0.6, xmax = 0.65, ymax = (0.05 + 0.1*(Item - 1))*MaxDiepte, 
             ymin = (0.1 + 0.1*(Item - 1))*MaxDiepte, fill = kleur_zandigklei) + 
    annotate("text", x = 0.7, y = (0.07 + 0.1*(Item - 1))*MaxDiepte, label = "zandige klei", hjust = 0, 
             size = tekstgrootte, colour = legendecolor)
  
  Item = 4
  Graf <- Graf + 
    annotate("rect", xmin = 0.6, xmax = 0.65, ymax = (0.05 + 0.1*(Item - 1))*MaxDiepte, ymin = (0.1 + 0.1*(Item - 1))*MaxDiepte, 
             fill = kleur_kleiigzand) + 
    annotate("text", x = 0.7, y = (0.07 + 0.1*(Item - 1))*MaxDiepte, label = "kleiig zand", hjust = 0, size = tekstgrootte, 
             colour = legendecolor)
  
  Item = 5
  Graf <- Graf + 
    annotate("rect", xmin = 0.6, xmax = 0.65, ymax = (0.05 + 0.1*(Item - 1))*MaxDiepte, ymin = (0.1 + 0.1*(Item - 1))*MaxDiepte, 
             fill = kleur_leem) +
    annotate("text", x = 0.7, y = (0.07 + 0.1*(Item - 1))*MaxDiepte, label = "leem", hjust = 0, size = tekstgrootte, 
             colour = legendecolor)
  
  Item = 6
  Graf <- Graf +
    annotate("rect", xmin = 0.6, xmax = 0.65, ymax = (0.05 + 0.1*(Item - 1))*MaxDiepte, ymin = (0.1 + 0.1*(Item - 1))*MaxDiepte, 
             fill = kleur_zandigleem) + 
    annotate("text", x = 0.7, y = (0.07 + 0.1*(Item - 1))*MaxDiepte, label = "zandig leem", hjust = 0, size = tekstgrootte, 
             colour = legendecolor)
  
  Item = 7
  Graf <- Graf + 
    annotate("rect", xmin = 0.6, xmax = 0.65, ymax = (0.05 + 0.1*(Item - 1))*MaxDiepte, ymin = (0.1 + 0.1*(Item - 1))*MaxDiepte, 
             fill = kleur_lemigzand) + 
    annotate("text", x = 0.7, y = (0.07 + 0.1*(Item - 1))*MaxDiepte, label = "lemig zand", hjust = 0, size = tekstgrootte, 
             colour = legendecolor)
  
  Item = 8
  Graf <- Graf + 
    annotate("rect", xmin = 0.6, xmax = 0.65, ymax = (0.05 + 0.1*(Item - 1))*MaxDiepte, ymin = (0.1 + 0.1*(Item - 1))*MaxDiepte, 
             fill = kleur_veen) + 
    annotate("text", x = 0.7, y = (0.07 + 0.1*(Item - 1))*MaxDiepte, label = "veen", hjust = 0, size = tekstgrootte, 
             colour = legendecolor)
  
  Graf <- Graf + geom_hline(yintercept = 0) + 
    annotate("curve",x = 0.89,xend = 0.9,y = -0.01*MaxDiepte, yend = -0.01*MaxDiepte, curvature = 0, linewidth = 1) + 
    annotate("curve",x = 0.88,xend = 0.91,y = -0.02*MaxDiepte, yend = -0.02*MaxDiepte, curvature = 0, linewidth = 1) + 
    annotate("curve",x = 0.87,xend = 0.92,y = -0.03*MaxDiepte, yend = -0.03*MaxDiepte, curvature = 0, linewidth = 1)
  
  return(Graf)
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
KaartOrthoMaptiles <- function(studiegebied, 
                               buffer_m = 50, 
                               zoom_plot = 17,
                               factor_extend = 1/3,
                               provider_name = "Esri.WorldImagery") {
  
  
  base_layer_df <- studiegebied %>% 
    st_buffer(., dist = buffer_m) %>% 
    AdjustPlotExtend(., factor = factor_extend) %>% 
    get_tiles(., provider = provider_name, crop = FALSE, zoom = zoom_plot) %>% 
    as.data.frame(., xy = TRUE) %>% 
    rename("red" = 3, "green" = 4, "blue" = 5)
  
  
  kaart <- ggplot() +
    geom_raster(data = base_layer_df, aes(x = x, y = y, fill = rgb(red = red,
                                                                   green = green,
                                                                   blue = blue,
                                                                   maxColorValue = 255))) +
    scale_fill_identity() +
    labs(caption = get_credit(provider_name)) +
    theme(
      plot.margin = margin(0, 0, 0, 0),  # Remove plot margins
      panel.spacing = unit(0, "lines"),
      panel.grid.major = element_blank(),  # Remove major grid lines
      panel.grid.minor = element_blank(),  # Remove minor grid lines
      axis.text = element_blank(),  # Remove axis text
      axis.ticks = element_blank(),
      axis.title = element_blank()
    )
  
  return(kaart)
}


KaartAddGebied <- function(kaart, studiegebied, alpha_polygon = 0, colors_polygon = "black", lwd_line = 2) {
  
  kaart <- kaart +
     geom_sf(data = studiegebied, alpha = alpha_polygon, color = colors_polygon, lwd = lwd_line)
  
  return(kaart)
}



KaartOrthoTIF <- function(gebied_sf,
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

# DTM -profiel

extract_dtm_profile <- function(line_sf, 
                                sampling_points = 100,
                                direction = TRUE,
                                dtm_location = "S:/Vlaanderen/Hoogte/DHMVII/DHMVIIDTMRAS1m.tif") {
  
  dtm_algemeen <- rast(dtm_location)
  
  raai_dtm <- line_sf %>% 
    vect(.) %>% 
    crop(dtm_algemeen, .) %>% 
    stats::setNames(., nm =  "Hoogte")
  
  raai_sampled <- line_sf %>% 
    st_line_sample(., n = sampling_points) %>% 
    st_cast("POINT") %>%
    st_as_sf()
  
  if (!direction) {
    raai_sampled <- raai_sampled[nrow(raai_sampled):1, ]
  }
  
  coords <- st_coordinates(raai_sampled)
  # Pythagoras
  deltas <- sqrt(diff(coords[,1])^2 + diff(coords[,2])^2)
  
  # 3. Extract hoogtewaarden
  raai_sampled_sf <- raai_sampled %>% 
    mutate(
      distance = c(0, cumsum(deltas)), 
      height = terra::extract(raai_dtm, vect(raai_sampled))[,2]) %>% 
    rename("geometry" = x)
  
  return(raai_sampled_sf)
}

distance_along_line <- function(line_sf, points_sf, distance = TRUE, density = 0.1) {
  
  line_points <- st_line_sample(line_sf, density = 1 / density) %>%
    st_cast("POINT") %>% 
    st_as_sf()
  
  if (distance) {
    line_points <- line_points %>% 
      slice_head(n = 1)
    
  } else {
    line_points <- line_points %>% 
      slice_tail(n = 1)
  }
  
  points_sf <- points_sf %>% 
    mutate(distance = st_distance(., line_points) %>% 
             drop_units() %>% 
             as.numeric(.))
  
  return(points_sf)
}


