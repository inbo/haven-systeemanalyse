location_annotation <- "bl" #library theme

width_fiche_kaart <- 16
height_fiche_kaart <- 10


width_graph <- 16
height_graph <- 10

height_graph_big <- 20

colour_INBO <- inbo_steun_donkerroos


color_ondiep_ANB = 	"#BFD6B6"
color_diep_ANB = "#356196"
color_midden_ANB = "#C77A44"

afmetingen_hydrochemie <- list(
  "geleidbaarheid en zuurtegraad" = c(width_graph, height_graph),
  "de ionen" = c(width_graph, height_graph_big),
  "nutriënten" = c(width_graph, height_graph_big), 
  "chlorofyl a en faeofytine" = c(width_graph, height_graph),
  "BOD en COD" = c(width_graph, height_graph))

past_years <- 10



title_text_size <- 7
axis_text_size <- 6
axis_title_size <- 7
legend_title_size <- 7
legend_text_size <- 6 

layout_timeseries <- function(figuur, enkel_meetpunt = TRUE,
                              facet = FALSE,
                              xlab = TRUE,
                              axis.text.size  = axis_text_size, axis.title.size = axis_title_size,
                              legend.title.size = legend_title_size,
                              legend.text.size = legend_text_size,
                              title.text.size = title_text_size,
                              colour_text = "#2D332B") {
  figuur <- figuur +
    theme(
      axis.title = element_text(size = axis.title.size, face = "bold"),
      axis.text = element_text(colour = colour_text, size = axis.text.size), 
      axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.6),
      title = element_text(size = title.text.size),
      plot.margin = unit(c(0,0, 0, 0), "cm"),
      panel.background = element_rect(fill = NA,
                                      colour = NA,
                                      linewidth = 0.25, linetype = "solid"),
      panel.grid.major = element_line(linewidth  = 0.5, linetype = 'solid',
                                      colour = "gray80"),
      panel.grid.minor = element_line(linewidth  = 0.1, linetype = 'solid',
                                      colour = "gray80"))
  
  if (!enkel_meetpunt) {
    figuur <- figuur +
      theme(legend.position = "right",  # Adjust the legend position
            legend.key.size = unit(0.4, "cm"),  # Size legend-labels
            legend.title = element_text(size = legend.title.size),  # size legend titles
            legend.spacing = unit(0.2, "cm"),  # sapce between different legend
            legend.text = element_text(size = legend.text.size,
                                       color = colour_text) # legend text size
            # legend.margin = margin(0, 0, 0, 0, unit = "cm"),  # legend bordered above and below legend.
            )
  }
  
  if (facet) {
    figuur <- figuur +
      theme(strip.text = element_text(size = axis.title.size, colour= "black"),
            strip.background = element_rect(colour = "black", fill = NA))
  }
  
  if (!xlab) {
    figuur <- figuur +
      labs(x = "")
    }
  
  return(figuur)
}




layout_maps <- function(kaart, legend.title.size = legend_title_size, axis.title.size = axis_title_size,
                        subtitle.size = 9,
                        expand = TRUE,
                        kleur_text = "#2D332B") {
  
  kaart <- kaart + 
    labs(x = "", y = "") +
    theme(
      legend.position = "right",  # Adjust the legend position
      legend.key.size = unit(0.4, "cm"),  # Size legend-labels
      legend.title = element_text(size = legend.title.size),  # size legend titles
      legend.spacing = unit(0.2, "cm"),  # sapce between different legend
      legend.text = element_text(size = axis.title.size, color = kleur_text), # legend text size
      legend.margin = margin(0, 0, 0, 0, unit = "cm"),  # legend bordered above and below legend.
      plot.subtitle = element_text(size = subtitle.size),
      # top right bottom left
      plot.margin = unit(c(0, 0, 0, 0), "cm"),
      axis.text = element_blank(),  # Remove axis text
      axis.ticks = element_blank(),
      axis.title = element_blank()
    )
  
  if (!expand) {
    kaart <- kaart +
      coord_sf(expand = FALSE)
  }
  return(kaart)
}
