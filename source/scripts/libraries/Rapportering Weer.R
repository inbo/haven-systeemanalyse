# Maak rapport van de weersomstandingheden in het broedseizoen van het gekozen werkjaar en van de winter voorafgaand aan dat broedseizoen

werk_jaar <- 2021

FigSave = TRUE # als TRUE worden de grafieken als afzonderlijke pdf gesaved om apart te kunnen doorsturen aan ANB

library(officer)
library(grid)
library(gridExtra)
library(tidyverse)
source("source/libraries/Kleurdefinities ANB.R")


# Lees gegevens
data_weer <- read.table(file = "data/tblGegBrasschaat.txt", header = T, sep = "\t", na.strings = c("NA")) %>% 
  tibble() %>% 
  mutate(
    DatumTijd = as.POSIXct(str_c(Jaar, "-", Maand, "-", Dag, " ", Uur, ":", Minuten), origin = "1970-01-01 00:00",  format = "%Y-%m-%d %H:%M"),
    across(.cols = c(Temperatuur, Neerslag, Windsnelheid, RV, Luchtdruk), ~ as.double(.x))
    )

jaarkeuze <- werk_jaar
weertabel <- data_weer

# Kleurdefinities voor Grafieken
Kleur.Temperatuur <- Kleur16
VulKleur.Temperatuur <- Kleur13

Kleur.Neerslag <- Kleur15
VulKleur.Neerslag <- Kleur14

# Maak Grafieken Temperatuur broedseizoen
G1T <- GrafiekWeerBroedseizoen(jaarkeuze = werk_jaar, weertabel = data_weer, 
                        parameter = "Tgem", lijnkleur = Kleur.Temperatuur) %>% 
  Opmaak.Graf(., Grid = FALSE) +
  annotate("text", x = as.POSIXct(paste0("20/03/",werk_jaar), format = "%d/%m/%Y", origin = "01/01/1970"), y = 23,label = "a")

G1T


G2T <- GrafiekWeerBroedseizoenHalveMaand(jaarkeuze = werk_jaar, weertabel = data_weer, 
                                        parameter = "Tgem",
                                        kleur = Kleur.Temperatuur,
                                        vulkleur = VulKleur.Temperatuur, alpha = 1, include_totaal  = TRUE) %>% 
  Opmaak.Graf(., Grid = FALSE) +
  annotate("text", x = 1, y = 21,label = "b")

G2T
# Maak Grafieken Neerslag broedseizoen


G1NS <- GrafiekWeerBroedseizoen(jaarkeuze = werk_jaar, weertabel = data_weer, 
                                parameter = "NStot", lijnkleur = Kleur.Neerslag) %>% 
  Opmaak.Graf(., Grid = FALSE) +
  annotate("text", x = as.POSIXct(paste0("20/03/",werk_jaar), format = "%d/%m/%Y", origin = "01/01/1970"), y = 23,label = "a")


G1NS

G2NS <- GrafiekWeerBroedseizoenHalveMaand(jaarkeuze = werk_jaar, weertabel = data_weer, 
                                          parameter = "NStot",
                                          kleur = Kleur.Neerslag,
                                          vulkleur = VulKleur.Neerslag, alpha = 1, include_totaal  = TRUE)

G2NS
# # Maak Grafieken Neerslag broedseizoen
# 
# G1 <- Opmaak.Graf(Grafiek.Weer.Broedseizoen(WerkJaar,Data.Weer,Parameter = "Ntot", LijnKleur = Kleur.Neerslag) + annotate("text",x = as.POSIXct(paste0("20/03/",WerkJaar),format = "%d/%m/%Y", origin = "01/01/1970"),y = 23,label = "a"),Grid = F)
# G2 <- Opmaak.Graf(Grafiek.Weer.Broedseizoen.HalveMaand(WerkJaar,Data.Weer,Parameter = "Ntot", Kleur = Kleur.Neerslag, VulKleur = VulKleur.Neerslag, Alpha = 1, IncludeTotaal = T) + annotate("text",x = 1,y = 20,label = "b"), Grid = F)
# 
# grid.newpage()
# pushViewport(viewport(layout = grid.layout(1, 14)))
# print(G1,vp = viewport(layout.pos.row = 1, layout.pos.col = 1:6))
# print(G2,vp = viewport(layout.pos.row = 1, layout.pos.col = 7:14))
# 
# Lay <- t(as.matrix(c(rep(1,6),rep(2,8))))
# ggsave(file = paste0("Neerslag Broedseizoen ",WerkJaar,".jpg"),grid.arrange(G1,G2,nrow = 1,layout_matrix = Lay), width = 16/2.54, height = 7/2.54, units = "in")
# 
# if (FigSave) {
#   Lay <- t(as.matrix(c(rep(1,6),rep(2,8))))
#   ggsave(file = paste(OutputLocation,paste0("Neerslag Broedseizoen ",WerkJaar,".pdf"),sep = "/"),grid.arrange(G1,G2,nrow = 1,layout_matrix = Lay), width = 16/2.54, height = 7/2.54, units = "in")
# }

# Maak Grafieken Temperatuur Winter

G1Twinter <- GrafiekWeerWinter(jaarkeuze = werk_jaar, weertabel = data_weer, 
                               parameter = "Tgem", lijnkleur = Kleur.Temperatuur, labels_x2 = FALSE) %>% 
  Opmaak.Graf(., Grid = FALSE)

G1Twinter


G1NSwinter <- GrafiekWeerWinter(jaarkeuze = werk_jaar, weertabel = data_weer, 
                                parameter = "NStot", lijnkleur = Kleur.Temperatuur, labels_x2 = FALSE) %>% 
  Opmaak.Graf(., Grid = FALSE)

G1NSwinter




G2T <- GrafiekWeerWintermaand(jaarkeuze = werk_jaar, weertabel = data_weer, 
                              parameter = "Tgem",
                              kleur = Kleur.Neerslag,
                              vulkleur = VulKleur.Neerslag, alpha = 1, include_totaal  = TRUE) %>% 
  Opmaak.Graf(., Grid = FALSE)

G2T

G2NS <- GrafiekWeerWintermaand(jaarkeuze = werk_jaar, weertabel = data_weer, 
                              parameter = "NStot",
                              kleur = Kleur.Neerslag,
                              vulkleur = VulKleur.Neerslag, alpha = 1, include_totaal  = TRUE)
G2NS

G1 <- Opmaak.Graf(Grafiek.Weer.Winter(WerkJaar - 1,Data.Weer,Parameter = "Tgem", LijnKleur = Kleur.Temperatuur) + annotate("text",x = as.POSIXct(paste0("20/10/",(WerkJaar - 1)),format = "%d/%m/%Y", origin = "01/01/1970"),y = 23,label = "a"),Grid = F)
G2 <- Opmaak.Graf(Grafiek.Weer.Winter.Maand(WerkJaar - 1,Data.Weer,Parameter = "Tgem", Kleur = Kleur.Temperatuur, VulKleur = VulKleur.Temperatuur, Alpha = 1, IncludeTotaal = T) + annotate("text",x = 1,y = 20,label = "b"), Grid = F)

grid.newpage()
pushViewport(viewport(layout = grid.layout(1, 14)))
print(G1,vp = viewport(layout.pos.row = 1, layout.pos.col = 1:6))
print(G2,vp = viewport(layout.pos.row = 1, layout.pos.col = 7:14))

Lay <- t(as.matrix(c(rep(1,6),rep(2,8))))
ggsave(file = paste0("Temperatuur Winter ",WerkJaar-1,"-","WerkJaar",".jpg"),grid.arrange(G1,G2,nrow = 1,layout_matrix = Lay), width = 16/2.54, height = 7/2.54, units = "in")

if (FigSave) {
  Lay <- t(as.matrix(c(rep(1,6),rep(2,8))))
  ggsave(file = paste(OutputLocation,paste0("Temperatuur Winter ",WerkJaar-1,"-",WerkJaar,".pdf"),sep = "/"),grid.arrange(G1,G2,nrow = 1,layout_matrix = Lay), width = 16/2.54, height = 7/2.54, units = "in")
}

# Maak Grafieken Neerslag winter

G1 <- Opmaak.Graf(Grafiek.Weer.Winter(WerkJaar - 1,Data.Weer,Parameter = "Ntot", LijnKleur = Kleur.Neerslag) + annotate("text",x = as.POSIXct(paste0("20/10/",(WerkJaar - 1)),format = "%d/%m/%Y", origin = "01/01/1970"),y = 33,label = "a"),Grid = F)
G2 <- Opmaak.Graf(Grafiek.Weer.Winter.Maand(WerkJaar - 1,Data.Weer,Parameter = "Ntot", Kleur = Kleur.Neerslag, VulKleur = VulKleur.Neerslag, Alpha = 1, IncludeTotaal = T) + annotate("text",x = 1,y = 500,label = "b"), Grid = F)

grid.newpage()
pushViewport(viewport(layout = grid.layout(1, 14)))
print(G1,vp = viewport(layout.pos.row = 1, layout.pos.col = 1:6))
print(G2,vp = viewport(layout.pos.row = 1, layout.pos.col = 7:14))

Lay <- t(as.matrix(c(rep(1,6),rep(2,8))))
ggsave(file = paste0("Neerslag Winter ",WerkJaar-1,"-",WerkJaar,".jpg"),grid.arrange(G1,G2,nrow = 1,layout_matrix = Lay), width = 16/2.54, height = 7/2.54, units = "in")

if (FigSave) {
  Lay <- t(as.matrix(c(rep(1,6),rep(2,8))))
  ggsave(file = paste(OutputLocation,paste0("Neerslag Winter ",WerkJaar-1,"-",WerkJaar,".pdf"),sep = "/"),grid.arrange(G1,G2,nrow = 1,layout_matrix = Lay), width = 16/2.54, height = 7/2.54, units = "in")
}


# Maak output document
Doc <- read_docx("Weer template.docx")

Doc <- body_replace_all_text(Doc, old_value = "Weer", new_value = paste("Weer", WerkJaar))

Doc <- cursor_reach(Doc, keyword = "Tekst1")
Doc <- body_add_img(Doc, paste0("Temperatuur Broedseizoen ",WerkJaar,".jpg"), width = 16/2.54, height = 7/2.54)
Doc <- cursor_reach(Doc, keyword = "Tekst1")
Doc <- body_remove(Doc)

Doc <- cursor_reach(Doc, keyword = "Tekst2")
Doc <- body_add_img(Doc, paste0("Neerslag Broedseizoen ",WerkJaar,".jpg"), width = 16/2.54, height = 7/2.54)
Doc <- cursor_reach(Doc, keyword = "Tekst2")
Doc <- body_remove(Doc)

Doc <- cursor_reach(Doc, keyword = "Tekst3")
Doc <- body_add_img(Doc, paste0("Temperatuur Winter ",WerkJaar-1,"-","WerkJaar",".jpg"), width = 16/2.54, height = 7/2.54)
Doc <- cursor_reach(Doc, keyword = "Tekst3")
Doc <- body_remove(Doc)

Doc <- cursor_reach(Doc, keyword = "Tekst4")
Doc <- body_add_img(Doc, paste0("Neerslag Winter ",WerkJaar-1,"-",WerkJaar,".jpg"), width = 16/2.54, height = 7/2.54)
Doc <- cursor_reach(Doc, keyword = "Tekst4")
Doc <- body_remove(Doc)

Doc_file <- print(Doc, target = paste0(OutputLocation,"/",OutputLocationName,".docx"))

unlink(paste0("Temperatuur Broedseizoen ",WerkJaar,".jpg"))
unlink(paste0("Neerslag Broedseizoen ",WerkJaar,".jpg"))
unlink(paste0("Temperatuur Winter ",WerkJaar-1,"-","WerkJaar",".jpg"))
unlink(paste0("Neerslag Winter ",WerkJaar-1,"-",WerkJaar,".jpg"))
