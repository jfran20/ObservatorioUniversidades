library(sf)
library(ggplot2)
library(dplyr)
library(openxlsx)
library(tools)
library(leaflet)
library(leaflet.minicharts)
library(RColorBrewer)
library(shiny)
library(plotly)
library(reactable)
library(shinycssloaders)
library(magrittr)
library(highcharter)

# Datos ---- 

municipios <- read_sf("./www/SHAPEFILE/areas_geoestadisticas_municipales.shp") %>% 
  filter(CVE_ENT == 11) %>% mutate(NOM_MUN = toupper(NOM_MUN)) %>% 
  st_transform("+init=epsg:4326")


otros_mun <- read_sf("./www/SHAPEFILE/areas_geoestadisticas_estatales.shp") %>% 
  filter(CVE_ENT %in% c("11","14","24","22","16","01","32"))  %>% 
  st_transform("+init=epsg:4326")

ANUIES <- read.xlsx("./www/Datos/ANUIES.xlsx") %>%  
  filter(ENTIDAD == "GUANAJUATO", CICLO == "2019-2020")

OANU <- read.xlsx("./www/Datos/ANUIES.xlsx") %>%  
  filter(ENTIDAD %in% c("SAN LUIS POTOSÍ","JALISCO","AGUASCALIENTES","MICHOACÁN","QUERÉTARO","ZACATECAS","GUANAJUATO"), CICLO == "2019-2020") %>% 
  mutate(CVE_ENT = ifelse(ENTIDAD == "SAN LUIS POTOSÍ","24",
                          ifelse(ENTIDAD == "JALISCO","14",
                                 ifelse(ENTIDAD == "AGUASCALIENTES","01",
                                        ifelse(ENTIDAD == "MICHOACÁN","16", 
                                               ifelse(ENTIDAD == "QUERÉTARO", "22",
                                                      ifelse(ENTIDAD == "GUANAJUATO","11","32")))))))

TOP25 <- read.csv("./www/Datos/TopUniversidades.csv")

addResourcePath("icono","./www/Iconos/LaSalleBlanco.png")
addResourcePath("Fondo","./www/Iconos/Puntos Salle2.png")

source("ANUIES.R", encoding= "UTF-8")