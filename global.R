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

# Datos de apoyo ----
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

#####################################################################

CarreraSalle <- ANUIES %>% 
  filter(NOMBRE.INSTITUCIÓN.ANUIES == "UNIVERSIDAD LA SALLE, A.C. - BAJÍO") %>% 
  pull(Carrera) %>% unique()

carreras <- ANUIES %>% select(Carrera) %>% 
  group_by(Carrera) %>% summarise(Veces = n()) %>% arrange(-Veces) %>% head(5)

base_map <- leaflet(options = leafletOptions(minZoom = 8, maxZoom = 8,zoomControl = F, attributionControl = F,dragging = F)) %>% 
  addTiles (urlTemplate = "https://{s}.basemaps.cartocdn.com/rastertiles/voyager_nolabels/{z}/{x}/{y}{r}.png")
# addTiles (urlTemplate = "https://{s}.basemaps.cartocdn.com/rastertiles/voyager/{z}/{x}/{y}{r}.png")

a <- colorRampPalette(c("white","blue"))
colors <- a(10)

SalleMapIcon <- icons(iconUrl = "./www/Presentación/Iconos/LaSalleBajio.png",
                      iconWidth = 40, iconHeight = 40,
                      iconAnchorX = 0, iconAnchorY = 0)


coords <- municipios %>% select(NOM_MUN) %>% mutate(lng = 0, lat = 0)
j <- 1
for (i in 1:nrow(coords)){
  coords$lng[i] <- suppressWarnings(st_centroid(coords[i,])$geometry[[1]][1])
  coords$lat[i] <- suppressWarnings(st_centroid(coords[i,])$geometry[[1]][2])
  j <- j + 1 
}

estcoords <- otros_mun %>% select(NOM_ENT) %>% mutate(lng = 0, lat = 0)
j <- 1
for (i in 1:nrow(estcoords)){
  estcoords$lng[i] <- suppressWarnings(st_centroid(estcoords[i,])$geometry[[1]][1])
  estcoords$lat[i] <- suppressWarnings(st_centroid(estcoords[i,])$geometry[[1]][2])
  j <- j + 1 
}


# Funciones -----



Map_Uni <- function(){
  NI <- ANUIES %>% 
    select(NOMBRE.INSTITUCIÓN.ANUIES,NOMBREMUN) %>% group_by(NOMBREMUN) %>%
    summarise(main = length(unique(NOMBRE.INSTITUCIÓN.ANUIES))) %>% ungroup()
  
  data <- left_join(municipios,NI, by = c("NOM_MUN" = "NOMBREMUN")) %>%   
    st_transform("+init=epsg:4326") 
  
  labels <-  sprintf("<strong>Mun:</strong> %s<br/><strong>Universidades:</strong> %s", 
                     data$NOM_MUN, data$main) %>% lapply(HTML)
  
  a <- colorRampPalette(c("white","blue"))
  colors <- a(10)
  pal <- colorNumeric(colors, domain = data$main,na.color = "black")
  
  map <- base_map %>%
    addPolygons(data = data, fillColor = ~pal(main),
                weight = 1,color = "black", opacity = 1, label = labels,
                layerId = ~NOM_MUN) %>% 
    addLabelOnlyMarkers(data = data, lng = coords$lng,lat = coords$lat,
                        label = ~main, labelOptions =
                          labelOptions(noHide = T, direction = 'center', textOnly = T,
                                       style = list("font-size" = "14px",
                                                    "color" = "darkblue"))) %>% 
    addLegend(pal = pal, values = data$main, opacity = 0.7, 
              title = "Univ",position = "bottomright")
  return(map)
}
Map_Carreras <- function(){
  NI <- ANUIES %>% 
    select(NOMBRE.CARRERA.SEP,NOMBREMUN) %>% group_by(NOMBREMUN) %>%
    summarise(main = length(unique(NOMBRE.CARRERA.SEP))) %>% ungroup()
  
  data <- left_join(municipios,NI, by = c("NOM_MUN" = "NOMBREMUN")) %>% 
    st_transform("+init=epsg:4326") 
  
  labels <- sprintf("<strong>Mun:</strong> %s<br/><strong>Carreras:</strong> %s", 
                    data$NOM_MUN, data$main) %>% lapply(HTML)
  
  pal <- colorNumeric(colors, domain = data$main,na.color = "black")
  
  map <- base_map %>% 
    addPolygons(data = data, fillColor = ~pal(main),
                weight = 1,color = "black", opacity = 1, label = labels,
                layerId = ~NOM_MUN) %>% 
    addLabelOnlyMarkers(data = data, lng = coords$lng,lat = coords$lat,
                        label = ~main, labelOptions =
                          labelOptions(noHide = T, direction = 'center', textOnly = T,
                                       style = list("font-size" = "14px",
                                                    "color" = "darkblue"))) %>% 
    addLegend(pal = pal, values = data$main, opacity = 0.7, 
              title = "Carreras",position = "bottomright")
  
  return(map)
}
Map_Areas <- function(){
  NI <- ANUIES %>% 
    select(CAMPO.AMPLIO,NOMBREMUN) %>% 
    group_by(NOMBREMUN) %>% summarise(main = length(unique(CAMPO.AMPLIO))) %>% 
    ungroup()
  data <- left_join(municipios,NI, by = c("NOM_MUN" = "NOMBREMUN")) %>% 
    st_transform("+init=epsg:4326")
  
  labels <- sprintf("<strong>Mun:</strong> %s<br/><strong>Areas:</strong> %s",
                    data$NOM_MUN, data$main) %>% lapply(HTML)
  pal <- colorNumeric(colors, domain = data$main,na.color = "black")
  
  map <- base_map %>% 
    addPolygons(data = data, fillColor = ~pal(main),
                weight = 1,color = "black", opacity = 1, label = labels,
                layerId = ~NOM_MUN) %>% 
    addLabelOnlyMarkers(data = data, lng = coords$lng,lat = coords$lat,
                        label = ~main, labelOptions =
                          labelOptions(noHide = T, direction = 'center', textOnly = T,
                                       style = list("font-size" = "14px",
                                                    "color" = "darkblue"))) %>% 
    addLegend(pal = pal, values = data$main, opacity = 0.7, 
              title = "Áreas",position = "bottomright")
  
  return(map)
}

sun <- function(M){
  
  areas <- ANUIES %>% filter(NOMBREMUN == M) %>% select(CAMPO.AMPLIO,Carrera,NI) %>% 
    group_by(CAMPO.AMPLIO) %>% summarise(value = sum(NI))
  top <- as.data.frame(cbind("label" = areas$CAMPO.AMPLIO, "parents" = "",
                             "value" = areas$value))
  if(nrow(areas) == 0){
    return()
  }
  else{
    bottom <- ANUIES %>% filter(NOMBREMUN == M) %>% select(CAMPO.AMPLIO,Carrera,NI) %>% 
      group_by(CAMPO.AMPLIO,Carrera) %>% summarise(value = sum(NI)) %>% 
      rename("parents" = "CAMPO.AMPLIO","label" = "Carrera")
    datos <- as.data.frame(rbind(top,bottom))
    
    plot_ly(data = datos, type = "sunburst", labels = ~label,
            parents = ~parents,values = ~value, branchvalues = 'total',
            insidetextorientation='radial') %>% 
      layout(title = list(text = M, font = list(color = "white")),plot_bgcolor= rgb(33,44,85,alpha = 200, max = 255),
             paper_bgcolor = rgb(33,44,85, alpha = 200, max = 255)) %>% 
      config(displayModeBar = F,displaylogo = F)
  }
}


SALLE <- function(carrera){
  
  busqueda <- switch (carrera,
                      "ADMINISTRACIÓN DE NEGOCIOS" = "ADMINISTRACIÓN (DE NEGOCIOS)$|(DE EMPRESAS)$",
                      "ADMINISTRACIÓN DE NEGOCIOS EN ENTORNOS VIRTUALES" = "ADMINISTRACIÓN .* (VIRTUALES)$|(ELECTRÓNICOS)$",
                      "ADMINISTRACIÓN TURÍSTICA" = "^(?!(TERAPIA))(.*NEGOCIOS)?(.*TUR[IÍ].*)",
                      "NEGOCIOS TURÍSTICOS" = "^(?!(TERAPIA))(.*NEGOCIOS)?(.*TUR[IÍ].*)",
                      "DESARROLLO DEL CAPITAL HUMANO" = "(CAPITAL)|(HUMANO(S)?)",
                      "CONTADURÍA PÚBLICA" = "CONTA",
                      "MERCADOTECNIA ESTRATÉGICA" = "MERCA",
                      "NEGOCIOS INTERNACIONALES" = "INTERN",
                      "AGRÓNOMO EN PRODUCCIÓN" = "AGR",
                      "MEDICINA VETERINARIA Y ZOOTECNIA" = "(VETE)|(ZOO)",
                      "DISEÑO AMBIENTAL Y DE ESPACIOS" = "(DISEÑO)? (ESPACIO)",
                      "DISEÑO DE MODAS Y CALZADO" = "(DISEÑO)? (MOD)|(CALZ)",
                      "DISEÑO GRAFICO ESTRATEGICO" = "((GR[AÁ]FICO)|(ARTE[^(RO)])) (?!C)(?!(ESC.*)).*",
                      "LENGUAS MODERNAS E INTERCULTURALIDAD" = "(LEN)",
                      "ODONTOLOGÍA" = "(ODO)|(DENT)",
                      "ENFERMERÍA" = "(ENF)",
                      "ACTUARIA" = "(ACTUA)",
                      "CIENCIAS DE LA COMUNICACIÓN" = "^(?!(TECNOLOGÍAS)|(DISEÑO))(.*(?<!TELE)COMUN)",
                      "PSICOLOGÍA" = "(PSI)",
                      "CRIMINOLOGÍA Y CRIMINALÍSTICA" = "(CRIM)",
                      "DERECHO" =  "(DERE)",
                      "EDUCACIÓN" = "(EDU)",
                      "ARQUITECTURA" = "(ARQ)",
                      "CIVIL" = "(CIV)",
                      "ELECTROMECÁNICA" = "(ELECTRO)",
                      "INDUSTRIAL" = "^(INDUSTRIAL)",
                      "DISEÑO INDUSTRIAL" = "^.*(DIS.*) (IND)",
                      "BIOMÉDICA" = "^.*(BIO)[(MÉD.*)|(TEC.*)|(NA.*)]",
                      "GOBERNANZA Y POLÍTICAS PÚBLICAS" = "(GOB.*)|(POLÍ.*)",
                      "GESTIÓN Y OPERACIÓN DE SERVICIOS GASTRONÓMICOS" = "(GAST.*)",
                      "TECNOLOGÍAS Y SOLUCIONES DE NEGOCIO" = "(TECNO.*).*(NEG.*)",
                      "ELECTRÓNICA Y TELECOMUNICACIONES" = "(?=(TELE.*)|(TECNO.*))(.*COMU.*)",
                      "DE SOFTWARE Y SISTEMAS COMPUTACIONALES" = "(SOFTW.*)(COMPU.*)?",
                      "TURISMO DE NEGOCIOS Y REUNIONES" = "^(?!(TERAPIA))(.*NEGOCIOS)?(.*TUR[IÍ].*)",
                      "AUTOMATIZACIÓN Y CONTROL INDUSTRIAL" = "(?=(AUTOMA.*)|(CONTROL))(.*INDU.*)?",
                      "ENTRENAMIENTO DEPORTIVO" = "(DEP.*)")
  
  info <- ANUIES[grep(busqueda,ANUIES$Carrera,perl = T),] %>% 
    select(NOMBRE.INSTITUCIÓN.ANUIES,Titulo,Carrera,NI,NOMBREMUN) %>% 
    rename("UNIVERSIDAD" = "NOMBRE.INSTITUCIÓN.ANUIES", "ni" = "NI") %>% 
    group_by(UNIVERSIDAD,NOMBREMUN) %>% summarise(Carrera = Carrera, ni = sum(ni)) %>%
    ungroup() %>% distinct()
  
  return(info)
}
plot_map_salle <- function(programa){
  
  UNI <- SALLE(programa)
  dat <- inner_join(TOP25,UNI,by = c("UNIVERSIDAD","NOMBREMUN"))
  
  labels <- paste0("<p style = 'text-align:center;'><strong>",dat$UNIVERSIDAD,"</strong></p>",
                   "<p><strong>CARRERA: </strong>",dat$Carrera,"</p>",
                   "<p style = 'text-align:center;'><strong>NI: </strong>",dat$ni,"</p>")
  
  
  a <- colorRampPalette(c("red","yellow","green"))
  colors <- a(6)
  pal <- colorNumeric(colors, domain = dat$ni,na.color = "black")
  
  map <-  leaflet(options = leafletOptions(minZoom = 8, attributionControl = F)) %>% 
    addTiles (urlTemplate = "https://{s}.basemaps.cartocdn.com/rastertiles/voyager/{z}/{x}/{y}{r}.png" ) %>% 
    addPolygons(data = municipios, weight = 1,color = "black",fillOpacity = 0) %>% 
    addCircleMarkers(data = dat, lng = ~LONG,lat = ~LAT, fillColor =  ~pal(ni),
                     opacity = 0, fillOpacity = 0.4,label = lapply(labels,HTML)) %>% 
    addMarkers(lng = -101.7212968, lat = 21.15665423, icon = SalleMapIcon) %>% 
    addMarkers(lng = -101.2281982, lat = 20.55036291, icon = SalleMapIcon) %>% 
    addLabelOnlyMarkers(data = dat, lng = ~LONG,lat = ~LAT,
                        label = ~ni, labelOptions =
                          labelOptions(noHide = T, direction = 'center', textOnly = T)) %>%
    addLegend("bottomright",pal = pal, values = dat$ni,title = "Nuevo Ingreso")
  
  return(map)
}

Map_carrera <- function(c){
  datos <- ANUIES %>% select(Carrera,NOMBREMUN,NI) %>% 
    filter(Carrera == c) %>% 
    group_by(NOMBREMUN) %>% summarise(Veces = n(), Ingreso = sum(NI))
  
  map_data <- left_join(municipios,datos, by = c("NOM_MUN" = "NOMBREMUN"))
  
  
  pal <- colorNumeric(colors, domain = map_data$Ingreso,na.color = "black")
  
  map <- base_map %>% 
    addPolygons(data = map_data, fillColor = ~pal(Ingreso), weight = 1, color = "black",
                popup = ~sprintf(
                  "<strong>Frec. del Prog.</strong> %d<p><strong>Ingreso:</strong> %d</p>",Veces,Ingreso)) %>%
    addLabelOnlyMarkers(data = map_data, lng = coords$lng,lat = coords$lat,
                        label = ~Ingreso, labelOptions =
                          labelOptions(noHide = T, direction = 'center', textOnly = T,
                                       style = list("font-size" = "14px",
                                                    "color" = "darkblue"))) %>% 
    addLegend("bottomright",pal = pal, values = map_data$Ingreso,title = "<p>Nuevo</p><p>Ingreso</p>") 
  
  return(map)
}
wordcarrera <- function(){
  
  dat <- ANUIES %>% select(Carrera) %>% 
    group_by(Carrera) %>% summarise(Veces = n()) %>% arrange(-Veces) %>% head(100)
  
  hchart(dat,type = "wordcloud", hcaes(x = Carrera, weight = Veces),
         tooltip = list(pointFormat = '{x} {point.Veces}')) %>% 
    hc_chart(backgroundColor = rgb(33,44,85,alpha = 200, max = 255))
}


CarrerasNI <- function(agrupador,tipo){
  
  if(agrupador == "Carrera"){
    if(tipo == "Mejores"){
      datos <-
        ANUIES %>% select(Carrera,NI) %>% 
        group_by(Carrera) %>% summarise(Ingreso = sum(NI)) %>%
        arrange(-Ingreso) %>% head(8) %>% arrange(Ingreso)  
    }
    else{
      datos <-
        ANUIES %>% select(Carrera,NI) %>% filter(NI != 0) %>% 
        group_by(Carrera) %>% summarise(Ingreso = sum(NI)) %>%
        arrange(-Ingreso) %>% tail(8) %>% arrange(Ingreso) 
    }
  }
  else{
    if(tipo == "Mejores"){
      datos <-
        ANUIES %>% select(NI,CAMPO.DETALLADO) %>% 
        group_by(CAMPO.DETALLADO) %>% summarise(Ingreso = sum(NI)) %>%
        arrange(-Ingreso) %>% head(8) %>% arrange(Ingreso) %>% 
        rename("Carrera" = "CAMPO.DETALLADO")
    }
    else{
      datos <-
        ANUIES %>% select(NI,CAMPO.DETALLADO) %>% filter(NI != 0) %>% 
        group_by(CAMPO.DETALLADO) %>% summarise(Ingreso = sum(NI)) %>%
        arrange(-Ingreso) %>% tail(8) %>% arrange(Ingreso) %>% 
        rename("Carrera" = "CAMPO.DETALLADO")
    }
  }
  
  
  plot_ly(datos, type = "bar", y = ~Carrera, x = ~Ingreso, color = I("#CE122D"),
          hovertemplate = '%{y}<extra></extra>', orientation = 'h',text = ~Ingreso,
          texttemplate = '%{text: .0f}',
          textposition = 'auto', textfont = list(color = 'white', size = 12)) %>% 
    layout(
      plot_bgcolor = rgb(33,44,85,alpha = 200, max = 255),
      paper_bgcolor = rgb(0,0,0,0),
      yaxis = list(categoryarray = ~Carrera,categoryorder = "array", title = "", color = "white",showticklabels = F),
      xaxis = list(color = "white",showgrid = T, showline =  F, title = "", showticklabels = F)) %>% 
    config(displayModeBar = F,displaylogo = F)
}


ProgramasNGTO <- function(){
  
  data <- OANU %>% select(CVE_ENT,Carrera) %>% distinct() %>% 
    group_by(CVE_ENT) %>% summarise(Programas = n())
  
  map_data <- inner_join(otros_mun,data, by = c("CVE_ENT" = "CVE_ENT"))
  
  pal <- colorNumeric(colors, domain = map_data$Programas,na.color = "black")
  
  map <- 
    leaflet(options = leafletOptions(minZoom = 6, maxZoom = 6,zoomControl = F, attributionControl = F,dragging = F)) %>% 
    addTiles (urlTemplate = "https://{s}.basemaps.cartocdn.com/rastertiles/voyager/{z}/{x}/{y}{r}.png" ) %>% 
    addPolygons(data = map_data, fillColor = ~pal(Programas), weight = 1, color = "black",
                label = ~sprintf("Programas: %d", Programas), layerId = ~CVE_ENT) %>%
    addLabelOnlyMarkers(data = map_data, lng = estcoords$lng,lat = estcoords$lat,
                        label = ~Programas, labelOptions =
                          labelOptions(noHide = T, direction = 'center', textOnly = T,
                                       style = list("font-size" = "14px",
                                                    "color" = "#CE122D"))) %>% 
    addLegend("bottomright",pal = pal, values = map_data$Programas, title = "Carreras")
  return(map)
}
UnisNGTO <- function(){
  
  data <- OANU %>% select(CVE_ENT,NOMBRE.INSTITUCIÓN.ANUIES) %>% distinct() %>% 
    group_by(CVE_ENT) %>% summarise(Universidades = n())
  
  map_data <- inner_join(otros_mun,data, by = c("CVE_ENT" = "CVE_ENT"))
  
  pal <- colorNumeric(colors, domain = map_data$Universidades,na.color = "black")
  
  map <- 
    leaflet(options = leafletOptions(minZoom = 6, maxZoom = 6,zoomControl = F, attributionControl = F,dragging = F)) %>% 
    addTiles (urlTemplate = "https://{s}.basemaps.cartocdn.com/rastertiles/voyager/{z}/{x}/{y}{r}.png" ) %>% 
    addPolygons(data = map_data, fillColor = ~pal(Universidades), weight = 1, color = "black",
                label = ~paste("Universidades:", Universidades),layerId = ~CVE_ENT) %>%
    addLabelOnlyMarkers(data = map_data, lng = estcoords$lng,lat = estcoords$lat,
                        label = ~Universidades, labelOptions =
                          labelOptions(noHide = T, direction = 'center', textOnly = T,
                                       style = list("font-size" = "14px",
                                                    "color" = "#CE122D"))) %>% 
    addLegend("bottomright",pal = pal, values = map_data$Universidades, title = "Universidades")
  return(map)
}
AreasNGTO <- function(){
  data <- OANU %>% select(CVE_ENT,CAMPO.AMPLIO) %>% distinct() %>% 
    group_by(CVE_ENT) %>% summarise(Áreas = n())
  
  map_data <- inner_join(otros_mun,data, by = c("CVE_ENT" = "CVE_ENT"))
  
  pal <- colorNumeric(colors, domain = map_data$Áreas,na.color = "black")
  
  map <- 
    leaflet(options = leafletOptions(minZoom = 6, maxZoom = 6,zoomControl = F, attributionControl = F,dragging = F)) %>% 
    addTiles (urlTemplate = "https://{s}.basemaps.cartocdn.com/rastertiles/voyager/{z}/{x}/{y}{r}.png" ) %>% 
    addPolygons(data = map_data, fillColor = ~pal(Áreas), weight = 1, color = "black",
                label = ~paste("Universidades:", Áreas),layerId = ~CVE_ENT) %>%
    addLabelOnlyMarkers(data = map_data, lng = estcoords$lng,lat = estcoords$lat,
                        label = ~Áreas, labelOptions =
                          labelOptions(noHide = T, direction = 'center', textOnly = T,
                                       style = list("font-size" = "14px",
                                                    "color" = "#CE122D")))
  return(map)
}

OtrosNI <- function(E){
  
  datos <-
    OANU %>% filter(CVE_ENT == E) %>% 
    select(NOMBRE.INSTITUCIÓN.ANUIES,NI) %>% 
    group_by(NOMBRE.INSTITUCIÓN.ANUIES) %>% summarise(Ingreso = sum(NI)) %>% 
    arrange(-Ingreso) %>% head(5) %>% arrange(Ingreso) 
  
  plot_ly(datos, type = "bar", y = ~NOMBRE.INSTITUCIÓN.ANUIES, x = ~Ingreso, color = I("#CE122D"),
          hovertemplate = '%{y}<extra></extra>', orientation = 'h',text = ~Ingreso,
          texttemplate = '%{text: .0f}',
          textposition = 'auto', textfont = list(color = 'white', size = 12)) %>% 
    layout(
      plot_bgcolor = rgb(33,44,85,alpha = 200, max = 255),
      paper_bgcolor = rgb(0,0,0,0),
      title = list(text = "Universidades con mayor ingreso", font = list(color = "white")),
      yaxis = list(categoryarray = ~NOMBRE.INSTITUCIÓN.ANUIES,
                   categoryorder = "array",title = NA,showticklabels = F),
      xaxis = list(color = "white",showgrid = T, showline =  F, title = NA, showticklabels = F)) %>% 
    config(displayModeBar = F,displaylogo = F)
}
# a <- ANUIES %>% group_by(NOMBRE.INSTITUCIÓN.ANUIES,NOMBREMUN) %>% summarise(NI =sum(NI)) %>% arrange(-NI)
