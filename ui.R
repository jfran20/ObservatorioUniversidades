ui <- fluidPage(
  
includeCSS("./www/style.css"),
tags$div(class = "main",
   
   tags$section(class = "graphs",id = "inicio",
                   h1("Universidad De La Salle Bajío")),      
         
   tags$section(class = "graphs",
                h3("Municipios"),
            column(8,leafletOutput("mainm",width = "90%", height = "90vh") %>% 
                      withSpinner(image = "icono", image.width = "200px", image.height = "200px")),
            column(4,
                   selectInput("tipo",label = "Por",choices = c("Universidades","Carreras","Áreas"), width = "50%"),
                   hr(),
                   plotlyOutput("sun"))),
   
   tags$section(class = "graphs",
            h3("Carreras de la Salle"),
            column(8,leafletOutput("salle", width = "90%",height = "90vh") %>% 
                      withSpinner(image = "icono", image.width = "200px", image.height = "200px")),
            column(4,selectInput("carrera","Carreras",choices = sort(CarreraSalle)))),
   
   tags$section(class = "graphs",
                h3("Carreras más comunes"),
                column(8,leafletOutput("topcarrera",width = "90%",height = "90vh") %>% 
                          withSpinner(image = "icono", image.width = "200px", image.height = "200px")),
                column(4,
                       selectInput("top","Carreras:",choices = carreras$Carrera,width = "60%"),
                       hr(),
                       highchartOutput("wordcloud"))),
   
   tags$section(class = "graphs",id = "barchart",
                
                h3("Top por Ingreso"),
                column(4,offset = 8,
                       column(6,
                              tags$div(id = "styletipo",selectInput("tipo2","",choices = c("Mejores","Peores"),width = "100px")),
                       column(6,
                              tags$div(id = "styleagr",
                                       selectInput("agrupador","",choices = c("Carrera","Campo"),width = "90px"))))),
                plotlyOutput("bar",width = "90%", height = "90vh") %>% 
                  withSpinner(image = "icono", image.width = "200px", image.height = "200px")),
   
                
   tags$section(class = "graphs",
                h3("Fuera de Guanajuato"),
                column(8,
                       leafletOutput("nogto",width = "90%", height = "90vh") %>% 
                          withSpinner(image = "icono", image.width = "200px", image.height = "200px")),
                column(4,
                       selectInput("fuera","Por",choices = c("Programas","Universidades","Áreas"),width = "50%"),
                       hr(),
                       plotlyOutput("top5"), style = "text-align:center"))
   
   )
)