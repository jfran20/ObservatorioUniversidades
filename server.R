server <- function(input, output){
  
# NI por municipio (Total) ----
  
  output$mainm <- renderLeaflet({
    switch(input$tipo,
           "Universidades" = Map_Uni(),
             "Carreras" = Map_Carreras(),
             "Áreas" = Map_Areas())
  })
  output$sun <- renderPlotly({sun("LEÓN")})
  observeEvent(input$mainm_shape_click,{
    output$sun <- renderPlotly({sun(input$mainm_shape_click$id)})})
  
  
  output$salle <- renderLeaflet({plot_map_salle(input$carrera)}) 
  
  output$topcarrera <- renderLeaflet({Map_carrera(input$top)})
  output$wordcloud <- renderHighchart({wordcarrera()})
  
  output$bar <- renderPlotly({CarrerasNI(input$agrupador,input$tipo2)})
  
  
  output$nogto <- renderLeaflet({
    switch(input$fuera,
           "Programas"= ProgramasNGTO(),
           "Universidades" = UnisNGTO(),
           "Áreas" = AreasNGTO())
    })
  output$top5  <- renderPlotly({OtrosNI(11)})
  observeEvent(input$nogto_shape_click,{
    output$top5 <- renderPlotly({OtrosNI(input$nogto_shape_click$id)})})
  
}
