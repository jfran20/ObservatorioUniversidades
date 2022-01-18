# Datos ---- 
# source("ANUIES.R", encoding= "UTF-8")

# Importanción de archivos ----
source("server.R", encoding= "UTF-8")
source("ui.R", encoding= "UTF-8")

# -----

shinyApp(ui = ui, server = server)

