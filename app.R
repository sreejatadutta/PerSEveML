## Include the ui and server files
source("ui.R")
source("server.R")
shinyApp(ui = ui, server = server)