setwd('C:\\Users\\Client\\Desktop\\Dashboard project')

library(shiny)
source('./Project/ui.R',local = TRUE)
source('./Project/server.R',local = TRUE)





shinyApp(ui=ui, server= server)
