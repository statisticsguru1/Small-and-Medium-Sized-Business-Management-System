library(shiny)
library(bslib)
source("FormModules.R")
ui<- page_fluid(
  createFieldUI("create_field")
  #sectionUI("sectionui")
)

server<-function(input, output, session) {
  createFieldServer("create_field", fields)
  #sectionServer("sectionui", fields)
}

shinyApp(ui,server)
