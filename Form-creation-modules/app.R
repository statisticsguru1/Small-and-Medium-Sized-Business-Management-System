library(shiny)
library(bslib)
library(tidyverse)
source("FormModules.R")
ui<- page_fluid(
  createFieldUI("create_field")
  #sectionUI("sectionui")
)

server<-function(input, output, session) {
  
  ## get section details and the existing fieldlists to send to newfield
  
  fields<-list()  #the existing t field list(persistent) 
  
  # get updated newfield 
  new_field<-createFieldServer("create_field","user profile",fields)
  
  # update the newfield on the section
  observe({
    field <- new_field()
    if (!is.null(field)) {
      
      field[length(field)]<-field # add the newfield list 
      
      # save the persistent list to disk 
      
      
      # in future when user wants to register I will be using do.call on each stored field details 
      # to reconstruct the fields like this 
      do.call(field$specific_type,field$params)

    }
  })
}

shinyApp(ui,server)

