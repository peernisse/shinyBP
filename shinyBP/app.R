
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(tidyverse)
library(shiny)
library(DT)
library(dplyr)
library(shinythemes)
library(googlesheets)
#source("helpers.R")


## ======================
googleform_embed_link <- "https://docs.google.com/forms/d/e/1FAIpQLSf3buc5st72mVYAQL9OuQZpR0hqdPHPenLcnhAF2dTIhqkj6Q/viewform?embedded=true"
#googleform_data_url <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vQxeAnH_IXgGLeTw3QcRiE_ua79Zs5nhKgjCqQgpSGDBXnqQfAFzfLoLxHiRD6w_88Wp7LFyW57MQFT/pubhtml"

googleform_data_url <- "https://docs.google.com/spreadsheets/d/1vc3shTj6WqyrPTIbwNYOlijL50ih5Vk-5KjTfS_pVPY/edit?usp=sharing"

## ======================

# Define the fields we want to save from the form
fields <- c("Date", "Blood Pressure Systolic", "Blood Pressure Diastolic",'Weight','Exercise Minutes','Drinks')

# Shiny app with 6 fields that the user can submit data for
shinyApp(
  ui = fluidPage(
    theme = shinytheme("flatly"),
    titlePanel("BP Data Entry"),
    sidebarLayout(
      sidebarPanel(
        # h6(a("Click Here to See Code on Github",
        #      href="https://github.com/jennybc/googlesheets/tree/master/inst/shiny-examples/04_embedded-google-form",
        #      target="_blank")),
        htmlOutput("googleForm"),
        width=6
      ),
      mainPanel(
        h2("Data Table"),
        DT::dataTableOutput("googleFormData"),
        actionButton("refresh", "Refresh Sheet"),
        h2("Time Series Daily Average Values (Last 60 days)"),
        plotOutput("plot")
      )
    )
  ),
  
  
  ########SERVER SIDE CODE##########################################S
  
  
  server = function(input, output, session) {
    
    ss <- gs_url(googleform_data_url, lookup = FALSE, visibility = "public")
    
    output$googleForm <- renderUI({
      tags$iframe(id = "googleform",
                  src = googleform_embed_link,
                  width = 640,
                  height = 1178,
                  frameborder = 0,
                  marginheight = 0,
                  marginwidth = 0)
    })
    
    
    output$googleFormData <- DT::renderDataTable({
      input$refresh
      ss_dat <- gs_read(ss) %>%
        mutate(Date = Timestamp %>%
                 as.Date(format = "%m/%d/%Y")) %>%
        select(Timestamp, `Blood Pressure Systolic`,`Blood Pressure Diastolic` ,`Weight`,`Exercise Minutes`,Drinks) %>%
        arrange(Timestamp)

      DT::datatable(ss_dat)
    })#output$googleFormData
    
    pData<-reactive({
      tbl<-gs_read(ss) %>%
        mutate(Date = Timestamp %>%
                 as.Date(format = "%m/%d/%Y")) %>%
        select(Date, `Blood Pressure Systolic`,`Blood Pressure Diastolic` ,`Weight`,`Exercise Minutes`,Drinks) %>%
        gather(PARAMETER,RESULT,2:6) %>% 
        filter(Date >= -60+Sys.Date()) %>% 
        group_by(Date,PARAMETER) %>% 
        summarize(
          RESULT = mean(RESULT)
          
        ) %>% 
        arrange(Date) %>% 
        filter(PARAMETER %in% c('Blood Pressure Systolic','Blood Pressure Diastolic' ,'Weight')) %>% 
        as.data.frame(.)
      
      return(tbl)
      
    })#pData
    
    output$plot<-renderPlot({
      g<-ggplot(pData(),aes(Date,RESULT,color=PARAMETER))+
        geom_point(size=4)+
        geom_line()+
        # geom_col(data=pData(),aes(Date,Weight,fill="Weight"),alpha=.5)+
        # geom_point(data=pData(),aes(Date,`Blood Pressure Systolic`,color="Blood Pressure Systolic"),size = 3)+
        # geom_line(data=pData(),aes(Date,`Blood Pressure Systolic`,color="Blood Pressure Systolic"))+
        # geom_point(data=pData(),aes(Date,`Blood Pressure Diastolic`,color="Blood Pressure Diastolic"),size = 3)+
        # geom_line(data=pData(),aes(Date,`Blood Pressure Diastolic`,color="Blood Pressure Diastolic"))+
        # scale_fill_manual(values=c('black'))+
        theme(legend.position="bottom",
              legend.title=element_blank())+
        labs(x="Date",y="Value")
      return(g)
      
    })
    
    
    
  }#server
)#end of app



