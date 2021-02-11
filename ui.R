#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Author: Shine Yao
# Date :April 2019
# 
#

library(shiny)
library(shinythemes)
library(rhandsontable)
library(openxlsx)
library(shinyjs)
library(shinydashboard)
library(dplyr)
library(V8)
library(shinyalert)

CustomerDB <- read.xlsx("Data/CustomerDB.xlsx")
SalesDB <- read.xlsx("Data/Salespeople.xlsx")


dirColors <-c("1"="#595490", "2"="#527525", "3"="#A93F35", "4"="#BA48AA")
#pre-processing customer DB
CustomerDB <- CustomerDB %>% filter(Branch.Code == "AM")
SalesDB <- SalesDB %>% filter(Branch.Code == "AM")

#add log
#title <- tags$a(href = "https://cdn.atgtire.com/wp-content/themes/avani/atg/images", tags$img(src = "atg-white.jpg"))

#create mandatory fields
fieldsMandatory <- c("date", "Rep", "Cus", "AC")

#print function
jsCode <- 'shinyjs.winprint = function(){
window.print();
}'

# Define UI for application that draws a histogram
shinyUI(
  
  fluidPage(
    
    shinyjs::useShinyjs(),
    
    extendShinyjs(text = jsCode),
    
    theme = shinytheme("united"),
    
    tags$head(HTML("<title>ATG Container Builder</title>")),
    # Application title
    titlePanel(title = div(img(src = "logo.jpg", height="120", width="200", style="margin-right: 20px"), "Container Builder")),
    
    br(),
    
    tabsetPanel(
    
      tabPanel(tags$h4("Builder"),
      br(),
      br(),
    
    div(
      id = 'form',
      
      # Sidebar with a slider input for number of bins 
      sidebarLayout(
        
        sidebarPanel(
          dateInput('date',
                    label = "Date of Order:",
                    value = Sys.Date()),
          
          br(),
          
          selectInput("Rep", 
                      label = "Requested By:",
                      SalesDB$Name),
          
          br(),
          
          selectInput("Cus", 
                      label = "Customer Name:",
                      CustomerDB$Name ),
          
          br(),
          
          #selectInput("AC", 
                      #label = "Account No.:",
                      #CustomerDB$No.),
          tags$strong("Account No.:"),
          
          verbatimTextOutput("value"),
          
          
          br(),
          
          
          div(style="display:inline-block;width:32%;text-align: center;margin-right: 30px",downloadButton("save", label = "Download")),
          
          #div(style = "margin-bottom: 0.5px;" ),
          
          div(style="display:inline-block;width:32%;text-align: center;margin-left: 5px",actionButton("submit_another", label = "Create New", icon = icon("file-alt"))),
          
          br(),
          br(),
          
          p("Time: ", span(date())),
          
          
          br(),
          br(),
          
          # Create a reference table
          tags$strong("Factory Index Reference", style = "font-weight: 800"),
          
          br(),
          br(),
        
          tags$table(class = "table",
                     tags$thead(tags$tr(
                       tags$th("Facory ID"),
                       tags$th("Factory Index"),
                       tags$th("Factory")
                     )),
                     
                     
                     tags$tbody(
                       tags$tr(
                         tags$td("ATCIN01"),
                         tags$td("IN/33"),
                         tags$td("Inida TNV")
                       ),
                     
                      
                       tags$tr(
                         tags$td("ATCIN03"),
                         tags$td("IG/36"),
                         tags$td("Inida DHJ")
                       ),
                     
                       tags$tr(
                         tags$td("ATCIS01"),
                         tags$td("AL/50"),
                         tags$td("Israel")
                       )
                     ) 
                       
        ),
          
          
          
          width = 3
          
          
        ),  
        
        
        mainPanel(
                          
          
                               rHandsontableOutput("hotable"),
                               br(),
                               br(),
                               helpText(HTML('<h5 style="color:black; font-size: 14pt">Container Summary</h5>')),
                               tableOutput("summary"),
                               tags$style(type="text/css", "#hotable table, th,td {height: 30px;font-family: verdana;font-size: 13px;}")
                               
                              
                      
          ,width = 9)
      )
    )),
    
    #Tab for help instruction
    tabPanel(tags$h4("Help"),
             
            titlePanel("Container Builder User Guide"),
            
            
            fluidRow(
              column(8,
                     wellPanel(HTML(
                       paste(
                         h3("Build Containers\n\n"),'<br/>',
                         p("Step 1. Fill 'Date of  Order', 'Requested By', 'Customer Name' on the left side\n\n"),'<br/>',
                         p("Step 2. Fill 'Parent.Code', 'Quantity' in the right side table \n\n"),'<br/>',
                         p("Step 3. If you need to switch factory code, use the dropdown list from 'Switch Factor' column, and the 'Item.Code' will also be updated accordingly\n\n"),'<br/>',
                         p("Step 4. Container percentage will be automatically calculated in single container column \n\n"),'<br/>',
                         p("Step 5. Overall container quantity will be automatically calculated in the 'Container Summary'table")
                       )
                     )),
                     wellPanel(HTML(
                       paste(
                         h3("Download Order Form\n\n"),'<br/>',
                         p("If you want to download the container builder form, please click the download button"),'<br/>'
                         
                       )
                     )
                    )

              )
              
              
            )
             
             
             
             
             
             
             
             
             )),
    hr(),
    tags$footer("Copyright © 2019 - ATG. All rights reserved.",align = "right", style = "position:absolutebottom:;
                width:90%;
                padding: 10px;")
    )
    )