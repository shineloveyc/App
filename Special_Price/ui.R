#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Author: Shine Yao
# Date :Nov 2018
# 
#    https://shiny.rstudio.com/gallery/tabsets.html
#

library(shiny)
library(shinythemes)
library(rhandsontable)
library(openxlsx)
library(shinyjs)

customerDB <- read.xlsx("C:/Users/syao/OneDrive - ATC Tires Pvt. Ltd/R Data/ATA_R_DB/Customer_Discount_Master.xlsx")


#fieldsMandatory <- c("Rep", "Cus")

# Define UI for application that draws a histogram
shinyUI(
  
  fluidPage(theme = shinytheme("spacelab"),
  
  # Application title
  titlePanel("Price Adjustment Request Submission (PARS)"),
  
  br(),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    
    sidebarPanel(
      dateInput('date',
                label = "Date of Request:",
                value = Sys.Date()),
      
      br(),
      
      selectInput("Rep", 
                label = "Requested By:",
                c("Eric Lasko", "Gary Suding", "Trent Wallin", "Mark Scanlon", "Keith Browne")),
      
      br(),
      
      selectInput("Cus", 
                label = "Customer Name:",
                customerDB$Name, NULL),
      
      br(),
      
      selectInput("AC", 
                label = "Account No.:",
                customerDB$No., NULL),
      
    br(),
    
    actionButton('submit', "Submit", class = "btn-primary")
     
    ),            
    mainPanel(
                  
      tabsetPanel(id = 'tabs',
        tabPanel("Brand Level Discount",
                 #https://shiny.rstudio.com/articles/layout-guide.html
                 #http://getbootstrap.com/2.3.2/scaffolding.html
                 br(),
                 rHandsontableOutput("table1"),
                 br(),
                 textAreaInput("brand_reason", "Reason for Requested Change")
                 #actionButton("save1","Save", class = "btn btn-primary mb1 black bg-darken-3")
      
         ),
        tabPanel("Item Category Level Discount",
                 br(),
                 rHandsontableOutput("table2"),
                 br(),
                 textAreaInput("cat_reason", "Reason for Requested Change")
                 #actionButton("save2","Save", class = "btn btn-primary mb1 black bg-darken-3")
               
        ),
        tabPanel("Item Net Price",
                 br(),
                 rHandsontableOutput("table3"),
                 br(),
                 textAreaInput("item_reason", "Reason for Requested Change")
                 #actionButton("save3","Save", class = "btn btn-primary mb1 black bg-darken-3")
            
                 )
                              
        )
                  
                  
                )
          )
  )
)
