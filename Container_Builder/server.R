#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Author: Shine Yao
# Date :Nov 2018
#

library(shiny)
library(openxlsx)
library(digest)

Item_DF <- read.xlsx("C:/Users/syao/OneDrive - ATC Tires Pvt. Ltd/R Data/ATA_R_DB/Items_From_Nav.xlsx")

#save the response 
fields_basic <- c("date", "Rep", "Cus", "AC")

fields_Brand <- c("Date", "Sales", "Customer_Name", "Customer_AC", "Brand", "Current_Level", "New_Level", "Start_Date", "End_Date")

fields_Cat <- c("Date", "Sales", "Customer_Name", "Customer_AC", "Category", "Sub_Category", "Channel", "Start_Date", "End_Date")

fields_Item <- c("Date", "Sales", "Customer_Name", "Customer_AC", "Item_Code", "Current_Price", "New_Price", "Start_Date", "End_Date")

humanTime <- function() format(Sys.time(), "%Y%m%d-%H%M%OS")

outdir=getwd()

outfilename="table"

#create a responses directory so that the saved responses can go there
responsesDir <- file.path("responses")
epochTime <- function() {
  as.integer(Sys.time())
}


#creating first table for brand discount
df1 <- data.frame(Update = rep(FALSE, 14), Brands=c("Alliance WHSE","Alliance Direct", "Aeolus WHSE", "Aeolus Direct", "Aeolus TR WHSE", "Aeolus TR Direct", "Constellation WHSE","Constellation Direct
", "Constellation TR WHSE", "Constellation TR Direct", "Galaxy WHSE", "Galaxy Direct", "Primex WHSE", "Primex Direct"),
                  Current = factor(14, c("Dealer", "Major","Distributor","Super","Select")), 
                  New = factor(14, c("Dealer", "Major","Distributor","Super","Select")),
                  Start_Date = seq(from = Sys.Date(), to = Sys.Date(), length.out = 14),
                  End_Date = seq(from = Sys.Date(), to = Sys.Date(), length.out = 14),
                  stringsAsFactors = FALSE)

#2nd table for category discount
df2 <- data.frame(Category = factor(6,c("TR", "LS", "IND", "OTR", "SS", "CU")), Sub_Category = factor(6,c("TR", "LS", "IND", "OTR", "SS", "CU")), Channel = factor(6,c("WHSE", "DIRECT", "BOTH")), Discount = c(0,0,0,0,0,0))

#third table for net price
df3 <- data.frame(Item_Code = factor(6, levels = Item_DF$No.), Current_Price=c(0), New_price = c(0), Start_Date = seq(from = Sys.Date(), to = Sys.Date(), length.out = 10),
                  End_Date = seq(from = Sys.Date(), to = Sys.Date(), length.out = 10),
                  stringsAsFactors = FALSE)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  values <- reactiveValues()
  
  ## Handsontable
  observe({
    if (!is.null(input$table1)) {
      values[["previous_one"]] <- isolate(values[["df1"]])
      df1 = hot_to_r(input$table1)
    } else {
      if (is.null(values[["df1"]]))
        df1 <- df1
      else
        df1 <- values[["df1"]]
    }
    values[["df1"]] <- df1
  })
  
  observeEvent(input$submit, {
    finaldf1 <- isolate(values[["df1"]])
    write.csv(x = finaldf1, file = file.path(outdir, outfilename,digest::digest(data)),
              row.names = FALSE)
    #dput(finaldf1, file=file.path(outdir, sprintf("%s.txt", outfilename)))
  })
  
  
  
  #load the table
  
  output$table1 <- renderRHandsontable({
    
    rhandsontable(df1)  
    
  })
  
  output$table2 <-renderRHandsontable({
    
    rhandsontable(df2) %>%
      hot_col("Discount", format = "0.00%") %>%
      hot_cols(colWidths = 100)
  })
  
  output$table3 <-renderRHandsontable({
    rhandsontable(df3)%>%
      hot_cols(colWidths = 100)
  })
  
  saveData <- function(data) {
    fileName <- sprintf("%s_%s.csv", humanTime())
    
    write.csv(x = data, file = file.path(responsesDir, fileName,digest::digest(data)),
              row.names = FALSE)
  }
  

    
 
})
