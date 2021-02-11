#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Author: Shine Yao
# Date :April 2019
#

library(shiny)
library(openxlsx)
library(digest)
library(dplyr)
library(stringr)
library(rhandsontable)
library(shinyjs)
library(forcats)
library(shinyalert)

##gs_auth(new_user = TRUE)


#load data from Nav Item Card
SKU_DF <- read.xlsx("Data/Items_From_Nav.xlsx", colNames = TRUE, rowNames = FALSE)
CustomerDB <- read.xlsx("Data/CustomerDB.xlsx")

#filter all SKUs not for AM
Active_SKU_DF <- SKU_DF %>% filter(str_detect(No.,"-") &(!Category.Code %in% c('MISC', "WHEEL", "ASSM", "VALVE STEM", "FLAP", "TR")) & !str_detect(Description,"REACH") & 
                                     !str_detect(No.,c("HK","B", "INTF"))  & Brand.Name %in% c("ALLIANCE", "GALAXY", "PRIMEX") & Vendor.No. %in%c("ATCIN01", "ATCIN03", "ATCIS01")) %>% 
  select(No.,Description, Size, Item.Status, Block.PO.Creation, Blocked, Parent.Item,Container.Quantity, Vendor.No.) %>% 
  filter(Item.Status %in% c("Active", "Discontinued") & Blocked!="Yes" & Block.PO.Creation != "Yes") %>% arrange(Parent.Item)
a
#build indicator for SKU produced in multiple factory
Active_SKU_DF$Produced.Multi.Factory <- Active_SKU_DF$Parent.Item %in% unique(Active_SKU_DF$Parent.Item[duplicated(Active_SKU_DF$Parent.Item)])

humanTime <- function() format(Sys.time(), "%Y%m%d-%H%M%OS")

#create a responses directory so that the saved responses can go there
epochTime <- function() {
  as.integer(Sys.time())
}

fieldsMandatory <- c("date", "Rep", "Cus", "AC")

builder_DF <- data.frame(Parent.Code = factor(NA, levels = unique(Active_SKU_DF$Parent.Item)), Item.Code = factor(NA, levels = Active_SKU_DF$No.), 
                         Description = character(15), Size = character(15), Quantity = numeric(15), DHJ.Container = numeric(15), 
                         TNV.Container = numeric(15),IS.Container = numeric(15), Switch.Factory = factor(NA, levels = c('ATCIN03', 'ATCIN01', 'ATCIS01')), stringsAsFactors = FALSE)

sum_table <- data.frame(DHJ.Container = numeric(1), TNV.Container = numeric(1), IS.Container = numeric(1)) 

#start the server
shinyServer(function(input, output, session) {
  
  # Initiate your table
  previous <- reactive({builder_DF})
  
  vals <- reactiveValues(reset=TRUE)
  
  MyChanges <- reactive({
    
    
    if(is.null(input$hotable)){return(previous())}
    
    else if(!identical(previous(),input$hotable)){
      # hot.to.df function will convert your updated table into the dataframe
      builder_DF <- as.data.frame(hot_to_r(input$hotable))
      
      temp_DF <- subset(builder_DF,!is.na(builder_DF$Parent.Code))
      
      #builder_DF$Item.Code[!is.na(builder_DF$Parent.Code)] <- ifelse(!is.na(builder_DF$Switch.Factory) & !is.na(builder_DF$Parent.Code), as.character(mapply(Move_Item_Fun, temp_DF$Parent.Code, temp_DF$Switch.Factory)),
                                     #as.character(sapply(temp_DF$Parent.Code, Prefer_Code_Fun)))
      
      builder_DF$Item.Code[!is.na(builder_DF$Parent.Code)] <- ifelse(is.na(temp_DF$Switch.Factory), as.character(sapply(temp_DF$Parent.Code, Prefer_Code_Fun)), as.character(mapply(Move_Item_Fun, temp_DF$Parent.Code, temp_DF$Switch.Factory)))
      
      builder_DF$Produced.Multi.Factory <- Active_SKU_DF$Produced.Multi.Factory[match(builder_DF$Parent.Code,Active_SKU_DF$Parent.Item)]
      
      builder_DF$Description <- Active_SKU_DF$Description[match(builder_DF$Item.Code,Active_SKU_DF$No.)]
      
      builder_DF$Size <- Active_SKU_DF$Size[match(builder_DF$Item.Code,Active_SKU_DF$No.)]
      
      builder_DF$Vendor.No.<- Active_SKU_DF$Vendor.No.[match(builder_DF$Item.Code,Active_SKU_DF$No.)]
      
      builder_DF$Container.Quantity <- Active_SKU_DF$Container.Quantity[match(builder_DF$Item.Code,Active_SKU_DF$No.)]
      
      builder_DF$Switch.Factory[!is.na(builder_DF$Switch.Factory)] <- builder_DF$Vendor.No.[!is.na(builder_DF$Switch.Factory)]
      
      builder_DF$DHJ.Container <- ifelse(builder_DF$Vendor.No.=="ATCIN03",builder_DF$Quantity/builder_DF$Container.Quantity, 0)
      
      builder_DF$TNV.Container <- ifelse(builder_DF$Vendor.No.=="ATCIN01",builder_DF$Quantity/builder_DF$Container.Quantity, 0)
      
      builder_DF$IS.Container <- ifelse(builder_DF$Vendor.No.=="ATCIS01",builder_DF$Quantity/builder_DF$Container.Quantity, 0)
      
      
    }  
    return(builder_DF[,c(1:10)]) #return container table
    
  })
  
  #output the table of container builder
  output$hotable <- renderRHandsontable({
    input$submit_another
    if(isolate(vals$reset) | is.null(input$hotable)) {
      isolate(vals$reset <- FALSE)
      builder_DF <- data.frame(Parent.Code = factor(NA, levels = unique(Active_SKU_DF$Parent.Item)), Item.Code = factor(NA, levels = Active_SKU_DF$No.), 
                               Description = character(15), Size = character(15), Quantity = numeric(15), DHJ.Container = numeric(15), 
                               TNV.Container = numeric(15),IS.Container = numeric(15), 
                               Switch.Factory = factor(NA, levels = c('ATCIN03', 'ATCIN01', 'ATCIS01')), stringsAsFactors = FALSE)
    } else{
      
      builder_DF <- MyChanges()
    } 
      
     rhandsontable(builder_DF) %>%
       hot_context_menu(allowColEdit = FALSE)
    
  })
  
  #create the container summary table
  output$summary <- renderTable({
    
     if(is.null(input$hotable)){return(sum_table)}
     
     else{
     
     mid_table <- as.data.frame(hot_to_r(input$hotable))
    
     #load summary information
     sum_table$DHJ.Container[1] <- sum(mid_table$DHJ.Container, na.rm = TRUE)
     
     sum_table$TNV.Container[1] <- sum(mid_table$TNV.Container, na.rm = TRUE)
     
     sum_table$IS.Container[1] <- sum(mid_table$IS.Container, na.rm = TRUE)
    
     sum_table
     }
     
   })
  
  
  # Downloadable csv of selected dataset ----
  output$save <- downloadHandler(
    filename = function(){paste(input$Cus, format(Sys.time(), "%Y-%m-%d"), 'Order.xlsx', sep = "_")},
    content = function(fname){
      download_DF <- as.data.frame(MyChanges())
      download_DF <- download_DF[-c(9)] %>% filter(Quantity!=0)
      write.xlsx(download_DF,fname)
      
    }
    
  )  
  
  #let user sbmit another PARS
  observeEvent(input$submit_another, {
    
    #action to take when submit button is pressed
    
    shinyjs::reset("form")
    
    vals$reset <- TRUE
  
  })
  
  
  Prefer_Code_Fun <- function(Parent.Code){
    
    Temp_DF <- Active_SKU_DF %>% filter(Active_SKU_DF$Parent.Item == Parent.Code)
  
    if(any(Temp_DF$Produced.Multi.Factory) == TRUE){
      if(any(sapply(Temp_DF$Vendor.No., str_detect, pattern ="ATCIN03"))){
        Prefer_Code = Temp_DF$No.[Temp_DF$Vendor.No == "ATCIN03"]
        
      }else if(any(sapply(Temp_DF$Vendor.No., str_detect, pattern ="ATCIN01"))){
        Prefer_Code = Temp_DF$No.[Temp_DF$Vendor.No == "ATCIN01"]
        
      }else{
        Temp_DF$Prefer_Code = Temp_DF$No.[Temp_DF$Vendor.No == "ATCIS01"]
      }
      
    }else{
      if(any(sapply(Temp_DF$Vendor.No., str_detect, pattern ="ATCIN03"))){
        Prefer_Code = Temp_DF$No.[Temp_DF$Vendor.No == "ATCIN03"]
        
      }else if(any(sapply(Temp_DF$Vendor.No., str_detect, pattern ="ATCIN01"))){
        Prefer_Code = Temp_DF$No.[Temp_DF$Vendor.No == "ATCIN01"]
        
      }else{
        Prefer_Code = Temp_DF$No.[Temp_DF$Vendor.No == "ATCIS01"]
        
      }
      
    }
    Prefer_Code
  }
  
  Move_Item_Fun <- function(Parent.Code, Switch.Factory){
    
      
    
    
      Temp_DF <- Active_SKU_DF %>% filter(Parent.Item %in% Parent.Code & Vendor.No. %in% Switch.Factory)
      
      
      Item.Code <- ifelse(nrow(Temp_DF)>=1, Temp_DF$No.[Temp_DF$Vendor.No. == Switch.Factory], Prefer_Code_Fun(Parent.Code))

      
      Item.Code
  }
  

  
  
    
  output$value <- renderText({
    ac_number = na.exclude( CustomerDB$No.[CustomerDB$Name == input$Cus])
    ac_number
  })
  
})
