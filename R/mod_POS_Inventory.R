#' POS_Inventory UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList
#' @import DBI
#' @import dplyr
mod_POS_Inventory_ui <- function(id){
  ns <- NS(id)
  #tagList(
    div(id = "inventory_div",
        
        div(id = "inventory_inputs_div",
            div(id = "inventory_inputs_title_div",
                paste("Quantity/Price Update")),
            
            div(id = "inventory_new_old_product_div",
                uiOutput(ns("inventory_new_old_product"))),
            
            div(id = "inventory_master_new_product_div",
                uiOutput(ns("inventory_master_new_product"))),
            
            div(id = "inventory_master_old_product_div",
        uiOutput(ns("inventory_master_old_product")))
       ),
        
        div(id = "inventory_updated_product_display_div",
            dataTableOutput(ns("inventory_updated_product_display"))))
 # )
}
    
#' POS_Inventory Server Functions
#'
#' @noRd 
mod_POS_Inventory_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    #################IMPORT DATABASE##########################
    sql_database <- load_db(db_name = "inst/app/www/pharma_database/Pharmacy_Database_Manager.db")
    
    pricelist_db_table <- open_db_table(db_name = "inst/app/www/pharma_database/Pharmacy_Database_Manager.db",
                                       db_table = "PriceList")
    
    
    #sql_database <- reactive({
     # con <- dbConnect(RSQLite::SQLite(),dbname = "inst/app/www/pharma_database/Pharmacy_Database_Manager.db")
    #})
    
    #sql_table <- reactive({
     # sql_table <- dbReadTable(sql_database(), "PriceList")
    #})
    
    output$inventory_new_old_product <- renderUI({
      selectInput(inputId = ns("inventory_new_old_product"),
                  label = "Select Feature",
                  choices = c("Select", "New Product", "Update Old Product"))
    })
    
    output$inventory_master_new_product <- renderUI({
      tagList(
        textInput(inputId = ns("inventory_new_product_name"),
                  label = "New Product Name",
                  placeholder = "Enter Product Name"),
        numericInput(ns("inventory_new_product_quantity"),
                     label = "Quantity",
                     value = 0),
        numericInput(ns("inventory_new_product_price"),
                     label = "Price",
                     value = 0),
        actionButton(ns("inventory_new_add_product"),
                     "Add New Product")
      )
    })
    
    output$inventory_master_old_product <- renderUI({
      tagList(
              
              div(id = "inventory_queried_product_div",
                  uiOutput(ns("inventory_queried_product"))),
        
              div(id = "inventory_old_product_div",
                  uiOutput(ns("inventory_old_price_quantity"))),
              
              div(id = "inventory_old_product_quantity_div",
                  
                  verbatimTextOutput(ns("inventory_old_quantity_on_hand")),
                  
                  numericInput(ns("inventory_old_new_quantity"),
                               label = "New Quantity to Add",
                               value = 0),
                  
                  actionButton(ns("inventory_old_add_quantity"),
                               label = "Add Quantity"),
              
                  verbatimTextOutput(ns("inventory_old_updated_quantity"))
              ),
              
              
                  
              div(id = "inventory_old_product_price_div",
                  
                  verbatimTextOutput(ns("inventory_old_current_price")),
                  
                  numericInput(ns("inventory_old_new_price"),
                               label = "New Price",
                               value = 0),
                  
                  actionButton(ns("inventory_old_change_price"),
                                   "Change Price"),
                  verbatimTextOutput(ns("inventory_old_updated_price")))
             )
    })
    
    output$inventory_old_price_quantity <- renderUI({
      selectInput(inputId = ns("inventory_old_price_quantity"),
                  label = "Select Feature",
                  choices = c("Select","Quantity", "Price"))
    })
    
    output$inventory_queried_product <- renderUI({
      selectInput(inputId = ns("inventory_queried_product"), 
                  label = "Select Product",
                  choices = c(sort(pricelist_db_table$Product)))
    })
    
    #####################CHOOSE NEW OR OLD PRODUCT#############
    observeEvent(input$inventory_new_old_product,{
      if (input$inventory_new_old_product == "New Product"){
        shinyjs::hide("inventory_master_old_product")
        shinyjs::show("inventory_master_new_product")
      }
      
      else if (input$inventory_new_old_product == "Update Old Product"){
        shinyjs::show("inventory_master_old_product")
        shinyjs::hide("inventory_master_new_product")
      }
      
      else if (input$inventory_new_old_product == "Select"){
        shinyjs::hide("inventory_master_old_product")
        shinyjs::hide("inventory_master_new_product")
      }
    })
    
    ##################SELECT QUANTITY OR PRICE################
    observeEvent(input$inventory_old_price_quantity,{
      if (input$inventory_old_price_quantity == "Price"){
        shinyjs::hide("inventory_old_quantity_on_hand")
        shinyjs::hide("inventory_old_new_quantity")
        shinyjs::hide("inventory_old_add_quantity")
        shinyjs::hide("inventory_old_updated_quantity")
        
        
        shinyjs::show("inventory_old_current_price")
        shinyjs::show("inventory_old_new_price")
        shinyjs::show("inventory_old_change_price")
        shinyjs::show("inventory_old_updated_price")
      }
      
      else if (input$inventory_old_price_quantity == "Quantity"){
        shinyjs::show("inventory_old_quantity_on_hand")
        shinyjs::show("inventory_old_new_quantity")
        shinyjs::show("inventory_old_add_quantity")
        shinyjs::show("inventory_old_updated_quantity")
        
        
        shinyjs::hide("inventory_old_current_price")
        shinyjs::hide("inventory_old_new_price")
        shinyjs::hide("inventory_old_change_price")
        shinyjs::hide("inventory_old_updated_price")
      }
      
      else if (input$inventory_old_price_quantity == "Select"){
        shinyjs::hide("inventory_old_current_price")
        shinyjs::hide("inventory_old_new_price")
        shinyjs::hide("inventory_old_change_price")
        shinyjs::hide("inventory_old_updated_price")
        
        shinyjs::hide("inventory_old_quantity_on_hand")
        shinyjs::hide("inventory_old_new_quantity")
        shinyjs::hide("inventory_old_add_quantity")
        shinyjs::hide("inventory_old_updated_quantity")
      }
    })
    
    #####################FOR QUANTITY#########################
    output$inventory_old_quantity_on_hand <- renderPrint({
      price_list <- pricelist_db_table
      selected_product_df <- subset(price_list, price_list$Product == input$inventory_queried_product)
      cat(paste("On Hand:",selected_product_df$QTY))
    })
    ########################SEND QUERY#######################
    observeEvent(input$inventory_old_add_quantity,{
      price_list <- pricelist_db_table
      selected_product_df <- subset(price_list,price_list$Product == input$inventory_queried_product)
      qty_update <- as.numeric(selected_product_df$QTY) + as.numeric(input$inventory_old_new_quantity)
      selected_product <- paste0('"',paste(input$inventory_queried_product), '"')
      
      send_update <- paste0("UPDATE PriceList SET QTY = ", qty_update,
                           " WHERE Product = ", selected_product, ";")
      dbSendStatement(sql_database, send_update)
      
      output$inventory_old_updated_quantity <- renderPrint({
        cat(paste("Updated:",qty_update))
      })
    })
    #################UPDATE DATABASE##########################
    observeEvent(input$inventory_old_add_quantity,{
      sql_database <- load_db(db_name = "inst/app/www/pharma_database/Pharmacy_Database_Manager.db")
      
      pricelist_db_table <- open_db_table(db_name = "inst/app/www/pharma_database/Pharmacy_Database_Manager.db",
                                          db_table = "PriceList")
      
      #sql_database <- reactive({
       # con <- dbConnect(RSQLite::SQLite(),
        #                 dbname = "inst/app/www/pharma_database/Pharmacy_Database_Manager.db")
      #})
      
      #sql_table <- reactive({
       # sql_table <- dbReadTable(sql_database, "PriceList")
      #})
      
      output$inventory_updated_product_display <- renderDataTable({
        pricelist_db_table
      })
    })
    
    #####################FOR PRICE#########################
    output$inventory_old_current_price <- renderPrint({
      price_list <- pricelist_db_table
      selected_product_df <- subset(price_list, price_list$Product == input$inventory_queried_product)
      cat(paste("Current Price:",selected_product_df$Price))
    })
    ########################SEND QUERY#######################
    observeEvent(input$inventory_old_change_price,{
      price_list <- pricelist_db_table
      selected_product_df <- subset(price_list,price_list$Product == input$inventory_queried_product)
      price_update <- as.numeric(input$inventory_old_new_price)
      selected_product <- paste0('"',paste(input$inventory_queried_product), '"')
      
      send_update <- paste0("UPDATE PriceList SET Price = ", price_update,
                            " WHERE Product = ", selected_product, ";")
      dbSendStatement(sql_database, send_update)
      
      output$inventory_old_updated_price <- renderPrint({
        cat(paste("Updated Price:",price_update))
      })
    })
    #################UPDATE DATABASE##########################
    observeEvent(input$inventory_old_change_price,{
      sql_database <- load_db(db_name = "inst/app/www/pharma_database/Pharmacy_Database_Manager.db")
      
      pricelist_db_table <- open_db_table(db_name = "inst/app/www/pharma_database/Pharmacy_Database_Manager.db",
                                          db_table = "PriceList")
      
      #sql_database <- reactive({
       # con <- dbConnect(RSQLite::SQLite(),
        #                 dbname = "inst/app/www/pharma_database/Pharmacy_Database_Manager.db")
      #})
      
      #sql_table <- reactive({
       # sql_table <- dbReadTable(sql_database, "PriceList")
      #})
      
      output$inventory_updated_product_display <- renderDataTable({
        pricelist_db_table
      })
    })
    
    ########################SEND QUERY FOR NEW PRODUCT#######################
    observeEvent(input$inventory_new_add_product,{
      price_list <- pricelist_db_table
      new_product_price <- as.numeric(input$inventory_new_product_price)
      new_product_quantity <- as.numeric(input$inventory_new_product_quantity)
      new_product_name <- paste0('"',paste(input$inventory_new_product_name), '"')
      
      send_update <- paste0("INSERT INTO PriceList (Product, Price, QTY) ", "VALUES(", 
                            new_product_name, ",",
                            new_product_price, ",",
                            new_product_quantity, ")")
      send_update
      dbSendStatement(sql_database, send_update)
    })
    #################UPDATE DATABASE WITH NEW PRODUCT##########################
    observeEvent(input$inventory_new_add_product,{
      sql_database <- reactive({
        con <- dbConnect(RSQLite::SQLite(),
                         dbname = "inst/app/www/pharma_database/Pharmacy_Database_Manager.db")
      })
      
      sql_table <- reactive({
        sql_table <- dbReadTable(sql_database, "PriceList")
      })
      
      output$inventory_updated_product_display <- renderDataTable({
        pricelist_db_table
      })
    })
    #############################################################
  })
}
    
## To be copied in the UI
# mod_POS_Inventory_ui("POS_Inventory_ui_1")
    
## To be copied in the server
# mod_POS_Inventory_server("POS_Inventory_ui_1")
