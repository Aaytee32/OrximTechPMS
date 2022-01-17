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
              paste("Quantity Update")),
        div(id = "inventory_queried_product_div",
            uiOutput(ns("inventory_queried_product"))),
        div(id = "inventory_quantity_on_hand_div",
            verbatimTextOutput(ns("inventory_quantity_on_hand"))),
        
        div(id = "inventory_new_quantity_div",
            numericInput(ns("inventory_new_quantity"),
                         "New Quantity to Add",
                         value = 0)),
        
        div(id = "add_quantity_div",
            actionButton(ns("add_quantity"),
                            "Add Quantity")),
        
        div(id = "inventory_updated_quantity_div",
            verbatimTextOutput(ns("inventory_updated_quantity")))),
        
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
    sql_database <- reactive({
      con <- dbConnect(RSQLite::SQLite(),dbname = "inst/app/www/pharma_database/Pharmacy_Database_Manager.db")
    })
    
    sql_table <- reactive({
      sql_table <- dbReadTable(sql_database(), "PriceList")
    })
    
    output$inventory_queried_product <- renderUI({
      selectInput(inputId = ns("inventory_queried_product"), 
                  label = "Select Product",
                  choices = c(sort(sql_table()$Product)))
    })
    
    output$inventory_quantity_on_hand <- renderPrint({
      price_list <- sql_table()
      selected_product_df <- subset(price_list, price_list$Product == input$inventory_queried_product)
      cat(paste("On Hand:",selected_product_df$QTY))
    })
    
    ########################SEND QUERY#######################
    observeEvent(input$add_quantity,{
      price_list <- sql_table()
      selected_product_df <- subset(price_list,price_list$Product == input$inventory_queried_product)
      qty_update <- as.numeric(selected_product_df$QTY) + as.numeric(input$inventory_new_quantity)
      selected_product <- paste0('"',paste(input$inventory_queried_product), '"')
      
      send_update <- paste0("UPDATE PriceList SET QTY = ", qty_update,
                           " WHERE Product = ", selected_product, ";")
      dbSendStatement(sql_database(), send_update)
      
      output$inventory_updated_quantity <- renderPrint({
        cat(paste("Updated:",qty_update))
      })
    })
    
    #################UPDATE DATABASE##########################
    observeEvent(input$add_quantity,{
      sql_database <- reactive({
        con <- dbConnect(RSQLite::SQLite(),
                         dbname = "inst/app/www/pharma_database/Pharmacy_Database_Manager.db")
      })
      
      sql_table <- reactive({
        sql_table <- dbReadTable(sql_database(), "PriceList")
      })
      
      output$inventory_updated_product_display <- renderDataTable({
        sql_table()
      })
    })
 
  })
}
    
## To be copied in the UI
# mod_POS_Inventory_ui("POS_Inventory_ui_1")
    
## To be copied in the server
# mod_POS_Inventory_server("POS_Inventory_ui_1")
