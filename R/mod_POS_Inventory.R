#' POS_Inventory UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
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
                         value = NULL)),
        
        div(id = "add_quantity_div",
            actionButton(ns("add_quantity"),
                            "Add Quantity")),
        
        div(id = "inventory_updated_quantity_div",
            verbatimTextOutput(ns("inventory_updated_quantity")))),
        
        div(id = "inventory_updated_product_div",
            tableOutput(ns("inventory_updated_product"))))
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
      con <- dbConnect(RSQLite::SQLite(),dbname = "inst/app/www/pharma_database/test_pharma_database.db")
    })
    
    sql_table <- reactive({
      sql_table <- dbReadTable(sql_database(), "Price List")
    })
    
    output$inventory_queried_product <- renderUI({
      selectInput(inputId = ns("inventory_queried_product"), 
                  label = "Select Product",
                  choices = sort(c(sql_table()$Product)))
    })
    
    output$inventory_quantity_on_hand <- renderPrint({
      selected_product <- subset(ds,ds$Product == input$inventory_queried_product)
      cat(paste(selected_product$QTY))
    })
    
    output$inventory_updated_quantity <- renderPrint({
      cat(paste(selected_product$QTY))
    })
 
  })
}
    
## To be copied in the UI
# mod_POS_Inventory_ui("POS_Inventory_ui_1")
    
## To be copied in the server
# mod_POS_Inventory_server("POS_Inventory_ui_1")
