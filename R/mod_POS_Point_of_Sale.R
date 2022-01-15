#' POS_Point_of_Sale UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList
#' @import DT
mod_POS_Point_of_Sale_ui <- function(id){
  ns <- NS(id)
  #tagList(
    div(id = "pos_div",
        div(id = "pos_entry_info_div",
            div(id = "pos_entry_info_title_div",
                paste("Entries")),
            textInput(inputId = "customer_name",
                      label = NULL,
                      placeholder = "Customer Name"),
            
            column(width = 12,
                   column(width = 6,
                          uiOutput(ns("product_name"))),
                   column(width = 6,
                          numericInput(inputId = ns("product_qty"),
                                       label = "Quantity",
                                       value = NULL))),
            
                        actionButton(inputId = ns("add_product"),

                     label = "Add"),
            
            div(id = "del_product_name_div",
                         selectInput(inputId = ns("del_product_name"),
                          label = "Select Product to Delete",
                          choices = NULL)),
        
            actionButton(inputId = ns("delete_product"),
                         label = "Drop")
            ),
        
        div(id = "pos_table_div",
                div(id = "pos_table_title_div",
                    paste("Transaction")
                ),
                div(id = "pos_table",
                    tableOutput(ns("pos_table_output")))),
        
        div(id = "pos_total_div",
            div(id = "pos_total_title_div",
                paste("Transaction Summary")
                ),
            div(id = "pos_total_amt",
                verbatimTextOutput("pos_table_total")),
            div(id = "pos_approve_div",
                actionButton(ns("pos_approve"), 
                             "Approve")),
            div(id = "pos_clear_div",
                actionButton(ns("pos_clear"), 
                             "Clear")),
            div(id = "pos_new_transaction_div",
                actionButton("pos_new_transaction", 
                             HTML("New<br/>Transaction"))))
        
        )
  #)
}
    
#' POS_Point_of_Sale Server Functions
#'
#' @noRd 
mod_POS_Point_of_Sale_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    ############################################
    pharma_data <- reactive({
      ds <- read_xlsx("C:\\Users\\aoppong\\OneDrive\\Manuscript3\\PharmaDataAnalysis\\sample_pharma_data.xlsx")
      as.data.frame(ds)
    })
    
    output$product_name <- renderUI({
      selectInput(inputId = ns("product_name"),
                  label = "Select Product",
                  choices = sort(unique(pharma_data()$Product)))
    })
    ################ADD########################
    addv <- reactiveValues()
    addv$DF <- data.frame(Item = as.character(), 
                          Qty = as.numeric(), 
                          Price = as.numeric(),
                          Total = as.numeric(),
                          check.names = FALSE)
    
    observeEvent(input$add_product, {
      pharma_data <- pharma_data()
      newRow <- data.frame(Item = input$product_name, 
                           Qty = input$product_qty, 
                           Price = pharma_data[pharma_data['Product'] ==input$product_name, ]["Price"][1,],
                           Total = input$product_qty * pharma_data[pharma_data['Product'] ==input$product_name, ]["Price"][1,],
                           check.names = FALSE)
      addv$DF <- rbind(addv$DF, newRow)
      
      output$pos_table_output <- renderTable({
        addv$DF #%>%
          #datatable(options = list(searching = FALSE,
              #                     lengthChange = FALSE))
      })
    })
    
    ########REMOVE##############
    remv <- reactiveValues()
    remv$DF <- c()
    
    observeEvent(input$add_product,{
      updateSelectInput(inputId = "del_product_name", 
                        label = "Select Product to Delete", 
                        choices = c(addv$DF$Item))
    })
    
    observeEvent(input$delete_product,{
      remv$DF <- input$del_product_name
      addv$DF <- filter(addv$DF, !(Item %in% remv$DF))
    })
  })
}
    
## To be copied in the UI
# mod_POS_Point_of_Sale_ui("POS_Point_of_Sale_ui_1")
    
## To be copied in the server
# mod_POS_Point_of_Sale_server("POS_Point_of_Sale_ui_1")
