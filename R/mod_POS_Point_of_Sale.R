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
#' @import RSQLite
#' @import dplyr
mod_POS_Point_of_Sale_ui <- function(id){
  ns <- NS(id)
  #tagList(
    div(id = "pos_div",
        div(id = "pos_entry_info_div",
            div(id = "pos_entry_info_title_div",
                paste("Entries")),
            
            textInput(inputId = ns("customer_name"),
                      label = NULL,
                      placeholder = "Customer Name"),
            
            uiOutput(ns("product_name")),
            
            numericInput(inputId = ns("product_qty"),
                         label = "Quantity",
                         value = NULL),
            
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
                verbatimTextOutput(ns("pos_table_total"))),
            div(id = "pos_approve_div",
                actionButton(ns("pos_approve"), 
                             "Approve")),
            div(id = "pos_clear_div",
                actionButton(ns("pos_clear"), 
                             "Clear")),
            div(id = "pos_print_div",
                uiOutput(ns("pos_print1"))
                ))
        
        )
  #)
}
    
#' POS_Point_of_Sale Server Functions
#'
#' @noRd 
mod_POS_Point_of_Sale_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
      output$pos_print1 <- renderUI({
        actionButton(ns("pos_print"), 
                     "Print",
                     disabled = TRUE)
    })
    #################IMPORT DATABASE##########################
    sql_database <- reactive({
      con <- dbConnect(RSQLite::SQLite(),dbname = "inst/app/www/pharma_database/test_pharma_database.db")
    })
    
    sql_table <- reactive({
      sql_table <- dbReadTable(sql_database(), "Price List")
    })
    
    output$product_name <- renderUI({
      selectInput(inputId = ns("product_name"), 
                  label = "Enter Product",
                  choices = sort(c(sql_table()$Product)))
    })
    ################ADD########################
    addv <- reactiveValues()
    addv$DF <- data.frame(Timestamp = as.character(),
                          Worker = as.character(),
                          Product = as.character(), 
                          QTY = as.numeric(), 
                          Price = as.numeric(),
                          Total = as.numeric(),
                          check.names = FALSE)
    
    observeEvent(input$add_product, {
      pharma_data <- sql_table()
      newRow <- data.frame(Timestamp = toString(Sys.time()),
                           Worker = input$customer_name,
                           Product = input$product_name, 
                           QTY = input$product_qty, 
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
                        choices = c("Select",addv$DF$Product))
    })
    
    observeEvent(input$delete_product,{
      remv$DF <- input$del_product_name
      addv$DF <- filter(addv$DF, !(Product %in% remv$DF))
    })
    
    ##################TOTAL AMOUNT################################
    output$pos_table_total <- renderPrint({
      cat(paste(sum(addv$DF$Total)))
    })

    ################APPROVE ########################
    observeEvent(input$pos_approve,{
      #worker_DF <- data.frame(Worker = rep(input$customer_name,length(rownames(addv$DF))))
      #addv$DF <- cbind(worker_DF, addv$DF)
      dbWriteTable(sql_database(), toString(paste(Sys.Date(),"Daily_Sales",collapse = "")), addv$DF, append =TRUE)
      dbWriteTable(sql_database(), "All Sales", addv$DF, append =TRUE)
      
      output$pos_print1 <- renderUI({
        actionButton(ns("pos_print"), 
                     HTML("Print"))
        })
      })
    
    ###################CLEAR TRANSACTION################
    observeEvent(input$pos_clear,{
      addv$DF <- data.frame(Timestamp = as.character(),
                            Product = as.character(), 
                            QTY = as.numeric(), 
                            Price = as.numeric(),
                            Total = as.numeric(),
                            check.names = FALSE)
      shinyjs::reset("del_product_name")
    })
    
    #################SAVE AND PUSH TO DATABASE##################

    ##################################################
  })
}
    
## To be copied in the UI
# mod_POS_Point_of_Sale_ui("POS_Point_of_Sale_ui_1")
    
## To be copied in the server
# mod_POS_Point_of_Sale_server("POS_Point_of_Sale_ui_1")
