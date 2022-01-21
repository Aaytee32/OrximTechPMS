#' POS_Point_of_Sale UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList
mod_POS_Point_of_Sale_ui <- function(id){
  ns <- NS(id)
  #tagList(
    div(id = "pos_div",
        div(id = "pos_entry_info_div",
            div(id = "pos_entry_info_title_div",
                paste("Entries")),
            
            textInput(inputId = ns("employee_name"),
                      label = NULL,
                      placeholder = "Employee Name"),
            
            uiOutput(ns("product_name")),
            
                verbatimTextOutput(ns("pos_qty_on_hand")),
            
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
      
    #################IMPORT DATABASE##########################
  
    ################SET DATABASE AS REACTIVEVALUE#################
    imported_database <- reactiveValues()
    imported_database$DB <- NULL
    
    extracted_sql_table <- reactiveValues()
    extracted_sql_table$DB <- NULL
    
    observeEvent(input$employee_name,{
      imported_database$DB <- load_db(db_name = "inst/app/www/pharma_database/Pharmacy_Database_Manager.db")
      
      extracted_sql_table$DB <- open_db_table(db_name = "inst/app/www/pharma_database/Pharmacy_Database_Manager.db",
                                          db_table = "PriceList")
    })
    
    #####################GETTING REACTIVE UIs####################
    output$pos_print1 <- renderUI({
      actionButton(ns("pos_print"), 
                   "Print",
                   disabled = TRUE)
    })
    
    output$product_name <- renderUI({
      selectInput(inputId = ns("product_name"), 
                  label = "Enter Product",
                  choices = sort(c(extracted_sql_table$DB$Product)))
    })
    
    output$pos_qty_on_hand <- renderPrint({
      pharma_data <- extracted_sql_table$DB
      qty_on_hand = pharma_data[pharma_data['Product'] ==input$product_name, ]["QTY"][1,]
      
      cat(paste("On Hand:", qty_on_hand))
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
      pharma_data <- extracted_sql_table$DB
      newRow <- data.frame(Timestamp = toString(Sys.time()),
                           Worker = input$employee_name,
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
      #addv$DF <- filter(addv$DF, !(Product %in% remv$DF))
      #addv$DF <- addv$DF %>% filter(!Product = remv$DF)
      
      to_drop <- which(with( addv$DF, Product == remv$DF ))
      addv$DF <- addv$DF[-to_drop,]
    })
    
    ##################TOTAL AMOUNT################################
    output$pos_table_total <- renderPrint({
      cat(paste(sum(addv$DF$Total)))
    })

    ################APPROVE ########################
    observeEvent(input$pos_approve,{
      output$pos_print1 <- renderUI({
        actionButton(ns("pos_print"),
                     HTML("Print"))
      })
    })
    
    observeEvent(input$pos_approve,{
      #worker_DF <- data.frame(Worker = rep(input$employee_name,length(rownames(addv$DF))))
      #addv$DF <- cbind(worker_DF, addv$DF)
      dbWriteTable(imported_database$DB, toString(paste(Sys.Date(),"Daily_Sales",collapse = "")), addv$DF, append =TRUE)
      dbWriteTable(imported_database$DB, "All Sales", addv$DF, append =TRUE)
    })
      
      ######################UPDATE MAIN DATABASE AFTER DISPENSING###############
    observeEvent(input$pos_approve,{
      temp_lastTransaction <- addv$DF
      #temp_lastTransaction
      dbWriteTable(imported_database$DB, "lastTransaction", temp_lastTransaction, overwrite= TRUE)
      upd_lastTransaction <- dbReadTable(imported_database$DB,"lastTransaction")
      #upd_lastTransaction
      product_list <- c(upd_lastTransaction$Product)
      #product_list
      quantity_list <- c(upd_lastTransaction$QTY)
      #quantity_list
      
      #iterate over the temporary database and update main database
      for (idx in 1:length(product_list)){
        product_name <- product_list[idx]
        #product_name
        dispensed_qty <- quantity_list[idx]
        #dispensed_qty
        
        #qty_on_hand <- sql_table %>% filter(Product == product_name) %>% select(QTY)
        #qty_on_hand
        
        qty_on_hand_product <- subset(extracted_sql_table$DB, 
                                      Product == product_name, 
                                      select= QTY)
        qty_on_hand <- qty_on_hand_product$QTY
        
        updated_qty_after_dispensed <- as.numeric(qty_on_hand) - as.numeric(dispensed_qty)
        #updated_qty_after_dispensed
        
        selected_product <- paste0('"',paste(product_name), '"')
        send_update <- paste0("UPDATE PriceList SET QTY = ", 
                              updated_qty_after_dispensed,
                              " WHERE Product = ",
                              selected_product, ";")
        #send_update
        dbSendStatement(imported_database$DB, send_update)
        
        #################RELOAD DATABASE AND TABLE#############
        #imported_database$DB <- load_db(db_name = "")
        #extracted_sql_table$DB <- open_db_table(, "PriceList")
        
        #imported_database$DB <- load_db(db_name = "inst/app/www/pharma_database/Pharmacy_Database_Manager.db")
        
        extracted_sql_table$DB <- open_db_table(db_name = "inst/app/www/pharma_database/Pharmacy_Database_Manager.db",
                                   db_table = "PriceList")

      }
      
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
