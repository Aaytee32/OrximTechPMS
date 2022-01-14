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
            textInput(inputId = "customer_name",
                      label = NULL,
                      placeholder = "Customer Name"),
            
            column(width = 12,
                   column(width = 6,
                          selectInput(inputId = "product_name",
                                      label = "Select Product",
                                      choices = NULL)),
                   column(width = 6,
                          numericInput(inputId = "product_qty",
                                       label = "Quantity",
                                       value = NULL))),
            
                        actionButton(inputId = "add_product",

                     label = "Add"),
            
            div(id = "del_product_name_div",
                         selectInput(inputId = "del_product_name",
                          label = "Select Product to Delete",
                          choices = NULL)),
        
            actionButton(inputId = "delete_product",
                         label = "Drop")
            ),
        
        div(id = "pos_table_div",
                div(id = "pos_table_title_div",
                    paste("Transaction")
                ),
                div(id = "pos_table",
                    tableOutput("pos_table_output"))),
        
        div(id = "pos_total_div",
            div(id = "pos_total_title_div",
                paste("Transaction Summary")
                ),
            div(id = "pos_total_amt",
                verbatimTextOutput("pos_table_total")),
            div(id = "pos_approve_div",
                actionButton("pos_approve", 
                             "Approve")),
            div(id = "pos_clear_div",
                actionButton("pos_clear", 
                             "Clear")),
            div(id = "pos_new_transaction_div",
                actionButton("pos_new_transaction", 
                             "New Transaction")))
        
        )
  #)
}
    
#' POS_Point_of_Sale Server Functions
#'
#' @noRd 
mod_POS_Point_of_Sale_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_POS_Point_of_Sale_ui("POS_Point_of_Sale_ui_1")
    
## To be copied in the server
# mod_POS_Point_of_Sale_server("POS_Point_of_Sale_ui_1")
