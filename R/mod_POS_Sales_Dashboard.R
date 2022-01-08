#' POS_Sales_Dashboard UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_POS_Sales_Dashboard_ui <- function(id){
  ns <- NS(id)
  #tagList(
    div(id = "pos_div",
        paste("This will have a point of dashboard feature"))
  #)
}
    
#' POS_Sales_Dashboard Server Functions
#'
#' @noRd 
mod_POS_Sales_Dashboard_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_POS_Sales_Dashboard_ui("POS_Sales_Dashboard_ui_1")
    
## To be copied in the server
# mod_POS_Sales_Dashboard_server("POS_Sales_Dashboard_ui_1")
