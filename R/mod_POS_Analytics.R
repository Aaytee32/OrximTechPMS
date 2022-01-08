#' POS_Analytics UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_POS_Analytics_ui <- function(id){
  ns <- NS(id)
  #tagList(
    div(id = "pos_div",
        paste("This will have a point of analytics feature"))
  #)
}
    
#' POS_Analytics Server Functions
#'
#' @noRd 
mod_POS_Analytics_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_POS_Analytics_ui("POS_Analytics_ui_1")
    
## To be copied in the server
# mod_POS_Analytics_server("POS_Analytics_ui_1")
