#' POS_HomePage UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_POS_HomePage_ui <- function(id){
  ns <- NS(id)
  tagList(
    
  )
}
    
#' POS_HomePage Server Functions
#'
#' @noRd 
mod_POS_HomePage_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_POS_HomePage_ui("POS_HomePage_ui_1")
    
## To be copied in the server
# mod_POS_HomePage_server("POS_HomePage_ui_1")
