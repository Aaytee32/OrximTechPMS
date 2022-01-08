#' POS_Help UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_POS_Help_ui <- function(id){
  ns <- NS(id)
  #tagList(
    div(id = "pos_div",
        paste("This will have a point of help feature"))
  #)
}
    
#' POS_Help Server Functions
#'
#' @noRd 
mod_POS_Help_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_POS_Help_ui("POS_Help_ui_1")
    
## To be copied in the server
# mod_POS_Help_server("POS_Help_ui_1")
