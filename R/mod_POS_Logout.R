#' POS_Logout UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_POS_Logout_ui <- function(id){
  ns <- NS(id)
  #tagList(
    div(id = "pos_div",
        paste("This will have a point of logout feature"))
  #)
}
    
#' POS_Logout Server Functions
#'
#' @noRd 
mod_POS_Logout_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_POS_Logout_ui("POS_Logout_ui_1")
    
## To be copied in the server
# mod_POS_Logout_server("POS_Logout_ui_1")
