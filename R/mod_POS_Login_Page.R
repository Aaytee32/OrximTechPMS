#' POS_Login_Page UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_POS_Login_Page_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' POS_Login_Page Server Functions
#'
#' @noRd 
mod_POS_Login_Page_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_POS_Login_Page_ui("POS_Login_Page_ui_1")
    
## To be copied in the server
# mod_POS_Login_Page_server("POS_Login_Page_ui_1")
