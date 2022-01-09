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
  #tagList(
        
    div(id = "login_div",
    
    div(id = "login",
        
        div("Login", id = "login_title_div"),
        
        div(textInput(inputId = "username", 
                      label = NULL, 
                      placeholder = "Username"), 
            id = "username_div"),
        div(textInput(inputId = "password",
                      label = NULL,
                      placeholder = "Password"),
            id = "password_div"),
        div(actionButton("employee", "Employee"), 
            actionButton("admin", "Administrator"), 
            id = "employee_admin_div")))
    #)
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
