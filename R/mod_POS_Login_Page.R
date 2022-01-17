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
            
            div(textInput(inputId = ns("username"), 
                          label = NULL, 
                          placeholder = "Username"), 
                id = "username_div"),
            div(passwordInput(inputId = ns("password"),
                          label = NULL,
                          placeholder = "Password"),
                id = "password_div"),
            div(actionButton(ns("employee"), "Employee"), 
                actionButton(ns("admin"), "Administrator"), 
                id = "employee_admin_div")),
        div(id = "feedback_div",
            verbatimTextOutput(ns("feedback"))))
    #)
}
    
#' POS_Login_Page Server Functions
#'
#' @noRd 
mod_POS_Login_Page_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    ###########################IMPORT DATABASE##########################
    sql_database <- reactive({
      sql_database <- dbConnect(RSQLite::SQLite(),dbname = "inst/app/www/pharma_database/Pharmacy_Database_Manager.db")
      sql_database
    })
    
    ##########################FOR EMPLOYEES ONLY#######################
    sql_table_authen_df <- reactive({
      sql_table_authen_df <- dbReadTable(sql_database(), "AuthenticationTable")
      sql_table_authen_df
    })
    
    auth_username_list <- reactive({
      username_list <- c(sql_table_authen_df()$Username)
      username_list
    })
    
    auth_password_list <- reactive({
      password_list <- c(sql_table_authen_df()$Password)
      password_list
    })
    #########################Conditions for log in##################################
    authv <- reactiveValues()
    authv$User <- c()
    authv$Pass <- c()
    authv$Enable <- FALSE
    
    observeEvent(input$password,{
      authv$User <- c(input$username)
      authv$Pass <- c(input$password)
    })
    
    observeEvent(input$employee,{
      if (authv$User %in% auth_username_list() &
          authv$Pass %in% auth_password_list()){
        if (which(auth_username_list() == authv$User) == which(auth_password_list() == authv$Pass)){
          authv$Enable <- TRUE
          shinyjs::hide("employee")
          shinyjs::hide("admin")
          shinyjs::hide("username")
          shinyjs::hide("password")
          output$feedback <- renderPrint({
            cat(paste("Login Successful!"))
          })
        }else{
          output$feedback <- renderPrint({
            cat(paste("Try Again or contact your Administrator"))
          })
        }
      } else{
        output$feedback <- renderPrint({
          cat(paste("Try Again or contact your Administrator"))
        })
      }
    })
    
    #################RE-ENABLING LINKS################
    observeEvent(input$employee,{
      if (authv$Enable == TRUE){
        output$page_links <- renderUI({
          fluidRow(width =12,
                   actionButton(inputId = "inventory_page", label = "Inventory"),
                   actionButton(inputId = "pos_page", label = "Point of Sale"),
                   actionButton(inputId = "salesdash_page", label = "Sales Dashboard"),
                   actionButton(inputId = "analytics_page", label = "Analytics"),
                   actionButton(inputId = "help_page", label = "Help"),
                   actionButton(inputId = "logout_page", label = "Logout"))
        })
      }
      
    })
 
  })
}
    
## To be copied in the UI
# mod_POS_Login_Page_ui("POS_Login_Page_ui_1")
    
## To be copied in the server
# mod_POS_Login_Page_server("POS_Login_Page_ui_1")
