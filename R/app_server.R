#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinyjs
#' @import plotly
#' @import RSQLite
#' @import DBI
#' @noRd
app_server <- function( input, output, session ) {
  # Your application server logic
  
  ####################LOGIN/HOMEPAGE###################
  
  #######################DISABLING LINKS####################
  output$page_links <- renderUI({
    fluidRow(width =12,
            actionButton(inputId = "inventory_page", label = "Inventory", disabled = TRUE),
            actionButton(inputId = "pos_page", label = "Point of Sale", disabled = TRUE),
            actionButton(inputId = "salesdash_page", label = "Sales Dashboard", disabled = TRUE),
            actionButton(inputId = "analytics_page", label = "Analytics", disabled = TRUE),
            actionButton(inputId = "help_page", label = "Help", disabled = TRUE),
            actionButton(inputId = "logout_page", label = "Logout"))
  })
  
  output$main_content <- renderUI({
    #mod_POS_Login_Page_ui("POS_Login_Page_ui_1")
    tagList(
      
      div(id = "login_div",
          
          div(id = "login",
              
              div("Login", id = "login_title_div"),
              
              div(textInput(inputId = "username", 
                            label = NULL, 
                            placeholder = "Username"), 
                  id = "username_div"),
              div(passwordInput(inputId = "password",
                                label = NULL,
                                placeholder = "Password"),
                  id = "password_div"),
              div(actionButton("employee", "Employee"), 
                  actionButton("admin", "Administrator"), 
                  id = "employee_admin_div")),
          div(id = "feedback_div",
              verbatimTextOutput("feedback")))
      )
  })
  
  #######################LINKS######################
  
  observeEvent(input$pos_page,{
    output$main_content <- renderUI({
      mod_POS_Point_of_Sale_ui("POS_Point_of_Sale_ui_1")
    })
  })
  
  observeEvent(input$salesdash_page,{
    output$main_content <- renderUI({
      mod_POS_Sales_Dashboard_ui("POS_Sales_Dashboard_ui_1")
    })
  })
  
  observeEvent(input$inventory_page,{
    output$main_content <- renderUI({
      mod_POS_Inventory_ui("POS_Inventory_ui_1")
    })
  })
  
  observeEvent(input$analytics_page,{
    output$main_content <- renderUI({
      mod_POS_Analytics_ui("POS_Analytics_ui_1")
    })
  })
  
  observeEvent(input$help_page,{
    output$main_content <- renderUI({
      mod_POS_Help_ui("POS_Help_ui_1")
    })
  })
  
  observeEvent(input$logout_page,{
    
    output$feedback <- renderUI({
      return()
    })
    
    authv$Enable <- FALSE
    adminv$Enable <- FALSE
    
    output$main_content <- renderUI({
      #mod_POS_Login_Page_ui("POS_Login_Page_ui_1")
      tagList(
        
        div(id = "login_div",
            
            div(id = "login",
                
                div("Login", id = "login_title_div"),
                
                div(textInput(inputId = "username", 
                              label = NULL, 
                              placeholder = "Username"), 
                    id = "username_div"),
                div(passwordInput(inputId = "password",
                                  label = NULL,
                                  placeholder = "Password"),
                    id = "password_div"),
                div(actionButton("employee", "Employee"), 
                    actionButton("admin", "Administrator"), 
                    id = "employee_admin_div")),
            div(id = "feedback_div",
                verbatimTextOutput("feedback")))
      )
    })
    
    output$page_links <- renderUI({
      fluidRow(
        actionButton(inputId = "inventory_page", label = "Inventory", disabled = TRUE),
        actionButton(inputId = "pos_page", label = "Point of Sale", disabled = TRUE),
        actionButton(inputId = "salesdash_page", label = "Sales Dashboard", disabled = TRUE),
        actionButton(inputId = "analytics_page", label = "Analytics", disabled = TRUE),
        actionButton(inputId = "help_page", label = "Help", disabled = TRUE),
        actionButton(inputId = "logout_page", label = "Logout")
      )
    })
  })
  
  
#############################################################################  
  #mod_POS_Login_Page_server("POS_Login_Page_ui_1")
  mod_POS_Point_of_Sale_server("POS_Point_of_Sale_ui_1")
  mod_POS_Sales_Dashboard_server("POS_Sales_Dashboard_ui_1")
  mod_POS_Inventory_server("POS_Inventory_ui_1")
  mod_POS_Analytics_server("POS_Analytics_ui_1")
  mod_POS_Help_server("POS_Help_ui_1")
  mod_POS_Logout_server("POS_Logout_ui_1")
  
  
  #####################TESTING LOGIN HERE#############################
  
  
  ###########################IMPORT DATABASE##########################
  #sql_database <- load_db(db_name = "inst/app/www/pharma_database/Pharmacy_Database_Manager.db")
  employee_db_table <- open_db_table(db_name = "inst/app/www/pharma_database/Pharmacy_Database_Manager.db",
                                     db_table = "AuthenticationTable")
  admin_db_table <- open_db_table(db_name = "inst/app/www/pharma_database/Pharmacy_Database_Manager.db",
                                  db_table = "AdminTable")
  
  ##########################FOR EMPLOYEES ONLY#######################
  auth_username_list <- reactive({
    username_list <- c(employee_db_table$Username)
    username_list
  })
  
  auth_password_list <- reactive({
    password_list <- c(employee_db_table$Password)
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
        shinyjs::hide("login")
        shinyjs::show("feedback")
        output$feedback <- renderPrint({
          cat(paste("Login Successful!"))
        })
      }else{
        authv$Enable <- FALSE
        output$feedback <- renderPrint({
          cat(paste("Try Again or contact your Administrator"))
        })
      }
    } else{
      authv$Enable <- FALSE
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
    }else{
      output$page_links <- renderUI({
        fluidRow(
          actionButton(inputId = "inventory_page", label = "Inventory", disabled = TRUE),
          actionButton(inputId = "pos_page", label = "Point of Sale", disabled = TRUE),
          actionButton(inputId = "salesdash_page", label = "Sales Dashboard", disabled = TRUE),
          actionButton(inputId = "analytics_page", label = "Analytics", disabled = TRUE),
          actionButton(inputId = "help_page", label = "Help", disabled = TRUE),
          actionButton(inputId = "logout_page", label = "Logout")
        )
      })
    }
  })
  
  
  ##########################FOR ADMIN ONLY###########################
  #sql_table_admin_df <- reactive({
   # sql_table_admin_df <- dbReadTable(sql_database(), "AdminTable")
    #sql_table_admin_df
  #})
  
  admin_username_list <- reactive({
    username_list <- c(admin_db_table$Username)
    username_list
  })
  
  admin_password_list <- reactive({
    password_list <- c(admin_db_table$Password)
    password_list
  })
  #########################Conditions for log in##################################
  adminv <- reactiveValues()
  adminv$User <- c()
  adminv$Pass <- c()
  adminv$Enable <- FALSE
  
  observeEvent(input$password,{
    adminv$User <- c(input$username)
    adminv$Pass <- c(input$password)
  })
  
  observeEvent(input$admin,{
    if (adminv$User %in% admin_username_list() &
        adminv$Pass %in% admin_password_list()){
      if (which(admin_username_list() == adminv$User) == which(admin_password_list() == adminv$Pass)){
        adminv$Enable <- TRUE
        shinyjs::hide("login")
        shinyjs::show("feedback")
        output$feedback <- renderPrint({
          cat(paste("Login Successful!"))
        })
      }else{
        adminv$Enable <- FALSE
        output$feedback <- renderPrint({
          cat(paste("Try Again or contact your Administrator"))
        })
      }
    } else{
      adminv$Enable <- FALSE
      output$feedback <- renderPrint({
        cat(paste("Try Again or contact your Administrator"))
      })
    }
  })
  
  #################RE-ENABLING LINKS################
  observeEvent(input$admin,{
    if (adminv$Enable == TRUE){
      output$page_links <- renderUI({
        fluidRow(width =12,
                 actionButton(inputId = "inventory_page", label = "Inventory"),
                 actionButton(inputId = "pos_page", label = "Point of Sale"),
                 actionButton(inputId = "salesdash_page", label = "Sales Dashboard"),
                 actionButton(inputId = "analytics_page", label = "Analytics"),
                 actionButton(inputId = "help_page", label = "Help"),
                 actionButton(inputId = "logout_page", label = "Logout"))
      })
    }else{
      output$page_links <- renderUI({
        fluidRow(
          actionButton(inputId = "inventory_page", label = "Inventory", disabled = TRUE),
          actionButton(inputId = "pos_page", label = "Point of Sale", disabled = TRUE),
          actionButton(inputId = "salesdash_page", label = "Sales Dashboard", disabled = TRUE),
          actionButton(inputId = "analytics_page", label = "Analytics", disabled = TRUE),
          actionButton(inputId = "help_page", label = "Help", disabled = TRUE),
          actionButton(inputId = "logout_page", label = "Logout")
        )
      })
    }
  })
  
}
