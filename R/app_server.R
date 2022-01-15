#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinyjs
#' @import plotly
#' @noRd
app_server <- function( input, output, session ) {
  # Your application server logic
  
  ####################LOGIN/HOMEPAGE###################
  output$page_links <- renderUI({
    fluidRow(width =12,
    actionButton(inputId = "pos_page", label = "Point of Sale"),
    actionButton(inputId = "salesdash_page", label = "Sales Dashboard"),
    actionButton(inputId = "inventory_page", label = "Inventory"),
    actionButton(inputId = "analytics_page", label = "Analytics"),
    actionButton(inputId = "help_page", label = "Help"),
    actionButton(inputId = "logout_page", label = "Logout"))
  })
  
  output$main_content <- renderUI({
    mod_POS_Login_Page_ui("POS_Login_Page_ui_1")
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
    output$main_content <- renderUI({
      mod_POS_Login_Page_ui("POS_Login_Page_ui_1")
    })
    
    output$page_links <- renderUI({
      fluidRow(
    actionButton(inputId = "pos_page", label = "Point of Sale", disabled = TRUE),
    actionButton(inputId = "salesdash_page", label = "Sales Dashboard", disabled = TRUE),
    actionButton(inputId = "inventory_page", label = "Inventory", disabled = TRUE),
    actionButton(inputId = "analytics_page", label = "Analytics", disabled = TRUE),
    actionButton(inputId = "help_page", label = "Help", disabled = TRUE),
    actionButton(inputId = "logout_page", label = "Logout")
      )
    })
  })
  
  
#############################################################################  
  mod_POS_Login_Page_server("POS_Login_Page_ui_1")
  mod_POS_Point_of_Sale_server("POS_Point_of_Sale_ui_1")
  mod_POS_Sales_Dashboard_server("POS_Sales_Dashboard_ui_1")
  mod_POS_Inventory_server("POS_Inventory_ui_1")
  mod_POS_Analytics_server("POS_Analytics_ui_1")
  mod_POS_Help_server("POS_Help_ui_1")
  mod_POS_Logout_server("POS_Logout_ui_1")
  
}
