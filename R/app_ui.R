#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinydashboard
#' @import shinyjs
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic 
    fluidPage(
      #h1("OrximTechPMS"),
      useShinyjs(),
      includeCSS("inst/app/www/1_custom.css"),
        
        tags$header("Pharmacy Management System"),
        div(id = "page_links",
            actionButton(inputId = "pos_page", label = "Point of Sale"),
            actionButton(inputId = "salesdash_page", label = "Sales Dashboard"),
            actionButton(inputId = "inventory_page", label = "Inventory"),
            actionButton(inputId = "analytics_page", label = "Analytics"),
            actionButton(inputId = "help_page", label = "Help"),
            actionButton(inputId = "logout_page", label = "Logout")),
        
        div(id = "main_content_div",
            uiOutput("main_content")
        ),
        
        tags$footer("Powered by OrximTech Microservices Inc.")
    )
  )
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
 
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'OrximTechPMS'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}

