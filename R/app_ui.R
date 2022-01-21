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
      tags$link(rel="stylesheet", type="text/css", href="www/1_custom.css"),
      #includeCSS("www/1_custom.css"),
        
        tags$header("Pharmacy Management System"),
        div(id = "page_links_div",
            uiOutput("page_links")),
        
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
    #,tags$link(rel="stylesheet", type="text/css", href="www/1_custom.css")
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}

