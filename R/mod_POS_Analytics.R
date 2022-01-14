#' POS_Analytics UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_POS_Analytics_ui <- function(id){
  ns <- NS(id)
  #tagList(
    div(id = "analytics_div",
        
        div(id = "analytics_inputs_div",
            
            div(id = "analytics_inputs_title_div",
                paste("Features")),
            
            div(id = "analytics_selectInput_div",
                selectInput(inputId = "analytics_selectInput",
                            label = "Choose Feature",
                            choices = c("Select",
                                        "Periodic Performance",
                                        "Employee Statistics",
                                        "Forecasting"
                                        ))),
            
            div(id = "analytics_selectInput_sub_div",
                selectInput(inputId = "analytics_selectInput_sub",
                            label = "Choose Filter",
                            choices = NULL)),
            
            div(id = "analytics_sliderInput_div",
                sliderInput(inputId = "analytics_sliderInput",
                            label = "Summary",
                            min = 0,
                            max = 1500,
                            value = 100)),
            
            div(id = "analytics_dateRangeInput_div",
                dateRangeInput(inputId = "analytics_dateRangeInput",
                               label = "Select Range",
                               start = "2020-01-01",
                               end = "2022-01-01"))
        ),
        div(id = "analytics_display_div"))
  #)
}
    
#' POS_Analytics Server Functions
#'
#' @noRd 
mod_POS_Analytics_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
 
  })
}
    
## To be copied in the UI
# mod_POS_Analytics_ui("POS_Analytics_ui_1")
    
## To be copied in the server
# mod_POS_Analytics_server("POS_Analytics_ui_1")
