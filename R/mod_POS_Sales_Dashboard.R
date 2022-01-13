#' POS_Sales_Dashboard UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_POS_Sales_Dashboard_ui <- function(id){
  ns <- NS(id)
  #tagList(
    div(id = "salesdash_div",
        
        div(id = "salesdash_inputs_div",
            
            div(id = "salesdash_inputs_title_div",
                paste("Features")),
            
            div(id = "salesdash_selectInput_div",
                selectInput(inputId = "salesdash_selectInput",
                            label = "Choose Feature",
                            choices = NULL)),
            
            div(id = "salesdash_sliderInput_div",
                sliderInput(inputId = "salesdash_sliderInput",
                            label = "Summary",
                            min = 0,
                            max = 1500,
                            value = 100)),
            
            div(id = "salesdash_dateRangeInput_div",
                dateRangeInput(inputId = "salesdash_dateRangeInput",
                               label = "Select Range",
                               start = "2020-01-01",
                               end = "2022-01-01"))
        ),
        div(id = "salesdash_display_div")
        )
  #)
}
    
#' POS_Sales_Dashboard Server Functions
#'
#' @noRd 
mod_POS_Sales_Dashboard_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_POS_Sales_Dashboard_ui("POS_Sales_Dashboard_ui_1")
    
## To be copied in the server
# mod_POS_Sales_Dashboard_server("POS_Sales_Dashboard_ui_1")
