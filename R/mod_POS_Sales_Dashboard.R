#' POS_Sales_Dashboard UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList
#' @import plotly
#' @import readr
#' @import readxl
mod_POS_Sales_Dashboard_ui <- function(id){
  ns <- NS(id)
  #tagList(
  
  
    div(id = "salesdash_div",
        
        div(id = "salesdash_inputs_div",
            
            div(id = "salesdash_inputs_title_div",
                paste("Features")),
            
            div(id = "salesdash_selectInput_div",
                selectInput(inputId = ns("salesdash_selectInput"),
                            label = "Choose Feature",
                            choices = c("Select",
                                        "Sales vrs Quantity",
                                        "Sales vrs Product",
                                        "Top Sales",
                                        "Bottom Sales",
                                        "Top Quantities",
                                        "Bottom Quantities"))),
            
            div(id = "salesdash_sliderInput_div",
                sliderInput(inputId = ns("salesdash_sliderInput"),
                            label = "Summary",
                            min = 0,
                            max = 1500,
                            value = 100)),
            
            div(id = "salesdash_dateRangeInput_div",
                dateRangeInput(inputId = ns("salesdash_dateRangeInput"),
                               label = "Select Range",
                               start = "2020-01-01",
                               end = "2022-01-01"))
        ),
        div(id = "salesdash_display_div",
            plotlyOutput(ns("salesdash_display")))
        )
  #)
}
    
#' POS_Sales_Dashboard Server Functions
#'
#' @noRd 
mod_POS_Sales_Dashboard_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    ds <- reactive({
      ds <- read_xlsx("C:\\Users\\aoppong\\OneDrive\\Manuscript3\\PharmaDataAnalysis\\sample_pharma_data.xlsx")
      as.data.frame(ds)
    })
    ######################PRODUCT VS PRICE###########################
    observeEvent(input$salesdash_selectInput,{
      if (input$salesdash_selectInput == "Sales vrs Product"){
        output$salesdash_display <- renderPlotly({
          group_ds_price <- aggregate(Price ~ Product, data = ds(), sum)
          
          plot_ly(data = group_ds_price, x = ~reorder(Product,-Price), y = ~Price, type = "bar") %>%
            layout(plot_bgcolor = "#000000",
                   paper_bgcolor = "#000000",
                   font = list(color = '#00FFFF'),
                   yaxis = list(title = "Sales"),
                   xaxis = list(title = "Products"),
                   title = "Sales by Product")
        })
      }
    })
    
    ###################QTY vs PRICE###########################
    observeEvent(input$salesdash_selectInput,{
      if (input$salesdash_selectInput == "Sales vrs Quantity"){
        output$salesdash_display <- renderPlotly({
          
          plot_ly(data = ds(), x = ~QTY, y = ~Price, type = "scatter", mode = "markers") %>%
            layout(plot_bgcolor = "#000000",
                   paper_bgcolor = "#000000",
                   font = list(color = '#00FFFF'),
                   yaxis = list(title = "Sales"),
                   title = "Sales vrs Quantity")
        })
      }
    })
    
    ###################TOP SALES###########################
    observeEvent(input$salesdash_selectInput,{
      if (input$salesdash_selectInput == "Top Sales"){
        output$salesdash_display <- renderPlotly({
          
          group_ds_price <- aggregate(Price ~ Product, data = ds(), sum)
          #head(group_ds_price,2)
          group_ds_price_order <- group_ds_price[order(-group_ds_price$Price),]
          #head(group_ds_price_order)
          group_ds_price_300 <- head(group_ds_price_order,input$salesdash_sliderInput)
          plot_ly(data = group_ds_price_300, x = ~reorder(Product,-Price), y = ~Price, type = "bar") %>%
            layout(plot_bgcolor = "#000000",
                   paper_bgcolor = "#000000",
                   font = list(color = '#00FFFF'),
                   yaxis = list(title = "Sales"),
                   xaxis = list(title = "Products"),
                   title = paste("Top ", toString(input$salesdash_sliderInput)," Sales by Product"))
        })
      }
    })
    
    
    ###################BOTTOM SALES###########################
    observeEvent(input$salesdash_selectInput,{
      if (input$salesdash_selectInput == "Bottom Sales"){
        output$salesdash_display <- renderPlotly({
          
          group_ds_price <- aggregate(Price ~ Product, data = ds(), sum)
          #head(group_ds_price,2)
          group_ds_price_order <- group_ds_price[order(-group_ds_price$Price),]
          #head(group_ds_price_order)
          group_ds_price_tail <- tail(group_ds_price_order,input$salesdash_sliderInput)
          plot_ly(data = group_ds_price_tail, 
                  x = ~reorder(Product,-Price), 
                  y = ~Price,
                  marker = list(color =c("red")), 
                  type = "bar") %>%
            layout(plot_bgcolor = "#000000",
                   paper_bgcolor = "#000000",
                   font = list(color = '#00FFFF'),
                   yaxis = list(title = "Sales"),
                   xaxis = list(title = "Products"),
                   title = paste("Bottom ", toString(input$salesdash_sliderInput)," Sales by Product"))
        })
      }
    })
    
    
    ###################TOP QUANTITIES###########################
    observeEvent(input$salesdash_selectInput,{
      if (input$salesdash_selectInput == "Top Quantities"){
        output$salesdash_display <- renderPlotly({
          
          group_ds_qty <- aggregate(QTY ~ Product, data = ds(), sum)
          group_ds_qty_order <- group_ds_qty[order(-group_ds_qty$QTY),]
          #head(group_ds_price_order)
          group_ds_qty_top <- head(group_ds_qty_order,input$salesdash_sliderInput)
          
          plot_ly(data = group_ds_qty_top, x = ~reorder(Product,-QTY), y = ~QTY, type = "bar") %>%
            layout(plot_bgcolor = "#000000",
                   paper_bgcolor = "#000000",
                   font = list(color = '#00FFFF'),
                   yaxis = list(title = "Total Quantities"),
                   xaxis = list(title = "Products"),
                   title = paste("Top ", toString(input$salesdash_sliderInput)," Most Dispensed Products"))
        })
      }
    })
    
    
    ###################BOTTOM QUANTITIES###########################
    observeEvent(input$salesdash_selectInput,{
      if (input$salesdash_selectInput == "Bottom Quantities"){
        output$salesdash_display <- renderPlotly({
          
          group_ds_qty <- aggregate(QTY ~ Product, data = ds(), sum)
          group_ds_qty_order <- group_ds_qty[order(-group_ds_qty$QTY),]
          #head(group_ds_price_order)
          group_ds_qty_bottom <- tail(group_ds_qty_order,input$salesdash_sliderInput)
          
          plot_ly(data = group_ds_qty_bottom, 
                  x = ~reorder(Product,-QTY), 
                  y = ~QTY, 
                  marker = list(color =c("red")),
                  type = "bar") %>%
            layout(plot_bgcolor = "#000000",
                   paper_bgcolor = "#000000",
                   font = list(color = '#00FFFF'),
                   yaxis = list(title = "Total Quantities"),
                   xaxis = list(title = "Products"),
                   title = paste(toString(input$salesdash_sliderInput)," Least Dispensed Products"))
        })
      }
    })
    
    
 
  })
}
    
## To be copied in the UI
# mod_POS_Sales_Dashboard_ui("POS_Sales_Dashboard_ui_1")
    
## To be copied in the server
# mod_POS_Sales_Dashboard_server("POS_Sales_Dashboard_ui_1")
