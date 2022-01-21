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
            
            div(id = "salesdash_dateRangeInput_div",
                dateRangeInput(inputId = ns("salesdash_dateRangeInput"),
                               label = "Select Range",
                               start = "2020-01-01",
                               end = "2022-01-01")),
            
            div(id = "salesdash_sql_table_input_div",
                uiOutput(ns("salesdash_sql_table_input"))),
            
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
                            value = 100))
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
    
    #################IMPORT DATABASE##########################
    sql_database <- load_db(db_name = "inst/app/www/pharma_database/Pharmacy_Database_Manager.db")

    all_sales_db_table <- open_db_table(db_name = "inst/app/www/pharma_database/Pharmacy_Database_Manager.db",
                               db_table = "All Sales")
    
    
    #sql_database <- reactive({
     # con <- dbConnect(RSQLite::SQLite(),dbname = "inst/app/www/pharma_database/Pharmacy_Database_Manager.db")
    #})
    
    #sql_table <- all_sales_dataframe(db_table = all_sales_db_table)
    sql_table <- reactive({
      sql_table <- all_sales_db_table
      
      ds1 <- sql_table %>%
        mutate(date_split = str_split(Timestamp, pattern = " ",simplify = TRUE)) %>%
        as.matrix() %>%
        as.data.frame() %>%
        mutate(split = str_split(date_split.1,pattern = "-",simplify = TRUE)) %>%
        mutate(date_split.1, Weekday = weekdays(ymd(date_split.1))) %>%
        as.matrix() %>%
        as.data.frame()%>%
        mutate(Month_Year = paste(split.2,split.1, sep = "-")) %>%
        mutate(Txt_Month_Year = as.Date(as.yearmon(paste(split.1,split.2, sep = "-")))) %>%
        mutate(split.2, Month = month.name[as.numeric(split.2)]) %>%
        mutate(Total1 = as.numeric(Total)) %>%
        mutate(QTY1 = as.numeric(QTY))
      
      names(ds1) <- c("Timestamp","Worker","Product","QTY_char","Price","Total_char","Date", 
                      "Time","Year","Month_num","Day","Weekday","Month_Year",   
                      "Txt_Month_Year","Month","Total","QTY")
      new_ds <- ds1 %>%
        select("Date", "Day", "Month", "Year", "Weekday","Txt_Month_Year", "Worker","Product","QTY", "Price", "Total", "Txt_Month_Year","Month")
      new_ds %>%
        subset(Date>=input$salesdash_dateRangeInput[1] & Date<=input$salesdash_dateRangeInput[2])
    })
    
    
    ######################PRODUCT VS PRICE###########################
    observeEvent(input$salesdash_selectInput,{
      if (input$salesdash_selectInput == "Sales vrs Product"){
        output$salesdash_display <- renderPlotly({
          group_ds_Total <- aggregate(Total ~ Product, data = sql_table(), sum)
          
          plot_ly(data = group_ds_Total, x = ~reorder(Product,-Total), y = ~Total, type = "bar") %>%
            layout(plot_bgcolor = "#000000",
                   paper_bgcolor = "#000000",
                   font = list(color = '#00FFFF'),
                   yaxis = list(title = "Sales"),
                   xaxis = list(title = "Products"),
                   title = "Sales by Product")
        })
      }
    })
    
    ###################QTY vs Total###########################
    observeEvent(input$salesdash_selectInput,{
      if (input$salesdash_selectInput == "Sales vrs Quantity"){
        output$salesdash_display <- renderPlotly({
          
          plot_ly(data = sql_table(), x = ~QTY, y = ~Total, type = "scatter", mode = "markers") %>%
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
          
          group_ds_Total <- aggregate(Total ~ Product, data = sql_table(), sum)
          #head(group_ds_Total,2)
          group_ds_Total_order <- group_ds_Total[order(-group_ds_Total$Total),]
          #head(group_ds_Total_order)
          group_ds_Total_300 <- head(group_ds_Total_order,input$salesdash_sliderInput)
          plot_ly(data = group_ds_Total_300, x = ~reorder(Product,-Total), y = ~Total, type = "bar") %>%
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
          
          group_ds_Total <- aggregate(Total ~ Product, data = sql_table(), sum)
          #head(group_ds_Total,2)
          group_ds_Total_order <- group_ds_Total[order(-group_ds_Total$Total),]
          #head(group_ds_Total_order)
          group_ds_Total_tail <- tail(group_ds_Total_order,input$salesdash_sliderInput)
          plot_ly(data = group_ds_Total_tail, 
                  x = ~reorder(Product,-Total), 
                  y = ~Total,
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
          
          group_ds_qty <- aggregate(QTY ~ Product, data = sql_table(), sum)
          group_ds_qty_order <- group_ds_qty[order(-group_ds_qty$QTY),]
          #head(group_ds_Total_order)
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
          
          group_ds_qty <- aggregate(QTY ~ Product, data = sql_table(), sum)
          group_ds_qty_order <- group_ds_qty[order(-group_ds_qty$QTY),]
          #head(group_ds_Total_order)
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
