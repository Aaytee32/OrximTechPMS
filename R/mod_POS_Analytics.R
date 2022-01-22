#' POS_Analytics UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList
#' @import shiny 
#' @importFrom stringr str_split
#' @import dplyr
#' @importFrom rlang .data
mod_POS_Analytics_ui <- function(id){
  ns <- NS(id)
  #tagList(
    div(id = "analytics_div",
        
        div(id = "analytics_inputs_div",
            
            div(id = "analytics_inputs_title_div",
                paste("Features")),
            
            div(id = "analytics_dateRangeInput_div",
                dateRangeInput(inputId = ns("analytics_dateRangeInput"),
                               label = "Select Range",
                               start = "2020-01-01",
                               end = "2022-01-01")),
            
            div(id = "analytics_sql_table_input_div",
                uiOutput(ns("analytics_sql_table_input"))),
            
            div(id = "analytics_selectInput_div",
                selectInput(inputId = ns("analytics_selectInput"),
                            label = "Choose Feature",
                            choices = c("Select",
                                        "Periodic Performance",
                                        "Employee Statistics",
                                        "Forecasting"
                                        ))),
            
            div(id = "analytics_selectInput_sub_div",
                uiOutput(ns("analytics_selectInput_sub")))#,
            
            #div(id = "analytics_sliderInput_div",
                #sliderInput(inputId = ns("analytics_sliderInput"),
                 #           label = "Summary",
                  #          min = 0,
                   #         max = 1500,
                    #        value = 100))
        ),
        div(id = "analytics_display_div",
            plotlyOutput(ns("analytics_display"))))
  #)
}
    
#' POS_Analytics Server Functions
#'
#' @noRd 
mod_POS_Analytics_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    #################IMPORT DATABASE##########################
    sql_database <- load_db(db_name = "C:/Users/Public/Documents/pharma_database/Pharmacy_Database_Manager.db")
    
    all_sales_db_table <- open_db_table(db_name = "C:/Users/Public/Documents/pharma_database/Pharmacy_Database_Manager.db",
                                        db_table = "All Sales")
    
    sql_table <- reactive({
      sql_table <- all_sales_db_table
      
      ds1 <- sql_table %>%
        dplyr::mutate(date_split = stringr::str_split(Timestamp, pattern = " ",simplify = TRUE)) %>%
        as.matrix() %>%
        as.data.frame() %>%
        dplyr::mutate(split = stringr::str_split(date_split.1,pattern = "-",simplify = TRUE)) %>%
        dplyr::mutate(date_split.1, Weekday = weekdays(lubridate::ymd(date_split.1))) %>%
        as.matrix() %>%
        as.data.frame()%>%
        dplyr::mutate(Month_Year = paste(split.2,split.1, sep = "-")) %>%
        dplyr::mutate(Txt_Month_Year = as.Date(zoo::as.yearmon(paste(split.1,split.2, sep = "-")))) %>%
        dplyr::mutate(split.2, Month = month.name[as.numeric(split.2)]) %>%
        dplyr::mutate(Total1 = as.numeric(Total)) %>%
        dplyr::mutate(QTY1 = as.numeric(QTY))
      
      names(ds1) <- c("Timestamp","Worker","Product","QTY_char","Price","Total_char","Date", 
                      "Time","Year","Month_num","Day","Weekday","Month_Year",   
                      "Txt_Month_Year","Month","Total","QTY")
      new_ds <- ds1 %>%
        select("Date", "Day", "Month", "Year", "Weekday","Txt_Month_Year", "Worker","Product","QTY", "Price", "Total", "Txt_Month_Year","Month")
      new_ds %>%
        subset(Date>=input$analytics_dateRangeInput[1] & Date<=input$analytics_dateRangeInput[2])
    })
    ###########################################################
    observeEvent(input$analytics_selectInput,{
      
      if (input$analytics_selectInput == "Select"){
        output$analytics_selectInput_sub <- renderUI({
          selectInput(inputId = ns("analytics_selectInput_sub"),
                      label = "Choose Filter",
                      choices = NULL)
        })
      }
    })
    
    observeEvent(input$analytics_selectInput,{
       
      if (input$analytics_selectInput == "Periodic Performance"){
        output$analytics_selectInput_sub <- renderUI({
          selectInput(inputId = ns("analytics_selectInput_sub"),
                      label = "Choose Filter",
                      choices = c("Select",
                                  "Sales by Year",
                                  "Sales by Month (Bar)",
                                  "Sales by Month (Line)",
                                  "Sales by Day",
                                  "Weekday Performance"))
        })
      }
    })
    
    observeEvent(input$analytics_selectInput,{
      if (input$analytics_selectInput == "Employee Statistics"){
        output$analytics_selectInput_sub <- renderUI({
          selectInput(inputId = ns("analytics_selectInput_sub"),
                      label = "Choose Filter",
                      choices = c("Select",
                                  "Bar Chart",
                                  "Pie Chart"))
        })
      }
    })
    
    ###################SALES BY YEAR###########################
    observeEvent(input$analytics_selectInput_sub,{
      
      if (input$analytics_selectInput_sub == "Sales by Year"){
        output$analytics_display <- renderPlotly({
          group_ds_year <- aggregate(Total~Year, data = sql_table(), sum)
          plot_ly(data = group_ds_year,
                  x = ~Year,
                  y = ~Total,
                  type = "bar") %>%
            layout(plot_bgcolor = "#000000",
                   paper_bgcolor = "#000000",
                   font = list(color = '#00FFFF'),
                   yaxis = list(title = "Sales"),
                   xaxis = list(title = "Year",
                                categoryorder = "array",
                                categoryarray = ~Year),
                   title = "Sales by Year")
        })
      }
    })
    
    ###################SALES BY MONTH (BAR)###########################
    observeEvent(input$analytics_selectInput_sub,{
      if (input$analytics_selectInput_sub == "Sales by Month (Bar)"){
        output$analytics_display <- renderPlotly({
          
          group_ds_month_bar <- aggregate(Total~Txt_Month_Year, data = sql_table(), sum)
          group_ds_month_bar %>%
            plot_ly(x = ~Txt_Month_Year,
                    y = ~Total,
                    type = "bar") %>%
            layout(plot_bgcolor = "#000000",
                   paper_bgcolor = "#000000",
                   font = list(color = '#00FFFF'),
                   yaxis = list(title = "Sales"),
                   xaxis = list(title = "Month"),
                   title = "Sales by Month")
        })
      }
    })
    
    ###################SALES BY MONTH (LINE)###########################
    observeEvent(input$analytics_selectInput_sub,{
      if (input$analytics_selectInput_sub == "Sales by Month (Line)"){
        output$analytics_display <- renderPlotly({
          
          group_ds_month_line <- aggregate(Total~Txt_Month_Year, data = sql_table(), sum)
          group_ds_month_line %>%
            plot_ly(x = ~Txt_Month_Year,
                    y = ~Total,
                    type = "scatter",
                    mode = "lines") %>%
            layout(plot_bgcolor = "#000000",
                   paper_bgcolor = "#000000",
                   font = list(color = '#00FFFF'),
                   yaxis = list(title = "Sales"),
                   xaxis = list(title = "Month"),
                   title = "Sales by Month")
        })
        
      }
    })
    
    ###################SALES BY DAY###########################
    observeEvent(input$analytics_selectInput_sub,{
      if (input$analytics_selectInput_sub == "Sales by Day"){
        output$analytics_display <- renderPlotly({
          ds_daily <- sql_table() %>%
            select(Date, Total) %>%
            mutate(ymd(Date))
          
          group_ds_daily <- aggregate(Total~ymd(Date), data = ds_daily, sum)
          #head(group_ds_daily)
          colnames(group_ds_daily) <- c("Date","dailySumTotal")
          plot_ly(data = group_ds_daily,
                  x = ~Date,
                  y = ~dailySumTotal,
                  type = 'scatter',
                  mode = "lines") %>%
            layout(plot_bgcolor = "#000000",
                   paper_bgcolor = "#000000",
                   font = list(color = '#00FFFF'),
                   yaxis = list(title = "Sales"),
                   xaxis = list(title = "Day",
                                categoryorder = "array",
                                categoryarray = ~Date),
                   title = "Daily Sales History")
        })
      }
    })
    
    ###################SALES BY WEEKDAY###########################
    observeEvent(input$analytics_selectInput_sub,{
      if (input$analytics_selectInput_sub == "Weekday Performance"){
        output$analytics_display <- renderPlotly({
          
          group_ds_weekday_bar <- aggregate(Total~Weekday, data = sql_table(), sum)
          
          plot_ly(data = group_ds_weekday_bar,
                  x = ~reorder(Weekday,-Total),
                  y = ~Total,
                  type = "bar") %>%
            layout(plot_bgcolor = "#000000",
                   paper_bgcolor = "#000000",
                   font = list(color = '#00FFFF'),
                   yaxis = list(title = "Sales"),
                   xaxis = list(title = "Day",
                                categoryorder = "array",
                                categoryarray = ~Weekday),
                   title = "Sales by Weekday")
        })
        
      }
    })
    
    ######################EMPLOYEE######################
    ds_worker <- reactive({
      ds2 <- sql_table() %>%
        select(Worker,Total) %>%
        #mutate(Split = str_split(Worker,pattern = "/",simplify = TRUE)) %>%
        #select(Split,Total) %>%
        as.matrix() %>%
        as.data.frame() %>%
        mutate(Total = as.numeric(Total))
      
      #names(ds2) <- c("A","B", "Worker", "D", "Total")
      ds_worker <- ds2 %>%
        select(Worker, Total)
      ds_worker
    })
    
    ######################EMPLOYEE BAR######################
    observeEvent(input$analytics_selectInput_sub,{
      if (input$analytics_selectInput_sub == "Bar Chart"){    
    output$analytics_display <- renderPlotly({
      group_ds_worker <- aggregate(Total~Worker, data = ds_worker(), sum)
      #group_ds_worker
      
      plot_ly(data = group_ds_worker,
              y = ~reorder(Worker,-Total),
              x = ~Total,
              type = "bar") %>%
        layout(plot_bgcolor = "#000000",
               paper_bgcolor = "#000000",
               font = list(color = '#00FFFF'),
               xaxis = list(title = "Sales"),
               yaxis = list(title = "Worker"),
               title = "Sales by Worker")
    })
      }
      })
    ######################EMPLOYEE PIE###################### 
    observeEvent(input$analytics_selectInput_sub,{
      if (input$analytics_selectInput_sub == "Pie Chart"){
        output$analytics_display <- renderPlotly({
          group_ds_worker <- aggregate(Total~Worker, data = ds_worker(), sum)
          #group_ds_worker
          
          plot_ly(data = group_ds_worker,
                  labels = ~reorder(Worker,-Total),
                  values = ~Total,
                  type = "pie",
                  hole = 0.4) %>%
            layout(plot_bgcolor = "#000000",
                   paper_bgcolor = "#000000",
                   font = list(color = '#00FFFF'),
                   title = "Sales by Worker")
        })
      }
      })
    
  })
}
    
## To be copied in the UI
# mod_POS_Analytics_ui("POS_Analytics_ui_1")
    
## To be copied in the server
# mod_POS_Analytics_server("POS_Analytics_ui_1")
