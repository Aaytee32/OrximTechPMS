#' POS_Analytics UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList
#' @import stringr
#' @import zoo
#' @import lubridate
#' @import reshape2
#' @import dplyr
mod_POS_Analytics_ui <- function(id){
  ns <- NS(id)
  #tagList(
    div(id = "analytics_div",
        
        div(id = "analytics_inputs_div",
            
            div(id = "analytics_inputs_title_div",
                paste("Features")),
            
            div(id = "analytics_selectInput_div",
                selectInput(inputId = ns("analytics_selectInput"),
                            label = "Choose Feature",
                            choices = c("Select",
                                        "Periodic Performance",
                                        "Employee Statistics",
                                        "Forecasting"
                                        ))),
            
            div(id = "analytics_selectInput_sub_div",
                uiOutput(ns("analytics_selectInput_sub"))),
            
            div(id = "analytics_sliderInput_div",
                sliderInput(inputId = ns("analytics_sliderInput"),
                            label = "Summary",
                            min = 0,
                            max = 1500,
                            value = 100)),
            
            div(id = "analytics_dateRangeInput_div",
                dateRangeInput(inputId = ns("analytics_dateRangeInput"),
                               label = "Select Range",
                               start = "2020-01-01",
                               end = "2022-01-01"))
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
    
    ds <- reactive({
      ds <- read_xlsx("C:\\Users\\aoppong\\OneDrive\\Manuscript3\\PharmaDataAnalysis\\sample_pharma_data.xlsx")
      as.data.frame(ds)
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
    
    
    #############################################################
    new_ds <- reactive({
      
      ds1 <- ds() %>%
        select(Date,Price) %>%
        mutate(split = str_split(Date,pattern = "/",simplify = TRUE)) %>%
        select(Date,split,Price) %>%
        mutate(Date, Weekday = weekdays(mdy(Date))) %>%
        as.matrix() %>%
        as.data.frame() %>%
        mutate(Month_Year = paste(split.3,split.1, sep = "-")) %>%
        mutate(Txt_Month_Year = as.Date(as.yearmon(paste(split.3,split.1, sep = "-")))) %>%
        mutate(split.1, Month = month.name[as.numeric(split.1)]) %>%
        mutate(Price1 = as.numeric(Price))
      
      names(ds1) <- c("Date", "Month_Num", "Day", "Year", "Price_Str", "Weekday", "Month_Year","Txt_Month_Year", "Month", "Price")
      new_ds <- ds1 %>%
        select("Date", "Month", "Year", "Weekday","Txt_Month_Year", "Price")
      new_ds
      
    })
    
    ###################SALES BY YEAR###########################
    observeEvent(input$analytics_selectInput_sub,{
      
      if (input$analytics_selectInput_sub == "Sales by Year"){
        output$analytics_display <- renderPlotly({
          group_ds_year <- aggregate(Price~Year, data = new_ds(), sum)
          plot_ly(data = group_ds_year,
                  x = ~Year,
                  y = ~Price,
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
          
          group_ds_month_bar <- aggregate(Price~Txt_Month_Year, data = new_ds(), sum)
          group_ds_month_bar %>%
            plot_ly(x = ~Txt_Month_Year,
                    y = ~Price,
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
          
          group_ds_month_line <- aggregate(Price~Txt_Month_Year, data = new_ds(), sum)
          group_ds_month_line %>%
            plot_ly(x = ~Txt_Month_Year,
                    y = ~Price,
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
          ds_daily <- ds() %>%
            select(Date, Price) %>%
            mutate(mdy(Date))
          
          group_ds_daily <- aggregate(Price~mdy(Date), data = ds_daily, sum)
          #head(group_ds_daily)
          colnames(group_ds_daily) <- c("Date","dailySumPrice")
          plot_ly(data = group_ds_daily,
                  x = ~Date,
                  y = ~dailySumPrice,
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
          
          group_ds_weekday_bar <- aggregate(Price~Weekday, data = new_ds(), sum)
          
          plot_ly(data = group_ds_weekday_bar,
                  x = ~reorder(Weekday,-Price),
                  y = ~Price,
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
      ds2 <- ds() %>%
        select(Worker,Price) %>%
        mutate(Split = str_split(Worker,pattern = "/",simplify = TRUE)) %>%
        select(Split,Price) %>%
        as.matrix() %>%
        as.data.frame() %>%
        mutate(Price = as.numeric(Price))
      
      names(ds2) <- c("A","B", "Worker", "D", "Price")
      ds_worker <- ds2 %>%
        select(Worker, Price)
      ds_worker
    })
    
    ######################EMPLOYEE BAR######################
    observeEvent(input$analytics_selectInput_sub,{
      if (input$analytics_selectInput_sub == "Bar Chart"){    
    output$analytics_display <- renderPlotly({
      group_ds_worker <- aggregate(Price~Worker, data = ds_worker(), sum)
      #group_ds_worker
      
      plot_ly(data = group_ds_worker,
              y = ~reorder(Worker,-Price),
              x = ~Price,
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
          group_ds_worker <- aggregate(Price~Worker, data = ds_worker(), sum)
          #group_ds_worker
          
          plot_ly(data = group_ds_worker,
                  labels = ~reorder(Worker,-Price),
                  values = ~Price,
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
