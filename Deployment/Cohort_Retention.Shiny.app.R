# RShiny App: Cohort Retention Analyzer 

# LIBRARIES ----
library(shiny)
library(shinyWidgets)
library(bslib)
library(crosstalk)
library(shinycssloaders)
 
# Core
library(tidyverse)
library(tidyquant)

# Visual + Others
library(plotly)
library(timetk)
library(glue)
library(rlang)

setwd("/Users/seunghyunsung/Documents/GitHub/Retail-Customer-Analytics")
# DATA SETUP ----
trans_invoice_tbl <- read_rds("00_Data/data_wranggled/trans_data/trans_invoice_tbl.rds")
trans_cust_tbl <- read_rds("00_Data/data_wranggled/trans_data/trans_cust_tbl.rds")

source("00_Script/select_period_n_cohort_n_statusIII.R")

periods <- c("month", "quarter")

# Period List (Input User)
period_list_tbl <- tibble(period = periods) 


# depending on the UI(period) selected ---> list for UI(cohorts_label) 
period_cohort_map_tbl  <- period_list_tbl %>% 
  mutate(cohort_label = map(period, generator_cohort_list_mapper))


ui <- fluidPage(
  # footer
  footer = tagList(
    "Made by Seung Hyun Sung"
  ),
  # Theme
  theme = bs_theme(version = 4, bootswatch = 'lux'),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  # Header
  div(
    h1("Cohort Analysis", "by Model Bakery"),
    p("Cohort analysis allows you to dig into the behaviour of customers of an online retailer.")
  ),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      h1("Retail Customer Cohort Heatmap"),
      h3("by Model Bakery") %>% tags$a(href = 'https://www.business-science.io/', target="_blank"),
      p("Cohort analysis allows you to segment customers based on when they first bought from the retailer, and see how many customers based on when they first bought from the retailer, and see how many customers remained after a set number of months.
         For each cohort, you can see how many remained over time, and what was the average spend per transcation for those remaining customers."),
      br(),
      hr(),
      h3("Period Controls"),
      wellPanel(
        pickerInput(
          inputId = "period_selection", 
          label = "Period List (Pick One to Analyse)",
          choices = periods,
          multiple = FALSE,
          selected = "month",
          options = pickerOptions(
            actionsBox = FALSE
          )
        ),
        actionButton(inputId = "analyse", label = "Analyse", icon = icon("download")),
        verbatimTextOutput(outputId = "selected_period")
      ),
      h3("Cohort Controls"),
      wellPanel(
        pickerInput(
          inputId = "period_selection1", 
          label = "Period List (Pick One to Analyse)",
          choices = NULL,
          multiple = FALSE,
          options = pickerOptions(
            liveSearch = TRUE,
            actionsBox = FALSE,
            size = 5
          )
        ),
        actionButton(inputId = "analyse1", label = "Analyse", icon = icon("download"))
      ),
      hr(),
      p("Customer retention measures show how many new customers bought per month, as well as number of returning customers (customers who bought both last month and this month) and the recovered customers (customers who had a break from buying but have returned for another purchase)."),
      p("Spend analysis shows the total and average spend transaction vary over time. Average distribution shows a histogram of customer and their average monthly spend amounts.")
      
      ),
    mainPanel(
      width = 9,
      radioGroupButtons(
        inputId = "cat",
        label = NULL,
        choices = c("Active Customers", 
                    "Customers Retented",
                    "Average Spend"
        ),
        justified = TRUE
      ),
      hr(style="margin-top:2px !important;"),
      uiOutput("mainP")
      
      )
    
  )

  
  
)

server <- function(input, output, session) {
  
  r<-reactiveValues(per="month",per1="all")
  
  observeEvent(input$analyse,{
    r$per<-input$period_selection
    r$per1<-input$period_selection1
  })
  
  observeEvent(input$analyse1,{
    r$per<-input$period_selection
    r$per1<-input$period_selection1
  })
  
  observeEvent(input$period_selection,{
    req(input$period_selection)
    cohort_list <- period_cohort_map_tbl %>% 
      filter(period == input$period_selection) %>% 
      select(cohort_label) %>% unnest(cols = cohort_label)
    
    updatePickerInput(session = session,inputId = 'period_selection1',choices = cohort_list)
  })
  
  
  
  output$mainP<-renderUI({
    tagList(
    h1("Retail Customer Cohort Heatmap ", tags$small(paste("BY",str_to_upper(input$cat)))),
    plotlyOutput("plot1") %>%
      withSpinner(),
    h1("Customer Retention Status"),
    fluidRow(
      column(6,
             plotlyOutput('plot2',height = '500px')%>%
               withSpinner(type = 7)
             ),
      column(6,
             plotlyOutput('plot3',height = '500px')%>%
               withSpinner(type = 7)
             )
    ),
    hr(),
    fluidRow(
      column(6,
             plotlyOutput('plot4',height = '500px')%>%
               withSpinner(type = 7)
             ),
      column(6,
             plotlyOutput('plot5',height = '500px')%>%
               withSpinner(type = 7)
             )
    ),
    br(),
    br()

    )
  })
  
  output$plot1<-renderPlotly({
    req(input$cat)
    g<-switch (input$cat,
            "Active Customers" = {trans_invoice_tbl %>% abstract_by_period(period = r$per) %>% get_cohort_heatmap() %>% plot_heatmap(measure_var = active_users)},
            "Customers Retented" = {trans_invoice_tbl %>% abstract_by_period(period = r$per) %>% get_cohort_heatmap() %>% plot_heatmap(measure_var = retention)},
            "Average Spend" = {trans_invoice_tbl %>% abstract_by_period(period = r$per) %>% get_cohort_heatmap() %>% plot_heatmap(measure_var = avg_spend)}
    )%>%
      layout(
             yaxis = list(title = 'Cohort Group', titlefont = list(size = 12) ),
             xaxis = list(title = paste("k-th",r$per), titlefont = list(size = 12)),
             showlegend=FALSE
      )%>%
      config(displayModeBar = F)
  })
  
  output$plot2<-renderPlotly({
    trans_invoice_tbl %>% abstract_by_period(period = r$per) %>% abstract_by_cohort(cohort_label = r$per1) %>% get_retention_by_status() %>% plot_retention_status()%>%
      layout(legend = list(orientation = 'h',x = -0.2, y = 1.3),
             title = list(y = 0.85),
             yaxis = list(title = 'No. Customers', titlefont = list(size = 12) ),
             xaxis = list(title = paste("Invoice by",r$per), titlefont = list(size = 12))
             )%>%
      config(displayModeBar = F)
  })
  output$plot3<-renderPlotly({
    trans_invoice_tbl %>% abstract_by_period(period = r$per) %>% abstract_by_cohort(cohort_label = r$per1) %>% get_retention_by_status() %>% plot_retention_breakdown()%>%
      layout(legend = list(orientation = 'h',x = -0.2, y = 1.3),
             title = list(y = 0.85),
             yaxis = list(title = 'No. Customers', titlefont = list(size = 12) ),
             xaxis = list(title = paste("Invoice by",r$per), titlefont = list(size = 12))
             )%>%
      config(displayModeBar = F)
  })
  output$plot4<-renderPlotly({
    trans_invoice_tbl %>% abstract_by_period(period = r$per) %>% abstract_by_cohort(cohort_label = r$per1) %>% plot_spending_trend()%>%
      config(displayModeBar = F)%>%
      layout(
        legend = list(orientation = 'h',x = -0.2, y = 1.3),
        yaxis = list(title = 'Total Spend', titlefont = list(size = 12) ),
        xaxis = list(title = paste("Invoice by",r$per), titlefont = list(size = 12))
      )
  })
  output$plot5<-renderPlotly({
    trans_cust_tbl %>% abstract_cust_spend_by_period(r$per) %>% abstract_by_cohort(cohort_label = r$per1) %>% plot_spending_distribution()%>%
      config(displayModeBar = F)%>%
      layout(
        legend = list(orientation = 'h',x = -0.2, y = 1.3),
        yaxis = list(title = 'No. Customers', titlefont = list(size = 12) ),
        xaxis = list(title = paste("Average",paste0(r$per,"ly"),"Spend"), titlefont = list(size = 12))
      )
  })

}

shinyApp(ui, server,options = list(port=1212))