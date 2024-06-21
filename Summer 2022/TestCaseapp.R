Sys.setlocale("LC_TIME", "English")
library(shiny)
library(shinydashboard)
library(dplyr)
library(highcharter)
library(shinyWidgets)
library(pivottabler)
library(tidyr)
library(janitor)

covid <- read.csv('TestData.csv',stringsAsFactors = F,header=T)
covid[["Date"]] <- lubridate::ymd(covid[["Date"]])
Month <- format(covid[["Date"]],'%B %Y')
Month <- factor(Month,levels=c("March 2022", "April 2022","May 2022"),ordered=TRUE)
covid$Month <- Month

testtypechoices = sort(unique(covid$Test),decreasing = F)
locationchoices = sort(unique(covid$Location),decreasing = F)

ui <- dashboardPage(
    
    # Dashboard Page Setup ----------------------------------------------------
    dashboardHeader(title = "Test Dashboard"),
    
    # Dashboard Sidebar -------------------------------------------------------
    dashboardSidebar(
        sidebarMenu(
            menuItem("Overview",tabName="Overview",icon=icon("list-alt")),
            menuItem("Breakdown",tabName="Breakdown",icon=icon("search",lib="glyphicon")),
            menuItem("Visit-us",icon=icon("send",lib="glyphicon"),href="https://www.ksldx.com/"))
    ),
    
    # Dashboard Body ----------------------------------------------------------
    dashboardBody(
        tabItems(
            # Overview -----------------------------------------------
            tabItem("Overview",
                    fluidRow(
                        valueBoxOutput("value1")),
                    fluidRow(
                        tabBox(
                            width = 12,
                            tabPanel(
                                title = "Test Number by Test Type",
                                highchartOutput("numbytype", height = "430px")),
                            tabPanel(
                                title = "Test Number by Date",
                                highchartOutput("numbydate", height = "430px")),
                            tabPanel(
                                title = "Test Number by Test Type and Date",
                                highchartOutput("numbytypedate", height = "430px"))   ))
            ),
            # BreaKdown ---------------------------------------------
            tabItem("Breakdown",
                    fluidRow(
                        box(pickerInput("Test", "Test Type", 
                                        testtypechoices, testtypechoices, 
                                        options = list(`actions-box` = TRUE),multiple = T)
                            ,width = 4),
                        box(pickerInput("Location", "Location", 
                                        locationchoices, locationchoices, 
                                        options = list(`actions-box` = TRUE, `live-search` = TRUE),multiple = T)
                            ,width = 4),
                        box(dateRangeInput("Date", strong("Date range"), 
                                           start = min(covid$Date), end = max(covid$Date),
                                           min = min(covid$Date), max = max(covid$Date))
                            ,width = 4)),
                    fluidRow(
                        tabBox(
                            width = 12,
                            tabPanel(
                                title = "Test Number Trend"
                                ,highchartOutput(outputId = "interactiveplot", height = "436px")),
                            tabPanel(
                                title = "Test Number Table (by Month)"
                                ,tableOutput("pvt"))  )))
        )
    )
)
server <- function(input, output) {
    
    #creating the valueBoxOutput content
    output$value1 <- renderValueBox({ 
        valueBox(
            formatC(length(covid$Test), format="d", big.mark=',')
            ,'Total Test Number'
            ,color = "navy")   
    })
    
    #creating the plotOutput content
    output$numbytype <- renderHighchart({
        covid %>% 
            count(Test) %>% 
            hchart('line', hcaes(x = 'Test', y = 'n'), name = "Count", dataLabels = list(enabled = TRUE)) %>%
            hc_colors(c("#3C8DBC")) %>%
            hc_yAxis(title = list(enabled=F))
    })
    output$numbydate <- renderHighchart({
        y = covid %>% 
            count(Date) 
        highchart(type = "stock") %>%
            hc_xAxis(type = 'datetime', labels = list(format = '{value:%b %d}')) %>%
            hc_add_series(y, "line", hcaes(x = Date, y = n), name = "Count") %>%
            hc_colors(c("#3C8DBC"))
    })
    output$numbytypedate <- renderHighchart({
        y = covid %>% 
            count(Date, Test)
        highchart(type = "stock") %>%
            hc_xAxis(type = 'datetime', labels = list(format = '{value:%b %d}')) %>%
            hc_add_series(y, "line", hcaes(x = 'Date', y = 'n', group = "Test")) %>%
            hc_colors(c("#7CB5EC", "#001F3F", "#0CAFFF", "#1D2E92", "#00FFFF", "#3C8DBC"))
        #hc_colors(c("#0CAFFF", "#001F3F", "#64A1F4", "#1D2E92", "#00FFFF", "#3C8DBC"))
    })
    
    # Create scatterplot object the plotOutput function is expecting
    output$interactiveplot <- renderHighchart({
        y = covid %>% 
            filter(Test %in% input$Test, 
                   Location %in% input$Location, 
                   Date >= input$Date[1] & Date <= input$Date[2]) %>%
            count(Date, Test)
        highchart(type = "stock") %>%
            hc_add_series(y, "column", hcaes(x = 'Date', y = 'n', group = Test), stacking = "normal") %>%
            hc_xAxis(type = 'datetime', labels = list(format = '{value:%b %Y}')) %>%
            hc_yAxis(stackLabels = list(enabled = TRUE, style = list(fontSize='7px'))) %>%
            hc_colors(c("#7CB5EC", "#001F3F", "#0CAFFF", "#1D2E92", "#00FFFF", "#3C8DBC")) %>%
            hc_tooltip(crosshairs = TRUE,borderWidth = 1,sort = TRUE,table = TRUE) %>%
            hc_legend(enabled=F)
    })
    output$pvt <- renderTable({
        data <- covid %>%
            count(Month, Test) %>%
            pivot_wider(names_from = Test,values_from = n,names_sort = T) %>%
            adorn_totals(c('row','col')) 
        data
    })
}

shinyApp(ui, server)
