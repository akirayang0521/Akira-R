Sys.setlocale("LC_TIME", "English")
library(shiny)
library(shinydashboard)
library(dplyr)
library(highcharter)
library(shinyWidgets)
library(pivottabler)
library(tidyr)
library(janitor)
library(purrr)

data = read.csv('CustomerData.csv', stringsAsFactors = F, header=T, check.name = F)
data[["Received Date"]] <- lubridate::ymd(data[["Received Date"]])
data[["Received Date"]] <- update(data[["Received Date"]], mday=01)
ryear <- format(data[["Received Date"]],"%Y")
YearR <- format(data[["Received Date"]],"%Y")
YearR <- factor(YearR,levels=sort(unique(YearR),decreasing = T))
data$YearR <- YearR
MonthR <- format(data[["Received Date"]],"%b")
MonthR <- factor(MonthR,levels=c("Jan","Feb","Mar", "Apr","May","Jun","Jul","Aug","Sep", "Oct","Nov","Dec"),ordered=TRUE)
data$MonthR <- MonthR

data[["Collection Date"]] <- lubridate::ymd(data[["Collection Date"]])
data[["Collection Date"]] <- update(data[["Collection Date"]], mday=01)
cyear <- format(data[["Collection Date"]],"%Y")
YearC <- format(data[["Collection Date"]],"%Y")
YearC <- factor(YearC,levels=sort(unique(YearC),decreasing = T))
data$YearC <- YearC
MonthC <- format(data[["Collection Date"]],"%b")
MonthC <- factor(MonthC,levels=c("Jan","Feb","Mar", "Apr","May","Jun","Jul","Aug","Sep", "Oct","Nov","Dec"),ordered=TRUE)
data$MonthC <- MonthC

type = sort(unique(data$Type),decreasing = F)
practice = sort(unique(data$Practice),decreasing = F)
yearr = sort(unique(ryear),decreasing = T)
yearc = sort(unique(cyear),decreasing = T)

ui <- dashboardPage(
    
    # Dashboard Page Setup ----------------------------------------------------
    dashboardHeader(title = "Payment Dashboard",titleWidth = 280),
    
    # Dashboard Sidebar -------------------------------------------------------
    dashboardSidebar(
        width = 280,
        sidebarMenu(
            menuItem("Case Summary by Received Date",tabName="case",icon=icon("inbox")),
            menuItem("Case Summary by Collection Date",tabName="caseC",icon=icon("inbox")),
            menuItem("Payment Summary by Received Date",tabName="payment",icon=icon("credit-card")),
            menuItem("Payment Summary by Collection Date",tabName="paymentC",icon=icon("credit-card")))
    ),
    
    # Dashboard Body ----------------------------------------------------------
    dashboardBody(
        tabItems(
            tabItem("case",
                    fluidRow(
                        valueBoxOutput("value1"),
                        box(pickerInput("year1", "Received Year", yearr, yearr, 
                                        options = list(`actions-box` = TRUE),multiple = T),
                            height = "103px",width = 4),
                        box(pickerInput("type1", "Type", type, type, 
                                        options = list(`actions-box` = TRUE),multiple = T),
                            height = "103px",width = 4)),
                    fluidRow(
                        box(title = "Case Summary by Year",status = "primary",solidHeader = TRUE,collapsible = TRUE
                            ,highchartOutput("cbyy"),width = 12),
                        box(title = "Case Summary by Month",status = "primary",solidHeader = TRUE,collapsible = TRUE
                            ,highchartOutput("cbymonth"),width = 12),
                        box(title = "Case Summary Table",status = "primary",solidHeader = TRUE,collapsible = TRUE
                            ,tableOutput("casepvt"),width = 12) ))
            ,
            tabItem("caseC",
                    fluidRow(
                        valueBoxOutput("value2"),
                        box(pickerInput("year2", "Collection Year", yearc, yearc, 
                                        options = list(`actions-box` = TRUE),multiple = T),
                            height = "103px",width = 4),
                        box(pickerInput("type2", "Type", type, type, 
                                        options = list(`actions-box` = TRUE),multiple = T),
                            height = "103px",width = 4)),
                    fluidRow(
                        box(title = "Case Summary by Year",status = "primary",solidHeader = TRUE,collapsible = TRUE
                            ,highchartOutput("cbyy1"),width = 12),
                        box(title = "Case Summary by Month",status = "primary",solidHeader = TRUE,collapsible = TRUE
                            ,highchartOutput("cbymonth1"),width = 12),
                        box(title = "Case Summary Table",status = "primary",solidHeader = TRUE,collapsible = TRUE
                            ,tableOutput("casepvt1"),width = 12) ))
            ,
            tabItem("payment",
                    fluidRow(
                        valueBoxOutput("value3"),
                        box(pickerInput("year3", "Received Year", yearr, yearr, 
                                        options = list(`actions-box` = TRUE),multiple = T),
                            height = "103px",width = 4),
                        box(pickerInput("practice1", "Practice", practice, practice, 
                                        options = list(`actions-box` = TRUE),multiple = T),
                            height = "103px",width = 4)),
                    fluidRow(
                        box(title = "Payment Summary by Year",status = "primary",solidHeader = TRUE,collapsible = TRUE
                            ,highchartOutput("pbyy"),width = 12),
                        box(title = "Payment Summary by Month",status = "primary",solidHeader = TRUE,collapsible = TRUE
                            ,highchartOutput("pbymonth"),width = 12),
                        box(title = "Payment Summary Table",status = "primary",solidHeader = TRUE,collapsible = TRUE
                            ,div(tableOutput("paypvt"), style = "font-size:90%"),width = 12) ))
            ,
            tabItem("paymentC",
                    fluidRow(
                        valueBoxOutput("value4"),
                        box(pickerInput("year4", "Collection Year", yearc, yearc, 
                                        options = list(`actions-box` = TRUE),multiple = T),
                            height = "103px",width = 4),
                        box(pickerInput("practice2", "Practice", practice, practice, 
                                        options = list(`actions-box` = TRUE),multiple = T),
                            height = "103px",width = 4)),
                    fluidRow(
                        box(title = "Payment Summary by Year",status = "primary",solidHeader = TRUE,collapsible = TRUE
                            ,highchartOutput("pbyy1"),width = 12),
                        box(title = "Payment Summary by Month",status = "primary",solidHeader = TRUE,collapsible = TRUE
                            ,highchartOutput("pbymonth1"),width = 12),
                        box(title = "Payment Summary Table",status = "primary",solidHeader = TRUE,collapsible = TRUE
                            ,div(tableOutput("paypvt1"), style = "font-size:90%"),width = 12) ))
        )
    )
)
server <- function(input, output) {
    
    #creating the valueBoxOutput content
    output$value1 <- renderValueBox({ 
        data %>%
            filter(YearR %in% input$year1) %>% 
            summarise(tc = length(unique(`Case Number`))) %>%
            as.integer() %>% prettyNum(big.mark = ",") %>%
            valueBox(subtitle = "Total Cases",color = "navy") 
    })
    output$value2 <- renderValueBox({ 
        data %>%
            filter(YearC %in% input$year2) %>% 
            summarise(tc = length(unique(`Case Number`))) %>%
            as.integer() %>% prettyNum(big.mark = ",") %>%
            valueBox(subtitle = "Total Cases",color = "navy")
    })
    output$value3 <- renderValueBox({ 
        data %>%
            filter(YearR %in% input$year3) %>% 
            summarise(tc = sum(Charge)) %>%
            as.integer() %>% prettyNum(big.mark = ",") %>%
            valueBox(subtitle = "Total Charge",color = "navy")
    })
    output$value4 <- renderValueBox({ 
        data %>%
            filter(YearC %in% input$year4) %>% 
            summarise(tc = sum(Charge)) %>%
            as.integer() %>% prettyNum(big.mark = ",") %>%
            valueBox(subtitle = "Total Charge",color = "navy")
    })
    
    # Case Summary by Received Date
    output$cbyy <- renderHighchart({
        data %>% 
            filter(YearR %in% input$year1, Type %in% input$type1) %>%
            group_by(YearR, Type) %>% 
            summarise(tc = length(unique(`Case Number`)), .groups = "drop") %>%
            hchart('column', hcaes(x = 'YearR', y = 'tc',group = 'Type'), dataLabels = list(enabled = TRUE)) %>%
            hc_colors(c("#001F3F", "#0CAFFF", "#7CB5EC", "#1D2E92")) %>%
            hc_tooltip(crosshairs = TRUE,borderWidth = 1,sort = TRUE,table = TRUE) %>%
            hc_yAxis(title = list(enabled=F)) %>%
            hc_xAxis(title = list(enabled=F)) %>%
            hc_legend(align = 'right',layout='vertical',verticalAlign='top')
    })
    output$cbymonth <- renderHighchart({
        y = data %>% 
            filter(YearR %in% input$year1, Type %in% input$type1) %>%
            group_by(`Received Date`, Type) %>% 
            summarise(tc = length(unique(`Case Number`)), .groups = "drop")
        highchart(type = "stock") %>%
            hc_add_series(y, "column", hcaes(x = `Received Date`, y = tc, group = Type), stacking = "normal") %>%
            hc_xAxis(type = 'datetime', labels = list(format = '{value:%b}')) %>%
            hc_yAxis(stackLabels = list(enabled = TRUE)) %>%
            hc_colors(c("#001F3F", "#0CAFFF", "#7CB5EC", "#1D2E92")) %>%
            hc_tooltip(crosshairs = TRUE,borderWidth = 1,sort = TRUE,table = TRUE) %>%
            hc_legend(enabled=F)
    })
    output$casepvt <- renderTable({
        y <- data %>%
            group_by(YearR, MonthR, Type) %>% 
            summarise(tc = length(unique(`Case Number`)), .groups = "drop") %>% 
            pivot_wider(names_from = MonthR,values_from = tc,names_sort = T) %>% 
            adorn_totals('col') %>%
            split(.[,"YearR"]) %>% 
            map_df(.,adorn_totals)
        y
    })
    
    # Case Summary by Collection Date
    output$cbyy1 <- renderHighchart({
        data %>% 
            filter(YearC %in% input$year2, Type %in% input$type2) %>%
            group_by(YearC, Type) %>% 
            summarise(tc = length(unique(`Case Number`)), .groups = "drop") %>%
            hchart('column', hcaes(x = 'YearC', y = 'tc',group = 'Type'), dataLabels = list(enabled = TRUE)) %>%
            hc_colors(c("#001F3F", "#0CAFFF", "#7CB5EC", "#1D2E92")) %>%
            hc_tooltip(crosshairs = TRUE,borderWidth = 1,sort = TRUE,table = TRUE) %>%
            hc_yAxis(title = list(enabled=F)) %>%
            hc_xAxis(title = list(enabled=F)) %>%
            hc_legend(align = 'right',layout='vertical',verticalAlign='top')
    })
    output$cbymonth1 <- renderHighchart({
        y = data %>% 
            filter(YearC %in% input$year2, Type %in% input$type2) %>%
            group_by(`Collection Date`, Type) %>% 
            summarise(tc = length(unique(`Case Number`)), .groups = "drop")
        highchart(type = "stock") %>%
            hc_add_series(y, "column", hcaes(x = `Collection Date`, y = tc, group = Type), stacking = "normal") %>%
            hc_xAxis(type = 'datetime', labels = list(format = '{value:%b}')) %>%
            hc_yAxis(stackLabels = list(enabled = TRUE)) %>%
            hc_colors(c("#001F3F", "#0CAFFF", "#7CB5EC", "#1D2E92")) %>%
            hc_tooltip(crosshairs = TRUE,borderWidth = 1,sort = TRUE,table = TRUE) %>%
            hc_legend(enabled=F)
    })
    output$casepvt1 <- renderTable({
        y <- data %>%
            group_by(YearC, MonthC, Type) %>% 
            summarise(tc = length(unique(`Case Number`)), .groups = "drop") %>% 
            pivot_wider(names_from = MonthC,values_from = tc,names_sort = T) %>% 
            adorn_totals('col') %>%
            split(.[,"YearC"]) %>% 
            map_df(.,adorn_totals)
        y
    })
    
    # Payment Summary by Received Date
    output$pbyy <- renderHighchart({
        data %>% 
            filter(YearR %in% input$year3, Practice %in% input$practice1) %>%
            group_by(YearR, Practice) %>% 
            summarise(tp = sum(Charge), .groups = "drop") %>%
            hchart('column', hcaes(x = 'YearR', y = 'tp',group = 'Practice'), dataLabels = list(enabled = TRUE,format='{point.y:,.0f}')) %>%
            hc_colors(c("#0CAFFF", "#7CB5EC", "#001F3F","#1D2E92")) %>%
            hc_tooltip(crosshairs = TRUE,borderWidth = 1,sort = TRUE,table = TRUE) %>%
            hc_yAxis(title = list(enabled=F)) %>%
            hc_xAxis(title = list(enabled=F)) %>%
            hc_legend(align = 'right',layout='vertical',verticalAlign='top')
    })
    output$pbymonth <- renderHighchart({
        y = data %>% 
            filter(YearR %in% input$year3, Practice %in% input$practice1) %>%
            group_by(`Received Date`, Practice) %>% 
            summarise(tp = sum(Charge), .groups = "drop")
        highchart(type = "stock") %>%
            hc_add_series(y, "column", hcaes(x = `Received Date`, y = tp, group = Practice), stacking = "normal") %>%
            hc_xAxis(type = 'datetime', labels = list(format = '{value:%b}')) %>%
            hc_yAxis(stackLabels = list(enabled = TRUE, style = list(fontSize='8px'))) %>%
            hc_colors(c("#0CAFFF", "#7CB5EC", "#001F3F","#1D2E92")) %>%
            hc_tooltip(crosshairs = TRUE,borderWidth = 1,sort = TRUE,table = TRUE) %>%
            hc_legend(enabled=F)
    })
    output$paypvt <- renderTable({
        y <- data %>%
            group_by(YearR, MonthR, Practice) %>% 
            summarise(tc = sum(Charge), .groups = "drop") %>% 
            pivot_wider(names_from = MonthR,values_from = tc,names_sort = T) %>% 
            adorn_totals('col') %>%
            split(.[,"YearR"]) %>% 
            map_df(.,adorn_totals)
        y
    })
    
    # Payment Summary by Collection Date
    output$pbyy1 <- renderHighchart({
        data %>% 
            filter(YearC %in% input$year4, Practice %in% input$practice2) %>%
            group_by(YearC, Practice) %>% 
            summarise(tp = sum(Charge), .groups = "drop") %>%
            hchart('column', hcaes(x = 'YearC', y = 'tp',group = 'Practice'), dataLabels = list(enabled = TRUE,format='{point.y:,.0f}')) %>%
            hc_colors(c("#0CAFFF", "#7CB5EC", "#001F3F","#1D2E92")) %>%
            hc_tooltip(crosshairs = TRUE,borderWidth = 1,sort = TRUE,table = TRUE) %>%
            hc_yAxis(title = list(enabled=F)) %>%
            hc_xAxis(title = list(enabled=F)) %>%
            hc_legend(align = 'right',layout='vertical',verticalAlign='top')
    })
    output$pbymonth1 <- renderHighchart({
        y = data %>% 
            filter(YearC %in% input$year4, Practice %in% input$practice2) %>%
            group_by(`Collection Date`, Practice) %>% 
            summarise(tp = sum(Charge), .groups = "drop")
        highchart(type = "stock") %>%
            hc_add_series(y, "column", hcaes(x = `Collection Date`, y = tp, group = Practice), stacking = "normal") %>%
            hc_xAxis(type = 'datetime', labels = list(format = '{value:%b}')) %>%
            hc_yAxis(stackLabels = list(enabled = TRUE, style = list(fontSize='8px'))) %>%
            hc_colors(c("#0CAFFF", "#7CB5EC", "#001F3F","#1D2E92")) %>%
            hc_tooltip(crosshairs = TRUE,borderWidth = 1,sort = TRUE,table = TRUE) %>%
            hc_legend(enabled=F)
    })
    output$paypvt1 <- renderTable({
        y <- data %>%
            group_by(YearC, MonthC, Practice) %>% 
            summarise(tc = sum(Charge), .groups = "drop") %>% 
            pivot_wider(names_from = MonthC,values_from = tc,names_sort = T) %>%
            adorn_totals('col') %>%
            split(.[,"YearC"]) %>% 
            map_df(.,adorn_totals)
        y
    })
}

shinyApp(ui, server)
