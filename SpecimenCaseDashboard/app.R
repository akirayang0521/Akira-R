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

data <- read.csv('SpecimenData.csv',stringsAsFactors = F,header=T)
data[["Specimen_ReceivedDate"]] <- lubridate::ymd(data[["Specimen_ReceivedDate"]])
Year <- format(data[["Specimen_ReceivedDate"]],"%Y")
Month <- format(data[["Specimen_ReceivedDate"]],"%b")
Month <- factor(Month,levels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"),ordered=TRUE)
data$Year <- Year
data$Month <- Month

sourcetype = sort(unique(data$Specimen_type),decreasing = F)
insurancetype = sort(unique(data$Billing_BillingUser2),decreasing = F)
year = sort(unique(data$Year),decreasing = F)

labcorp <- read.csv('LabCorpPriceList.csv',stringsAsFactors = F,header=T, check.name = F)
labcorp[["Receive Date"]] <- lubridate::ymd(labcorp[["Receive Date"]])
labcorp[["Receive Date"]] <- update(labcorp[["Receive Date"]], mday=01)
Yearlab <- format(labcorp[["Receive Date"]],"%Y")
Monthlab <- format(labcorp[["Receive Date"]],"%b")
Monthlab <- factor(Monthlab,levels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"),ordered=TRUE)
labcorp$Year <- Yearlab
labcorp$Month <- Monthlab

revenue <- read.csv('revenue.csv',stringsAsFactors = F,header=T, check.name = F)
revenue[["MONTH"]] <- factor(revenue[["MONTH"]],levels=c("JAN","FEB","MAR", "APR","MAY","JUN","JUL","AUG","SEP", "OCT","NOV","DEC"),ordered=TRUE)
revenue[["Category"]] <- factor(revenue[["Category"]],levels=c("Monthly Charges","Monthly Payments","YTD Payments","Direct Bill","Immco",'LabCorp'),ordered=TRUE)

revenuesum <- read.csv('revenuesum.csv',stringsAsFactors = F,header=T, check.name = F)
revenuesum[["MONTH"]] <- factor(revenuesum[["MONTH"]],levels=c("JAN","FEB","MAR", "APR","MAY","JUN","JUL","AUG","SEP", "OCT","NOV","DEC"),ordered=TRUE)
revenuesum[["Category"]] <- factor(revenuesum[["Category"]],levels=c("Monthly Charges","Monthly Payments","YTD Payments","Adjustment"),ordered=TRUE)

ui <- dashboardPage(
    
    # Dashboard Page Setup ----------------------------------------------------
    dashboardHeader(title = "Case Dashboard"),
    
    # Dashboard Sidebar -------------------------------------------------------
    dashboardSidebar(
        sidebarMenu(
            menuItem("2021-2022 Receive Summary",tabName="Source",icon=icon("search",lib="glyphicon")),
            menuItem("2021-2022 Practice Summary",tabName="Insurance",icon=icon("search",lib="glyphicon")),
            menuItem("2022 LabCorp Summary",tabName="Lab",icon=icon("search",lib="glyphicon")),
            menuItem("2019-2022 Monthly Payment",tabName="Payment",icon=icon("search",lib="glyphicon")),
            menuItem("TAT Averages",tabName="TAT",icon=icon("search",lib="glyphicon")))
    ),
    
    # Dashboard Body ----------------------------------------------------------
    dashboardBody(
        tabItems(
            tabItem("Source",
                    fluidRow(
                        valueBoxOutput("value1"),
                        box(pickerInput("year1", "Year", year,year, 
                                        options = list(`actions-box` = TRUE),multiple = T)
                            ,height = "103px",width = 4),
                        box(pickerInput("source", "Source Type", sourcetype, sourcetype, 
                                        options = list(`actions-box` = TRUE),multiple = T)
                            ,height = "103px",width = 4)),
                    fluidRow(
                        tabBox(
                            width = 12,
                            tabPanel(
                                title = "By Source",
                                highchartOutput("sourcebysource", height = "430px")),
                            tabPanel(
                                title = "By Month",
                                highchartOutput("sourcebymonth", height = "430px")),
                            tabPanel(
                                title = "Detail (Plot)",
                                fluidRow(column(6,highchartOutput("biopsy", height = "215px")),
                                         column(6,highchartOutput("skin", height = "215px")),
                                         column(6,highchartOutput("Mucosa", height = "215px")),
                                         column(6,highchartOutput("Conjunctiva", height = "215px")),
                                         column(6,highchartOutput("allbiopsy", height = "215px")),
                                         column(6,highchartOutput("allserum", height = "215px")) )),
                            tabPanel(
                                title = "Detail (Pivot Table)",
                                tableOutput("sourcepvt")) ))
            ),
            tabItem("Insurance",
                    fluidRow(
                        valueBoxOutput("value2"),
                        box(pickerInput("year2", "Year", year,year, 
                                        options = list(`actions-box` = TRUE),multiple = T)
                            ,height = "103px",width = 4),
                        box(pickerInput("insurance", "Billing Insurance Type", insurancetype, insurancetype, 
                                        options = list(`actions-box` = TRUE),multiple = T)
                            ,height = "103px",width = 4)),
                    fluidRow(
                        tabBox(
                            width = 12,
                            tabPanel(
                                title = "By Billing Insurance",
                                highchartOutput("insurancebyinsurance", height = "430px")),
                            tabPanel(
                                title = "By Month",
                                highchartOutput("insurancebymonth", height = "430px")),
                            tabPanel(
                                title = "Detail (Pivot Table)",
                                tableOutput("insurancepvt"))  ))
            ),
            tabItem("TAT",
                    fluidRow(
                        box(pickerInput("year3", "Year", year,year, 
                                        options = list(`actions-box` = TRUE),multiple = T)
                            ,height = "103px",width = 4),
                        box(pickerInput("source3", "Source Type", sourcetype, sourcetype, 
                                        options = list(`actions-box` = TRUE),multiple = T)
                            ,height = "103px",width = 4)                        ),
                    fluidRow(
                        tabBox(
                            width = 12,
                            tabPanel(
                                title = "Received",
                                highchartOutput("receive", height = "430px"),
                                tableOutput("receivetatpvt")),
                            tabPanel(
                                title = "Collected",
                                highchartOutput("collect", height = "430px"),
                                tableOutput("collecttatpvt"))))
            ),
            tabItem("Payment",
                    fluidRow(
                        tabBox(
                            width = 12,
                            tabPanel(
                                title = "Revenue Report",
                                div(tableOutput("revpvt"), style = "font-size:90%")),
                            tabPanel(
                                title = "Monthly Charges",
                                highchartOutput("charge", height = "400px"),
                                div(tableOutput("chargepvt"), style = "font-size:90%")),
                            tabPanel(
                                title = "Monthly Payments",
                                highchartOutput("payment", height = "400px"),
                                div(tableOutput("paymentpvt"), style = "font-size:90%")),
                            tabPanel(
                                title = "YDT Payments",
                                highchartOutput("ydt", height = "400px"),
                                div(tableOutput("ydtpvt"), style = "font-size:90%")),
                            tabPanel(
                                title = "MONTHLY REVENUE BY YEAR AND DEPARTMENT",
                                div(tableOutput("yearpvt"), style = "font-size:90%"))))
            ),
            tabItem("Lab",
                    fluidRow(
                        tabBox(
                            width = 12,
                            tabPanel(
                                title = "Test Code Report",
                                highchartOutput("lab", height = "400px"),
                                tableOutput("labpvt"))))
            )
        )
    )
)
server <- function(input, output) {
    
    output$value1 <- renderValueBox({ 
        data %>%
            filter(Specimen_type %in% input$source, Year %in% input$year1) %>% 
            summarise(tc = sum(F_replace_2)) %>%
            as.integer() %>% prettyNum(big.mark = ",") %>%
            valueBox(subtitle = "Total Case Number",color = "navy") 
    })
    output$value2 <- renderValueBox({ 
        data %>%
            filter(Billing_BillingUser2 %in% input$insurance, Year %in% input$year2) %>% 
            summarise(tc = sum(F_replace_2)) %>%
            as.integer() %>% prettyNum(big.mark = ",") %>%
            valueBox(subtitle = "Total Case Number",color = "navy") 
    })
    
    # 2021-2022 Receive Summary
    output$sourcebysource <- renderHighchart({
        data %>% 
            filter(Specimen_type %in% input$source, Year %in% input$year1) %>%
            group_by(Specimen_type,Year) %>% 
            summarise(tc = sum(F_replace_2), .groups = "drop") %>%
            hchart('column', hcaes(x = 'Specimen_type', y = 'tc', group = "Year"), 
                   dataLabels = list(enabled = TRUE)) %>%
            hc_yAxis(title = list(enabled=F), stackLabels = list(enabled = TRUE)) %>%
            hc_xAxis(title = list(enabled=F)) %>%
            hc_colors(c("#98C1D9","#001F3F")) %>%
            hc_tooltip(crosshairs = TRUE,borderWidth = 1,table = TRUE) %>%
            hc_legend(align = 'right',layout='vertical',verticalAlign='top') 
    })
    output$sourcebymonth <- renderHighchart({
        data %>% 
            filter(Specimen_type %in% input$source, Year %in% input$year1) %>%
            group_by(Month,Year) %>% 
            summarise(tc = sum(F_replace_2), .groups = "drop") %>%
            hchart('column', hcaes(x = 'Month', y = 'tc', group = "Year"), 
                   dataLabels = list(enabled = TRUE)) %>%
            hc_yAxis(title = list(enabled=F)) %>%
            hc_xAxis(title = list(enabled=F)) %>%
            hc_colors(c("#98C1D9","#001F3F")) %>%
            hc_tooltip(crosshairs = TRUE,borderWidth = 1,table = TRUE) %>%
            hc_legend(align = 'right',layout='vertical',verticalAlign='top') 
    })
    output$sourcepvt <- renderTable({
        y <- data %>%
            filter(Specimen_type %in% input$source, Year %in% input$year1) %>%
            group_by(Specimen_type,F_replace_1,Month,Year) %>% 
            summarise(tc = sum(F_replace_2), .groups = "drop") %>% 
            pivot_wider(names_from = Month,values_from = tc,names_sort = T) %>% 
            adorn_totals('col') %>%
            split(.[,"Year"]) %>% 
            map_df(.,adorn_totals)
        y
    })
    output$biopsy <- renderHighchart({
        data %>% 
            filter(F_replace_1=="Biopsy", Year %in% input$year1) %>%
            group_by(Month,Year) %>% 
            summarise(tc = sum(F_replace_2), .groups = "drop") %>%
            hchart('column', hcaes(x = 'Month', y = 'tc', group = "Year"), 
                   dataLabels = list(enabled = TRUE, style = list(fontSize='8px'))) %>%
            hc_title(text = "Biopsy", style = list(fontSize='12px')) %>%
            hc_yAxis(title = list(enabled=F)) %>%
            hc_xAxis(title = list(enabled=F)) %>%
            hc_colors(c("#98C1D9","#001F3F")) %>%
            hc_tooltip(crosshairs = TRUE,borderWidth = 1,table = TRUE) %>%
            hc_legend(enabled=F) 
    })
    output$skin <- renderHighchart({
        data %>% 
            filter(F_replace_1=="Biopsy, Skin", Year %in% input$year1) %>%
            group_by(Month,Year) %>% 
            summarise(tc = sum(F_replace_2), .groups = "drop") %>%
            hchart('column', hcaes(x = 'Month', y = 'tc', group = "Year"), 
                   dataLabels = list(enabled = TRUE, style = list(fontSize='8px'))) %>%
            hc_title(text = "Biopsy, Skin", style = list(fontSize='12px')) %>%
            hc_yAxis(title = list(enabled=F)) %>%
            hc_xAxis(title = list(enabled=F)) %>%
            hc_colors(c("#98C1D9","#001F3F")) %>%
            hc_tooltip(crosshairs = TRUE,borderWidth = 1,table = TRUE) %>%
            hc_legend(enabled=F) 
    })
    output$Mucosa <- renderHighchart({
        data %>% 
            filter(F_replace_1=="Biopsy, Mucosa", Year %in% input$year1) %>%
            group_by(Month,Year) %>% 
            summarise(tc = sum(F_replace_2), .groups = "drop") %>%
            hchart('column', hcaes(x = 'Month', y = 'tc', group = "Year"), 
                   dataLabels = list(enabled = TRUE, style = list(fontSize='8px'))) %>%
            hc_title(text = "Biopsy, Mucosa", style = list(fontSize='12px')) %>%
            hc_yAxis(title = list(enabled=F)) %>%
            hc_xAxis(title = list(enabled=F)) %>%
            hc_colors(c("#98C1D9","#001F3F")) %>%
            hc_tooltip(crosshairs = TRUE,borderWidth = 1,table = TRUE) %>%
            hc_legend(enabled=F) 
    })
    output$Conjunctiva <- renderHighchart({
        data %>% 
            filter(F_replace_1=="Biopsy, Conjunctiva", Year %in% input$year1) %>%
            group_by(Month,Year) %>% 
            summarise(tc = sum(F_replace_2), .groups = "drop") %>%
            hchart('column', hcaes(x = 'Month', y = 'tc', group = "Year"), 
                   dataLabels = list(enabled = TRUE, style = list(fontSize='8px'))) %>%
            hc_title(text = "Biopsy, Conjunctiva", style = list(fontSize='12px')) %>%
            hc_yAxis(title = list(enabled=F)) %>%
            hc_xAxis(title = list(enabled=F)) %>%
            hc_colors(c("#98C1D9","#001F3F")) %>%
            hc_tooltip(crosshairs = TRUE,borderWidth = 1,table = TRUE) %>%
            hc_legend(enabled=F)
    })
    output$allbiopsy <- renderHighchart({
        data %>% 
            filter(Specimen_type=="Biopsy", Year %in% input$year1) %>%
            group_by(Month,Year) %>% 
            summarise(tc = sum(F_replace_2), .groups = "drop") %>%
            hchart('column', hcaes(x = 'Month', y = 'tc', group = "Year"), 
                   dataLabels = list(enabled = TRUE, style = list(fontSize='8px'))) %>%
            hc_title(text = "All Biopsy", style = list(fontSize='12px')) %>%
            hc_yAxis(title = list(enabled=F)) %>%
            hc_xAxis(title = list(enabled=F)) %>%
            hc_colors(c("#98C1D9","#001F3F")) %>%
            hc_tooltip(crosshairs = TRUE,borderWidth = 1,table = TRUE) %>%
            hc_legend(enabled=F)
    })
    output$allserum <- renderHighchart({
        data %>% 
            filter(Specimen_type=="Serum", Year %in% input$year1) %>%
            group_by(Month,Year) %>% 
            summarise(tc = sum(F_replace_2), .groups = "drop") %>%
            hchart('column', hcaes(x = 'Month', y = 'tc', group = "Year"), 
                   dataLabels = list(enabled = TRUE, style = list(fontSize='8px'))) %>%
            hc_title(text = "All Serum", style = list(fontSize='12px')) %>%
            hc_yAxis(title = list(enabled=F)) %>%
            hc_xAxis(title = list(enabled=F)) %>%
            hc_colors(c("#98C1D9","#001F3F")) %>%
            hc_tooltip(crosshairs = TRUE,borderWidth = 1,table = TRUE) %>%
            hc_legend(enabled=F)
    })
    
    # 2021-2022 Practice Summary
    output$insurancebyinsurance <- renderHighchart({
        data %>% 
            filter(Billing_BillingUser2 %in% input$insurance, Year %in% input$year2) %>%
            group_by(Billing_BillingUser2,Year) %>% 
            summarise(tc = sum(F_replace_2), .groups = "drop") %>%
            hchart('column', hcaes(x = 'Billing_BillingUser2', y = 'tc', group = "Year"), 
                   dataLabels = list(enabled = TRUE)) %>%
            hc_yAxis(title = list(enabled=F), stackLabels = list(enabled = TRUE)) %>%
            hc_xAxis(title = list(enabled=F)) %>%
            hc_colors(c("#98C1D9","#001F3F")) %>%
            hc_tooltip(crosshairs = TRUE,borderWidth = 1,table = TRUE) %>%
            hc_legend(align = 'right',layout='vertical',verticalAlign='top') 
    })
    output$insurancebymonth <- renderHighchart({
        data %>% 
            filter(Billing_BillingUser2 %in% input$insurance, Year %in% input$year2) %>%
            group_by(Month,Year) %>% 
            summarise(tc = sum(F_replace_2), .groups = "drop") %>%
            hchart('column', hcaes(x = 'Month', y = 'tc', group = "Year"), 
                   dataLabels = list(enabled = TRUE)) %>%
            hc_yAxis(title = list(enabled=F), stackLabels = list(enabled = TRUE)) %>%
            hc_xAxis(title = list(enabled=F)) %>%
            hc_colors(c("#98C1D9","#001F3F")) %>%
            hc_tooltip(crosshairs = TRUE,borderWidth = 1,table = TRUE) %>%
            hc_legend(align = 'right',layout='vertical',verticalAlign='top') 
        # hc_legend(enabled=F)
    })
    output$insurancepvt <- renderTable({
        y <- data %>%
            filter(Billing_BillingUser2 %in% input$insurance, Year %in% input$year2) %>%
            group_by(Billing_BillingUser2,Billing_BillingUser1,Month,Year) %>% 
            summarise(tc = sum(F_replace_2), .groups = "drop") %>% 
            pivot_wider(names_from = Month,values_from = tc,names_sort = T) %>% 
            adorn_totals('col') %>%
            split(.[,"Year"]) %>% 
            map_df(.,adorn_totals)
        y
    })
    
    # 2022 LabCorp Summary
    output$lab <- renderHighchart({
        y = labcorp %>% 
            count(`Test Code`, `Receive Date`)
        highchart(type = "stock") %>%
            hc_add_series(y, "column", hcaes(x = `Receive Date`, y = n, group = `Test Code`), dataLabels = list(enabled = TRUE)) %>%
            hc_xAxis(type = 'datetime', labels = list(format = '{value:%b %Y}')) %>%
            hc_colors(c("#0000FF", "#0CAFFF", "#001F3F", "#64A1F4", "#1D2E92", "#00FFFF", "#3C8DBC"))
    })
    output$labpvt <- renderTable({
        y <- labcorp %>%
            count(`Test Code`, Year, Month) %>%
            pivot_wider(names_from = Month,values_from = n,names_sort = T) %>%
            adorn_totals('col') %>%
            split(.[,"Year"]) %>% 
            map_df(.,adorn_totals) 
        y
    })
    
    # 2019-2022 Monthly Payment
    output$revpvt <- renderTable({
        y <- revenue %>% 
            pivot_longer(cols = starts_with("20"), names_to = "Year", values_to = "value") %>% 
            group_by(MONTH,Category,Year) %>% 
            summarise(tc = sum(value), .groups = "drop") %>% 
            pivot_wider(names_from = MONTH,values_from = tc,names_sort = T) %>% 
            adorn_totals('col')
        y
    })
    output$charge <- renderHighchart({
        revenuesum %>% 
            filter(Category == "Monthly Charges") %>% 
            subset(select = -c(Category)) %>% 
            pivot_longer(!MONTH, names_to = "year", values_to = "value") %>%
            hchart("column", hcaes(x="MONTH", y="value", group = "year")) %>%
            hc_yAxis(title = list(enabled=F), stackLabels = list(enabled = TRUE)) %>%
            hc_xAxis(title = list(enabled=F)) %>%
            hc_colors(c("#3D5A80", "#98C1D9", "#E0FBFC","#EE6C4D")) %>%
            hc_tooltip(crosshairs = TRUE,borderWidth = 1,table = TRUE) %>%
            hc_legend(align = 'right',layout='vertical',verticalAlign='top')
    })
    output$chargepvt <- renderTable({
        y <- revenuesum %>% 
            filter(Category == "Monthly Charges") %>% 
            subset(select = -c(Category)) %>% 
            pivot_longer(cols = starts_with("20"), names_to = "Year", values_to = "value") %>% 
            group_by(MONTH,Year) %>% 
            summarise(tc = sum(value), .groups = "drop") %>% 
            pivot_wider(names_from = MONTH,values_from = tc,names_sort = T) %>% 
            adorn_totals(c('row','col'))
        y
    })
    output$payment <- renderHighchart({
        revenuesum %>% 
            filter(Category == "Monthly Payments") %>% 
            subset(select = -c(Category)) %>% 
            pivot_longer(!MONTH, names_to = "year", values_to = "value") %>%
            hchart("column", hcaes(x="MONTH", y="value", group = "year")) %>%
            hc_yAxis(title = list(enabled=F), stackLabels = list(enabled = TRUE)) %>%
            hc_xAxis(title = list(enabled=F)) %>%
            hc_colors(c("#3D5A80", "#98C1D9", "#E0FBFC","#EE6C4D")) %>%
            hc_tooltip(crosshairs = TRUE,borderWidth = 1,table = TRUE) %>%
            hc_legend(align = 'right',layout='vertical',verticalAlign='top')
    })
    output$paymentpvt <- renderTable({
        y <- revenuesum %>% 
            filter(Category == "Monthly Payments") %>% 
            subset(select = -c(Category)) %>% 
            pivot_longer(cols = starts_with("20"), names_to = "Year", values_to = "value") %>% 
            group_by(MONTH,Year) %>% 
            summarise(tc = sum(value), .groups = "drop") %>% 
            pivot_wider(names_from = MONTH,values_from = tc,names_sort = T) %>% 
            adorn_totals(c('row','col'))
        y
    })
    output$ydt <- renderHighchart({
        revenuesum %>% 
            filter(Category == "YTD Payments") %>% 
            subset(select = -c(Category)) %>% 
            pivot_longer(!MONTH, names_to = "year", values_to = "value") %>%
            hchart("column", hcaes(x="MONTH", y="value", group = "year")) %>%
            hc_yAxis(title = list(enabled=F), stackLabels = list(enabled = TRUE)) %>%
            hc_xAxis(title = list(enabled=F)) %>%
            hc_colors(c("#3D5A80", "#98C1D9", "#E0FBFC","#EE6C4D")) %>%
            hc_tooltip(crosshairs = TRUE,borderWidth = 1,table = TRUE) %>%
            hc_legend(align = 'right',layout='vertical',verticalAlign='top')
    })
    output$ydtpvt <- renderTable({
        y <- revenuesum %>% 
            filter(Category == "YTD Payments") %>% 
            subset(select = -c(Category)) %>% 
            pivot_longer(cols = starts_with("20"), names_to = "Year", values_to = "value") %>% 
            group_by(MONTH,Year) %>% 
            summarise(tc = sum(value), .groups = "drop") %>% 
            pivot_wider(names_from = MONTH,values_from = tc,names_sort = T) %>% 
            adorn_totals(c('row','col'))
        y
    })
    output$yearpvt <- renderTable({
        y <- revenuesum %>% 
            pivot_longer(cols = starts_with("20"), names_to = "Year", values_to = "value") %>% 
            group_by(MONTH,Category,Year) %>% 
            summarise(tc = sum(value), .groups = "drop") %>% 
            pivot_wider(names_from = MONTH,values_from = tc,names_sort = T) %>% 
            adorn_totals('col') %>% 
            arrange(desc(Year))
        y
    })
    
    # TAT Averages
    output$receive <- renderHighchart({
        data %>% 
            filter(Specimen_type %in% input$source3, Year %in% input$year3) %>%
            group_by(Month,Year) %>% 
            summarise(tc = mean(Received), .groups = "drop") %>%
            hchart('column', hcaes(x = 'Month', y = 'tc', group = "Year"), 
                   dataLabels = list(enabled = TRUE, format='{point.y:,.2f}')) %>%
            hc_yAxis(title = list(enabled=F)) %>%
            hc_xAxis(title = list(enabled=F)) %>%
            hc_colors(c("#98C1D9","#001F3F")) %>%
            hc_tooltip(crosshairs = TRUE,borderWidth = 1,table = TRUE) %>%
            hc_legend(align = 'right',layout='vertical',verticalAlign='top') 
    })
    output$receivetatpvt <- renderTable({
        y <- data %>%
            group_by(Specimen_type,Month,Year) %>% 
            summarise(re = mean(Received), .groups = "drop") %>%
            pivot_wider(names_from = Month,values_from = re,names_sort = T)
        y
    })
    output$collect <- renderHighchart({
        data %>% 
            filter(Specimen_type %in% input$source3, Year %in% input$year3) %>%
            group_by(Month,Year) %>% 
            summarise(tc = mean(Collected), .groups = "drop") %>%
            hchart('column', hcaes(x = 'Month', y = 'tc', group = "Year"), 
                   dataLabels = list(enabled = TRUE, format='{point.y:,.2f}')) %>%
            hc_yAxis(title = list(enabled=F),max=12) %>%
            hc_xAxis(title = list(enabled=F)) %>%
            hc_colors(c("#98C1D9","#001F3F")) %>%
            hc_tooltip(crosshairs = TRUE,borderWidth = 1,table = TRUE) %>%
            hc_legend(align = 'right',layout='vertical',verticalAlign='top') 
    })
    output$collecttatpvt <- renderTable({
        y <- data %>%
            group_by(Specimen_type,Month,Year) %>% 
            summarise(co = mean(Collected), .groups = "drop") %>%
            pivot_wider(names_from = Month,values_from = co,names_sort = T)
        y
    })
}

shinyApp(ui, server)
