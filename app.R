library(shiny)
library(shinydashboard)

aggregate_data_oecd <- aggregate(oecd$Value, by=list(LOCATION=oecd$LOCATION), FUN=sum)
aggregate_data_oecd_greece <- aggregate_data_oecd[aggregate_data_oecd$LOCATION=="GRC",]$x
sum_aggregate_data_oecd <- sum(aggregate_data_oecd$x)
aggregate_data_oecd_greece_ltu <- aggregate_data_oecd[aggregate_data_oecd$x==max(aggregate_data_oecd$x),]$x

ui <- dashboardPage(skin = "red",
  dashboardHeader(
      title = "Suicides in Europe",
      titleWidth = 250
  ),

  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Raw data", tabName = "rawdata", icon = icon("table"))
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem("dashboard",
        fluidRow(
            valueBoxOutput("aggregateSuicidesEuropeValueBox"),
            valueBoxOutput("aggregateSuicidesLithuaniaValueBox"),
            valueBoxOutput("aggregateSuicidesGreeceValueBox")
        )
      ),
      tabItem("rawdata",
        # numericInput("maxrows", "Rows to show", 25),
        # verbatimTextOutput("rawtable"),
        # downloadButton("downloadCsv", "Download as CSV")
      )
    )
  )

)

server <- function(input, output, session) {

    output$aggregateSuicidesEuropeValueBox <- renderValueBox({
        valueBox(
            value = sum_aggregate_data_oecd,
            subtitle = "Total Suicides in Europe (per 1000000 person) 2004-2014",
            icon = icon("skull-crossbones"),
            color = "red"
        )
    })

    output$aggregateSuicidesLithuaniaValueBox <- renderValueBox({
        valueBox(
            value = aggregate_data_oecd_greece_ltu,
            subtitle = "Lithuania - Country with most suicides in Europe (per 1000000 person) 2004-2014",
            icon = icon("syringe"),
            color = "orange"
        )
    })

    output$aggregateSuicidesGreeceValueBox <- renderValueBox({
        valueBox(
            value = aggregate_data_oecd_greece,
            subtitle = "Total Suicides in Greece (per 1000000 person) 2004-2014",
            icon = icon("capsules"),
            color = "aqua"
        )
    })
}

shinyApp(ui = ui, server = server)

