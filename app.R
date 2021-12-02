library(shiny)
library(shinydashboard)
library("readxl")
library(DT)


oecd<- read.csv("suicide II OECD.csv.txt", header = TRUE)
eurostat <- read_excel("suicide1.xls",col_names =TRUE, skip=2)


aggregate_data_oecd <- aggregate(oecd$Value, by=list(LOCATION=oecd$LOCATION), FUN=sum)
aggregate_data_oecd_greece <- aggregate_data_oecd[aggregate_data_oecd$LOCATION=="GRC",]$x
sum_aggregate_data_oecd <- sum(aggregate_data_oecd$x)
aggregate_data_oecd_greece_ltu <- aggregate_data_oecd[aggregate_data_oecd$x==max(aggregate_data_oecd$x),]$x


barplot_shiny_data <-as.matrix(t(eurostat), rownames.force = NA)
colnames(barplot_shiny_data)<-barplot_shiny_data[1,]
barplot_shiny_data<-barplot_shiny_data[-1,]
class(barplot_shiny_data)<-"numeric"



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
        ),
        sidebarLayout(      
    
          # Define the sidebar with one input
          sidebarPanel(
            selectInput("Country", "Country:", 
                        choices=colnames(barplot_shiny_data)),
            hr()
          ),
          
          # Create a spot for the barplot
          mainPanel(
            plotOutput("countryPlot")  
          )
    
        )
      ),
      tabItem("rawdata",
        fluidRow( 
          box(title = "Eurostat Dataset",
              solidHeader = T,
              width = 8,
              collapsible = T,
              div(DT::DTOutput("eurostat"), style = "font-size: 70%;")),
          box(title = "Oecd Dataset",
              solidHeader = T,
              width = 8,
              collapsible = T,
              div(DT::DTOutput("oecd"), style = "font-size: 70%;"))

        )
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

    # Fill in the spot we created for a plot
    output$countryPlot <- renderPlot({
      
      # Render a barplot
      barplot(barplot_shiny_data[,input$Country],
        main=input$Country,
              ylab="Number of Suicides",
              xlab="Year")
    })

    output$eurostat <- DT::renderDataTable(eurostat,
                                       rownames=F, options = list(pageLength = 10))
    output$oecd <- DT::renderDataTable(oecd,
                                       rownames=F, options = list(pageLength = 15))                                    

}

shinyApp(ui = ui, server = server)

