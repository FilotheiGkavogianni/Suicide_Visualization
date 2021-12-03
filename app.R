library(shiny)
library(shinydashboard)
library("readxl")
library(DT)
library(highcharter)
library(ggplot2)
library(emojifont)

# oecd<- read.csv("suicide II OECD.csv.txt", header = TRUE)
# eurostat <- read_excel("suicide1.xls",col_names =TRUE, skip=2)


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
            selectInput("Country", "Country(eurostat):", 
                        choices=colnames(barplot_shiny_data)),
          ),
          
          # Create a spot for the barplot
          mainPanel(
            h3(paste0("Bar chart"), align = "center"),
            plotOutput("countryPlot")
          )
    
        ),

        fluidRow( 
          h3(paste0("Time series chart"), align = "center"),
          column( width = 10,h4(paste0("Suicides in Europe (2004-2014) (per 1000000 person)"), align = 'center'), highchartOutput('timeseries') ),
        ),

        sidebarLayout (
          sidebarPanel(
            selectInput("LOCATION", "Location(oecd):",
                        choices=unique(oecd$LOCATION))
          ),
          
          # Create a spot for the barplot
          mainPanel(
            h3(paste0("Boxplot"), align = "center"),
            plotOutput("boxplot"),
            h3(paste0("Scatterplot"), align = "center"),
            plotOutput("scatterplot")  
          )
        ),

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

    transformdat <- reactive({
      choices <- unique(oecd$LOCATION)
      selected <- isolate(input$LOCATION)    
      updateSelectInput(session, "LOCATION", choices = choices, selected = selected)
      oecd
    })

    transformdata <- reactive({
      x <- transformdat()
      x[x$LOCATION == input$LOCATION,,drop=FALSE]
    })

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
              main= paste0("Suicides per year in ",input$Country),
              ylab="Suicides",
              xlab="Year")
    })

    output$timeseries <-renderHighchart({
         highchart() %>%
            hc_chart(type = 'line') %>%
            hc_series( list(name = 'Greece', data = oecd$Value[oecd$LOCATION=='GRC'] , color='blue', marker = list(symbol = 'circle') ),
                       list(name = 'Deutsch', data = oecd$Value[oecd$LOCATION=='DEU'], color = 'black', dashStyle = 'shortDot', marker = list(symbol = 'triangle') ),
                       list(name = 'France', data = oecd$Value[oecd$LOCATION=='FRA'], color = 'red', marker = list(symbol = 'circle') ),
                       list(name = 'Belgium', data = oecd$Value[oecd$LOCATION=='BEL'], color = 'yellow', dashStyle = 'shortDot', marker = list(symbol = 'triangle'))
            )%>%
            hc_xAxis( categories = unique(oecd$TIME) ) %>%
            hc_yAxis( title = list(text = "Suicides"),
                      labels = list( format = "{value:,.0f}")  ) %>%
            hc_plotOptions(column = list(
               dataLabels = list(enabled = F),
               #stacking = "normal",
               enableMouseTracking = T ) 
            )%>%
            hc_tooltip(table = TRUE,
                       sort = TRUE,
                       pointFormat = paste0( '<br> <span style="color:{point.color}">\u25CF</span>',
                                             " {series.name}: {point.y}"),
                       headerFormat = '<span style="font-size: 13px">Year {point.key}</span>'
            ) %>%
            hc_legend( layout = 'vertical', align = 'right', verticalAlign = 'bottom', floating = T, x = 000, y = -020 )
      })

    output$eurostat <- DT::renderDataTable(eurostat,
                                       rownames=F, options = list(pageLength = 10))
    output$oecd <- DT::renderDataTable(oecd,
                                       rownames=F, options = list(pageLength = 15))

    output$boxplot <- renderPlot({
    ggplot(data = transformdata(), aes(x = LOCATION, y = Value, fill = LOCATION)) +
      geom_boxplot() +
      theme_bw(base_size = 14) + xlab("") + ylab("Suicides") + ggtitle("Suicides per Location(per 1000000 person)") +
      theme(axis.text=element_text(size=15, face = "bold", color = "black", hjust = 0.5),
            plot.title=element_text(size=15, face = "bold", color = "black", hjust = 0.5),
            strip.text = element_text(size=15, face = "bold", color = "black", hjust = 0.5))
    })

    output$scatterplot <- renderPlot({
    ggplot(data = transformdata(), aes(x = TIME, y = Value, group = LOCATION)) +
      geom_fontawesome(alias = "fa-tint",color = "red", size = 7, x = transformdata()$TIME,y = transformdata()$Value, group = transformdata()$LOCATION)+
      theme(axis.text=element_text(size=15, face = "bold", color = "black"),
            plot.title=element_text(size=15, face = "bold", color = "black", hjust = 0.5),
            strip.text = element_text(size=15, face = "bold", color = "black"))+
      xlab("Time")+
      ylab("Suicides")+
      ggtitle("Suicides through Time(per 1000000 person)")
    })
                                    

}

shinyApp(ui = ui, server = server)

