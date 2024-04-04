library(shiny)
library(dplyr)
library(plotly)
library(leaflet)

# Load the Uber data
uber.apr <- read.csv("data/uber-raw-data-apr14.csv")
uber.apr$Date.Time <- as.POSIXct(uber.apr$Date.Time, format = "%m/%d/%Y %H:%M:%S")
uber.apr$day <- as.integer(format(uber.apr$Date.Time, "%d"))
uber.apr$weekday <- weekdays(uber.apr$Date.Time)
uber.apr$hour <- as.integer(format(uber.apr$Date.Time, "%H"))

# Define UI
ui <- fluidPage(
  titlePanel("Uber Bookings Analysis"),
  sidebarLayout(
    sidebarPanel(
      h4("Choose Analysis:"),
      tabsetPanel(
        tabPanel("Time Series", plotlyOutput("timeSeriesPlot")),
        tabPanel("Scatter Plot", 
                 selectInput("scatter_x", "X-axis:", choices = c("day", "hour")),
                 selectInput("scatter_y", "Y-axis:", choices = c("hour", "day")),
                 plotOutput("scatterPlot"))
      )
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Interactive Map", leafletOutput("mapPlot"))
      )
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  # Combined Time Series Plot
  output$timeSeriesPlot <- renderPlotly({
    plot_ly(data = uber.apr) %>%
      add_trace(x = ~Date.Time, y = ~day, type = 'scatter', mode = 'lines', name = "Day of the Month") %>%
      add_trace(x = ~Date.Time, y = ~weekday, type = 'scatter', mode = 'lines', name = "Day of the Week") %>%
      add_trace(x = ~Date.Time, y = ~hour, type = 'scatter', mode = 'lines', name = "Time of the Day") %>%
      layout(
        title = "Interactive Time Series Plot",
        xaxis = list(title = "Date/Time"),
        yaxis = list(title = "Value")
      )
  })
  
  # Scatter Plot
  output$scatterPlot <- renderPlot({
    x_var <- input$scatter_x
    y_var <- input$scatter_y
    ggplot(uber.apr, aes_string(x = x_var, y = y_var)) +
      geom_point(color = "darkgreen") +
      labs(x = x_var, y = y_var, title = "Uber Bookings Scatter Plot") +
      theme_minimal()
  })
  
  # Interactive Map
  output$mapPlot <- renderLeaflet({
    leaflet(uber.apr) %>%
      addTiles() %>%
      addMarkers(lng = ~Lon, lat = ~Lat, popup = ~paste("Date/Time: ", Date.Time))
  })
}

# Run the application
shinyApp(ui = ui, server = server)
