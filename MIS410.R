# Required libraries

library(shiny)
library(shinydashboard)
library(ggplot2)

# Load data
advertising_data <- read.csv("advertising.csv")

# Custom CSS for style
custom_css <- "
body {
  background-color: #f5f5f5;
  font-family: 'Segoe UI', sans-serif;
}
.skin-blue .main-sidebar {
  background-color: #2c3e50;
}
.skin-blue .sidebar-menu > li > a {
  color: #ecf0f1;
  font-weight: bold;
}
.skin-blue .sidebar-menu > li.active > a {
  background-color: #34495e;
  color: #ffffff;
}
.skin-blue .main-header .logo {
  background-color: #1abc9c;
  color: white;
  font-weight: bold;
}
.skin-blue .main-header .navbar {
  background-color: #16a085;
}
.form-control {
  border-radius: 8px;
  border: 1px solid #ecf0f1;
  padding: 8px;
}
.btn {
  background-color: #1abc9c;
  color: white;
  font-weight: bold;
  border-radius: 8px;
  padding: 10px 20px;
}
#sales_prediction {
  font-size: 24px;
  font-weight: bold;
  color: #2c3e50;
  text-align: center;
  padding-top: 15px;
}
.content-wrapper {
  padding: 50px;
}
"

# UI
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "📊 Sales Prediction Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Prediction", tabName = "prediction", icon = icon("chart-line")),
      menuItem("Visualization", tabName = "visualization", icon = icon("chart-bar"))
    )
  ),
  dashboardBody(
    tags$head(tags$style(HTML(custom_css))),
    tabItems(
      tabItem(tabName = "prediction",
              fluidRow(
                box(
                  width = 4,
                  numericInput("tv", "TV Advertising ($)", value = 0),
                  numericInput("radio", "Radio Advertising ($)", value = 0),
                  numericInput("newspaper", "Newspaper Advertising ($)", value = 0),
                  actionButton("predict", "Predict Sales"),
                  style = "margin-bottom: 200px;"
                )
              ),
              fluidRow(
                box(
                  width = 8,
                  status = "success",
                  solidHeader = TRUE,
                  title = "Prediction Output",
                  div(verbatimTextOutput("sales_prediction"), style = "min-height: 60px;")
                )
              )),
      tabItem(tabName = "visualization",
              fluidRow(
                box(width = 12, plotOutput("tv_plot", height = "300px")),
                box(width = 12, plotOutput("radio_plot", height = "300px")),
                box(width = 12, plotOutput("news_plot", height = "300px"))
              ))
    )
  )
)

# Server
server <- function(input, output, session) {
  sales_model <- lm(Sales ~ TV + Radio + Newspaper, data = advertising_data)
  
  observeEvent(input$predict, {
    new_data <- data.frame(
      TV = input$tv,
      Radio = input$radio,
      Newspaper = input$newspaper
    )
    
    prediction <- predict(sales_model, new_data)
    
    output$sales_prediction <- renderText({
      paste0("🎯 Predicted Sales: $", round(prediction, 2))
    })
  })
  
# TV plot
  output$tv_plot <- renderPlot({
    ggplot(advertising_data, aes(x = TV, y = Sales)) +
      geom_point(color = "#3498db", size = 2) +
      geom_smooth(method = "lm", color = "#1abc9c", size = 1.2) +
      labs(title = "Sales vs. TV Advertising", x = "TV Advertising ($)", y = "Sales ($)") +
      theme_minimal(base_size = 14)
  })
  
# Radio plot
  output$radio_plot <- renderPlot({
    ggplot(advertising_data, aes(x = Radio, y = Sales)) +
      geom_point(color = "#9b59b6", size = 2) +
      geom_smooth(method = "lm", color = "#1abc9c", size = 1.2) +
      labs(title = "Sales vs. Radio Advertising", x = "Radio Advertising ($)", y = "Sales ($)") +
      theme_minimal(base_size = 14)
  })
  
# Newspaper plot
  output$news_plot <- renderPlot({
    ggplot(advertising_data, aes(x = Newspaper, y = Sales)) +
      geom_point(color = "#e67e22", size = 2) +
      geom_smooth(method = "lm", color = "#1abc9c", size = 1.2) +
      labs(title = "Sales vs. Newspaper Advertising", x = "Newspaper Advertising ($)", y = "Sales ($)") +
      theme_minimal(base_size = 14)
  })
}

# Run the app
shinyApp(ui, server)