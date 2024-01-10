#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Load libraries
# Load Packages
library(shiny)
library(plotly)
library(ggplot2)
library(gridExtra)
library(shinythemes)
library(lmtest)
library(car)
library(olsrr)
library(nortest)
library(DT)

# Data
data <- data.frame(
  month = c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec"),
  x1 = c(150000, 160000, 170000, 180000, 190000, 200000, 210000, 220000, 230000, 240000, 250000, 260000),
  x2 = c(8000, 9500, 10000, 10500, 11000, 9000, 11500, 12000, 12500, 13000, 14000, 15000),
  x3 = c(5, 4.5, 4.8, 4.6, 5.1, 4.7, 4.9, 5, 5.2, 5.3, 5.4, 5.5),
  x4 = c(8.5, 8.2, 8.4, 8.5, 8.6, 8.7, 8.8, 8.9, 8.7, 8.8, 8.9, 9),
  x5 = c(20000, 22000, 25000, 23000, 30000, 28000, 27000, 35000, 40000, 45000, 50000, 60000),
  y = c(120, 150, 160, 165, 180, 170, 190, 210, 230, 250, 300, 350)
)

# User Interface
User.Interface <- dashboardPage(skin = "black",
                                dashboardHeader(title = "Sales Prediction Dashboard"),
                                dashboardSidebar(
                                  sidebarMenu(
                                    menuItem("Dashboard", tabName = "dashboard"),
                                    menuItem("Residual", tabName = "residual"),
                                    menuItem("Summary Model", tabName = "summary"),
                                    menuItem("Heterocesdasticity", tabName = "heteroscedasticity"),
                                    menuItem("Multicollinearity", tabName = "multicollinearity"),
                                    menuItem("Autocorrelation", tabName = "autocorrelation"),
                                    menuItem("Prediction", tabName = "prediction"),
                                    menuItem("Data", tabName = "data")
                                  )
                                ),
                                dashboardBody(
                                  tabItems(
                                    # Home tab
                                    tabItem(tabName = "dashboard",
                                            fluidPage(
                                              box(
                                                title = "Mothly Sales Volume",
                                                width = 12, background = "black", solidHeader = TRUE,
                                                plotOutput("monthly_sales_plot")
                                              ),
                                              box(
                                                title = "Relationship Plot",
                                                width = 12, background = "black", solidHeader = TRUE,
                                                plotOutput("relationship_plot")
                                              )
                                            )
                                    ),
                                    
                                    # Residual tab
                                    tabItem(tabName = "residual",
                                            fluidPage(
                                              box(
                                                title = "Residual All Data",
                                                width = 12, background = "black", solidHeader = TRUE,
                                                verbatimTextOutput("residual_output1")
                                              ),
                                              box(
                                                title = "Residual All Data Plot",
                                                width = 12, background = "black", solidHeader = TRUE,
                                                plotOutput("residual_plot1")
                                              ),
                                              box(
                                                title = "Residual Significant Data",
                                                width = 12, background = "black", solidHeader = TRUE,
                                                verbatimTextOutput("residual_output2")
                                              ),
                                              box(
                                                title = "Residual Significant Data Plot",
                                                width = 12, background = "black", solidHeader = TRUE,
                                                plotOutput("residual_plot2")
                                              )
                                            )
                                    ),
                                    
                                    # Summary Model tab
                                    tabItem(tabName = "summary",
                                            fluidPage(
                                              box(
                                                title = "Summary Model",
                                                width = 12, background = "black", solidHeader = TRUE,
                                                verbatimTextOutput("summary_output1")
                                              ),
                                              box(
                                                title = "Summary Significant Model",
                                                width = 12, background = "black", solidHeader = TRUE,
                                                verbatimTextOutput("summary_output2")
                                              )
                                            )
                                    ),
                                    
                                    # Heteroscedasticity Plot tab
                                    tabItem(tabName = "heteroscedasticity",
                                            fluidPage(
                                              box(
                                                title = "Heteroscedasticity All Data",
                                                width = 12, background = "black", solidHeader = TRUE,
                                                verbatimTextOutput("hetero_plot1")
                                              ),
                                              box(
                                                title = "Heteroscedasticity Significant Data",
                                                width = 12, background = "black", solidHeader = TRUE,
                                                verbatimTextOutput("hetero_plot2")
                                              )
                                            )
                                    ),
                                    
                                    # Multicollinearity tab
                                    tabItem(tabName = "multicollinearity",
                                            fluidPage(
                                              box(
                                                title = "Multicollinearity All Data",
                                                width = 12, background = "black", solidHeader = TRUE,
                                                verbatimTextOutput("multicollinearity_output1")
                                              ),
                                              box(
                                                title = "Multicollinearity Significant Data",
                                                width = 12, background = "black", solidHeader = TRUE,
                                                verbatimTextOutput("multicollinearity_output2")
                                              )
                                            )
                                    ),
                                    
                                    # Autocorrelation tab
                                    tabItem(tabName = "autocorrelation",
                                            fluidPage(
                                              box(
                                                title = "Autocorrelation All Data",
                                                width = 12, background = "black", solidHeader = TRUE,
                                                verbatimTextOutput("autocorrelation_plot1")
                                              ),
                                              box(
                                                title = "Autocorrelation Significant Data",
                                                width = 12, background = "black", solidHeader = TRUE,
                                                verbatimTextOutput("autocorrelation_plot2")
                                              )
                                            )
                                    ),
                                    
                                    
                                    # Prediction tab
                                    tabItem(tabName = "prediction",
                                            fluidPage(
                                              box(
                                                title = "Sales Prediction",
                                                width = 12, background = "black", solidHeader = TRUE,
                                                numericInput("input_x2", "Enter Number of Transactions:", value = 10000, min = 8000, max = 15000),
                                                numericInput("input_x5", "Enter Number of Advertisements:", value = 25000, min = 20000, max = 60000),
                                                actionButton("predict_button", "Predict", icon("calculator")),
                                                plotOutput("prediction_plot"),
                                                dataTableOutput("prediction_table")
                                              )
                                            )
                                    ),
                                    # Data tab
                                    tabItem(tabName = "data",
                                            fluidPage(
                                              box(
                                                title = "Dataset",
                                                width = 12,  solidHeader = TRUE,
                                                dataTableOutput("data_table")
                                              )
                                            )
                                    )
                                  )
                                )
)

# Server
server <- function(input, output) {
  # Output Regresi
  fit1 <- lm(y ~ x1 + x2 + x3 + x4 + x5, data = data)
  fit2 <- lm(y ~ x2 + x5, data = data)
  names(fit1$coefficients)
  names(fit2$coefficients)
  summaryoutput1 <- summary(fit1)
  summaryoutput2 <- summary(fit2)
  residual_plot1 <- ad.test(fit1$residuals)
  residual_plot2 <- ad.test(fit2$residuals)
  autokorelasiplot1 <- dwtest(fit1)
  autokorelasiplot2 <- dwtest(fit2)
  heteroplot1 <- bptest(fit1, studentize = F, data= data)
  heteroplot2 <- bptest(fit2, studentize = F, data= data)
  multicollinearityoutput1 <- vif(fit1)
  multicollinearityoutput2 <- vif(fit2)
  
  output$residual_output1 <- renderPrint({
    residual_plot1
  })
  
  output$residual_output2 <- renderPrint({
    residual_plot2
  })
  
  output$summary_output1 <- renderPrint({
    summaryoutput1
  })
  
  output$summary_output2 <- renderPrint({
    summaryoutput2
  })
  
  output$hetero_plot1 <- renderPrint({
    heteroplot1
  })
  
  output$hetero_plot2 <- renderPrint({
    heteroplot2
  })
  
  output$multicollinearity_output1 <- renderPrint({
    multicollinearityoutput1
  })
  
  output$multicollinearity_output2 <- renderPrint({
    multicollinearityoutput2
  })
  
  output$autocorrelation_plot1 <- renderPrint({
    autokorelasiplot1
  })
  
  output$autocorrelation_plot2 <- renderPrint({
    autokorelasiplot2
  })
  
  # Update residual plot 1
  output$residual_plot1 <- renderPlot({
    residuals_plot1 <- plot(residuals(fit1), main = "Residuals Significant Data Plot")
    return(residuals_plot1)
  })
  
  # Update residual plot 2
  output$residual_plot2 <- renderPlot({
    residuals_plot2 <- plot(residuals(fit2), main = "Residuals Significant DataPlot")
    return(residuals_plot2)
  })
  
  # Monthly sales Volume
  output$monthly_sales_plot <- renderPlot({
    warna_bulan <- c("jan" = "#D9E0EB", "feb" = "#DBE1EA", "mar" = "#D2DDEE", "apr" = "#E2E4E7",
                     "may" = "#CFDCEF", "jun" = "#D4DEED", "jul" = "#D6DFEC", "aug" = "#DFE3E8", 
                     "sep" = "#C9DAF1", "oct" = "#CBDAF1", "nov" = "#CDDBF0", "dec" = "#DDE2E9")
    ggplot(data, aes(x = month, y = y, fill = month)) +
      geom_bar(stat = "identity") +
      scale_fill_manual(values = warna_bulan) +
      labs(x = "Month", y = "Sales Volume") +
      theme_minimal()
  })
  
  # Relationship plot
  output$relationship_plot <- renderPlot({
    ggplot(data, aes(x = x2, y = x5)) +
      geom_point() +
      labs(x = "Number of Transactions",
           y = "Number of Advertisements") +
      theme_minimal()
  })
  
  # Data table
  output$data_table <- renderDataTable({
    data
  })
  
  # Regression model
  model <- lm(y ~ x2 + x5, data = data)
  
  # Function to predict sales
  predict_sales <- function(x2, x5) {
    new_data <- data.frame(x2 = x2, x5 = x5)
    predicted_sales <- predict(model, newdata = new_data)
    return(predicted_sales)
  }
  
  # Observe button click and update plots and tables
  observeEvent(input$predict_button, {
    x2_value <- input$input_x2
    x5_value <- input$input_x5
    result <- predict_sales(x2_value, x5_value)
    
    # Update plot
    output$prediction_plot <- renderPlot({
      ggplot(data, aes(x = y, y = result)) +
        geom_point() +
        geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
        labs(title = "Actual vs Predicted Sales Volume",
             x = "Actual Sales Volume",
             y = "Predicted Sales Volume") +
        theme_minimal()
    })
    
    # Update table
    output$prediction_table <- renderDataTable({
      data.frame(Actual = data$y, Predicted = result)
    })
  })
}

# Run the app
shinyApp(User.Interface, server)