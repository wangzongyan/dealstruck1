# ui.R
require("shiny")
source("Preprocessing.R")
shinyUI(fluidPage(
  tags$h2("Brief Summary of ANOVA and Logistic Regression"),
  tabsetPanel(
    tabPanel("ANOVA", 
             fluidRow(column(6, sliderInput(inputId = "ANOVA_p", 
                                            label = "Select p-value threshold", 
                                            value = 0.05,
                                            min = 0.001, max = 1, step = 0.001)),
                      column(6, sliderInput(inputId = "ANOVA_pbar", 
                                            label = "Select p-value red line", 
                                            value = 0.01,
                                            min = 0.001, max = 0.1, step = 0.001)) 
             ),
             plotOutput(outputId = "ANOVA_plot", height = "450px"),
             column(4, downloadButton(outputId = "downloadPlot1",
                                      label = "Download current plot"
             ))
    ),
    tabPanel("Logistic Regression", 
             sidebarLayout(
               sidebarPanel(
                 selectInput(inputId = "log_out",
                             label = "Select the pair of classification",
                             choices = c(
                               "Fully_Paid and Potential Delinquency Events",
                               "Current and Potential Delinquency Events",
                                         "Current and Fully_Paid"
                             ),
                             selected = "Current"),
                 sliderInput(inputId = "ANOVA_p2", 
                             label = "Select p-value", 
                             value = 0.01,
                             min = 0.001, max = 1, step = 0.001),
                 sliderInput(inputId = "threshold", 
                             label = "Select threshold", 
                             value = 0.5,
                             min = 0.01, max = 1, step = 0.01),
                 sliderInput(inputId = "cost_of_FP", 
                             label = "cost of FP (FP: pred=1,true=0)",
                             min = 1, max = 10, step = 1, value = 1),
                 sliderInput(inputId = "cost_of_FN",
                             label = "cost of FN (FN:pred=0, true=1)",
                             min = 1, max = 10, step = 1, value = 1),
                 downloadButton(outputId = "downloadPlot2",
                                label = "Download Prediction Accuracy Plot"
                 ),
                 downloadButton(outputId = "downloadPlot3",
                                label = "Download ROC Curve Plot"
                 )
               ),
               mainPanel(tabsetPanel(
                 tabPanel("Prediction distribution", 
                          plotOutput(outputId = "logistic_plot", height = "450px"),       
                          textOutput(outputId = "pred_error")
                 ),
                 tabPanel("ROC Curve", plotOutput(outputId = "roc_plot", height = "450px"),
                          textOutput(outputId = "cost")
                 ))
               )
             )
    )
  )
))