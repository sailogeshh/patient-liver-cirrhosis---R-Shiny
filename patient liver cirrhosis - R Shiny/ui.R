library(shiny)
library(shinydashboard)
library(DT)
shinyUI(dashboardPage(skin = 'blue',
                      dashboardHeader(title="Liver cirrhosis Prediction",titleWidth = 300),
                      
                      dashboardSidebar(
                        
                        fileInput("file","Upload a Data",accept=c('csv','comma-seperated-values','.csv')),
                        downloadButton("downloadData", "Download")
                      ),
                      dashboardBody(
                        fluidRow(
                          
                          fluidPage(theme="bootstrap.min.css",
                                    
                                    tabBox ( width = 2000, height = 5500,
                                             
                                             
                                             
                                             tabPanel("Output", p("The aim of this application is to Predict Liver cirrhosis disease...",
                                                                  style="font-family:'Berlin sans FB Demi';font-size:12pt"), 
                                                      box( width = 8, height = 600 ,
                                                           title = "RESULT",status = "primary", solidHeader = TRUE,collapsible = TRUE,
                                                           dataTableOutput("y_cap", height = 250, width = NULL))), 
                                             
                                             
                                             tabPanel("Not affected ",
                                                      box( width = 8, height = 600,
                                                           title = "Not affected ",status = "success", solidHeader = TRUE,collapsible = TRUE,
                                                           dataTableOutput("Not_affected",height = 250, width = NULL))), 
                                             
                                             
                                             tabPanel("Affected", 
                                                      box( width = 8, height = 600,
                                                           title = "Affected",status = "danger", solidHeader = TRUE,collapsible = TRUE,
                                                           dataTableOutput("affected",height = 250, width = NULL)))
                                    )
                          )
                        )
                      )))
                      