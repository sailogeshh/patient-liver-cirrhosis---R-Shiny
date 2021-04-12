library(shiny)
library(shinydashboard)
library(randomForest)
library(DT)
set.seed(4)
load("rf.rda")

Logged = FALSE
my_username <- "patient"
my_password <- "liver"

ui <- dashboardPage(skin='blue',
                    
  ###################################################################################################################       
                    
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
                         
                         
                         tabPanel("Not likely to affected ",
                                  box( width = 8, height = 600,
                                       title = "Not likely to affected ",status = "success", solidHeader = TRUE,collapsible = TRUE,
                                       dataTableOutput("Not_affected",height = 250, width = NULL))), 
                         
                         
                         tabPanel("Likely to Affected", 
                                  box( width = 8, height = 600,
                                       title = "Likely to Affected",status = "danger", solidHeader = TRUE,collapsible = TRUE,
                                       dataTableOutput("affected",height = 250, width = NULL)))
                  )
    )),verbatimTextOutput("dataInfo")
                    )
  #####################################################################################################################################################       
)


server = function(input, output,session) {
  
  values <- reactiveValues(authenticated = FALSE)
  
  # Return the UI for a modal dialog with data selection input. If 'failed' 
  # is TRUE, then display a message that the previous value was invalid.
  dataModal <- function(failed = FALSE) {
    modalDialog(
      textInput("username", "Username:"),
      passwordInput("password", "Password:"),
      footer = tagList(
        # modalButton("Cancel"),
        actionButton("ok", "OK")
      )
    )
  }
  
  # Show modal when button is clicked.  
  # This `observe` is suspended only whith right user credential
  
  obs1 <- observe({
    showModal(dataModal())
  })
  
  # When OK button is pressed, attempt to authenticate. If successful,
  # remove the modal. 
  
  obs2 <- observe({
    req(input$ok)
    isolate({
      Username <- input$username
      Password <- input$password
    })
    Id.username <- which(my_username == Username)
    Id.password <- which(my_password == Password)
    if (length(Id.username) > 0 & length(Id.password) > 0) {
      if (Id.username == Id.password) {
        Logged <<- TRUE
        values$authenticated <- TRUE
        obs1$suspend()
        removeModal()
        
      } else {
        values$authenticated <- FALSE
      }     
    } 
  })
  
  
  
  obs3 <- observe({
    if(Logged <<- TRUE)
    req(input$myuser)
    showModal(dataModal())
  })
  
  output$dataInfo <- renderPrint({
    

 ####################################################################################################       
        
    out<-reactive({
      file1 <- input$file
      if(is.null(file1)) {return(NULL)}
      full_data <- read.csv(file1$datapath,header=TRUE)
      withProgress(message='Loading table',value=30,{
        n<-10
        
        for(i in 1:n){
          incProgress(1/n,detail=paste("Doing Part", i, "out of", n))
          Sys.sleep(0.1)
        }
      })
      full_data$Gender=ifelse(full_data$Gender=='M',1,0)
      full_data$Gender=as.factor(full_data$Gender)
      pred<-predict(rf,newdata=full_data[,-c(1,2)] ,type = "prob")
      Prediction <- ifelse(pred[,2] < 0.5,"Not likely to affected","likely to affected")
      probability <-pred[,2]
      final_tab<-data.frame(full_data[2],Prediction)
      final_tab
      
    })
    
    output$y_cap <- renderDT({
      out()
    }) 
    output$Not_affected <- renderDT({
      subset(out(),Prediction=="Not likely to affected")
    }) 
    output$affected <- renderDT({
      subset(out(),Prediction=="likely to affected")
    }) 
    
    output$downloadData <- downloadHandler(
      filename = function() {
        paste("final_table_output", ".csv", sep = "")
      },
      content = function(file) {
        write.csv(out(), file, row.names = FALSE)
      }
    )
    
  })
  
}

shinyApp(ui,server)
  
  