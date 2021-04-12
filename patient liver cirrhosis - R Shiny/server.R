library(shiny)
library(shinydashboard)
library(randomForest)
library(DT)
set.seed(4)
load("rf.rda")

shinyServer(function(input,output){
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
    Prediction <- ifelse(pred[,2] < 0.5,"Not_affected","affected")
    probability <-pred[,2]
    final_tab<-data.frame(full_data[2],Prediction)
    final_tab
  
  })
  
    output$y_cap <- renderDT({
    out()
    }) 
    output$Not_affected <- renderDT({
      subset(out(),Prediction=="Not_affected")
      }) 
    output$affected <- renderDT({
      subset(out(),Prediction=="affected")
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

