#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(xts)
library(plotly)
library(fpp2)
library(ggplot2)
library(tidyverse)
library(dplyr)
# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    navbarPage("Time Series Forecasting",
               tabPanel("About"),
               tabPanel("Data",sidebarLayout(
                   sidebarPanel(
                       h4("Import and Build Dataset"),
                       fileInput("file1", "Choose a File",
                                 accept = c(
                                     "text/csv",
                                     "text/comma-separated-values,text/plain",
                                     ".csv")
                       
                   ),
                   radioButtons("per","Choose periodicity ",c("Daily","Monthly","Yearly"))
                   ),
                   mainPanel(
                       tabsetPanel(
                           tabPanel("View",DT::dataTableOutput("dt")),
                           tabPanel("Description",verbatimTextOutput("des")),
                           tabPanel("Summary",DT::dataTableOutput("sum"))
                       )
                       
                   )
               )
                        ),
               tabPanel("Visualization",sidebarLayout(
                   sidebarPanel(h4("Take a look at your data"),radioButtons("type","Choose the function",c("Normal","log","diff","log_diff")),br(),selectInput("gg","Choose a variable","not_selected"))
               ,
               mainPanel(
                   plotlyOutput("plot1"),
                   plotlyOutput("plot2")
               )
               )),
               tabPanel("Statistics",tabsetPanel(
                   tabPanel("Description",br(),"This data has :",verbatimTextOutput("periodicity"),br(),"Number of years :",verbatimTextOutput("nyear"),br(),"Number of quarters : ",verbatimTextOutput("nquar")),
                   tabPanel("ACF",sidebarLayout(
                       sidebarPanel(h4("Analyse your data"),radioButtons("type1","Choose the function",c("Normal","log","diff","log_diff")),selectInput("cc","Choose a variable","not_selected")),
                       mainPanel(plotlyOutput("plot3"))
                   )),
                   tabPanel("PACF",sidebarLayout(
                       sidebarPanel(h4("Analyse your data"),radioButtons("type2","Choose the function",c("Normal","log","diff","log_diff")),selectInput("pp","Choose a variable","not_selected")),
                       mainPanel(plotlyOutput("plot4"))
                   ))
                   ,
                   tabPanel("Ljung-Box-test",sidebarLayout(
                       sidebarPanel(selectInput("bb","Choose a variable","not_selected")),
                       mainPanel(verbatimTextOutput("boxt"))
                   )
               ))
                        
                        ),
               navbarMenu("Models",
                        tabPanel("SARIMA",),
                        tabPanel("Exponential smoothing"),
                        tabPanel("TBATS"),
                        tabPanel("COMPARE")
                        )
               ),

        theme=shinytheme("superhero")
    )


# Define server logic required to draw a histogram
server <- function(session,input, output) {
    my_data<-reactive({
        inFile <- input$file1
        if (is.null(inFile))
            return(NULL)
        tbl <- read.csv(inFile$datapath)
        dates<- as.Date(tbl$Date, format = "%Y-%m-%d")
        tbl<-tbl[,sapply(tbl, is.numeric)]
        tbl<-xts(tbl,order.by = dates)
        return(tbl)
    })
    output$dt<-DT::renderDataTable({
        switch(input$per,
               Daily=my_data(),
               Monthly=to.monthly(my_data()),
               Yearly=to.yearly(my_data())
               )
    })
    output$sum<-DT::renderDataTable({
        switch(input$per,
               Daily=summary(my_data()),
               Monthly=summary(to.monthly(my_data())),
               Yearly=summary(to.yearly(my_data()))
        )
    })
    output$des<-renderPrint({
        switch(input$per,
               Daily=str(my_data()),
               Monthly=str(to.monthly(my_data())),
               Yearly=str(to.yearly(my_data()))
        )
    })
    observeEvent(my_data(), {
        
        mySeries <- my_data()
        
        updateSelectInput( session,
                          'gg', 
                          label = 'Select Variable :',
                          choices = names(mySeries)
                          )
        
    })
    output$plot1<-renderPlotly({
        dft<-ts(my_data())
        dfts<-dft[,input$gg]
        lg<-log(dfts)
        dff<-diff(dfts)
        lgd<-log(dff)
        switch(input$type,
               Normal=autoplot(dfts,main = "Analyse your data") + geom_area(alpha=0.15),
               log=autoplot(lg,main = "Analyse your data") + geom_area(alpha=0.15),
               diff=autoplot(dff,main = "Analyse your data") + geom_area(alpha=0.15),
               log_diff=autoplot(lgd,main = "Analyse your data") + geom_area(alpha=0.15)
            
        )
    })
    output$plot2<-renderPlotly({
        dfts<-ts(my_data())
        lg<-log(dfts)
        dff<-diff(dfts)
        lgd<-log(dff)
        switch(input$type,
               Normal=autoplot(dfts,main = "Analyse your data") + geom_area(alpha=0.15),
               log=autoplot(lg,main = "Analyse your data") + geom_area(alpha=0.15),
               diff=autoplot(dff,main = "Analyse your data") + geom_area(alpha=0.15),
               log_diff=autoplot(lgd,main = "Analyse your data") + geom_area(alpha=0.15)
               
        )
    })
    observeEvent(my_data(), {
        
        mySeries <- my_data()
        
        updateSelectInput( session,
                           'cc', 
                           label = 'Select Variable :',
                           choices = names(mySeries)
        )
        
    })
    
    output$plot3<-renderPlotly({
        dft<-ts(my_data())
        dfts<-dft[,input$cc]
        lg<-log(dfts)
        dff<-diff(dfts)
        lgd<-log(dff)
        switch (input$type1,
            Normal =ggAcf(dfts,lag.max = NULL,type = c("correlation", "covariance", "partial"),plot = TRUE),
            log =ggAcf(lg,lag.max = NULL,type = c("correlation", "covariance", "partial"),plot = TRUE),
            diff=ggAcf(dff,lag.max = NULL,type = c("correlation", "covariance", "partial"),plot = TRUE),
            log_diff=ggAcf(lgd,lag.max = NULL,type = c("correlation", "covariance", "partial"),plot = TRUE)
        )
    })
    observeEvent(my_data(), {
        
        mySeries <- my_data()
        
        updateSelectInput( session,
                           'pp', 
                           label = 'Select Variable :',
                           choices = names(mySeries)
        )
        
    })
    
    output$plot4<-renderPlotly({
        dft<-ts(my_data())
        dfts<-dft[,input$pp]
        lg<-log(dfts)
        dff<-diff(dfts)
        lgd<-log(dff)
        switch (input$type2,
                Normal =ggAcf(dfts,lag.max = NULL,type = c("correlation", "covariance", "partial"),plot = TRUE),
                log =ggAcf(lg,lag.max = NULL,type = c("correlation", "covariance", "partial"),plot = TRUE),
                diff=ggAcf(dff,lag.max = NULL,type = c("correlation", "covariance", "partial"),plot = TRUE),
                log_diff=ggAcf(lgd,lag.max = NULL,type = c("correlation", "covariance", "partial"),plot = TRUE)
        )
    })
    observeEvent(my_data(), {
        
        mySeries <- my_data()
        
        updateSelectInput( session,
                           'bb', 
                           label = 'Select Variable :',
                           choices = names(mySeries)
        )
        
    })
    output$boxt<-renderPrint({
        dft<-ts(my_data())
        dfts<-dft[,input$bb]
        Box.test(dfts,lag=24,fitdf=0,type="Ljung-Box")
    })
    output$periodicity<-renderPrint({
        periodicity(my_data())
    })
    output$nquar<-renderPrint({
        nquarters(my_data())
    })
    output$nyear<-renderPrint({
        nyears(my_data())
    })
     
    
}

# Run the application 
shinyApp(ui = ui, server = server)
