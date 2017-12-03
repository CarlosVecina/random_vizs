library(shinydashboard)
library(shiny)
library(ggplot2)
library(MASS)
library(plotly)
library(shinythemes)

ui <- 
  
  dashboardPage(
    dashboardHeader(title = "Sample generation",
                    # Menu de mensajes.
                    dropdownMenu(type = "messages", badgeStatus = "success",
                                 messageItem("Carlos Vecina",
                                             "Welcome to my shiny app!",
                                             time = "Now"
                                 ),
                                 messageItem("Carlos Vecina",
                                             "Any questions, contact me:",
                                             time = "Now"
                                 ),
                                 messageItem("Carlos Vecina",
                                             "carlos.vecina@cunef.edu",
                                             time = "Now"
                                 )
                    )),
    dashboardSidebar(bootstrapPage(
      numericInput(inputId = "samplesize",label = "Sample size",  value = 100),
      numericInput(inputId = "correlation",label = "Correlation", value= 0.5, step =0.1, min=-1, max=1),
      actionButton('generate','Generate',icon('refresh')),
      checkboxInput("checkbox", label = "Training / Test samples", value = F),
      conditionalPanel(
        condition = "input.checkbox=='1'",
        textInput(inputId = "porcentajet",label = "Porcentaje de training",value = .5),
        actionButton('genetrain','Train/Test',icon('refresh'))
      ),
      numericInput(inputId = "order",label = "Polynomic Order", value=NA, min=1, max=10),
      actionButton('genelm','Linear Model',icon('area-chart')),
      theme = shinytheme("united")
      
    )),
    dashboardBody(
        tags$div(    
            HTML("<CENTER><H4>Welcome to my shiny app! Here you can generate bivariate samples from the specified multivariate normal distribution. </H4></CENTER><br/>")
        ),
        tags$div(    
            HTML("<CENTER><H5>Press the buton 'Generate' to plot it. <br/>
                 Also you can fit a regresion model for the sample. Choose the polynomial order and press 'Plot model'.</H5></CENTER><br/>")
            ),
      plotlyOutput('grafica'),
      verbatimTextOutput('summary'),
      tags$div(    
          HTML("<CENTER><H5> * Note that if you set a small sample (e.g. 10) and try to fit a high regression order (ofc with correlation != 1), you will show clearly the overfitting problem.</H5></CENTER><br/>")
      )
    )
  )
  


server <- function(input, output, session) {
  mu <- c(0,0)
  sigma <- matrix(c(1,0.5,0.5,1),2,2,byrow =T)
  Data <- reactiveValues(sample = as.data.frame(mvrnorm(100,mu,sigma)))
  Plot1 <- reactiveValues(main=NULL, layer1=NULL, layer2=NULL)
  
  
  observeEvent(input$generate,{
    mu <- c(0,0)
    a<- as.numeric(input$correlation)
    sigma <- matrix(c(1,a,a,1),2,2,byrow =T)
    n <- as.numeric(input$samplesize)    
    Data$sample <-as.data.frame(mvrnorm(n,mu,sigma))
    
  })
  
  observeEvent(input$checkbox | input$genetrain | input$generate,{
    prc <- as.numeric(input$porcentajet)
    prcfinal<- sample(nrow(Data$sample),prc*nrow(Data$sample),replace = F)
    Data$sample[prcfinal,3]<-'train'
    Data$sample[-prcfinal,3]<-'test'
    Data$sample[,3]<- as.factor(Data$sample[,3])
    
  })
  
  observeEvent(input$genelm| input$checkbox | input$genetrain ,{
    grado <- as.numeric(input$order)
    d1 <- Data$sample[Data$sample[,3]=='train',]
    d2 <- Data$sample[Data$sample[,3]=='test',]
    Plot1$layer1 <- geom_smooth(method = 'lm', se = FALSE,
                                formula = y ~ poly(x,grado),colour=grado+1,
                                show.legend = T)
    
    if(input$checkbox & !is.na(grado)){
      Plot1$layer1 <- geom_smooth(data = d1,aes(x=V1,y=V2),method = 'lm', se = FALSE,
                                  formula = y ~ poly(x,grado),colour=grado+1,
                                  show.legend = T)
      Plot1$layer2 <- geom_smooth(data = d2,aes(x=V1,y=V2),method = 'lm', se = FALSE,
                                  formula = y ~ poly(x,grado),colour=grado+2,
                                  show.legend = T)
    }else{
      Plot1$layer1
    }
  })
  
  observeEvent(input$generate,{
  output$grafica <- renderPlotly({
    
    Plot1$main <- ggplot(Data$sample,aes(x=V1,y=V2))+
      geom_point() + Plot1$layer1 + xlab('X') + ylab('Y') 
    if(input$checkbox){
      plotly_graf <- ggplotly(Plot1$main +  geom_point(aes(colour=V3)) + Plot1$layer2 )
      plotly_graf
      
    }else{
      plotly_graf <- ggplotly(Plot1$main)
      plotly_graf
    }
    
  })
 
 
  output$summary <- renderText({
    paste0('La correlación de Pearson es: ',cor(Data$sample[,1:2])[1,2])
  })
  })
}


shinyApp(ui, server)

