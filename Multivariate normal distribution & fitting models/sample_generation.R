library(shiny)
library(shinydashboard)
library(ggplot2)
library(MASS)
library(shinythemes)



ui <- dashboardPage(
    
    
    
    dashboardHeader(title = "Sample generation",
                    # Dropdown menu for messages
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
                    )
    )
                    ,
    dashboardSidebar(bootstrapPage(
        
        #tags$style(".logo {background-color: red !important}"), #El contenido del style puede ser cualquier regla CSS que queramos. #tags$div(style=XXX) para que ocurra solo en un div.
        textInput("samplesize", label = "Sample", placeholder = "Sample Size" , value=100),
        checkboxGroupInput("check", label = NULL, 
                           choices = list("Divide sample in training and test" = 1)),
        conditionalPanel(
            condition = "input.check == '1'",
            numericInput("procentajet", label= "Set training %", value=50, min=1, max=100)),
        numericInput("correlation", label = "Pearson correlation", value= 0.5, min=-1, max=1, step=0.1),
        numericInput("semilla", label= "Set seed", value=2017, min=1),
        actionButton("generate", label = "Generate", icon= icon("refresh")),
        numericInput("orden", label= "Regression order", value=1, min=1, max=10),
        actionButton("pregresion", label = "Plot model", icon= icon("line-chart")),
        theme = shinytheme("united")
        
        
    )),
    dashboardBody(
        tags$div(    
            HTML("<CENTER><H4>Welcome to my shiny app! Here you can generate bivariate samples from the specified multivariate normal distribution. </H4></CENTER><br/>")
        ),
        tags$div(    
            HTML("<CENTER><H5>Press the buton 'Generate' to plot it. <br/>
                 Also you can fit a regresion model for the sample. Choose the regression order and press 'Plot model'.</H5></CENTER><br/>")
        ),
       
        textOutput("bienvenida"),
        plotOutput("grafica", click = "puntitobomba"),
        tags$div(    
            HTML("<CENTER><H5> * Note that if you set a small sample (e.g. 10) and try to fit a high regression order (ofc with correlation != 1), you will show clearly the overfitting problem.</H5></CENTER><br/>")
            )
    )
    
)




server <- function(input, output) {
    
    misDatos<- reactiveValues(dataset = NULL)
    
    
    observeEvent(input$generate, {
        
        
        set.seed(input$semilla)
        
        samplesize<- as.numeric(input$samplesize)
        
        correlation<- as.numeric(input$correlation)
        
        x <- rnorm(samplesize)
        
        y <- correlation*x + rnorm(samplesize,sd=1-correlation^2)
        
        orden <- NULL
        misDatos$dataset<- data.frame(x,y)
        
        output$grafica <- renderPlot({
            
            ggplot(misDatos$dataset) + geom_point(aes(x = x, y = y)) +xlab("Var 1")+ ylab("Var 2") #+ geom_smooth(method="lm", aes(x = x, y = y), formula=  y ~ poly(x, degree = orden),  se=FALSE, linetype = 1)
        })
    })
    
    
    
    
    observeEvent(input$pregresion, {
        
        orden<- input$orden
        output$grafica <- renderPlot({
            
            ggplot(misDatos$dataset) + geom_point(aes(x = x, y = y)) +xlab("Var 1")+ ylab("Var 2") + geom_smooth(method="lm", aes(x = x, y = y), formula=  y ~ poly(x, degree = orden),  se=FALSE, linetype = 1)
        })
        
    })
    
    
    
    
    
    
    
    
    
    
    
    # observeEvent(input$generate, {
    #     output$grafica <- renderPlot({
    #         
    #         ggplot(misDatos) + geom_point(aes(x = x, y = y)) +xlab("Variable 1")+ ylab("Variable 2") + geom_smooth(method="lm", aes(x = x, y = y), formula=  y ~ poly(x, degree = 9),  se=FALSE, linetype = 1)
    #     })
    # })
    
    
}


shinyApp(ui,server)