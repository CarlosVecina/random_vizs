library(shiny)
library(shinydashboard)
library(ggplot2)
library(MASS)
library(shinythemes)



ui <- dashboardPage(
    
    
    
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
                    )
    )
    ,
    dashboardSidebar(bootstrapPage(
        
        #tags$style(".logo {background-color: red !important}") # Cambiar color del logo del encabezado.
        numericInput("samplesize", label = "Sample size", value=100),  
        checkboxGroupInput("check", label = NULL, 
                           choices = list("Divide sample in training and test" = 1)),
        conditionalPanel(
            condition = "input.check == '1'",
            numericInput("procentajet", label= "Set training %", value=50, min=1, max=100)),
        numericInput("correlation", label = "Pearson correlation", value= 0.5, min=-1, max=1, step=0.1),
        numericInput("semilla", label= "Set seed", value=2017, min=1),
        actionButton("generate", label = "Generate", icon= icon("refresh")),
        numericInput("orden", label= "Polynomial order", value=1, min=1, max=10),
        actionButton("pregresion", label = "Plot model", icon= icon("area-chart")),
        theme = shinytheme("united")
        
        
    )),
    dashboardBody(
        # Insertamos HTML para mejorar el formato
        tags$div(    
            HTML("<CENTER><H4>Welcome to my shiny app! Here you can generate bivariate samples from the specified multivariate normal distribution. </H4></CENTER><br/>")
        ),
        tags$div(    
            HTML("<CENTER><H5>Press the buton 'Generate' to plot it. <br/>
                 Also you can fit a regresion model for the sample. Choose the polynomial order and press 'Plot model'.</H5></CENTER><br/>")
        ),
        
        textOutput("bienvenida"),
        plotOutput("grafica", click = "puntitobomba"),
        tags$div(    
            HTML("<CENTER><H5> * Note that if you set a small sample (e.g. 10) and try to fit a high regression order (ofc with correlation != 1), you will show clearly the overfitting problem.</H5></CENTER><br/>")
        )
    )
    
)




server <- function(input, output) {
    
    
    
    
    misDatos<- reactiveValues(muestra=NULL) # Dentro de esta lista reactive creamos una variable y la iniciamos. 
 
    observeEvent(input$generate, {
        
        
        isolate(set.seed(input$semilla))
        Sigma <- isolate(matrix(c(1,input$correlation,input$correlation,1),2,2)) # Isolate para aislar los inputs. Creamos una matriz de correlación con la corr introducida por el usuario.
        misDatos$muestra<-as.data.frame(mvrnorm(isolate(input$samplesize),c(0,0),Sigma)) # Generación de muestra multivariante. Con la matriz de correlación indicamos queson dos variables.
        
        output$grafica<- renderPlot({   # Ploteo la grafica que cambia con el reactive.
            
            
            ggplot(misDatos$muestra) + geom_point(aes(x= V1, y = V2)) +
                labs(x = "Variable 1",y = "Variable 2") #+ geom_smooth(method="lm", aes(x =misDatos$muestra$V1, y = misDatos$muestra$V2), formula=  y ~ poly(x, degree = 4),  se=FALSE, linetype = 1)
            
        })
        
    })  
    
    observe({   
        input$puntitobomba  # Quiero que se ejecute cada vez que se haga click en el grafico.
        isolate({
            misDatos$x<-c(misDatos$x,input$puntitobomba$x)  # Guardo la cordenada del eje x en el reactive.
            misDatos$y<-c(misDatos$y, input$puntitobomba$y) # Guardo la cordenada del eje y en el reactive.
            misDatos$muestra<- rbind(misDatos$muestra, cbind(input$puntitobomba$x, input$puntitobomba$y))    # Los introduzco en "muestra".
            
        })
    })  
    
    
    # Fitar el modelo del orden indicado.
    observeEvent(input$pregresion, {
        
        orden<- input$orden
        output$grafica <- renderPlot({
            
            ggplot(misDatos$muestra) + geom_point(aes(x = V1, y = V2)) +xlab("Var 1")+ ylab("Var 2") + geom_smooth(method="lm", aes(x = V1, y = V2), formula=  y ~ poly(x, degree = orden),  se=FALSE, linetype = 1)
        
            })
        
    })
    
}


shinyApp(ui,server)