library (shiny)
library (ggplot2)
library(shinythemes)
library(dygraphs)
library(CausalImpact)
# library(devtools)
# install_github("rga", "skardhamar")
library(rga)
library(tseries)




ui <- fluidPage(theme = shinytheme("flatly"),
    sidebarLayout(
        
        
        sidebarPanel(
            
            
            selectInput("dataset", label = h3("Dataset"),
                        choices = list("Volkswagen Stocks" = "WVN", "Prepared Random Serie" = "W", "Delay" = "D", "Your Own Analytics Account" = "A", "Analytics Sample" = "Sample"),
                        selected = "A"
                        ),                 # Tamaño de la muestra
            conditionalPanel(
                condition = "input.dataset=='A'",
                uiOutput("AuthGAURL")
                
            ),
            dateRangeInput("startDate", label = h5("Date range"),start = Sys.Date()-365,
                           end = Sys.Date()),
            conditionalPanel(
                condition = "input.dataset =='Sample'",
                dateInput("eventDate", label = h3("Start Day of event"), value = "2017-12-15")
                
            ),
            conditionalPanel(
                condition = "input.dataset =='WVN'",
                dateInput("eventDate2", label = h3("Start Day of event"), value = "2015-09-21")
                
            ),
            conditionalPanel(
                condition = "input.dataset =='W'",
                dateInput("eventDate3", label = h3("Start Day of event"), value = "2014-03-11")
                
            ),
            
            
            h3("End Day of event"),
            selectInput("end", label = h5("Criteria:"),
                        choices = list("Fixed interval" = "Fix", "Accumulated impact reaches" = "Reaches", "Impact is less than" = "Less"),
                        selected = "Fix"
                        ),
            conditionalPanel(
                condition = "input.end=='Fix'",
                numericInput("fixedDays", label=h5("Days"), value = 1)
                
            ),
            conditionalPanel(
                condition = "input.end=='Reaches'",
                numericInput("upperDays", label=h5("Upper limit"), value = 2345)
                
            ),
            conditionalPanel(
                condition = "input.end=='Less'",
                numericInput("lowerDays", label=h5("Lower limit"), value = 2345)
                
            )
            
            
        ),
        mainPanel(
            fluidRow(
                
                navbarPage("Steps",
                           tabPanel("1. Prepare Dataset / Set options",value="Prepare",
                                    h5(" In this first step you can try and explore the possible options and choose, with the aim of center the research."),
                                    h5("Mandatory step if you choose your own Google Analytics account."),
                                    h5(HTML("&nbsp")),
                                    h3(" Number of dates (days or weeks) in the time serie."),
                                    verbatimTextOutput("statsDataset"),
                                    h3(" Explanation of the Time Serie and the Impact."),
                                    textOutput("explanation"),
                                    h3(" Time Serie and preview of the Impact Effect"),
                                    plotOutput("prueba", click = "pruebaClick"),
                                    plotOutput("preparePlot", click="preparePlotClick")
                                    
                           ),
                           tabPanel("2. Results",value="Results",
                                    
                                    plotOutput("mainPlot", click="mainPlotClick"),
                                    verbatimTextOutput("totalImpact"),
                                    verbatimTextOutput("daysRecovery")
                                    
                           )
                           
                           
                )
                
            )
        )
        
    )
)




server <- function(input, output){
    
##################################  Google Analytics Dataset  ################################
    ## auth.r
    CLIENT_ID      <-  ##
    CLIENT_SECRET  <-  ##
    CLIENT_URL     <-  ##

    
    ### Authentication functions
    
    ## generate the URL the user clicks on.  
    ## The redirect URL is then returned to with the extra 'code' and 'state' URL parameters appended to it.
    ShinyGetTokenURL <- function(client.id     = CLIENT_ID,
                                 client.secret = CLIENT_SECRET,
                                 redirect.uri  = CLIENT_URL) {
        
        url <- paste('https://accounts.google.com/o/oauth2/auth?',
                     'scope=https%3A%2F%2Fwww.googleapis.com%2Fauth%2Fanalytics+',  ## plus any other scopes you need
                     'https%3A%2F%2Fwww.googleapis.com%2Fauth%2Fanalytics.readonly&',
                     'state=securitytoken&',
                     'redirect_uri=', redirect.uri, '&',
                     'response_type=code&',
                     'client_id=', client.id, '&',
                     'approval_prompt=auto&',
                     'access_type=online', sep='', collapse='');
        return(url)
    }
    
    ## gets the token from Google once you have the code that is in the return URL
    ShinyGetToken <- function(code,
                              client.id     = CLIENT_ID,
                              client.secret = CLIENT_SECRET,
                              redirect.uri  = CLIENT_URL){
        
        token <- MErga.authenticate(client.id = client.id,
                                    client.secret = client.secret,
                                    code = code,
                                    redirect.uri = redirect.uri);
        
        return(token)
    }
    
    ## posts your code to google to get the current refresh
    MErga.authenticate <- function(client.id, client.secret, code, redirect.uri) {
        opts <- list(verbose = FALSE);
        raw.data <- postForm('https://accounts.google.com/o/oauth2/token',
                             .opts = opts,
                             code = code,
                             client_id = client.id,
                             client_secret = client.secret,
                             redirect_uri = redirect.uri,
                             grant_type = 'authorization_code',
                             style = 'POST');
        
        token.data <- fromJSON(raw.data);
        now <- as.numeric(Sys.time());
        token <- c(token.data, timestamp = c('first'=now, 'refresh'=now));
        
        return(token);
    }
    #### end auth.r
    
    #### Then in Shiny these are the appropriate server.r and ui.r functions
    ##
    ## server.r
    #

        
        ### Authentication Functions 
        ##
        ##   AuthCode() - checks for presence of code in URL
        ##   AccessToken() - creates a token once a code is available
        ##   ShinyMakeGAProfileTable - the table of profiles taken from API
        ##   output$AuthGAURL - creates the authentication URL
        ##   output$GAProfile - table of the profiles belonging to user
        
        AuthCode  <- reactive({
            
            ## gets all the parameters in the URL. Your authentication code should be one of them
            pars <- parseQueryString(session$clientData$url_search) 
            
            if(length(pars$code) > 0){
                return(pars$code)
            } 
        })
        
        AccessToken <- reactive({ 
            validate(
                need(AuthCode(), "Authenticate To See")
            )
            access_token <- ShinyGetToken(code = AuthCode())
            
            token <- access_token$access_token
        })
        
        output$AuthGAURL <- renderUI({
            a("Click Here to Authorise Your Google Analytics Access", href=ShinyGetTokenURL())
        })
        
        ShinyMakeGAProfileTable <- reactive({
            
            token <- AccessToken()
            
            ### ... do your call to the Google API with the token .. etc.
        })
        output$prueba <- renderText({
            print(rga)
        })
        
################################# MAIN PLOT ####################################
        # in server.r of a shiny app
        casualImpactData <- reactive({

            ## only if we have the data ready
            validate(
                need(chartData(), "Need data")
            )

            data  <- chartData()

            ## from user input in ui.r
            start  <- input$startDate[1]
            end    <- input$startDate[2]
            event  <- input$eventDate
            #season <- as.numeric(input$season)

            ## setting up the necessary data for CausalImpact
            pre.period  <- as.Date(c(start, event))
            post.period <- as.Date(c(event + 1, end))

            ## doing the CausalImpact call and creating the model data
            CausalImpact(data, pre.period, post.period)#, model.args = list(nseasons = season))

        })
    
        library(CausalImpact)
        

        
        # matplot(data, type = "l")
        
 react <- reactiveValues(data = NULL, pre.period=NULL,post.period=NULL)       

        
observeEvent(input$dataset,{

        if(input$dataset == "Sample"){
            data <- read.csv("./datasetAnalytics.csv", sep=",", header = T)
            colnames(data) <- c("date","session")
            data$date<- as.Date(data$date, format = "%d/%m/%y")
            react$data <- zoo(data$session, as.Date(data$date))
            react$pre.period <- as.Date(c("2017-11-01", "2017-12-15"))
            react$post.period <- as.Date(c("2017-12-16", "2017-12-26"))
            # impact <- CausalImpact(data, pre.period, post.period)
            # 
            # plot(impact)
            output$explanation <- renderText({
                print(" This is my Google Analytics searchs in my webpage")
            })
        }
  
        if(input$dataset == "W"){
    set.seed(1)
    x1 <- 100 + arima.sim(model = list(ar = 0.999), n = 100)
    y <- 1.2 * x1 + rnorm(100)
    y[71:100] <- y[71:100] + 10
    time.points <- seq.Date(as.Date("2014-01-01"), by = 1, length.out = 100)
    
    react$data <- zoo(cbind(y, x1), time.points)
        react$pre.period <- as.Date(c("2014-01-01", "2014-03-11"))
        react$post.period <- as.Date(c("2014-03-21", "2014-04-10"))
        # impact <- CausalImpact(data, pre.period, post.period)
        # plot(impact)
        }
        
    if(input$dataset == "WVN"){
        start = "2011-01-03"
        end = "2017-03-20"
        quote = "AdjClose"
        VolksWagen <- get.hist.quote(instrument = "VOW.DE", start, end, quote, compression = "w")
        BMW <- get.hist.quote(instrument = "BMW.DE", start, end, quote, compression = "w")
        Allianz <- get.hist.quote(instrument = "ALV.DE", start, end, quote, compression = "w")
        series <- cbind(VolksWagen, BMW, Allianz)
        react$data <- series
        
        # colnames(series) <- c("VolksWagen", "BMW", "Allianz")
        # autoplot(series, facet = NULL) + xlab("") + ylab("Adjusted Close Price")
        #
        react$pre.period <- as.Date(c(start,"2015-09-21"))  
        react$post.period <- as.Date(c("2015-09-21", end))
        
        output$explanation <- renderText({
            print(" This is the Volsvaguen stock time serie. We can see the impact of the the 
                  Emissions Scandal that broke on Friday the 18th of September 2015.")
        })
        # impact<- CausalImpact(series, pre.period, post.period, model.args = list(niter = 50, nseasons = 52))
        # plot(impact)
    }
    
      
        
})      
        
  observeEvent(input$eventDate,{
      if(input$eventDate != "2017-01-01"){
      react$pre.period[2] <- as.Date(input$eventDate)
      react$post.period[1] <- as.Date(input$eventDate)+1
      }
  })      
  observeEvent(input$eventDate2,{
      if(input$eventDate != "2017-01-01"){
          react$pre.period[2] <- as.Date(input$eventDate2)
          react$post.period[1] <- as.Date(input$eventDate2)+1
      }
  }) 
  observeEvent(input$eventDate3,{
      if(input$eventDate != "2017-01-01"){
          react$pre.period[2] <- as.Date(input$eventDate3)
          react$post.period[1] <- as.Date(input$eventDate3)+1
      }
  }) 
output$statsDataset <- renderPrint({
    len <- length(index(react$data))
    print(len)
})
output$prueba <- renderPlot ({
    
    impact <- CausalImpact(react$data, react$pre.period, react$post.period)
    plot(impact, "original")
})
        
output$mainPlot <- renderPlot ({
    
    impact <- CausalImpact(react$data, react$pre.period, react$post.period)
    plot(impact)
})        
        
        
        
}




shinyApp(ui=ui, server = server)