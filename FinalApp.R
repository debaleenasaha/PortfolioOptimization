#*****Note: Before running the Application for the first time, user needs to install all
#the following packages individually*****

#Once the packages are installed successfully, user should comment out the 
#"install.packages(<packagename>)" commands using the # symbol and run the code

#It is important to comment the installation commands since this
#may unnecesaarily hamper the launching of the application

#There are often problems in R that some packages may not get installed correctly
#when installed all at once

#install and load the requisite packages
install.packages("stockPortfolio")
library(stockPortfolio)

install.packages("broom")
library(broom)

install.packages("shinyjs")
library(shinyjs)

install.packages("magrittr")
library(magrittr)

install.packages("shiny")
library(shiny)

install.packages("ggplot2")
library(ggplot2)

install.packages("quantmod")
library(quantmod)

install.packages("xts")
library(xts)

install.packages("lubridate")
library(lubridate)

install.packages("forcats")
library(forcats)

install.packages("rvest")
library(rvest)

install.packages("devtools")
library(devtools)

install.packages("tidyverse")
library(tidyverse)

install.packages("stringr")
library(stringr)

install.packages("corrplot")
library(corrplot)

install.packages("plotly")
library(plotly)

install.packages("quadprog")
library(quadprog)

install.packages("DT")
library(DT)

install.packages("shinythemes")
library(shinythemes)

install.packages("devtools")
library(devtools)

devtools::install_github("ShinySky","AnalytixWare")
library(shinysky)

#suppress unnecessary warnings
options(warn = -1)

#the server part of the Shiny Application
server = function(input, output, session) {
  #Validation checks to show the modal
  validation = function()
  {
    #The Portfolio optimization is based on the premise that atleast 2 stocks need to 
    #be considered for analysis
    if(length(stocks_input()) < 2)
      {
        showModal(modalDialog(
          title = "Error",
          "Select at least two stocks to optimize!!!",
          easyClose = FALSE
        ))
      }
        if(input$startdate>input$enddate)
        {
          #the end date should be greater than the start date
          showModal(modalDialog(
            title = "Error",
            "Start date should be less than end date!!! Resetting the dates!",
            easyClose = TRUE
          ))
          
        }
      if(input$enddate-input$startdate<60)
      {
        #the quadratic optimizer(used below) requires sufficient number of observations
        #to be able to run the optimizing algorithm
        showModal(modalDialog(
          title = "Error",
          "Too few observation for performing optimization. 
          Please ensure date range for analysis to be at least 60 working days!",
          easyClose = TRUE
        ))
        
      }
  }
  
  #Taking stock names from the user input
  stocks_input <- reactive({
    if(input$btnGenerate == 0)
      return()
    return(input$inputstocks) 
   })
  
  #Load the data for monthly returns 
  data.plt_mnthlyRtrn <- reactive({
    #Checking validation errors(Start)
    if(input$btnGenerate == 0)
      return()
    if(length(stocks_input()) < 2)
      return()
    if(input$startdate>input$enddate)
      return()
    if(input$enddate-input$startdate<60)
      return()
    #Checking validation errors(End)
    stocksdata = lapply(stocks_input(), function(sym) {
      monthlyReturn(na.omit(getSymbols(sym, from=Sys.Date()-365, auto.assign=FALSE)))
    })
    stocksdata <- do.call(merge,stocksdata)
    names(stocksdata) <- stocks_input()
    index(stocksdata) <- as.Date(index(stocksdata))
    return(stocksdata)
  })
  
  # Displaying reference table for stocks
  output$allstockdata <- DT::renderDataTable({stockdetails}
     ,server = FALSE, rownames = FALSE,selection = "none",
     options = list(scrollY = '300px', paging = FALSE)
  )
  
  #Generating plot for monthly returns using the data loaded in data.plt_mnthlyRtrn function
  output$plt_mnthlyRtrn <- renderPlot({
    #Checking validation errors(Start)
    if(input$btnGenerate == 0)
      return()
    if(length(stocks_input()) < 2)
      return()
    if(input$startdate>input$enddate)
      return()
    if(input$enddate-input$startdate<60)
      return()
    #Checking validation errors(End)
    isolate(tidy(data.plt_mnthlyRtrn()) %>% ggplot(aes(x=index,y=value, color=series)) + 
      geom_line() +
      ggtitle("Monthly Returns for Last One Year") +
      theme(plot.title = element_text(face = "bold",
                                      color="black", 
                                      size=20, 
                                      hjust=0.5))+
      labs(x="Time Period", y = "Returns")
    )
  
  })
  
  #Populating drop down to select individual stock performance
  output$select_stocks <- renderUI({
    #Checking validation errors(Start)
    if(input$btnGenerate == 0)
      return()
    if(length(stocks_input()) < 2)
      return()
    if(input$startdate>input$enddate)
      return()
    if(input$enddate-input$startdate<60)
      return()
    #Checking validation errors(End)
    selectInput("analyzestock", "Select stock to analyze:",stocks_input())
    
  })
  
  #get the name of the stock selected from dropdown
  #for individual stock performance
  txt <- reactive({
    input$analyzestock
  })
  #Generating data for individual plot performance after capturing the name of the stock
  #in the txt reactive variable
  data.plt_individual <- reactive({
    #Checking validation errors(Start)
    if(input$btnGenerate == 0)
         return()
    if(input$startdate>input$enddate)
      return()
    if(input$startdate>input$enddate)
      return()
    if(input$enddate-input$startdate<60)
      return()
    if(is.null(txt()))
      return()
    #Checking validation errors(End)
    stock.data <- getSymbols(Symbols = txt(),
                                from = input$startdate,
                                to = input$enddate,
                                auto.assign = FALSE)
       return(stock.data)
     })
      
  #Generating plots for the Adjeusted Returns of the individual stock selected
  output$plt_individual <- renderPlot({
    if(is.null(data.plt_individual()))
      return()
    data.plt_individual() %>% chartSeries(TA='addVo();
                           addBBands();
                           addBBands(draw="p");
                           addMACD()', 
                           theme="white"
               )
    
  })
  
  #Reactive function to calculate the Portfolio Weights, Expected return, Volatility(risk)
  # and the Sharpe Ratio
  #Also finding the Optimal Allocation % weights based on Sharpe ratio
  data.plt_shrpRatio <- reactive({
    #Checking validation errors(Start)
    if(input$btnGenerate == 0)
       return()
    if(length(stocks_input()) < 2)
      return()
    if(input$startdate>input$enddate)
      return()
    if(input$enddate-input$startdate<60)
      return()
    #Checking validation errors(End)
    #Get the daily returns for each of the input stocks
    stocks.returns <- getReturns(stocks_input(), 
                                 freq="day",
                                 start = input$startdate,
                                 end = input$enddate)
    #stocks.returns will have Open,High,Low,Close and Adjusted Prices alongwith the Volume
    #Extracting the Adjusted returns for each of the stocks for the period
    #under consideration
    returns.data <- stocks.returns$R
    #efficient frontier function to get the optimized allocations
    #the increment value of risk is use to calculate the number of times to run
    #the optimizer
    efficient.frontier <- function (data, incr.value=.002){
      #calculate covariance. This will be the matrix in the quadratic function to be
      #minimized
      covar <- cov(data)
      #print(covar)
      #get the number of stocks. This will be useful in future calculations and checking
      #for correct value of maximum allocation percentages
      n <- length(stocks_input())
      #Initialize the matrix defining the constraints to be used for minimizing the quadratic function
      constraints.matrix <- matrix (1, nrow=n)
      #initialize the vector against whom the contraints matrix will be compared
      b_intercept <- 1
      # define the column in constraints matrix which is to be used as an equality constraint
      #for our analysis we are considering only one equality constraint that the sum of weights 
      #assigned to the different stock returns should be equal to 1
      equality.constraint.col <- 1
      #defining the constraints matrix and weights vector
      constraints.matrix <- cbind(1, diag(n))
      b_intercept <- c(b_intercept, rep(0, n))
      max.alloc.pct = (1/n)+0.3
      #after running the checks, modify the constraints matrix and the b_intercept
      constraints.matrix <- cbind(constraints.matrix, -diag(n))
      b_intercept <- c(b_intercept, rep(-max.alloc.pct, n))
      
      #Calculate the number of times the quadratic optimizer will run using
      #the maximum allowable risk and the risk incremental value
      #We can vary the maximum risk depending on USer's choice
      max.risk = 0.6
      times.optimize <- (max.risk / incr.value) + 1
      #initializing a counter to assist in assigning the allocated weights,return
      #volatility and sharpe ratio for each iteration
      counter <- 1
      #Initialize the matrix to store the calcuated weights during optimisation as well
      #as the corresponding expected return, volatility and sharpe ratio 
      efficient.frontier.matrix <- matrix(nrow=times.optimize, ncol=n+3)
      colnames(efficient.frontier.matrix) <- c(colnames(data), "Volatility", "ExpectedReturn", "SharpeRatio")
      #Execute the loop for quadratic optimisation for implementing the efficient frontier algorithm
      for (i in seq(from=0, to=max.risk, by=incr.value)){
        #calculate the vector in the quadratic function to be minimized
        #this is the average returns for each stock
        vector.minimize <- colMeans(data) * i 
        #execute the quadratic optimizer function
        quad.opt <- solve.QP(covar, 
                             dvec=vector.minimize, 
                             Amat=constraints.matrix, 
                             bvec=b_intercept, 
                             meq=equality.constraint.col)
        #the optimized weights will be in the solution parameter of the optimizer object
        #we assume that the total number of Trading days is 252
        #We have assumed the Risk free rate to be 0 for simplifying calculation
        #Future scope can be to calculate the Sharpe ratio with the risk free rate in consideration 
        efficient.frontier.matrix[counter,1:n] <- quad.opt$solution
        efficient.frontier.matrix[counter,"Volatility"] <- sqrt(sum(quad.opt$solution *colSums((covar * quad.opt$solution)))*252)
        efficient.frontier.matrix[counter,"ExpectedReturn"] <- as.numeric(quad.opt$solution %*% colMeans(data))*252
        efficient.frontier.matrix[counter,"SharpeRatio"] <- efficient.frontier.matrix[counter,"ExpectedReturn"] / (efficient.frontier.matrix[counter,"Volatility"])
        #increment counter for next row of optimized values
        counter = counter+1
      }
      #return the final dataframe containing the optimized weights and corresponding assessment metrics
      return(as.data.frame(efficient.frontier.matrix))
    }
    #the choice incremental value is critical here
    #Too high a value will have less number iterations for the solver
    #To low a value will result in too many iterations
    optimized.values <- efficient.frontier(data=returns.data,
                                           incr.value=.001)
    #convert the allocations take absolute values
    for(i in 1:length(stocks_input())){
      optimized.values[,i] <- optimized.values[,i] * 100
    }
    optimized.values <- abs(optimized.values)
    return(optimized.values)
    
  })
  
  #Extract the data for plotting the Sharpe Ratio using the calculated 
  #Expected Returns and Volatility(Risk)
  plotdata.plt_shrpRatio <- reactive({
    #Checking validation errors(Start)
    if(input$btnGenerate == 0)
       return()
    if(length(stocks_input()) < 2)
      return()
    if(input$startdate>input$enddate)
      return()
    if(input$enddate-input$startdate<60)
      return()
    #Checking validation errors(End)
    
    #Displaying stock allocation and generating sharpe ratio plot
    optimal.allocation <- data.plt_shrpRatio()[data.plt_shrpRatio()$SharpeRatio==max(data.plt_shrpRatio()$SharpeRatio),]
    p <- ggplot(data.plt_shrpRatio(), aes(x=Volatility, y=ExpectedReturn,label = SharpeRatio))+
      geom_point(alpha=.2, color= "dodgerblue1")+
      geom_point(data=optimal.allocation, color="blue3", size=6)+
      ggtitle("Optimized Portfolio")+
      labs(x="Volatility", y="Expected Return")+
      theme(panel.background=element_rect(fill="gray97"), text=element_text(color="black"),
            plot.title=element_text(size=18,hjust = 0.5 ,color="black",face = "bold"))+
      theme(axis.title.y=element_text(hjust=0.4))
    return(p)
    
  })
  
  #Plotting the Sharpe ratio
  output$plt_shrpRatio <- renderPlotly({
    #Checking validation errors - Start
    if(input$btnGenerate == 0)
       return()
    if(length(stocks_input()) < 2)
      return()
    if(input$startdate>input$enddate)
      return()
    if(input$enddate-input$startdate<60)
      return()
    #Checking validation errors - End
    isolate(
    ggplotly(plotdata.plt_shrpRatio())
    )
    
  })
  
  #Displaying the optimal allocation of weights, Expected Returns, Volatility and Sharpe Ratio
  output$summary <- renderTable({
    #Checking validation errors - Start
    if(input$btnGenerate == 0)
       return()
    if(length(stocks_input()) < 2)
      return()
    if(input$startdate>input$enddate)
      return()
    if(input$enddate-input$startdate<60)
      return()
    #Checking validation errors - End
    isolate({
    optimal.allocation <- data.plt_shrpRatio()[data.plt_shrpRatio()$SharpeRatio==max(data.plt_shrpRatio()$SharpeRatio),]
    optimal.allocation[1,]
    })
  })
  #Resetting input stocks text box on clicking 'clear' button
  observeEvent(input$reset_input, {
    reset("inputstocks")
  })
  #Calling validation() on clicking Genertate plot button
  observeEvent(input$btnGenerate, {
    validation()
    return()
  })

}#server code ends

#suppress unnecessary warnings
options("getSymbols.warning4.0"=FALSE)
#fetch list of stocks and associated tickers
stockdetails <- stockSymbols()
#sort by Symbol
p = order(xtfrm(stockdetails$Symbol))
stockdetails <- stockdetails[p,]
#select only the Symbol and Name of stocks
stockdetails <- select(stockdetails,c(Symbol,Name))
#it was observed that some stock names have duplicate tickers. We should select only one
stockdetails <- subset(stockdetails,!duplicated(stockdetails[,2]))

#the user-interface part of the Shiny Application
ui <- fluidPage(
  useShinyjs(),
  #Displaying busy indicator while waiting to load data
  busyIndicator(text = "Loading, please wait...", wait = 4),   
  theme = shinytheme("sandstone"),
  br(),
  titlePanel('Portfolio Optimization'),
  br(),
  #Displays sidebar panels and it's contents
  sidebarPanel(
    style ='overflow-y: auto; max-height: 600px;',
     
    helpText("Choose the Ticker Symbol, Start and End Dates"),
    selectInput("inputstocks","Select Stocks for Portfolio:",
                choices = as.factor(stockdetails[,1]), multiple = TRUE),
    verbatimTextOutput("dfStr"),
    actionButton("reset_input", "Clear"),
    HTML(paste0("<br><br><b>","Reference table to find stocks:","</b>")),
    DT::dataTableOutput('allstockdata'),
    br(),
    helpText("(Range between start date and end date should be minimum 60 working days)"),
    dateInput("startdate","Start Date:",value = Sys.Date()-365),
    dateInput("enddate","End Date:",value = Sys.Date()),
    actionButton("btnGenerate", "Generate Plots",icon("line-chart"))
                 
  ),
  #Displays the tabs and contents of the tabs
  mainPanel(
    style ='overflow-y: auto; ',
    tabsetPanel(type = 'tabs',
                tabPanel("Monthly Returns",
                         fluidRow(
                         column(8,helpText("The below plot shows monthly return of all the selected stocks based on their performance over last 1 year.")),
                         column(8,helpText("For individual stock performance click on the next tab 'Individual Stock Performance'.")),
                         column(8,helpText("For optimized portfolio click on the tab 'Optimized Portfolio'.")),
                         column(12,plotOutput("plt_mnthlyRtrn",width = "100%"))
                         
                         )
                ),
                tabPanel("Individual Stock Performance",
                         fluidRow(
                         column(8,uiOutput("select_stocks")),
                         column(12,plotOutput("plt_individual",width = "100%"))
                         )
                         
                ),
                tabPanel("Optimized Portfolio",
                         fluidRow(
                           column(8,helpText("Optimal Allocation Weights:")),
                           column(10,tableOutput("summary")),
                           column(8,plotlyOutput("plt_shrpRatio"))
                         )
                  
                )
                
    )#tabsetPanel ends
  )#mainPanel ends
)#ui function ends

#the following code will run the Shiny Application
shinyApp(ui = ui, server = server) 

