#
################# Finance Strategy:  Bollinger Pops and Sinks  ##################################
#

library(shiny)
library(DT)
library(data.table)
source("bollingerPops.R")
source("stratDeathGoldenCross.R")

ui <- shinyUI(pageWithSidebar(
  
  # Application title
  h3("Bollinger Pops/Sinks"),
 
    sidebarPanel(
    div(style="display:inline-block;width:50px",HTML("<br>")),
    div(style="display:inline-block;width:100px;vertical-align:top",textInput("symb", "Enter Symbol:", "NGL")),

        br(),
    # helpText("OR Pick Sector:"),
    radioButtons("sector", "OR Pick Sector Type:", list("S&P" = "SPY",  "Metals" = "XME", "Finance" = "XLF", "Tech" = "XLK","Retail" = "XRT")),
    sliderInput("n", "Number of trading days:", value = 30, min = 20, max = 50),
    div(style="display:inline-block;width:100px",checkboxInput("goButton", "Update Market")),
    div(style="display:inline-block;width:50px",checkboxInput("filterStocks", "Filter Stocks < $10")),
    submitButton("Update View")
   
  ),

  # Show a tabset that includes a plot, summary, and table view
 
    
  
  mainPanel(
    tabsetPanel(
      tabPanel("Market Pops", dataTableOutput("pops")), 
      tabPanel("Market Sinks", dataTableOutput("sinks")), 
      tags$head(tags$style("#description{color: purple; font-size: 15px;  font-style: italic;strong ;}" ) ),
      tabPanel("Asset",textOutput("description"),br(),dataTableOutput("table"),br(),helpText("Random Forest Preditive Probablities:"),dataTableOutput("asset"))
         )
  )
))

# Define server logic required to draw a histogram
server <- shinyServer(function(input, output) {
 
  ticker<- function() {dt=data.table(init()) ;setkey(dt,"symbol"); dt}
  
  processAsset <- reactive({  
    dist <- switch(input$sector, XME = rnorm, XLE = runif,  XLF = rlnorm,  XLK = rexp, XRT = rexp, rnorm)
    data = processData(input$symb,Sys.Date(),input$n)
    return(data[order(data[,1],decreasing = T),])
  })
  
  asset <- reactive({  
    processRF(input$symb)
  })
  
    all <- reactive({
    dt = as.data.table(processAll())
    # if(input$filterStocks) dt = dt[close>10,]
    return(dt)
  })
  output$plot <- renderPlot({
    dist <- input$dist
    n <- input$n
    
    # hist(data(),  main=paste('r', dist, '(', n, ')', sep=''))
  })
  
  output$pops <- DT::renderDataTable({
    
    withProgress(message = 'Making plot: Pops',{
   
    if(input$goButton) {dt = all()[changeP>0,] } 
    if(input$filterStocks) dt=dt[close>10,] 
    }) # Progress Bar 
    
    dt
  })
  
  output$sinks <- DT::renderDataTable({
    withProgress(message = 'Making plot: Sink',{
   if(input$goButton) {dt = all()[changeP<0,];dt =dt[order(-dates,changeP)] }
    if(input$filterStocks) dt=dt[close>10,] 
    }) # Progress Bar 
    dt
  })
  
  # Generate an HTML table view of the data - Asset
  output$description <- renderText({ paste("Asset: ",ticker()[toupper(input$symb)==symbol,Name]," -:: ",ticker()[toupper(input$symb)==symbol,Sector]) })
  output$table <- DT::renderDataTable({ (processAsset())   }, options = list(pageLength = 5,searching=F, fitering=F))
  output$asset <- DT::renderDataTable({    dt=(processRF(input$symb)); dt } ,options = list(searching=F, fitering=F,autoWidth = TRUE,columnDefs=list(list(className='dt-center',targets=c(1:4) ),list(width='100px',targets=c(1:4) ))))
})

# Run the application 
shinyApp(ui = ui, server = server)

