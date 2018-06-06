# hatgame 
# sompob@tropmedres.ac
#

library(shiny)

source("hatgame.R")

ui <- fluidPage(
   
   titlePanel("Hat Game"),
   
   sidebarLayout(
      sidebarPanel(
         textInput("npop", "Number of players", value = 30),
         textInput("R0", "R0", value = 3),
         actionButton("load.button", "load players"),
         actionButton("next.button", "Next"),
         actionButton("back.button", "Back")
      ),
      
      mainPanel(
        tabsetPanel(type = "tabs",
          tabPanel("Network",plotOutput("network")),
          tabPanel("Infected plot", plotOutput("plot.infected")),
          tabPanel("table", tableOutput("table.infected"))
         )
      )
   )
)

server <- function(input, output) {
  
  values <- reactiveValues()
  values$loaded <- FALSE
  
  npop <- eventReactive(input$npop,{as.numeric(input$npop)})
  R0 <- eventReactive(input$R0,{as.numeric(input$R0)})
  
  # click load button
  observeEvent(input$load.button,{
    values$loaded <- TRUE
    g <- randomNetwork(npop(),R0())
    state <- addInfectedNode(g)
    values$g <- g
    values$state <- state
    values$m.state <- matrix(state,nrow = 1) 
    values$time <- 0 #init time
    values$layout <- layout_nicely(g)
    values$df.infected <- data.frame(time = c(0), infected = c(1))
    output$network <- renderPlot(plotNetwork(g,values$state,values$layout))
    
  })
  
  # click next button
  observeEvent(input$next.button,{
    if(values$loaded==TRUE){
      values$state <- updateState(values$g, values$state)
      
      inf <- stateCount(values$state)[2]
      values$time <- values$time + 1
      values$m.state <- rbind(values$m.state,values$state)
      values$df.infected <- rbind(values$df.infected, data.frame(time=c(values$time),infected=c(inf)))
      
      output$network <- renderPlot(plotNetwork(values$g,values$state,values$layout))
      
      output$plot.infected <- renderPlot(plotInfected(values$df.infected))
    }
  })
  
  #click back button
  observeEvent(input$back.button,{
    if(values$loaded==TRUE){
      nrow.df.infected <- nrow(values$df.infected)
      nrow.m.state <- nrow(values$m.state)
      
      if(nrow.m.state > 2){
        values$time <- values$time - 1
        values$df.infected <- values$df.infected[1:(nrow.df.infected - 1), ]
        values$m.state <- values$m.state[1:(nrow.m.state - 1), ]
        values$state <- values$m.state[nrow(values$m.state),]
      
      }
      
      output$network <- renderPlot(plotNetwork(values$g,values$state,values$layout))
      output$plot.infected <- renderPlot(plotInfected(values$df.infected))
    }
    
  })
  
  output$table.infected <- renderTable(values$df.infected)

  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

