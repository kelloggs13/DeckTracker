require(shiny)
require(shinyWidgets)

ui <- fluidPage(
  
  hr(),
  
  actionBttn('addcard', 'Card', icon("plus"), color = "success", style = "gradient"),
  actionBttn('subcard', 'Card', icon("minus"), color = "success", style = "gradient"),
  
  br(),
  
  actionBttn('adddraw', 'Add Draw', icon("plus"), color = "primary", style = "gradient"),
  actionBttn('subdraw', 'Subtract Draw', icon("minus"), color = "primary", style = "gradient"),
  
  br(),
  
  actionBttn('addaction', 'Add Action', icon("plus"), color = "warning", style = "gradient"),
  actionBttn('subaction', 'Subtract Action', icon("minus"), color = "warning", style = "gradient"),
  
  hr(),
  
  textOutput("cards"),
  textOutput("draws"),
  textOutput("actions"),
  
  hr(),
  
  textOutput("perc_draws"),
  textOutput("perc_actions"),
  
  hr(),
  
  actionBttn('reset', 'Reset All', icon("power-off"), color = "royal", style = "gradient"),
  
  hr()
)


server <- function(input, output, session) {
  
  # set initial values for counters
  init.counter.cards <- 10
  init.counter.draws <- 0
  init.counter.actions <- 0
  
  # initialize counters
  counter.cards <- reactiveValues(countervalue = init.counter.cards)
  counter.draws <- reactiveValues(countervalue = init.counter.draws)
  counter.actions <- reactiveValues(countervalue = init.counter.actions)
  
  # observe and change counters
  observeEvent(input$addcard, {
    counter.cards$countervalue <- counter.cards$countervalue + 1
  })
  observeEvent(input$subcard, {
    counter.cards$countervalue <- counter.cards$countervalue - 1
  })
  
  observeEvent(input$adddraw, {
    counter.draws$countervalue <- counter.draws$countervalue + 1
  })
  observeEvent(input$subdraw, {
    counter.draws$countervalue <- counter.draws$countervalue - 1
  })
  
  observeEvent(input$addaction, {
    counter.actions$countervalue <- counter.actions$countervalue + 1
  })
  observeEvent(input$subaction, {
    counter.actions$countervalue <- counter.actions$countervalue - 1
  })
  
  # output current values of counterss
  output$cards <- renderText({
    paste("Number of cards ", counter.cards$countervalue)    
  })
  output$draws <- renderText({
    paste("Number of draws ", counter.draws$countervalue)   
  })
  output$actions <- renderText({
    paste("Number of actions ", counter.actions$countervalue)  
  })
  
  # initialize percentagess
  perc.draws <- reactiveValues(value = 0)
  perc.actions <- reactiveValues(value = 0)
  
  # observe and update percentages
  observeEvent(c(input$addcard, input$subcard, input$adddraw, input$subdraw),
               {perc.draws$value <- counter.draws$countervalue / counter.cards$countervalue
               })
  
  observeEvent(c(input$addcard, input$subcard, input$addaction, input$subaction),
               {perc.actions$value <- counter.actions$countervalue / counter.cards$countervalue
               })
  
  # output current values of percentages
  output$perc_draws <- renderText({
    perc.draws$value  
  })
  output$perc_actions  <- renderText({
    perc.actions$value  
  })
  
  
  # reset all counters
  observeEvent(input$reset, {
    counter.cards$countervalue <- init.counter.cards
    counter.draws$countervalue <- init.counter.draws
    counter.actions$countervalue <- init.counter.actions
    perc.draws$value <- 0
    perc.actions$value <- 0
  })
  
}



shinyApp(ui, server)