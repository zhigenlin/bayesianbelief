library(shiny)

ui <- fluidPage(
  
  titlePanel("Belief estimator based on Bayes' theorem"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      numericInput("occurences", "How many times has the unlikely event happened?", value = 1, min = 0, max = 10000),
      numericInput("intentional", "What is the percentage chance the event is intentional? (E.g. 4 in 100,000.)", value = 0.004, min = 0, max = 100),
      numericInput("unlikely.not.intentional", "What is the percentage chance of the unlikely event given it is not intentional? (E.g. 1 in 10,000.)", value = 0.01, min = 0, max = 1),
      numericInput("unlikely.intentional", "What is the percentage chance of the unlikely event given it is intentional?", value = 1, min = 0, max = 1)
      
    ),
    
    mainPanel(
      h3("Result"),
      textOutput("number1"),
      h3("Explanation"),
      p("Bayes' theorem is thought to roughly estimate how our beliefs change over time after seeing more evidence (see Wikipedia)."),
      p("You will need the following information to calculate the estimate:"),
      p("1. The percentage chance the event is intentional: Base this off your personal belief or possibly some prior evidence/study."),
      p("2. The percentage chance of the unlikely event given it is not intentional: Try to base this off some prior studies, e.g. the percentage chance of car brakes malfunctioning purely by accident based on a study of a million vehicles is 0.00005% (I just made that up)."),
      p("The app also includes 'the percentage chance of the unlikely event given it is intentional'. This is usually 100%, so generally don't worry about it."),
      p("Note: 'percentage chance' refers to decimal probability multipled by 100. For example, if you calculate the value to be '0.8%', just write '0.8'."),
      
    )
    
  )
)

server <- function(input, output) {
  
  output$number1 <- renderText({ 
    
    # P(x|y)=p(y|x)*p(x)/p(y)
    # P(x|y)=p(y|x)*p(x)/sum(p(y|x.i)*p(x.i))
    # P(x|y)=p(y|x)*p(x)/[p(y|x)*p(x)+p(y|not.x)*p(not.x)]
    
    # x: "It is intentional"
    # y: "Unlikely event"
    
    # number of timesevent occured
    times = input$occurences
    
    # Probability event is intentional
    p.x = input$intentional/100
    
    # Probability of unlikely event given it is intentional
    p.y.x = input$unlikely.intentional
    
    # Probability of unlikely event given it is not intentional
    p.y.given.notx = input$unlikely.not.intentional/100
    
    for (i in 1:times) {
      # Probability it is intentional given unlikely event
      # (repeated for number of times you saw the event)
      p.x = p.y.x*p.x/(p.y.x*p.x+p.y.given.notx*(1-p.x))
    }
    finaltext = paste("Based on Bayes' theorem, the estimated percentage chance that the event was intentional given the unlikely event happened ",times," time(s) is ",round(p.x*100,2),"%.",sep="")
    finaltext
  })
  
}

shinyApp(ui = ui, server = server)
