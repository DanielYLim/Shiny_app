# Import libraries
library(shiny)
library(shinythemes)
library(ggplot2)

hazardLL<-function(alpha1,beta1,t){
  ((beta1/alpha1)*(t/alpha1)^(beta1-1))/(1+(t/alpha1)^beta1)
}

ui <- fluidPage(theme = shinytheme("united"),
  
  # Page header
  headerPanel('Log-Logistic Hazard Function'),
  
  # Input values
  sidebarPanel(
    HTML("<h3>Input parameters</h3>"),
    
    sliderInput("alpha1", "alpha1:",
                min = 0, max = 15,
                value = 10),
    sliderInput("beta1", "beta1:",
                min = 0, max = 15,
                value = 10),
    
   # actionButton("submitbutton", "Submit", class = "btn btn-primary")
  ),
  
  mainPanel(
    tags$label(h3('Status/Output')), # Status/Output Text Box
    verbatimTextOutput('contents'),
   	plotOutput("varPlot")
    #tableOutput('tabledata') # Prediction results table
  )
)

####################################
# Server                           #
####################################

server <- function(input, output, session) {



  # Input Data
  datasetInput <- reactive({  
    
  alpha = input$alpha1
  beta = input$beta1

  x = seq(0,30, 0.5)
  y = hazardLL(alpha,beta,x)

  df = data.frame(x=x,y=y)

  #print(df)
  
  })

   
  
  output$varPlot <- renderPlot({ 
   
	ggplot() +
  	geom_line(data = datasetInput(), aes(x, y))

  })

  #  # Prediction results table
  # output$tabledata <- renderTable({
  #   if (input$submitbutton>0) { 
  #     isolate(datasetInput()) 
  #   } 
  # })


  
}

####################################
# Create the shiny app             #
####################################
shinyApp(ui = ui, server = server)
