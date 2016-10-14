library(shiny)
# Rely on the 'WorldPhones' dataset in the datasets
# package (which generally comes preloaded).
library(datasets)

tags$style(type="text/css",
           ".shiny-output-error { visibility: hidden; }",
           ".shiny-output-error:before { visibility: hidden; }"
)

# Define the overall UI
shinyUI(
  
  # Use a fluid Bootstrap layout
  fluidPage(    
    
    # Give the page a title
    titlePanel("Topic Count by Day"),
    
    # Generate a row with a sidebar
    sidebarLayout(      
      
      # Define the sidebar with one input
      sidebarPanel(
        selectInput("name", "Name:", 
                    choices=colnames(All_counts)[2:length(All_counts)]),
        hr(),
        helpText("Data from topics and dates")
      ),
      
      # Create a spot for the barplot
      mainPanel(
        plotOutput("countPlot")  
      )
      
    )
  )
)