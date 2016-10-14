library(shiny)

setwd("C:/Users/FuserLimon/Documents/Semester 2/Data Visualization/ps4/Shiny")
All_counts <- read.csv("ShinyApp.csv", header = TRUE, sep = ",", quote = "\"")



# Define a server for the Shiny app
shinyServer(function(input, output) {
  
  # Fill in the spot we created for a plot
  output$countPlot <- renderPlot({
    
    # Render a barplot
    barplot(All_counts[,input$name], 
            main=input$name,
            ylab="Topic Count",
            xlab="Topics",
            names.arg=c ("religion", "china", "economy", "healthcare", "gun control",
                         "race", "clim.chng", "immigr.", "military", "trade"),
            col=rainbow(10),
            las=2,
            )
  })
})  
