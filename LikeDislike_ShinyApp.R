#set-up
library(shiny)
#-----------------------------------------------------------------------------------------------------------------------------------------------------
# ui module

ui <- shinyUI(
  fluidPage(
    tags$b("Simple Like/Dislike functionality using reactiveValues()"), # Heading of the page
    numericInput("LikeNumber", "Like count", 100), # Number input for number of Likes
    actionButton("Likebtn", "Like"), # Like Button
    textOutput("LikebtnStatus"), # Like Button Status
    numericInput("DislikeNumber", "Input number", 25), # Number input for number of Dislikes
    actionButton("Dislikebtn", "Dislike"), #  Dislike Button
    textOutput("DislikebtnStatus") # Dislike Button Status
  )
)

#-----------------------------------------------------------------------------------------------------------------------------------------------------
# server module

server <-  function(input, output, session) {
  
  # Initialising Reactive Values
  likecounter <- reactiveValues(likecountervalue = 100) # Defining & initializing the reactiveValues object for Number of Likes
  dislikecounter <- reactiveValues(dislikecountervalue = 25) # Defining & initializing the reactiveValues object for Number of Dislikes
  LikebtnStatus <- reactiveValues(l="FALSE") # Defining & initializing the reactiveValues object for the status of Like button - TRUE/FALSE
  DislikebtnStatus <- reactiveValues(d="FALSE") # Defining & initializing the reactiveValues object for the status of DisLike button - TRUE/FALSE 
 
  
  # Event Handler for click on the Like button
  observeEvent(input$Likebtn, {
    
    # Condition when the page is neither Liked nor Disliked
    if(LikebtnStatus$l=="FALSE" & DislikebtnStatus$d=="FALSE")
    {
      likecounter$likecountervalue <- input$LikeNumber+1  # increment the Likes value by 1 and update it
      updateNumericInput(session, "LikeNumber", value = likecounter$likecountervalue)
      LikebtnStatus$l <- "TRUE" # Update the Like button status to TRUE
      output$LikebtnStatus <- renderText(LikebtnStatus$l)
    }
    
    # Condition when the page is already Liked
    else if(LikebtnStatus$l=="TRUE" & DislikebtnStatus$d=="FALSE")
    {
      likecounter$likecountervalue <- input$LikeNumber-1  # decrement the Likes value by 1 and update it
      updateNumericInput(session, "LikeNumber", value = likecounter$likecountervalue)
      LikebtnStatus$l <- "FALSE"  # Update the Like button status to FALSE
      output$LikebtnStatus <- renderText(LikebtnStatus$l)
    }
    
    # Condition when the page is already Disliked
    else if(LikebtnStatus$l=="FALSE" & DislikebtnStatus$d=="TRUE")
    {
      likecounter$likecountervalue <- input$LikeNumber+1  # increment the Likes value by 1 and update it
      updateNumericInput(session, "LikeNumber", value = likecounter$likecountervalue)
      LikebtnStatus$l <-"TRUE"  # Update the Like button status to TRUE
      output$LikebtnStatus <- renderText(LikebtnStatus$l)
      
      dislikecounter$dislikecountervalue <- input$DislikeNumber-1  # decrement the Dislikes value by 1 and update it
      updateNumericInput(session, "DislikeNumber", value = dislikecounter$dislikecountervalue)
      DislikebtnStatus$d <- "FALSE"  # Update the Dislike button status to FALSE
      output$DislikebtnStatus <- renderText(DislikebtnStatus$d)

    }
  })
  
  
  # Event Handler for click on the Dislike button
  observeEvent(input$Dislikebtn, {
    
    # Condition when the page is neither Liked nor Disliked
    if(LikebtnStatus$l=="FALSE" & DislikebtnStatus$d=="FALSE")
    {
      dislikecounter$dislikecountervalue <- input$DislikeNumber + 1  # increment the Dislike value by 1 and update it
      updateNumericInput(session, "DislikeNumber", value = dislikecounter$dislikecountervalue)
      DislikebtnStatus$d = "TRUE" # Update the Dislike button status to TRUE
      output$DislikebtnStatus <- renderText(DislikebtnStatus$d)
    }
    
    # Condition when the page is already Disliked
    else if(LikebtnStatus$l=="FALSE" & DislikebtnStatus$d=="TRUE")
    {
      dislikecounter$dislikecountervalue <- input$DislikeNumber - 1  # decrement the Dislike value by 1 and update it
      updateNumericInput(session, "DislikeNumber", value = dislikecounter$dislikecountervalue)
      DislikebtnStatus$d = "FALSE"  # Update the Dislike button status to FALSE
      output$DislikebtnStatus <- renderText(DislikebtnStatus$d)
    }
    
    # Condition when the page is already Liked
    if(LikebtnStatus$l=="TRUE" & DislikebtnStatus$d=="FALSE")
    {
      dislikecounter$dislikecountervalue <- input$DislikeNumber + 1  # increment the Dislike value by 1 and update it
      updateNumericInput(session, "DislikeNumber", value = dislikecounter$dislikecountervalue)
      DislikebtnStatus$d = "TRUE" # Update the Dislike button status to TRUE
      output$DislikebtnStatus <- renderText(DislikebtnStatus$d)
      print(DislikebtnStatus$d)
      
      likecounter$likecountervalue <- input$LikeNumber - 1     # decrement the Like value by 1 and update it
      updateNumericInput(session, "LikeNumber", value = likecounter$likecountervalue)
      LikebtnStatus$l = "FALSE" # Update the Like button status to FALSE
      output$LikebtnStatus <- renderText(LikebtnStatus$l)
      print(LikebtnStatus$l)
    }
  })
}

#-----------------------------------------------------------------------------------------------------------------------------------------------------
# Shiny app module
shinyApp(ui, server)
