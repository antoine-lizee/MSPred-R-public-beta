# MSPred  CLinician app
# This script defines the global variables and other general conventions used throughout the app.
# 
# Copyright Antoine Lizee 03/2016 antoine.lizee@gmail.com. See the license included in the project.



# Initialization ----------------------------------------------------------

source("server/utilities.R", local = TRUE) # local = TRUE is actually not necessary here, just a bit cleaner (no need to go up to fetch the utilities from the global env.)

# Initialization of the db
con <- getCon() # see line 30
initializeDB() # Warning: We need the con here
# # Reset code:
# dbDisconnect(con)
# file.remove(userDBName)


# Server definition -------------------------------------------------------

shinyServer(function(input, output, session) {
  
  ### Session management ####################
  
  ## Geting the connection.
  # Three solution: 
  #   - sourcing the utilities here (non optimal launching of the session), or copying and attaching a "utilities" environment created earlier
  #   - using one connection for all (outside the server function), 
  #   - passing around the connection. 
  #   We use the second one for now, relying on the single-threaded operation of the app. (shiny-server non-pro)
  
  #   con <- getCon()
  #   sendDEBUG("Opening connection")
  #   browser()
  #   # Closing connection
  #   session$onSessionEnded(function(){
  #     sendDEBUG("Closing db connection")
  #     #     closeCon(con)
  #   })
  
  t0 <- Sys.time()
  
  ### DATA Unpacking ####################
  
  Clinician <- reactive({input$s_Clinician})
  # `visitShown` is a placeholder for a FULL visit, with the CV, the characterisitcs, and the timestamps.
  # It should be the only interface with the `backend` functions.
  rv <- reactiveValues(loggedInClinician = "",
                       visitShown = currentVisit0)
  
  
  ### OUTPUTS ##################
  
  output$raterIdentity <- renderText({ 
    sprintf("Current Rater: %s", 
            ifelseFun(clinicianNames[rv$loggedInClinician], 
                      "[not logged in]",
                      is.na)) })
  
  output$nextButton <- renderUI({
    if (rv$loggedInClinician != "") {
      actionButton("b_Next", "Validate Score")
    } else { NULL }
  })
  
  renderMetric <- function(value, name) {
    value <- ifelseFun(value, "[Not logged in]", is.null)
    renderUI({
      list(
        column(8, sprintf("%s : ", name), align = "right", class = "col-xs-9", 
               style = "padding-right: 5px !important;"), # margin: 0 !important;
        column(4, value, align = "left", class = "col-xs-3", 
               style = "padding-left: 5px !important;"))
    })
  }
  
  observe({
    visit <- rv$visitShown 
    output$Vaae <- renderMetric(visit$AgeAtExam, "Age")
    output$Vdd <- renderMetric(visit$DiseaseDuration, "Disease Duration")
    output$Vdc <- renderMetric(visit$DiseaseCourse, "Disease Course")
    output$Vedss <- renderMetric(visit$EDSS, "EDSS")
    output$Vmsss <- renderMetric(visit$MSSS, "MSSS")
    output$Vt25 <- renderMetric(visit$T25FW, "Timed 25 foot walk")
    output$Vnhpt <- renderMetric(visit$NHPT, "Nine hole Peg Test")
    updateSliderInput(session, "i_Score", value = 0.5)
  })
  
  ### PLOT  ####################
  
  output$scorePlot <- renderPlot({
    scoresDf <- getScores(rv$loggedInClinician)
    if (hasFewScores <- is.null(scoresDf)) {
      scoresDf <- data.frame(score = runif(20))
    }
    gg <- ggplot(scoresDf, aes(x = score)) +
      geom_histogram(binwidth = 0.05, origin = - 0.025, alpha = 0.5) + 
      geom_line(aes(y = row_number(scoresDf$score)), size = 1) +
      theme_minimal() + 
      scale_x_continuous(breaks = seq(0, 1, 0.1)) + 
      # scale_y_continuous(breaks = seq(0, 200, 1)) +
      labs(x = "Scores", y = "Cumulative count of ratings")
    if (hasFewScores) {
      gg + annotate("label", x = 0.5, y = nrow(scoresDf) / 2, label = "FAKE DATA", color = "red", size = 20)
    } else { gg }
  })
  
  
  ### UIs  ####################
  
  loginUI <- div(
    textInput(inputId = "s_Password", label = "Password", ""),
    p(textOutput("LoginMessage", inline = TRUE), align = "center")
  )
  
  loggedInUI <- div(
    p(textOutput("LoginMessage", inline = TRUE), align = "center"),
    actionButton("b_Logout", "Log out")
  )
  
  
  ### Backend Listeners ##################################
  
  ## Main listener for the login and password field.
  output$LoginAction <- renderUI({
    
    if (is.null(Clinician()) || Clinician() == "") { #Listen and check for the name inbox
      return(NULL)
    } else {
      if (rv$loggedInClinician == Clinician()) { # loggged in successfully
        updateTextInput(session, inputId = "s_Password", value = "")
        output$LoginMessage <- renderText( "Logged in")
        rv$visitShown = getCurrentVisit(rv$loggedInClinician)
        return(loggedInUI)  
      } else {
        return(loginUI)
      }
    }
    
  })
  
  ## Login Listener
  observe({
    if (!is.null(input$s_Password) && Clinician() != "") {
      if (input$s_Password == passwords[Clinician()]) {
        rv$loggedInClinician <- Clinician()
      } else {
        output$LoginMessage <- renderText( "Wrong Password")
      }
    }
  })
  
  ## Logout Listener
  observe({
    if (!is.null(input$b_Logout) && input$b_Logout != 0){
      # Default all values
      updateTextInput(session, inputId = "s_Clinician", value = "")
      rv$loggedInClinician <- ""
      rv$visitShown <- currentVisit0
    }
  })
  
  ## Next Listener
  observe({
    if (!is.null(input$b_Next) && input$b_Next != 0){
      sendDEBUG("Receiving button event!")
      writeScore(isolate(input$i_Score), isolate(rv$visitShown))
      updateCurrentVisit(isolate(rv$loggedInClinician))
      rv$visitShown <- getCurrentVisit(isolate(rv$loggedInClinician))
      }
  })
  
  ### Debugging #############################################
  
  if (b_DEBUG) {
    source("server/debugS.R", local = TRUE)
  }
  
})
