# MSPred  CLinician app
# This script defines the user interface of the app.
# As a convention, "input" variable names are prefixed by a one-letter 
# indicator on the type of the variable,
# while "output" variable names are capitalized.
# 
# Copyright Antoine Lizee 03/2016 antoine.lizee@gmail.com. See the license included in the project.


library(shiny)

shinyUI(navbarPage(
  "The MSPred Clinician's App",
  tabPanel("Your Profile",
           fixedPage(
             fluidRow(
               column(4,
                      h3("Select your identity", align = "center"),
                      selectInput("s_Clinician", label = "Select your identifier", 
                                  choices = c("Login here..."="", clinicians), multiple = FALSE),
                      conditionalPanel(
                        condition = "typeof input.s_Clinician !== 'undefined' & input.s_Clinician != ''",
                        wellPanel(
                          uiOutput("LoginAction")
                        ))),
               column(8, 
                      h3("Your Predictions", align = "center"),
                      plotOutput("scorePlot")
               ))
           )),
  tabPanel("Patient Assesment",
           fixedPage(
             fluidRow(
               fluidRow( 
                 h4(textOutput("raterIdentity"), align = "center")
               ),
               hr(),
               fluidRow(
                 h3("Patient characeristics", align = "center"),
                 column(4,
                        h4("General", align = "center"),
                        uiOutput("Vdc"),
                        uiOutput("Vdd"),
                        uiOutput("Vaae")),
                        # style = "border-right: 1px dashed #333; border-left: 1px dashed #333;"),
                 column(4,
                        h4("Scores", align = "center"),
                        uiOutput("Vedss"),
                        uiOutput("Vmsss")),
                        # style = "border-right: 1px dashed #333; border-left: 1px dashed #333;"),
                 column(4,
                        h4("MSFCs", align = "center"),
                        uiOutput("Vt25"),
                        uiOutput("Vnhpt")),
                        # style = "border-right: 1px dashed #333; border-left: 1px dashed #333;")
                 class = "vertical-divider"
               )),
             hr(),
             fluidRow(
               h4("Probability of worsening", align = "center"),
               column(1, offset = 2, p("Not likely to worsen")),
               column(6,
                      sliderInput("i_Score", NULL, 
                                  min = 0, max = 1, step = 0.01, value = 0.5),
                      align = "center"),
               column(1, p("Very likely to worsen"))),
             uiOutput("nextButton"),
             class = "text-center"
           )),
  source("ui/debugUI.R", local = TRUE)$value,
  footer = list(
    hr(),
    p("Created with Shiny, love and pain by Antoine Lizee", a("(Github)", href = "https://github.com/antoine-lizee/MS-Pred-R"),
      align = "right"),
    
    #### CSS & additional scripts ###########
    includeCSS("www/vertical-divider.css"),
    includeScript("www/linkToTab.js"))
))


