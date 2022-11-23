# MSPred  CLinician app
# This script defines the global variables and other general conventions used throughout the app.
# 
# Copyright Antoine Lizee 04/2016 antoine.lizee@gmail.com. See the license included in the project.


# Switches ----------------------------------------------------------------

b_DEBUG = TRUE

# libraries ---------------------------------------------------------------

library(shiny)
# "back-end"
library(RSQLite)
library(jsonlite) 
library(ggplot2)
library(dplyr)

# debuging
if (b_DEBUG) {
  options(shiny.trace=TRUE)
  # options(shiny.error=browser)
}


# Static info -------------------------------------------------------------

clinicians <- c("Riley" = "RB",
                "Carolyn" = "CB",
                "Test" = "TT")

clinicianNames <- names(clinicians)
names(clinicianNames) <- clinicians

passwords <- c(RB = "1234",
               CB = "poiu",
               TT = "0000")

# Probability of positive
probs <- list(pos = 1/3,
              rep = 0.1)


# Schema ------------------------------------------------------------------

DBpath <- "data/clinApp.sqlite"
dir.create(path = "data", showWarnings = F)
tableNames <- list(visits = "visits",
                   results = "results",
                   currentVisit = "currentVisit")

currentVisit0 <- data.frame(
  visit_id = 1,
  clinician_id = "TT",
  updated_at = Sys.time()
)


# Misc tool for the ui script ---------------------------------------------------------------

# Password field:
passwordInput <- function (inputId, label, value = "") 
{
  tagList(shiny:::`%AND%`(label, 
                          tags$label(label, `for` = inputId)), 
          tags$input(id = inputId, 
                     type = "password", value = value))
}

