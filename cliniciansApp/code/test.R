
# Create & initialise test environment ----------------------------------------------------

# Prepare external data
unlink("testClinApp/data", recursive = TRUE)
dir.create("testClinApp/data")
file.copy("cliniciansApp/data/visits.csv", "testClinApp/data")
# Load utilities
source('cliniciansApp/global.R')
source('cliniciansApp/server/utilities.R')
# Initialize DB and db con
source("testClinApp/initTestDb.R", chdir = TRUE)
con <- getCon("testClinApp/data/clinApp.sqlite") # <- this is what makes us hit the test db!


# Simulate usage ----------------------------------------------------------

simulateScoring <- function(clinicianID) {
  cv <- getCurrentVisit(clinicianID)
  writeScore(runif(1), cv)
  updateCurrentVisit(clinicianID)
}

data_frame(clinician_id = names(clinicianNames), n_simul = c(25, 42, 100)) %>% 
  group_by(clinician_id) %>% 
  do(simulateScoring(quote(clinician_id)))

scoreDf <- getScoreTable()
visitsDf <- getVisitsTable()
