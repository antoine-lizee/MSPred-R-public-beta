# 00_get_IDBAPS.R
# This scripts gathers the data needed for the replication.
#
# Copyright Antoine Lizee @ UCSF 10/2015
# antoine.lizee@ucsf.edu

source("Code/Helpers.R")

doFreeze <- T
freezeName <- "IDBAPS"
freezeVersion <- "v2"
freezeObjects <- c("tableClassI", "decTableI")

# Read and prepare the data -----------------------------------------------------

# Need to create the local version of the sufl data first
#sourceThere("../msbioscreen.SUFL/SUFL/UB_SUFL_1.0.R")
objs <- load("../msbioscreen.SUFL/SUFL/Output/UB_SUFL_1.0.RData")
fullTableIDBAPS <- endpointsData$visits %>% left_join(endpointsData$subjects,
                                                      by = c(patient_source_id = "source_id",
                                                             patient_external_identifier = "external_identifier"))
rm(list = objs)
tableFinalI <- fullTableIDBAPS %>% transmute(
  AgeOfOnset = age_of_onset,
  Gender = factor(gender, c("F", "M")),
  AgeAtExam = age_at_visit,
  DiseaseDuration = disease_duration,
  DiseaseCourse = factor(disease_course, c("CIS", "PP", "RR", "SP")),
  EDSS = edss,
  MSSS = getMSSS(DiseaseDuration, EDSS),
  PASAT = pasat_score,
  T25FW = timed_walk_trial1_time,
  NHPT = (nhpt_dominant_time + nhpt_nondominant_time) / 2,
  EPICID = patient_external_identifier,
  VisitID = external_identifier,
  ExamDate = as.Date(date)
)


# Classify and Label ------------------------------------------------------

source("Code/01_Label.R")
source("Code/02_Classify.R")

# Create the delayed edss table
edssExpTableI <- delayedEdss(tableFinalI %>% 
                              select(EPICID, VisitID, t = ExamDate, EDSS))

# Create decision table
decTableI <- classify(edssExpTableI) #thredEDSSpos = 0.5)  #thredEDSSneg = 0.5) # thredEDSSstable = NA

# Create final, labeled dataset
tableClassI <- decTableI %>% select(VisitID, class) %>% 
  left_join(tableFinalI) %>% 
  filter(!is.na(class)) %>%
  doLabel()

## Have a look
table(decTableI$class, decTableI$anticlass)


# Freeze ------------------------------------------------------------------

if (doFreeze) {
  freeze(freezeObjects, freezeName, freezeVersion)
}
