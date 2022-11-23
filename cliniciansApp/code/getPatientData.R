## 06_PredictAll.R
# This script creates runs the Whole predictive analysis.
#
# Copyright Antoine Lizee 2017-08

source("Code/Helpers.R")
source("Code/Helpers/CVer.R")


# Create patient database -------------------------------------------------

unFreeze("clean", "v1")

write.csv(as.data.frame(fullPredictTable), file = "cliniciansApp/data/visits.csv")
