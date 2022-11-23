# 00_get_data.R
# This scripts gathers the data needed for the analysis.
# This is a data-in-your-worksapce-destructive script (per it first line...)
#
# Copyright Antoine Lizee @ UCSF 10/2015
# antoine.lizee@ucsf.edu

rm(list = ls())

library(RMySQL)

source("Code/Helpers.R")


# Script parameters -------------------------------------------------------

# Are we creating a freezed version of the data?
doFreeze <- F
freezeName <- "RawExtracted"
freezeVersion <- "v2"
freezeObjects <- c("tableFinal", "DMT", "famRaw")


# From FAM data - first tier ---------------------------------------------------

# That is actually from first extraction of EPIC data
famDataObjects1 <- load("../MS-QOL/step0/result.RData")
#Remove whatever is def unneeded
fTRCI <- fullTable32 %>% filter(Study == "EPIC") %>% 
  select(-starts_with("OWT"), # remove the Overweigth stuff
         -starts_with("VisitStatus"), -VisitSeq, -DateDCF, -DueDate, -VisitType, # Remove the management stuff
         -starts_with("Init"), # Remove because NAs
         -YrsEducation,
         -matches("Yr.*EDSS"), # Will be recreated
         -matches("Yr.*RR.*SP"), # ""
         -DiseaseType,
         -Handedness,
         -starts_with("MSCriteria"),
         -DiagnosticCategory,
         -Comments,
         -SiteOnSet,
         -LocationString,
         -VisitScheduled,
         -DMTidAtVisit, # To be done again later
         -DMTLastDosageDate,
         -EDSSAdministrator,
         -Study,
         -VitaminD_Level,
         -TOB1_Expression,
         -CE,
         -starts_with("SIENA."),
         -AlcoholConsumption) %>% 
  rename(EDSS = ActualEDSS)

# Recode some of the factors:
fTRCI$Gender <- factor(fTRCI$Gender, c("F", "M"))
fTRCI$Overweight[fTRCI$Overweight == "Don't know"] <- NA
fTRCI$Overweight <- fTRCI$Overweight == "Yes"
fTRCI$MemoryDecreaseLastYear <- fTRCI$MemoryDecreaseLastYear == "Yes" # All the Not sure are no.
fTRCI$MemoryConcerns <- fTRCI$MemoryConcerns == "Yes" # All the Not sure are no.
# Same for clinical factors (1-4)
fTRCI$OpticNeuritis <- factor(fTRCI$OpticNeuritis, labels = c("N", "UNI", "BI", "BI SIMUL"))
fTRCI$VisualLoss <- factor(fTRCI$VisualLoss, labels = c("NO", "UNI", "BI"))

# Get the EDSS right:
fTRCI$MSSS <- getMSSS(fTRCI$DiseaseDuration, fTRCI$EDSS)

# Get Verbose....
fullTableRawClinicalImaging <- fTRCI
rm(list = famDataObjects1)

# Same for the QOL data (more relevant)
famDataObjects2 <- load("../MS-QOL/step1/result.RData")
fam2 <- plyr::rename(fam2, c("VisitId" = "VisitID"))
famRaw <- fam2
rm(list = famDataObjects2)

modfam <- read.csv("../MS-QOL/step3/all_data_pca.csv")

cat("\n## Importing the dates.\n")
# Dates
for (colname in colnames(fullTableRawClinicalImaging)) {
  col <- fullTableRawClinicalImaging[[colname]]
  isDate <- grepl("date", x = colname, ignore.case = T)
  if (isDate) {
    cat(sprintf("Coercing to Date for column '%s'.\n", colname))
    newCol <- as.POSIXct(col, tz = "GMT", format = "%Y-%m-%d %H:%M:%S")
    newCol <- as.POSIXct(col, format = "%Y-%m-%d")
    newCol[is.na(newCol)] <- as.POSIXct(col[is.na(newCol)], format = "%d/%m/%y")
    newCol[is.na(newCol)] <- as.POSIXct(col[is.na(newCol)], format = "%d/%m/%y")
    newCol[is.na(newCol)] <- as.POSIXct(paste0("15/", col[is.na(newCol)]), format = "%d/%m/%y")
    isPb <- is.na(newCol) & (!is.na(col))
    if (sum(isPb)) {
      cat("Problems:\n")
      print(data.frame(fullTableRawClinicalImaging['VisitID'], problematic.value = col)[isPb,])
    }
    fullTableRawClinicalImaging[[colname]] <- newCol
  }
}


# From MSPrediction-R directly --------------------------------------------

DMT <- read.table("../REX/MSPrediction-R/Data Scripts/tableDMT.csv")

cat("\n## Importing the dates.\n")
# Dates
for (colname in colnames(DMT)) {
  col <- DMT[[colname]]
  isDate <- grepl("date", x = colname, ignore.case = T)
  if (isDate) {
    cat(sprintf("Coercing to Date for column '%s'.\n", colname))
    newCol <- as.POSIXct(col, tz = "GMT", format = "%Y-%m-%d %H:%M:%S")
    newCol[is.na(newCol)] <- as.POSIXct(col[is.na(newCol)], format = "%d/%m/%Y")
    newCol[is.na(newCol)] <- as.POSIXct(paste0("15/", col[is.na(newCol)]), format = "%d/%m/%Y")
    newCol[is.na(newCol)] <- as.POSIXct(paste0("01/07/", col[is.na(newCol)]), format = "%d/%m/%Y")
    isPb <- is.na(newCol) & (!is.na(col))
    DMT[[colname]] <- newCol
    if (sum(isPb)) {
      cat("Removing Problems:\n")
      print(data.frame(DMT['TreatmentID'], problematic.value = col)[isPb,])
      DMT <- DMT[!isPb, ]
    }
  }
}


# From the database directly ----------------------------------------------

con <- RMySQL::dbConnect(MySQL(), 
                         host = "chablis.ucsf.edu", 
                         port = 6750, 
                         username = "MSBASE", 
                         password = "chartreuse",
                         dbname = "epic_etl_db")

treatments <- RMySQL::dbReadTable(con, "treatment")
treatments[c("START", "END")] <- lapply(treatments[c("START", "END")], as.POSIXct)

msfcRaw <- RMySQL::dbReadTable(con, "visit_msfc") 
# Problem w/ SDMT
# msfcRaw$SDMT_Score[!grepl(pattern = "/", msfcRaw$SDMT_Score) & !is.na(msfcRaw$SDMT_Score)]
msfcRaw <- msfcRaw %>% 
  separate(SDMT_Score, c("sdmt_correct", "sdmt_total"), sep = "/") %>% 
  transmute(VisitID = VisitId,
            SDMT = sdmt_correct,
            T25FW = (Walk_Trial1 + Walk_Trial2) / 2,
            NHPT = (DomHand_Trial1 + DomHand_Trial2 + NonDomHand_Trial1 + NonDomHand_Trial2) / 4 )

if (exploDB <- FALSE) {
  ## Quick Missingness study:
  t0 <- Sys.time()
  TABLES <- sapply(RMySQL::dbListTables(con), RMySQL::dbReadTable, conn = con)
  cat(sprintf("Size: %s", format(object.size(TABLES), units = "auto")))
  cat(sprintf("Done in %f sec", Sys.time() - t0))
  qplot(data = TABLES$visit_mri %>% 
          mutate(Date = as.Date(ExamDate)) %>% 
          tidyr::gather("Type", "Value", Brain_volume:V_scale) %>% 
          mutate(na = is.na(Value))
        , x = Date, fill = na, position = "stack", bin = 365.25/12) + facet_grid(Type ~ .) +
    theme_bw() + labs(x = NULL, y = "number of scan per month")
  source("../R/DB_function.R")
  find_field(TABLES, "pasat")
  # Close all connections: sapply(dbListConnections(MySQL()), dbDisconnect)
}

RMySQL::dbDisconnect(con)


# Prepare and Annotate ----------------------------------------------------------------

tableFinal <- fullTableRawClinicalImaging %>% 
  join(modfam, by = "VisitID") %>% 
  join(msfcRaw, by = "VisitID")

## Based of off DMT (some join is already done, not great)
# tableFinal[c("EPICID", "VisitID", "ExamDate")] %>% 
#   join(DMT %>% 
#          join(tableFinal[c("VisitID", "EPICID")]) %>% 
#          select(-VisitID),
#        by = "EPICID") %>% tbl_df %>% 
#   filter(ExamDate - 6*31*3600*24 >= StartDate, ExamDate - 15*3600*24 <= EndDate)

## Based of off treatments
DMT2 <- tableFinal[c("EPICID", "VisitID", "ExamDate")] %>% 
  join(treatments, by = "EPICID") %>% tbl_df %>% 
  filter(ExamDate - 6*31*3600*24 >= START, ExamDate - 16*3600*24 <= END) %>% 
  select_("VisitID", "TreatmentID", "Duration", "StoppedEfficacy", "SideEffects", "START", "END")

DMT <- DMT2


# Freeze ------------------------------------------------------------------

if (doFreeze) {
  freeze(freezeObjects, freezeName, freezeVersion)
}

