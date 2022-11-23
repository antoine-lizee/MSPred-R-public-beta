## 01 -- Label and prepare data
# This scripts helps for the labelling of the columns into categories 
# that are meaningful.
#
# Copyright Antoine Lizee antoine.lizee@gmail.com 10/2015


# dput(colnames(tableFinal))

tableLabels <- list(
  "Patient" = c("AgeOfOnset", "Gender"),
  "Core" = c("AgeAtExam", "EDSS", "MSSS", "DiseaseDuration", "DiseaseCourse"),
  "MSFC" = c("PASAT", "T25FW", "NHPT"),
  "Clinical" = c("ATM", "OpticNeuritis", "INO", "MotorWeakness", 
                 "SensoryDisturbance", "Ataxia", "BladderDisturbance", "BowelDisturbance", 
                 "Myelopathy", "CogDisturbance", "VisualLoss"),
  "FSSs" = c("Visual", "Brainstem", "Pyramidal", 
             "Cerebellar", "Sensory", "Bowel", "Mental"),
  "QOL" = c( "LackFocus", "LackEnergy", 
             "PsychoDown", "RelativePain", "NotEnjoyLife"),
  "Life" = c("CaffeineFreq", 
             "Overweight", 
             "MemoryConcerns", 
             "MemoryDecreaseLastYear",
             "Smoking"),
  "Genetics" = c("DRB1_1501", "MSGB"),
  "MRI" = c("CE_Lesion", "New_T2_Lesions", 
            "GM_Volume", "Siena_PBVC"),
  "Treatments" = c("DMTname", "nDMT", "hasIM", "hasIS", "hasMA", "DMTcategory"),
  "Predicted" = c("class"),
  "MetaSubj" = c("EPICID"),
  "Meta" = c("VisitID"))

tableLabelsVec <- unlist(tableLabels)
tableLabelsDf <- data.frame(name = tableLabelsVec, 
                            label = sub("[0-9]{,2}$", "", names(tableLabelsVec)))

catNames <- function(df = NULL, cns = NULL) {
  if (is.null(cns)) {
    cns <- colnames(df)
  }
  sapply(strsplit(cns, "\\."), '[', 1)
}

featNames <- function(df = NULL, cns = NULL) {
  if (is.null(cns)) {
    cns <- colnames(df)
  }
  sapply(strsplit(cns, "\\."), '[', -1)
}

doLabel <- function(df, labelsDf = tableLabelsDf, arrange = TRUE, na.rm = TRUE) {
  cNs <- colnames(df)
  
  featCats <- labelsDf$label[match(cNs, labelsDf$name)]
  colnames(df) <- paste(featCats, cNs, sep = ".")
  
  if (arrange) {
    cNs <- colnames(df)
    pdctCols <- which(featCats == "Predicted")
    df <- df[c(cNs[pdctCols], cNs[-pdctCols][order(featCats[-pdctCols])])]
  }
  
  if (na.rm) {
    return(df[catNames(df) != "NA"])
  } else {
    return(df)
  }
}
