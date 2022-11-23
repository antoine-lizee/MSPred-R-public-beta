## Freezer.R
# This helper freezes / unfreezes data prior to analyses.
#
# Copyright Antoine Lizee antoine.lizee@ucsf.edu 10/2015

datePattern <- "%Y%m%d.%H%M%S" # Format of the date in the file name
inputDirDefault <- "Input" # Default directory to store them if inputDir is non-existent.

freeze <- function(list = ls(), name = "frozenData", version = "", folderPath = inputDir, ...) {
  # Assign and create input folder
  if(!exists('inputDir')) {
    inputDir <- inputDirDefault
  }
  if(suppressWarnings(dir.create(folderPath, recursive = TRUE))) {
    warning(sprintf("HELPER: Frozen space location '%s' had to be created.", folderPath))
  }
  # Build file name
  date <- format(Sys.time(), datePattern)
  version <- ifelse(version == "", "", paste0(version, "_"))
  fileName <- sprintf("%s_%s%s.RData", name, version, date)
  save(list = list, file = file.path(folderPath, fileName), ...)
}

unFreeze <- function(name = "frozenData", version = NULL, date = NULL, folderPath = inputDir, ...) {
  if(!exists('inputDir')) {
    inputDir <- inputDirDefault
  }
  if (!is.null(date) & !is.null(version)) {
    fileName <- dir(folderPath, sprintf("%s_%s_%s.RData", name, version, date))
    if (length(fileName) == 0) {
      stop(sprintf("FREEZER: cannot find file to load in the folder '%s', with the date stamp'%s' and the version '%s'.\nTry to drop the version?", 
                   folderPath, date, version))
    }
  }
  if (!is.null(date) & is.null(version)) { # 
    fileName <- dir(folderPath, sprintf("%s_.*%s.RData", name, date))
    if (length(fileName) == 0) {
      stop(sprintf("FREEZER: cannot find file in the folder '%s', with the date '%s'", 
                   folderPath, date))
    } else if(length(fileName) > 1) {
      stop(sprintf("FREEZER: several versions have been found in folder '%s', with the date stamp '%s'", 
                   folderPath, date))
    }
  } 
  if (is.null(date) & !is.null(version)) { # get the last date with the current version
    fileNames <- dir(folderPath, sprintf("%s_%s.*.RData", name, version))
    if (length(fileNames) == 0){
      stop(sprintf("FREEZER: cannot find file in the folder '%s', with the version '%s'",
                   folderPath, version))
    }
    fileName <- sort(fileNames, decreasing = T)[1]
    if (length(fileNames) > 1) {
      date <- rev(strsplit(sub("\\.[^\\.]*$", "", fileName), "_")[[1]])[1]
      warning(sprintf("FREEZER: loading file detected as most recent, with date stamp '%s'", date))
    }
  }
  if (is.null(date) & is.null(version)) { # 
    stop("FREEZER: date nor version is provided, cannot unfreeze data.")
  } 
  load(file.path(folderPath, fileName), envir = .GlobalEnv, ...)
}