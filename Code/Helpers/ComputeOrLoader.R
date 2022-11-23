#'  ComputeOrLoader.R
#'  A commonly used helper to load expensive results if they've been already 
#'  computed.
#'  
#'  Copyright Antoine Lizee @ UCSF 2015/10
#'  antoine.lizee@ucsf.edu

#' Compute a result from an expression or 
#' load a file that holds the results of this 
#' expression, if the file exists.
#'
#' @param fileName A file name to store the results.
#' @param objectNames The resulting objects to be saved.
#' @param expr The expression to be evaluated.
#' @param folderPath An optional path to save the object. The default
#' is the current 'scratchDir' if it exists. If it doesn't, a scratch space
#' is created.
#' @return Nothing!
#' @family Helpers
computeOrLoad <- function(fileName, objectNames, expr, folderPath = scratchDir) {
  if(!exists('scratchDir')) {
    scratchDir <- "Scratch"
  }
  if(suppressWarnings(dir.create(folderPath, recursive = TRUE))) {
    warning(sprintf("HELPER: Scratch space location '%s' had to be created.", folderPath))
  }
  if (!grepl(".*\\.rdata", fileName, ignore.case = TRUE)) {
    fileName <- sprintf("%s.RData", fileName)
  }
  filePath <- file.path(folderPath, fileName)
  if (file.exists(filePath)) {
    cat("Loading from RData file '", filePath, "'...\n", sep = "")
    load(filePath, envir = parent.env(environment()))
  } else {
    cat("Executing...\n")
    eval(expr)
    save(file = filePath, list = objectNames)
  }
}
