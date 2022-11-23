# Clinician App
# This script defines the several utilities that are used by the server.R part of the app.
# 
# Copyright Antoine Lizee 06/2016 antoine.lizee@gmail.com. See the license included in the project.


# Genereal utilities ------------------------------------------------------

#' This is a 'fix' for sample so it never uses a one-element vector as 
#' a length specification.
sample.vec <- function(x, size, replace = FALSE, prob = NULL) {
  if (missing(size)) 
    size <- length(x)
  x[sample.int(length(x), size, replace, prob)]
}

getHex <- function(RGB) {
  paste0("#", paste(sapply(RGB, function(i) as.character(as.hexmode(i))), collapse = ""))
}

ifelseFun <- function(vec, ifVec, fun, ...) {
  resVec <- fun(vec, ...)
  if (length(resVec) == 1){
    if (resVec) {
      return(ifVec)
    } else {
      return(vec)
    }
  } else {
    return(ifelse(resVec, ifVec, vec))
  }
}
# # Tested like that:
# ifelseFun(c(1,2,NA,3), 0, is.na)
# ifelseFun(c(1,2,NA,3), "", is.null)
# ifelseFun(NULL, "", is.null)

sendDEBUG <- function(...) {
  cat(sprintf("#### DEBUG: %s ############################\n", paste(..., collapse = "\n")), file = stderr())
}


# Backend general -----------------------------------------------------------------

# Connection to DB
getCon <- function(dbPath = DBpath) {
  RSQLite::dbConnect(RSQLite::SQLite(), dbPath, cache_size = 5000, synchronous = "full")
}

closeCon <- function(con) {
  RSQLite::dbDisconnect(con)
}

# General Connectors
unpackDF <- function(Object) {
  stopifnot(nrow(Object) == 1)
  modDf <- lapply(Object, 
                  function(col) { 
                    if(is.character(col)) 
                      sprintf("'%s'", col)
                    else
                      col
                  })
  return(sprintf("( %s )", paste(modDf, collapse = ", ")))
}

# Initializer
# Watch out for special variables defined in global
initializeDB <- function() {
  wroteSomething <- FALSE
  if (!dbExistsTable(con, tableNames$currentVisit)) {
    wroteSomething <- TRUE
    dbWriteTable(con, name = tableNames$currentVisit, value = currentVisit0, row.names = F)
    dbGetQuery(con, sprintf("DELETE FROM %s WHERE visit_id = %d", tableNames$currentVisit, currentVisit0$visit_id))
  }
  if (!dbExistsTable(con, tableNames$visits)) {
    wroteSomething <- TRUE
    dbWriteTable(con, name = tableNames$visits, read.csv("data/visits.csv"))
  }
  if (!dbExistsTable(con, tableNames$results)) {
    wroteSomething <- TRUE
    visitShown0 <- data.frame(
      currentVisit0,
      getVisit(51),
      shown_at = Sys.time(),
      stringsAsFactors = FALSE
    )
    result0 <- data.frame(
      visitShown0,
      score = -2,
      saved_at = Sys.time(),
      stringsAsFactors = FALSE
    )
    dbWriteTable(con, tableNames$results, result0, row.names = F)
    dbGetQuery(con, sprintf("DELETE FROM %s WHERE score = %f AND clinician_id = '%s'", tableNames$results, result0$score, currentVisit0$clinician_id))
  }
  return(wroteSomething)
}


# Backend visits ----------------------------------------------------------

## Main logic: chose next visit for 
# a given clinician based on 
# - the visits already rated by the other physicians
# - the repeat rate (how many of the visits should be assessed twice)
# - the positive probability (how much of the visits should be positives)
getNextVisitID <- function(clinicianID, posProb = probs$pos, repeatProb = probs$rep) {
  # Get objects
  visitTable <- getVisitsTable()[c("EPICID", "VisitID", "class")]
  scoreTable <- getScoreTable()
  clinicianScores <- getScores(clinicianID)
  visitTable$scored <- visitTable$VisitID %in% scoreTable$visit_id
  visitTable$clinScored <- visitTable$VisitID %in% clinicianScores$visit_id
  nS <- nrow(scoreTable)
  tS <- table(scoreTable$visit_id)
  # Try to repeat with a probability of 2 * repeatProb if more than
  # 20 observations have been already recorded.
  if (nS > 20 && runif(1) < 2 * repeatProb) {
    sendDEBUG("Trying to repeat...")
    # less than probs$rep that are repeated -> Sample from the visits already rated once.
    if (mean(tS == 2) < repeatProb) { 
      sendDEBUG("Repeating...")
      return(sample.vec(as.integer(names(tS))[tS == 1], 1))
    }
  }
  # Get something from the pool of visits already rated if available.
  toRateIds <- visitTable[visitTable$scored & !visitTable$clinScored, ][["VisitID"]]
  if (length(toRateIds) > 0) {
    sendDEBUG("Drawing from pool of already existing Visits")
    return(sample.vec(toRateIds, 1))
  } else {
    # Sample Visit from the initial pool, following positive probability parameter
    freeVisits <- visitTable[!visitTable$scored, ]
    # Watch out for determination
    if (length(unique(freeVisits$class)) == 1) {
      warning(sprintf("VISIT_GENERATOR: determination in visit class - Assesment for the clinician %s is over.",
                      clinicianID))
      return(NULL)
    }
    # Fetch a positive or Negative visit depending on the probability parameter
    if (runif(1) < posProb) {
      sendDEBUG("Getting random positive Visit")
      return(sample.vec(freeVisits[freeVisits$class == 1, ][["VisitID"]], 1))
    } else {
      sendDEBUG("Getting random negative Visit")
      return(sample.vec(freeVisits[freeVisits$class == 0, ][["VisitID"]], 1))
    }
  }
}

# Helper: keep only the information we need
pruneVisitDf <- function(visitDf, cats = c("Core", "MSFC"), idCats = NULL) {
  cols <- grepl(colnames(visitDf), pattern = paste(c(cats, idCats), collapse = "|"))
  res <- visitDf[cols]
  colnames(res) <- gsub(pattern = "^[^.]*[.]", "", colnames(res))
  return(res)
}

# Low level: get a given visit
# in: id as integer
# out: NULL if visit is non-existent
getVisit <- function(id) {
  query <- sprintf('SELECT * FROM %s WHERE "Meta.VisitID" == %d;', tableNames$visits, id)
  visit <- pruneVisitDf(
    dbGetQuery(con, query)
  )
  if (nrow(visit) == 0) {
    return(NULL)
  } else {
    return(visit)
  }
}

# Low level: get all the visits
getVisitsTable <- function() {
  pruneVisitDf(dbReadTable(con, tableNames$visits),
               idCats = c("Meta", "Predicted"))
}


# Current Visit Backend ---------------------------------------------------

# Connector: get the current visit for a given clinician
# with full CV management
# in: clinician ID
# out: complete visit object, w/ timestamp.
getCurrentVisit <- function(clinicianID) {
  # Handle emplty clinician
  if (clinicianID == "") {
    return(NULL)
  }
  # Initialize
  if (is.null(getCV(clinicianID))){
    visit_id <- getNextVisitID(clinicianID = clinicianID)
    createCV(clinicianID, visit_id)
  }
  # Get the visit to show
  CV <- getCV(clinicianID)
  # Construct the current visit
  return(data.frame(
    CV,
    getVisit(CV$visit_id),
    shown_at = Sys.time(),
    stringsAsFactors = FALSE
  ))
}

# Connector: Update the current visit for a given clinician
# with full management of the current visit.
# in: clinician ID that already have a current visit
updateCurrentVisit <- function(clinicianID) {
  visit_id <- getNextVisitID(clinicianID)
  updateCVid(clinicianID, visit_id)
}

# Low level: get complete CV table
getCVTable <- function() {
  dbReadTable(con, tableNames$currentVisit)
}

# Low level: update visit_id & timestamp for a given clinician 
updateCVid <- function(clinicianID, visitID) {
  sendDEBUG(sprintf("Updating CV for clinician %s to id %d", clinicianID, visitID))
  query <- sprintf('UPDATE %s SET "visit_id" = %d, "updated_at" = \'%s\' WHERE "clinician_id" = \'%s\';', 
                   tableNames$currentVisit, visitID, Sys.time(), clinicianID)
  dbGetQuery(con, query)
}

# Low level: get the current visit for a given clinician
getCV <- function(clinicianID) {
  query <- sprintf('SELECT * FROM %s WHERE "clinician_id" = \'%s\';', 
                   tableNames$currentVisit, clinicianID)
  res <- dbGetQuery(con, query)
  if (nrow(res) == 0) {
    return(NULL)
  } else if (nrow(res) == 1) {
    return(res)
  } else { # More than one record ??
    warning(sprintf("DB_WARNING: More than one record (%d) for clinician %s! Removing them...", 
                    nrow(res), clinicianID))
    query <- sprintf('DELETE FROM %s WHERE "clinician_id" = \'%s\';', 
                     tableNames$currentVisit, clinicianID)
    res <- dbGetQuery(con, query)
    return(NULL)
  }
}

# Low level: create the first CV
createCV <- function(clinicianID, visitID) {
  CV <- data.frame(
    visit_id = visitID,
    clinician_id = clinicianID,
    updated_at = Sys.time(),
    stringsAsFactors = FALSE
  )
  query <- sprintf("INSERT INTO %s VALUES %s;", tableNames$currentVisit, unpackDF(CV))
  dbGetQuery(con, query)
}


# Score backend -----------------------------------------------------------

# Connector: get scores for a given clinician
getScores <- function(clinicianID) {
  if (clinicianID == "") {
    return(NULL)
  }
  scores <- dbGetQuery(con,
                       paste0('SELECT "visit_id", "score" FROM ', tableNames$results, ' WHERE "clinician_id" = \'', clinicianID, '\''))
  if (nrow(scores) == 0) {
    res <- NULL
  } else {
    res <- scores
  }
  return(res)
}

# Low level: write a single score object
writeScore <- function(score, currentVisit) {
  result <- data.frame(
    currentVisit,
    score = score,
    saved_at = Sys.time()
  )
  query <- sprintf("INSERT INTO %s VALUES %s;", tableNames$results, unpackDF(result))
  dbSendQuery(con, query)
}

# Low Level: get the full table of the scores
getScoreTable <- function() {
  dbReadTable(con, tableNames$results)
}




