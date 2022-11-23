## CVer.R
# This file defines the ML framework used in the model assessmnt.
#
# Copyright Antoine Lizee @ UCSF 10/15
# antoine.lizee@ucsf.edu 

library(dplyr)
library(readr)
library(caTools)
globalRunDir <- "RunData"

# Low level ---------------------------------------------------------------

read.matrix <- function(..., doubles = TRUE, verbose = FALSE) {
  columns_spec <- if (doubles) cols(.default = col_double()) else NULL
  is_old <- grepl('^"(V1|x)', read_lines(list(...)[[1]], n_max = 1))
  if (is_old){  # parse the old way, with column names and row names
    if (verbose) message(" -- Reverting to legacy function for reading files -- ")
    df <- read_delim(..., col_types = columns_spec, delim = " ", skip = 1, col_names = FALSE)
    if (nrow(problems(df)) >0 ) {
      warning("WARN: CVer: read.matrix: remaining problems reading the matrix.")
    }
    return(unname(as.matrix(df[-1])))
  }
  df <- read_delim(..., delim = " ", col_types = columns_spec, col_names = FALSE)
  if (nrow(problems(df)) >0 ) {
    warning("WARN: CVer: read.matrix: remaining problems reading the matrix.")
  }
  return(unname(as.matrix(df)))
}

write.matrix <- function(...) {
  write.table(..., col.names = FALSE, row.names = FALSE)
}


# foreach management ------------------------------------------------------

# Function to assign correct environment to function that are part of other objects:
putInCurrentEnv <- function(object, thisEnv = NULL) {
  # Define the recursive method
  setFunEnv <- function(object, env) {
    invisible(return(
      switch(class(object),
             "list" = lapply(object, setFunEnv, env = env),
             "function" = {environment(object) <- env; object},
             object
      )))
  }
  # Call it with the appropriate environment:
  if (is.null(thisEnv)) thisEnv <- parent.env(environment(function(){}))
  return(setFunEnv(object, env = thisEnv))
}
# # Test with something along the lines of:
# fun <- function() a + 2
# testl <- list(); testl$fun <- function() a + 2
# foreach(i = 1:10) %dopar% { testl <- putInCurrentEnv(testl); list(environment(fun), environment(testl$fun), environment(fun), environment(function() {})) }


# traceback stuff (to be moved ?) -----------------------------------------

create_traceback <- function(callstack) {
  if (length(callstack) == 0) return()
  max_lines <- getOption("deparse.max.lines", Inf)
  
  # Convert to text
  calls <- lapply(callstack, deparse, width = getOption("width"))
  if (is.finite(max_lines)) {
    calls <- lapply(calls, function(x) x[seq_len(min(length(x), max_lines))])
  }
  calls <- vapply(calls, paste0, collapse = "\n", FUN.VALUE = character(1))
  
  # Extract srcrefs
  srcrefs <- lapply(callstack, attr, "srcref")
  has_ref <- !vapply(srcrefs, is.null, logical(1))
  files <- vapply(srcrefs[has_ref], function(x) attr(x, "srcfile")$filename,
                  FUN.VALUE = character(1))
  lines <- vapply(srcrefs[has_ref], function(x) as.vector(x)[1],
                  FUN.VALUE = integer(1))
  
  calls[has_ref] <- paste0(calls[has_ref], " at ", files, ":", lines)
  
  # Number and indent
  calls <- paste0(seq_along(calls), ": ", calls)
  calls <- gsub("\n", "\n ", calls)
  calls
}

# Run directory & Fold indices management ------------------------------------------------------------

# Create a time-stamped dirname
tsDir <- function(dn, ts = Sys.time()) {
  ts <- ifelse(any(class(ts) %in% c("POSIXct", "POSIXt", "POSIXlt")), 
               format(ts, "%F_%H-%M"), ts)
  paste(dn, ts, sep = "_")
}

# Create the Run filename.
runDirName <- function(nFolds, nCV, nR, prefix = "Def") {
  paste(prefix, nFolds, nCV, nR, sep = "-")
}

# get the latest Run Directory or returns NULL 
runDir <- function(nFolds = 5, nCV = 10, nR = nrow(X), ts = "", prefix) {
  dns <- dir(path = globalRunDir, pattern = tsDir(runDirName(nFolds, nCV, nR, prefix), ts), full.names = T)
  if (length(dns) == 0) { 
    return(NULL) 
  } else {
    return(sort(dns, decreasing = T)[1])
  }
}

# Get the latest Run Directory based on an arbitrary pattern
fuzzyRunDir <- function(pat) {
  rns <- dir(path = globalRunDir, pattern = pat, full.names = TRUE)
  rns[order(gsub(".*(_[-[:digit:]]*_[-[:digit:]])", "\\1", rns), decreasing = TRUE)[1]]
}

# Get the latest or create a set of fold indices for this run.
# Returns the appropriate run directory as well.
getFI <- function(nFolds = 5, nCV = 10, nR = nrow(X), new = F, ts = "", prefix = "", partition = NULL) {
  runDir <- runDir(nFolds, nCV, nR, ts, prefix)
  if (is.null(runDir) | new) {
    cat("Creating new run directory...\n")
    runDir <- file.path(globalRunDir, tsDir(runDirName(nFolds, nCV, nR, prefix)))
    if (is.null(partition)) {
      FIs <- replicate(sample(rep(1:nFolds, length.out = nR)), n = nCV)
    } else {
      FIs <- replicate(sample(rep(1:nFolds, length.out = max(partition)))[partition], n = nCV)  
    }
    dir.create(runDir, recursive = TRUE)
    write.matrix(FIs, file = file.path(runDir, "indices.tsv"))
  } else {
    FIs <- read.matrix(file.path(runDir, "indices.tsv"))
  }
  return(list(FIs = FIs, runDir = runDir))
}


# CV function -------------------------------------------------------------

run <- function(X, Y, predictor, FIs) {
  nCV <- ncol(FIs)
  nFolds <- length(unique(c(FIs)))
  Yps <- replicate(rep(NA, length(Y)), n = nCV)
  for (iCV in 1:nCV) {
    cats("CV num %d:\n", iCV)
    foldIdx <- FIs[, iCV]
    for (i in 1:nFolds) {
      res <- predictor$trainPredict(X[foldIdx != i,], Y[foldIdx != i], X[foldIdx == i,])
      stopifnot(length(res) == sum(foldIdx == i))
      Yps[foldIdx == i, iCV] <- res
    }
  }
  return(Yps)
}

runAll <- function(predictTable, nFolds, nCV, prefix = "Def", predictors, catGroups, parallel = FALSE, nParallel = 10, partition = NULL) {
  
  t0 <- Sys.time()
  
  # Get the run characteristics
  list[FIs, RD] <- getFI(nFolds, nCV, nR = nrow(predictTable), prefix = prefix, partition = partition)
  if(dir.exists(RD)) cats("Using existing directory for RunData: %s\n", RD)
  else cats("Creating new directory for Rundata: %s\n", RD)
  dir.create(logdir <- file.path(RD, "logs"), showWarnings = FALSE)
  logFilePathPattern <- file.path(logdir, paste0(tsDir("run"), "%s", ".log"))
  
  # Write the Y file
  Y <- predictTable$Predicted.class
  write.matrix(Y, file.path(RD, "Y.tsv"))
  # Write the predicting columns file
  write.matrix(colnames(predictTable), file.path(RD, "cols.tsv"))
  
  tryCatch({
  
  # Initialize the parallel cluster
  library(foreach)
  if (parallel) {
    cats("Starting up parallel cluster...\n")
    library(doParallel)
    cl <- parallel::makeCluster(nParallel)
    doParallel::registerDoParallel(cl)
  }
  
  foreach(catGroup = iterators::iter(catGroups), 
          .export = c("cats", "run", "write.matrix", "putInCurrentEnv", 
                      "create_traceback", "catNames", "bool2facs",
                      "AUROC", "calcAuroc"),
          .packages = c("plyr", "dplyr")
          ) %dopar% {

    options(warn = 1)  ## Print warnings as they occur
    logFilePath <- sprintf(logFilePathPattern, paste0("_", Sys.getpid()))
    sink(file = file(logFilePath, open = "a"), append = TRUE)
    sink(file = file(logFilePath, open = "a"), append = TRUE, type = "message")
    run <- putInCurrentEnv(run, environment())
    
    catGroupLabel <- paste(sort(substr(catGroup, 1, 2)), collapse = "")
    tCatGroup <- Sys.time()
    cats("###### Treating group of Categories: %s\n", catGroupLabel)
    X <- predictTable %>% 
      select(grep(paste(catGroup, collapse = "|"), catNames(.))) %>% 
      select(which(catNames(.) != "Predicted"))
    cats("Selected columns:\n%s\n", paste(colnames(X), collapse = ", "))
    
    for (predName in names(predictors)) {
      
      pred <- putInCurrentEnv(predictors[[predName]])
      t0 <- Sys.time()
      try(withCallingHandlers({
        if (file.exists(fn <- file.path(RD, paste0(predName, "_", catGroupLabel, ".tsv")))) {
          cats("Already run predictor '%s' on group '%s', skipping...\n",
               predName, catGroupLabel)
        } else {
          cats("Running predictor '%s' for group '%s'.\n", predName, catGroupLabel)
          Yps <- run(X, Y, pred, FIs)
          write.matrix(Yps, fn)
          cats("Ran predictor '%s' for group '%s' in %.2f minutes.\n", predName, catGroupLabel, difftime(Sys.time(), t0, units = "min"))
        } 
      }, error = function(e) {
        sc <- sys.calls()
        cats("ERROR running train/test for predictor '%s' and group '%s': \n %s\nTRACEBACK:\n%s\n", 
             predName, catGroupLabel, e$message, paste(create_traceback(sc[25:(length(sc)-2)]), collapse='\n'))
      }))

    }
    cats("## Ran group '%s' in %.2f minutes.\n\n", catGroupLabel, difftime(Sys.time(), tCatGroup, units = "min"))
    sink()
    sink(type = "message")
    
  }
  
  }, finally = {
    # Stop the cluster
    if (parallel) {
      cats("Stopping cluster...\n")
      parallel::stopCluster(cl)
    }
    # Logging
    cats("Time elapsed for the run:\n%s\n", format(Sys.time() - t0))
  })
}


# Extract the results -----------------------------------------------------

extractYpred <- function(pat = "") {
  RD <- fuzzyRunDir(pat)
  filenames <- dir(path = RD, pattern = ".tsv")
  list[methods, catGroups] <- lapply(1:2, function(i) sapply(strsplit(gsub("\\.[^\\.]*$", "", filenames), "_"), '[', i))
  return(list(
    ypredDf = data_frame(filename = filenames, method = methods, catGroup = catGroups) %>%
      filter(!is.na(catGroup)) %>% 
      rowwise() %>% mutate(Yp = list(read.matrix(file.path(RD, filename)))),
    y = read.matrix(file.path(RD, "Y.tsv"), doubles = FALSE, verbose = TRUE),  # Verbose to print once if we've reverted to legacy format
    FIs = read.matrix(file.path(RD, "indices.tsv"), doubles = FALSE),
    cns = read.matrix(file.path(RD, "cols.tsv"), doubles = FALSE),
    RD = RD
  ))
}

perfYpredDf <- function(pat = "", yExtr = extractYpred(pat), perFolds = FALSE) {
  nCV <- ncol(yExtr$FIs)
  predDf <-
    if (perFolds) {
      nFolds <- length(unique(c(yExtr$FIs)))
      idx <- yExtr$FIs + rep((1:nCV - 1) * nFolds, each = nrow(yExtr$FIs))
      Yt <- rep(yExtr$y, nCV)
      yExtr$ypredDf %>% group_by(method, catGroup) %>% 
        do(data_frame(cv = rep(1:nCV, each = nFolds), fold = rep(1:nFolds, each = nCV),
                      pred = sapply(1:(nCV*nFolds), function(i) ROCR::prediction(.$Yp[[1]][idx == i], labels = Yt[idx == i])))) %>% 
        ungroup
    } else {
      yExtr$ypredDf %>% group_by(method, catGroup) %>% 
        do(data_frame(cv = 1:nCV, pred = apply(.$Yp[[1]], 2, ROCR::prediction, labels = yExtr$y))) %>% 
        ungroup
    }
  perfDf <- predDf %>% mutate(pr = lapply(pred, ROCR::performance, "prec", "rec"),
                              roc = lapply(pred, ROCR::performance, "tpr", "fpr"),
                              rch = lapply(pred, ROCR::performance, "rch"),
                              acc = lapply(pred, ROCR::performance, "acc"),
                              auc = sapply(pred, calcAuroc),
                              aurch = sapply(rch, calcAurch),
                              aucpr = sapply(pr, calcAucpr),
                              aucpr5 = sapply(pr, calcAucpr5),
                              macc = sapply(acc, function(acc) max(acc@y.values[[1]])))
  ### Proof of auroc <=> Wilcox. test
#   # Alternate way, using wilcoxon-Mann-Whitney
#   perfDf2 <- yExtr$ypredDf %>% group_by(method, catGroup) %>% 
#     do(data_frame(cv = 1:nCV, 
#                  auc2 = apply(.$Yp[[1]], 2, function(Ypred) {
#                    ys = unname(split(Ypred, yExtr$y))
#                    stat = do.call(wilcox.test, ys)$statistic
#                    return(stat / prod(sapply(ys, length)))
#                    })))
#   compDf <- perfDf %>% inner_join(perfDf2) %>% mutate(auc2 = 1 - auc2)
  return(perfDf)
}



