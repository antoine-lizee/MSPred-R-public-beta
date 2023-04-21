

# Helper blocks & packages -----------------------------------------------------------------

try(detach("package:dplyr"), silent = TRUE) # to make sure it's loaded after plyr
library(plyr)
library(dplyr)
library(tidyr)
library(ggplot2)

source("Code/Helpers/Logger.R")
source("Code/Helpers/ComputeOrLoader.R")
source("Code/Helpers/Freezer.R")
source("Code/Helpers/Plotter.R")
source("Code/Helpers/MSSS.R")
source("Code/Helpers/Evaluator.R")


# Project Specific --------------------------------------------------------

outputDir <- "Output"
outputPath <- function(fileName, extension = "csv") {
  filePath <- file.path(outputDir, paste(fileName, extension, sep = "."))
  dir.create(dirname(filePath), showWarnings = FALSE)
  return(filePath)
}



# Low-level ---------------------------------------------------------------

# Redefine list so it can be used for several arguments passing
list <- structure(NA,class="result")
"[<-.result" <- function(x,...,value) {
  args <- as.list(match.call())
  args <- args[-c(1:2,length(args))]
  length(value) <- length(args)
  for(i in seq(along=args)) {
    a <- args[[i]]
    if(!missing(a)) eval.parent(substitute(a <- v,list(a=a,v=value[[i]])))
  }
  x
}

# cat with sprintf
cats <- function(fmt, ..., file = if(interactive() | sink.number() > 0) "" else "run.log", timing = TRUE) {
  out <- sprintf(fmt, ...)
  if (timing) out <- paste0(sprintf('%s:  ', Sys.time()), out)
  cat(out, file = file, append = TRUE)
}


# Data Exploration --------------------------------------------------------

giveMeTable <- function(vec) {
  tt <- table(vec, useNA = "ifany")
  tt[order(tt, decreasing = T)]
}

showDuplicates <- function(dataframe, column) {
  duplicatedRows <- dataframe[duplicated(dataframe[column]) | duplicated(dataframe[column], fromLast=T),]
  View(duplicatedRows)
}

showIntersect <- function(...) {
  # Example: showIntersect(w = LETTERS[1:20], x = LETTERS[1:6], y = LETTERS[2:8], z = LETTERS[10:14])
  args <- list(...)
  args <- lapply(args, unique)
  argNames <- sapply(eval(substitute(alist(...))), deparse) # That's quite something.... see below for explanations.
  if(!is.null(names(args))) {
    argNames <- ifelse(names(args) != "", names(args), argNames)
  }
  df <- data.frame(value = args[[1]],  isa = 1)
  colnames(df)[2] <- argNames[1]
  i <- 2
  while (i <= length(args)) {
    stopifnot(is.vector(argi <- args[[i]]))
    dfi <- data.frame(value = argi, isa = 1)
    colnames(dfi)[2] <- argNames[i]
    df <- merge(df, dfi, all = TRUE)
    i <- i + 1
  }
  df[do.call(order, lapply(df[-1], is.na)),]
}
# substitute will replace '...' with the arguments.
# alist() is like list() but without evaluating the arguments, 
# using symbols instead.
# eval creates it, deparse turns the symbol into the string.
# good ref: http://adv-r.had.co.nz/Computing-on-the-language.html
# could have used pryr::named_dots(...) for a same goal.


# Data processing ---------------------------------------------------------

na.omit.verbose <- function(object, ...) {
  res <- na.omit(object, ...)
  warning(sprintf("Removing %d rows (%.2f %%) which have NAs.", 
                  d <- (t <- nrow(object)) - nrow(res),
                  d / t * 100))
  res
}

# Use it with `[]` for reassigning the columns without changing the structure properties!
bool2facs <- function(df) {
  lapply(df, function(col) {
    if ("logical" %in% class(col)) {
      factor(col)
    } else {
      col
    }
  })
}


# Performance Assessment and estimation (miscs) -------------------------------------------------------------

uniquifyCatGroups <- function(cg) {
  CG <- unique(lapply(lapply(cg, sort), unique))
  CG[order(sapply(CG, function(group) if (all(group == ".")) Inf else length(group)))]
}

createCatGroups <- function(featCats, level = length(featCats)) {
  cCG <- function(featCats, level) {
    if (level == 0) {
      return(list(NULL))
    } else {
      sapply(featCats, 
             function(featCat) lapply(cCG(featCats, level - 1),
                                      'c', featCat))
    }
  }
  uniquifyCatGroups(c(cCG(featCats, level)))
}

createArgsDf <- function(..., maxRuns = 200) {
  args <- list(...)
  ns <- cumprod(sapply(args, length))
  stopifnot(max(ns) < maxRuns)
  do.call(data.frame,
          c(mapply(rep, args, each = c(1, ns[-length(ns)]), SIMPLIFY = FALSE),
            stringsAsFactors = F))
}


# Plotting ----------------------------------------------------------------

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

