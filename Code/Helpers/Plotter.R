## Plotter.R
# This helper provides a nice function for plotting Heatmaps
#
# Copyright Antoine Lizee @ UCSF 10/15
# antoine.lizee@ucsf.edu 

library(pheatmap)

# Package change for displaying NA cells in grey --------------------------------

# # Not needed when build from modified local source. cf devtools::install_github("antoine-lizee/pheatmap")
# scale_vec_colours2 <- function (x, col = rainbow(10), breaks = NA, na_col = "#DDDDDD") 
# {
#   res <- col[as.numeric(cut(x, breaks = breaks, include.lowest = T))]
#   res[is.na(res)] <- na_col
#   return(res)
# }
# assignInNamespace(x='scale_vec_colours', value=scale_vec_colours2, pos = "package:pheatmap")


# Main function -----------------------------------------------------------

ppheatmap <- function(DF, method = "pearson", addVar=NULL, addVarname,
                      annotation = NA, newsort = F, display_numbers = T, annotation_colors = NULL, ...) {
  cor_d <- CORD(DF, method = method)
  Color <- colorRampPalette(rev(c("#D73027", "#FC8D59", "#FEE090", 'grey99',  "#E0F3F8", "#91BFDB", "#4575B4")))(100)
  Breaks <- seq(-1,1,length.out = 101)
  Annotation_colors = NA
  if (missing(addVarname))
    addVarname <- cut_legend(deparse(substitute(addVar))) 
  if (is.na(annotation) & !is.null(addVar)) {
    COR_add <- cor( data.matrix(DF), addVar, use = "pairwise.complete.obs", method = method)
    annotation = data.frame(COR_add)
    Annotation_colors = list( Color )
    names(annotation) <- addVarname
    names(Annotation_colors) <- addVarname
  } else {
    Annotation_colors = annotation_colors
  }  
  if (newsort) {
    new_index <- sort2_Mat(as.matrix(D))[1,]
    param_HM <- pheatmap(COR[new_index, new_index], cluster_rows=F, cluster_cols=F, annotation = annotation, annotation_colors = Annotation_colors,
                         display_numbers = display_numbers, number_format = "%.2f",
                         legend_breaks = -2:2/2, 
                         color = Color,
                         breaks = Breaks, ...)
    suppressWarnings({
      param_HM$tree_row$order <- new_index
      param_HM$tree_col$order <- new_index })
  }
  else {
    param_HM <- pheatmap(cor_d$COR, clustering_distance_rows=cor_d$D, clustering_distance_cols=cor_d$D,annotation = annotation, annotation_colors = Annotation_colors,
                         display_numbers = display_numbers, number_format = "%.2f",
                         legend_breaks = -2:2/2, 
                         color = Color,
                         breaks = Breaks, ...)
  }
  param_HM$DMAT <- D
  invisible(param_HM)
}


CORD <- function (DF, method = "pearson") {
  if (is.function(method)) {
    COR <- outer() ## NON FINISHED
  } else {
    COR <- cor(data.matrix(DF), use = "pairwise.complete.obs", method = method)   
  }
  ## Check for meaningless values (eg, variables with a single value)
  b_diag <- is.na(diag(COR)) 
  if (any(b_diag)) {
    warning('Removing ', sum(b_diag),' variables with no auto-correlation because of lack of meaning (null values or single values)...')
    message('Columns removed: ', paste(colnames(COR)[b_diag], collapse = ', ') )    
    COR <- COR[!b_diag, !b_diag]
  }  
  ## Check for low "case filling"
  thre <- max(15, floor(nrow(DF)*0.05) )
  b_diag2 <- sapply(DF[colnames(COR)], function(x) sum(!is.na(x)) <= thre) # check for whole columns
  if (any(b_diag2)) {
    warning('Removing ', sum(b_diag2),' variables with too sparse filling')
    message('Columns removed: ', paste(colnames(COR)[b_diag2], collapse = ', ') )    
    COR <- COR[!b_diag2, !b_diag2]
  } 
  N_case <- t(!is.na(DF[colnames(COR)])) %*% !is.na(DF[colnames(COR)])
  b_low <- N_case <= thre & N_case > 1
  if (any(b_low)) {
    warning("Removing ", sum(b_low), " values for cases with low filling") # check for single cases
    COR[b_low] <- NA
  }
  D <- as.dist(1-abs(COR)) 
  ## Check for NAs and adjust
  if (any(is.na(D))) {  
    warning("Missing correlations because of missing values, adjusting distance...") 
    D[is.na(D)] <- 1
  } 
  list(COR = COR, D = D)
}

# Quick helpers -----------------------------------------------------------

QP <- function (MAT,...) {
  pheatmap(MAT, cluster_rows=F, cluster_cols=F,...)
}


# Carry on stuff (some of this is needed) ---------------------------------

### Matrix Sorting functions, useful for nearest neighbors look up
# by average
sort_Mat <- function(DMAT){
  tot1 <- apply(DMAT, 2, sum)
  tot2 <- apply(DMAT, 1, sum)
  return(DMAT[order(tot1),order(tot2)])
}
# by distance to the rest of the world (sort of top down hclust)
sort2_Mat <- function(DMAT, index_x, index_y) {
  if (missing(index_x)){ # First Call
    L <- dim(DMAT)[1]
    index_x <- 1:L
    index_y <- index_x
    mx <- which.max(apply(DMAT, 1, sum))
    my <- which.max(apply(DMAT, 2, sum))
    DMAT2 <- DMAT[-mx,-my]
    index_x2 <- index_x[-mx]
    index_y2 <- index_y[-my]
    return(cbind(c(index_x[mx],index_y[my]),sort2_Mat(DMAT2, index_x2, index_y2))[,L:1]) # For rev()
  }
  else {
    if (is.matrix(DMAT) ) {
      mx <- which.max(apply(DMAT, 1, sum))
      my <- which.max(apply(DMAT, 2, sum))
      DMAT2 <- DMAT[-mx,-my]
      index_x2 <- index_x[-mx]
      index_y2 <- index_y[-my]
      return(cbind(c(index_x[mx],index_y[my]),sort2_Mat(DMAT2, index_x2, index_y2)))
    }
    else # last Call
      return(c(index_x,index_y))
  }
}
# by the best couples by couple (will not work on a distance matrix !)
sort3_Mat <- function(DMAT, index_x, index_y) {
  if (missing(index_x)){ # First Call
    index_x <- 1:dim(DMAT)[1]
    index_y <- index_x
  }
  if (is.matrix(DMAT)) {
    l <- dim(DMAT)[1]
    #     print(dim(DMAT))
    M <- which.min(DMAT)
    mx <- (M-1) %% l +1 # For borders !
    my <- (M-1) %/% l + 1
    DMAT2 <- DMAT[-mx,-my]
    index_x2 <- index_x[-mx]
    index_y2 <- index_y[-my]
    #     print(c(mx,index_x[mx],my,index_y[my]))
    return(cbind(c(index_x[mx],index_y[my]),sort3_Mat(DMAT2, index_x2, index_y2)))
  }
  else # bottom Call
    return(c(index_x,index_y))
}

cut_legend <- function (string, num = 15) {
  return(ifelse(nchar(string) > num, paste(strtrim(string, num-2), '...', sep= ""), string))
}


# 
# # OLD STUFF ---------------------------------------------------------------
# 
# ### New Scatterplot Matrix function accepting NAs
# source('./MISC_spm2.R')
# 
# ### ACCESSORY FUNCTIONS

