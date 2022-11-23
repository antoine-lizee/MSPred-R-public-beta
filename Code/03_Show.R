# Show
# This script creates a few first graphs to describe the dataset 
# and its components.
#
# Copyright Antoine Lizee 2015-10

devtools::install_github("antoine-lizee/pheatmap", ref = "bottomJustAnnotationLegend2")

# Merge data --------------------------------------------------------------

tableClass <- decTable %>% select(VisitID, class) %>% 
  left_join(tableFinal) %>% 
  filter(!is.na(class))

table <- tableClass %>% doLabel() %>%  # Label and remove the non-labeled
  select(matches("Meta"))
featureCat <- catNames(table)
featureName <- featNames(table)
# Compute correlation and distance matrices:
cor_d <- CORD(table, method = "spearman")
# Create annotation data:
corClass <- sapply(table, function(col) cor(as.numeric(col), tableClass$class, use = "pairwise.complete.obs"))
corClass['Predicted.class'] <- 0
annDf <- data.frame(class = corClass, type = featureCat)

# Colors and breaks
nCol <- 100
colors <- colorRampPalette(rev(c("#D73027", "#FC8D59", "#FEE090", 'grey99',  "#E0F3F8", "#91BFDB", "#4575B4")))(nCol)
breaks <- seq(-1,1,length.out = nCol + 1)
# Create annotations, including for the correlation metric
# type
types <- unique(featureCat)
lt <- length(types)
# typeCols <- colorRampPalette(RColorBrewer::brewer.pal(lt, "Set1"))(lt)
# typeCols <- rainbow(n = lt + 1)[1:(lt)]
typeCols <- RColorBrewer::brewer.pal(lt, "Paired")
names(typeCols) <- types
# class
totalRange <- c(-1, 1)
rangeX <- range(annDf$class)
rangeX <- rangeX / (max(abs(rangeX)) + 0.1) # Magnify the color scale
rangeIndex <- floor((rangeX - totalRange[1]) / diff(totalRange) * (nCol) + 1)
annColors <- list(class = colors[rangeIndex[1]:rangeIndex[2]],
                  type =  typeCols )

pheatmap(cor_d$COR, clustering_distance_rows = cor_d$D, clustering_distance_cols = cor_d$D, 
         filename = "Output/covHMClustered.pdf", w = 14, h = 9,
         color = colors, breaks = breaks, display_numbers = FALSE, border_color = NA,
         annotation_colors = annColors, annotation_col = annDf, annotation_row = annDf,
         fontsize_number = 6, show_colnames = FALSE, treeheight_col = FALSE, annotation_names_row = FALSE, annotation_legend_bottom = TRUE)

# Tweek the clustering to respect the families
sameCat <- outer(featureCat, featureCat, '==')
Dmod <- as.matrix(cor_d$D)
Dmod[sameCat] <- Dmod[sameCat] / 2
Dmod <- as.dist(Dmod)

pheatmap(cor_d$COR, clustering_distance_rows = Dmod, clustering_distance_cols = Dmod,
         filename = "Output/covHM.pdf", w = 15, h = 9,
         color = colors, breaks = breaks, display_numbers = TRUE, border_color = NA,
         labels_row = featureName, cutree_rows = length(unique(featureCat)), cutree_cols = length(unique(featureCat)),
         annotation_colors = annColors, annotation_col = annDf, annotation_row = annDf,
         fontsize_number = 6, show_colnames = FALSE, treeheight_col = FALSE, annotation_names_row = FALSE, annotation_legend_bottom = TRUE)



# Missingness -------------------------------------------------------------

pheatmap(is.na(data.matrix(table)) + 0, scale = "none", treeheight_row = FALSE)


# More precise missingness ------------------------------------------------

in_table <- tableClass %>% doLabel()
# in_table <- tableClass %>% doLabel() %>% select(-c(Life.MemoryDecreaseLastYear, MRI.New_T2_Lesions))

colNaStats <- unlist(in_table %>% summarise_each(funs(sum(is.na(.)))))
tableClassNa <- in_table %>%
  mutate_each(funs(is.na(.))) %>% 
  mutate(totalNa = rowSums(.)) %>%
  filter(totalNa>0) %>%  # Optional: focus on sparese rows
  arrange_(.dots=names(colNaStats)[order(colNaStats)]) %>% arrange(totalNa)
heatmap(
  t(data.matrix(tableClassNa %>% select(-totalNa) %>% `[`(colNaStats>0)))[order(colNaStats[c(colNaStats>0)]),], 
  labCol = FALSE,
  scale = "none",
  Colv = NA,  # Optional: rely on heuristic ordering instead of hsclust
  Rowv = NA
)


# Re-install CRAN package -------------------------------------------------

detach("package:pheatmap", unload = TRUE)
install.packages("pheatmap")
library(pheatmap)


