# Show
# This script creates a few first graphs to describe the dataset 
# and its components.
#
# Copyright Antoine Lizee 2023-03
#
# R version: 4.2
# ✔ ggplot2 3.3.6     ✔ purrr   0.3.4
# ✔ tibble  3.1.8     ✔ dplyr   1.0.9
# ✔ tidyr   1.2.0     ✔ stringr 1.4.0
# ✔ readr   2.1.2     ✔ forcats 0.5.1

source("Code/Helpers.R")
source("Code/Helpers/CVer.R")
source("Code/01_Label.R")

if (create <- FALSE) {

  # Import and characterize data --------------------------------------------
  
  unFreeze("RawExtractedWithTreatments", "v2")
  source("Code/02_Classify.R")
  
  # Create the delayed edss table
  delayedEdssTable <- delayedEdss(
    tableFinal %>% select(EPICID, VisitID, t = ExamDate, EDSS)
  )
  
  # Create decision table
  decisionTable <- classify(delayedEdssTable)
  
  # Create final, labeled dataset
  withClassTable <- tableFinal %>%
    left_join(decisionTable %>% select(VisitID, class), by = "VisitID") %>% 
    doLabel() %>%
    # Remove raw DMT name information
    select(-Treatments.DMTname) %>%
    # Remove now very sparse features that we know we don't want (1474 / 2977, ~50%)
    # Note that we remove it from the correlation plot below as a result
    select(-Life.MemoryDecreaseLastYear)
  
  naTable <- withClassTable %>% select(-MetaSubj.EPICID) %>%
    mutate_at(vars(-Meta.VisitID), is.na)
  
  .hasMissingValueInRow <- function(df) {
    rowSums(df %>% select(-Predicted.class, -Meta.VisitID, -starts_with("Genetics"), -starts_with("MRI"))) > 0
  }
  setsTable <- naTable %>% 
    transmute(
      Meta.VisitID = Meta.VisitID
      , is_in_0_full = TRUE
      , is_in_0bis_others = !.hasMissingValueInRow(.)
      
      , is_in_1_has_dec = !Predicted.class
      , is_in_1bis_has_dec_others = is_in_1_has_dec & is_in_0bis_others
      
      , is_in_2_has_dec_dna = is_in_1_has_dec & !Genetics.DRB1_1501 & !Genetics.MSGB
      , is_in_2bis_has_dec_dna_others = is_in_2_has_dec_dna & is_in_0bis_others
      
      , is_in_3_has_dec_mri = is_in_1_has_dec & !MRI.New_T2_Lesions & !MRI.GM_Volume & !MRI.Siena_PBVC & !MRI.CE_Lesion
      , is_in_3bis_has_dec_mri_others = is_in_3_has_dec_mri & is_in_0bis_others
      
      , is_in_4_has_dec_dna_mri = is_in_2_has_dec_dna & is_in_3_has_dec_mri
      , is_in_4bis_has_dec_dna_mri_others = is_in_4_has_dec_dna_mri & is_in_0bis_others
  )
  setsTable[,-1] %>% summarise_all(sum)
  
  withSetsTable <- withClassTable %>%
    inner_join(setsTable)
  
  mp <- mice::md.pattern(
    withSetsTable %>%
      # filter(is_in_1_has_dec)
      filter(is_in_1bis_has_dec_others)
    , rotate.names = TRUE
    , plot = FALSE
  )
  missingness_patterns <- mp[,mp[dim(mp)[1],] != 0]
  mp_dims <- dim(missingness_patterns)
  heatmap(1-missingness_patterns[-mp_dims[1],-mp_dims[2]], Rowv = NA, Colv = NA, scale = "none")
  
  withSetsReplicatedTable <- withSetsTable %>% mutate(set = "is_in_0_full") %>%
    bind_rows(withSetsTable %>% filter(is_in_0bis_others) %>% mutate(set = "is_in_0bis_others")) %>% 
    bind_rows(withSetsTable %>% filter(is_in_1_has_dec) %>% mutate(set = "is_in_1_has_dec")) %>% 
    bind_rows(withSetsTable %>% filter(is_in_1bis_has_dec_others) %>% mutate(set = "is_in_1bis_has_dec_others")) %>% 
    bind_rows(withSetsTable %>% filter(is_in_2_has_dec_dna) %>% mutate(set = "is_in_2_has_dec_dna")) %>% 
    bind_rows(withSetsTable %>% filter(is_in_2bis_has_dec_dna_others) %>% mutate(set = "is_in_2bis_has_dec_dna_others")) %>% 
    bind_rows(withSetsTable %>% filter(is_in_3_has_dec_mri) %>% mutate(set = "is_in_3_has_dec_mri")) %>% 
    bind_rows(withSetsTable %>% filter(is_in_3bis_has_dec_mri_others) %>% mutate(set = "is_in_3bis_has_dec_mri_others")) %>% 
    bind_rows(withSetsTable %>% filter(is_in_4_has_dec_dna_mri) %>% mutate(set = "is_in_4_has_dec_dna_mri")) %>% 
    bind_rows(withSetsTable %>% filter(is_in_4bis_has_dec_dna_mri_others) %>% mutate(set = "is_in_4bis_has_dec_dna_mri_others"))
  
  summary_func_num <- function(vec) {
    qs <- quantile(vec, c(0.25, 0.75))
    return(sprintf("%.1f ( %s [%s - %s] %s )", mean(vec), min(vec), qs[1], qs[2], max(vec)))
  }
  
  summary_func_cat <- function(vec) {
    tt <- table(vec, useNA = "ifany")
    tt[] <- paste0(tt, sprintf(" (%.0f%%)", tt / sum(tt) * 100))
    return(as.data.frame(as.list(tt)))
  }
  
  demoTable <- withSetsReplicatedTable %>%  group_by(set) %>% summarize(
    n_visits = n_distinct(Meta.VisitID)
    , n_patients = n_distinct(MetaSubj.EPICID)
    , n_female_patients = length(unique(MetaSubj.EPICID[Patient.Gender == 'F']))
    , female_ratio = n_female_patients / n_patients 
    , age_at_visit = summary_func_num(Core.AgeAtExam)
    , disease_duration = summary_func_num(Core.DiseaseDuration)
    , disease_course = summary_func_cat(Core.DiseaseCourse)
    , under_nothing = sum((!Treatments.hasIM & !Treatments.hasIS & !Treatments.hasMA) + 0)
    , under_IM = sum(Treatments.hasIM + 0)
    , under_IS = sum(Treatments.hasIS + 0)
    , under_MA = sum(Treatments.hasMA + 0)
    , class = summary_func_cat(Predicted.class)
  ) %>% unpack(disease_course)
  
  
  # Prepare datasets and export ---------------------------------------------
  
  fullPredictTable <- withClassTable %>%
    na.omit.verbose()  # Corresponds to "is_in_4bis_has_dec_dna_mri_others" -> 1275 observations only 
  
  predictTable <- fullPredictTable %>% select(-starts_with("Meta"))
  epicids <- (fullPredictTable %>% select(starts_with("MetaSubj")))[[1]]
  
  freeze(c("fullPredictTable", "predictTable", "epicids", "demoTable", "withClassTable"), "cleanWithAll", "v1")
  
  fullPredictTable <- withClassTable %>%
    select(-starts_with('MRI')) %>% 
    select(-starts_with('Genetics')) %>% 
    na.omit.verbose()  # Corresponds to "is_in_1bis_has_dec_others" -> 1921 observations
  
  predictTable <- fullPredictTable %>% select(-starts_with("Meta"))
  epicids <- (fullPredictTable %>% select(starts_with("MetaSubj")))[[1]]
  
  freeze(c("fullPredictTable", "predictTable", "epicids", "demoTable", "withClassTable"), "cleanWithoutMRIandGenetics", "v1")

} else {
  
  unFreeze("cleanWithAll", "v1")

}


# Create Summary table for decision ---------------------------------------

my_table <- table1::table1(~ . | Predicted.class, data = predictTable)
file_name <- "Output/decision_table.html"
html_output <- c(table1:::html.standalone.head, as.character(my_table), table1:::html.standalone.foot)
cat(file = file_name, append = FALSE, html_output)



# Create Correlation plot --------------------------------------------------------------

try(detach("package:pheatmap", unload = TRUE))
devtools::install_github("antoine-lizee/pheatmap", ref = "bottomJustAnnotationLegend2", upgrade = FALSE)

tableClass <- withClassTable %>%
  filter(!is.na(class))  # Could challenge this removal at this stage, but likely to not change anything.

table <- tableClass %>%  # Label and remove the non-labeled
  select(-matches("Meta"))
featureCat <- catNames(table)
featureName <- featNames(table)
# Compute correlation and distance matrices:
cor_d <- CORD(table, method = "spearman")
# Create annotation data:
corClass <- sapply(table, function(col) cor(as.numeric(col), tableClass$Predicted.class, use = "pairwise.complete.obs"))
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

pheatmap::pheatmap(cor_d$COR, clustering_distance_rows = cor_d$D, clustering_distance_cols = cor_d$D, 
  filename = "Output/covHMClustered.pdf", w = 14, h = 9,
  color = colors, breaks = breaks, display_numbers = FALSE, border_color = NA,
  annotation_colors = annColors, annotation_col = annDf, annotation_row = annDf,
  fontsize_number = 6, show_colnames = FALSE, treeheight_col = FALSE, annotation_names_row = FALSE, annotation_legend_bottom = TRUE)

# Tweek the clustering to respect the families
sameCat <- outer(featureCat, featureCat, '==')
Dmod <- as.matrix(cor_d$D)
Dmod[sameCat] <- Dmod[sameCat] / 2
Dmod <- as.dist(Dmod)

pheatmap::pheatmap(cor_d$COR, clustering_distance_rows = Dmod, clustering_distance_cols = Dmod,
  filename = "Output/covHM.pdf", w = 15, h = 9,
  color = colors, breaks = breaks, display_numbers = TRUE, border_color = NA,
  labels_row = featureName, cutree_rows = length(unique(featureCat)), cutree_cols = length(unique(featureCat)),
  annotation_colors = annColors, annotation_col = annDf, annotation_row = annDf,
  fontsize_number = 6, show_colnames = FALSE, treeheight_col = FALSE, annotation_names_row = FALSE, annotation_legend_bottom = TRUE)


# Re-install CRAN package -------------------------------------------------
install.packages("pheatmap")

