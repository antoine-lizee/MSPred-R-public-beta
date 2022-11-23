
# Unfreeze version before
unFreeze("RawExtracted", "v2")

# Are we creating a freezed version of the data?
doFreeze <- TRUE
freezeName <- "RawExtractedWithTreatments"
freezeVersion <- "v2"
freezeObjects <- c("tableFinal", "DMTfinal")


# Stand-alone treatment re-processing based on csv ------------------------

treatments_raw <- readr::read_delim('Input/treatment.csv', ",", quote="\"", escape_backslash = TRUE, escape_double = FALSE)

## Show repartition
# treatments_raw %>%count(TreatmentMolecule, Class, AnalysisGroup) %>% arrange(AnalysisGroup, Class, TreatmentMolecule) %>% View

treatments <- treatments_raw %>% 
  mutate_each(funs(as.POSIXct), START, END) %>% 
  filter(!is.null(START) & !is.null(END) & !is.null(TreatmentMolecule) & TreatmentMolecule != "NULL") %>% 
  filter(AnalysisGroup != "Other") %>%  # Filtering out low-effect and non-dmts.
  filter(Class != "other") %>%  # Filtering out steroids.
  mutate(
    Class = factor(Class, levels = c("immunomodulator", "immunosuppressant", "monoclonal antibody")),
    Category = factor(AnalysisGroup, levels = c("Platform therapy", "High potency"))
  ) %>%   # Featurization
  select(EPICID, Molecule=TreatmentMolecule, Class, Category, START, END)

treatments[treatments$Molecule == "Ocrelizumab", "Category"] <- "High potency"  # Missing group for ocrlizumab
treatments[treatments$Molecule == "Gilenya (double blind)", "Molecule"] <- "Gilenya"  # Removing double blind annotation for Gilenya

# treatments %>% count(Molecule, Class, Category) %>% arrange(Category, Class, Molecule) %>% View

DMTfinal <- tableFinal[c("EPICID", "VisitID", "ExamDate")] %>%
  inner_join(treatments, by = "EPICID") %>%
  filter(is.na(START) | ExamDate - 2*31*24*3600 >= START,
         is.na(END) | ExamDate + 4*31*24*3600 <= END) %>%
  select(-EPICID) %>%
  distinct(VisitID, Molecule, .keep_all=TRUE) %>%  # Remove same treatments with overlapping windows.
  right_join(tableFinal['VisitID']) %>%
  group_by(VisitID) %>%
  summarise(
    DMTname = ifelse(is.na(Molecule[1]), "None", paste(Molecule[order(Class, Category, Molecule)], collapse=" + ")),
    nDMT = ifelse(is.na(Molecule[1]), 0, length(Molecule)),
    hasIM = ifelse(is.na(Molecule[1]), FALSE, "immunomodulator" %in% Class),
    hasIS = ifelse(is.na(Molecule[1]), FALSE, "immunosuppressant" %in% Class),
    hasMA = ifelse(is.na(Molecule[1]), FALSE, "monoclonal antibody" %in% Class),
    DMTcategory = ifelse(is.na(Molecule[1]), 0, max(as.numeric(Category)))
  ) %>% ungroup

DMTfinal %>% filter(nDMT == 2)

tableFinal <- tableFinal %>%
  left_join(DMTfinal, by = "VisitID")


# Save updated data -------------------------------------------------------

if (doFreeze) {
  freeze(freezeObjects, freezeName, freezeVersion)
}

