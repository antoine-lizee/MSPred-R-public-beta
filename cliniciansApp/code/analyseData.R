
source("cliniciansApp/global.R")
source("cliniciansApp/server/utilities.R")
con <- getCon(dbPath = sprintf('cliniciansApp/%s', DBpath))

scoreDf <- getScoreTable()
visitsDf <- getVisitsTable()

ftable((data.frame(scoreDf, occ = 1) %>% 
          select(visit_id, clinician_id, occ) %>% 
          tidyr::spread(clinician_id, occ, fill = 0))[-1])

scoreDf %>% 
  left_join(visitsDf, by = c(visit_id = 'VisitID')) %>% 
  group_by(clinician_id) %>% 
  summarise(posProb = mean(class[!duplicated(visit_id)]),
            repeatProb = sum(duplicated(visit_id))/sum(!duplicated(visit_id)),
            tab2 = sum(table(visit_id) == 2),
            tab3 = sum(table(visit_id) == 3))


# Test getNext ------------------------------------------------------------

simulateScoring <- function(clinicianID) {
  writeScore(runif(1), getCurrentVisit(clinicianID))
  updateCurrentVisit(clinicianID)
}

data_frame(clinician_id = names(clinicianNames), n_simul = c(25, 42, 100)) %>% 
  group_by(clinician_id) %>% 
  do(visit_id = simulateScoring(clinician_id))
  
left_join(visitsDf, by = c(visit_id = 'VisitID')) %>% 
  group_by(clinician_id) %>% 
  summarise(posProb = mean(class[!duplicated(visit_id)]),
            repeatProb = sum(duplicated(visit_id))/sum(!duplicated(visit_id)),
            tab2 = sum(table(visit_id) == 2),
            tab3 = sum(table(visit_id) == 3))


