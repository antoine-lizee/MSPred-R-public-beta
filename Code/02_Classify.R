## Class
# Create the Classification functions
#
# Copyright Antoine Lizee @ UCSF 10/15
# antoine.lizee@ucsf.edu 


# delayed output table function
delayedEdss <- function(edssTable, id = "EPICID") {
  edssTable %>% group_by_(id) %>% 
  arrange(t) %>% 
  mutate(t1 = lead(t),
         t2 = lead(t, 2),
         EDSS1 = lead(EDSS),
         EDSS2 = lead(EDSS, 2),
         dT = t1 - t,
         dT1 = t2 - t1,
         dEDSS = EDSS1 - EDSS,
         dEDSS1 = EDSS2 - EDSS1) %>% 
  ungroup()
}

# Classification function
month <- as.difftime(365.25/12, units = "days")
classify <- function (edssExpTable, 
                      threNoInfo =  (24 + 2) * month,
                      threSig = (12 + 2) * month,
                      threEDSSsig = 3, 
                      thredEDSSstable = 0,
                      thredEDSSneg = 0,
                      thredEDSSpos = 1) {
  
  ## Creating the decision Table
  decTable <- edssExpTable %>% 
    mutate(VisitID = VisitID,
           # is the dt between visit too big?
           dTnoInfo = dT >= threNoInfo,
           # dEDSS classif
           dEDSSneg = dEDSS <= thredEDSSneg,
           dEDSSpos = dEDSS >= thredEDSSpos,
           # dEDSS anti-classif
           dEDSSnegAnti = dEDSS >= -thredEDSSneg,
           dEDSSposAnti = dEDSS <= -thredEDSSpos,
           # indecision
           dTsig = dT <= threSig,
           EDSSsig = EDSS >= threEDSSsig,
           # Confirm
           dEDSSind = !dEDSSneg & !dEDSSpos,
           dEDSS1Stable = dEDSS1 >= thredEDSSstable,
           # Anti-Confirm
           dEDSSindAnti = !dEDSSnegAnti & !dEDSSposAnti,
           dEDSS1StableAnti = dEDSS1 <= thredEDSSstable,
           class = NA,
           anticlass = NA)
  
  ## Class
  # What we know from scratch:
  decTable$class[decTable$dEDSSneg] <- FALSE
  decTable$class[decTable$dEDSSpos] <- TRUE
  # The indecision cases:
  decTable$class[decTable$dEDSSind & !decTable$dTsig] <- FALSE # Too far away
  decTable$class[decTable$dEDSSind & decTable$dTsig & decTable$EDSSsig] <- TRUE # Significant
  # Non-significant:
  decTable$class[decTable$dEDSSind & decTable$dTsig & !decTable$EDSSsig & decTable$dEDSS1Stable] <- TRUE 
  decTable$class[decTable$dEDSSind & decTable$dTsig & !decTable$EDSSsig & !decTable$dEDSS1Stable] <- FALSE 
  decTable$class[decTable$dEDSSind & decTable$dTsig & !decTable$EDSSsig & is.na(decTable$dEDSS1Stable)] <- FALSE 
  
  # NA to the non-informative:
  decTable$class[decTable$dTnoInfo] <- NA

  ## Anti-Class
  # That we know from scratch:
  decTable$anticlass[decTable$dEDSSnegAnti] <- FALSE
  decTable$anticlass[decTable$dEDSSposAnti] <- TRUE
  # The indecision cases:
  decTable$anticlass[decTable$dEDSSindAnti & !decTable$dTsig] <- FALSE # Too far away
  decTable$anticlass[decTable$dEDSSindAnti & decTable$dTsig & decTable$EDSSsig] <- TRUE # Significant
  # Non-significant:
  decTable$anticlass[decTable$dEDSSindAnti & decTable$dTsig & !decTable$EDSSsig & decTable$dEDSS1StableAnti] <- TRUE 
  decTable$anticlass[decTable$dEDSSindAnti & decTable$dTsig & !decTable$EDSSsig & !decTable$dEDSS1StableAnti] <- FALSE 
  decTable$anticlass[decTable$dEDSSindAnti & decTable$dTsig & !decTable$EDSSsig & is.na(decTable$dEDSS1StableAnti)] <- FALSE 
  
  # NA to the non-informative:
  decTable$anticlass[decTable$dTnoInfo] <- NA
  
  return(decTable)
}


