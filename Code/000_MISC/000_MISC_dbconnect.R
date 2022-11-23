library(RMySQL)

con <- dbConnect(MySQL(), host = "chablis.ucsf.edu", 
                 port = msql_port, user = msql_u, password = msql_pswd,
                 dbname = "epic_etl_db")

visits <- dbReadTable(con, "visit")
visits %>% select(VisitID, DiseaseDuration, EDSS, MSSS) %>% 
  mutate(MSSS_new = getMSSS(tt$DiseaseDuration, tt$EDSS)) %>% 
  filter(MSSS != MSSS_new)