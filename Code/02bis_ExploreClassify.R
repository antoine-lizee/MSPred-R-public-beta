## Class
# Explore the classification
#
# Copyright Antoine Lizee @ UCSF 12/15
# antoine.lizee@ucsf.edu 

source("Code/Helpers.R")
source("Code/Helpers/CVer.R")

unFreeze("RawExtracted", "v2")
source("Code/01_Label.R")
source("Code/02_Classify.R")


# Merge both classifications ----------------------------------------------

decTotClass <- decTable %>% select(VisitID, class, anticlass) %>% 
  mutate(class = factor(class, levels = c(TRUE, FALSE), labels = c("worsening", "non-worsening")),
         anticlass = factor(anticlass, levels = c(TRUE, FALSE), labels = c("improvement", "non-improvement"))) %>% 
  mutate(classTot = c("problem", "improvement", "worsening", "stable")[as.numeric(class) + 2 * (as.numeric(anticlass) - 1)]) %>% 
  left_join(edssExpTable) %>% 
  left_join(tableFinal) 

decDf <- decTotClass %>% filter(!is.na(class)) %>% 
  mutate(t25Inv = 1 / T25FW,
         pbvcSh = asinh(Siena_PBVC * 100) / 100)

x_name = "EDSS"
ggplot(decDf, aes_string(x = x_name)) + 
  geom_density(aes(group = class, fill = class), position = 'fill')
ggplot(decDf, aes_string(x = x_name)) + 
  geom_density(aes(group = class, fill = class), position = 'stack')
ggplot(decDf, aes_string(x = x_name)) + 
  geom_density(aes(group = anticlass, fill = anticlass), position = 'fill')
ggplot(decDf, aes_string(x = x_name)) + 
  geom_density(aes(group = anticlass, fill = anticlass), position = 'stack')
  

# Plot them -------------------------------------------------------------

evoPlot <- function(x_name, adj = 1, pos = 'fill', lim = c(NA, NA), df = decDf){
  ggplot(df, aes_string(x = x_name)) + theme_bw() +
    geom_density(aes(group = classTot, fill = classTot), position = pos, adjust = adj, color = NA) +
    geom_density(aes(y = ..scaled..)) +
    labs(title = sprintf("Probability of evolution against %s", x_name), y = NULL) +
    scale_x_continuous(limits = lim) +
    scale_fill_discrete(limits = rev(levels(as.factor(df$classTot))))
}

pdf("Output/ClassExploEvo.pdf", w = 10, h = 6)

# EDSS
print(evoPlot("EDSS"))
print(evoPlot("EDSS", lim = c(1,7)))

# MSSS
print(evoPlot("MSSS"))

# DD, 
print(evoPlot("DiseaseDuration"))

# T25FW
print(evoPlot("T25FW", adj = 3))
print(evoPlot("T25FW", lim = c(2,10)))
print(evoPlot("t25Inv"))

# MRI
print(evoPlot("Siena_PBVC", adj = 1.5))
print(evoPlot("Siena_PBVC", lim = c(-0.03, 0.02)))
print(evoPlot("pbvcSh", lim = c(-0.03, 0.01)))

dev.off()



# Re-classify using different thresholds ----------------------------------

decTable2 <- classify(edssExpTable, 
                      threNoInfo =  365.25/12 * (24 + 2), 
                      threSig = 365.25/12 * (12 + 2), 
                      threEDSSsig = 3, 
                      thredEDSSstable = 0,
                      thredEDSSpos = 1.5,
                      thredEDSSneg = 0.5) 

decTotClass2 <- decTable2 %>% select(VisitID, class, anticlass) %>% 
  mutate(class = factor(class, levels = c(TRUE, FALSE), labels = c("worsening", "non-worsening")),
         anticlass = factor(anticlass, levels = c(TRUE, FALSE), labels = c("improvement", "non-improvement"))) %>% 
  mutate(classTot = c("problem", "improvement", "worsening", "stable")[as.numeric(class) + 2 * (as.numeric(anticlass) - 1)]) %>% 
  left_join(edssExpTable) %>% 
  left_join(tableFinal) 

decDf <- decTotClass2 %>% filter(!is.na(class)) %>% 
  mutate(t25Inv = 1 / T25FW,
         pbvcSh = asinh(Siena_PBVC * 100) / 100)

pdf("Output/ClassExploEvo2.pdf", w = 10, h = 6)

# EDSS
print(evoPlot("EDSS"))
print(evoPlot("EDSS", lim = c(1,7)))

# MSSS
print(evoPlot("MSSS"))

# DD, 
print(evoPlot("DiseaseDuration"))

# T25FW
print(evoPlot("T25FW", adj = 3))
print(evoPlot("T25FW", lim = c(2,10)))
print(evoPlot("t25Inv"))

# MRI
print(evoPlot("Siena_PBVC", adj = 1.5))
print(evoPlot("Siena_PBVC", lim = c(-0.03, 0.02)))
print(evoPlot("pbvcSh", lim = c(-0.03, 0.01)))

dev.off()

