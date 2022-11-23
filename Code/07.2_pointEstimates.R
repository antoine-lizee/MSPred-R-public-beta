# 07.2
# Sub script for Analysis, tackling point estimates
# and creating the 2 way tables.
#
# Copyright Antoine Lizee @ UCSF 2016

require(XLConnect)


# Prepare data ------------------------------------------------------------

# All run from best median
perfCMTop <- perfDfCatMeth %>% filter(medAuroc == max(medAuroc))

# Single best median run from pack above:
perfTop <- perfCMTop %>%
  filter(row_number(auc) == (n() + 1) %/% 2) # select second median (the median could not exist in the sample if even cardinal)

tmaccF <- format2by2S(do.call(rbind, lapply(perfCMTop$pred, calc2by2, "macc")), sep = "\r")
tmacc <- format2by2S(calc2by2(perfTop$pred[[1]], "macc"))
# tr1 <- format2by2S(calc2by2(perfTop$pred[[1]], costRatio = 1)) # theoretically same as above
tmssF <- format2by2S(do.call(rbind, lapply(perfCMTop$pred, calc2by2, "mss")), sep = "\r")
tmss <- format2by2S(calc2by2(perfTop$pred[[1]], "mss"))
# tmf <- format2by2S(calc2by2(perfTop$pred[[1]], "mf")) # practically same as above
tr.5F <- format2by2S(do.call(rbind, lapply(perfCMTop$pred, calc2by2, costRatio = 0.2)), sep = "\r")
tr.5 <- format2by2S(calc2by2(perfTop$pred[[1]], costRatio = 0.2))
tr2F <- format2by2S(do.call(rbind, lapply(perfCMTop$pred, calc2by2, costRatio = 2)), sep = "\r")
tr2 <- format2by2S(calc2by2(perfTop$pred[[1]], costRatio = 2))


# Write tables to templates -----------------------------------------------

wb <- loadWorkbook("Output/ext2by2_template.xlsx", create = FALSE)
setStyleAction(wb,XLC$"STYLE_ACTION.NONE")
writeWorksheet(wb, tmacc, "Combined", startRow=3, startCol=9, header=FALSE)
writeWorksheet(wb, tr.5, "Combined", startRow=9, startCol=3, header=FALSE)
writeWorksheet(wb, tr2, "Combined", startRow=9, startCol=9, header=FALSE)
writeWorksheet(wb, tmaccF, "Full", startRow=3, startCol=9, header=FALSE)
writeWorksheet(wb, tr.5F, "Full", startRow=9, startCol=3, header=FALSE)
writeWorksheet(wb, tr2F, "Full", startRow=9, startCol=9, header=FALSE)
saveWorkbook(wb, file = file.path(yExtr$RD, "tables.xlsx"))
