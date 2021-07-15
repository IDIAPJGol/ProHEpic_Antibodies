library(compareGroups)
data <- xlsx::read.xlsx("TableS2_Data.xlsx", sheetIndex = 1)


form <- as.formula(paste0("Sex ~", paste0(names(data%>%select(-ClinicalSpectrum,-Sex, -Id)), collapse = "+")))

tabMild <- createTable(compareGroups(form, data = data%>%filter(str_detect(ClinicalSpectrum, "Mild")), method = 3), hide.no = 0)
tabSevere <- createTable(compareGroups(form, data = data%>%filter(str_detect(ClinicalSpectrum, "Severe")), method = 3), hide.no = 0)

form <- as.formula(paste0("ClinicalSpectrum ~", paste0(names(data%>%select(-ClinicalSpectrum,-Sex, -Id)), collapse = "+")))
tabOverall <- createTable(compareGroups(form, data = data%>%filter(!str_detect(ClinicalSpectrum, "Asymp")), method = 3), hide.no = 0)

xlsx::write.xlsx(as.data.frame(tabMild$descr), file = "Table2.xlsx", sheetName = "Mild")
xlsx::write.xlsx(as.data.frame(tabSevere$descr), file = "Table2.xlsx", sheetName = "Severe")
xlsx::write.xlsx(as.data.frame(tabOverall$descr), file = "Table2.xlsx", sheetName = "Overall")
