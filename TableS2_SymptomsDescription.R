library(compareGroups)
data <- xlsx::read.xlsx("./SynData/TableS2_Data.xlsx", sheetIndex = 1)


form <- as.formula(paste0("Gender ~", paste0(names(data%>%select(-ClinicalSpectrum,-Sex)), collapse = "+")))

tabMild <- createTable(compareGroups(form, data = data%>%filter(stringr::str_detect(ClinicalSpectrum, "Mild")), method = 3), hide.no = 0)
tabSevere <- createTable(compareGroups(form, data = data%>%filter(stringr::str_detect(ClinicalSpectrum, "Severe")), method = 3), hide.no = 0)

form <- as.formula(paste0("ClinicalSpectrum ~", paste0(names(data%>%select(-ClinicalSpectrum,-Sex)), collapse = "+")))
tabOverall <- createTable(compareGroups(form, data = data%>%filter(!stringr::str_detect(ClinicalSpectrum, "Asymp")), method = 3), hide.no = 0)

xlsx::write.xlsx(as.data.frame(tabMild$descr), file = "Table2.xlsx", sheetName = "Mild")
xlsx::write.xlsx(as.data.frame(tabSevere$descr), file = "Table2.xlsx", sheetName = "Severe")
xlsx::write.xlsx(as.data.frame(tabOverall$descr), file = "Table2.xlsx", sheetName = "Overall")
