# library(arsenal)

data <- xlsx::read.xlsx("./Table1_Data.xlsx", sheetIndex = 1)


library(compareGroups)
export2xls(createTable(compareGroups(ClinicalSpectrum~Age+Sex+Profession+Education+MaritalStatus+Nationality+NSymptoms+followUp+SinceDiag+PCR, data = data, method = 2), q.type= c(2,3), show.p.overall = F), file = "Table1.xlsx")

data%>%group_by(ClinicalSpectrum)%>%summarize(Age = paste0("[", min(Age),"-", max(Age), "]"),
                                              NSymptoms = paste0("[", min(NSymptoms),"-", max(NSymptoms), "]"),
                                              FollowUp = paste0("[", min(followUp),"-", max(followUp), "]"),
                                              SinceDiag = paste0("[", min(SinceDiag,na.rm=T),"-", max(SinceDiag,na.rm=T), "]"),)%>%
  xlsx::write.xlsx("Table1.xlsx", sheetName = "MinMax", append = T)
