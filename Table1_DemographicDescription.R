data <- xlsx::read.xlsx("./SynData/Table1_Data.xlsx", sheetIndex = 1)

data$ClinicalSpectrum = factor(data$ClinicalSpectrum, levels = c("Negative", "Asymptomatic or Presymptomatic Infection", "Mild moderate Illness", "Severe + Critical Illness"))

library(compareGroups)
export2xls(createTable(compareGroups(ClinicalSpectrum~Age+Sex+Profession+Education+MaritalStatus+Nationality+NSymptoms+followUp+SinceDiag+PCR, data = data, method = 2), q.type= c(2,3), show.p.overall = F), file = "Table1.xlsx")
export2xls(createTable(compareGroups(~Age+Sex+Profession+Education+MaritalStatus+Nationality+NSymptoms+followUp+SinceDiag+PCR, data = data, method = 2), q.type= c(2,3), show.p.overall = F), file = "Table1_Total.xlsx")

data%>%group_by(ClinicalSpectrum)%>%summarize(Age = paste0("[", min(Age),"-", max(Age), "]"),
                                              NSymptoms = paste0("[", min(NSymptoms,na.rm=T),"-", max(NSymptoms,na.rm=T), "]"),
                                              FollowUp = paste0("[", min(followUp),"-", max(followUp), "]"),
                                              SinceDiag = paste0("[", min(SinceDiag,na.rm=T),"-", max(SinceDiag,na.rm=T), "]"),)%>%
  as.data.frame()
  xlsx::write.xlsx("Table1.xlsx", sheetName = "MinMax", append = T, row.names = F)

data%>%summarize(Age = paste0("[", min(Age),"-", max(Age), "]"),
                                              NSymptoms = paste0("[", min(NSymptoms,na.rm=T),"-", max(NSymptoms,na.rm=T), "]"),
                                              FollowUp = paste0("[", min(followUp),"-", max(followUp), "]"),
                                              SinceDiag = paste0("[", min(SinceDiag,na.rm=T),"-", max(SinceDiag,na.rm=T), "]"),)%>%
  as.data.frame()
xlsx::write.xlsx("Table1_Overall.xlsx", sheetName = "MinMax", append = T, row.names = F)
