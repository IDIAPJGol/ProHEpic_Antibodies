data <- xlsx::read.xlsx("./SynData/Table2_Data.xlsx", sheetIndex = 1)

library(compareGroups)

data$DaysFromDays = factor(data$DaysFromDays, levels = c("0", "15", "30", "60", "90", "180", "270", "360"))

table2A_Data = droplevels(data[complete.cases(data),])
table2A <- createTable(compareGroups(DaysFromDays~Pattern, data = table2A_Data, max.ylev = 8), show.p.overall = F)

table2B_Data = droplevels(data[!is.na(data$IgG_c) & !is.na(data$IgM_c), ])
table2B_Data$Pattern = paste0(table2B_Data$IgM_c, table2B_Data$IgG_c)
table2B <- createTable(compareGroups(DaysFromDays~Pattern, data = table2B_Data, max.ylev = 8), show.p.overall = F)

export2xls(table2A, file = "Table2A.xlsx")
export2xls(table2B, file = "Table2B.xlsx")
