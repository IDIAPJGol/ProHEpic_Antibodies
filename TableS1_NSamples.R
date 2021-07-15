data <- xlsx::read.xlsx("TableS1_Data.xlsx", sheetIndex = 1)

library(dplyr)
data$DaysFromDays = factor(data$DaysFromDays, levels = c("0", "15", "30", "60", "90", "180", "270", "360"))

data%>%group_by(DaysFromDays)%>%summarize(MeanDays = mean(DaysFromStart), SDDays = sd(DaysFromStart),
                                          NIgM = sum(!is.na(IgM)), NIgG = sum(!is.na(IgG)), NIgG_S = sum(!is.na(IgS)))%>%
  xlsx::write.xlsx("TableS1.xlsx")
