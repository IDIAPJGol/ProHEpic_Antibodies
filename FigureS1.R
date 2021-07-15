# Libraries
library(nlme)
library(tidyverse)
library(ggpubr)
library(wesanderson)


# Data
serology <- xlsx::read.xlsx("Figures2_3_S1_Data.xlsx", sheetName = "Complete")
data <- xlsx::read.xlsx("Figures2_3_S1_Data.xlsx", sheetName = "TimeDiscretized")
data$DaysFromDays = factor(data$DaysFromDays, levels = c("0", "15", "30", "60", "90", "180", "270", "360"))
cols = wes_palette("Rushmore1", n = 5)[c(3,5,4)]

# Complete

## IgM(N)
means0IgM = data %>%
  group_by(DaysFromDays) %>%
  summarise(mean=mean(IgM,na.rm = T),
            sd = sd(IgM,na.rm = T),
            lower = mean-sd,
            upper=mean+sd)%>%as.data.frame()
all.IgM = nlme(IgM~b1+(b0-b1)*exp(-k1*DaysFromDays/360)-b1*k1*(exp(-k1*DaysFromDays/360)-exp(-k2*DaysFromDays/360)),
               data=data,
               fixed=b0+b1+k1+k2~1,
               random=k1~1,
               groups=~Id,
               na.action=na.pass,
               correlation = corAR1(),
               start=c(b0=1.8,b1=1.3,k1=6, k2= 12),
               control = nlmeControl(maxIter = 200,msMaxIter = 200))
cat("BIC: ", summary(all.IgM)$BIC, "\nAIC: ", summary(all.IgM)$AIC,"\n")
cat("Sigma residuals random effect: ", summary(all.IgM)$sigma,
    "\nDescription residuals (", names(summary(all.IgM)$residuals), "):", summary(all.IgM)$residuals, "\n")
summary(all.IgM)$tTable %>% data.frame %>%
  mutate(params=rownames(.)) %>% select(params,value = 1,se=2,pvalue=5)%>%mutate_if(is.numeric, round,4)%>%DT::datatable()
coefs = summary(all.IgM)$coefficients$fixed
b0 = coefs[1]
b1 = coefs[2]
k1 = coefs[3]
k2 = coefs[4]

days=seq(0,360,0.05)
all.IgM.forPlot = lapply(1:length(days),function(i){
  value = b1+(b0-b1)*exp(-k1*days[i]/360)-b1*k1*(exp(-k1*days[i]/360)-exp(-k2*days[i]/360))
  data.frame(DaysFromDays=days[i],IgM=value)
}) %>% bind_rows()%>%group_by(DaysFromDays)%>%summarize(IgM = mean(IgM))

IgM.plot = ggplot(left_join(all.IgM.forPlot, means0IgM),aes(x=DaysFromDays,y=IgM))+
  geom_point(aes(y=mean),size=3, alpha = 0.6)+
  geom_errorbar(aes(ymin=lower,ymax=upper), alpha = 0.6, size = 1)+
  geom_line(size = 1, alpha = 0.8, col = "RoyalBlue")+
  # ylim(c(0,4))+
  ylab('IgM(N) (index units)')+
  xlab("Time from symptoms onset (days)")+
  theme_classic()+
  theme(legend.position = "top")+
  geom_hline(yintercept = 1.1, size = 1, alpha = 0.5)+
  geom_hline(yintercept = 0.9, linetype = "dashed", size = 1, alpha = 0.5)+
  scale_x_continuous(breaks = c(0,15,30,60,90,180,270,360))


## IgG(N)
means0IgG = data %>%
  group_by(DaysFromDays) %>%
  summarise(mean=mean(IgG,na.rm = T),
            sd = sd(IgG, na.rm = T),
            lower = mean-sd,
            upper=mean+sd)%>%as.data.frame()
ggplot(data,aes(x=DaysFromDays,y=IgG,group=DaysFromDays))+
  geom_violin()+theme_classic()

all.IgG = nlme(IgG~b1+(b0-b1)*exp(-k1*DaysFromDays/360)-b1*k1*(exp(-k1*DaysFromDays/360)-exp(-k2*DaysFromDays/360)),
               data=data,
               fixed=b0+b1+k1+k2~1,
               random=b0~1,
               groups=~Id,
               na.action=na.pass,
               correlation = corAR1(),
               start=c(b0=3,
                       b1=3.5,
                       k1=6,
                       k2=12),
               control = nlmeControl(maxIter = 300,msMaxIter = 200, minScale = 0.00000001, pnlsTol = 0.01))
cat("BIC: ", summary(all.IgG)$BIC, "\nAIC: ", summary(all.IgG)$AIC,"\n")
cat("Sigma residuals random effect: ", summary(all.IgG)$sigma,"\nDescription residuals (", names(summary(all.IgG)$residuals), "):", summary(all.IgG)$residuals, "\n")
summary(all.IgG)$tTable %>% data.frame %>%
  mutate(params=rownames(.)) %>% select(params,value = 1,se=2,pvalue=5)%>%mutate_if(is.numeric, round,4)%>%DT::datatable()
coefs = summary(all.IgG)$coefficients$fixed
b0 = coefs[1]
b1 = coefs[2]
k1 = coefs[3]
k2 = coefs[4]

days=seq(0,360,0.1)
all.IgG.forPlot = lapply(1:length(days),function(i){
  value = b1+(b0-b1)*exp(-k1*days[i]/360)-b1*k1*(exp(-k1*days[i]/360)-exp(-k2*days[i]/360))
  data.frame(DaysFromDays=days[i],IgG=value)
}) %>% bind_rows()%>%group_by(DaysFromDays)%>%summarize(IgG = mean(IgG))

IgG.plot = ggplot(left_join(all.IgG.forPlot, means0IgG),aes(x=DaysFromDays,y=IgG))+
  geom_point(aes(y=mean),size=3, alpha = 0.6)+
  geom_errorbar(aes(ymin=lower,ymax=upper), alpha = 0.6, size = 1)+
  geom_line(size = 1, alpha = 0.8, col = "RoyalBlue")+
  ylab('IgG(N) (index units)')+
  xlab("Time from symptoms onset (days)")+
  theme_classic()+
  theme(legend.position = "top")+
  geom_hline(yintercept = 1.1, size = 1, alpha = 0.5)+
  geom_hline(yintercept = 0.9, linetype = "dashed", size = 1, alpha = 0.5)+
  scale_x_continuous(breaks = c(0,15,30,60,90,180,270,360))

## IgG(S)
means0IgS = data %>%filter(!DaysFromDays%in% c(360))%>%
  group_by(DaysFromDays) %>%
  summarise(mean=mean(IgS,na.rm = T),
            sd = sd(IgS, na.rm = T),
            lower = mean-sd,
            upper=mean+sd)%>%as.data.frame()
ggplot(data,aes(x=DaysFromDays,y=IgS,group=DaysFromDays))+
  geom_violin()+theme_classic()

all.IgS = nlme(IgS~b1+(b0-b1)*exp(-k1*DaysFromDays/270)-b1*k1*(exp(-k1*DaysFromDays/270)),
               data=data%>%filter(!DaysFromDays%in% c(360)),
               fixed=b0+b1+k1~1,
               random=k1~1,
               groups=~Id,
               na.action=na.pass,
               correlation = corAR1(),
               start=c(b0=90,
                       b1=210,
                       k1=26),
               control = nlmeControl(maxIter = 300,msMaxIter = 200, minScale = 0.00000001, pnlsTol = 0.01))
cat("BIC: ", summary(all.IgS)$BIC, "\nAIC: ", summary(all.IgS)$AIC,"\n")
cat("Sigma residuals random effect: ", summary(all.IgS)$sigma,"\nDescription residuals (", names(summary(all.IgS)$residuals), "):", summary(all.IgS)$residuals, "\n")
summary(all.IgS)$tTable %>% data.frame %>%
  mutate(params=rownames(.)) %>% select(params,value = 1,se=2,pvalue=5)%>%mutate_if(is.numeric, round,4)%>%DT::datatable()
coefs = summary(all.IgS)$coefficients$fixed
b0 = coefs[1]
b1 = coefs[2]
k1 = coefs[3]
k2 = coefs[4]

days=seq(0,270,0.1)
all.IgS.forPlot = lapply(1:length(days),function(i){
  value = b1+(b0-b1)*exp(-k1*days[i]/270)-b1*k1*(exp(-k1*days[i]/270))
  data.frame(DaysFromDays=days[i],IgS=value)
}) %>% bind_rows()%>%group_by(DaysFromDays)%>%summarize(IgS = mean(IgS))

IgS.plot = ggplot(left_join(all.IgS.forPlot, means0IgS),aes(x=DaysFromDays,y=IgS))+
  geom_point(aes(y=mean),size=3, alpha = 0.6)+
  geom_errorbar(aes(ymin=lower,ymax=upper), alpha = 0.6, size = 1)+
  geom_line(size = 1, alpha = 0.8, col = "RoyalBlue")+
  ylab('IgG(S) (IU/mL)')+
  xlab("Time from diagnosis (days)")+
  theme_classic()+
  theme(legend.position = "top")+
  geom_hline(yintercept = 40, size = 1, alpha = 0.5)+
  geom_hline(yintercept = 32, linetype = "dashed", size = 1, alpha = 0.5)+
  scale_x_continuous(breaks = c(0,15,30,60,90,180,270))

# Sex

## IgM(N)
means = data %>%
  group_by(Sex,DaysFromDays) %>%
  summarise(mean=mean(IgM,na.rm = T),
            sd = sd(IgM, na.rm =T),
            lower = mean-sd,
            upper=mean+sd)%>%as.data.frame()
means = rbind(means,means0IgM%>%mutate(ClinicalSpectrum = "All"))

group.IgM = nlme(IgM~b1+(b0-b1)*exp(-k1*DaysFromDays/360)-b1*k1*(exp(-k1*DaysFromDays/360)-exp(-k2*DaysFromDays/360)),
                 data=data,
                 fixed=b0+b1+k1+k2~Sex,
                 random=k2~1,
                 groups=~Id,
                 start=c(1.7,0,
                         1.4,0,
                         12,0,
                         6,0),
                 na.action=na.pass,
                 correlation = corAR1(),
                 control=nlmeControl(maxIter=100,msMaxIter =100))
cat("BIC: ", summary(group.IgM)$BIC, "\nAIC: ", summary(group.IgM)$AIC,"\n")
cat("Sigma residuals random effect: ", summary(group.IgM)$sigma,"\nDescription residuals (", names(summary(group.IgM)$residuals), "):", summary(group.IgM)$residuals, "\n")
summary(group.IgM)$tTable %>% data.frame %>%
  mutate(params=rownames(.)) %>% select(params,value = 1,se=2,pvalue=5)%>%mutate_if(is.numeric, round,4)%>%DT::datatable()

coefs = group.IgM$coefficients$fixed
b0=coefs[1:3]
b0[2:3] = b0[2:3]+b0[1]
b1=coefs[4:6]
b1[2:3] = b1[2:3]+b1[1]
k1=coefs[7:9]
k1[2:3] = k1[2:3]+k1[1]
k2=coefs[10:12]
k2[2:3] = k2[2:3]+k2[1]

days=seq(0,360,0.05)
group.IgM.forPlot = lapply(1:length(days),function(i){
  Mild = b1[1]+(b0[1]-b1[1])*exp(-k1[1]*days[i]/360)-b1[1]*k1[1]*(exp(-k1[1]*days[i]/360)-exp(-k2[1]*days[i]/360))
  Assymp = b1[2]+(b0[2]-b1[2])*exp(-k1[2]*days[i]/360)-b1[2]*k1[2]*(exp(-k1[2]*days[i]/360)-exp(-k2[2]*days[i]/360))
  Severe = b1[3]+(b0[3]-b1[3])*exp(-k1[3]*days[i]/360)-b1[3]*k1[3]*(exp(-k1[3]*days[i]/360)-exp(-k2[3]*days[i]/360))
  data.frame(DaysFromDays=days[i], Male = Male, Female = Female)
}) %>% bind_rows()

group.IgM.forPlot = reshape2::melt(group.IgM.forPlot,id.vars='DaysFromDays') %>%
  select(DaysFromDays,Sex=variable, value)%>%rename("IgM" = "value")
IgM.forPlot = rbind(all.IgM.forPlot%>%mutate(Sex = "All"), group.IgM.forPlot%>%select(DaysFromDays, IgM, Sex))
IgM.forPlot = left_join(IgM.forPlot, means)


IgM.plot = ggplot(IgM.forPlot,aes(x=DaysFromDays,y=IgM,col=Sex, fill = Sex))+
  geom_point(aes(y=mean),size=3, alpha = 0.6)+
  geom_errorbar(aes(ymin=lower,ymax=upper), alpha = 0.6, size = 1)+
  geom_line(size = 1, alpha = 0.6)+
  ylab('IgM(N) (index units)')+
  xlab("Time from symptoms onset (days)")+
  theme_classic()+
  theme(legend.position = "top")+
  scale_fill_manual(values = cols)+
  scale_colour_manual(values = cols)+
  geom_hline(yintercept = 1.1, size = 1, alpha = 0.5)+
  geom_hline(yintercept = 0.9, linetype = "dashed", size = 1, alpha = 0.5)+
  scale_x_continuous(breaks = c(0,15,30,60,90,180,270,360))

## IgG(N)
means = data %>%
  group_by(Sex,DaysFromDays) %>%
  summarise(mean=mean(IgG,na.rm = T),
            sd = sd(IgG,na.rm = T),
            lower = mean-sd,
            upper=mean+sd)%>%as.data.frame()
means = rbind(means,means0IgG%>%mutate(Sex = "All"))

group.IgG = nlme(IgG~b1+(b0-b1)*exp(-k1*DaysFromDays/360)-b1*k1*(exp(-k1*DaysFromDays/360)-exp(-k2*DaysFromDays/360)),
                 data=data,
                 fixed=b0+b1+k1+k2~Sex,
                 random=b0~1,
                 groups=~Id,
                 start=c(3,0,
                         3.5,0,
                         6,3,
                         12,2),
                 na.action=na.pass,
                 correlation = corAR1(),
                 control=nlmeControl(maxIter=100,msMaxIter =200, minScale = 0.00000001, pnlsTol = 0.01))
cat("BIC: ", summary(group.IgG)$BIC, "\nAIC: ", summary(group.IgG)$AIC,"\n")
cat("Sigma residuals random effect: ", summary(group.IgG)$sigma,"\nDescription residuals (", names(summary(group.IgG)$residuals), "):", summary(group.IgG)$residuals, "\n")
summary(group.IgG)$tTable %>% data.frame %>%
  mutate(params=rownames(.)) %>% select(params,value = 1,se=2,pvalue=5)%>%mutate_if(is.numeric, round,4)%>%DT::datatable()

coefs = group.IgG$coefficients$fixed
b0=coefs[1:3]
b0[2:3] = b0[2:3]+b0[1]
b1=coefs[4:6]
b1[2:3] = b1[2:3]+b1[1]
k1=coefs[7:9]
k1[2:3] = k1[2:3]+k1[1]
k2=coefs[10:12]
k2[2:3] = k2[2:3]+k2[1]


group.IgG.forPlot = lapply(1:length(days),function(i){
  Mild = b1[1]+(b0[1]-b1[1])*exp(-k1[1]*days[i]/360)-b1[1]*k1[1]*(exp(-k1[1]*days[i]/360)-exp(-k2[1]*days[i]/360))
  Assymp = b1[2]+(b0[2]-b1[2])*exp(-k1[2]*days[i]/360)-b1[2]*k1[2]*(exp(-k1[2]*days[i]/360)-exp(-k2[2]*days[i]/360))
  Severe = b1[3]+(b0[3]-b1[3])*exp(-k1[3]*days[i]/360)-b1[3]*k1[3]*(exp(-k1[3]*days[i]/360)-exp(-k2[3]*days[i]/360))
  data.frame(DaysFromDays=days[i], Male = Male, Female = Female)
}) %>% bind_rows()

group.IgG.forPlot = reshape2::melt(group.IgG.forPlot,id.vars='DaysFromDays') %>%
  select(DaysFromDays,Sex=variable, value)%>%rename("IgG" = "value")

IgG.forPlot = rbind(all.IgG.forPlot%>%mutate(Sex = "All"), group.IgG.forPlot%>%select(DaysFromDays, IgG, Sex))
IgG.forPlot = left_join(IgG.forPlot, means)

IgG.plot = ggplot(IgG.forPlot,aes(x=DaysFromDays,y=IgG,col=Sex, fill = Sex))+
  geom_point(aes(y=mean),size=3, alpha = 0.6)+
  geom_errorbar(aes(ymin=lower,ymax=upper), alpha = 0.6, size = 1)+
  geom_line(size = 1, alpha = 0.6)+
  ylab('IgG(N) (index units)')+
  xlab("Time from symptoms onset (days)")+
  theme_classic()+
  theme(legend.position = "top")+
  scale_fill_manual(values = cols)+
  scale_colour_manual(values = cols)+
  geom_hline(yintercept = 1.1, size = 1, alpha = 0.5)+
  geom_hline(yintercept = 0.9, linetype = "dashed", size = 1, alpha = 0.5)+
  scale_x_continuous(breaks = c(0,15,30,60,90,180,270,360))

## IgG(S)
means = data %>% filter(!DaysFromDays %in% c(360))%>%
  group_by(Sex,DaysFromDays) %>%
  summarise(mean=mean(IgS,na.rm = T),
            sd = sd(IgS,na.rm = T),
            lower = mean-sd,
            upper=mean+sd)%>%as.data.frame()
means = rbind(means,means0IgS%>%mutate(Sex = "All"))
group.IgS = nlme(IgS~b1+(b0-b1)*exp(-k1*DaysFromDays/270)-b1*k1*(exp(-k1*DaysFromDays/270)),
                 data=data%>% filter(!DaysFromDays %in% c(360)),
                 fixed=b0+b1+k1~Sex,
                 random=b0~1,
                 groups=~Id,
                 start=c(80,40,
                         240,-40,
                         24,0),
                 na.action=na.pass,
                 correlation = corAR1(),
                 control=nlmeControl(maxIter=200,msMaxIter =200, minScale = 0.00000001, pnlsTol = 0.01))
cat("BIC: ", summary(group.IgS)$BIC, "\nAIC: ", summary(group.IgS)$AIC,"\n")
cat("Sigma residuals random effect: ", summary(group.IgS)$sigma,"\nDescription residuals (", names(summary(group.IgS)$residuals), "):", summary(group.IgS)$residuals, "\n")
summary(group.IgS)$tTable %>% data.frame %>%
  mutate(params=rownames(.)) %>% select(params,value = 1,se=2,pvalue=5)%>%mutate_if(is.numeric, round,4)%>%DT::datatable()

coefs = group.IgS$coefficients$fixed
b0=coefs[1:3]
b0[2:3] = b0[2:3]+b0[1]
b1=coefs[4:6]
b1[2:3] = b1[2:3]+b1[1]
k1=coefs[7:9]
k1[2:3] = k1[2:3]+k1[1]

days=seq(0,270,0.1)
group.IgS.forPlot = lapply(1:length(days),function(i){
  Mild = b1[1]+(b0[1]-b1[1])*exp(-k1[1]*days[i]/270)-b1[1]*k1[1]*(exp(-k1[1]*days[i]/270))
  Assymp = b1[2]+(b0[2]-b1[2])*exp(-k1[2]*days[i]/270)-b1[2]*k1[2]*(exp(-k1[2]*days[i]/270))
  Severe = b1[3]+(b0[3]-b1[3])*exp(-k1[3]*days[i]/270)-b1[3]*k1[3]*(exp(-k1[3]*days[i]/270))
  data.frame(DaysFromDays=days[i], Male = Male, Female = Female)
}) %>% bind_rows()

group.IgS.forPlot = reshape2::melt(group.IgS.forPlot,id.vars='DaysFromDays') %>%
  select(DaysFromDays,Sex=variable, value)%>%rename("IgS" = "value")

IgS.forPlot = rbind(all.IgS.forPlot%>%mutate(Sex = "All"), group.IgS.forPlot%>%select(DaysFromDays, IgS, Sex))
IgS.forPlot = left_join(IgS.forPlot, means)

IgS.plot = ggplot(IgS.forPlot,aes(x=DaysFromDays,y=IgS,col=Sex, fill = Sex))+
  geom_point(aes(y=mean),size=3, alpha = 0.6)+
  geom_errorbar(aes(ymin=lower,ymax=upper), alpha = 0.6, size = 1)+
  geom_line(size = 1, alpha = 0.6)+
  ylab('IgG(S) (IU/mL)')+
  xlab("Time from diagnosis (days)")+
  theme_classic()+
  theme(legend.position = "top")+
  scale_fill_manual(values = cols)+
  scale_colour_manual(values = cols)+
  geom_hline(yintercept = 40, size = 1, alpha = 0.5)+
  geom_hline(yintercept = 32, linetype = "dashed", size = 1, alpha = 0.5)+
  scale_x_continuous(breaks = c(15,30,60,90,180,270,360))

# LOESS

## IgM(N)
IgM.Time = serology%>%
  ggplot(aes(y=IgM, x = DaysFromStart,  col = Sex, fill = Sex))+
  geom_line(aes(group = Id),alpha = 0.2, col = "black")+
  geom_point(alpha = 0.6, size = 3)+
  geom_point(alpha = 0, size = 3, aes(col = "All", fill = "All"))+
  geom_smooth(method = "loess", fill = "black", aes(col = "All", fill = "All"), se = T, alpha = 0.2 )+
  geom_smooth(method = "loess", fill = "black", se = T, alpha = 0.2, aes(group=Sex))+
  scale_fill_manual(values = cols)+
  scale_colour_manual(values = cols)+
  xlab("Time from diagnosis (days)")+
  ylab("IgM(N) (index units)")+
  theme(legend.position = "top")+
  geom_hline(yintercept = 1.1, size = 1, alpha = 0.5)+
  geom_hline(yintercept = 0.9, linetype = "dashed", size = 1, alpha = 0.5)+
  theme_classic()+
  scale_x_continuous(breaks = c(0,15,30,60,90,180,270,360))

## IgG(N)
IgG.Time = serology%>%
  ggplot(aes(y=IgG, x = DaysFromStart,  col = Sex, fill = Sex))+
  geom_line(aes(group = Id),alpha = 0.2, col = "black")+
  geom_point(alpha = 0.6, size = 3)+
  geom_point(alpha = 0, size = 3, aes(col = "All", fill = "All"))+
  geom_smooth(method = "loess", fill = "black", aes(col = "All", fill = "All"), se = T, alpha = 0.2 )+
  geom_smooth(method = "loess", fill = "black", se = T, alpha = 0.2, aes(group=Sex))+
  scale_fill_manual(values = cols)+
  scale_colour_manual(values = cols)+
  xlab("Time from diagnosis (days)")+
  ylab("IgG(N) (index units)")+
  theme(legend.position = "top")+
  geom_hline(yintercept = 1.1, size = 1, alpha = 0.5)+
  geom_hline(yintercept = 0.9, linetype = "dashed", size = 1, alpha = 0.5)+
  theme_classic()+
  scale_x_continuous(breaks = c(0,15,30,60,90,180,270,360))
## IgG(S)
IgS.Time = serology%>%
  ggplot(aes(y=IgS, x = DaysFromStart,  col = Sex, fill = Sex))+
  geom_line(aes(group = Id),alpha = 0.2, col = "black")+
  geom_point(alpha = 0.6, size = 3)+
  geom_point(alpha = 0, size = 3, aes(col = "All", fill = "All"))+
  geom_smooth(method = "loess", fill = "black", aes(col = "All", fill = "All"), se = T, alpha = 0.2 )+
  geom_smooth(method = "loess", fill = "black", se = T, alpha = 0.2, aes(group=Sex))+
  scale_fill_manual(values = cols)+
  scale_colour_manual(values = cols)+
  xlab("Time from diagnosis (days)")+
  ylab("IgG(S) (IU/mL)")+
  theme(legend.position = "top")+
  geom_hline(yintercept = 1.1, size = 1, alpha = 0.5)+
  geom_hline(yintercept = 0.9, linetype = "dashed", size = 1, alpha = 0.5)+
  theme_classic()+
  scale_x_continuous(breaks = c(0,15,30,60,90,180,270,360))


# Plot

ggarrange(plotlist = list(IgM.Time, IgM.plot, IgG.Time, IgG.plot, IgS.Time, IgS.plot), ncol = 2, nrow = 3, common.legend = T)
