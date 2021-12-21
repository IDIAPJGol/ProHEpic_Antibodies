
# Libraries
library(ggpubr)
library(rstatix)
library(patchwork)
library(wesanderson)

# Data
data <- xlsx::read.xlsx("./SynData/Figures1_2_3_S2_Data.xlsx", sheetName = "TimeDiscretized")
data$DaysFromDays = factor(data$DaysFromDays, levels = c("0", "15", "30", "60", "90", "180", "270", "360"))

# General plot

## IgM(N)
test = dunn_test(data, IgM~DaysFromDays)%>%add_xy_position(x = "DaysFromDays")
test$y.position = 15 +  c(1:nrow(test))

means <- aggregate(IgM~DaysFromDays, data, mean)
means$IgM <- round(means$IgM,2)

p1<-ggboxplot(data%>%filter(DaysFromDays != 450), x = "DaysFromDays", y = "IgM", xlab = "Time from diangosis (days)", ylab ="IgM(N) (index units)", add = c("mean", "jitter"), add.params = list(alpha = 0.05))+
  stat_pvalue_manual(test, label = "p.adj.signif", hide.ns = T, tip.length = 0.01)+
  geom_hline(yintercept = 1.1, size = 1, alpha = 0.5, col = "darkred")+
  geom_hline(yintercept = 0.9, linetype = "dashed", size = 1, alpha = 0.5, col = "darkred")+
  geom_label(data = means, alpha = 0.5, aes(label = IgM, y = IgM + 0.3*IQR(data$IgM, na.rm = T)))


## IgG(N)
test = dunn_test(data, IgG~DaysFromDays)%>%add_xy_position(x = "DaysFromDays")
test$y.position = 15 +  c(1:nrow(test))

means <- aggregate(IgG~DaysFromDays, data, mean)
means$IgG <- round(means$IgG,2)

p4<-ggboxplot(data, x = "DaysFromDays", y = "IgG", xlab = "Time from diangosis (days)", ylab ="IgG(N) (index units)", add = c("mean", "jitter"), add.params = list(alpha = 0.05))+
  stat_pvalue_manual(test, label = "p.adj.signif", hide.ns = T, tip.length = 0.01)+
  geom_hline(yintercept = 1.1, size = 1, alpha = 0.5, col = "darkred")+
  geom_hline(yintercept = 0.9, linetype = "dashed", size = 1, alpha = 0.5, col= "darkred")+
  geom_label(data = means, alpha = 0.5, aes(label = IgG, y = IgG + 0.2*IQR(data$IgG, na.rm = T)))

## IgG(S)
test = dunn_test(data%>%filter(DaysFromDays!=360), IgS~DaysFromDays)#%>%add_xy_position(x = "DaysFromDays")
test$y.position = 1400 +  100*c(1:nrow(test))

means <- aggregate(IgS~DaysFromDays, data%>%filter(DaysFromDays!=360), mean)
means$IgS <- round(means$IgS,2)

p7<-ggboxplot(data%>%filter(DaysFromDays!=360), x = "DaysFromDays", y = "IgS", xlab = "Time from diagnosis (days)", ylab ="IgG(S) (IU/mL)", add = c("mean", "jitter"), add.params = list(alpha = 0.05))+
  stat_pvalue_manual(test, label = "p.adj.signif", hide.ns = T, tip.length = 0.01)+
  geom_hline(yintercept = 40, size = 1, alpha = 0.5, col = "darkred")+
  geom_hline(yintercept = 32, linetype = "dashed", size = 1, alpha = 0.5, col= "darkred")+
  geom_label(data = means, alpha = 0.5, aes(label = IgS, y = IgS + 0.45*IQR(data$IgS, na.rm = T)))

# Clinical Spectrum
## IgM(N)
test = data%>%group_by(DaysFromDays)%>%dunn_test(IgM~ClinicalSpectrum)%>%
  filter(p.adj.signif != "ns")%>%
  add_xy_position(x = "DaysFromDays", step.increase = 0.03)

means <- data%>%group_by(DaysFromDays, ClinicalSpectrum)%>%summarize(IgM = mean(IgM))%>%as.data.frame()
means$IgM <- round(means$IgM,2)

p2<-ggboxplot(data, x = "DaysFromDays", y = "IgM", col = "ClinicalSpectrum", group = "ClinicalSpectrum",
              xlab = "Time from diangosis (days)", ylab ="IgM(N) (index units)", add = c("mean", "jitter"), add.params = list(alpha = 0.05))+
  stat_pvalue_manual(test, label = "p.adj.signif", hide.ns = T, tip.length = 0.01)+
  geom_hline(yintercept = 1.1, size = 1, alpha = 0.5)+
  geom_hline(yintercept = 0.9, linetype = "dashed", size = 1, alpha = 0.5)+
  scale_color_manual(values = c("#00AFBB", "#E7B800", "#FC4E07"), name = "Disease severity")+
  stat_summary(geom = "label", alpha = 0.5, mapping = aes(label = round(..y.., 1), group = ClinicalSpectrum, vjust = -1.), position = position_dodge(0.8))

## IgG(N)
test = data%>%group_by(DaysFromDays)%>%dunn_test(IgG~ClinicalSpectrum)%>%
  filter(p.adj.signif != "ns")%>%
  add_xy_position(x = "DaysFromDays", step.increase = 0.03)

means <- aggregate(IgG~ClinicalSpectrum, data%>%group_by(DaysFromDays), mean)
means$IgG <- round(means$IgG,2)


p5<-ggboxplot(data, x = "DaysFromDays", y = "IgG", col = "ClinicalSpectrum",
              xlab = "Time from diangosis (days)", ylab ="IgG(N) (index units)", add = c("mean", "jitter"), add.params = list(alpha = 0.05))+
  stat_pvalue_manual(test, label = "p.adj.signif", hide.ns = T, tip.length = 0.01)+
  geom_hline(yintercept = 1.1, size = 1, alpha = 0.5)+
  geom_hline(yintercept = 0.9, linetype = "dashed", size = 1, alpha = 0.5)+
  scale_color_manual(values = c("#00AFBB", "#E7B800", "#FC4E07"), name = "Disease severity")+
   stat_summary(geom = "label", alpha = 0.5, mapping = aes(label = round(..y.., 1), group = ClinicalSpectrum,vjust = -1), position = position_dodge(0.8))

## IgG(S)
test = data%>%filter(DaysFromDays!=360)%>%group_by(DaysFromDays)%>%dunn_test(IgS~ClinicalSpectrum)%>%
  filter(p.adj.signif != "ns")%>%
  add_xy_position(x = "DaysFromDays", step.increase = 0.03)
means <- aggregate(IgS~ClinicalSpectrum, data%>%filter(DaysFromDays!=360)%>%group_by(DaysFromDays), mean)
means$IgS <- round(means$IgS,2)


p8<-ggboxplot(data%>%filter(DaysFromDays!=360), x = "DaysFromDays", y = "IgS", col = "ClinicalSpectrum",
              xlab = "Time from diagnosis (days)", ylab ="IgG(S) (IU/mL)", add = c("mean", "jitter"), add.params = list(alpha = 0.05))+
  stat_pvalue_manual(test, label = "p.adj.signif", hide.ns = T, tip.length = 0.01)+
  geom_hline(yintercept = 40, size = 1, alpha = 0.5)+
  geom_hline(yintercept = 32, linetype = "dashed", size = 1, alpha = 0.5)+
  scale_color_manual(values = c("#00AFBB", "#E7B800", "#FC4E07"), name = "Disease severity")+
   stat_summary(geom = "label", alpha = 0.5, mapping = aes(label = round(..y.., 1), group = ClinicalSpectrum,vjust = -1), position = position_dodge(0.8))

# Gender
cols = wes_palette("Rushmore1", n = 5)[c(5,4)]
## IgM(N)
test = data%>%group_by(DaysFromDays)%>%dunn_test(IgM~Gender)%>%
  add_xy_position(x = "DaysFromDays")
test$y.position = 15 +  c(1:nrow(test))

means <- aggregate(IgM~Gender, data%>%group_by(DaysFromDays), mean)
means$IgM <- round(means$IgM,2)

p3<-ggboxplot(data, x = "DaysFromDays", y = "IgM", col = "Gender",
              xlab = "Time from diangosis (days)", ylab ="IgM(N) (index units)", palette = cols, add = c("mean", "jitter"), add.params = list(alpha = 0.05))+
  stat_pvalue_manual(test, label = "p.adj.signif", hide.ns = T)+
  geom_hline(yintercept = 1.1, size = 1, alpha = 0.5)+
  geom_hline(yintercept = 0.9, linetype = "dashed", size = 1, alpha = 0.5)+
  scale_color_manual(values = cols, name = "Gender")+
   stat_summary(geom = "label", alpha = 0.5, mapping = aes(label = round(..y.., 1), group = Gender,vjust = -1), position = position_dodge(0.8))

## IgG(N)
test = data%>%group_by(DaysFromDays)%>%dunn_test(IgG~Gender)%>%
  add_xy_position(x = "DaysFromDays", step.increase = 1)
test$y.position = 15 +  0.2*c(1:nrow(test))

means <- aggregate(IgG~Gender, data%>%group_by(DaysFromDays), mean)
means$IgG <- round(means$IgG,2)

p6 <-ggboxplot(data, x = "DaysFromDays", y = "IgG", col = "Gender",
               xlab = "Time from diangosis (days)", ylab ="IgG(N) (index units)", palette = cols, add = c("mean", "jitter"), add.params = list(alpha = 0.05))+
  stat_pvalue_manual(test, label = "p.adj.signif", hide.ns = T, tip.length = 0.01)+
  geom_hline(yintercept = 1.1, size = 1, alpha = 0.5)+
  geom_hline(yintercept = 0.9, linetype = "dashed", size = 1, alpha = 0.5)+
  scale_color_manual(values = cols, name = "Gender")+
   stat_summary(geom = "label", alpha = 0.5, mapping = aes(label = round(..y.., 1), group = Gender,vjust = -1), position = position_dodge(0.8))


## IgG(S)
test = data%>%filter(DaysFromDays!=360)%>%group_by(DaysFromDays)%>%dunn_test(IgS~Gender)%>%
  add_xy_position(x = "DaysFromDays", step.increase = 0.3)

means <- aggregate(IgS~Gender, data%>%filter(DaysFromDays!=360)%>%group_by(DaysFromDays), mean)
means$IgS <- round(means$IgS,2)

p9 <-ggboxplot(data%>%filter(DaysFromDays!=360), x = "DaysFromDays", y = "IgS", col = "Gender",
               xlab = "Time from diagnosis (days)", ylab ="IgG(S) (IU/mL)", palette = cols, add = c("mean", "jitter"), add.params = list(alpha = 0.05))+
  stat_pvalue_manual(test, label = "p.adj.signif", hide.ns = T, tip.length = 0.01)+
  geom_hline(yintercept = 40, size = 1, alpha = 0.5)+
  geom_hline(yintercept = 32, linetype = "dashed", size = 1, alpha = 0.5)+
  scale_color_manual(values = cols, name = "Gender")+
   stat_summary(geom = "label", alpha = 0.5, mapping = aes(label = round(..y.., 1), group = Gender,vjust = -1), position = position_dodge(0.8))

date = Sys.Date()
#pdf(paste0('./IgX_Descriptive',date,'.pdf'),height=18,width=18)
ggarrange(plotlist = list(p1,p4, p7), common.legend = T, labels = c("A", "B", "C"), ncol=3)/ggarrange(plotlist = list(p2,p5, p8), common.legend = T, labels = c("D", "E", "F"), ncol=3)/ggarrange(plotlist = list(p3,p6, p9), common.legend = T, labels = c("G", "H", "I"), ncol=3)
#dev.off()
=======
# Libraries
library(ggpubr)
library(rstatix)
library(patchwork)
library(wesanderson)

# Data
data <- xlsx::read.xlsx("./SynData/Figures2_3_S1_Data.xlsx", sheetName = "TimeDiscretized")
data$DaysFromDays = factor(data$DaysFromDays, levels = c("0", "15", "30", "60", "90", "180", "270", "360"))

# General plot

## IgM(N)
test = dunn_test(data, IgM~DaysFromDays)%>%add_xy_position(x = "DaysFromDays")
test$y.position = 15 +  c(1:nrow(test))

means <- aggregate(IgM~DaysFromDays, data, mean)
means$IgM <- round(means$IgM,2)

p1<-ggboxplot(data, x = "DaysFromDays", y = "IgM", xlab = "Time from diangosis (days)", ylab ="IgM(N) (index units)", add = "mean")+
  stat_pvalue_manual(test, label = "p.adj.signif", hide.ns = T, tip.length = 0.01)+
  geom_hline(yintercept = 1.1, size = 1, alpha = 0.5, col = "darkred")+
  geom_hline(yintercept = 0.9, linetype = "dashed", size = 1, alpha = 0.5, col = "darkred")+
  geom_text(data = means, aes(label = IgM, y = IgM + 0.3*IQR(data$IgM, na.rm = T)))

## IgG(N)
test = dunn_test(data, IgG~DaysFromDays)%>%add_xy_position(x = "DaysFromDays")
test$y.position = 15 +  c(1:nrow(test))

means <- aggregate(IgG~DaysFromDays, data, mean)
means$IgG <- round(means$IgG,2)

p4<-ggboxplot(data, x = "DaysFromDays", y = "IgG", xlab = "Time from diangosis (days)", ylab ="IgG(N) (index units)", add = "mean")+
  stat_pvalue_manual(test, label = "p.adj.signif", hide.ns = T, tip.length = 0.01)+
  geom_hline(yintercept = 1.1, size = 1, alpha = 0.5, col = "darkred")+
  geom_hline(yintercept = 0.9, linetype = "dashed", size = 1, alpha = 0.5, col= "darkred")+
  geom_text(data = means, aes(label = IgG, y = IgG + 0.2*IQR(data$IgG, na.rm = T)))

## IgG(S)
test = dunn_test(data%>%filter(DaysFromDays!=360), IgS~DaysFromDays)#%>%add_xy_position(x = "DaysFromDays")
test$y.position = 1400 +  100*c(1:nrow(test))

means <- aggregate(IgS~DaysFromDays, data%>%filter(DaysFromDays!=360), mean)
means$IgS <- round(means$IgS,2)

p7<-ggboxplot(data%>%filter(DaysFromDays!=360), x = "DaysFromDays", y = "IgS", xlab = "Time from diagnosis (days)", ylab ="IgG(S) (IU/mL)", add = "mean")+
  stat_pvalue_manual(test, label = "p.adj.signif", hide.ns = T, tip.length = 0.01)+
  geom_hline(yintercept = 40, size = 1, alpha = 0.5, col = "darkred")+
  geom_hline(yintercept = 32, linetype = "dashed", size = 1, alpha = 0.5, col= "darkred")+
  geom_text(data = means, aes(label = IgS, y = IgS + 0.45*IQR(data$IgS, na.rm = T)))

# Clinical Spectrum
## IgM(N)
test = data%>%group_by(DaysFromDays)%>%dunn_test(IgM~ClinicalSpectrum)%>%
  filter(p.adj.signif != "ns")%>%
  add_xy_position(x = "DaysFromDays", step.increase = 0.03)

means <- data%>%group_by(DaysFromDays, ClinicalSpectrum)%>%summarize(IgM = mean(IgM))%>%as.data.frame()
means$IgM <- round(means$IgM,2)

p2<-ggboxplot(data, x = "DaysFromDays", y = "IgM", col = "ClinicalSpectrum", group = "ClinicalSpectrum",
              xlab = "Time from diangosis (days)", ylab ="IgM(N) (index units)", add = "mean")+
  stat_pvalue_manual(test, label = "p.adj.signif", hide.ns = T, tip.length = 0.01)+
  geom_hline(yintercept = 1.1, size = 1, alpha = 0.5)+
  geom_hline(yintercept = 0.9, linetype = "dashed", size = 1, alpha = 0.5)+
  scale_color_manual(values = c("#00AFBB", "#E7B800", "#FC4E07"), name = "Disease severity")+
  stat_summary(geom = "text", mapping = aes(label = round(..y.., 1), group = ClinicalSpectrum, vjust = -1.), position = position_dodge(0.8))

## IgG(N)
test = data%>%group_by(DaysFromDays)%>%dunn_test(IgG~ClinicalSpectrum)%>%
  filter(p.adj.signif != "ns")%>%
  add_xy_position(x = "DaysFromDays", step.increase = 0.03)

means <- aggregate(IgG~ClinicalSpectrum, data%>%group_by(DaysFromDays), mean)
means$IgG <- round(means$IgG,2)


p5<-ggboxplot(data, x = "DaysFromDays", y = "IgG", col = "ClinicalSpectrum",
              xlab = "Time from diangosis (days)", ylab ="IgG(N) (index units)", add = "mean")+
  stat_pvalue_manual(test, label = "p.adj.signif", hide.ns = T, tip.length = 0.01)+
  geom_hline(yintercept = 1.1, size = 1, alpha = 0.5)+
  geom_hline(yintercept = 0.9, linetype = "dashed", size = 1, alpha = 0.5)+
  scale_color_manual(values = c("#00AFBB", "#E7B800", "#FC4E07"), name = "Disease severity")+
  stat_summary(geom = "text", mapping = aes(label = round(..y.., 1), group = ClinicalSpectrum,vjust = -1), position = position_dodge(0.8))

## IgG(S)
test = data%>%filter(DaysFromDays!=360)%>%group_by(DaysFromDays)%>%dunn_test(IgS~ClinicalSpectrum)%>%
  filter(p.adj.signif != "ns")%>%
  add_xy_position(x = "DaysFromDays", step.increase = 0.03)
means <- aggregate(IgS~ClinicalSpectrum, data%>%filter(DaysFromDays!=360)%>%group_by(DaysFromDays), mean)
means$IgS <- round(means$IgS,2)


p8<-ggboxplot(data%>%filter(DaysFromDays!=360), x = "DaysFromDays", y = "IgS", col = "ClinicalSpectrum",
              xlab = "Time from diagnosis (days)", ylab ="IgG(S) (IU/mL)", add = "mean")+
  stat_pvalue_manual(test, label = "p.adj.signif", hide.ns = T, tip.length = 0.01)+
  geom_hline(yintercept = 40, size = 1, alpha = 0.5)+
  geom_hline(yintercept = 32, linetype = "dashed", size = 1, alpha = 0.5)+
  scale_color_manual(values = c("#00AFBB", "#E7B800", "#FC4E07"), name = "Disease severity")+
  stat_summary(geom = "text", mapping = aes(label = round(..y.., 1), group = ClinicalSpectrum,vjust = -1), position = position_dodge(0.8))

# Sex
cols = wes_palette("Rushmore1", n = 5)[c(5,4)]
## IgM(N)
test = data%>%group_by(DaysFromDays)%>%dunn_test(IgM~Sex)%>%
  add_xy_position(x = "DaysFromDays")
test$y.position = 15 +  c(1:nrow(test))

means <- aggregate(IgM~Sex, data%>%group_by(DaysFromDays), mean)
means$IgM <- round(means$IgM,2)

p3<-ggboxplot(data, x = "DaysFromDays", y = "IgM", col = "Sex",
              xlab = "Time from diangosis (days)", ylab ="IgM(N) (index units)", palette = cols, add = "mean")+
  stat_pvalue_manual(test, label = "p.adj.signif", hide.ns = T)+
  geom_hline(yintercept = 1.1, size = 1, alpha = 0.5)+
  geom_hline(yintercept = 0.9, linetype = "dashed", size = 1, alpha = 0.5)+
  scale_color_manual(values = cols, name = "Sex")+
  stat_summary(geom = "text", mapping = aes(label = round(..y.., 1), group = Sex,vjust = -1), position = position_dodge(0.8))

## IgG(N)
test = data%>%group_by(DaysFromDays)%>%dunn_test(IgG~Sex)%>%
  add_xy_position(x = "DaysFromDays", step.increase = 1)
test$y.position = 15 +  0.2*c(1:nrow(test))

means <- aggregate(IgG~Sex, data%>%group_by(DaysFromDays), mean)
means$IgG <- round(means$IgG,2)

p6 <-ggboxplot(data, x = "DaysFromDays", y = "IgG", col = "Sex",
               xlab = "Time from diangosis (days)", ylab ="IgG(N) (index units)", palette = cols, add = "mean")+
  stat_pvalue_manual(test, label = "p.adj.signif", hide.ns = T, tip.length = 0.01)+
  geom_hline(yintercept = 1.1, size = 1, alpha = 0.5)+
  geom_hline(yintercept = 0.9, linetype = "dashed", size = 1, alpha = 0.5)+
  scale_color_manual(values = cols, name = "Sex")+
  stat_summary(geom = "text", mapping = aes(label = round(..y.., 1), group = Sex,vjust = -1), position = position_dodge(0.8))


## IgG(S)
test = data%>%filter(DaysFromDays!=360)%>%group_by(DaysFromDays)%>%dunn_test(IgS~Sex)%>%
  add_xy_position(x = "DaysFromDays", step.increase = 0.3)

means <- aggregate(IgS~Sex, data%>%filter(DaysFromDays!=360)%>%group_by(DaysFromDays), mean)
means$IgS <- round(means$IgS,2)

p9 <-ggboxplot(data%>%filter(DaysFromDays!=360), x = "DaysFromDays", y = "IgS", col = "Sex",
               xlab = "Time from diagnosis (days)", ylab ="IgG(S) (IU/mL)", palette = cols, add = "mean")+
  stat_pvalue_manual(test, label = "p.adj.signif", hide.ns = T, tip.length = 0.01)+
  geom_hline(yintercept = 40, size = 1, alpha = 0.5)+
  geom_hline(yintercept = 32, linetype = "dashed", size = 1, alpha = 0.5)+
  scale_color_manual(values = cols, name = "Sex")+
  stat_summary(geom = "text", mapping = aes(label = round(..y.., 1), group = Sex,vjust = -1), position = position_dodge(0.8))

date = Sys.Date()
#pdf(paste0('./IgX_Descriptive',date,'.pdf'),height=18,width=18)
ggarrange(plotlist = list(p1,p4, p7), common.legend = T, labels = c("A", "B", "C"), ncol=3)/ggarrange(plotlist = list(p2,p5, p8), common.legend = T, labels = c("D", "E", "F"), ncol=3)/ggarrange(plotlist = list(p3,p6, p9), common.legend = T, labels = c("G", "H", "I"), ncol=3)
#dev.off()
