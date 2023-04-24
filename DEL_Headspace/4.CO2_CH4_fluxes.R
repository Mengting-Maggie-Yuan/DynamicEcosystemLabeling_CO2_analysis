# Calculate plot ecosystem respiration (Reco) and CH4 flux data
# University of California, Berkeley
# Maggie Mengting Yuan
# Night_Reco_L1toL4.txt can be partly generated based on Reco.R, which used L3 as an example. Here data from multiple labelings are combined.
# CH4_flux is from Jordan Hoff (weblink here).

rm(list=ls())

library(agricolae)
library(ggplot2)
library(gridExtra)

setwd("/Users/maggieyuan/Documents/GitHub/DynamicEcosystemLabeling_CO2_analysis/DEL_Headspace")

## plot overnight respiration
info = read.table("Data/plot_trt.txt", sep="\t", header=T)
reco = read.table("Data/Night_Reco_L1toL4.txt", header=T, sep="\t")
ch4 = read.table("Data/CH4_flux.txt", header=T, sep="\t")

L1234.reco = ggplot (reco, aes (x=Labeling, y=Reco.ugC.s.m2, fill=Water)) + 
  geom_boxplot (alpha = 1, outlier.shape = NA) + 
  geom_jitter (shape = 16, size =1, alpha = 0.6, position = position_jitterdodge(jitter.width = 0.01, dodge.width = 0.8)) +
  scale_fill_manual (values = c("#51b0f5", "#e6e34c"))+
  labs (x = "Labeling", y = "Night Reco (ugC/m2/s)") +
  theme(legend.position="none") 

## plot overnight respiration, separate dates, Figure S3
L1.reco = ggplot (reco[which(reco$Labeling=="L1"),], aes (x=Date, y=Reco.ugC.s.m2, fill=Water)) + 
  geom_boxplot (alpha = 1, outlier.shape = NA) + 
  geom_jitter (shape = 16, size =1, alpha = 0.6, position = position_jitterdodge(jitter.width = 0.01, dodge.width = 0.8)) +
  scale_fill_manual (values = c("#51b0f5", "#e6e34c"))+
  labs (x = "Date in 2018", y = "Night Reco (ugC/m2/s)") +
  ylim(0,80) +
  theme(legend.position="none") 

L2.reco = ggplot (reco[which(reco$Labeling=="L2"),], aes (x=Date, y=Reco.ugC.s.m2, fill=Water)) + 
  geom_boxplot (alpha = 1, outlier.shape = NA) + 
  geom_jitter (shape = 16, size =1, alpha = 0.6, position = position_jitterdodge(jitter.width = 0.01, dodge.width = 0.8)) +
  scale_fill_manual (values = c("#51b0f5", "#e6e34c"))+
  labs (x = "Date in 2018", y = element_blank()) +
  ylim(0,80)+
  theme(legend.position= "none") 

L3.reco = ggplot (reco[which(reco$Labeling=="L3"),], aes (x=Date, y=Reco.ugC.s.m2, fill=Water)) + 
  geom_boxplot (alpha = 1, outlier.shape = NA) + 
  geom_jitter (shape = 16, size =1, alpha = 0.6, position = position_jitterdodge(jitter.width = 0.01, dodge.width = 0.8)) +
  scale_fill_manual (values = c("#51b0f5", "#e6e34c"))+
  labs (x = "Date in 2019", y = element_blank()) +
  ylim(0,80)+
  theme(legend.position= "none") 

L4.reco = ggplot (reco[which(reco$Labeling=="L4"),], aes (x=Date, y=Reco.ugC.s.m2, fill=Water)) + 
  geom_boxplot (alpha = 1, outlier.shape = NA) + 
  geom_jitter (shape = 16, size =1, alpha = 0.6, position = position_jitterdodge(jitter.width = 0.01, dodge.width = 0.8)) +
  scale_fill_manual (values = c("#51b0f5", "#e6e34c"))+
  labs (x = "Date in 2020", y = element_blank()) +
  ylim(0,80)+
  theme(legend.position= "none") 

#pdf("night_Reco.pdf", width=20, height=6) 
grid.arrange(
  grobs = list(L1.reco, L2.reco, L3.reco, L4.reco),
  widths = c(5,1.8,12,7.5),
  layout_matrix = rbind(c(1,2,3,4))
)
#dev.off()

## plot CH4 flux, Figure S4
ch4$grp = paste(ch4$labelling_period, ch4$day_night, ch4$Water)
p.ch4 = ggplot(ch4, aes(x=grp, y=ch4_micrograms_per_second_per_m2, fill=Water)) + 
  geom_boxplot (alpha = 1, outlier.shape = NA) + 
  geom_jitter (shape = 16, size =1, alpha = 0.6, position = position_jitterdodge(jitter.width = 0.01, dodge.width = 0.8)) +
  scale_fill_manual (values = c("#51b0f5", "#e6e34c"))+
  labs (x = "Labeling-Day/night-Water", y = "CH4 flux (ug/m2/s)") +
  ylim(-0.0075,0.0075) +
  geom_hline(yintercept=0, color="black") +
  theme(axis.text.x=element_text(angle = 90))
p.ch4 # removed 7 data points

#pdf("CH4_flux.pdf", width=10, height=6)
p.ch4
#dev.off()

## ANOVA for overnight respiration
reco_L = reco[which(reco$Labeling=="L1"),] # or L3, L4
reco_aov_rep = aov(Reco.ugC.s.m2 ~ Water + Error(Date), data = reco_L)
summary(reco_aov_rep)
reco_aov = aov(Reco.ugC.s.m2 ~ Water*Date, data = reco_L)
resp_aov_lsd = LSD.test(reco_aov, "Water", p.adj="holm")

# for L2
reco_L = reco[which(reco$Labeling=="L2"),]
reco_aov = aov(Reco.ugC.s.m2 ~ Water, data = reco_L)
summary(reco_aov)
resp_aov_lsd = LSD.test(reco_aov, "Water", p.adj="holm")

