# Figures for soil GWC
# University of California, Berkeley
# Maggie Mengting Yuan

rm(list=ls())
library(ggplot2)
library(agricolae)

setwd("/Users/maggieyuan/Documents/GitHub/DynamicEcosystemLabeling_CO2_analysis/soil_water")

harvest_names <- function(original_names){
  new_names = original_names
  new_names = gsub("L1", "Feb18-", new_names)
  new_names = gsub("L2", "Apr18-", new_names)
  new_names = gsub("L3", "Mar19-", new_names)
  new_names = gsub("L4", "Mar20-", new_names)
  new_names = gsub("T0", "1day", new_names)
  new_names = gsub("T1", "3days", new_names)
  new_names = gsub("T2", "3weeks", new_names)
  return(new_names)
}
harvest.names.ord = c("Feb18-1day", "Feb18-3days", "Feb18-3weeks", "Apr18-1day", "Apr18-3days", "Apr18-3weeks", "Mar19-1day", "Mar19-3days", "Mar19-3weeks", "Mar20-1day", "Mar20-3days", "Mar20-3weeks")

### GWC
gwc = read.table("GWC_long.txt", sep="\t", header=T, row.names=1)
gwc$Timepoint = paste(gwc$Labeling, gwc$Harvest, sep="")
gwc$Timepoint1 = harvest_names(gwc$Timepoint)
#aggregate(gwc$GWC, by=list(gwc$Timepoint, gwc$Water), FUN=mean, na.rm=T)
#aggregate(gwc$GWC, by=list(gwc$Timepoint), FUN=mean, na.rm=T)

# plot gwc
gwc$Timepoint1 <- factor(gwc$Timepoint1, levels = harvest.names.ord)
p.gwc = ggplot(gwc, aes(x=Timepoint1, y=GWC, fill=Water)) + 
  geom_boxplot (alpha = 1, outlier.shape = NA) + 
  geom_jitter (shape = 16, size =1, alpha = 0.6, position = position_jitterdodge(jitter.width = 0.01, dodge.width = 0.8)) +
  scale_fill_manual (values = c("#51b0f5", "#e6e34c"))+
  labs (x = "Harvest", y = "Gravimetric water content (GWC)") +
  ylim(0,0.4) +
  scale_x_discrete(breaks=harvest.names.ord,
                   labels=gsub("-", "\n-",harvest.names.ord))

#pdf("gwc.pdf", width=7, height=4)
p.gwc
#dev.off()

# ANOVA test
# all data points
gwc_aov = aov(GWC ~ Water + Error(Timepoint), data = gwc)
summary(gwc_aov)
gwc_aov.lsd = aov(GWC ~ Water*Timepoint, data = gwc)
summary(gwc_aov.lsd)
gwc_aov_lsd = LSD.test(gwc_aov.lsd, "Water", p.adj="holm")
gwc_aov_lsd

# separate harvests
hvst = unique(gwc$Timepoint)
gwc.out = matrix(NA, nrow=length(hvst), ncol=8)
for (i in 1:length(hvst)){
  dat = gwc[which(gwc$Timepoint==hvst[i]),]
  dat.aov = aov(GWC ~ Water, data = dat)
  dat.lsd = LSD.test(dat.aov, "Water", p.adj="holm")
  gwc.out[i,] <- as.numeric(c(summary(dat.aov)[[1]]$F[1],summary(dat.aov)[[1]]$'Pr(>F)'[1],
                              dat.lsd$means[1,c(1:3)], dat.lsd$means[2,c(1:3)]))
}
row.names(gwc.out)=hvst
