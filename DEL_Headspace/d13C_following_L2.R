# Plot d13C in ecosystem respiration following April 2018 labeling
# University of California, Berkeley
# Maggie Mengting Yuan

rm(list=ls())
library(ggplot2)

setwd("/Users/maggieyuan/Documents/GitHub/DynamicEcosystemLabeling_CO2_analysis/DEL_Headspace")

flux_4_27 = read.table("Data/gas_flux/gas_flux_4_27_2018.txt", header=T, sep="\t")
flux_4_30 = read.table("Data/gas_flux/gas_flux_4_30_2018.txt", header=T, sep="\t")
flux_5_09 = read.table("Data/gas_flux/gas_flux_5_9_2018.txt", header=T, sep="\t")
flux_5_22 = read.table("Data/gas_flux/gas_flux_5_22_2018.txt", header=T, sep="\t")
flux_6_13 = read.table("Data/gas_flux/gas_flux_6_13_2018.txt", header=T, sep="\t")
flux_8_27 = read.table("Data/gas_flux/gas_flux_8_27_2018.txt", header=T, sep="\t")

p4.27 <- ggplot(flux_4_27, aes(EPOCH_TIME)) + 
  geom_point(aes(y=Delta_Raw_iCO2), size=0.1) + 
  ylab(expression(paste(delta, CO[2], " (\u2030)"))) + 
  scale_x_continuous(breaks=seq(from=1524852000, by=3600, length.out=5), labels=c("4/27/18\n-11AM", "12AM", "1PM", "2PM", "3PM")) +
  ylim(-50,1050) +
  theme(axis.title.x = element_blank())

p4.30 <- ggplot(flux_4_30, aes(EPOCH_TIME)) + 
  geom_point(aes(y=Delta_Raw_iCO2), size=0.1) + 
  ylab(expression(paste(delta, CO[2], " (\u2030)"))) + 
  scale_x_continuous(breaks=seq(from=1525111200, by=3600, length.out=5), labels=c("4/30/18\n-11AM", "12AM", "1PM", "2PM", "3PM")) +
  ylim(-50,1050) +
  theme(axis.title.x = element_blank())

p5.09 <- ggplot(flux_5_09, aes(EPOCH_TIME)) + 
  geom_point(aes(y=Delta_Raw_iCO2), size=0.1) + 
  ylab(expression(paste(delta, CO[2], " (\u2030)"))) + 
  scale_x_continuous(breaks=seq(from=1525888800+2*3600, by=3600, length.out=3), labels=c("5/9/18\n-1PM", "2PM", "3PM")) +
  ylim(-50,1050) +
  theme(axis.title.x = element_blank())

p5.22 <- ggplot(flux_5_22, aes(EPOCH_TIME)) + 
  geom_point(aes(y=Delta_Raw_iCO2), size=0.1) + 
  ylab(expression(paste(delta, CO[2], " (\u2030)"))) + 
  xlab("Time") +
  scale_x_continuous(breaks=seq(from=1527012000, by=3600, length.out=2), labels=c("5/22/18\n-11AM", "12AM")) +
  ylim(-50,1050)

#pdf("/Users/maggieyuan/Documents/!method/gas_flux_delta.pdf", width=8, height=8)
grid.arrange(
  grobs = list(p4.27, p4.30, p5.09, p5.22),
  heights = c(1,1,1,1),
  layout_matrix = rbind(c(1),
                        c(2),
                        c(3),
                        c(4))
)
#dev.off()


