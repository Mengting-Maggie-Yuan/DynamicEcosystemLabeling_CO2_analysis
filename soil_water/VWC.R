# Figures for soil VWC
# University of California, Berkeley
# Maggie Mengting Yuan

rm(list=ls())
library(ggplot2)
library(tidyverse)
library(gridExtra)

setwd("/Users/maggieyuan/Documents/GitHub/DynamicEcosystemLabeling_CO2_analysis/soil_water")

## VWC
vwc = read.table("Data/VWC_long_20180202_to_20200428.txt", sep="\t", header=T, row.names=1) %>% # read VWC table
  filter(!is.na(VWC_EC5)) %>% # remove missing rows
  group_by(EpochTime, Plot, Water) %>% summarise(VWC_EC5=mean(VWC_EC5)) %>% # get one value per plot per timestamp by taking means of multiple probes installed in some plot - this is to keep the inter-plot variation equally contributed by the biological plots.
  mutate(Date=as.Date(as.POSIXct(EpochTime, origin="1970-01-01"), tz = 'PST8PDT')) # add date

info = read.table("Data/plot_trt.txt", header=T) # read plot treatment info
shelter = read.table("Data/shelter.txt", header=T, sep="\t") # read shelter on and off time
precip = read.table("Data/precip_20171128-20210610.csv", header=T, sep=";") %>% # read daily precipitation
  mutate(Date = as.Date(Date, "%m/%d/%Y")) # match date format with vwc
watering = read.table("Data/watering.txt", header=T, sep="\t") %>% # read manual watering amount
  mutate(Date = as.Date(Date, "%m/%d/%Y")) # match date format with vwc

# calculate daily average - 2 hour data too messy on plot
vwc.daily = vwc %>%
  group_by(Date, Plot) %>% # for each plot and date
  select(VWC_EC5) %>% # for VWC and water potential
  summarise(VWC_EC5=mean(VWC_EC5, na.rm=T)) %>% # get average
  mutate(EpochTime=as.numeric(as.POSIXlt(Date, format = "%Y-%m-%d"))) %>% # add EpochTime
  left_join(., info) # add treatment info

vwc.daily.mean = vwc.daily %>%
  group_by(EpochTime, Date, Water) %>% # for each timepoint and treatment level
  select(VWC_EC5) %>% # for VWC and water potential
  summarise(VWC_EC5_mean=mean(VWC_EC5, na.rm=T)) # get average
  
vwc.daily.sd = vwc.daily %>%
  group_by(EpochTime, Date, Water) %>% # for each timepoint and treatment level
  select(VWC_EC5) %>% # for VWC and water potential
  summarise(VWC_EC5_sd=sd(VWC_EC5, na.rm=T)) # get sd
  
vwc.daily.by.trt = left_join(vwc.daily.mean, vwc.daily.sd)

## add shelter info
shelter = shelter %>% mutate(
    epc.on=as.numeric(as.POSIXlt(as.character(Timestamp.on), format="%m/%d/%Y %I:%M:%S %p", tz="PST8PDT", origin="1970-01-01")),
    epc.off=as.numeric(as.POSIXlt(as.character(Timestamp.off), format="%m/%d/%Y %I:%M:%S %p", tz="PST8PDT", origin="1970-01-01"))
    )

shelter_full = shelter %>% filter(Water=="full") # on and off timestamps for full water plots
shelter_half = shelter %>% filter(Water=="half") # on and off timestamps for half water plots

epc.on.full = c()
for (i in 1:nrow(shelter_full)){
  epc.on.full = c(epc.on.full, seq(from=shelter_full$epc.on[i], to=shelter_full$epc.off[i], by=900))
} # all the possible epoc timestamps that the shelter is on (not able to received rain) for full water plots
epc.on.half = c()
for (i in 1:nrow(shelter_half)){
  epc.on.half = c(epc.on.half, seq(from=shelter_half$epc.on[i], to=shelter_half$epc.off[i], by=900))
} # all the possible epoc timestamps that the shelter is on (not able to received rain) for half water plots

vwc.daily.by.trt$shelter = rep(1, nrow(vwc.daily.by.trt)) # assume no shelter
for (i in 1:nrow(vwc.daily.by.trt)){
  if (vwc.daily.by.trt$Water[i]=="full"){
    if (vwc.daily.by.trt$EpochTime[i] %in% epc.on.full){
      vwc.daily.by.trt$shelter[i]<-0
    }
  }
  if (vwc.daily.by.trt$Water[i]=="half"){
    if (vwc.daily.by.trt$EpochTime[i] %in% epc.on.half){
      vwc.daily.by.trt$shelter[i]<-0
    }
  }
}

## add water
vwc.daily.by.trt.water = left_join(vwc.daily.by.trt, precip) %>% select(!Time) %>% # add precipitation amount, in inches 
  left_join(., watering) %>% select(!Water_gallon) %>% # add manual water amount, in inches
  mutate(Water_inch = ifelse(is.na(Water_inch), 0, Water_inch)) # replace NA with 0
  
## add actual top-down water received by plots
vwc.topdown = vwc.daily.by.trt.water %>%
  mutate(topdown = Precip*shelter + Water_inch)

## Generate figures
vwc.topdown.plot = vwc.topdown %>% mutate(
  shelter.F = ifelse(Water=="full" & shelter==0, 0.58, -1),
  shelter.H = ifelse(Water=="half" & shelter==0, 0.56, -1),
  topdown.F = ifelse(Water=="full" & topdown>0, topdown, -1),
  topdown.H = ifelse(Water=="half" & topdown>0, topdown, -1)
)

# separate seasons
vwc.18 = vwc.topdown.plot %>% filter(EpochTime<1546358400)# 2018/2 - 2018/7
vwc.19 = vwc.topdown.plot %>% filter(EpochTime>1546358400 & EpochTime<1564671600) # 2019/2 to 2019/6
vwc.20 = vwc.topdown.plot %>% filter(EpochTime>1564671600)# # 2019/11 to 2020/4

# axis epoch time and label
s18_epoch = c(1519891200,1522566000,1525158000,1527836400,1530428400)
s18_text = c("3/1/18", "4/1", "5/1", "6/1", "7/1")

s19_epoch = c(1551427200,1554102000,1556694000,1559372400)
s19_text = c("3/1/19", "4/1", "5/1", "6/1")

s20_epoch = c(1575187200,1577865600,1580544000,1583049600,1585724400)
s20_text = c("12/1/19", "1/1/20", "2/1", "3/1", "4/1")

figure.vwc.18 <- ggplot(vwc.18, aes(x=EpochTime, y=VWC_EC5_mean, fill=Water)) + 
  geom_ribbon(aes(ymin=VWC_EC5_mean-VWC_EC5_sd, ymax=VWC_EC5_mean+VWC_EC5_sd), alpha=0.5) +
  geom_line(aes(color=Water)) + 
  geom_point(aes(color=Water)) + 
  geom_point(aes(x=EpochTime, y=shelter.F), color="#51b0f5", shape=15) + # shelter full
  geom_point(aes(x=EpochTime, y=shelter.H), color="#8c8a2b", shape=15) + # shelter half
  geom_point(aes(x=EpochTime, y=topdown.F), color="#5d5a59", shape=17) + # precip full
  geom_point(aes(x=EpochTime, y=topdown.H), color="black", shape=2) + # precip half
  scale_x_continuous(breaks=s18_epoch, labels=s18_text) +
  scale_y_continuous(
    name = "Volumetric water content", limits=c(0,0.6),
    sec.axis = sec_axis(~.*0.5, name="Water received by plots (in)")
  ) +
  scale_fill_manual(values = c("#51b0f5", "#e6e34c"))+
  scale_color_manual(values = c("#51b0f5", "#8c8a2b"))+ # had to change yellow color to darker
  xlab("Date in 2018") +
  theme(legend.position= "none") 

figure.vwc.19 <- ggplot(vwc.19, aes(x=EpochTime, y=VWC_EC5_mean, fill=Water)) + 
  geom_ribbon(aes(ymin=VWC_EC5_mean-VWC_EC5_sd, ymax=VWC_EC5_mean+VWC_EC5_sd), alpha=0.5) +
  geom_line(aes(color=Water)) + 
  geom_point(aes(color=Water)) + 
  geom_point(aes(x=EpochTime, y=shelter.F), color="#51b0f5", shape=15) + # shelter full
  geom_point(aes(x=EpochTime, y=shelter.H), color="#8c8a2b", shape=15) + # shelter half
  geom_point(aes(x=EpochTime, y=topdown.F), color="#5d5a59", shape=17) + # precip full
  geom_point(aes(x=EpochTime, y=topdown.H), color="black", shape=2) + # precip half
  scale_x_continuous(breaks=s19_epoch, labels=s19_text) +
  scale_y_continuous(
    name = "Volumetric water content", limits=c(0,0.6),
    sec.axis = sec_axis(~.*0.5, name="Water received by plots (in)")
  ) +
  scale_fill_manual(values = c("#51b0f5", "#e6e34c"))+
  scale_color_manual(values = c("#51b0f5", "#8c8a2b"))+ # had to change yellow color to darker
  xlab("Date in 2019") +
  theme(legend.position= "none") 

figure.vwc.20 <- ggplot(vwc.20, aes(x=EpochTime, y=VWC_EC5_mean, fill=Water)) + 
  geom_ribbon(aes(ymin=VWC_EC5_mean-VWC_EC5_sd, ymax=VWC_EC5_mean+VWC_EC5_sd), alpha=0.5) +
  geom_line(aes(color=Water)) + 
  geom_point(aes(color=Water)) + 
  geom_point(aes(x=EpochTime, y=shelter.F), color="#51b0f5", shape=15) + # shelter full
  geom_point(aes(x=EpochTime, y=shelter.H), color="#8c8a2b", shape=15) + # shelter half
  geom_point(aes(x=EpochTime, y=topdown.F), color="#5d5a59", shape=17) + # precip full
  geom_point(aes(x=EpochTime, y=topdown.H), color="black", shape=2) + # precip half
  scale_x_continuous(breaks=s20_epoch, labels=s20_text) +
  scale_y_continuous(
    name = "Volumetric water content", limits=c(0,0.6),
    sec.axis = sec_axis(~.*0.5, name="Water received by plots (in)")
  ) +
  scale_fill_manual(values = c("#51b0f5", "#e6e34c"))+
  scale_color_manual(values = c("#51b0f5", "#8c8a2b"))+ # had to change yellow color to darker
  xlab("Date in 2020") +
  theme(legend.position= "none") 

#pdf("vwc.shelter.precip.pdf", width=20, height=5) 
grid.arrange(
  grobs = list(figure.vwc.18, figure.vwc.19, figure.vwc.20),
  widths = c(5,4.5,5),
  layout_matrix = rbind(c(1,2,3))
)
#dev.off()















