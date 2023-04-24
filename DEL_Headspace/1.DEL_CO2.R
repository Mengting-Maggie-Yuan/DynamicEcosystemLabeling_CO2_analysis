# Headspace CO2 data clean up for Dynamic Ecosystem Labeling (DEL) System output
# University of California, Berkeley
# Maggie Mengting Yuan
# Input files for this code is from CR1000 datalogger program 'Control Program for Dynamic Ecosystem Labeling (DEL) System (https://github.com/rinaest/DEL_system/blob/main/DEL_system_code.CR1).

rm(list=ls())
library(tidyverse)

## define functions used

# match weather station temperature by the closest time stamp 
match_epoch_temp = function(epoch, temperature_table){
  epoch_diff = min(abs(epoch - temperature_table$EpochTime), na.rm=T)
  epoch_temp = temperature_table$Temp_C[which(abs(epoch - temperature_table$EpochTime) == epoch_diff)[1]]
  return(c(epoch_temp, epoch_diff))
}

## read datalogger output and clean it to reflect correct CO2 data from 12C and 13C chambers
setwd("/Users/maggieyuan/Documents/GitHub/DynamicEcosystemLabeling_CO2_analysis/DEL_Headspace")

L3 = read.table("Data/CR1000_L3.dat", sep=",", skip=1, header=T) %>% .[-c(1:2),] # read data and remove units
L3 = L3 %>% mutate(EpochTime = as.numeric(as.POSIXlt(TIMESTAMP, format="%Y-%m-%d %H:%M:%S")), # add a column of Epoch time
         Plot = gsub("1C", "", Valve1Pos) %>% gsub(" ", "", .), # add a column of field plot number
         Irga_CO2 = rep(0,nrow(L3)), # add a column for 12CO2 data
         Picarro_CO2 = rep(0,nrow(L3)) # add a column for 13CO2 data
         ) %>% as_tibble

for (i in 1:(nrow(L3)-1)){
  Plot.id = L3$Plot[i] %>% gsub("P","",.) %>% as.numeric() # get plot ID to identify columns
  L3$Irga_CO2[i] = L3 %>% dplyr::slice(i) %>% select(starts_with("IRGA_1_CO2.") & contains(paste(".",Plot.id,".",sep=""))) # IRGA data for 12C chambers are from the same row
  L3$Picarro_CO2[i] = L3 %>% dplyr::slice(i+1) %>% select(starts_with("Picarro_CO2.") & contains(paste(".",Plot.id,".",sep=""))) # Picarro data for 13C chambers are from one row below 
  if (i%%500==0){print(paste("i =",i,"in",nrow(L3)-1))}
}
L3 = L3 %>% mutate(Picarro_Atm_Pct = lead(Atom_Pct_CO2)) # Picarro data - use one row below

## Add weather station temperature data
## Note: each chamber has a temperature probe but the data came out messy with errors and missing values. Thus use weather station temperature data as approxi for chamber temperatures. During L3, the maximum chamber temperature (36.2C) was 14.2C higher than weather station (22C). But nighttime temperatures from inside and outside of chambers lined up well with a slight different (minimum temperature was 1.5C inside vs 0.3C outside).
ws_temp = read.table("Data/AirTemp.csv", sep=";", header=T) %>% # read weather station temperature
  mutate(EpochTime = as.numeric(as.POSIXct(paste(Date, Time), format="%m/%d/%Y %I:%M:%S %p", tz="PST8PDT", origin="1970-01-01")), # add a column of Epoch time
         Temp_C = (as.numeric(as.character(Temp))-32)/1.8 # add a column of temperature in Celsius
         ) # Discard the warning message "NAs introduced by coercion" due to missing records.

# Stop here and manually check and correct the time stamps around daylight saving time start/end. R conversion from human to epoch time, or vise versa, may be off for these time stamps. Does not apply to this labeling.

temp_match = lapply(L3$EpochTime, FUN=match_epoch_temp, temperature_table=ws_temp) %>% unlist %>% split(.,1:2) # get matching temperature and gap between the time stamps (in seconds) for CO2 and temperature data
max(temp_match[[2]]) # the gap between time stamps should be <2hrs (7200 seconds) - interval of temperature records
L3 = L3 %>% mutate(ws_Temp_C = temp_match[[1]]) # add the weather station temperature to CO2 table

## Analyze head space CO2 data
L3r = L3 %>% 
  select(TIMESTAMP, EpochTime, Plot, Irga_CO2, Picarro_CO2, Picarro_Atm_Pct, ws_Temp_C, PAR_In) %>% # pick columns needed
  filter(Irga_CO2!=0, !is.na(Irga_CO2), Picarro_CO2!=0, !is.na(Picarro_CO2)) %>% # remove missing values
  mutate(Irga_CO2=as.numeric(unlist(Irga_CO2)), 
         Picarro_CO2=as.numeric(unlist(Picarro_CO2)), 
         Picarro_Atm_Pct=as.numeric(Picarro_Atm_Pct), 
         PAR_In=as.numeric(PAR_In)) %>% # correct format from list/character to double
  mutate(Date = as.Date(TIMESTAMP, origin="1970-01-01")) # Add a column of date

write.table(L3r, "Data/CR1000_L3_clean.dat")

