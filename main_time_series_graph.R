### Time Series of CPUE Measures
rm(list=ls())

setwd("/Users/lynham/Documents/CSF MPA Project/")
library(ggplot2)
library(readxl)
library(data.table)
library(wesanderson)

NOAA.raw <- read.csv(file="/Users/lynham/Documents/CSF MPA Project/cleaned_observer_data_090618.csv", header=TRUE, sep=",")

setnames(NOAA.raw, "set_year", "SET_YEAR")
setnames(NOAA.raw, "set_month", "SET_MONTH")
setnames(NOAA.raw, "set_day", "SET_DAY")
setnames(NOAA.raw, "hawaii", "HAWAII")

NOAA.raw$PRI_dummy <- as.numeric((NOAA.raw$SET_YEAR >= 2014) & (NOAA.raw$SET_MONTH >= 9) & (NOAA.raw$SET_DAY >= 25)| (NOAA.raw$SET_YEAR >= 2014) & (NOAA.raw$SET_MONTH >= 10) | (NOAA.raw$SET_YEAR >= 2015))
NOAA.raw$PMNM_dummy <- as.numeric((NOAA.raw$SET_YEAR >= 2016) & (NOAA.raw$SET_MONTH >= 8) & (NOAA.raw$SET_DAY >= 26)| (NOAA.raw$SET_YEAR >= 2016) & (NOAA.raw$SET_MONTH >= 9) | (NOAA.raw$SET_YEAR >= 2017))
NOAA.raw$tuna <- NOAA.raw$count_bigeye_tuna + NOAA.raw$count_yellowfin_tuna
NOAA.raw$all_tuna <- NOAA.raw$count_bigeye_tuna + NOAA.raw$count_yellowfin_tuna + NOAA.raw$count_albacore_tuna + NOAA.raw$count_skipjack_tuna + NOAA.raw$count_bluefin_tuna + NOAA.raw$count_unid_tuna
NOAA.raw$tuna.cpue <- (NOAA.raw$tuna/NOAA.raw$num_hks_set)*1000
NOAA.raw$all_tuna.cpue <- (NOAA.raw$all_tuna/NOAA.raw$num_hks_set)*1000
NOAA.raw$sword.cpue <- (NOAA.raw$count_swordfish/NOAA.raw$num_hks_set)*1000

month.catch.mean <-aggregate(tuna~SET_YEAR + SET_MONTH + HAWAII + sword + nino3 + lag1nino3 + lag2nino3 + lag3nino3,FUN=mean,data=NOAA.raw)
month.catch.mean$PRI_dummy <- as.numeric((month.catch.mean$SET_YEAR >= 2014) & (month.catch.mean$SET_MONTH >= 10) | (month.catch.mean$SET_YEAR >= 2015))
month.catch.mean$PMNM_dummy <- as.numeric((month.catch.mean$SET_YEAR >= 2016) & (month.catch.mean$SET_MONTH >= 9) | (month.catch.mean$SET_YEAR >= 2017))


month.cpue.mean <-aggregate(tuna.cpue~SET_YEAR + SET_MONTH + HAWAII + sword,FUN=mean,data=NOAA.raw)

month.cpue.mean$date <- as.Date(paste(15,month.cpue.mean$SET_MONTH,month.cpue.mean$SET_YEAR),"%d%m%Y")
hawaii.tuna.cpuemean <- subset(month.cpue.mean, HAWAII==1 & SET_YEAR>=2010 & sword==0)
hawaii.sword.cpuemean <- subset(month.cpue.mean, HAWAII==1 & SET_YEAR>=2010 & sword==1)
samoa.tuna.cpuemean <- subset(month.cpue.mean, HAWAII==0 & SET_YEAR>=2010 & sword==0)

month.catch.mean$date <- as.Date(paste(15,month.catch.mean$SET_MONTH,month.catch.mean$SET_YEAR),"%d%m%Y")
hawaii.tuna.catchmean <- subset(month.catch.mean, HAWAII==1 & SET_YEAR>=2010 & sword==0)
hawaii.sword.catchmean <- subset(month.catch.mean, HAWAII==1 & SET_YEAR>=2010 & sword==1)
samoa.tuna.catchmean <- subset(month.catch.mean, HAWAII==0 & SET_YEAR>=2010 & sword==0)

ggplot(data=hawaii.tuna.catchmean, aes(x=date, y=tuna)) +
  geom_line() +
  geom_vline(xintercept = as.numeric(as.Date("2014-09-25")), linetype=4) +
  geom_vline(xintercept = as.numeric(as.Date("2016-08-26")), linetype=4) + labs(x = "Date",y="Bigeye and Yellowfin Tuna Caught per Set")

ggplot(data=hawaii.tuna.cpuemean, aes(x=date, y=tuna.cpue)) +
  geom_line() +
  geom_vline(xintercept = as.numeric(as.Date("2014-09-25")), linetype=4) +
  geom_vline(xintercept = as.numeric(as.Date("2016-08-26")), linetype=4) + labs(x = "Date",y="Bigeye and Yellowfin Tuna Caught per 1,000 Hooks")

NOAAdata <- read.csv(file="/Users/lynham/Documents/CSF MPA Project/collapsed_distanceTRIPID.csv", header=TRUE, sep=",")

setnames(NOAAdata, "set_year", "SET_YEAR")
setnames(NOAAdata, "set_month", "SET_MONTH")
setnames(NOAAdata, "set_day", "SET_DAY")
setnames(NOAAdata, "hawaii", "HAWAII")

NOAAdata$PRI_dummy <- as.numeric((NOAAdata$SET_YEAR >= 2014) & (NOAAdata$SET_MONTH >= 9) & (NOAAdata$SET_DAY >= 25)| (NOAAdata$SET_YEAR >= 2014) & (NOAAdata$SET_MONTH >= 10) | (NOAAdata$SET_YEAR >= 2015))
NOAAdata$PMNM_dummy <- as.numeric((NOAAdata$SET_YEAR >= 2016) & (NOAAdata$SET_MONTH >= 8) & (NOAAdata$SET_DAY >= 26)| (NOAAdata$SET_YEAR >= 2016) & (NOAAdata$SET_MONTH >= 9) | (NOAAdata$SET_YEAR >= 2017))

NOAAdata$dec.2017 <- as.numeric((NOAAdata$SET_YEAR == 2017) & (NOAAdata$SET_MONTH == 12) )

trips.data <- NOAAdata

trips.data$tuna <- trips.data$count_bigeye_tuna + trips.data$count_yellowfin_tuna

trips.data$catch_km <- trips.data$tuna/trips.data$distance_km

### Calculate catch per trip per month

month.catch.trip <-aggregate(cbind(tuna, catch_km)~SET_YEAR + SET_MONTH + HAWAII + sword,FUN=mean,data=trips.data)
month.catch.trip$PRI_dummy <- as.numeric((month.catch.trip$SET_YEAR >= 2014) & (month.catch.trip$SET_MONTH >= 10) | (month.catch.trip$SET_YEAR >= 2015))
month.catch.trip$PMNM_dummy <- as.numeric((month.catch.trip$SET_YEAR >= 2016) & (month.catch.trip$SET_MONTH >= 9) | (month.catch.trip$SET_YEAR >= 2017))

month.catch.trip$date <- as.Date(paste(15,month.catch.trip$SET_MONTH,month.catch.trip$SET_YEAR),"%d%m%Y")
hawaii.tuna.month.catch.trip <- subset(month.catch.trip, HAWAII==1 & SET_YEAR>=2010 & sword==0)
hawaii.sword.month.catch.trip <- subset(month.catch.trip, HAWAII==1 & SET_YEAR>=2010 & sword==1)
samoa.tuna.month.catch.trip <- subset(month.catch.trip, HAWAII==0 & SET_YEAR>=2010 & sword==0)

ggplot(data=hawaii.tuna.month.catch.trip, aes(x=date, y=tuna)) +
  geom_line() +
  geom_vline(xintercept = as.numeric(as.Date("2014-09-25")), linetype=4) +
  geom_vline(xintercept = as.numeric(as.Date("2016-08-26")), linetype=4) + labs(x = "Date",y="Catch per Trip")

### Calculate catch per km traveled

ggplot(data=hawaii.tuna.month.catch.trip, aes(x=date, y=catch_km)) +
  geom_line() +
  geom_vline(xintercept = as.numeric(as.Date("2014-09-25")), linetype=4) +
  geom_vline(xintercept = as.numeric(as.Date("2016-08-26")), linetype=4) + labs(x = "Date",y="Catch per Kilometer Traveled")

### American Samoa CPUE hooks

ggplot(data=samoa.tuna.cpuemean, aes(x=date, y=tuna.cpue)) +
  geom_line() +
  geom_vline(xintercept = as.numeric(as.Date("2014-09-25")), linetype=4) +
  geom_vline(xintercept = as.numeric(as.Date("2016-08-26")), linetype=4) + labs(x = "Date",y="Bigeye and Yellowfin Tuna Caught per 1,000 Hooks")

# Building the nice dataset that will graph all the lines together

combo1 <- merge(hawaii.tuna.month.catch.trip,hawaii.tuna.catchmean,by=c("date"),all.x = TRUE)
combo2 <- merge(combo1,hawaii.tuna.cpuemean,by=c("date"),all.x = TRUE)

setnames(combo2, "tuna.x", "catch_trip")
setnames(combo2, "tuna.y", "catch_set")
setnames(combo2, "tuna.cpue", "catch_hooks")

clean <- subset(combo2, select = c(date,catch_trip,catch_set,catch_hooks,catch_km))

pre_reserve <- subset(clean,date<"2014-09-25")

catch_trip_mu <- mean(pre_reserve$catch_trip)
catch_trip_sigma <- sd(pre_reserve$catch_trip)
clean$catch_tripN <- (clean$catch_trip-catch_trip_mu)/catch_trip_sigma
catch_set_mu <- mean(pre_reserve$catch_set)
catch_set_sigma <- sd(pre_reserve$catch_set)
clean$catch_setN <- (clean$catch_set-catch_set_mu)/catch_set_sigma
catch_hooks_mu <- mean(pre_reserve$catch_hooks)
catch_hooks_sigma <- sd(pre_reserve$catch_hooks)
clean$catch_hooksN <- (clean$catch_hooks-catch_hooks_mu)/catch_hooks_sigma
catch_km_mu <- mean(pre_reserve$catch_km)
catch_km_sigma <- sd(pre_reserve$catch_km)
clean$catch_kmN <- (clean$catch_km-catch_km_mu)/catch_km_sigma

clean$mean_all <- rowMeans(cbind(clean$catch_tripN,clean$catch_setN,clean$catch_hooksN,clean$catch_kmN),na.rm = TRUE, dims = 1)

## Getting the standardized values for Swordfish

sword.combo1 <- merge(hawaii.sword.month.catch.trip,hawaii.sword.catchmean,by=c("date"),all.x = TRUE)
sword.combo2 <- merge(sword.combo1,hawaii.sword.cpuemean,by=c("date"),all.x = TRUE)

setnames(sword.combo2, "tuna.x", "catch_trip")
setnames(sword.combo2, "tuna.y", "catch_set")
setnames(sword.combo2, "tuna.cpue", "catch_hooks")

sword.clean <- subset(sword.combo2, select = c(date,catch_trip,catch_set,catch_hooks,catch_km))

sword.pre_reserve <- subset(sword.clean,date<"2014-09-25")

sword.catch_trip_mu <- mean(sword.pre_reserve$catch_trip)
sword.catch_trip_sigma <- sd(sword.pre_reserve$catch_trip)
sword.clean$catch_tripN <- (sword.clean$catch_trip-sword.catch_trip_mu)/sword.catch_trip_sigma
sword.catch_set_mu <- mean(sword.pre_reserve$catch_set)
sword.catch_set_sigma <- sd(sword.pre_reserve$catch_set)
sword.clean$catch_setN <- (sword.clean$catch_set-sword.catch_set_mu)/sword.catch_set_sigma
sword.catch_hooks_mu <- mean(sword.pre_reserve$catch_hooks)
sword.catch_hooks_sigma <- sd(sword.pre_reserve$catch_hooks)
sword.clean$catch_hooksN <- (sword.clean$catch_hooks-sword.catch_hooks_mu)/sword.catch_hooks_sigma
sword.catch_km_mu <- mean(sword.pre_reserve$catch_km)
sword.catch_km_sigma <- sd(sword.pre_reserve$catch_km)
sword.clean$catch_kmN <- (sword.clean$catch_km-sword.catch_km_mu)/sword.catch_km_sigma

sword.clean$mean_all <- rowMeans(cbind(sword.clean$catch_tripN,sword.clean$catch_setN,sword.clean$catch_hooksN,sword.clean$catch_kmN),na.rm = TRUE, dims = 1)

####################################################
## Getting the standardized values for American Samoa

samoa.combo1 <- merge(samoa.tuna.month.catch.trip,samoa.tuna.catchmean,by=c("date"),all.x = TRUE)
samoa.combo2 <- merge(samoa.combo1,samoa.tuna.cpuemean,by=c("date"),all.x = TRUE)

setnames(samoa.combo2, "tuna.x", "catch_trip")
setnames(samoa.combo2, "tuna.y", "catch_set")
setnames(samoa.combo2, "tuna.cpue", "catch_hooks")

samoa.clean <- subset(samoa.combo2, select = c(date,catch_trip,catch_set,catch_hooks,catch_km))

samoa.pre_reserve <- subset(samoa.clean,date<"2014-09-25")

samoa.catch_trip_mu <- mean(samoa.pre_reserve$catch_trip)
samoa.catch_trip_sigma <- sd(samoa.pre_reserve$catch_trip)
samoa.clean$catch_tripN <- (samoa.clean$catch_trip-samoa.catch_trip_mu)/samoa.catch_trip_sigma
samoa.catch_set_mu <- mean(samoa.pre_reserve$catch_set)
samoa.catch_set_sigma <- sd(samoa.pre_reserve$catch_set)
samoa.clean$catch_setN <- (samoa.clean$catch_set-samoa.catch_set_mu)/samoa.catch_set_sigma
samoa.catch_hooks_mu <- mean(samoa.pre_reserve$catch_hooks)
samoa.catch_hooks_sigma <- sd(samoa.pre_reserve$catch_hooks)
samoa.clean$catch_hooksN <- (samoa.clean$catch_hooks-samoa.catch_hooks_mu)/samoa.catch_hooks_sigma
samoa.catch_km_mu <- mean(samoa.pre_reserve$catch_km)
samoa.catch_km_sigma <- sd(samoa.pre_reserve$catch_km)
samoa.clean$catch_kmN <- (samoa.clean$catch_km-samoa.catch_km_mu)/samoa.catch_km_sigma

samoa.clean$mean_all <- rowMeans(cbind(samoa.clean$catch_tripN,samoa.clean$catch_setN,samoa.clean$catch_hooksN,samoa.clean$catch_kmN),na.rm = TRUE, dims = 1)

####################################################

pre_means <- c(mean(clean$catch_tripN[1:57]),mean(clean$catch_setN[1:57]),mean(clean$catch_hooksN[1:57]),mean(clean$catch_kmN[1:57]))
pre_line <- mean(pre_means)

PRI_means <- c(mean(clean$catch_tripN[58:80]),mean(clean$catch_setN[58:80]),mean(clean$catch_hooksN[58:80]),mean(clean$catch_kmN[58:80]))
PRI_line <- mean(PRI_means)

PMNM_means <- c(mean(clean$catch_tripN[81:96]),mean(clean$catch_setN[81:96]),mean(clean$catch_hooksN[81:96]),mean(clean$catch_kmN[81:96]))
PMNM_line <- mean(PMNM_means)

sword.PRI_means <- c(mean(sword.clean$catch_tripN[48:68]),mean(sword.clean$catch_setN[48:68]),mean(sword.clean$catch_hooksN[48:68]),mean(sword.clean$catch_kmN[48:68]))
sword.PRI_line <- mean(sword.PRI_means)

samoa.PMNM_means <- c(mean(samoa.clean$catch_tripN[72:81]),mean(samoa.clean$catch_setN[72:81]),mean(samoa.clean$catch_hooksN[72:81]),mean(samoa.clean$catch_kmN[72:81]))
samoa.PMNM_line <- mean(samoa.PMNM_means)

##################################
# This saves the data in a format that can be shared. removes everything else and then reloads it
save(clean, pre_line, PRI_line, PMNM_line, sword.PRI_line, samoa.PMNM_line, file = "clean_monthly_cpue_data.RData")
rm(list=ls())
load("clean_monthly_cpue_data.RData")

############################################

my_palette1 = wes_palette(n=5, "Darjeeling1")[c(1,2,4,5)] # very nice
my_palette2 = wes_palette(n=5, "FantasticFox1")[c(2,3,4,5)]
my_palette3 <- c(my_palette2,"#7D7D7D")

wes_palette(n=4,name="FantasticFox1")

# Policy Forum Graph

ggplot(clean, aes(date)) + 
  scale_color_manual(values=my_palette2) +
  geom_line(aes(y = catch_tripN, colour="Catch per Fishing Trip"),size=1) + 
  geom_line(aes(y = catch_setN,colour="Catch per Fishing Set"),size=1) +
  geom_line(aes(y = catch_hooksN, col = "Catch per 1,000 Hooks"),size=1) +
  geom_line(aes(y = catch_kmN,col="Catch per Kilometer Traveled"),size=1) +
  geom_segment(data=clean, aes(x=as.Date("2010-01-01"), xend=as.Date("2014-09-25"), y=pre_line, yend=pre_line), inherit.aes=F,linetype = 1,col="gray49",size=0.8) +
  geom_segment(data=clean, aes(x=as.Date("2014-09-25"), xend=as.Date("2016-08-26"), y=PRI_line, yend=PRI_line), inherit.aes=F,linetype = 1,col="gray49",size=0.8) +
  geom_segment(aes(x=as.Date("2014-09-25"), xend=as.Date("2016-08-26"), y=sword.PRI_line, yend=sword.PRI_line), inherit.aes=F,linetype = 2,col="gray49",size=0.8) +
  geom_segment(data=clean, aes(x=as.Date("2016-08-26"), xend=as.Date("2017-12-31"), y=PMNM_line, yend=PMNM_line), inherit.aes=F,linetype = 1,col="gray49",size=0.8) +
  geom_segment(aes(x=as.Date("2016-08-26"), xend=as.Date("2017-12-31"), y=samoa.PMNM_line, yend=samoa.PMNM_line), inherit.aes=F,linetype = 2,col="gray49",size=0.8) +
   geom_vline(xintercept = as.Date("2014-09-25"), linetype=4, col=wes_palette("Zissou1")[1],size=0.75) +
  geom_vline(xintercept = as.Date("2016-08-26"), linetype=4, col=wes_palette("Zissou1")[1],size=0.75) + labs(x = "Date",y="Standardized Catch Per Unit of Effort") +
  annotate("text", x=as.Date("2014-09-30"), y=3.75, 
           label="09/25/14: President Obama \n expands Pacific Remote Islands \n Marine National Monument", size=4.5,hjust = 0,colour=wes_palette("Zissou1")[1]) +
  annotate("text", x=as.Date("2016-08-30"), y=-1, 
           label="08/26/16: President Obama \n expands Papahanaumokuakea \n Marine National Monument", size=4.5,hjust = 0,col=wes_palette("Zissou1")[1]) +
   theme_minimal() +
guides(color=guide_legend(title=NULL)) +
  theme(legend.position="bottom",legend.text=element_text(size=14),axis.title=element_text(size=14))