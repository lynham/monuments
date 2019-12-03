## Catch and CPUE Regressions

rm(list = ls())

setwd("/Users/lynham/Documents/CSF MPA Project")
library(foreign)
library(sandwich)
library(lmtest)
library(car)
library(AER)
library(stargazer)
library(data.table)
library(ggplot2)
options(max.print=100000)

NOAA.raw <- read.csv(file="/Users/lynham/Documents/CSF MPA Project/cleaned_observer_data_090618.csv", header=TRUE, sep=",")

setnames(NOAA.raw, "set_year", "SET_YEAR")
setnames(NOAA.raw, "set_month", "SET_MONTH")
setnames(NOAA.raw, "set_day", "SET_DAY")
setnames(NOAA.raw, "hawaii", "HAWAII")

NOAA.raw$date_value <- as.Date(NOAA.raw$date,"%m/%d/%Y")

NOAA.raw$PRI_dummy <- as.numeric((NOAA.raw$SET_YEAR >= 2014) & (NOAA.raw$SET_MONTH >= 9) & (NOAA.raw$SET_DAY >= 25)| (NOAA.raw$SET_YEAR >= 2014) & (NOAA.raw$SET_MONTH >= 10) | (NOAA.raw$SET_YEAR >= 2015))
NOAA.raw$PMNM_dummy <- as.numeric((NOAA.raw$SET_YEAR >= 2016) & (NOAA.raw$SET_MONTH >= 8) & (NOAA.raw$SET_DAY >= 26)| (NOAA.raw$SET_YEAR >= 2016) & (NOAA.raw$SET_MONTH >= 9) | (NOAA.raw$SET_YEAR >= 2017))

NOAA.raw$WCPFC_2017 <- as.numeric((NOAA.raw$SET_YEAR == 2017) & (NOAA.raw$SET_MONTH == 9) | (NOAA.raw$SET_YEAR == 2017) & (NOAA.raw$SET_MONTH == 10) & (NOAA.raw$SET_DAY <= 9))
NOAA.raw$WCPFC_2016 <- as.numeric((NOAA.raw$SET_YEAR == 2016) & (NOAA.raw$SET_MONTH == 7) & (NOAA.raw$SET_DAY >= 22) | (NOAA.raw$SET_YEAR == 2016) & (NOAA.raw$SET_MONTH == 8) |(NOAA.raw$SET_YEAR == 2016) & (NOAA.raw$SET_MONTH == 9) & (NOAA.raw$SET_DAY <= 8))
NOAA.raw$WCPFC_2015 <- as.numeric((NOAA.raw$SET_YEAR == 2015) & (NOAA.raw$SET_MONTH == 8) & (NOAA.raw$SET_DAY >= 15) | (NOAA.raw$SET_YEAR == 2015) & (NOAA.raw$SET_MONTH == 9) |(NOAA.raw$SET_YEAR == 2015) & (NOAA.raw$SET_MONTH == 10) & (NOAA.raw$SET_DAY <= 8))
NOAA.raw$WCPFC_2010 <- as.numeric((NOAA.raw$SET_YEAR == 2010) & (NOAA.raw$SET_MONTH == 11) & (NOAA.raw$SET_DAY >= 22) | (NOAA.raw$SET_YEAR == 2010) & (NOAA.raw$SET_MONTH == 12))
NOAA.raw$WCPFC_2009 <- as.numeric((NOAA.raw$SET_YEAR == 2009) & (NOAA.raw$SET_MONTH == 12) & (NOAA.raw$SET_DAY >= 27))

NOAA.raw$WCPFC_close <- NOAA.raw$WCPFC_2009 + NOAA.raw$WCPFC_2010 + NOAA.raw$WCPFC_2015 + NOAA.raw$WCPFC_2016 + NOAA.raw$WCPFC_2017
NOAA.raw$WCPFC_2009<-NULL 
NOAA.raw$WCPFC_2010<-NULL
NOAA.raw$WCPFC_2015<-NULL
NOAA.raw$WCPFC_2016<-NULL
NOAA.raw$WCPFC_2017<-NULL

NOAA.raw$IATTC_2017 <- as.numeric((NOAA.raw$SET_YEAR == 2017) & (NOAA.raw$SET_MONTH == 9) & (NOAA.raw$SET_DAY >= 8) | (NOAA.raw$SET_YEAR == 2017) & (NOAA.raw$SET_MONTH == 10) | (NOAA.raw$SET_YEAR == 2017) & (NOAA.raw$SET_MONTH == 11) | (NOAA.raw$SET_YEAR == 2017) & (NOAA.raw$SET_MONTH == 12))
NOAA.raw$IATTC_2016 <- as.numeric((NOAA.raw$SET_YEAR == 2016) & (NOAA.raw$SET_MONTH == 7) & (NOAA.raw$SET_DAY >= 25) | (NOAA.raw$SET_YEAR == 2016) & (NOAA.raw$SET_MONTH == 8) | (NOAA.raw$SET_YEAR == 2016) & (NOAA.raw$SET_MONTH == 9) | (NOAA.raw$SET_YEAR == 2016) & (NOAA.raw$SET_MONTH == 10) & (NOAA.raw$SET_DAY < 4))
NOAA.raw$IATTC_2015 <- as.numeric((NOAA.raw$SET_YEAR == 2015) & (NOAA.raw$SET_MONTH == 8) & (NOAA.raw$SET_DAY >= 12) | (NOAA.raw$SET_YEAR == 2015) & (NOAA.raw$SET_MONTH >= 9) )
NOAA.raw$IATTC_2014 <- as.numeric((NOAA.raw$SET_YEAR == 2014) & (NOAA.raw$SET_MONTH == 10) & (NOAA.raw$SET_DAY >= 31) | (NOAA.raw$SET_YEAR == 2014) & (NOAA.raw$SET_MONTH >= 11) )
NOAA.raw$IATTC_2013 <- as.numeric((NOAA.raw$SET_YEAR == 2013) & (NOAA.raw$SET_MONTH == 11) & (NOAA.raw$SET_DAY >= 11) | (NOAA.raw$SET_YEAR == 2013) & (NOAA.raw$SET_MONTH == 12) )

NOAA.raw$IATTC_close <- NOAA.raw$IATTC_2013 + NOAA.raw$IATTC_2014 + NOAA.raw$IATTC_2015 + NOAA.raw$IATTC_2016 + NOAA.raw$IATTC_2017
NOAA.raw$IATTC_2013<-NULL 
NOAA.raw$IATTC_2014<-NULL 
NOAA.raw$IATTC_2015<-NULL
NOAA.raw$IATTC_2016<-NULL
NOAA.raw$IATTC_2017<-NULL

NOAA.raw$tuna <- NOAA.raw$count_bigeye_tuna + NOAA.raw$count_yellowfin_tuna
NOAA.raw$all_tuna <- NOAA.raw$count_bigeye_tuna + NOAA.raw$count_yellowfin_tuna + NOAA.raw$count_albacore_tuna + NOAA.raw$count_skipjack_tuna + NOAA.raw$count_bluefin_tuna + NOAA.raw$count_unid_tuna
NOAA.raw$tuna.cpue <- (NOAA.raw$tuna/NOAA.raw$num_hks_set)*1000
NOAA.raw$all_tuna.cpue <- (NOAA.raw$all_tuna/NOAA.raw$num_hks_set)*1000
NOAA.raw$sword.cpue <- (NOAA.raw$count_swordfish/NOAA.raw$num_hks_set)*1000

NOAA.raw$deep <- NOAA.raw$sword-1
NOAA.raw <- within(NOAA.raw, deep[sword==0] <- 1)
NOAA.raw$HAWAIITUNA <- NOAA.raw$deep
NOAA.raw <- within(NOAA.raw, HAWAIITUNA[HAWAII==0] <- 0)

newdata <- NOAA.raw[order(NOAA.raw$date_value),]
NOAA.raw <- newdata
rm(newdata)

##################################### 
### Total annual and monthly catch
#####################################
annual.catch.sum <-aggregate(tuna~SET_YEAR + HAWAII + sword,FUN=sum,data=NOAA.raw)
annual.catch.sum$PRI_dummy <- as.numeric((annual.catch.sum$SET_YEAR >= 2015))
annual.catch.sum$PMNM_dummy <- as.numeric( (annual.catch.sum$SET_YEAR >= 2017))

zero.lm = lm(tuna ~ PRI_dummy + PMNM_dummy, data=subset(annual.catch.sum, SET_YEAR>=2010 & HAWAII==1 & sword==0) )
summary(zero.lm)
coeftest(zero.lm, vcov = vcovHC(zero.lm, type="HC1")) 

month.catch.sum <-aggregate(tuna~SET_YEAR + SET_MONTH + HAWAII + sword + nino3 + lag1nino3 + lag2nino3 + lag3nino3,FUN=sum,data=NOAA.raw)
month.catch.sum$PRI_dummy <- as.numeric((month.catch.sum$SET_YEAR >= 2014) & (month.catch.sum$SET_MONTH >= 10) | (month.catch.sum$SET_YEAR >= 2015))
month.catch.sum$PMNM_dummy <- as.numeric((month.catch.sum$SET_YEAR >= 2016) & (month.catch.sum$SET_MONTH >= 9) | (month.catch.sum$SET_YEAR >= 2017))

one.lm = lm(tuna ~ PRI_dummy + PMNM_dummy, data=subset(month.catch.sum, SET_YEAR>=2010 & HAWAII==1 & sword==0) )
summary(one.lm)
coeftest(one.lm, vcov = vcovHC(one.lm, type="HC1")) 

two.lm = lm(tuna ~ PRI_dummy + PMNM_dummy + factor(SET_MONTH), data=subset(month.catch.sum, SET_YEAR>=2010 & HAWAII==1 & sword==0) )
summary(two.lm)
coeftest(two.lm, vcov = vcovHC(two.lm, type="HC1")) 

three.lm = lm(tuna ~ PRI_dummy + PMNM_dummy + factor(SET_MONTH) + factor(SET_YEAR), data=subset(month.catch.sum, SET_YEAR>=2010 & HAWAII==1 & sword==0) )
summary(three.lm)
coeftest(three.lm, vcov = vcovHC(three.lm, type="HC1")) 

four.lm = lm(tuna ~ PRI_dummy + PMNM_dummy + factor(SET_MONTH) + factor(SET_YEAR) + nino3 + lag1nino3 + lag2nino3 + lag3nino3, data=subset(month.catch.sum, SET_YEAR>=2010 & HAWAII==1 & sword==0) )
summary(four.lm)
coeftest(four.lm, vcov = vcovHC(four.lm, type="HC1")) 

robust_se <- list(sqrt(diag(vcovHC(zero.lm, type = "HC1"))),
                  sqrt(diag(vcovHC(one.lm, type = "HC1"))),
                  sqrt(diag(vcovHC(two.lm, type = "HC1"))),
                  sqrt(diag(vcovHC(three.lm, type = "HC1"))),
                  sqrt(diag(vcovHC(four.lm, type = "HC1"))))

stargazer(zero.lm, one.lm, two.lm, three.lm, four.lm, title="Total Catch of Bigeye and Yellowfin Tuna",
          intercept.bottom = FALSE, label="tab:total_catch",
          dep.var.labels.include = FALSE, dep.var.caption = "",
          se        = robust_se,
          add.lines = list(c("Month Dummies", "No", "No", "Yes","Yes","Yes"), c("Year Dummies", "No", "No", "No","Yes","Yes"), c("El Nino Controls", "No", "No", "No","No","Yes")),
          omit.stat=c("LL","ser","f","adj.rsq"), no.space=TRUE, keep = c("PRI_dummy","PMNM_dummy","Constant"),
          covariate.labels = c("Constant","PRI Expansion","PMNM Expansion"),
          notes.append = FALSE,
          notes = "\\parbox[t]{6in}{Notes: The dependent variable in Column (1) is total annual catch. The dependent variable in Columns (2)-(5) is total monthly catch. Each regression tests whether catch increases following the first expansion, and again following the second expansion. The sample runs from January 1st 2010 to December 31st 2017. Heteroskedasticity-robust standard errors presented in parentheses. $^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01}",
          notes.align = "l",
          out="/Volumes/GoogleDrive/My Drive/Large MPA Data Depository/drafts/total_catch.tex")

rm(zero.lm,one.lm,two.lm,three.lm,four.lm,robust_se)

#####################################

####################################
# Catch per 1,000 Hooks regressions:
####################################
one.lm = lm(tuna.cpue ~ PRI_dummy + PMNM_dummy, data=subset(NOAA.raw, SET_YEAR>=2010 & HAWAII==1 & sword==0) )
summary(one.lm)
coeftest(one.lm, vcov = vcovHC(one.lm, type="HC1")) 

two.lm = lm(tuna.cpue ~ PRI_dummy + PMNM_dummy + factor(SET_MONTH), data=subset(NOAA.raw, SET_YEAR>=2010 & HAWAII==1 & sword==0) )
summary(two.lm)
coeftest(two.lm, vcov = vcovHC(two.lm, type="HC1")) 

three.lm = lm(tuna.cpue ~ PRI_dummy + PMNM_dummy + factor(SET_MONTH) + factor(SET_YEAR), data=subset(NOAA.raw, SET_YEAR>=2010 & HAWAII==1 & sword==0) )
summary(three.lm)
coeftest(three.lm, vcov = vcovHC(three.lm, type="HC1")) 

four.lm = lm(tuna.cpue ~ PRI_dummy + PMNM_dummy + factor(SET_MONTH) + factor(SET_YEAR) + factor(vessel_id), data=subset(NOAA.raw, SET_YEAR>=2010 & HAWAII==1 & sword==0) )
summary(four.lm)
coeftest(four.lm, vcov = vcovHC(four.lm, type="HC1"))

five.lm = lm(tuna.cpue ~ PRI_dummy + PMNM_dummy + WCPFC_close + IATTC_close + experiment + nino3 + lag1nino3 + lag2nino3 + lag3nino3 + factor(SET_MONTH) + factor(SET_YEAR) + factor(vessel_id), data=subset(NOAA.raw, SET_YEAR>=2010 & HAWAII==1 & sword==0) )
summary(five.lm)
coeftest(five.lm, vcov = vcovHC(five.lm, type="HC1"))

robust_se <- list(sqrt(diag(vcovHC(one.lm, type = "HC1"))),
                  sqrt(diag(vcovHC(two.lm, type = "HC1"))),
                  sqrt(diag(vcovHC(three.lm, type = "HC1"))),
                  sqrt(diag(vcovHC(four.lm, type = "HC1"))),
                  sqrt(diag(vcovHC(five.lm, type = "HC1"))))

stargazer(one.lm, two.lm, three.lm, four.lm, five.lm, title="Catch of Bigeye and Yellowfin Tuna per 1,000 Hooks",
          intercept.bottom = FALSE, label="tab:cpue",
          se        = robust_se,
          dep.var.labels.include = FALSE, dep.var.caption = "",
          add.lines = list(c("Month Dummies", "No", "Yes", "Yes","Yes","Yes"),c("Year Dummies", "No", "No", "Yes","Yes","Yes"), c("Vessel Dummies", "No", "No","No","Yes","Yes"), c("Additional Controls", "No", "No","No","No","Yes")),
          omit.stat=c("LL","ser","f","adj.rsq"), no.space=TRUE, keep = c("PRI_dummy","PMNM_dummy","Constant"),
          covariate.labels = c("Constant","PRI Expansion","PMNM Expansion"),
           notes.label = NULL,
          notes = "\\parbox[t]{4.75in}{Notes: Each successive column adds additional controls to a simple regression test of whether CPUE increases following the first expansion and again following the second expansion (Column (1)). The sample runs from January 1st 2010 to December 31st 2017. Heteroskedasticity-robust standard errors presented in parentheses. The Additional Controls are whether the set included an experimental component, a dummy variable for whether the WCPFC waters were closed to fishing, a dummy variable for whether IATTC waters were closed to vessels longer than 24m, Monthly El Nino indicator, Monthly El Nino indicator lagged by one year, Monthly El Nino indicator lagged by two years, and Monthly El Nino indicator lagged by three years. $^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01 for two-sided t-test of statistical significance using heteroskedasticity-robust standard errors.}",      notes.append = FALSE,    
          notes.align = "l",
          out="/Volumes/GoogleDrive/My Drive/Large MPA Data Depository/drafts/cpue.tex")


rm(one.lm,two.lm,three.lm,four.lm,five.lm,robust_se)

#################################
# Catch per Set Regressions
#################################

one.lm = lm(tuna ~ PRI_dummy + PMNM_dummy, data=subset(NOAA.raw, SET_YEAR>=2010 & HAWAII==1 & sword==0) )
summary(one.lm)
coeftest(one.lm, vcov = vcovHC(one.lm, type="HC1")) 

two.lm = lm(tuna ~ PRI_dummy + PMNM_dummy + factor(SET_MONTH), data=subset(NOAA.raw, SET_YEAR>=2010 & HAWAII==1 & sword==0) )
summary(two.lm)
coeftest(two.lm, vcov = vcovHC(two.lm, type="HC1")) 

three.lm = lm(tuna ~ PRI_dummy + PMNM_dummy + factor(SET_MONTH) + factor(SET_YEAR), data=subset(NOAA.raw, SET_YEAR>=2010 & HAWAII==1 & sword==0) )
summary(three.lm)
coeftest(three.lm, vcov = vcovHC(three.lm, type="HC1")) 

four.lm = lm(tuna ~ PRI_dummy + PMNM_dummy + factor(SET_MONTH) + factor(SET_YEAR) + factor(vessel_id), data=subset(NOAA.raw, SET_YEAR>=2010 & HAWAII==1 & sword==0) )
summary(four.lm)
coeftest(four.lm, vcov = vcovHC(four.lm, type="HC1"))

five.lm = lm(tuna ~ PRI_dummy + PMNM_dummy + WCPFC_close + IATTC_close + experiment + nino3 + lag1nino3 + lag2nino3 + lag3nino3 + factor(SET_MONTH) + factor(SET_YEAR) + factor(vessel_id), data=subset(NOAA.raw, SET_YEAR>=2010 & HAWAII==1 & sword==0) )
summary(five.lm)
coeftest(five.lm, vcov = vcovHC(five.lm, type="HC1"))

robust_se <- list(sqrt(diag(vcovHC(one.lm, type = "HC1"))),
                  sqrt(diag(vcovHC(two.lm, type = "HC1"))),
                  sqrt(diag(vcovHC(three.lm, type = "HC1"))),
                  sqrt(diag(vcovHC(four.lm, type = "HC1"))),
                  sqrt(diag(vcovHC(five.lm, type = "HC1"))))

stargazer(one.lm, two.lm, three.lm, four.lm, five.lm, title="Catch of Bigeye and Yellowfin Tuna per Set",
          intercept.bottom = FALSE, label="tab:catch",
          se        = robust_se,
          dep.var.labels.include = FALSE, dep.var.caption = "",
          add.lines = list(c("Month Dummies", "No", "Yes", "Yes","Yes","Yes"),c("Year Dummies", "No", "No", "Yes","Yes","Yes"), c("Vessel Dummies", "No", "No","No","Yes","Yes"), c("Additional Controls", "No", "No","No","No","Yes")),
          omit.stat=c("LL","ser","f","adj.rsq"), no.space=TRUE, keep = c("PRI_dummy","PMNM_dummy","Constant"),
          covariate.labels = c("Constant","PRI Expansion","PMNM Expansion"),
          notes.label = NULL,
          notes = "\\parbox[t]{5in}{Notes: Each successive column adds additional controls to a simple regression test of whether CPUE increases following the first expansion and again following the second expansion (Column (1)). The sample runs from January 1st 2010 to December 31st 2017. Heteroskedasticity-robust standard errors presented in parentheses. The Additional Controls are whether the set included an experimental component, a dummy variable for whether the WCPFC waters were closed to fishing, a dummy variable for whether IATTC waters were closed to vessels longer than 24m, Monthly El Nino indicator, Monthly El Nino indicator lagged by one year, Monthly El Nino indicator lagged by two years, and Monthly El Nino indicator lagged by three years. $^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01}",      notes.append = FALSE,    
          notes.align = "l",
          out="/Volumes/GoogleDrive/My Drive/Large MPA Data Depository/drafts/catch.tex")

rm(one.lm,two.lm,three.lm,four.lm,five.lm,robust_se)

#############################
### Catch Per Trip
#############################

NOAAdata <- read.csv(file="/Users/lynham/Documents/CSF MPA Project/collapsed_distanceTRIPID.csv", header=TRUE, sep=",")
# This dataset comes from the STATA collapse_trip_distance.do file
setnames(NOAAdata, "set_year", "SET_YEAR")
setnames(NOAAdata, "set_month", "SET_MONTH")
setnames(NOAAdata, "set_day", "SET_DAY")
setnames(NOAAdata, "hawaii", "HAWAII")
NOAAdata$PRI_dummy <- as.numeric((NOAAdata$SET_YEAR >= 2014) & (NOAAdata$SET_MONTH >= 9) & (NOAAdata$SET_DAY >= 25)| (NOAAdata$SET_YEAR >= 2014) & (NOAAdata$SET_MONTH >= 10) | (NOAAdata$SET_YEAR >= 2015))
NOAAdata$PMNM_dummy <- as.numeric((NOAAdata$SET_YEAR >= 2016) & (NOAAdata$SET_MONTH >= 8) & (NOAAdata$SET_DAY >= 26)| (NOAAdata$SET_YEAR >= 2016) & (NOAAdata$SET_MONTH >= 9) | (NOAAdata$SET_YEAR >= 2017))
NOAAdata$dec.2017 <- as.numeric((NOAAdata$SET_YEAR == 2017) & (NOAAdata$SET_MONTH == 12) )

NOAAdata$WCPFC_2017 <- as.numeric((NOAAdata$SET_YEAR == 2017) & (NOAAdata$SET_MONTH == 9) | (NOAAdata$SET_YEAR == 2017) & (NOAAdata$SET_MONTH == 10) & (NOAAdata$SET_DAY <= 9))
NOAAdata$WCPFC_2016 <- as.numeric((NOAAdata$SET_YEAR == 2016) & (NOAAdata$SET_MONTH == 7) & (NOAAdata$SET_DAY >= 22) | (NOAAdata$SET_YEAR == 2016) & (NOAAdata$SET_MONTH == 8) |(NOAAdata$SET_YEAR == 2016) & (NOAAdata$SET_MONTH == 9) & (NOAAdata$SET_DAY <= 8))
NOAAdata$WCPFC_2015 <- as.numeric((NOAAdata$SET_YEAR == 2015) & (NOAAdata$SET_MONTH == 8) & (NOAAdata$SET_DAY >= 15) | (NOAAdata$SET_YEAR == 2015) & (NOAAdata$SET_MONTH == 9) |(NOAAdata$SET_YEAR == 2015) & (NOAAdata$SET_MONTH == 10) & (NOAAdata$SET_DAY <= 8))
NOAAdata$WCPFC_2010 <- as.numeric((NOAAdata$SET_YEAR == 2010) & (NOAAdata$SET_MONTH == 11) & (NOAAdata$SET_DAY >= 22) | (NOAAdata$SET_YEAR == 2010) & (NOAAdata$SET_MONTH == 12))
NOAAdata$WCPFC_2009 <- as.numeric((NOAAdata$SET_YEAR == 2009) & (NOAAdata$SET_MONTH == 12) & (NOAAdata$SET_DAY >= 27))

NOAAdata$WCPFC_close <- NOAAdata$WCPFC_2009 + NOAAdata$WCPFC_2010 + NOAAdata$WCPFC_2015 + NOAAdata$WCPFC_2016 + NOAAdata$WCPFC_2017
NOAAdata$WCPFC_2009<-NULL 
NOAAdata$WCPFC_2010<-NULL
NOAAdata$WCPFC_2015<-NULL
NOAAdata$WCPFC_2016<-NULL
NOAAdata$WCPFC_2017<-NULL

NOAAdata$IATTC_2017 <- as.numeric((NOAAdata$SET_YEAR == 2017) & (NOAAdata$SET_MONTH == 9) & (NOAAdata$SET_DAY >= 8) | (NOAAdata$SET_YEAR == 2017) & (NOAAdata$SET_MONTH == 10) | (NOAAdata$SET_YEAR == 2017) & (NOAAdata$SET_MONTH == 11) | (NOAAdata$SET_YEAR == 2017) & (NOAAdata$SET_MONTH == 12))
NOAAdata$IATTC_2016 <- as.numeric((NOAAdata$SET_YEAR == 2016) & (NOAAdata$SET_MONTH == 7) & (NOAAdata$SET_DAY >= 25) | (NOAAdata$SET_YEAR == 2016) & (NOAAdata$SET_MONTH == 8) | (NOAAdata$SET_YEAR == 2016) & (NOAAdata$SET_MONTH == 9) | (NOAAdata$SET_YEAR == 2016) & (NOAAdata$SET_MONTH == 10) & (NOAAdata$SET_DAY < 4))
NOAAdata$IATTC_2015 <- as.numeric((NOAAdata$SET_YEAR == 2015) & (NOAAdata$SET_MONTH == 8) & (NOAAdata$SET_DAY >= 12) | (NOAAdata$SET_YEAR == 2015) & (NOAAdata$SET_MONTH >= 9) )
NOAAdata$IATTC_2014 <- as.numeric((NOAAdata$SET_YEAR == 2014) & (NOAAdata$SET_MONTH == 10) & (NOAAdata$SET_DAY >= 31) | (NOAAdata$SET_YEAR == 2014) & (NOAAdata$SET_MONTH >= 11) )
NOAAdata$IATTC_2013 <- as.numeric((NOAAdata$SET_YEAR == 2013) & (NOAAdata$SET_MONTH == 11) & (NOAAdata$SET_DAY >= 11) | (NOAAdata$SET_YEAR == 2013) & (NOAAdata$SET_MONTH == 12) )

NOAAdata$IATTC_close <- NOAAdata$IATTC_2013 + NOAAdata$IATTC_2014 + NOAAdata$IATTC_2015 + NOAAdata$IATTC_2016 + NOAAdata$IATTC_2017
NOAAdata$IATTC_2013<-NULL 
NOAAdata$IATTC_2014<-NULL 
NOAAdata$IATTC_2015<-NULL
NOAAdata$IATTC_2016<-NULL
NOAAdata$IATTC_2017<-NULL

trips.data <- NOAAdata

trips.data$tuna <- trips.data$count_bigeye_tuna + trips.data$count_yellowfin_tuna
trips.data$tuna_km <- trips.data$tuna/trips.data$distance_km

trips.data$deep <- trips.data$sword-1
trips.data <- within(trips.data, deep[sword==0] <- 1)
trips.data$HAWAIITUNA <- trips.data$deep
trips.data <- within(trips.data, HAWAIITUNA[HAWAII==0] <- 0)

#####################################

one.lm = lm(tuna ~ PRI_dummy + PMNM_dummy, data=subset(trips.data, SET_YEAR>=2010 & HAWAII==1 & sword==0 & dec.2017==0) )
summary(one.lm)
coeftest(one.lm, vcov = vcovHC(one.lm, type="HC1")) 

two.lm = lm(tuna ~ PRI_dummy + PMNM_dummy + factor(SET_MONTH), data=subset(trips.data, SET_YEAR>=2010 & HAWAII==1 & sword==0 & dec.2017==0) )
summary(two.lm)
coeftest(two.lm, vcov = vcovHC(two.lm, type="HC1")) 

three.lm = lm(tuna ~ PRI_dummy + PMNM_dummy + factor(SET_MONTH) + factor(SET_YEAR), data=subset(trips.data, SET_YEAR>=2010 & HAWAII==1 & sword==0 & dec.2017==0) )
summary(three.lm)
coeftest(three.lm, vcov = vcovHC(three.lm, type="HC1"))

four.lm = lm(tuna ~ PRI_dummy + PMNM_dummy + factor(SET_MONTH) + factor(SET_YEAR) + factor(vessel_id), data=subset(trips.data, SET_YEAR>=2010 & HAWAII==1 & sword==0 & dec.2017==0) )
summary(four.lm)
coeftest(four.lm, vcov = vcovHC(four.lm, type="HC1")) 

five.lm = lm(tuna ~ PRI_dummy + PMNM_dummy + WCPFC_close + IATTC_close + experiment + nino3 + lag1nino3 + lag2nino3 + lag3nino3 + factor(SET_MONTH) + factor(SET_YEAR) + factor(vessel_id), data=subset(trips.data, SET_YEAR>=2010 & HAWAII==1 & sword==0 & dec.2017==0) )
summary(five.lm)
coeftest(five.lm, vcov = vcovHC(five.lm, type="HC1")) 

robust_se <- list(sqrt(diag(vcovHC(one.lm, type = "HC1"))),
                  sqrt(diag(vcovHC(two.lm, type = "HC1"))),
                  sqrt(diag(vcovHC(three.lm, type = "HC1"))),
                  sqrt(diag(vcovHC(four.lm, type = "HC1"))),
                  sqrt(diag(vcovHC(five.lm, type = "HC1"))))

stargazer(one.lm, two.lm, three.lm, four.lm, five.lm, title="Catch of Bigeye and Yellowfin Tuna per Trip",
          intercept.bottom = FALSE, label="tab:catch_trip",
          se        = robust_se,
          dep.var.labels.include = FALSE, dep.var.caption = "",
          add.lines = list(c("Month Dummies", "No", "Yes", "Yes","Yes","Yes"),c("Year Dummies", "No", "No", "Yes","Yes","Yes"), c("Vessel Dummies", "No", "No","No","Yes","Yes"), c("Additional Controls", "No", "No","No","No","Yes")),
          omit.stat=c("LL","ser","f","adj.rsq"), no.space=TRUE, keep = c("PRI_dummy","PMNM_dummy","Constant"),
          covariate.labels = c("Constant","PRI Expansion","PMNM Expansion"),
          notes.label = NULL,
          notes = "\\parbox[t]{5.5in}{Notes: Each successive column adds additional controls to a simple regression test of whether CPUE increases following the first expansion and again following the second expansion (Column (1)). The sample runs from January 1st 2010 to December 31st 2017. Heteroskedasticity-robust standard errors presented in parentheses. The Additional Controls are whether the set included an experimental component, a dummy variable for whether the WCPFC waters were closed to fishing, a dummy variable for whether IATTC waters were closed to vessels longer than 24m, Monthly El Nino indicator, Monthly El Nino indicator lagged by one year, Monthly El Nino indicator lagged by two years, and Monthly El Nino indicator lagged by three years. $^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01}",      notes.append = FALSE,    
          notes.align = "l",
          out="/Volumes/GoogleDrive/My Drive/Large MPA Data Depository/drafts/catch_trip.tex")

rm(one.lm,two.lm,three.lm,four.lm,five.lm,robust_se)

################################
### Catch Per Km Traveled
################################

one.lm = lm(tuna_km ~ PRI_dummy + PMNM_dummy, data=subset(trips.data, SET_YEAR>=2010 & HAWAII==1 & sword==0 & dec.2017==0) )
summary(one.lm)
coeftest(one.lm, vcov = vcovHC(one.lm, type="HC1")) 

two.lm = lm(tuna_km ~ PRI_dummy + PMNM_dummy + factor(SET_MONTH), data=subset(trips.data, SET_YEAR>=2010 & HAWAII==1 & sword==0 & dec.2017==0) )
summary(two.lm)
coeftest(two.lm, vcov = vcovHC(two.lm, type="HC1")) 

three.lm = lm(tuna_km ~ PRI_dummy + PMNM_dummy + factor(SET_MONTH) + factor(SET_YEAR), data=subset(trips.data, SET_YEAR>=2010 & HAWAII==1 & sword==0 & dec.2017==0) )
summary(three.lm)
coeftest(three.lm, vcov = vcovHC(three.lm, type="HC1"))

four.lm = lm(tuna_km ~ PRI_dummy + PMNM_dummy + factor(SET_MONTH) + factor(SET_YEAR) + factor(vessel_id), data=subset(trips.data, SET_YEAR>=2010 & HAWAII==1 & sword==0 & dec.2017==0) )
summary(four.lm)
coeftest(four.lm, vcov = vcovHC(four.lm, type="HC1")) 

five.lm = lm(tuna_km ~ PRI_dummy + PMNM_dummy + WCPFC_close + IATTC_close + experiment + nino3 + lag1nino3 + lag2nino3 + lag3nino3 + factor(SET_MONTH) + factor(SET_YEAR) + factor(vessel_id), data=subset(trips.data, SET_YEAR>=2010 & HAWAII==1 & sword==0 & dec.2017==0) )
summary(five.lm)
coeftest(five.lm, vcov = vcovHC(five.lm, type="HC1")) 

robust_se <- list(sqrt(diag(vcovHC(one.lm, type = "HC1"))),
                  sqrt(diag(vcovHC(two.lm, type = "HC1"))),
                  sqrt(diag(vcovHC(three.lm, type = "HC1"))),
                  sqrt(diag(vcovHC(four.lm, type = "HC1"))),
                  sqrt(diag(vcovHC(five.lm, type = "HC1"))))

stargazer(one.lm, two.lm, three.lm, four.lm, five.lm,title="Catch of Bigeye and Yellowfin Tuna per Kilometer Traveled",
          intercept.bottom = FALSE, label="tab:catch_km",
          se        = robust_se,
          dep.var.labels.include = FALSE, dep.var.caption = "",
          add.lines = list(c("Month Dummies", "No", "Yes", "Yes","Yes","Yes"),c("Year Dummies", "No", "No", "Yes","Yes","Yes"), c("Vessel Dummies", "No", "No","No","Yes","Yes"), c("Additional Controls", "No", "No","No","No","Yes")),
          omit.stat=c("LL","ser","f","adj.rsq"), no.space=TRUE, keep = c("PRI_dummy","PMNM_dummy","Constant"),
          covariate.labels = c("Constant","PRI Expansion","PMNM Expansion"),
          notes.label = NULL,
          notes = "\\parbox[t]{5.5in}{Notes: Each successive column adds additional controls to a simple regression test of whether CPUE increases following the first expansion and again following the second expansion (Column (1)). The sample runs from January 1st 2010 to December 31st 2017. Heteroskedasticity-robust standard errors presented in parentheses. The Additional Controls are whether the set included an experimental component, a dummy variable for whether the WCPFC waters were closed to fishing, a dummy variable for whether IATTC waters were closed to vessels longer than 24m, Monthly El Nino indicator, Monthly El Nino indicator lagged by one year, Monthly El Nino indicator lagged by two years, and Monthly El Nino indicator lagged by three years. $^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01}",      notes.append = FALSE,    
          notes.align = "l",
          out="/Volumes/GoogleDrive/My Drive/Large MPA Data Depository/drafts/catch_km.tex")

rm(one.lm,two.lm,three.lm,four.lm,five.lm,robust_se)

###############################

# checking the correlation between PMNM and 2017 dummy

month.catch.sum.hawaii.tuna <- subset(month.catch.sum,SET_YEAR>=2010 & HAWAII==1 & sword==0)

month.catch.sum.hawaii.tuna$dummy_2017 <- ifelse(month.catch.sum.hawaii.tuna$SET_YEAR==2017,1,0)

cor(month.catch.sum.hawaii.tuna$PMNM_dummy,month.catch.sum.hawaii.tuna$dummy_2017)

rm(one.lm,two.lm,three.lm,four.lm,robust_se)

### Making the Table for the Appendix

robust_se <- list(sqrt(diag(vcovHC(full.results.total, type = "HC1"))),
     sqrt(diag(vcovHC(full.results.catchset, type = "HC1"))),
     sqrt(diag(vcovHC(full.results.catchhooks, type = "HC1"))))

stargazer(full.results.total, full.results.catchset, full.results.catchhooks, title="Catch of Bigeye and Yellowfin Tuna - Full Results",
          intercept.bottom = FALSE, label="tab:catch_full",
          se        = robust_se,
          dep.var.labels   = c("Total Catch","Catch per Set","Catch per 1,000 Hooks"),
          add.lines = list(c("Month Dummies", "Yes", "Yes","Yes"), c("Vessel Dummies", "No", "Yes","Yes"),c("Additional Controls", "Yes", "Yes","Yes")),
          omit.stat=c("LL","ser","f","adj.rsq"), no.space=TRUE, keep = c("PRI_dummy","PMNM_dummy","experiment","nino3","lag1nino3","lag2nino3","lag3nino3","Constant"),
          out="/Volumes/GoogleDrive/My Drive/Large MPA Data Depository/drafts/catch_full.tex")


## Graphs

month.catch.sum$date <- as.Date(paste(15,month.catch.sum$SET_MONTH,month.catch.sum$SET_YEAR),"%d%m%Y")
hawaii.tuna.catchsum <- subset(month.catch.sum, HAWAII==1 & SET_YEAR>=2010 & sword==0)

ggplot(data=hawaii.tuna.catchsum, aes(x=date, y=tuna)) +
  geom_line() +
  geom_vline(xintercept = as.numeric(as.Date("2014-09-25")), linetype=4) +
  geom_vline(xintercept = as.numeric(as.Date("2016-08-26")), linetype=4) + labs(x = "Date",y="Bigeye and Yellowfin Tuna Caught")

month.cpue.mean <-aggregate(tuna.cpue~SET_YEAR + SET_MONTH + HAWAII + sword,FUN=mean,data=NOAA.raw)

month.cpue.mean$date <- as.Date(paste(15,month.cpue.mean$SET_MONTH,month.cpue.mean$SET_YEAR),"%d%m%Y")
hawaii.tuna.catchmean <- subset(month.cpue.mean, HAWAII==1 & SET_YEAR>=2010 & sword==0)

ggplot(data=hawaii.tuna.catchmean, aes(x=date, y=tuna.cpue)) +
  geom_line() +
  geom_vline(xintercept = as.numeric(as.Date("2014-09-25")), linetype=4) +
  geom_vline(xintercept = as.numeric(as.Date("2016-08-26")), linetype=4) + labs(x = "Date",y="Bigeye and Yellowfin Tuna Caught per 1,000 Hooks")


# Diff-in-Diff

# Total Catch

one.lm = lm(tuna ~ PMNM_dummy + PMNM_dummy*HAWAII, data=subset(month.catch.sum, PRI_dummy==1 & sword==0) )
summary(one.lm)
coeftest(one.lm, vcov = vcovHC(one.lm, type="HC1")) 

two.lm = lm(tuna ~ PMNM_dummy + PMNM_dummy*HAWAII + factor(SET_MONTH), data=subset(month.catch.sum, PRI_dummy==1 & sword==0) )
summary(two.lm)
coeftest(two.lm, vcov = vcovHC(two.lm, type="HC1")) 

three.lm = lm(tuna ~ PMNM_dummy + PMNM_dummy*HAWAII + nino3 + lag1nino3 + lag2nino3 + lag3nino3 + factor(SET_MONTH), data=subset(month.catch.sum, PRI_dummy==1 & sword==0) )
summary(three.lm)
coeftest(three.lm, vcov = vcovHC(three.lm, type="HC1"))

robust_se <- list(sqrt(diag(vcovHC(one.lm, type = "HC1"))),
                  sqrt(diag(vcovHC(two.lm, type = "HC1"))),
                  sqrt(diag(vcovHC(three.lm, type = "HC1"))))

stargazer(one.lm, two.lm, three.lm, title="Difference-In-Differences: Total Monthly Catch of Bigeye and Yellowfin Tuna",
          intercept.bottom = FALSE, label="tab:DIDtotal_catch",
          se        = robust_se,
          add.lines = list(c("Month Dummies", "No", "Yes","Yes"),c("Additional Controls", "No", "No","Yes")),
          omit.stat=c("LL","ser","f","adj.rsq"), no.space=TRUE, keep = c("PMNM_dummy","HAWAII","PMNM_dummy*HAWAII","Constant"),
          out="/Volumes/GoogleDrive/My Drive/Large MPA Data Depository/drafts/DIDtotal_catch.tex")

rm(one.lm,two.lm,three.lm,robust_se)

## CPUE

# What is the correlation between CPUE in Hawaii and American Samoa?

combo <-merge(hawaii.tuna.cpuemean,samoa.tuna.cpuemean,by=c("SET_MONTH","SET_YEAR"),all.x = TRUE)
cor(combo$tuna.cpue.x,combo$tuna.cpue.y,use = "complete.obs") # -0.1996694

# What is swordfish fleet CPUE of tuna?
hawaii.sword.cpuemean <- subset(month.cpue.mean, HAWAII==1 & SET_YEAR>=2010 & sword==1)

tuna.sword <-merge(hawaii.tuna.cpuemean,hawaii.sword.cpuemean,by=c("SET_MONTH","SET_YEAR"),all.x = TRUE)
cor(tuna.sword$tuna.cpue.x,tuna.sword$tuna.cpue.y,use = "complete.obs") # 0.4265842

one.lm = lm(tuna.cpue ~ PMNM_dummy + PMNM_dummy*HAWAII, data=subset(NOAA.raw, PRI_dummy==1 & sword==0) )
summary(one.lm)
coeftest(one.lm, vcov = vcovHC(one.lm, type="HC1")) 

one.lm = lm(tuna.cpue ~ PMNM_dummy + PMNM_dummy*HAWAII, data=subset(NOAA.raw, SET_YEAR>=2010 & sword==0) )
summary(one.lm)
coeftest(one.lm, vcov = vcovHC(one.lm, type="HC1")) 

one.lm = lm(tuna.cpue ~ PMNM_dummy + PRI_dummy*HAWAII + PMNM_dummy*HAWAII, data=subset(NOAA.raw, SET_YEAR>=2010 & sword==0) )
summary(one.lm)
coeftest(one.lm, vcov = vcovHC(one.lm, type="HC1"))

one.lm = lm(tuna.cpue ~ PRI_dummy*deep, data=subset(NOAA.raw, SET_YEAR>=2010 & PMNM_dummy==0) )
summary(one.lm)
coeftest(one.lm, vcov = vcovHC(one.lm, type="HC1"))

four.lm = lm(tuna.cpue ~ PRI_dummy*deep + experiment + nino3 + lag1nino3 + lag2nino3 + lag3nino3 + factor(SET_MONTH) + factor(vessel_id), data=subset(NOAA.raw, SET_YEAR>=2010 & PMNM_dummy==0) )
summary(four.lm)
coeftest(four.lm, vcov = vcovHC(four.lm, type="HC1"))

two.lm = lm(tuna.cpue ~ PMNM_dummy + PMNM_dummy*HAWAII + factor(SET_MONTH), data=subset(NOAA.raw, PRI_dummy==1 & sword==0) )
summary(two.lm)
coeftest(two.lm, vcov = vcovHC(two.lm, type="HC1")) 

three.lm = lm(tuna.cpue ~ PMNM_dummy + PMNM_dummy*HAWAII + factor(SET_MONTH) + factor(vessel_id), data=subset(NOAA.raw, PRI_dummy==1 & sword==0) )
summary(three.lm)
coeftest(three.lm, vcov = vcovHC(three.lm, type="HC1"))

four.lm = lm(tuna.cpue ~ PMNM_dummy + PMNM_dummy*HAWAII + experiment + nino3 + lag1nino3 + lag2nino3 + lag3nino3 + factor(SET_MONTH) + factor(vessel_id), data=subset(NOAA.raw, PRI_dummy==1 & sword==0) )
summary(four.lm)
coeftest(four.lm, vcov = vcovHC(four.lm, type="HC1"))

four.lm = lm(tuna.cpue ~ PMNM_dummy + PMNM_dummy*HAWAII + experiment + nino3 + lag1nino3 + lag2nino3 + lag3nino3 + factor(SET_MONTH) + factor(vessel_id), data=subset(NOAA.raw, SET_YEAR>=2010 & sword==0) )
summary(four.lm)
coeftest(four.lm, vcov = vcovHC(four.lm, type="HC1"))

four.lm = lm(tuna.cpue ~ PMNM_dummy + PRI_dummy*HAWAII + PMNM_dummy*HAWAII + experiment + nino3 + lag1nino3 + lag2nino3 + lag3nino3 + factor(SET_MONTH) + factor(vessel_id), data=subset(NOAA.raw, SET_YEAR>=2010 & sword==0) )
summary(four.lm)
coeftest(four.lm, vcov = vcovHC(four.lm, type="HC1"))

robust_se <- list(sqrt(diag(vcovHC(one.lm, type = "HC1"))),
                  sqrt(diag(vcovHC(two.lm, type = "HC1"))),
                  sqrt(diag(vcovHC(three.lm, type = "HC1"))),
                  sqrt(diag(vcovHC(four.lm, type = "HC1"))))

stargazer(one.lm, two.lm, three.lm, four.lm, title="Diference-in-Differences: Catch of Bigeye and Yellowfin Tuna per 1,000 Hooks",
          intercept.bottom = FALSE, label="tab:DIDcpue",
          se        = robust_se,
          add.lines = list(c("Month Dummies", "No", "Yes", "Yes","Yes"), c("Vessel Dummies", "No", "No","Yes","Yes"), c("Additional Controls", "No", "No","No","Yes")),
          omit.stat=c("LL","ser","f","adj.rsq"), no.space=TRUE, keep = c("PMNM_dummy","HAWAII","PMNM_dummy*HAWAII","Constant"),
          notes.label = NULL,
          # notes        = "Sometimes you just have to start over.",
          notes = "\\parbox[t]{7in}{Notes: Each successive column adds additional controls to a simple regression test of whether CPUE increases following the first expansion, and again following the second expansion (Column (1)). The sample runs from January 1st 2010 to December 31st 2017. Heteroskedasticity-robust standard errors presented in parentheses. The Additional Controls are whether the set included an experimental component, a dummy variable for whether the WCPFC waters were closed to fishing, a dummy variable for whether IATTC waters were closed to vessels longer than 24m, Monthly El Nino indicator, Monthly El Nino indicator lagged by one year, Monthly El Nino indicator lagged by two years, and Monthly El Nino indicator lagged by three years. $^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01 for two-sided t-test of statistical significance using heteroskedasticity-robust standard errors.}",      notes.append = FALSE,    
          notes.align = "l",
          
          out="/Volumes/GoogleDrive/My Drive/Large MPA Data Depository/drafts/DIDcpue.tex")

#################################
#Policy Forum Diff-in-Diff Table
#################################

sapply(subset(NOAA.raw, SET_YEAR>=2010), function(x)length(unique(x))) #2,922

one.lm = lm(tuna.cpue ~ PRI_dummy*HAWAIITUNA, data=subset(NOAA.raw, SET_YEAR>=2010 & PMNM_dummy==0 & HAWAII==1) )
summary(one.lm)
coeftest(one.lm, vcov = vcovHC(one.lm, type="HC1"))
coeftest(one.lm, vcov = NeweyWest(one.lm))

two.lm = lm(tuna.cpue ~ PRI_dummy*HAWAIITUNA + factor(SET_MONTH) + factor(SET_YEAR), data=subset(NOAA.raw, SET_YEAR>=2010 & PMNM_dummy==0 & HAWAII==1) )
summary(two.lm)
coeftest(two.lm, vcov = vcovHC(two.lm, type="HC1"))
coeftest(two.lm, vcov = NeweyWest(two.lm))

three.lm = lm(tuna.cpue ~ PRI_dummy*HAWAIITUNA + +factor(SET_YEAR) + WCPFC_close + IATTC_close + experiment + nino3 + lag1nino3 + lag2nino3 + lag3nino3 + factor(SET_MONTH) + factor(vessel_id), data=subset(NOAA.raw, SET_YEAR>=2010 & PMNM_dummy==0 & HAWAII==1) )
summary(three.lm)
coeftest(three.lm, vcov = vcovHC(three.lm, type="HC1"))

four.lm = lm(tuna.cpue ~ PMNM_dummy + PMNM_dummy*HAWAIITUNA, data=subset(NOAA.raw, SET_YEAR>=2010 & sword==0) )
summary(four.lm)
coeftest(four.lm, vcov = vcovHC(four.lm, type="HC1")) 
# fourth root of (365*8=2920) is 7.35
bgtest(four.lm, order = 7)
dwtest(four.lm)
coeftest(four.lm, vcov = NeweyWest(four.lm, lag = 30))
coeftest(four.lm, vcov = NeweyWest(four.lm, lag = 10))
coeftest(four.lm, vcov = NeweyWest(four.lm, lag = 9))
coeftest(four.lm, vcov = NeweyWest(four.lm, lag = 7))
coeftest(four.lm, vcov = NeweyWest(four.lm, lag = 6))

five.lm = lm(tuna.cpue ~ PMNM_dummy + PMNM_dummy*HAWAIITUNA + factor(SET_MONTH) +factor(SET_YEAR), data=subset(NOAA.raw, SET_YEAR>=2010 & sword==0) )
summary(five.lm)
coeftest(five.lm, vcov = vcovHC(five.lm, type="HC1")) 
coeftest(five.lm, vcov = NeweyWest(five.lm))

six.lm = lm(tuna.cpue ~ PMNM_dummy + PMNM_dummy*HAWAIITUNA + factor(SET_YEAR) + WCPFC_close + IATTC_close + experiment + nino3 + lag1nino3 + lag2nino3 + lag3nino3 + factor(SET_MONTH) + factor(vessel_id), data=subset(NOAA.raw, SET_YEAR>=2010 & sword==0) )
summary(six.lm)
coeftest(six.lm, vcov = vcovHC(six.lm, type="HC1"))
#coeftest(six.lm, vcov = NeweyWest(six.lm))

robust_se <- list(sqrt(diag(vcovHC(one.lm, type = "HC1"))),
                  sqrt(diag(vcovHC(two.lm, type = "HC1"))),
                  sqrt(diag(vcovHC(three.lm, type = "HC1"))),
                  sqrt(diag(vcovHC(four.lm, type = "HC1"))),
                  sqrt(diag(vcovHC(five.lm, type = "HC1"))),
                  sqrt(diag(vcovHC(six.lm, type = "HC1"))))

star <- stargazer(one.lm, two.lm, three.lm, four.lm, five.lm, six.lm, title="Diference-in-Differences Estimation",
          intercept.bottom = FALSE, label="tab:PolicyForumDID",
          dep.var.labels   = "Catch of Bigeye and Yellowfin Tuna per 1,000 Hooks",
          se        = robust_se,
          add.lines = list(c("Month Dummies", "No", "Yes", "Yes","No", "Yes", "Yes"),c("Year Dummies", "No", "Yes", "Yes","No", "Yes", "Yes"), c("Vessel Dummies", "No", "No","Yes","No", "No","Yes"), c("Additional Controls", "No", "No","Yes","No", "No","Yes")),
          omit.stat=c("LL","ser","f","adj.rsq"), no.space=TRUE, keep = c("PRI_dummy","HAWAIITUNA","PRI_dummy*HAWAIITUNA","PMNM_dummy","PMNM_dummy*HAWAIITUNA"),
 #         notes.label = "Notes:",
          notes.label = NULL,
           # notes        = "Sometimes you just have to start over.",
          notes = "\\parbox[t]{7in}{Notes: In Columns (1)-(3), the control group is Hawaii-based swordfish trips and the sample runs from January 1st 2010 to August 25th 2016. In Columns (4)-(6), the control group is American Samoa-based tuna trips and the sample runs from January 1st 2010 to December 31st 2017. Heteroskedasticity-robust standard errors presented in parentheses. The Additional Controls are whether the set included an experimental component, a dummy variable for whether the WCPFC waters were closed to fishing, a dummy variable for whether IATTC waters were closed to vessels longer than 24m, Monthly El Nino indicator, Monthly El Nino indicator lagged by one year, Monthly El Nino indicator lagged by two years, and Monthly El Nino indicator lagged by three years. $^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01 for two-sided t-test of statistical significance using heteroskedasticity-robust standard errors.}",
          notes.append = FALSE,    
          notes.align = "l",
          covariate.labels = c("PRI Expansion","PMNM Expansion","Hawaii-based Tuna Trips","PRI * Hawaii","PMNM * Hawaii"),
          out="/Volumes/GoogleDrive/My Drive/Large MPA Data Depository/drafts/PolicyForumDID.tex")

rm(one.lm,two.lm,three.lm,four.lm,five.lm,six.lm,robust_se)

#################################
## Diff-in-Diff for Catch per Set
#################################

one.lm = lm(tuna ~ PRI_dummy*HAWAIITUNA, data=subset(NOAA.raw, SET_YEAR>=2010 & PMNM_dummy==0 & HAWAII==1) )
summary(one.lm)
coeftest(one.lm, vcov = vcovHC(one.lm, type="HC1"))

two.lm = lm(tuna ~ PRI_dummy*HAWAIITUNA + factor(SET_MONTH) + factor(SET_YEAR), data=subset(NOAA.raw, SET_YEAR>=2010 & PMNM_dummy==0 & HAWAII==1) )
summary(two.lm)
coeftest(two.lm, vcov = vcovHC(two.lm, type="HC1"))

three.lm = lm(tuna ~ PRI_dummy*HAWAIITUNA + WCPFC_close + IATTC_close + experiment + nino3 + lag1nino3 + lag2nino3 + lag3nino3 + factor(SET_MONTH) + factor(SET_YEAR) + factor(vessel_id), data=subset(NOAA.raw, SET_YEAR>=2010 & PMNM_dummy==0 & HAWAII==1) )
summary(three.lm)
coeftest(three.lm, vcov = vcovHC(three.lm, type="HC1"))

four.lm = lm(tuna ~ PMNM_dummy + PMNM_dummy*HAWAIITUNA, data=subset(NOAA.raw, SET_YEAR>=2010 & sword==0) )
summary(four.lm)
coeftest(four.lm, vcov = vcovHC(four.lm, type="HC1")) 

five.lm = lm(tuna ~ PMNM_dummy + PMNM_dummy*HAWAIITUNA + factor(SET_MONTH) + factor(SET_YEAR), data=subset(NOAA.raw, SET_YEAR>=2010 & sword==0) )
summary(five.lm)
coeftest(five.lm, vcov = vcovHC(five.lm, type="HC1")) 

six.lm = lm(tuna ~ PMNM_dummy + PMNM_dummy*HAWAIITUNA + WCPFC_close + IATTC_close + experiment + nino3 + lag1nino3 + lag2nino3 + lag3nino3 + factor(SET_MONTH) + factor(SET_YEAR) + factor(vessel_id), data=subset(NOAA.raw, SET_YEAR>=2010 & sword==0) )
summary(six.lm)
coeftest(six.lm, vcov = vcovHC(six.lm, type="HC1"))

robust_se <- list(sqrt(diag(vcovHC(one.lm, type = "HC1"))),
                  sqrt(diag(vcovHC(two.lm, type = "HC1"))),
                  sqrt(diag(vcovHC(three.lm, type = "HC1"))),
                  sqrt(diag(vcovHC(four.lm, type = "HC1"))),
                  sqrt(diag(vcovHC(five.lm, type = "HC1"))),
                  sqrt(diag(vcovHC(six.lm, type = "HC1"))))

star <- stargazer(one.lm, two.lm, three.lm, four.lm, five.lm, six.lm, title="Diference-in-Differences Estimation of Catch per Set",
                  intercept.bottom = FALSE, label="tab:DIDcatch_set",
                  dep.var.labels   = "Catch of Bigeye and Yellowfin Tuna per Fishing Set",
                  se        = robust_se,
                  add.lines = list(c("Month Dummies", "No", "Yes", "Yes","No", "Yes", "Yes"),c("Year Dummies", "No", "Yes", "Yes","No", "Yes", "Yes"), c("Vessel Dummies", "No", "No","Yes","No", "No","Yes"), c("Additional Controls", "No", "No","Yes","No", "No","Yes")),
                  omit.stat=c("LL","ser","f","adj.rsq"), no.space=TRUE, keep = c("PRI_dummy","HAWAII","PRI_dummy*HAWAII","PMNM_dummy","PMNM_dummy*HAWAII"),
                  notes = "\\parbox[t]{6in}{Notes: In Columns (1)-(3), the control group is Hawaii-based swordfish trips and the sample runs from January 1st 2010 to August 25th 2016. In Columns (4)-(6), the control group is American Samoa-based tuna trips and the sample runs from January 1st 2010 to December 31st 2017. Heteroskedasticity-robust standard errors presented in parentheses. The Additional Controls are whether the set included an experimental component, a dummy variable for whether the WCPFC waters were closed to fishing, a dummy variable for whether IATTC waters were closed to vessels longer than 24m, Monthly El Nino indicator, Monthly El Nino indicator lagged by one year, Monthly El Nino indicator lagged by two years, and Monthly El Nino indicator lagged by three years. $^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01}",
                  notes.append = FALSE,    
                  notes.align = "l",
                  covariate.labels = c("PRI Expansion","PMNM Expansion","Hawaii-based Tuna Trips","PRI * Hawaii","PMNM * Hawaii"),
                  out="/Volumes/GoogleDrive/My Drive/Large MPA Data Depository/drafts/DIDcatch_set.tex")

rm(one.lm,two.lm,three.lm,four.lm,five.lm,six.lm,robust_se)

##################################
## Diff-in-Diff for Catch per Trip
##################################

one.lm = lm(tuna ~ PRI_dummy*HAWAIITUNA, data=subset(trips.data, SET_YEAR>=2010 & PMNM_dummy==0 & HAWAII==1) )
summary(one.lm)
coeftest(one.lm, vcov = vcovHC(one.lm, type="HC1"))

two.lm = lm(tuna ~ PRI_dummy*HAWAIITUNA + factor(SET_MONTH) + factor(SET_YEAR), data=subset(trips.data, SET_YEAR>=2010 & PMNM_dummy==0 & HAWAII==1) )
summary(two.lm)
coeftest(two.lm, vcov = vcovHC(two.lm, type="HC1"))

three.lm = lm(tuna ~ PRI_dummy*HAWAIITUNA + WCPFC_close + IATTC_close + experiment + nino3 + lag1nino3 + lag2nino3 + lag3nino3 + factor(SET_MONTH) + factor(SET_YEAR) + factor(vessel_id), data=subset(trips.data, SET_YEAR>=2010 & PMNM_dummy==0 & HAWAII==1) )
summary(three.lm)
coeftest(three.lm, vcov = vcovHC(three.lm, type="HC1"))

four.lm = lm(tuna ~ PMNM_dummy + PMNM_dummy*HAWAIITUNA, data=subset(trips.data, SET_YEAR>=2010 & sword==0 & dec.2017==0) )
summary(four.lm)
coeftest(four.lm, vcov = vcovHC(four.lm, type="HC1")) 

five.lm = lm(tuna ~ PMNM_dummy + PMNM_dummy*HAWAIITUNA + factor(SET_MONTH) + factor(SET_YEAR), data=subset(trips.data, SET_YEAR>=2010 & sword==0 & dec.2017==0) )
summary(five.lm)
coeftest(five.lm, vcov = vcovHC(five.lm, type="HC1")) 

six.lm = lm(tuna ~ PMNM_dummy + PMNM_dummy*HAWAIITUNA + WCPFC_close + IATTC_close + experiment + nino3 + lag1nino3 + lag2nino3 + lag3nino3 + factor(SET_MONTH) + factor(SET_YEAR) + factor(vessel_id), data=subset(trips.data, SET_YEAR>=2010 & sword==0 & dec.2017==0) )
summary(six.lm)
coeftest(six.lm, vcov = vcovHC(six.lm, type="HC1"))

robust_se <- list(sqrt(diag(vcovHC(one.lm, type = "HC1"))),
                  sqrt(diag(vcovHC(two.lm, type = "HC1"))),
                  sqrt(diag(vcovHC(three.lm, type = "HC1"))),
                  sqrt(diag(vcovHC(four.lm, type = "HC1"))),
                  sqrt(diag(vcovHC(five.lm, type = "HC1"))),
                  sqrt(diag(vcovHC(six.lm, type = "HC1"))))

star <- stargazer(one.lm, two.lm, three.lm, four.lm, five.lm, six.lm, title="Diference-in-Differences Estimation of Catch per Trip",
                  intercept.bottom = FALSE, label="tab:DIDcatch_trip",
                  dep.var.labels   = "Catch of Bigeye and Yellowfin Tuna per Fishing Trip",
                  se        = robust_se,
                  add.lines = list(c("Month Dummies", "No", "Yes", "Yes","No", "Yes", "Yes"),c("Year Dummies", "No", "Yes", "Yes","No", "Yes", "Yes"), c("Vessel Dummies", "No", "No","Yes","No", "No","Yes"), c("Additional Controls", "No", "No","Yes","No", "No","Yes")),
                  omit.stat=c("LL","ser","f","adj.rsq"), no.space=TRUE, keep = c("PRI_dummy","HAWAIITUNA","PRI_dummy*HAWAIITUNA","PMNM_dummy","PMNM_dummy*HAWAIITUNA"),
                  notes = "\\parbox[t]{7in}{Notes: In Columns (1)-(3), the control group is Hawaii-based swordfish trips and the sample runs from January 1st 2010 to August 25th 2016. In Columns (4)-(6), the control group is American Samoa-based tuna trips and the sample runs from January 1st 2010 to December 31st 2017. Heteroskedasticity-robust standard errors presented in parentheses. The Additional Controls are whether the set included an experimental component, a dummy variable for whether the WCPFC waters were closed to fishing, a dummy variable for whether IATTC waters were closed to vessels longer than 24m, Monthly El Nino indicator, Monthly El Nino indicator lagged by one year, Monthly El Nino indicator lagged by two years, and Monthly El Nino indicator lagged by three years. $^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01}",
                  notes.append = FALSE,    
                  notes.align = "l",
                  covariate.labels = c("PRI Expansion","PMNM Expansion","Hawaii-based Tuna Trips","PRI * Hawaii","PMNM * Hawaii"),
                  out="/Volumes/GoogleDrive/My Drive/Large MPA Data Depository/drafts/DIDcatch_trip.tex")

rm(one.lm,two.lm,three.lm,four.lm,five.lm,six.lm,robust_se)

################################
## Diff-in-Diff for Catch per Km
################################

one.lm = lm(tuna_km ~ PRI_dummy*HAWAIITUNA, data=subset(trips.data, SET_YEAR>=2010 & PMNM_dummy==0 & HAWAII==1) )
summary(one.lm)
coeftest(one.lm, vcov = vcovHC(one.lm, type="HC1"))

two.lm = lm(tuna_km ~ PRI_dummy*HAWAIITUNA + factor(SET_MONTH) + factor(SET_YEAR), data=subset(trips.data, SET_YEAR>=2010 & PMNM_dummy==0 & HAWAII==1) )
summary(two.lm)
coeftest(two.lm, vcov = vcovHC(two.lm, type="HC1"))

three.lm = lm(tuna_km ~ PRI_dummy*HAWAIITUNA + WCPFC_close + IATTC_close + experiment + nino3 + lag1nino3 + lag2nino3 + lag3nino3 + factor(SET_MONTH) + factor(SET_YEAR) + factor(vessel_id), data=subset(trips.data, SET_YEAR>=2010 & PMNM_dummy==0 & HAWAII==1) )
summary(three.lm)
coeftest(three.lm, vcov = vcovHC(three.lm, type="HC1"))

four.lm = lm(tuna_km ~ PMNM_dummy + PMNM_dummy*HAWAIITUNA, data=subset(trips.data, SET_YEAR>=2010 & sword==0 & dec.2017==0) )
summary(four.lm)
coeftest(four.lm, vcov = vcovHC(four.lm, type="HC1")) 

five.lm = lm(tuna_km ~ PMNM_dummy + PMNM_dummy*HAWAIITUNA + factor(SET_MONTH) + factor(SET_YEAR), data=subset(trips.data, SET_YEAR>=2010 & sword==0 & dec.2017==0) )
summary(five.lm)
coeftest(five.lm, vcov = vcovHC(five.lm, type="HC1")) 

six.lm = lm(tuna_km ~ PMNM_dummy + PMNM_dummy*HAWAIITUNA + WCPFC_close + IATTC_close + experiment + nino3 + lag1nino3 + lag2nino3 + lag3nino3 + factor(SET_MONTH) + factor(SET_YEAR) + factor(vessel_id), data=subset(trips.data, SET_YEAR>=2010 & sword==0 & dec.2017==0) )
summary(six.lm)
coeftest(six.lm, vcov = vcovHC(six.lm, type="HC1"))

robust_se <- list(sqrt(diag(vcovHC(one.lm, type = "HC1"))),
                  sqrt(diag(vcovHC(two.lm, type = "HC1"))),
                  sqrt(diag(vcovHC(three.lm, type = "HC1"))),
                  sqrt(diag(vcovHC(four.lm, type = "HC1"))),
                  sqrt(diag(vcovHC(five.lm, type = "HC1"))),
                  sqrt(diag(vcovHC(six.lm, type = "HC1"))))

star <- stargazer(one.lm, two.lm, three.lm, four.lm, five.lm, six.lm, title="Diference-in-Differences Estimation of Catch per Kilometer Traveled",
                  intercept.bottom = FALSE, label="tab:DIDcatch_km",
                  dep.var.labels   = "Catch of Bigeye and Yellowfin Tuna per Kilometer Traveled",
                  se        = robust_se,
                  add.lines = list(c("Month Dummies", "No", "Yes", "Yes","No", "Yes", "Yes"),c("Year Dummies", "No", "Yes", "Yes","No", "Yes", "Yes"), c("Vessel Dummies", "No", "No","Yes","No", "No","Yes"), c("Additional Controls", "No", "No","Yes","No", "No","Yes")),
                  omit.stat=c("LL","ser","f","adj.rsq"), no.space=TRUE, keep = c("PRI_dummy","HAWAIITUNA","PRI_dummy*HAWAIITUNA","PMNM_dummy","PMNM_dummy*HAWAIITUNA"),
                  notes = "\\parbox[t]{6in}{Notes: In Columns (1)-(3), the control group is Hawaii-based swordfish trips and the sample runs from January 1st 2010 to August 25th 2016. In Columns (4)-(6), the control group is American Samoa-based tuna trips and the sample runs from January 1st 2010 to December 31st 2017. Heteroskedasticity-robust standard errors presented in parentheses. The Additional Controls are whether the set included an experimental component, a dummy variable for whether the WCPFC waters were closed to fishing, a dummy variable for whether IATTC waters were closed to vessels longer than 24m, Monthly El Nino indicator, Monthly El Nino indicator lagged by one year, Monthly El Nino indicator lagged by two years, and Monthly El Nino indicator lagged by three years. $^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01}",
                  notes.append = FALSE,    
                  notes.align = "l",
                  covariate.labels = c("PRI Expansion","PMNM Expansion","Hawaii-based Tuna Trips","PRI * Hawaii","PMNM * Hawaii"),
                  out="/Volumes/GoogleDrive/My Drive/Large MPA Data Depository/drafts/DIDcatch_km.tex")

rm(one.lm,two.lm,three.lm,four.lm,five.lm,six.lm,robust_se)

################################
## Why are we getting this negative catch per trip result when everything else is positive?
################################

# Is it because of hooks? Yes

one.lm = lm(num_hks_set ~ PMNM_dummy + PMNM_dummy*HAWAIITUNA + WCPFC_close + IATTC_close + experiment + nino3 + lag1nino3 + lag2nino3 + lag3nino3 + factor(SET_MONTH) + factor(SET_YEAR) + factor(vessel_id), data=subset(trips.data, SET_YEAR>=2010 & sword==0 & dec.2017==0) )
summary(one.lm)
coeftest(one.lm, vcov = vcovHC(one.lm, type="HC1"))

# Is it because of sets? Yes

two.lm = lm(total_sets ~ PMNM_dummy + PMNM_dummy*HAWAIITUNA + WCPFC_close + IATTC_close + experiment + nino3 + lag1nino3 + lag2nino3 + lag3nino3 + factor(SET_MONTH) + factor(SET_YEAR) + factor(vessel_id), data=subset(trips.data, SET_YEAR>=2010 & sword==0 & dec.2017==0) )
summary(two.lm)
coeftest(two.lm, vcov = vcovHC(two.lm, type="HC1"))

# Is it because of trip length? Yes

three.lm = lm(trip_length ~ PMNM_dummy + PMNM_dummy*HAWAIITUNA + WCPFC_close + IATTC_close + experiment + nino3 + lag1nino3 + lag2nino3 + lag3nino3 + factor(SET_MONTH) + factor(SET_YEAR) + factor(vessel_id), data=subset(trips.data, SET_YEAR>=2010 & sword==0 & dec.2017==0) )
summary(three.lm)
coeftest(three.lm, vcov = vcovHC(three.lm, type="HC1"))

robust_se <- list(sqrt(diag(vcovHC(one.lm, type = "HC1"))),
                  sqrt(diag(vcovHC(two.lm, type = "HC1"))),
                  sqrt(diag(vcovHC(three.lm, type = "HC1"))))

stargazer(one.lm, two.lm, three.lm, title="Diference-in-Differences: Fishing Effort per Trip",
          intercept.bottom = FALSE, label="tab:DIDeffort_trip",
          dep.var.labels   = c("Total Hooks","Total Sets","Days at Sea"),
          se        = robust_se,
          add.lines = list(c("Month Dummies", "Yes", "Yes", "Yes"), c("Vessel Dummies", "Yes","Yes","Yes"), c("Additional Controls", "Yes", "Yes","Yes")),
          omit.stat=c("LL","ser","f","adj.rsq"), no.space=TRUE, keep = c("PMNM_dummy","HAWAII","PMNM_dummy*HAWAII","Constant"),
          notes.label = NULL,
          # notes        = "Sometimes you just have to start over.",
          notes = "\\parbox[t]{5in}{Notes: The dependent variable in Column (1) is total hooks deployed per trip. In Column (2) it is total sets per trip and in Column (3) it is the length of the trip in days. The sample runs from January 1st 2010 to December 31st 2017. Heteroskedasticity-robust standard errors presented in parentheses. The Additional Controls are whether the set included an experimental component, a dummy variable for whether the WCPFC waters were closed to fishing, a dummy variable for whether IATTC waters were closed to vessels longer than 24m, Monthly El Nino indicator, Monthly El Nino indicator lagged by one year, Monthly El Nino indicator lagged by two years, and Monthly El Nino indicator lagged by three years. $^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01}",      notes.append = FALSE,    
          notes.align = "l",
          covariate.labels = c("Constant","PMNM Expansion","Hawaii-based Tuna Trips","PMNM * Hawaii"),
          
          out="/Volumes/GoogleDrive/My Drive/Large MPA Data Depository/drafts/DIDeffort_trip.tex")
