### Catch Bar Chart (or Line Graph)
rm(list=ls())

setwd("/Users/lynham/Documents/CSF MPA Project/")
library(ggplot2)
library(plotly)
library(readxl)
library(webshot)
library(grid)
library(gridExtra)
library(scales)

annual.catch <- read_excel("/Volumes/GoogleDrive/My Drive/Large MPA Data Depository/NOAA & Council Data/noaa_logbook_data.xlsx", 
                                sheet = "tidy_data")

annual.catch$catch.hooks <- (annual.catch$catch/annual.catch$hooks)*1000
annual.catch$catch.sets <- (annual.catch$catch/annual.catch$sets)
annual.catch$catch.trips <- (annual.catch$catch/annual.catch$trips)

ggplot(data=annual.catch, aes(year, y=catch,fill = species)) +
  geom_col()+
  theme_minimal() + 
  scale_y_continuous(labels = comma) +
  labs(x = "Year",y="Numbers Caught",fill = "Species") +theme(text = element_text(size=20))+
  scale_fill_viridis_d(labels = c("Bigeye Tuna", "Other Tuna Species","Swordfish"))

pdf(file ="/Volumes/GoogleDrive/My Drive/Large MPA Data Depository/drafts/figures/annual_catch.pdf")
ggplot(data=annual.catch, aes(year, y=catch,fill = species)) +
geom_col()+
  theme_minimal() + 
  scale_y_continuous(labels = comma) +
  labs(x = "Year",y="Numbers Caught",fill = "Species") +theme(text = element_text(size=20))+
  scale_fill_viridis_d(labels = c("Bigeye Tuna", "Other Tuna Species","Swordfish"))
dev.off()


p1 <- ggplot(data=annual.catch, aes(year, y=catch.hooks,fill = species)) +
  geom_col()+
  theme_minimal() + labs(x = "Year",y="Catch per 1,000 Hooks",fill = "Species") +theme(text = element_text(size=20))+
  scale_fill_viridis_d(labels = c("Bigeye Tuna", "Other Tuna Species","Swordfish"))

p2 <- ggplot(data=annual.catch, aes(year, y=catch.sets,fill = species)) +
  geom_col()+
  theme_minimal() + labs(x = "Year",y="Catch per Fishing Set",fill = "Species") +theme(text = element_text(size=20))+
  scale_fill_viridis_d(labels = c("Bigeye Tuna", "Other Tuna Species","Swordfish"))

p3 <- ggplot(data=annual.catch, aes(year, y=catch.trips,fill = species)) +
  geom_col()+
  theme_minimal() + labs(x = "Year",y="Catch per Fishing Trip",fill = "Species") +theme(text = element_text(size=20))+
  scale_fill_viridis_d(labels = c("Bigeye Tuna", "Other Tuna Species","Swordfish"))

pdf(file ="/Volumes/GoogleDrive/My Drive/Large MPA Data Depository/drafts/figures/annual_cpue.pdf", width = 20, height = 5)
grid.arrange(p1, p2, p3, ncol = 3)
dev.off()



