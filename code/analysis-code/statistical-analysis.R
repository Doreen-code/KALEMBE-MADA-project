#load packages
library("readxl")
library("readr")
library("dplyr")
library("ggplot2")
library("tidyverse")
library("here")
# load the data sets
Immunology_lab_results <- read_excel("data/raw-data/Immunology lab results.xlsx")
View(Immunology_lab_results)
GENDER_DEUTERIUM_DILUTION_DATA <- read_excel("data/raw-data/GENDER DEUTERIUM DILUTION DATA.xlsx")
View(GENDER_DEUTERIUM_DILUTION_DATA)

#select variables in both the data sets and later merge them together.

gender_data<-GENDER_DEUTERIUM_DILUTION_DATA%>%
  select(`Participant ID`, Sex, `Participant age`,BMI, Dose,`Fat in kg`)
gender_data<-gender_data%>%
  rename("ID"= `Participant ID`)

#select variables you will need in the next data set
immune_data<-Immunology_lab_results%>%
  select(ID,`CD4+`, `CD4 Immune activation count`)

#we can now merge the two data sets int one.
Tb_immune_data<-full_join(gender_data, immune_data,by = "ID")

#let me first save this data set. i will save this data in the processed data
# folder since this is the data that i have cleaned and plan on using through
# out the annalysis.

save_data_location <- here::here("data","processed-data","Tb_immune_data.rds")
saveRDS(Tb_immune_data, file = save_data_location)

