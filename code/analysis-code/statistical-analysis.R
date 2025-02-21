#load packages
install.packages("table1")
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

##data cleaning.


#select variables in both the data sets and later merge them together.

gender_data<-GENDER_DEUTERIUM_DILUTION_DATA%>%
  select(`Participant ID`, Sex, `Participant age`,BMI, Dose,`Fat in kg`,`LBM in kg`,
          `Fat %`, `LBM in %`)
         
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

#dropping the last observation from the dataset since the last observation
#doesnt make sense to our annalysis 
#loading the data
data<- read_rds(data_location)

# dropping the last observation in the dataset since it makes no sense
# to our annalysis and also dropping the variable dose since we dont really need it.
data<- data[-61,-5] #dropped already. 

#Lets categorise age so we can have a clear picture of what is happening.
#first lets look at its 
summary(data$`Participant age`)
Final_data <- data %>%
  mutate(Age_cat = case_when(
    `Participant age` >= 17 & `Participant age` < 28 ~ 0,
    `Participant age` >= 28 & `Participant age` < 40 ~ 1,
    `Participant age` >= 40 & `Participant age` < 60 ~ 2,
    `Participant age` >= 60  ~ 3,  # for all graeter than 60
    TRUE ~ NA_real_  # Handles missing or unexpected values
  ))



##eXPLORATION OF OUR DATA.
#summary of the data
summary(Final_data)
#mean age is of the participants is 30, minimum age is 17 while the maax is 70

#saving the cleaned data
save_data_location <- here::here("data","processed-data","Final_data.rds")
saveRDS(Final_data, file = save_data_location)

table(Final_data$Sex) #There are 30 males and 30 females
table(Final_data$Age_cat)

##VISUALIZING THE VARIABLES IN THE Final_data
# creating a hists 
hist(Final_data$`CD4 Immune activation count`, main="Histogram of CD4 Counts",
     xlab="CD4 counts", col="blue") #histogram showing the cd4 counts

#CD4+
ggplot(Final_data, aes(x=`CD4+`)) + geom_histogram(binwidth=10, fill="blue", 
                                             color="black")

#LBM IN KG
ggplot(Final_data, aes(x=`LBM in kg`)) + geom_histogram(binwidth=10, fill="red", 
                                             color="black")
##sexDistributions
ggplot(Final_data, aes(x=factor(Sex), y= `LBM in kg`)) +
  geom_boxplot(fill="lightblue") +
  theme_minimal()

##checking for any correlations

library(corrplot)
cor_matrix <- cor(Final_data[sapply(Final_data, is.numeric)], use="complete.obs")
print(cor_matrix)

corrplot(cor_matrix, method="color", tl.cex=0.8)
#We see that none of the variabls are correlated with each other.

#Scatter plots of numeric variables
p1<-ggplot(Final_data, aes(x=`LBM in kg`
                 , y=`CD4+`)) + 
  geom_point(color="blue") + 
  geom_smooth(method="lm", col="red") +
  theme_minimal()
plot(p1)

figure_file = here("results","figures","Scatterplot1.png")
ggsave(filename = figure_file, plot=p1, width = 7, height = 5, dpi = 300)


##considering immune counts
ggplot(Final_data, aes(x=`LBM in kg`
                 , y=`CD4 Immune activation count`)) + 
  geom_point(color="blue") + 
  geom_smooth(method="lm", col="red") +
  theme_minimal()



#comparing fat to cd4
p2<-ggplot(Final_data, aes(x= `Fat in kg`
                 , y=`CD4+`)) + 
  geom_point(color="black") + 
  geom_smooth(method="lm", col="green") +
  theme_minimal()
plot(p2)
figure_file = here("results","figures","Scatterplot2.png")
ggsave(filename = figure_file, plot=p2, width = 7, height = 5, dpi = 300)
##immune activation counts
ggplot(Final_data, aes(x= `Fat in kg`
                 , y=`CD4 Immune activation count`)) + 
  geom_point(color="black") + 
  geom_smooth(method="lm", col="green") +
  theme_minimal()

##### other visualisations

p3<-ggplot(Final_data, aes(x = factor(Sex), y = `CD4 Immune activation count`, 
                 fill = factor(Sex))) +
  geom_boxplot() +
  labs(x = "Sex", y = "CD4 Count", title = "CD4 Count Distribution by Sex") +
  theme_minimal()
plot(p3)
figure_file = here("results","figures","boxplot1.png")
ggsave(filename = figure_file, plot=p3, width = 7, height = 5, dpi = 300)



p4<-ggplot(Final_data, aes(x = Sex, y = `Fat in kg`, fill = Sex)) +
  geom_boxplot() +
  theme_minimal() +
  ggtitle("Fat in kg by Sex")
plot(p4)
figure_file = here("results","figures","boxplot2.png")
ggsave(filename = figure_file, plot=p4, width = 7, height = 5, dpi = 300)

##Mean cd4 count by sex
ggplot(Final_data, aes(x = factor(Sex), y = `CD4 Immune activation count`, fill = factor(Sex))) +
  stat_summary(fun = mean, geom = "bar") +
  labs(x = "Sex", y = "Mean CD4 Count", title = "Mean CD4 Count by Sex") +
  theme_minimal()

#####RUNNING MODELS ON THE Final_data.
model <- lm(`CD4 Immune activation count` ~ relevel(factor(Sex), ref = "M")+ `Participant age`+ `Fat in kg` +`LBM in kg`, data = Final_data)
summary(model)

# Regression plot
ggplot(Final_data, aes(x = factor(Sex), y = `CD4 Immune activation count`, color = factor(Sex))) +
  geom_jitter(width = 0.2, alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Sex", y = "CD4 Count", title = "CD4 Count vs. Sex (Regression)") +
  theme_minimal()


####TABLES
######making a table with sex as our exposure.
#installing the required packages for the tables.
# Load required packages
install.packages("table1")  # Install if not already installed
install.packages("gtsummary")
library(table1)
library(gtsummary)
library(plotly)
library(ggtext)
#descriptive statistics table with Sex as the exposure:
TABLE1<-table1(~ `CD4+`+`CD4 Immune activation count` + `Participant age` + `Fat in kg` 
       + `LBM in kg` | Sex, data = Final_data)
TABLE1
save_tabel1<- here::here("results","tables", "TABLE1.rds")
saveRDS(TABLE1,file =save_tabel1 )


# t-tests to compare means between sexes
t.test(`CD4 Immune activation count` ~ Sex, data = Final_data)
t.test(`Fat in kg`~ Sex, data = Final_data)

#Final_data %>%
 # select(Sex,`CD4+`, `CD4 Immune activation count`, `Participant age`, 
   #      `Fat in kg`, `LBM in kg`) %>%
  #tbl_summary(by = Sex, 
   ##           statistic = list(all_continuous() ~ "{mean} ({sd})"), 
     #         missing = "no") %>%
  #add_p() %>%
  #modify_header(label = "**Variable**") %>%
  #modify_spanning_header(c("stat_1", "stat_2") ~ "**Sex Groups**") 
#bold_labels()


  
