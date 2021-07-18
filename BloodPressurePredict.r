# This project amis to predict Blood Pressure without medical device. 

# Nearly half of adults in the United States (108 million, or 45%) have hypertension (also called high blood pressure (HBP)) (ie., systolic blood pressure ≥ 130 mm Hg). 
# Blood Pressure is an important indicator for predicting health.
# Long-term high blood pressure is a major risk factor for stroke, heart disease, heart failure, vision loss, chronic kidney disease, and dementia.

# To keep track a peron blood pressure, blood pressure guage can be used. However, it required professionals assistance, and most people may not have a blood pressure guage on hands.
# Some people do not even know that they have high blood pressure issue.
# Have a blood pressure AI algorithms to instantly estimated their blood pressure with simple parameters, such age, BMI, Hours of Sleep, etc. could provide an early alert and attention on their health. If there is double, they should go for a proper checkup and medical follow-up.

if(!require(readr)) install.packages("readr", repos = "https://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse", repos = "https://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "https://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "https://cran.us.r-project.org")
if(!require(corrplot)) install.packages("corrplot", repos = "https://cran.us.r-project.org")
if(!require(RColorBrewer)) install.packages("RColorBrewer", repos = "https://cran.us.r-project.org")
if(!require(PerformanceAnalytics)) install.packages("PerformanceAnalytics", repos = "https://cran.us.r-project.org")
if(!require(keras)) install.packages("keras", repos = "https://cran.us.r-project.org")
if(!require(MLmetric)) install.packages("MLmetric", repos = "https://cran.us.r-project.org")

library(readr)

#setwd("~/Documents/Bachcode/Course/Data Science edx/nhanes homework")
#NHANES <- read_csv("NHANES.csv")

NHANES <- read.csv(file = 'https://raw.githubusercontent.com/Shreyas3108/house-price-prediction/master/kc_house_data.csv')

head(NHANES) ## take a look at the first rows of the dataset
tail(NHANES) ## take a look at the last rows of the dataset

NHANES[c(1,3,4,7,17,20,21,25, 50, 53,59, 76)]  ## some record are duplicated
NHANES[c(1,4,5,6,7,8),c(1,3,4,7,17,20,21,25, 50, 53,59, 76)] ## take a look at a subset of the dataset

dplyr::glimpse(NHANES)
# or glimpse(NHANES) to see all the variables in the dataset

head(NHANES)  #10,000

# Randomly keep only a record for each ID (person)
#NHANES <- NHANES %>% group_by(ID) %>% sample_n(size = 1)
NHANES <- NHANES %>% distinct(ID,.keep_all=TRUE)


head(NHANES)  #6779 unique user records now
dim(NHANES)


NHANES$AlcoholDay[is.na(NHANES$AlcoholDay)] <- 0
NHANES$PhysActiveDays[is.na(NHANES$PhysActiveDays)] <- 0

original_NHANES <- NHANES

#Data Tidying
library(tidyverse)

#Data wrangling with dplyr
library(dplyr)

select <- dplyr::select

# Size of Dataset
message <- paste("The NHANES dataset contains", dim(NHANES)[1], "unique people record.")
write(message,stdout())

message <-  paste("Each Records has", dim(NHANES)[2], "features.")
write(message,stdout())


NHANESfemale3065 <- NHANES %>% 
  filter(!is.na(BMI), !is.na(BPSysAve), between(Age,18,65), Gender =="female")


NHANESmale3065 <- NHANES %>% 
  filter(!is.na(BMI), !is.na(BPSysAve), between(Age,18,65), Gender =="male")

message <-  paste("Number of Adult Female Aged 18-65 with Blood Pressure and BMI Records : ", dim(NHANESfemale3065)[1])
write(message,stdout())

message <-  paste("Number of Adult Male Aged 18-65 with Blood Pressure and BMI Records : ", dim(NHANESmale3065)[1])
write(message,stdout())



################################################
# Data Visualization
# -Univaritate Statistics
################################################

# Let take a look on BMI to get a quick idea of the overall health status of population

library(ggplot2)

set.seed(2) ## to make the horizontal position of the jitter non-random
give.n <- function(x){
  return(c(y = 50, label = length(x))) 
}

# --Boxplot -- provide us with information on the mean/median values of the data
# the Mean BMI for adult (aged 18-65), with the 1000 samples (for a clearer view), the meam BMI is 27.9, which is considered as overweight.
# BMI ranging between 18.5 and 24.9. Overweight: BMI between 25 and 29.9. Obese: BMI of 30 or higher.
# Age over 65 year old are not considered in this analysis, due to more complicated medications this group of people may have.
# This analysis aims to help Prevention of high blood pressure and chronic diseases.


NHANES %>%
  filter(!is.na(BMI), between(Age,18,65), between(BMI, 10,60)) %>%
  head(NHANES, n=1000) %>%
  ggplot(aes(x="", y=BMI)) +
  geom_boxplot(outlier.shape=NA) + 
  geom_jitter(width=0.3) +
  stat_summary(geom="text", fun=quantile,
               aes(label=sprintf("%1.1f", ..y..)),
               position=position_nudge(x=0.5), size=4.5) +
  annotate("text", x = c(1.5,1.5,1.5,1.5,1.5), y = c(12.5,22,26,30,60), label = c("(minimum)","(25%)","(median)","(75%)","(maximum)"),size=3) #+
  #stat_summary(fun.data = give.n, geom = "text") 

# -- Scatter Plot -- provides us 

#Create scatter plots using geom_point()

# Under the same BMI, Male tends to have higher blood pressure then Female.

NHANES %>%
  filter(!is.na(BMI), !is.na(BPSysAve), between(Age,18,65)) %>%
  #head(NHANES, n=5000) %>%
  ggplot(aes(x = BMI, y = BPSysAve, color = Gender)) +
  geom_point(alpha = 0.5) + 
  xlab("BMI") + 
  ylab("BPSysAve (mmHg)") +
  xlim(10,70) + 
  ylim(80,200)

dim(NHANES)


# We want to see if the dataset certain contains the true mean of the population, by looking into the confidence interval (90%, 95%).

# Let look into healthy group people, compare their Blood Pressure with literatures.
# Here, we assume healthy as being a non-smoker, without a history of diabetes, hard drugs or sleeping trouble, with a general health that is not considered poor and that has a BMI between 18.5 and 25.

# female group is chosen, as we have found female and male blood pressure seems behave a bit differently even under the same BMI.

Healthfemale3065_NHANES <- NHANES %>%
  filter(!is.na(Smoke100n), !is.na(BMI_WHO), !is.na(HardDrugs), !is.na(HealthGen), !is.na(SleepTrouble)) %>%
  filter(!is.na(BPSysAve), between(Age,30,65), Gender == "female") %>% 
  filter(Smoke100n == "Non-Smoker", Diabetes == "No", HardDrugs == "No", HealthGen != "Poor", SleepTrouble == "No", BMI_WHO == "18.5_to_24.9")

dim(Healthfemale3065_NHANES)  #138

# To investigate the CI, we have to see if the data is normally distributed.

# Histogram of Healthy Female Blood Pressure Distribution (mean = 111 mmHg)
Healthfemale3065_NHANES %>%
  ggplot(aes(x=BPSysAve)) +
  geom_histogram(bins = 25) +
  geom_vline(aes(xintercept = mean(x=BPSysAve)), col = "red", lwd = 2) +
  geom_text(aes(x=mean(x=BPSysAve), y=30, label=round(mean(BPSysAve),digits=0)),hjust=-0.5, size=9) +
  ylim(0,20)

# It is quite obvious that it is not normally distributed. Long tail is quite common for health data.
# Let's also see the QQ Plot
# QQplot - to check distribution
Healthfemale3065_NHANES %>%
  ggplot(aes(sample=BPSysAve))+
  geom_qq()+
  geom_qq_line()

#We use the log2 transformation
# QQplot - to check distribution

# QQplot (with Log2 scale) - to check how good is the distribution is normally distrbuted.
Healthfemale3065_NHANES %>%
  ggplot(aes(sample=BPSysAve %>% log2))+
  geom_qq()+
  geom_qq_line()

# The log2 systolic blood pressure for healthy subjects is approximately normally distributed.

## to get the 90% or 95% confidence interval for the healthy group;

summary_NHANES <- Healthfemale3065_NHANES %>%
  mutate(BPSysAveLog2 = BPSysAve %>% log2) %>%
  summarize_at("BPSysAveLog2",
               list(mean=~mean(.,na.rm=TRUE),
                    sd=~sd(.,na.rm=TRUE),
                    n=function(x) x%>%is.na%>%`!`%>%sum)) %>%
  mutate(se=sd/sqrt(n))

summary_NHANES

paste("Mean value (log2):", format(summary_NHANES$mean, digits=3))  # 6.79
paste("Geometric mean value (log2):", format(2^summary_NHANES$mean, digits=4))  # 110.5  (# 95% )
paste("Standard deviation (log2):", format(summary_NHANES$sd, digits=3))  # 0.181

#General Form of 95% Confidnece Interval
#  = sample statistic mean +/- 2*stardard error

paste0("95% Reference interval (in log2 scale): [", format(summary_NHANES$mean - 2*summary_NHANES$sd, digits=3), ";", 
                                                           format(summary_NHANES$mean + 2*summary_NHANES$sd, digits=3), "]")
# [6.4265177613329;7.14932205657843]
paste0("95% Reference interval (mmHg in original scale): [", format(2^(summary_NHANES$mean - 2*summary_NHANES$sd), digits=4), ";", 
                                                                    format(2^(summary_NHANES$mean + 2*summary_NHANES$sd), digits=4), "]")
# [86.0150832141602;141.958168536746]

#General Form of 90% Confidnece Interval
#  = sample statistic mean +/- stardard error

paste0("90% Reference interval (in log2 scale): [", format(summary_NHANES$mean - summary_NHANES$sd, digits=3), ";", 
                                                           format(summary_NHANES$mean + summary_NHANES$sd, digits=3), "]")
# [6.60721883514428;6.96862098276705]
paste0("90% Reference interval (mmHg in original scale): [", format(2^(summary_NHANES$mean - summary_NHANES$sd), digits=4), ";", 
                                                                    format(2^(summary_NHANES$mean + summary_NHANES$sd), digits=4), "]")
# [97.4924663606747;125.24602295366]

#This allows us to set up the (90%) reference interval, for what we can consider to be normal blood pressure values. 

# Note that in the literature a value 125 mmHg for the systolic blood pressure is typically considered 
# to be the upper limit of healthy group in aged of 30-65. (90% CI)

# Note that in the literature a value 125 mmHg for the systolic blood pressure is typically considered 
# to be the upper limit of normality in aged of 30-65. (95% CI)

# The dataset is reasonably representing, when considering 90% and 95% CI.

# Let's continue of analysis

# --Histogram  -- know the distribution and mean of Blood Pressure of All Gender Adult 18-65 (=118mmHg)
NHANES %>%
  filter(!is.na(BPSysAve), between(Age,18,65)) %>%
  #head(NHANES, n=200) %>% ## to make the visualization more clear, we take only the first 100 subjects
  ggplot(aes(x=BPSysAve)) +
  geom_histogram(bins = 30) +
  geom_vline(aes(xintercept = mean(x=BPSysAve)), col = "red", lwd = 2) +
  geom_text(aes(x=mean(x=BPSysAve), y=30, label=round(mean(BPSysAve),digits=0)),hjust=-0.5, size=9)

# --Histogram  -- know the distribution and mean of Blood Pressure of All Gender Adult 18-29 (=113mmHg)
NHANES %>%
  filter(!is.na(BPSysAve), between(Age,18,29)) %>%
  #head(NHANES, n=200) %>% ## to make the visualization more clear, we take only the first 100 subjects
  ggplot(aes(x=BPSysAve)) +
  geom_histogram(bins = 30) +
  geom_vline(aes(xintercept = mean(x=BPSysAve)), col = "red", lwd = 2) +
  geom_text(aes(x=mean(x=BPSysAve), y=30, label=round(mean(BPSysAve),digits=0)),hjust=-0.5, size=9)


# --Histogram  -- know the distribution and mean of Blood Pressure of All Gender Aged 30-65 (=120mmHg)
NHANES %>%
  filter(!is.na(BPSysAve), between(Age,30,65)) %>%
  #head(NHANES, n=200) %>% ## to make the visualization more clear, we take only the first 100 subjects
  ggplot(aes(x=BPSysAve)) +
  geom_histogram(bins = 30) +
  geom_vline(aes(xintercept = mean(x=BPSysAve)), col = "red", lwd = 2) +
  geom_text(aes(x=mean(x=BPSysAve), y=30, label=round(mean(BPSysAve),digits=0)),hjust=-0.5, size=9)

# You can see Blood Pressure is higher for aged.

# Age 18-65 : 118mmHg
# Age 18-29 : 113mmHg
# Age 30-65 : 120mmHg

# --Histogram  -- know the distribution and mean of Blood Pressure of Male Aged 30-65 (=123mmHg)
NHANES %>%
  filter(!is.na(BPSysAve), between(Age,30,65), Gender == "male") %>%
  #head(NHANES, n=200) %>% ## to make the visualization more clear, we take only the first 100 subjects
  ggplot(aes(x=BPSysAve)) +
  geom_histogram(bins = 30) +
  geom_vline(aes(xintercept = mean(x=BPSysAve)), col = "red", lwd = 2) +
  geom_text(aes(x=mean(x=BPSysAve), y=30, label=round(mean(BPSysAve),digits=0)),hjust=-0.5, size=9)


# --Histogram  -- know the distribution and mean of Blood Pressure of Female Aged 30-65 (=117mmHg)
NHANES %>%
  filter(!is.na(BPSysAve), between(Age,30,65), Gender == "female") %>%
  #head(NHANES, n=200) %>% ## to make the visualization more clear, we take only the first 100 subjects
  ggplot(aes(x=BPSysAve)) +
  geom_histogram(bins = 30) +
  geom_vline(aes(xintercept = mean(x=BPSysAve)), col = "red", lwd = 2) +
  geom_text(aes(x=mean(x=BPSysAve), y=30, label=round(mean(BPSysAve),digits=0)),hjust=-0.5, size=9)


#Summary

#Male Aged 30-65 (=123mmHg)
#Female Aged 30-65 (=117mmHg)


# It is make sense to analyse the Blood Pressure by different Gender and Age Group

# Again, our aims is to predict Blood Pressure by using HANDY features of a person.
# Let's focus on Female Aged 30 to 65. And see what HANDY features can be used to predict blood pressure.
# HANDY featurers "Age", "Weight", "Height","BMI", "SleepHrsNight","PhysActiveDays","AlcoholDay"

# filter healthy female aged 30 to 65 --- Healthfemale3065_NHANES

#The correlation heatmap -- to explore the relationship (or the correlation) among all this parameter
library(corrplot)
library(RColorBrewer)

library(dplyr)
select <- dplyr::select

NHANES = original_NHANES 

female3065 <- NHANES %>% 
  filter(Gender == "female") %>%    #female/male , PregnantNow !="Yes"
  filter(between(Age,30,65)) %>%
  filter(is.na(PregnantNow)| PregnantNow != "Yes") %>%
  select("Age", "Weight", "Height","BMI",
         "SleepHrsNight","PhysActiveDays","AlcoholDay","BPSysAve") #%>%
  #filter(!is.na(SleepHrsNight) & !is.na(BPSysAve) & !is.na(BMI))

dim(female3065)  #1446
head(female3065)

select <- dplyr::select

temp <- female3065 %>%
  select(one_of("Age", "Weight", "Height","BMI",
                                 "SleepHrsNight","PhysActiveDays","AlcoholDay","BPSysAve")) %>% as.matrix()
dim(temp)
M <- cor(temp, use = "pairwise.complete.obs")  

# Type 1 -- color and sizes only

corrplot(M, order = "hclust", addrect = 2, type = "lower", col = brewer.pal(8, "PiYG"))

# Type 2 -- with parameters shown

corrplot(M, method = "number")
# form the correlation table, weigth, height and BMI are being significantly correlated to Blood Pressure.
# However, wight, height, BMI also correlated to one another. We would only choose one features among features that can correlating to each other. 
# Therefore, we choose BMI.

# Type 3 -- with graph plotted

library("PerformanceAnalytics")
chart.Correlation(temp, histogram=TRUE, pch=19)


# Deeper Look at EACH features

library(tidyverse)
library(dslabs)

#0. Blood Press

# The Distribution in the 1446 blood pressure sample

p <- ggplot(female3065, aes(x=BPSysAve)) +
  geom_histogram(aes(y=..density..), binwidth = 5, colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666")
p

#1. Age 

qplot(Age, BPSysAve, data = female3065, ylim=c(70,200))

# Let's plot for smoothing curve for a clear idea.
p <- ggplot(female3065, aes(Age, BPSysAve)) +
  geom_point() +
  stat_smooth(span = 0.9, method.args = list(degree=2)) +
  lims(x=c(30, 65), y =c(70,200))
p
#People at age of 30 is having a blood pressure of 113 mmHg. This evalates to almost 130 mmHg at the age of 65.

#2. BMI
qplot(BMI, BPSysAve, data = female3065, ylim=c(70,200), xlim=c(15,65))

p <- ggplot(female3065, aes(BMI, BPSysAve)) +
  #geom_line() +
  geom_point() +
  stat_smooth(span = 0.9, method.args = list(degree=2)) +
  lims(x=c(15, 60), y =c(70,200))
p
# People of BMI = 25 or high is having Blood Pressure of 120 mmHg, the upper limit of healthy range.

# 3. SleepHrsNight
qplot(factor(SleepHrsNight), BPSysAve, data = female3065, 
      geom = c("boxplot", "jitter"), alpha = 0.001,
      ylim =c(80,180))
# People sleeps for 4 to 9 hours are having an average value of lower Blood Pressure closed to 110 mmHg or lower
# Sleeping less than 4 hours or more an 9 hours have an significant increase in the group blood pressure up to almost 125 mmHg.
# This is something we can't observe just from the correlation chart (or the heat map).

# 4. PhysActiveDays
qplot(factor(PhysActiveDays), BPSysAve, data = female3065, 
      geom = c("boxplot", "jitter"), alpha = 0.001,
      ylim =c(80,180))
# Based on the available data, increase Physical Activities does not have much effect on Blood Presure

#5. AlcoholDay
qplot(factor(AlcoholDay), BPSysAve, data = female3065, 
      geom = c("boxplot", "jitter"), alpha = 0.001,
      ylim =c(80,180))
# In general, if number of alcohol taken lis ower per per (1-2 protions per day), the lower the blood pressure.
# Limitation : some of the NA response are those who does not take any alcohol.

table(female3065$AlcoholDay)

# From the analysis, Age, BMI, SleepHrsNight, AlcoholDay are found to affect blood pressure.
# The relationship before some of the features may not in a linear relationship with blood pressure.
# But we would still try to predict blood pressure by Multiple Regression, as a baseline approach
# Besides the Multiple Regression, Keras Artificial Neural Network will also be used.

# Next we try to predict by
# (1) Multiple Regression (a baseline approach)
# (2) Keras ANN

#///////////Multiple Regression ///////////



# Multiple regression generally explains the relationship between multiple independent or predictor variables and one dependent or criterion variable. ... The multiple regression equation explained above takes the following form:   
# y = b1 x1 + b2 x2 + … + bn xn + c.   
# In our case:  
# y = b1 x Age + b2 x BMI + .. + b5 x AlcoholDay + c   

# Multiple Linear Regression
# The purpose of a multiple linear regression is to:
# 1.Determine the size and nature of the coefficient for each feature in explaining the dependent variable.
# 2.Determine the signficance or insignificance of each feature.


# Omit those NA
head(female3065)

female3065_nona <- female3065 %>% 
  select("Age", "BMI", "SleepHrsNight", "AlcoholDay", "PhysActiveDays","BPSysAve")  %>%  #female: "AlcoholDay", #male: "PhysActiveDays",
  na.omit()

dim(female3065)
dim(female3065_nona)

# keep a copy for later use
female3065_nona_original <- female3065_nona

# Split the training and testing set (70:30)


library(caret)
set.seed(2345)

y <- female3065_nona$BPSysAve
test_index <- createDataPartition(y, times = 1, p = 0.1, list = FALSE)


train_set <- female3065_nona %>% slice(-test_index)
test_set <- female3065_nona %>% slice(test_index)

# Get the average value Blood Pressure as a reference
avg <- mean(train_set$BPSysAve)
avg #117.3252
# The average (our guessing is 117.35 mmHg)

# To compare performance of different models, the Root Mean Square Error (RMSE)  is used, 
# as it is more sensitive to large error.
# And the RMSE of guessing with Average is computed:

sqrt(mean((avg - test_set$BPSysAve)^2))  #16.27
#RMSE is 16.3 for guessing with average Blood Pressure.


# Let's see if the Multiple Regression perform better than guessing with average:

head(female3065_nona)
head(train_set)
# Use all the feature first
fit <- lm(BPSysAve ~ Age + BMI + SleepHrsNight + PhysActiveDays + AlcoholDay, data = train_set)  
summary(fit)

# Intercept is 114.98, from the Estimate, the larger the number of the more significants
# of the features. 1 year older means 0.55 mmHg blood pressure higher.
# 1 Alcohol portion means means 0.75 mmHg blood pressure higher, while

fit$coef
fit$coef[1]

# Let's predict the Blood Pressure, and Work out the RMSE
y_pred <- fit$coef[1] + 
  fit$coef[2]*test_set$Age + 
  fit$coef[3]*test_set$BMI +
  fit$coef[4]*test_set$SleepHrsNight +
  fit$coef[5]*test_set$PhysActiveDays +
  fit$coef[6]*test_set$AlcoholDay

# The RMSE
sqrt(mean((y_pred - test_set$BPSysAve)^2)) #15.2

# RMSE is 15.2 for multiple regression analysis, which is not really significantly improve.

# Let's omit the most insignificant feature among those five : PhysActiveDays

fit2 <- lm(BPSysAve ~ Age + BMI + SleepHrsNight + AlcoholDay, data = train_set)  
summary(fit2)

# Intercept is 114.98, from the Estimate, the larger the number of the more significants
# of the features. 1 year older means 0.55 mmHg blood pressure higher.
# 1 Alcohol portion means means 0.75 mmHg blood pressure higher, while

fit2$coef
fit2$coef[1]

# Let's predict the Blood Pressure, and Work out the RMSE
y_pred <- fit2$coef[1] + 
  fit2$coef[2]*test_set$Age + 
  fit2$coef[3]*test_set$BMI +
  fit2$coef[4]*test_set$SleepHrsNight +
  fit2$coef[5]*test_set$AlcoholDay

sqrt(mean((y_pred - test_set$BPSysAve)^2)) # 15.1
# RMSE is 15.1 for multiple regression analysis, which is similar.

#mean(abs(y_pred - test_set$BPSysAve))


# Therefore, we assume Age, BMI, AlcoholDay affect the blood pressure for femaled aged 30 to 65.
# We fit in the model again to get more idea.



# Next, we try Keras ANN

# Taking the findings of both the correlation plots and multiple linear regression into account, 
# Age, BMI, AlcoholDay are kept as the relevant features for the analysis.


########### Keras  NN ###############

#Data Preparation

#library(dplyr)
#select <- dplyr::select

dim(female3065_nona)
female3065_nona <- female3065_nona_original

# Get Rid of Some Obvious overweight or underweight.
female3065_nona <- female3065_nona %>% 
  select("Age", "BMI", "AlcoholDay", "BPSysAve")  %>%  #"SleepHrsNight", 
  filter(between(BMI,18,40)) %>%
  na.omit()

head(female3065_nona) 
dim(female3065_nona) #1355 -->1210

#Neural network are very sensitive to non-normalized data.
# We need to normalize:

#Max-Min Normalization 

normalize <- function(x) {
  min(x)
  max(x)
  return ((x - min(x)) / (max(x) - min(x)))
}

maxmin_female <- as.data.frame(lapply(female3065_nona, normalize))
attach(maxmin_female)
maxmin_female<-as.matrix(maxmin_female)

head(maxmin_female)
dim(maxmin_female)

library(caret)
set.seed(0) 

#The train-test set is split 90/10.
indexx <- sample(2, nrow(maxmin_female), replace=TRUE, prob = c(0.9,0.1))
#head(indexx)

X_train <- maxmin_female[indexx==1, 1:3]  #4
X_test <- maxmin_female[indexx==2, 1:3]
y_train <- maxmin_female[indexx==1, 4]
y_test <- maxmin_female[indexx==2, 4]


#Sequential Model

#----------------------

# Initialize a sequential model: The first step is to initialize a sequential model with keras_model_sequential(), which is the beginning of our Keras model. The sequential model is composed of a linear stack of layers.

# Apply layers to the sequential model: Layers consist of the input layer, hidden layers and an output layer. 

# The input layer is the data and provided it’s formatted correctly there’s nothing more to discuss. The hidden layers and output layers are what controls the ANN inner workings.

# Hidden Layers: Hidden layers form the neural network nodes that enable non-linear activation using weights. The hidden layers are created using layer_dense(). We’ll add two hidden layers. We’ll apply units = 16, which is the number of nodes. We’ll select kernel_initializer = "uniform" and activation = "relu" for both layers. The first layer needs to have the input_shape = 35, which is the number of columns in the training set. Key Point: While we are arbitrarily selecting the number of hidden layers, units, kernel initializers and activation functions, these parameters can be optimized through a process called hyperparameter tuning that is discussed in Next Steps.

#-----------------------


# Input layer: Number of Features + 1
# Hidden layer : Training Data Samples/(Factor * (Input Neurons + Output Neurons))
# Output layer: 1

# A factor of Scaling Factor is set in this case, the purpose of the factor being to prevent overfitting. 
# A factor can take a value between 5 and 10. With 4 neurons in the input layer, 
# 1 neuron in the output layer and ~1000 observations in the training set, 
# the hidden layer is assigned 1000/((5 TO 10) *(4+1)) =  neurons. (20-40) 


# Start RUN here

library(keras)

model <- keras_model_sequential() 
model %>% 
  layer_dense(units = 4, activation = 'relu', kernel_initializer='RandomNormal', input_shape = c(3)) %>% 
  layer_dropout(0.1) %>%
  layer_dense(units = 20, activation = 'relu', kernel_initializer='RandomNormal') %>% 
  layer_dropout(0.1) %>%
  layer_dense(units = 1, activation = 'linear', kernel_initializer='RandomNormal')
summary(model)

# We’re using RMSprop as our optimizer here. RMSprop stands for Root Mean Square Propagation. 
# It’s one of the most popular gradient descent optimization algorithms for deep learning networks. 
# RMSprop is an optimizer that’s reliable and fast.

# Adam realizes the benefits of both AdaGrad and RMSProp.
# Instead of adapting the parameter learning rates based on the average first moment (the mean) as in RMSProp, Adam also makes use of the average of the second moments of the gradients (the uncentered variance).
# https://machinelearningmastery.com/adam-optimization-algorithm-for-deep-learning/

# Optimizing MSE is same as optimizing RMSE
model %>% compile(
  loss = 'mean_squared_error',  #mean_absolute_error, #mean_squared_error  #mean_squared_logarithmic_error
  optimizer = 'adam',  #adam  #RMSprop #sgd
  metrics = c('mse')  #Mean Absolute Error (MAE) #mea
)

#mean_squared_error converge faster in episode 7, also suitable for target value having outliners

#We can see that the model converged reasonably quickly and both train and test performance remained equivalent. The performance and convergence behavior of the model suggest that mean squared error is a good match for a neural network learning this problem.

#We use the fit() function to run the ANN on our training data. The object is our model, and x and y are our training data in matrix and numeric vector forms, respectively. The batch_size = 50 sets the number samples per gradient update within each epoch. We set epochs = 35 to control the number training cycles. Typically we want to keep the batch size high since this decreases the error within each training cycle (epoch). We also want epochs to be large, which is important in visualizing the training history (discussed below). We set validation_split = 0.30 to include 30% of the data for model validation, which prevents overfitting. The training process should complete in 15 seconds or so.
history <- model %>% fit(
  X_train, y_train, 
  epochs = 35, batch_size = 50, 
  validation_split = 0.2
)

#plot(history)
print(history)


#female3065_nona <- female3065_nona_original


model %>% evaluate(X_test, y_test)
model
pred <- data.frame(y = predict(model, as.matrix(X_test)))

predicted = pred$y * abs(diff(range(female3065_nona$BPSysAve))) + min(female3065_nona$BPSysAve)
actual = y_test * abs(diff(range(female3065_nona$BPSysAve))) + min(female3065_nona$BPSysAve)


set.seed(755)

#calculate the MAPE (mean absolute percentage error).

# install.packages("MLmetrics")
library(MLmetrics)
MAPE(predicted, actual) 

RMSE(predicted, actual)






