## MATERNAL HEALTH RISK ANALYSIS USING RANDOM FOREST & CLASSIFICATION TREE
# load the necessary libraries
library(mice)
library(ggplot2)
library(dplyr)
library(corrplot)
library(caTools)
library(caret)
library(randomForest)
library(vip)
library(rpart)
library(rpart.plot)

# load the data set
df <- read.csv("maternal.health.risk.csv", sep = ",")

# dim of the data set
dim(df)

# structure of the df
str(df)

# changing the class of the RiskLevel col
df$RiskLevel <- as.factor(df$RiskLevel)

# number of patients belonging each risk group
table(df$RiskLevel)

# missing data
md.pattern(df) # completely observed

## EDA
# number of patients in each risk level
ggplot(df, aes(x = RiskLevel)) +
  geom_bar(aes(fill = RiskLevel), color = "black") +
  labs(x = "Risk Level",
       y = "# of Patients",
       caption = "Prepared by ST",
       fill = "Risk Level") +
  ggtitle("# of Patients in Each Risk Level") +
  theme_dark() +
  geom_text(aes(label = ..count..), 
            stat = "count",
            vjust = -1)

# age histogram of the patients
ggplot(df, aes(x = Age)) +
  geom_histogram(aes(y = ..density..),
                 bins = 50,
                 color = "black",
                 fill = "hotpink") +
  scale_x_continuous(breaks = seq(0, 80, by = 5)) +
  geom_density() +
  theme_dark() +
  ggtitle("Age Distribution of the Patients") +
  labs(x = "Age",
       y = "Frequency",
       caption = "Prepared by ST")

# mean ages for each risk level
mean.ages.df <- df %>%
  group_by(RiskLevel) %>%
  summarise(mean.ages = mean(Age))

# Density plot of age by risk level with mean ages for each risk level
ggplot(df, aes(x = Age)) +
  geom_histogram(aes(fill = RiskLevel,
                     y = ..density..),
                 position = "dodge",
                 bins = 30,
                 binwidth = 2,
                 alpha = 0.5) +
  geom_density(aes(color = RiskLevel),
               alpha = 0.4,
               size = 0.8) +
  scale_x_continuous(breaks = seq(10, 70, by = 5)) +
  geom_vline(data = mean.ages.df,
             aes(xintercept = mean.ages, color = RiskLevel),
             linetype = "dashed",
             size = 1) +
  theme_dark() +
  ggtitle("Age Histogram by Risk Level with Mean Ages") +
  labs(x = "Age",
       y = "Frequency",
       caption = "Prepared by ST",
       color = "Risk Level") +
  guides(fill = FALSE)

# scatter plot of heart rate vs. body temp by risk level
# ~ 0 correlation between them
ggplot(df, aes(x = HeartRate,
               y = BodyTemp,
               color = RiskLevel)) +
  geom_point() +
  scale_x_continuous(breaks = seq(0, 90, by = 10)) +
  geom_jitter(width = 3.5, height = 0.5) +
  ggtitle("Heart Rate vs. Body Temp by Risk Level") +
  labs(x = "Heart Rate",
       y = "Body Temp (F)",
       caption = "Prepared by ST",
       color = "Risk Level") +
  theme_dark()

# scatter plot of systolic bp vs. diastolic bp by risk level
# positive correlation
ggplot(df, aes(x = SystolicBP,
               y = DiastolicBP,
               color = RiskLevel)) +
  geom_point() +
  geom_jitter(width = 2.5,
              height = 1) +
  ggtitle("Systloic Bp vs. Diastolic Bp by Risk Level") +
  labs(x = "Systolic Bp",
       y = "Diastolic Bp",
       caption = "Prepared by ST",
       color = "Risk Level") +
  theme_dark()

# scatter plot of bs vs. heart rate by risk level
# almost no correlation between them
ggplot(df, aes(x = BS, y = HeartRate, color = RiskLevel)) +
  geom_point() +
  geom_jitter(width = 2.5, height = 1) +
  scale_y_continuous(breaks = seq(20, 100, by = 10)) +
  ggtitle("BS vs. Heart Rate by Risk Level") +
  labs(x = "BS",
       y = "Heart Rate",
       caption = "Prepared by ST",
       color = "Risk Level") +
  theme_dark()

# scatter plot of systolic bp vs. diastolic bp by age
ggplot(df, aes(x = SystolicBP,
               y = DiastolicBP,
               color = Age)) +
  geom_point() +
  geom_jitter(width = 2.5,
              height = 1) +
  ggtitle("Systloic Bp vs. Diastolic Bp by Age") +
  labs(x = "Systolic Bp",
       y = "Diastolic Bp",
       caption = "Prepared by ST",
       color = "Age") +
  theme_dark() +
  scale_color_gradient(low = "green", high = "red")

# box plot of age - risk level
ggplot(df, aes(x = RiskLevel, y = Age)) +
  geom_boxplot(aes(fill = RiskLevel), color = "black") +
  ggtitle("Age Distribution by Risk Levels") +
  labs(x = "Risk Level",
       y = "Age",
       fill = "Risk Level") +
  scale_y_continuous(breaks = seq(20, 60, by = 5)) +
  theme_dark()

# box plot of heart rate - risk level
ggplot(df, aes(x = RiskLevel, y = HeartRate)) +
  geom_boxplot(aes(fill = RiskLevel), color = "black") +
  scale_y_continuous(breaks = seq(0, 90, by = 5)) +
  ggtitle("Heart Rate Distribution by Risk Levels") +
  labs(x = "Risk Level",
       y = "Heart Rate",
       caption = "Prepared by ST",
       fill = "Risk Level") +
  theme_dark()

# box plot of BS - risk level
ggplot(df, aes(x = RiskLevel, y = BS)) +
  geom_boxplot(aes(fill = RiskLevel), color = "black") +
  scale_y_continuous(breaks = seq(0, 20, by = 1)) +
  ggtitle("BS Distribution by Risk Levels") +
  labs(x = "Risk Level",
       y = "BS",
       fill = "Risk Level",
       caption = "Prepared by ST") +
  theme_dark()

# box plot of body temp - risk level
ggplot(df, aes(x = RiskLevel, y = BodyTemp)) +
  geom_boxplot(aes(fill = RiskLevel), color = "black") +
  scale_y_continuous(breaks = seq(90, 110, by = 1)) +
  ggtitle("Body Temperature Distribution by Risk Levels") +
  labs(x = "Risk Level",
       y = "Body Temperature",
       caption = "Prepared by ST",
       fill = "Risk Level") +
  theme_dark()

# box plot of diastolic bp - risk level
ggplot(df, aes(x = RiskLevel, y = DiastolicBP)) +
  geom_boxplot(aes(fill = RiskLevel), color = "black") +
  scale_y_continuous(breaks = seq(50,100 , by = 5)) +
  ggtitle("Diastolic BP Distribution by Risk Levels") +
  labs(x = "Risk Level",
       y= "Diastolic BP",
       caption = "Prepared by ST",
       fill = "Risk Level") +
  theme_dark()

# box plot of systolic bp - risk level
ggplot(df, aes(x = RiskLevel, y = SystolicBP)) +
  geom_boxplot(aes(fill = RiskLevel), color = "black") +
  scale_y_continuous(breaks = seq(70,160 , by = 10)) +
  ggtitle("Systolic BP Distribution by Risk Levels") +
  labs(x = "Risk Level",
       y= "Systolic BP",
       caption = "Prepared by ST",
       fill = "Risk Level") +
  theme_dark()

## correlation
# correlation table
cor.data <- cor(df [, -7])

# correlation graph
corrplot(cor.data, method = "number")

### OUTLIER DETECTION
## AGE 
# outlier detection of the age
boxplot.stats(df$Age)$out # potential outliers of the age 

# imputation of age col
uv.1 <- quantile(df$Age, 0.75) + ((1.5) * (quantile(df$Age, 0.75) - quantile(df$Age, 0.25)))
df$Age [df$Age > uv.1] <- uv.1 # all age values greater than uv.1 are replaced with this uv.1

## SYSTOLIC BP
# outlier detection of the systolic bp 
boxplot.stats(df$SystolicBP)$out # potential outliers of the systolic bp

# imputation of the systolic bp
uv.2 <- quantile(df$SystolicBP, 0.75) + ((1.5) * (quantile(df$SystolicBP, 0.75) - quantile(df$SystolicBP, 0.25)))
df$SystolicBP [df$SystolicBP > uv.2] <- uv.2 # all systolic bp greater than uv.2 are replaced with this uv.2

## BS
# outlier detection of the BS
boxplot.stats(df$BS)$out # potential outliers of the BS

# imputation of the BS
uv.3 <- quantile(df$BS, 0.75) + ((1.5) * (quantile(df$BS, 0.75) - quantile(df$BS, 0.25)))
df$BS [df$BS > uv.3] <- uv.3 # all BS values greater than uv.3 are replaced with this uv.3

## HEARTRATE
# outlier detection of the heart rate
boxplot.stats(df$HeartRate)$out # potential outliers of the heart rate

# imputation of the heart rate
uv.4 <- quantile(df$HeartRate, 0.25) - ((1.5) * (quantile(df$HeartRate, 0.75) - quantile(df$HeartRate, 0.25)))
df$HeartRate [df$HeartRate < uv.4] <- uv.4 # all heart rate values lower than uv.4 are replaced with this uv.4

### Test & Train Split
# test & train split
split <- sample.split(df$RiskLevel, SplitRatio = 0.7)
test <- subset(df, split == FALSE)
train <- subset(df, split == TRUE)

# dim of the train set
dim(test)

# dim of the test set
dim(train)

### RANDOM FOREST
# creating parameter grid for random forest model
param.grid.rf <- expand.grid(mtry = seq(1, 5, by = 1))

# setting cross - validation parameters with the control function
ctrl.rf <- trainControl(method = "cv",
                        number = 5)

# select the best parameters for random forest
parameter.search.rf <- train(RiskLevel ~.,
                             data = train,
                             method = "rf",
                             trControl = ctrl.rf,
                             tuneGrid = param.grid.rf)

# best tune of the random forest model
parameter.search.rf$bestTune$mtry

# plot of cv - predictors
plot(parameter.search.rf)

# building a rf model with best tune mtry value
set.seed(101)
rf.model <- randomForest(RiskLevel ~.,
                         train,
                         mtry = parameter.search.rf$bestTune$mtry,
                         ntree = 70)

# rf model OOB estimate of error rate
rf.model

# plot of the trees - error of the random forest model
plot(rf.model)

# predictions of the random forest model
rf.preds <- predict(rf.model, test)

# confusion matrix of the random forest model
confusionMatrix(rf.preds, test$RiskLevel)

# variable importance plot
vip(rf.model)

### CLASSIFICATION TREE
# define the parameter grid of the ct
param.grid.ct <- expand.grid(cp = seq(0.01, 1, by = 0.01))

# control parameter of the ct
ctrl.ct <- trainControl(method = "cv",
                        number = 5)

# select the best parameters for the ct
parameter.search.ct <- train(RiskLevel ~.,
                             data = train,
                             method = "rpart",
                             trControl = ctrl.ct,
                             tuneGrid = param.grid.ct)

# best tune of the ct
parameter.search.ct$bestTune$cp

# building the ct model
set.seed(101)
ct.model <- rpart(RiskLevel ~., 
                  data = train,
                  cp = parameter.search.ct$bestTune$cp)

# cross - validation error vs. CP
plotcp(ct.model)

# complexity parameter vs. accuracy (CV) plot 
plot(parameter.search.ct)

# classification tree plot
rpart.plot(ct.model, digits = 3, box.palette = "pink")

# predictions of the CT model (test)
ct.preds <- predict(ct.model, test, type = "class")

# confusion matrix of the ct model (test)
confusionMatrix(ct.preds, test$RiskLevel)

# predictions of the CT model (train)
ct.preds.train <- predict(ct.model, train, type = "class")

# confusion matrix of the ct model (train)
confusionMatrix(ct.preds.train, train$RiskLevel)