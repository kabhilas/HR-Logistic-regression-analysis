# Install the required packages
requiredPackages = c('MASS','car','caret','cowplot','ggplot2','GGally',
                     'caTools','lubridate','e1071')

#Installing the required packages
for(item in requiredPackages){
  if(!require(item,character.only = TRUE)){
    install.packages(item)
  }
  library(item, character.only = TRUE)
}

# Loading the five input files
employee_survey_data <- read.csv("employee_survey_data.csv", stringsAsFactors = F)
manager_survey_data <- read.csv("manager_survey_data.csv", stringsAsFactors = F)
general_data <- read.csv("general_data.csv", stringsAsFactors = F)
in_time <- read.csv("in_time.csv", stringsAsFactors = F)
out_time <- read.csv("out_time.csv", stringsAsFactors = F)

#################################### Data Understanding and preparation
#################################### Fixing in_time and out_time file
str(in_time)
str(out_time)
# Renaming the EmployeeId and other column names
colnames(in_time)[1] <- "EmployeeID"
colnames(in_time) <- gsub("X", "", colnames(in_time))
colnames(out_time)[1] <- "EmployeeID"
colnames(out_time) <- gsub("X", "", colnames(out_time))

# Checking for NA's present in single columns and removing those from dataframes
colnames(in_time)[which(sapply(in_time, function(x) sum(is.na(x)) == nrow(in_time)))]
colnames(out_time)[which(sapply(out_time, function(x) sum(is.na(x)) == nrow(out_time)))]
in_time <- in_time[, -c(as.numeric(which(sapply(in_time, function(x) sum(is.na(x)) == nrow(in_time)))))]
out_time <- out_time[, -c(as.numeric(which(sapply(out_time, function(x) sum(is.na(x)) == nrow(out_time)))))]

# Checking NA's for each column
sapply(in_time, function(x) sum(is.na(x)))
max_in <- max(as.numeric(sapply(in_time, function(x) sum(is.na(x)))))
round((max_in/nrow(in_time)), 2) * 100     # 6% of the data is NA and can be ignored 
sapply(out_time, function(x) sum(is.na(x)))
max_out <- max(as.numeric(sapply(out_time, function(x) sum(is.na(x)))))
round((max_out/nrow(out_time)), 2) * 100     # 6% of the data is NA and can be ignored

# Checking for any different column in in_time and out_time dataframes
which(!colnames(in_time) == colnames(out_time))

# Converting date columns in POSIXct format
in_time[,2:ncol(in_time)] <- lapply(in_time[,2:ncol(in_time)], function(x) as_datetime(x))
out_time[,2:ncol(out_time)] <- lapply(out_time[,2:ncol(out_time)], function(x) as_datetime(x))
str(in_time)
str(out_time)

TimeSpent <- cbind(in_time[, 1], out_time[,2:ncol(out_time)]-in_time[,2:ncol(out_time)])
colnames(TimeSpent)[1] <- "EmployeeID"
str(TimeSpent)
TimeSpent[,2:ncol(TimeSpent)] <- lapply(TimeSpent[,2:ncol(TimeSpent)], function(x) { round(as.numeric(x), 2) })

TimeSpent$Avg_TimeSpent <- rowMeans(TimeSpent[, 2:ncol(TimeSpent)], na.rm = TRUE)
TimeSpent$Avg_TimeSpent <- round(TimeSpent$Avg_TimeSpent, 2)
TimeSpent$OverTime <- ifelse((TimeSpent$Avg_TimeSpent > 8.5), 1, 0)

################################## 

str(employee_survey_data)
str(manager_survey_data)
str(general_data)

length(unique(tolower(employee_survey_data$EmployeeID)))    # 4410, confirming customerID is key 
length(unique(tolower(manager_survey_data$EmployeeID))) # 4410, confirming customerID is key
length(unique(tolower(general_data$EmployeeID))) # 4410, confirming customerID is key

setdiff(employee_survey_data$EmployeeID,manager_survey_data$EmployeeID) 
setdiff(employee_survey_data$EmployeeID,general_data$EmployeeID)
setdiff(manager_survey_data$EmployeeID,general_data$EmployeeID)

# Analyse employee_survey_data 
str(employee_survey_data)
sapply(employee_survey_data, function(x) sum(is.na(x)))
nrow(employee_survey_data) == length(unique(employee_survey_data$EmployeeID))

# Analyse manager_survey_data 
str(manager_survey_data)
sapply(manager_survey_data, function(x) sum(is.na(x)))
nrow(manager_survey_data) == length(unique(manager_survey_data$EmployeeID))

# Analyse general_data 
str(general_data)
sapply(general_data, function(x) sum(is.na(x)))
nrow(general_data) == length(unique(general_data$EmployeeID))

# Merging employee_survey_data, manager_survey_data, general_data 
df <- merge(employee_survey_data,manager_survey_data, by="EmployeeID", all = F)
df <- merge(df, general_data, by="EmployeeID", all = F)
str(df)
df <- cbind(df, TimeSpent[, c(251, 252)])

sapply(df, function(x) sum(is.na(x)))   # Leaving NA values as it is

# Remove columns having unique value for all rows
df <- df[, -c(as.numeric(which(sapply(df, function(x) length(unique(x)) == 1))))]
# %age of NAs in the data set
round((sum(as.numeric(which(sapply(df, function(x) sum(is.na(x))) != 0)))/nrow(df)) * 100, 2)
# Replacing NA values with mean of the column
which(sapply(df, function(x) sum(is.na(x))) != 0)
# EnvironmentSatisfaction         JobSatisfaction      NumCompaniesWorked       TotalWorkingYears         WorkLifeBalance 
# 10                               15                      18                      23                      25
df$EnvironmentSatisfaction[which(is.na(df$EnvironmentSatisfaction))] <- median(df$EnvironmentSatisfaction, na.rm = TRUE)
df$JobSatisfaction[which(is.na(df$JobSatisfaction))] <- median(df$JobSatisfaction, na.rm = TRUE)
df$NumCompaniesWorked[which(is.na(df$NumCompaniesWorked))] <- median(df$NumCompaniesWorked, na.rm = TRUE)
df$TotalWorkingYears[which(is.na(df$TotalWorkingYears))] <- median(df$TotalWorkingYears, na.rm = TRUE)
df$WorkLifeBalance[which(is.na(df$WorkLifeBalance))] <- median(df$WorkLifeBalance, na.rm = TRUE)
sum(is.na(df))

df.main <- df

# Univariate analysis
colnames(df.main)
str(df.main)
# $ Age                    : int  51 31 32 38 32 46 28 29 31 25 ...                                                         
# $ Attrition              : chr  "No" "Yes" "No" "No" ...                                                                  Categorical (2 levels)
# $ Avg_TimeSpent          : num  7.37 7.72 7.01 7.19 8.01 ...                                                              
# $ BusinessTravel         : chr  "Travel_Rarely" "Travel_Frequently" "Travel_Frequently" "Non-Travel" ...                  Categorical (3 levels)
# $ Department             : chr  "Sales" "Research & Development" "Research & Development" "Research & Development" ...    Categorical (3 levels)  
# $ DistanceFromHome       : int  6 10 17 2 10 8 11 18 1 7 ...
# $ Education              : int  2 1 4 5 1 3 2 3 3 4 ...                                                                   Categorical (5 levels)
# $ EducationField         : chr  "Life Sciences" "Life Sciences" "Other" "Life Sciences" ...                               Categorical (6 levels)
# $ EmployeeID             : int  1 2 3 4 5 6 7 8 9 10 ...
# $ EnvironmentSatisfaction: int  3 3 2 4 4 3 1 1 2 2 ...                                                                   Categorical (4 levels)  
# $ Gender                 : chr  "Female" "Female" "Male" "Male" ...                                                       Categorical (2 levels)
# $ JobInvolvement         : int  3 2 3 2 3 3 3 3 3 3 ...                                                                   Categorical (4 levels)    
# $ JobLevel               : int  1 1 4 3 1 4 2 2 3 4 ...                                                                   Categorical (5 levels)  
# $ JobRole                : chr  "Healthcare Representative" "Research Scientist" "Sales Executive" "Human Resources" ...  Categorical (9 levels)
# $ JobSatisfaction        : num  4 2 2 4 1 2 3 2 4 1 ...                                                                   Categorical (4 levels)  
# $ MaritalStatus          : chr  "Married" "Single" "Married" "Married" ...                                                Categorical (3 levels)
# $ MonthlyIncome          : int  131160 41890 193280 83210 23420 40710 58130 31430 20440 134640 ...
# $ NumCompaniesWorked     : int  1 0 1 3 4 3 2 2 0 1 ...
# $ OverTime               : num  0 0 0 0 0 1 0 0 0 0 ...
# $ PercentSalaryHike      : int  11 23 15 11 12 13 20 22 21 13 ...
# $ PerformanceRating      : int  3 4 3 3 3 3 4 4 4 3 ...                                                                   Categorical (2 levels)
# $ StockOptionLevel       : int  0 1 3 3 2 0 1 3 0 1 ...
# $ TotalWorkingYears      : int  1 6 5 13 9 28 5 10 10 6 ...
# $ TrainingTimesLastYear  : int  6 3 2 5 2 5 2 2 2 2 ...
# $ WorkLifeBalance        : num  2 4 1 3 3 2 1 3 3 3 ...                                                                   Categorical (4 levels)
# $ YearsAtCompany         : int  1 5 5 8 6 7 0 0 9 6 ...
# $ YearsSinceLastPromotion: int  0 1 0 7 0 7 0 0 7 1 ...
# $ YearsWithCurrManager   : int  0 4 3 5 4 7 0 0 8 5 ...

# Barcharts for categorical features 
bar_theme1<- theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), 
                   legend.position="none")

plot_grid(ggplot(df.main, aes(x=factor(BusinessTravel),fill=Attrition))+ geom_bar(), 
          ggplot(df.main, aes(x=factor(Department),fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(df.main, aes(x=factor(Education),fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(df.main, aes(x=factor(EducationField),fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(df.main, aes(x=factor(EnvironmentSatisfaction),fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(df.main, aes(x=factor(JobInvolvement),fill=Attrition))+ geom_bar()+bar_theme1,
          align = "h")   

plot_grid(ggplot(df.main, aes(x=factor(JobLevel),fill=Attrition))+ geom_bar(), 
          ggplot(df.main, aes(x=factor(JobRole),fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(df.main, aes(x=factor(JobSatisfaction),fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(df.main, aes(x=factor(MaritalStatus),fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(df.main, aes(x=factor(PerformanceRating),fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(df.main, aes(x=factor(WorkLifeBalance),fill=Attrition))+ geom_bar()+bar_theme1,
          align = "h")   

# Histogram and Boxplots for numeric variables 
box_theme<- theme(axis.line=element_blank(),axis.title=element_blank(), 
                  axis.ticks=element_blank(), axis.text=element_blank())

box_theme_y<- theme(axis.line.y=element_blank(),axis.title.y=element_blank(), 
                    axis.ticks.y=element_blank(), axis.text.y=element_blank(),
                    legend.position="none")

plot_grid(ggplot(df.main, aes(Age))+ geom_histogram(binwidth = 10),
          ggplot(df.main, aes(x="",y=Age))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)

plot_grid(ggplot(df.main, aes(Avg_TimeSpent))+ geom_histogram(binwidth = 10),
          ggplot(df.main, aes(x="",y=Avg_TimeSpent))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)

plot_grid(ggplot(df.main, aes(DistanceFromHome))+ geom_histogram(binwidth = 10),
          ggplot(df.main, aes(x="",y=DistanceFromHome))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)

plot_grid(ggplot(df.main, aes(MonthlyIncome))+ geom_histogram(binwidth = 10),
          ggplot(df.main, aes(x="",y=MonthlyIncome))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)

plot_grid(ggplot(df.main, aes(NumCompaniesWorked))+ geom_histogram(binwidth = 10),
          ggplot(df.main, aes(x="",y=NumCompaniesWorked))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)

plot_grid(ggplot(df.main, aes(PercentSalaryHike))+ geom_histogram(binwidth = 10),
          ggplot(df.main, aes(x="",y=PercentSalaryHike))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)

plot_grid(ggplot(df.main, aes(StockOptionLevel))+ geom_histogram(binwidth = 10),
          ggplot(df.main, aes(x="",y=StockOptionLevel))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)

plot_grid(ggplot(df.main, aes(TotalWorkingYears))+ geom_histogram(binwidth = 10),
          ggplot(df.main, aes(x="",y=TotalWorkingYears))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)

plot_grid(ggplot(df.main, aes(TrainingTimesLastYear))+ geom_histogram(binwidth = 10),
          ggplot(df.main, aes(x="",y=TrainingTimesLastYear))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)

plot_grid(ggplot(df.main, aes(YearsAtCompany))+ geom_histogram(binwidth = 10),
          ggplot(df.main, aes(x="",y=YearsAtCompany))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)

plot_grid(ggplot(df.main, aes(YearsSinceLastPromotion))+ geom_histogram(binwidth = 10),
          ggplot(df.main, aes(x="",y=YearsSinceLastPromotion))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)

plot_grid(ggplot(df.main, aes(YearsWithCurrManager))+ geom_histogram(binwidth = 10),
          ggplot(df.main, aes(x="",y=YearsWithCurrManager))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)

sapply(df.main[,c("Age", "Avg_TimeSpent", "DistanceFromHome", "MonthlyIncome", "NumCompaniesWorked", "PercentSalaryHike")], 
       function(x) quantile(x,seq(0,1,.01),na.rm = T))  # no outliers

sapply(df.main[,c("StockOptionLevel", "TotalWorkingYears", "TrainingTimesLastYear", "YearsAtCompany", "YearsSinceLastPromotion", "YearsWithCurrManager")], 
       function(x) quantile(x,seq(0,1,.01),na.rm = T))

###########################
# Convert 2 level categorical columns in 0 and 1
df.main$Attrition <- ifelse(df.main$Attrition == "Yes", 1,0)
df.main$Gender <- ifelse(df.main$Gender == "Female",1,0)

# Converting categorical variables with more than two levels in factor
df.main.categorical <- df.main[,c("BusinessTravel","Department","Education",
                                     "EducationField","EnvironmentSatisfaction","JobInvolvement", "JobLevel",
                                     "JobRole","JobSatisfaction","MaritalStatus","PerformanceRating",
                                     "WorkLifeBalance")]

df.main.categorical <- data.frame(sapply(df.main.categorical, function(x) factor(x)))
str(df.main.categorical)

#Creating dummy attributes for factor attributes
dummies <- data.frame(sapply(df.main.categorical, function(x)
  data.frame(model.matrix(~x-1, data = df.main.categorical))[,-1]))

#Removing the categorical attributes and adding the corresponding dummy attributes.
df.main <- cbind(df.main[,-c(9,10,12,13,2,5,15,16,3,17,6,4)], dummies)
View(df.main)  # 4410 observations with 56 attributes

# Columns to be scaled 
# "Age", "DistanceFromHome","MonthlyIncome","NumCompaniesWorked","PercentSalaryHike","StockOptionLevel","TotalWorkingYears","TrainingTimesLastYear"  
# "YearsAtCompany","YearsSinceLastPromotion","YearsWithCurrManager","Avg_TimeSpent"
scale_var <- c( 2,4,5,7,8,9,10,11,12,13,14,15)
df.main.scaled <- df.main
for(i in scale_var)
{
  df.main.scaled[,i] <- scale(x=df.main[,i],center = TRUE,scale = TRUE)
}
summary(df.main.scaled)

##################################################################################
# splitting the data between train and test
set.seed(100)

indices = sample.split(df.main$EmployeeID, SplitRatio = 0.7)

train = df.main.scaled[indices,c(3,2,c(4:56))]
test = df.main.scaled[!(indices),c(3,2,c(4:56))]

model_1 <- glm(Attrition~.,data=train,family = 'binomial')
summary(model_1)

model_2 <- stepAIC(model_1, direction="both")
summary(model_2)

vif(model_2)

#removing JobInvolvement.x4
model_3 <- glm(formula = Attrition ~ Age + MonthlyIncome + NumCompaniesWorked + 
                 PercentSalaryHike + TotalWorkingYears + TrainingTimesLastYear + 
                 YearsSinceLastPromotion + YearsWithCurrManager + OverTime + 
                 BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                 Department.xResearch...Development + Department.xSales + 
                 Education.x2 + EducationField.xLife.Sciences + EnvironmentSatisfaction.x2 + 
                 EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                 JobInvolvement.x2 + JobInvolvement.x3 +  
                 JobLevel.x4 + JobLevel.x5 + JobRole.xManufacturing.Director + 
                 JobRole.xResearch.Director + JobRole.xSales.Executive + JobSatisfaction.x2 + 
                 JobSatisfaction.x3 + JobSatisfaction.x4 + MaritalStatus.xSingle + 
                 PerformanceRating + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                 WorkLifeBalance.x4, family = "binomial", data = train)

summary(model_3)
vif(model_3)

#removing JobInvolvement.x2
model_4 <- glm(formula = Attrition ~ Age + MonthlyIncome + NumCompaniesWorked + 
                 PercentSalaryHike + TotalWorkingYears + TrainingTimesLastYear + 
                 YearsSinceLastPromotion + YearsWithCurrManager + OverTime + 
                 BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                 Department.xResearch...Development + Department.xSales + 
                 Education.x2 + EducationField.xLife.Sciences + EnvironmentSatisfaction.x2 + 
                 EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                 JobInvolvement.x3 +  JobLevel.x4 + JobLevel.x5 + JobRole.xManufacturing.Director + 
                 JobRole.xResearch.Director + JobRole.xSales.Executive + JobSatisfaction.x2 + 
                 JobSatisfaction.x3 + JobSatisfaction.x4 + MaritalStatus.xSingle + 
                 PerformanceRating + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                 WorkLifeBalance.x4, family = "binomial", data = train)

summary(model_4)
vif(model_4)

#removing Education.x2 
model_5 <- glm(formula = Attrition ~ Age + MonthlyIncome + NumCompaniesWorked + 
                 PercentSalaryHike + TotalWorkingYears + TrainingTimesLastYear + 
                 YearsSinceLastPromotion + YearsWithCurrManager + OverTime + 
                 BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                 Department.xResearch...Development + Department.xSales + 
                 EducationField.xLife.Sciences + EnvironmentSatisfaction.x2 + 
                 EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                 JobInvolvement.x3 +  JobLevel.x4 + JobLevel.x5 + JobRole.xManufacturing.Director + 
                 JobRole.xResearch.Director + JobRole.xSales.Executive + JobSatisfaction.x2 + 
                 JobSatisfaction.x3 + JobSatisfaction.x4 + MaritalStatus.xSingle + 
                 PerformanceRating + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                 WorkLifeBalance.x4, family = "binomial", data = train)

summary(model_5)
vif(model_5)

#removing MonthlyIncome
model_6 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + PercentSalaryHike + TotalWorkingYears + TrainingTimesLastYear + 
                 YearsSinceLastPromotion + YearsWithCurrManager + OverTime + 
                 BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                 Department.xResearch...Development + Department.xSales + 
                 EducationField.xLife.Sciences + EnvironmentSatisfaction.x2 + 
                 EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                 JobInvolvement.x3 +  JobLevel.x4 + JobLevel.x5 + JobRole.xManufacturing.Director + 
                 JobRole.xResearch.Director + JobRole.xSales.Executive + JobSatisfaction.x2 + 
                 JobSatisfaction.x3 + JobSatisfaction.x4 + MaritalStatus.xSingle + 
                 PerformanceRating + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                 WorkLifeBalance.x4, family = "binomial", data = train)

summary(model_6)
vif(model_6)

#removing JobLevel.x4
model_7 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + PercentSalaryHike + TotalWorkingYears + TrainingTimesLastYear + 
                 YearsSinceLastPromotion + YearsWithCurrManager + OverTime + 
                 BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                 Department.xResearch...Development + Department.xSales + 
                 EducationField.xLife.Sciences + EnvironmentSatisfaction.x2 + 
                 EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                 JobInvolvement.x3 +  JobLevel.x5 + JobRole.xManufacturing.Director + 
                 JobRole.xResearch.Director + JobRole.xSales.Executive + JobSatisfaction.x2 + 
                 JobSatisfaction.x3 + JobSatisfaction.x4 + MaritalStatus.xSingle + 
                 PerformanceRating + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                 WorkLifeBalance.x4, family = "binomial", data = train)

summary(model_7)
vif(model_7)

#removing JobRole.xSales.Executive
model_8 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + PercentSalaryHike + TotalWorkingYears + TrainingTimesLastYear + 
                 YearsSinceLastPromotion + YearsWithCurrManager + OverTime + 
                 BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                 Department.xResearch...Development + Department.xSales + 
                 EducationField.xLife.Sciences + EnvironmentSatisfaction.x2 + 
                 EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                 JobInvolvement.x3 +  JobLevel.x5 + JobRole.xManufacturing.Director + 
                 JobRole.xResearch.Director + JobSatisfaction.x2 + 
                 JobSatisfaction.x3 + JobSatisfaction.x4 + MaritalStatus.xSingle + 
                 PerformanceRating + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                 WorkLifeBalance.x4, family = "binomial", data = train)

summary(model_8)
vif(model_8)

#removing PerformanceRating
model_9 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + PercentSalaryHike + TotalWorkingYears + TrainingTimesLastYear + 
                 YearsSinceLastPromotion + YearsWithCurrManager + OverTime + 
                 BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                 Department.xResearch...Development + Department.xSales + 
                 EducationField.xLife.Sciences + EnvironmentSatisfaction.x2 + 
                 EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                 JobInvolvement.x3 +  JobLevel.x5 + JobRole.xManufacturing.Director + 
                 JobRole.xResearch.Director + JobSatisfaction.x2 + 
                 JobSatisfaction.x3 + JobSatisfaction.x4 + MaritalStatus.xSingle + 
                 WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                 WorkLifeBalance.x4, family = "binomial", data = train)

summary(model_9)
vif(model_9)

#removing PercentSalaryHike
model_10 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                 YearsSinceLastPromotion + YearsWithCurrManager + OverTime + 
                 BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                 Department.xResearch...Development + Department.xSales + 
                 EducationField.xLife.Sciences + EnvironmentSatisfaction.x2 + 
                 EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                 JobInvolvement.x3 +  JobLevel.x5 + JobRole.xManufacturing.Director + 
                 JobRole.xResearch.Director + JobSatisfaction.x2 + 
                 JobSatisfaction.x3 + JobSatisfaction.x4 + MaritalStatus.xSingle + 
                 WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                 WorkLifeBalance.x4, family = "binomial", data = train)

summary(model_10)
vif(model_10)

#removing EducationField.xLife.Sciences
model_11 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                  YearsSinceLastPromotion + YearsWithCurrManager + OverTime + 
                  BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                  Department.xResearch...Development + Department.xSales + 
                  EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                  JobInvolvement.x3 +  JobLevel.x5 + JobRole.xManufacturing.Director + 
                  JobRole.xResearch.Director + JobSatisfaction.x2 + 
                  JobSatisfaction.x3 + JobSatisfaction.x4 + MaritalStatus.xSingle + 
                  WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                  WorkLifeBalance.x4, family = "binomial", data = train)

summary(model_11)
vif(model_11)

#removing JobLevel.x5
model_12 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                  YearsSinceLastPromotion + YearsWithCurrManager + OverTime + 
                  BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                  Department.xResearch...Development + Department.xSales + 
                  EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                  JobInvolvement.x3 + JobRole.xManufacturing.Director + 
                  JobRole.xResearch.Director + JobSatisfaction.x2 + 
                  JobSatisfaction.x3 + JobSatisfaction.x4 + MaritalStatus.xSingle + 
                  WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                  WorkLifeBalance.x4, family = "binomial", data = train)

summary(model_12)
vif(model_12)

#removing JobRole.xResearch.Director
model_13 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                  YearsSinceLastPromotion + YearsWithCurrManager + OverTime + 
                  BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                  Department.xResearch...Development + Department.xSales + 
                  EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                  JobInvolvement.x3 + JobRole.xManufacturing.Director + 
                  JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + MaritalStatus.xSingle + 
                  WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                  WorkLifeBalance.x4, family = "binomial", data = train)

summary(model_13)
vif(model_13)

#removing Department.xResearch...Development
model_14 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                  YearsSinceLastPromotion + YearsWithCurrManager + OverTime + 
                  BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                  Department.xSales + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                  JobInvolvement.x3 + JobRole.xManufacturing.Director + 
                  JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + MaritalStatus.xSingle + 
                  WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                  WorkLifeBalance.x4, family = "binomial", data = train)

summary(model_14)
vif(model_14)

#removing Department.xSales
model_15 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                  YearsSinceLastPromotion + YearsWithCurrManager + OverTime + 
                  BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                  EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                  JobInvolvement.x3 + JobRole.xManufacturing.Director + 
                  JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + MaritalStatus.xSingle + 
                  WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                  WorkLifeBalance.x4, family = "binomial", data = train)

summary(model_15)
vif(model_15)

#removing BusinessTravel.xTravel_Rarely
model_16 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                  YearsSinceLastPromotion + YearsWithCurrManager + OverTime + 
                  BusinessTravel.xTravel_Frequently + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                  JobInvolvement.x3 + JobRole.xManufacturing.Director + 
                  JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + MaritalStatus.xSingle + 
                  WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                  WorkLifeBalance.x4, family = "binomial", data = train)

summary(model_16)
vif(model_16)

#removing JobInvolvement.x3
model_17 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                  YearsSinceLastPromotion + YearsWithCurrManager + OverTime + 
                  BusinessTravel.xTravel_Frequently + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                  JobRole.xManufacturing.Director + JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + MaritalStatus.xSingle + 
                  WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                  WorkLifeBalance.x4, family = "binomial", data = train)

summary(model_17)
vif(model_17)

#removing WorkLifeBalance.x3
model_18 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                  YearsSinceLastPromotion + YearsWithCurrManager + OverTime + 
                  BusinessTravel.xTravel_Frequently + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                  JobRole.xManufacturing.Director + JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + MaritalStatus.xSingle + 
                  WorkLifeBalance.x2 + WorkLifeBalance.x4, family = "binomial", data = train)

summary(model_18)
vif(model_18)

#removing WorkLifeBalance.x2
model_19 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                  YearsSinceLastPromotion + YearsWithCurrManager + OverTime + 
                  BusinessTravel.xTravel_Frequently + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                  JobRole.xManufacturing.Director + JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + MaritalStatus.xSingle + 
                  WorkLifeBalance.x4, family = "binomial", data = train)

summary(model_19)
vif(model_19)

#removing WorkLifeBalance.x4
model_20 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                  YearsSinceLastPromotion + YearsWithCurrManager + OverTime + BusinessTravel.xTravel_Frequently + 
                  EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                  JobRole.xManufacturing.Director + JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                  MaritalStatus.xSingle, family = "binomial", data = train)

summary(model_20)
vif(model_20)

final_model <- model_20

#######################################################################

### Model Evaluation

### Test Data ####

#predicted probabilities of attrition 1 for test data

test_pred = predict(final_model, type = "response", 
                    newdata = test[,-1])


# Let's see the summary 

summary(test_pred)

test$prob <- test_pred
View(test)
# Let's use the probability cutoff of 50%.

test_pred_attrition <- factor(ifelse(test_pred >= 0.50, "Yes", "No"))
test_actual_attrition <- factor(ifelse(test$Attrition==1,"Yes","No"))


table(test_actual_attrition,test_pred_attrition)


#######################################################################
test_pred_attrition <- factor(ifelse(test_pred >= 0.16, "Yes", "No"))

install.packages("e1071")
library(e1071)

test_conf <- confusionMatrix(test_pred_attrition, test_actual_attrition, positive = "Yes")
test_conf
#######################################################################

#########################################################################################
# Let's Choose the cutoff value. 
# 

# Let's find out the optimal probalility cutoff 

perform_fn <- function(cutoff) 
{
  predicted_attrition <- factor(ifelse(test_pred >= cutoff, "Yes", "No"))
  conf <- confusionMatrix(predicted_attrition, test_actual_attrition, positive = "Yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

# Creating cutoff values from 0.003575 to 0.812100 for plotting and initiallizing a matrix of 100 X 3.

# Summary of test probability

summary(test_pred)

s = seq(.01,.80,length=100)

OUT = matrix(0,100,3)


for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 


plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))


cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.01)]


# Let's choose a cutoff value of 0.3132 for final model

test_cutoff_attrition <- factor(ifelse(test_pred >=0.1616, "Yes", "No"))

conf_final <- confusionMatrix(test_cutoff_attrition, test_actual_attrition, positive = "Yes")

acc <- conf_final$overall[1]

sens <- conf_final$byClass[1]

spec <- conf_final$byClass[2]

acc

sens

spec

View(test)
##################################################################################################
### KS -statistic - Test Data ######

test_cutoff_attrition <- ifelse(test_cutoff_attrition=="Yes",1,0)
test_actual_attrition <- ifelse(test_actual_attrition=="Yes",1,0)


library(ROCR)
#on testing  data
pred_object_test<- prediction(test_cutoff_attrition, test_actual_attrition)

performance_measures_test<- performance(pred_object_test, "tpr", "fpr")

ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - 
  (attr(performance_measures_test, "x.values")[[1]])

max(ks_table_test)


####################################################################
# Lift & Gain Chart 

# plotting the lift chart

# Loading dplyr package 
require(dplyr)
library(dplyr)

lift <- function(labels , predicted_prob,groups=10) {
  
  if(is.factor(labels)) labels  <- as.integer(as.character(labels ))
  if(is.factor(predicted_prob)) predicted_prob <- as.integer(as.character(predicted_prob))
  helper = data.frame(cbind(labels , predicted_prob))
  helper[,"bucket"] = ntile(-helper[,"predicted_prob"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(labels ), funs(total = n(),
                                     totalresp=sum(., na.rm = TRUE))) %>%
    
    mutate(Cumresp = cumsum(totalresp),
           Gain=Cumresp/sum(totalresp)*100,
           Cumlift=Gain/(bucket*(100/groups))) 
  return(gaintable)
}

attrition_decile = lift(test_actual_attrition, test_pred, groups = 10)
attrition_decile
