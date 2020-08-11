library(dplyr)
library(ggplot2)
library(reshape2)
library(Information)
library(tidyverse) 

# Open data from the City of Fredericton
Traffic_Accidents <- read.csv("~/MBA6693/Traffic_Accidents___Accidents_de_la_circulation.csv")
View(Traffic_Accidents)
str(Traffic_Accidents)

attach(Traffic_Accidents)

# Checking for outliers in Month_
Traffic_Accidents$Month_ %>%
  quantile(seq(0,1, 0.01))
# Actual values of the outlier
boxplot(Traffic_Accidents$Month_)$out
# Assign the outlier values into a vector
outliers <- boxplot(Traffic_Accidents$Month_, plot=FALSE)$out
# Check the results
print(outliers)
# Find in which rows the outlier(s) are
Traffic_Accidents[which(Traffic_Accidents$Month_ %in% outliers),]
# Remove the rows containing the outliers
Traffic_Accidents <- Traffic_Accidents[-which(Traffic_Accidents$Month_ %in% outliers),]
# Verify with boxplot
boxplot(Traffic_Accidents$Month_)
# Examine results
Traffic_Accidents$Month_ %>%
  summary()

# Creating 4 Season Brackets
# Season Bracket Function
season_bin <- function(Month_ = 0){
  if(Month_ >= 1 && Month_ <=2)
    return ("Winter")
  else if(Month_ >= 3 && Month_ <=5)
    return ("Spring")
  else if(Month_ >= 6 && Month_ <=8)
    return ("Summer")
  else if(Month_ >= 9 && Month_ <=11)
    return ("Autumn")
  else if(Month_ == 12)
    return ("Winter")
 }

# Examine results
Traffic_Accidents$Season %>%
  summary()

# Add Season column to dataset
Traffic_Accidents$Season <-  Traffic_Accidents$Month_ %>%
  lapply(season_bin)
# %>% as.factor() #gives error
# in that case, do it manually
Traffic_Accidents$Season <- factor(Traffic_Accidents$Season, levels=c("Winter","Spring", "Summer", "Autumn"))

#plot
Traffic_Accidents %>%
  filter(!is.na(Traffic_Accidents$Season)) %>%
ggplot(aes(x=Season)) + geom_bar(position = "dodge") +
  labs(x="Season", y="Total Traffic Accidents", title="Fredericton Traffic Accidents by Day Type") + 
  theme_minimal()

# Creating Daytype Bracket (weekday or weekend)
Traffic_Accidents$Daytype <- factor(Traffic_Accidents$DayOfWeek)
# Use levels on list for the two brackets
levels(Traffic_Accidents$Daytype) <- list(
  'Weekday' = c('Mon','Tue','Wed','Thurs','Fri'),
  'Weekend' = c('Sat','Sun')
)
# check result
str(Traffic_Accidents)
# plot
Traffic_Accidents %>%
  filter(!is.na(Traffic_Accidents$Daytype)) %>%
ggplot(aes(x=Daytype)) + geom_bar(position = "dodge") +
labs(x="Day Type", y="Total Traffic Accidents", title="Fredericton Traffic Accidents by Day Type") +
theme_minimal()

# Plotting existing DayOfWeek variable
# Re-order the days for Sun-Sat week
Traffic_Accidents$DayOfWeek <- ordered(Traffic_Accidents$DayOfWeek, levels=c("Sun","Mon","Tue","Wed","Thurs", 
                                         "Fri","Sat"))
# Plot
Traffic_Accidents %>%
  filter(!is.na(Traffic_Accidents$DayOfWeek)) %>%
  ggplot(aes(x=DayOfWeek)) + geom_bar(position = "dodge") +
  labs(x="Day of Week", y="Total Traffic Accidents", title="Fredericton Traffic Accidents by Day of Week") +
  theme_minimal()


# Convert Day_Night to factor
Traffic_Accidents$Day_Night <- factor(Traffic_Accidents$Day_Night)
str(Traffic_Accidents$Day_Night)

# Plotting existing Day or Night accident variable
Traffic_Accidents %>%
  drop_na(Day_Night) %>%
  ggplot(aes(x=Day_Night)) + geom_bar(position = "dodge") +
  labs(x="Day (D) versus Night (N)", y="Total Traffic Accidents", title="Fredericton Traffic Accidents, Daytime versus Nightime") +
  theme_minimal()

# Creating Rush Hour Bracket
rushhour_bin <- function(Hour_ = 0){
  if(Traffic_Accidents$Daytype == "Weekday" && Hour_ >= 700 && Hour_ <=900)
    return ("Rush Hour")
  else if(Traffic_Accidents$Daytype == "Weekday" && Hour_ >= 1200 && Hour_ <=1300)
    return ("Rush Hour")
  else if(Traffic_Accidents$Daytype == "Weekday" && Hour_ >= 1600 && Hour_ <=1800)
    return ("Rush Hour")
  else
    return ("Normal Traffic")
}

# Apply rushhour_bin function
Traffic_Accidents$Rush_hour <- Traffic_Accidents$Hour_ %>%
  sapply(rushhour_bin)
# Convert to factor
Traffic_Accidents$Rush_hour <- factor(Traffic_Accidents$Rush_hour)
# Plot
Traffic_Accidents %>%
  filter(!is.na(Traffic_Accidents$Rush_hour)) %>%
  ggplot(aes(x=Rush_hour)) + geom_bar(position = "dodge") +
  labs(x="Day Type", y="Total Traffic Accidents", title="Fredericton Traffic Accidents by Traffic Conditions") +
  theme_minimal()
# Check result
view(Traffic_Accidents)

# Create new logical variable Severe for Accidents coded 2 in Severity
# Default is 1 in Severity, so in Severe we want 1 to become 0 and 2 to become 1.
Traffic_Accidents$Severe <- Traffic_Accidents$Severity -1
Traffic_Accidents$Severe = as.logical(Traffic_Accidents$Severe)

str(Traffic_Accidents)

# Logistic Regression

# First examining the newly created factors
glm.accidents.new=glm(Severe ~ Traffic_Accidents$Rush_hour + Traffic_Accidents$Daytype +
                        Traffic_Accidents$Season, data=Traffic_Accidents, family=binomial)
summary(glm.accidents.new)

# Then comparing to original variables (as factors)
glm.accidents.old=glm(Severe ~ as.factor(Day_Night) + as.factor(Month_) + as.factor(DayOfWeek) + 
                        as.factor(Hour_) +, data=Traffic_Accidents, family=binomial)
summary(glm.accidents.old)

# Linear Discriminant Analysis

library(MASS)
# First examining the newly created factors
lda.accidents.new=lda(Severe ~ Traffic_Accidents$Rush_hour + Traffic_Accidents$Daytype +
                     Traffic_Accidents$Season, data=Traffic_Accidents)
lda.accidents.new

# Then comparing to original variables (as factors)
lda.accidents.old=lda(Severe ~ as.factor(Day_Night) + as.factor(DayOfWeek) + as.factor(Hour_), 
                      data=Traffic_Accidents)
lda.accidents.old





