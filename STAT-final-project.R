library('dplyr')
library('readxl')
library('ggplot2')
library('flextable')
library(tidyr)
##install.packages('flextable')

data <- read_excel('/Users/kirtananambiar/Desktop/STAT/ageandincomebysex.xlsx',sheet = "data")


##Data pre-processing
#split to remove  brackets
data$year <- sapply(strsplit(data$year,'\\('),`[`,1)
data$year <- trimws(data$year)
data$diff <- data$`Mean Income Current dollar-Male`-data$`Mean income Current dollars-Female`


##selecting interested columns
d <- data %>% select(-'Number with income  (thousands)-Male',-'Mean Income 2020 dollars-Male',
-'Mean income 2020 dollars-Female',-'Number with income (thousands)- Female') 

##Converting into Male and female columns
d<-gather(d, key="Gender", value="Mean Income Current dollars", 3:4)

#Correcting column values
d$Gender<-sapply(strsplit(d$Gender,'-'),`[`,2)
head(d)


d$age <- as.factor(d$age)
d$Gender <- as.factor(d$Gender)

d <-d %>% filter(!d$age %in% c('15 Years and Over','65 Years and Older') ) 


d$sqrtIncome <- sqrt(d$`Mean Income Current dollars`)
##Data Exploratory analysis
hist(d$`Mean Income Current dollars`)
hist(d$diff)

library(moments)
skewness(d$sqrtIncome)
skewness(d$`Mean Income Current dollars`)
skewness(d$diff)

kurtosis(d$sqrtIncome)
kurtosis(d$`Mean Income Current dollars`)
kurtosis(d$diff)

##Histogram after transformation
hist(d$sqrtIncome)
##discrete
ggplot(data = d) + geom_bar(mapping = aes(x = age))  +coord_flip()

##year vs income for male and female
ggplot(d,aes(x=d$year,y=d$sqrtIncome,fill=d$Gender, group = d$Gender))+
  geom_line() +
  geom_point(size = 4, shape = 21)+labs(x="Year",y="Mean Income dollars", 
                                        title = "Income distribution by year") + 
  theme(plot.title = element_text(hjust = 0.5)) +
theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
 
##which age group has max difference in income 
ggplot(d,aes(x=d$year,y=d$diff,fill=d$age, group = d$age))+
  geom_line() +
  geom_point(size = 4, shape = 21)+labs(x="Year",y="Difference in Mean Income dollars", 
                                        title = "Income distribution by age group") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

unique(d$age)
d$age <- as.character(d$age)
test <- d %>% group_by(age,Gender) %>% mutate(total_income=mean(diff))
head(test)

ggplot(data=test,aes(x=age,y=test$diff))+
  geom_bar(stat = "identity") + labs(x='Age-Group',y='Wage-difference')

###Maximum female income value is on lower side as compared to male.
ggplot(data = d, mapping = aes(x = d$`Mean Income Current dollars`)) + 
  geom_freqpoly(mapping = aes(colour = Gender), binwidth = 500)+labs(x="Mean Income dollars", 
               title = "Income distribution") +
  theme(plot.title = element_text(hjust = 0.5))
  
##Median income by gender
ggplot(data = d, mapping = aes(y = d$sqrtIncome, x = d$Gender)) +
  geom_boxplot() +labs(x="Gender", y="Mean Income",
                       title = "Mean Income in Dollars by gender") +
  theme(plot.title = element_text(hjust = 0.5))
  

lm.all <- lm(sqrtIncome~Gender+age+year,data = d)
summary(lm.all)

par(mfrow=c(2,2))
plot(lm.all)

#Divide the data in training and test sets
set.seed(1)
train = sample(1:nrow(d),0.7*nrow(d))
IncomeTest = d$sqrtIncome[-train]

length(train)
##fitting random forest model
library(randomForest)
set.seed(123)
##Fit the model
rf.Income=randomForest(sqrtIncome~Gender+age+year,data=d,subset=train,
                        ntree=500,importance=TRUE)

rf.Income

## predict 
rf.pred = predict(rf.Income,type = 'class',newdata =d[-train,])


cat('Mean Squared Value:',(MSE = mean((rf.pred-IncomeTest)^2)))
cat('\nMean Square Root Value:',sqrt(MSE))

# Plot test-set  value vs. predicted  value 
ggplot(data.frame(rf.pred, IncomeTest), aes(x=rf.pred ,y=IncomeTest)) +
  geom_point() +
  geom_abline(slope=1,intercept=0) +
  labs(x="predicted income", 
       y="test-set income",
       title="regression")

# Print and plot the variable-importance measures
importance(rf.Income)
varImpPlot(rf.Income)


head(d)

head(data_edu)
unique(data_edu$`Educational attainment`)
##educationandincomebysex