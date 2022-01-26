library('dplyr')
library('readxl')
library('ggplot2')
library('flextable')
library(tidyr)
#install.packages('flextable')

data <- read_excel('/Users/kirtananambiar/Desktop/STAT/occupationlongestjobbysex.xlsx',sheet = "data")
data<- data[-c(1, 2),]          
head(data)
str(data)

##Data pre-processing
#split to remove  brackets
data$year <- sapply(strsplit(data$year,'\\('),`[`,1)
data$year <- trimws(data$year)
data[data == "(B)"] <- NA

data$`Mean earnings Current dollars-Male` <- as.numeric(data$`Mean earnings Current dollars-Male`)
data$`Mean earnings Current dollars-Female` <- as.numeric(data$`Mean earnings Current dollars-Female`)



data$diff <- data$`Mean earnings Current dollars-Male`-data$`Mean earnings Current dollars-Female`


##selecting interested columns
d <- data %>% dplyr::select(-'Number with earnings (thousands)-Male',-'Mean earnings 2020 dollars-Male',
                     -'Mean earnings 2020 dollars-Female',-'Number with earnings (thousands)-Female') 

##Converting into Male and female columns
d<-gather(d, key="Gender", value="Mean Income Current dollars", 2:3)

#Correcting column values
d$Gender<-sapply(strsplit(d$Gender,'-'),`[`,2)
head(d)


d$Occupation <- as.factor(d$Occupation)
d$Gender <- as.factor(d$Gender)
d$year <- as.factor(d$year)
str(d)
na.omit(d)
##Data Exploratory analysis

hist(d$diff)

library(moments)

ggplot(data = data) + 
  geom_point(mapping = aes(x = data$`Mean earnings Current dollars-Male`, 
                           y = data$`Mean earnings Current dollars-Female`))
unique(d$Gender)

ggplot(data = d) + geom_bar(mapping = aes(x = d$Occupation))  +coord_flip()

ggplot(d,aes(x=d$Occupation,y=d$diff)) +
  geom_bar(stat='identity',fill="Blue") +labs(x="Occupation",
                                    y="Mean income difference") + coord_flip()
options("scipen"=100, "digits"=4)
library(reshape2)
test <- data %>% dplyr::select (-`Number with earnings (thousands)-Male`,-`Mean earnings 2020 dollars-Male`,
                         -`Number with earnings (thousands)-Female`,-`Mean earnings 2020 dollars-Female`,-`diff`)



test.long<-melt(test)
test.long <-na.omit(test.long)

###Gender gap with year
ggplot(test.long,aes(year,value,fill=variable))+
  geom_bar(stat="identity",position="dodge") + labs(x="Year",y="Mean Income",
                                                    fill="Gender")+ 
  scale_fill_discrete(name = "Gender", labels = c("Male","Female"))+
  theme(legend.position = "bottom",
        legend.direction = "horizontal"
  )


ggplot(test.long,aes(year,value,fill=variable))+
  geom_bar(stat="identity",position="dodge") + labs(x="Occupation",y="Mean Income",
      fill="Gender")+ 
  scale_fill_discrete(name = "Gender", labels = c("Male","Female"))+
  theme(legend.position = "bottom",
        legend.direction = "horizontal"
       )+
  coord_flip()

unique(test.long$variable)
str(test.long)
attach(d)
##Difference in income vs occupation
ggplot(d, aes(x = year, y = d$diff, fill = d$Gender, group = d$Gender)) +
  geom_line()+
  geom_point(size = 4, shape = 21)+labs(x="Year",y="Mean Income dollars", 
                                        title = "Income distribution by occupation") + 
  theme(plot.title = element_text(hjust = 0.5)) + coord_flip()

##year vs income for male and female
ggplot(test.long,aes(x=year,y=value,fill=variable, group = variable))+
  geom_line() +
  geom_point(size = 4, shape = 21)+labs(x="Year",y="Mean Income dollars", 
                                        title = "Income distribution by year")+
  theme(legend.position = "bottom",legend.direction = "horizontal")


hist(test.long$value)
hist(log(test.long$value))

min(test.long$value,na.rm = TRUE)
max(test.long$value,na.rm = TRUE)

##Accuracy on plain model
lm.all <- lm(value~variable+Occupation+year,data = test.long)
summary(lm.all)

par(mfrow=c(2,2))
plot(lm.all)

##accuracy after log transformation
lm.all.log <- lm(log(value)~variable+Occupation+year,data = test.long)
summary(lm.all.log)

par(mfrow=c(2,2))
plot(lm.all.log)
library(MASS)
##accuracy after boxcox transformation
boxcox(lm.all.log, seq(-1,1 ,length=20))
lm.all.bc <- lm(I(value^0.3)~variable+Occupation+year,data = test.long)
summary(lm.all.bc)

par(mfrow=c(2,2))
plot(lm.all.bc)


skewness(lm.all$residuals)
kurtosis(lm.all$residuals)

skewness(lm.all.log$residuals)
kurtosis(lm.all.log$residuals)

d<-na.omit(d)

#Divide the data in training and test sets
set.seed(1)
train = sample(1:nrow(d),0.7*nrow(d))
#IncomeTest = d$`Mean Income Current dollars`[-train]
IncomeTest = (d$`Mean Income Current dollars`[-train])

length(train)
##fitting random forest model
library(randomForest)
set.seed(123)
##Fit the model
rf.Income=randomForest(d$`Mean Income Current dollars`~Gender+Occupation+year,data=d,subset=train,
                       ntree=500,importance=TRUE)

rf.Income
str(d)
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

docc <- data %>% select(-'Mean.earnings.2020.dollars.Male',
                                -'Mean.earnings.2020.dollars.Female') 


docc  <-  docc %>% rename(age=Educational.attainment..year..and.age,Male=Mean.earnings.Current.dollars.Male,
                                  Female=Mean.earnings.Current.dollars.Female,
                                  number_male=Number.with.earnings..thousands..Male,
                                  number_female=Number.with.earnings..thousands..Female)
unique(data_edu$education_attainment)
