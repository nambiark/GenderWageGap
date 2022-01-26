library('dplyr')
library('readxl')
library('ggplot2')
library('flextable')
library(tidyr)

data_edu <- read.csv('/Users/kirtananambiar/Desktop/STAT/educationandearningsbyageandsex.csv',as.is = TRUE)

unique(data_edu$education_attainment)

sum(is.na(data_edu))


data_edu[data_edu == "(B)"] <- 0


#write.csv(d,"/Users/kirtananambiar/Desktop/STAT/test.csv", row.names = FALSE)

('/Users/kirtananambiar/Desktop/STAT/test.csv')

data_edu$Mean.earnings.Current.dollars.Male <- as.integer(data_edu$Mean.earnings.Current.dollars.Male)
data_edu$Mean.earnings.Current.dollars.Female <- as.integer(data_edu$Mean.earnings.Current.dollars.Female)
data_edu$Number.with.earnings..thousands..Male <- as.integer(data_edu$Number.with.earnings..thousands..Male)
data_edu$Number.with.earnings..thousands..Female<- as.integer(data_edu$Number.with.earnings..thousands..Female)

data_edu <- data_edu %>% replace(is.na(.), 0)
sum(is.na(data_edu))


str(data_edu)
data_edu$edu_diff <- data_edu$Mean.earnings.Current.dollars.Male-data_edu$Mean.earnings.Current.dollars.Female

data_edu <- data_edu %>% select(-'X',-'Mean.earnings.2020.dollars.Male',
                                -'Mean.earnings.2020.dollars.Female') 


data_edu  <-  data_edu %>% rename(age=Educational.attainment..year..and.age,Male=Mean.earnings.Current.dollars.Male,
                                  Female=Mean.earnings.Current.dollars.Female,
                                  number_male=Number.with.earnings..thousands..Male,
                                  number_female=Number.with.earnings..thousands..Female)
unique(data_edu$education_attainment)


result <- data_edu  %>% group_by(education_attainment) %>%
  summarise(m=mean(Male),f=mean(Female))
str(result)

r <-gather(result, key="Gender", value="Income", 2:3)
#test <- data_edu %>% filter(age=='Total') 

#test %>% group_by(education_attainment) %>% 
#  summarise(n=mean(test$Female))

temp <- data_edu
##Converting into Male and female columns
d<-gather(temp, key="Gender", value="Mean Income Current dollars", c(3,5))

d<-gather(d, key="Gender", value="Number", 2:3)

str(d)
d$Gender<-sapply(strsplit(d$Gender,'_'),`[`,2)
unique(d$Gender)

d$Number <- as.integer(d$Number)

d$age <- as.factor(d$age)
d$Gender <- as.factor(d$Gender)
d$education_attainment <- as.factor(d$education_attainment)
##Data Exploratory analysis

d$sqrtIncome <- sqrt(d$`Mean Income Current dollars`)

#p <- d %>% group_by(year,age,education_attainment) %>% mutate()
#data_edu$edu_diff <- data_edu$Mean.earnings.Current.dollars.Male-data_edu$Mean.earnings.Current.dollars.Female

hist(d$`Mean Income Current dollars`) 
hist(d$edu_diff)

hist(d$sqrtIncome) 


library(moments)
skewness(d$sqrtIncome)
skewness(sqrt(1+d$`Mean Income Current dollars`))


skewness(d$edu_diff)
kurtosis(d$sqrtIncome)
kurtosis(d$`Mean Income Current dollars`)
kurtosis(d$edu_diff)

##Univariate analysis
ggplot(data = d) +
  geom_bar(mapping = aes(x = d$education_attainment)) + coord_flip()

ggplot(data = d) +
  geom_count(mapping = aes(x = Gender, y = education_attainment))

ggplot(data = d) +
  geom_boxplot(mapping = aes(x = reorder(d$education_attainment,d$edu_diff, FUN = median), y = d$edu_diff)) +
  labs(x="Education Level",y="Difference in wage") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+coord_flip()

outliers <- boxplot.stats(d$`Mean Income Current dollars`)$out

out_ind <- which(d$`Mean Income Current dollars` %in% c(outliers))
                 
d[out_ind, ]
#Covariate analysis
ggplot(data = d) + geom_bar(mapping = aes(x = d$education_attainment))  +coord_flip()

ggplot(data = d) + 
  geom_point(mapping = aes(x = education_attainment, y = d$edu_diff)) +coord_flip()

##Difference in number in thousands per education level per gender
ggplot(d,aes(x=d$education_attainment,y=d$Number,fill=d$Gender,group=d$Gender))+
  geom_bar(stat = 'identity', position="dodge")+labs(x="Education Level",y="Number in thousands") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))



##Difference in wage per education level per gender
ggplot(r,aes(x=r$education_attainment,y=r$Income,fill=r$Gender, group = r$Gender))+
  geom_bar(stat = 'identity', position="dodge")+labs(x="Education Level",y="Mean Income dollars", 
                                        title = "Income distribution by level of education") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 



##Model building
dr <-  d[-out_ind,]
dim(d)
dim(dr)
##With outliers
lm.all <- lm(sqrt(d$`Mean Income Current dollars`)~Gender+age+year+education_attainment,data = d)
summary(lm.all)
par(mfrow=c(2,2))
plot(lm.all)

####Without outliers
lm.all.dr <- lm(sqrt(dr$`Mean Income Current dollars`)~Gender+age+year+education_attainment,data = dr)
summary(lm.all.dr)

par(mfrow=c(2,2))
plot(lm.all.dr)

lm.bw <- step(lm.all.dr)




#Divide the data in training and test sets
set.seed(1)
train = sample(1:nrow(d),0.7*nrow(d))
IncomeTest = d$`Mean Income Current dollars`[-train]

length(train)
##fitting random forest model
library(randomForest)
set.seed(123)
##Fit the model
rf.Income=randomForest(`Mean Income Current dollars`~Gender+age+education_attainment+year,data=d,subset=train,
                       ntree=500,importance=TRUE)
str(r)
rf.Income
plot(rf.Income)
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

install.packages("rpart.plot")

library(rpart)
library(rpart.plot)



rpart.income=rpart(log(1+`Mean Income Current dollars`)~Gender+age+education_attainment+year,data = d[train,],
                   method="anova",
                   cp=0.0000000001)


plotcp(rpart.income)
printcp(rpart.income)

##pruning the tree after finding a complexity value
rpart.plot.1se <- prune(rpart.income,cp=0.00010)

rpart.plot(rpart.plot.1se , extra=1,
           roundint=FALSE, digits=3, main="1-se Mean Income regression tree")



rpart.income.prune <- prune(rpart.income, cp=0.00010)
rpart.plot(rpart.income.prune, roundint=FALSE, digits=3, extra=1,
           main="Min-error  Mean Income regression tree")
printcp(rpart.income.prune)



#Computing  MSE and sqrt(MSE)
pred=predict(rpart.income.prune, newdata=d[-train,])
income.test=d[-train,"Mean Income Current dollars"]
(MSE = mean((pred-income.test)^2))
sqrt(MSE)

#Plotting test-set value against  predicted median value 
ggplot(data.frame(pred, income.test), aes(x=pred ,y=income.test)) +
  geom_point() +
  geom_abline(slope=1,intercept=0) +
  labs(x="predicted value", 
       y="test-set value",
       title="regression tree")



