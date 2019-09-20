
##------------Bank Marketing Analysis---------------------##

#----------------------------------------------------------
# The standard process followed in analytics projects is:
# 1. Business Understanding
# 2. Data Understanding  
# 3. Data Preparation
# 4. Modelling
# 5. Model Evaluation
# 6. Model Deployment and Recommendations

#-------------------------------------------------------
## Business Understanding:- Prospect Profiling
#-------------------------------------------------------

# Loading bank marketing data in the working directory. 

bank_data<- read.csv("bank_marketing.csv")

# Checking structure of dataset 

str(bank_data)

# Summary of dataset

summary(bank_data)

#-------------------------------------------------------

# Checking response rate of prospect customer

response <- 4640/(36548+4640)
response

# Checking missing values

sum(is.na(bank_data))

#-------------------------------------------------------

# Loading ggplot2 library
library(ggplot2)

# Plotting Age histogram
ggplot(bank_data,aes(age))+geom_histogram()

# Let's check the outlier in the variables 

quantile(bank_data$age,seq(0,1,0.01))

# Box plot 

boxplot(bank_data$age)

# Capping the upper values of age with 71.

bank_data[(which(bank_data$age>71)),]$age <- 71


# Binning the age variable and store it into "binning.age".

bank_data$binning.age <- as.factor(cut(bank_data$age, breaks = c(16, 20, 30, 40, 50, 60, 70, 80)))

# Change the response value to numbers i.e"yes-no" to "1-0"

bank_data$response <- ifelse(bank_data$response == "yes", 1, 0)

# Check the numeric value of response rate in each bucket

agg_age <- merge(aggregate(response ~ binning.age, bank_data, mean),aggregate(response~binning.age, bank_data, sum),by = "binning.age") 

# Adding No.of_prospect
count <- data.frame(table(bank_data$binning.age))
count <- count[,-1]
agg_age <- cbind(agg_age,count)


# changing column name of each variables in agg_age dataframe

colnames(agg_age) <- c("age", "response_rate", "count_prospects","No.of_prospect")

# Round Off the values

agg_age$response_rate <- format(round(agg_age$response_rate, 2))

agg_age

#-------------------------------------------------------

# Let's see the response rate of each age bucket in the plot

ggplot(agg_age, aes(age, No.of_prospect,label = response_rate)) + 
  geom_bar(stat = 'identity') + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
  geom_text(size = 3, vjust = -0.5)

# Let's check the dataset of age less than 20 years. 
Bank_data_age20 <- subset(bank_data,age <20)

View(Bank_data_age20)
summary(Bank_data_age20)

##--------------------------------------------------------  

# Checking structure of dataset

str(bank_data)

#-----Next Variable is "job"

# Checking the levels of the job

levels(bank_data$job)


# Plotting bar graph for job variable.

# Writing a function "plot_response" to do the same task for each variable

plot_response <- function(cat_var, var_name){
  a <- aggregate(response~cat_var, bank_data, mean)
  count <- data.frame(table(cat_var))
  count <- count[,-1]
  agg_response <- cbind(a, count)
  
  colnames(agg_response) <- c(var_name, "response_rate","No.of_Prospect")
  agg_response[, 2] <- format(round(agg_response[, 2], 2))
  
  ggplot(agg_response, aes(agg_response[, 1], count, label = response_rate)) + geom_bar(stat = 'identity') + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + geom_text(size = 3, vjust = -0.5) + xlab(var_name)
  
}

plot_response(bank_data$job, "job")

##--------------------------------------------------------  

# Checking structure of dataset 

str(bank_data)

# Checking Marital status

summary(bank_data$marital)

# Let's replace Unknown level to married

levels(bank_data$marital)[4] <- "married"

# Plotting marital status

plot_response(bank_data$marital,"marital")

# Let's see the education variables

plot_response(bank_data$education,"Education")



# Reducing the levels of education variable

levels(bank_data$education)[c(1:3,5)] <- "Primary_Education"
levels(bank_data$education)[2] <- "Secondary_Education"
levels(bank_data$education)[4]<- "Tertiary_Education"

# Let's again check the education plot

plot_response(bank_data$education,"Education_levels")


#-------------------------------------------------------
# Let's see the default variable

table(bank_data$default)

plot_response(bank_data$default, "Default")
bank_data <- bank_data[,-5]

#-------------------------------------------------------

# Let's understand the housing variables 

summary(bank_data$housing)


plot_response(bank_data$housing, "Housing")

#-------------------------------------------------------

#-- Let's see the next variable which is "loan"

summary(bank_data$loan)

plot_response(bank_data$loan, "Loan Status")
#-------------------------------------------------------



colnames(bank_data)
###---------------------------------------------------------####
##---------Logistic Regression----------#

# Required Packages
#install.packages('dummies')
library(caret)
library(caTools)
library(dummies)

#---------------------------------------------------------    

# Removing binning variables 

bank_data <- bank_data[, -21]


#creating dummy variables

bank_data$response <- as.integer(bank_data$response)

k1 <- bank_data

bank_data <- dummy.data.frame(bank_data)

bank_data$response <- as.factor(ifelse(bank_data$response == 1, "yes", "no"))

#---------------------------------------------------------    

# splitting into train and test data

set.seed(1)

split_indices <- sample.split(bank_data$response, SplitRatio = 0.70)

train <- bank_data[split_indices, ]

test <- bank_data[!split_indices, ]

nrow(train)/nrow(bank_data)

nrow(test)/nrow(bank_data)


#-----------------------------------------------------------
### Logistic Regression


library(MASS)

library(car)

logistic_1 <- glm(response ~ .-duration, family = "binomial", data = train)

summary(logistic_1)

# Using stepwise algorithm for removing insignificant variables 

logistic_1 <- stepAIC(logistic_1, direction = "both")


# stepAIC has removed some variables and only the following ones remain
logistic_2 <- glm(formula = response ~ jobadmin. + jobretired + jobstudent + 
                    jobtechnician + jobunemployed + educationprofessional.course + 
                    educationTertiary_Education + contactcellular + monthmay + monthapr + 
                    monthjun + monthmar + monthjul + monthnov + day_of_weekfri + monthoct +
                    day_of_weekmon + day_of_weekthu + duration + campaign + 
                    educationPrimary_Education + maritaldivorced + campaign + pdays +
                    contactcellular + nr.employed +
                    poutcomefailure + emp.var.rate + cons.price.idx + cons.conf.idx + 
                    euribor3m, family = "binomial", data = train)



# checking vif for logistic_2 

vif(logistic_2)

summary(logistic_2)


#model 3
#remove euribor3m

logistic_3 <- glm(formula = response ~ jobadmin. + jobretired + jobstudent + 
                    jobtechnician + jobunemployed + educationprofessional.course + 
                    educationTertiary_Education + contactcellular + monthmay + monthapr + 
                    monthjun + monthmar + monthjul + monthnov + day_of_weekfri + monthoct +
                    day_of_weekmon + day_of_weekthu + duration + campaign + 
                    educationPrimary_Education + maritaldivorced + campaign + pdays +
                    contactcellular + nr.employed +
                    poutcomefailure + emp.var.rate + cons.price.idx + cons.conf.idx,
                    family = "binomial", data = train)

vif(logistic_3)
summary(logistic_3)


# model 4
#remove emp.var.rate
logistic_4 <- glm(formula = response ~ jobadmin. + jobretired + jobstudent + 
                    jobtechnician + jobunemployed + educationprofessional.course + 
                    educationTertiary_Education + contactcellular + monthmay + monthapr + 
                    monthjun + monthmar + monthjul + monthnov + day_of_weekfri + monthoct +
                    day_of_weekmon + day_of_weekthu + duration + campaign + 
                    educationPrimary_Education + maritaldivorced + campaign + pdays +
                    contactcellular + nr.employed +
                    poutcomefailure + cons.price.idx + cons.conf.idx,
                  family = "binomial", data = train)

vif(logistic_4)
summary(logistic_4)


# model 5
#remove cons.conf.idx
logistic_5 <- glm(formula = response ~ jobadmin. + jobretired + jobstudent + 
                    jobtechnician + jobunemployed + educationprofessional.course + 
                    educationTertiary_Education + contactcellular + monthmay + monthapr + 
                    monthjun + monthmar + monthjul + monthnov + day_of_weekfri + monthoct +
                    day_of_weekmon + day_of_weekthu + duration + campaign + 
                    educationPrimary_Education + maritaldivorced + campaign + pdays +
                    contactcellular + nr.employed +
                    poutcomefailure + cons.price.idx,
                  family = "binomial", data = train)

vif(logistic_5)
summary(logistic_5)

# model 6
#remove maritaldivorced
logistic_6 <- glm(formula = response ~ jobadmin. + jobretired + jobstudent + 
                    jobtechnician + jobunemployed + educationprofessional.course + 
                    educationTertiary_Education + contactcellular + monthmay + monthapr + 
                    monthjun + monthmar + monthjul + monthnov + day_of_weekfri + monthoct +
                    day_of_weekmon + day_of_weekthu + duration + campaign + 
                    educationPrimary_Education + campaign + pdays +
                    contactcellular + nr.employed +
                    poutcomefailure + cons.price.idx,
                  family = "binomial", data = train)

vif(logistic_6)
summary(logistic_6)

# model 7
#remove educationPrimary_Education
logistic_7 <- glm(formula = response ~ jobadmin. + jobretired + jobstudent + 
                    jobtechnician + jobunemployed + educationprofessional.course + 
                    educationTertiary_Education + contactcellular + monthmay + monthapr + 
                    monthjun + monthmar + monthjul + monthnov + day_of_weekfri + monthoct +
                    day_of_weekmon + day_of_weekthu + duration + campaign + 
                    campaign + pdays +
                    contactcellular + nr.employed +
                    poutcomefailure + cons.price.idx,
                  family = "binomial", data = train)

vif(logistic_7)
summary(logistic_7)

# model 8
#remove day_of_weekthu
logistic_8 <- glm(formula = response ~ jobadmin. + jobretired + jobstudent + 
                    jobtechnician + jobunemployed + educationprofessional.course + 
                    educationTertiary_Education + contactcellular + monthmay + monthapr + 
                    monthjun + monthmar + monthjul + monthnov + day_of_weekfri + monthoct +
                    day_of_weekmon + duration + campaign + 
                    campaign + pdays +
                    contactcellular + nr.employed +
                    poutcomefailure + cons.price.idx,
                  family = "binomial", data = train)

vif(logistic_8)
summary(logistic_8)

# model 9
#remove monthoct
logistic_9 <- glm(formula = response ~ jobadmin. + jobretired + jobstudent + 
                    jobtechnician + jobunemployed + educationprofessional.course + 
                    educationTertiary_Education + contactcellular + monthmay + monthapr + 
                    monthjun + monthmar + monthjul + monthnov + day_of_weekfri +
                    day_of_weekmon + duration + campaign + 
                    campaign + pdays +
                    contactcellular + nr.employed +
                    poutcomefailure + cons.price.idx,
                  family = "binomial", data = train)

vif(logistic_9)
summary(logistic_9)

# model 10
#remove day_of_weekfri
logistic_10 <- glm(formula = response ~ jobadmin. + jobretired + jobstudent + 
                    jobtechnician + jobunemployed + educationprofessional.course + 
                    educationTertiary_Education + contactcellular + monthmay + monthapr + 
                    monthjun + monthmar + monthjul + monthnov +
                    day_of_weekmon + duration + campaign + 
                    campaign + pdays +
                    contactcellular + nr.employed +
                    poutcomefailure + cons.price.idx,
                  family = "binomial", data = train)

vif(logistic_10)
summary(logistic_10)

# model 11
#remove monthapr
logistic_11 <- glm(formula = response ~ jobadmin. + jobretired + jobstudent + 
                     jobtechnician + jobunemployed + educationprofessional.course + 
                     educationTertiary_Education + contactcellular + monthmay + 
                     monthjun + monthmar + monthjul + monthnov +
                     day_of_weekmon + duration + campaign + 
                     campaign + pdays +
                     contactcellular + nr.employed +
                     poutcomefailure + cons.price.idx,
                   family = "binomial", data = train)

vif(logistic_11)
summary(logistic_11)

# model 12
#remove jobunemployed
logistic_12 <- glm(formula = response ~ jobadmin. + jobretired + jobstudent + 
                     jobtechnician + educationprofessional.course + 
                     educationTertiary_Education + contactcellular + monthmay + 
                     monthjun + monthmar + monthjul + monthnov +
                     day_of_weekmon + duration + campaign + 
                     campaign + pdays +
                     contactcellular + nr.employed +
                     poutcomefailure + cons.price.idx,
                   family = "binomial", data = train)

vif(logistic_12)
summary(logistic_12)

# model 13
#remove educationprofessional.course
logistic_13 <- glm(formula = response ~ jobadmin. + jobretired + jobstudent + 
                     jobtechnician + 
                     educationTertiary_Education + contactcellular + monthmay + 
                     monthjun + monthmar + monthjul + monthnov +
                     day_of_weekmon + duration + campaign + 
                     campaign + pdays +
                     contactcellular + nr.employed +
                     poutcomefailure + cons.price.idx,
                   family = "binomial", data = train)

vif(logistic_13)
summary(logistic_13)

# model 14
#remove monthjul
logistic_14 <- glm(formula = response ~ jobadmin. + jobretired + jobstudent + 
                     jobtechnician + 
                     educationTertiary_Education + contactcellular + monthmay + 
                     monthjun + monthmar + monthnov +
                     day_of_weekmon + duration + campaign + 
                     campaign + pdays +
                     contactcellular + nr.employed +
                     poutcomefailure + cons.price.idx,
                   family = "binomial", data = train)

vif(logistic_14)
summary(logistic_14)

# model 15
#remove jobadmin.
logistic_15 <- glm(formula = response ~ jobretired + jobstudent + jobtechnician + 
                     educationTertiary_Education + contactcellular + monthmay + 
                     monthjun + monthmar + monthnov +
                     day_of_weekmon + duration + campaign + campaign + pdays +
                     contactcellular + nr.employed + poutcomefailure + cons.price.idx,
                   family = "binomial", data = train)

vif(logistic_15)
summary(logistic_15)

# model 16
#remove jobtechnician
logistic_16 <- glm(formula = response ~ jobretired + jobstudent + 
                     educationTertiary_Education + contactcellular + monthmay + 
                     monthjun + monthmar + monthnov +
                     day_of_weekmon + duration + campaign + campaign + pdays +
                     contactcellular + nr.employed + poutcomefailure + cons.price.idx,
                   family = "binomial", data = train)

vif(logistic_16)
summary(logistic_16)

final_model <- logistic_16

#--------------------------------------------------------------
# Predicting probabilities of test data

predictions <- predict(final_model, newdata = test, type = "response")
summary(predictions)

#--------------------------------------------------------- 
## Model Evaluation
# Consider probability cutoff of 50%.

predicted_response <- factor(ifelse(predictions >= 0.50, "yes", "no"))

# confusion matrix
conf <- confusionMatrix(predicted_response, test$response, positive = "yes")
conf
#this cut off value(50%) has a very low sensitivity(.40)

#---------------------------------------------------------    
# Findint optimal probalility cutoff 

perform_fn <- function(cutoff) 
{
  predicted_response <- factor(ifelse(predictions>= cutoff, "yes", "no"))
  conf <- confusionMatrix(predicted_response, test$response, positive = "yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

#---------------------------------------------------------    
# Creating cutoff values from 0.01 to 0.99 for plotting and initiallizing a matrix of 1000 X 4.
s = seq(.01,.99,length=100)
OUT = matrix(0,100,3)

for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 

#---------------------------------------------------------    
# plotting cutoffs 
plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))


#---------------------------------------------------------    

cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.01)]

predicted_response_final <- factor(ifelse(predictions >= 0.128, "yes", "no"))
conf <- confusionMatrix(predicted_response_final, test$response, positive = "yes")
conf
#as per the confusion matrix, after excluding duration from the model
#accuracy, sensitivity and specificity are all 86%

test$predictions_prob <- predictions
View(test[order('predictions_prob'),])


#-------------------------------------------------------------
##creating a data frame with variables prospect ID, actual response, predicted response,
##predicted probability of response, duration of call in sec, cost of call

prospect_data <- data.frame('Prospect_ID'=1:nrow(test),'Actual_response'=test$response,
                            'Predicted_response'=test$predicted_response_final,
                            'Predicted_probability'=predictions, 
                            'Duration_of_call'=test$duration,stringsAsFactors = FALSE)

prospect_data$'Cost_of_call' <- (prospect_data$'Duration_of_call'*0.033)+0.8

prospect_data$Actual_response <- 
  as.factor(ifelse(prospect_data$Actual_response=="yes",1,0))
prospect_data$Predicted_response <- 
  as.factor(ifelse(prospect_data$Predicted_response=="yes",1,0))

head(prospect_data)

#The data ordered descending by predicted probability
data_by_desc_prob <- 
  prospect_data[order(prospect_data$Predicted_probability,decreasing = TRUE),]
View(data_by_desc_prob)

#---------------------------------------------------------------
top_30 <- round(nrow(prospect_data)*0.3) 
mean(prospect_data$Duration_of_call[1:top_30])
#the average duration of call for top 30% prospects is 252.7 seconds

#---------------------------------------------------------------

# plotting the lift chart

# Loading dplyr package 
require(dplyr)
library(dplyr)

lift <- function(labels , predicted_prob, groups=10) {
  
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

# Table for cumulative gain and lift 
LG = lift(prospect_data$Actual_response, prospect_data$Predicted_probability, groups = 10)
LG

# Gain Chart 
plot(LG$bucket,LG$Gain,col="red",type="l",main="Gain Chart",xlab="% of total targeted",ylab = "% of positive Response")

# Lift Chart 
plot(LG$bucket,LG$Cumlift,col="red",type="l",main="Gain Chart",xlab="% of total targeted",ylab = "Lift")

#This is the table for lift and gain values as per the deciles
#The Insight section below gives a better understanding
View(LG)

####### Insight from gain and lift charts
#If the campaign is run only for the top 30% customers as per the model we would have
#captured 94.8% responses which is very good considering the total number of customers
#the whole campaign would need to contact

#In other words, if we select top 30% customers to run the campaign our output will be
#3.1 times better than selecting 30% customers randomly




