library(caret)
library(rpart)
library(rpart.plot)
library(ggplot2)
library(moments)
library(neuralnet)
library(corrplot)
library(pROC)
library(moments)
library(caTools)
library(rpart)
library(rpart.plot)
library(party)
library(rattle)
library(partykit)
library(lmtest)
#https://view.officeapps.live.com/op/view.aspx?src=https%3A%2F%2Fzindi-private-release.s3.eu-west-2.amazonaws.com%2Fuploads%2Fcompetition_datafile%2Ffile%2F85%2FVariable_Codebook.docx%3FX-Amz-Algorithm%3DAWS4-HMAC-SHA256%26X-Amz-Credential%3DAKIAZF6GMQOOWFPUAZPE%252F20220501%252Feu-west-2%252Fs3%252Faws4_request%26X-Amz-Date%3D20220501T162932Z%26X-Amz-Expires%3D900%26X-Amz-SignedHeaders%3Dhost%26X-Amz-Signature%3Dae202b4993efbc21f64d0f77e964799f7802369910913a72bb1599d3e6ccfd5b&wdOrigin=BROWSELINK
#https://zindi.africa/competitions/mobile-money-and-financial-inclusion-in-tanzania-challenge/data?fbclid=IwAR1NaqodDsBDYjxDS1vyzRVgeeb3MXxprcJ1NfaOWYAn-hRdZMXHQrtwvNE
#https://towardsdatascience.com/predicting-mobile-financial-service-adoption-with-machine-learning-a59744986bce


########## Luare in evidenta doar liniile complete ###############
data <- data[complete.cases(data),]
########### Eliminare varfiabila id ##########################
data <- data[-c(1)]
data <- data[-c(9)]
View(data)
############# redenumire coloane ################
names(data)
new_columns_name <- c('age', 'gender', 'marital_status', 
                      'educ', 'pers_own', 'mobile_phone','wages',
                      'work', 'sent_money', 'received_money', 
                      'use_mobile_banking',
                      'latitude', 'longitude', 'mobile_banking', 'savings',
                      'insurance')
names(data) = new_columns_name
data <- data[-c(11)]

############## analiza exploratorie numerica #############

prop.table(table(test_data$mobile_banking))*100
#proportia persoanelor care folosesc internet banking este de 55.39%, iar
#proportia pesoanelor care nu folosesc este de 44.61%

summary(data)
str(data)
data$age<-as.numeric(data$age)
data$gender<-as.numeric(data$gender)
data$marital_status<-as.numeric(data$marital_status)
data$educ<-as.numeric(data$educ)
data$pers_own<-as.numeric(data$pers_own)
data$mobile_phone<-as.numeric(data$mobile_phone)
data$wages<-as.numeric(data$wages)
data$work<-as.numeric(data$work)
data$latitude<-as.numeric(data$latitude)
data$longitude<-as.numeric(data$longitude)
data$mobile_banking<-as.numeric(data$mobile_banking)
data$savings <- as.numeric(data$savings)
data$insurance<- as.numeric(data$insurance)
summary(data$age)
skewness(data$age, na.rm = TRUE)
kurtosis(data$age, na.rm = TRUE)

summary(data$latitude)
skewness(data$latitude, na.rm = TRUE)
kurtosis(data$latitude, na.rm = TRUE)
prop.table(table(data$mobile_banking))*100

######### analiza grafica #############
###AGE##############
options(repr.plot.width = 5, repr.plot.height = 5)

ggplot(data, aes(age)) +
  geom_histogram(bins = 30, aes(y = ..density..), fill = 'purple') + 
  geom_density(alpha = 0.2, fill = 'purple') +
  ggtitle('Distribution of age') +
  theme(axis.title = element_text(), axis.title.x = element_text()) +
  geom_vline(xintercept = round(mean(data$age), 2), size = 2, linetype = 3)
#asimetrie la dreapta
boxplot(data$age,
        main = toupper("Boxplot of Age"),
        ylab = "age",
        col = "purple")
outliers_age <- boxplot(data$age, plot=FALSE)
outliers_age <-outliers_age $out
data2 <- data[-which(data$age %in% outliers_age),]
##https://link.springer.com/content/pdf/10.1007/s42081-020-00091-y.pdf#

######GENDER VS MOBILE BANKING#########
data2 <- data
gender_mobile_banking <- table(data2$gender, data2$mobile_banking)
barplot(gender_mobile_banking, main="Gender vs Mobile_banking",
        xlab="Mobile banking No vs Yes", col=c("darkblue","red"),
        legend = rownames(gender_mobile_banking), beside=TRUE)

######## MARITAL STATUS VD MOBILE BANKING##########
marital_mobile_banking <- table(data2$marital_status, data2$mobile_banking)
barplot(marital_mobile_banking, main="Marital status vs Mobile_banking",
        xlab="Mobile banking No vs Yes", col=c("darkblue","red",'green','pink'),
        legend = rownames(marital_mobile_banking), beside=TRUE)

########## EDUC VS MOBILE BANKING #############
educ_mobile_banking <- table(data2$educ, data2$mobile_banking)
barplot(educ_mobile_banking, main="Education vs Mobile_banking",
        xlab="Mobile banking No vs Yes", col=c("darkblue","red", 'yellow', 'green','purple',
                                               'orange', 'pink'),
        legend = rownames(educ_mobile_banking), beside=TRUE)

######### mobile phone vs mobile banking ###########
phone_mobile_banking <- table(data2$mobile_phone, data2$mobile_banking)
barplot(phone_mobile_banking, main="Mobile phone vs Mobile_banking",
        xlab="Mobile banking No vs Yes", col=c("darkblue","red"),
        legend = rownames(phone_mobile_banking), beside=TRUE)
##### CEVA GRESIT

######## WAGES VS MOBILE BANKING#######
wages_mobile_banking <- table(data2$wages, data2$mobile_banking)
barplot(wages_mobile_banking, main="Income vs Mobile_banking",
        xlab="Mobile banking No vs Yes", col=c("darkblue","red"),
        legend = rownames(wages_mobile_banking), beside=TRUE)
####ceva gresit

####### work vs wages ###########
work_wages <- table(data2$work, data2$wages)
barplot(work_wages, main="Work vs Income",
        xlab="Income No vs Yes", col=c("darkblue","red", 'yellow', 'green','purple',
                                               'orange', 'pink'),
        legend = rownames(work_wages), beside=TRUE)
######## savings vs mobile banking ###########
savings_mobile_banking <- table(data2$savings, data2$mobile_banking)
barplot(savings_mobile_banking, main="Savings vs Mobile_banking",
        xlab="Mobile banking No vs Yes", col=c("darkblue","red"),
        legend = rownames(savings_mobile_banking), beside=TRUE)
######### insurance vs mobile banking #############
insurance_mobile_banking <- table(data2$insurance, data2$mobile_banking)
barplot(insurance_mobile_banking, main="Insurance vs Mobile_banking",
        xlab="Mobile banking No vs Yes", col=c("darkblue","red"),
        legend = rownames(insurance_mobile_banking), beside=TRUE)
write.csv(data2,'C:/Users/salav/OneDrive/Desktop/Disertatie/data.csv')

###### REGRESIE LOGISTICA ############
Data <- read.csv('C:/Users/salav/OneDrive/Desktop/Disertatie/data.csv')
Data <- Data[-c(1)]
#1. MWTODA PURPOSEFUL
# Purposeful selection steps
# 1. Uni-variable analysis
# 2. Multi-variable model comparisons 
# 3. Interactions among covariates
# 4. Assessing fit of the model

# 1. Uni-variable analysis
model_for_age<- glm(Data$mobile_banking~Data$age, family=binomial)
summary(model_for_age) # P-value < 0.05 =>  OK

model_for_gender<- glm(Data$mobile_banking~Data$gender, family=binomial)
summary(model_for_gender) # P-value < 0.05 =>  OK

model_for_marital<- glm(Data$mobile_banking~Data$marital_status, family=binomial)
summary(model_for_marital) # P-value > 0.05 => NOT  OK

model_for_pers_own<- glm(Data$mobile_banking~Data$pers_own, family=binomial)
summary(model_for_pers_own) # P-value < 0.05 =>  OK

model_for_educ<- glm(Data$mobile_banking~Data$educ, family=binomial)
summary(model_for_educ) # P-value > 0.05 => NOT OK

model_for_mobile<- glm(Data$mobile_banking~Data$mobile_phone, family=binomial)
summary(model_for_mobile) # P-value < 0.05 =>  OK

model_for_wages<- glm(Data$mobile_banking~Data$wages, family=binomial)
summary(model_for_wages) # P-value > 0.05 => not  OK

model_for_work<- glm(Data$mobile_banking~Data$work, family=binomial)
summary(model_for_work) # P-value > 0.05 => not  OK

model_for_latitude<- glm(Data$mobile_banking~Data$latitude, family=binomial)
summary(model_for_latitude) # P-value > 0.05 => not  OK

model_for_longitude<- glm(Data$mobile_banking~Data$longitude, family=binomial)
summary(model_for_longitude) # P-value > 0.05 => not  OK

model_for_savings<- glm(Data$mobile_banking~Data$savings, family=binomial)
summary(model_for_savings) # P-value > 0.05 => not OK

model_for_insurance<- glm(Data$mobile_banking~Data$insurance, family=binomial)
summary(model_for_insurance) # P-value > 0.05 => not  OK




model_0 <- glm(Data$mobile_banking ~
                 Data$age + Data$gender + Data$marital_status
               + Data$educ + Data$pers_own + Data$mobile_phone
               + Data$wages + Data$work + 
                 Data$latitude + Data$longitude
               + Data$savings + Data$insurance, family = binomial
)
summary(model_0)   #p value pt gender, marital_status, wages, work, longitude, latitude > 0.05, deci le vom elimina

model_1 <- glm(Data$mobile_banking ~
                 Data$age + Data$educ 
               + Data$pers_own + Data$mobile_phone
                 + Data$savings + Data$insurance
            , family = binomial
)
summary(model_1) #OK.

anova(model_0, model_1, test='Chisq')


model_with_inter_0 <- glm(Data$mobile_banking~
                            Data$age + 
                            Data$educ+ 
                            Data$pers_own + Data$mobile_phone+ Data$savings
                          + Data$insurance +
                            Data$educ:Data$insurance,
                          family=binomial
)
summary(model_with_inter_0)
# ANOVA (model_1 versus model_with_inter_0) 
anova(model_1, model_with_inter_0, test='Chisq') # P-value = 0.0004 => the two models are  significantly different
#vom pastra noul model

# 4. Assessing fit of the model
predictions = predict(model_with_inter_0, 
                      type = 'response') 


predictions <- ifelse(predictions > 0.5, 1, 0)
#Making the confusion matrix
predictions_fact <- as.factor(predictions)
M <- confusionMatrix(predictions_fact, as.factor(Data$mobile_banking), mode = "everything", positive="1")
M
#plot the ROC curve
auc(Data$mobile_banking, predictions, plot=TRUE, print.auc=TRUE, col='blue', lwd=4)

#MODELING
#Splitting the dataset into the Training set and Test set
#Stratified sample
ib_yes <- Data[which(Data$mobile_banking == '1'), ]
ib_no <- Data[which(Data$mobile_banking == '0'), ]
set.seed(123)
train_yes_rows <- sample(1:nrow(ib_yes), 0.80*nrow(ib_yes))
train_no_rows <- sample(1:nrow(ib_no), 0.80*nrow(ib_no))
train_yes <- ib_yes[train_yes_rows, ]  
train_no <- ib_no[train_no_rows, ]
training_set <- rbind(train_yes, train_no)
#Test Data
test_yes <- ib_yes[-train_yes_rows, ]
test_no <- ib_no[-train_no_rows, ]
test_set <- rbind(test_yes, test_no)
model_final <- glm(mobile_banking ~ age + educ 
               + pers_own + mobile_phone + savings + insurance +
                 educ:insurance, data = training_set
               , family = binomial)
coef(summary(model_final))
exp(coef((model_final)))

#calculate VIF values for each predictor variable in our model
car::vif(model_final)

#Predictions
predictions_2 = predict(model_final, 
                      type = 'response') 

predictions_2 <- ifelse(predictions_2 > 0.5, 1, 0)

#Making the confusion matrix
predictions2_fact <- as.factor(predictions_2)
M <- confusionMatrix(predictions2_fact, as.factor(training_set$mobile_banking), mode = "everything", positive="1")
M
#plot the ROC curve
auc(test_set$mobile_banking, test_predictions, plot=TRUE, print.auc=TRUE, col='blue', lwd=4)
# Making predictions on the test set.
test_predictions <- ifelse(predict(model_final, newdata = test_set, type = "response") > 0.5, "1", "0")
test_predictions_fact <- as.factor(test_predictions)
M_test <- confusionMatrix(test_predictions_fact, as.factor(test_set$mobile_banking), mode = "everything", positive="1")
M_test
head(test_predictions, 3)
#plot the ROC curve
auc(test_set$mobile_banking, test_predictions, plot=TRUE, print.auc=TRUE, col='green', lwd=4)

########### RETEA NEURALA CU 1 STRAT ASCUNS SI 3 NEURONI  ###############
data <- read.csv('C:/Users/salav/OneDrive/Desktop/Disertatie/Data.csv')
data <- data[c(-1)]

f1 <- mobile_banking ~ age + gender + marital_status + educ + pers_own + 
  mobile_phone + wages + work + latitude + longitude + savings + insurance

set.seed (1)
n=nrow(data)
train1 <- sample (1:n, 5618 , FALSE )

nn1 <- neuralnet(f1, 
                 data =  data[train1 ,],
                 hidden = 1,
                 #threshold=0.04,
                 #act.fct="tanh",
                 #algorithm = "rprop+",
                 linear.output = FALSE)
print( nn1)
plot(nn1 , intercept = FALSE ,
     show.weights = FALSE )


nn1$result.matrix

## Comparare cu figura urmãtoare
plot(nn1 , intercept = TRUE ,
     show.weights = TRUE )
# Previziune pe baza re??elei neurale MLP
pred1 <- compute(nn1 , data[train1 , c('age', 'gender', 'marital_status', 'educ', 'pers_own', 'mobile_phone', 
                                        'wages', 'work', 'latitude',
                                        'longitude', 'savings', 'insurance')] )

## [-Train] = subsetul de testare
head(pred1$net.result ,5)


## Valorile sunt probabilitã??i ??i pot fi transformate în valori binare (pragul de 0,5)
r1 <- ifelse( pred1$net.result <= 0.5, 0, 1)

head(r1,5)



## Tabelul de contingen??ã (confusion matrix)
table(sign(r1),sign(data[train1, 'mobile_banking']) ,
      dnn =c("Predicted" , "Observed"))
r1_fact <- as.factor(r1)
M1 <- confusionMatrix(r1_fact, as.factor(data[train1, 'mobile_banking']), mode = "everything", positive="1")
M1


f2 <-  mobile_banking ~  gender  + educ + pers_own + wages +
  mobile_phone  + savings + insurance

set.seed (1)
n=nrow(Data)
#train2 <- sample (1:n, 5000 , FALSE )

nn2 <- neuralnet(f2, 
                 data =  data[train1 ,],
                 hidden = 1,
                 #threshold=0.04, act.fct="tanh",
                 linear.output = FALSE)
print( nn2)
plot(nn2 , intercept = FALSE ,
     show.weights = FALSE )


nn2$result.matrix

## Comparare cu figura urmãtoare
plot(nn2 , intercept = TRUE ,
     show.weights = TRUE )
# Previziune pe baza re??elei neurale MLP
pred2 <- compute(nn2 , data[-train1 , c(  'gender', 'educ', 'pers_own','wages',
                                        'mobile_phone',  'savings',
                                        'insurance')] )

## [-Train] = subsetul de testare
head(pred2$net.result ,5)


## Valorile sunt probabilitã??i ??i pot fi transformate în valori binare (pragul de 0,5)
r2 <- ifelse( pred2$net.result <= 0.5, 0, 1)

head(r2,5)



## Tabelul de contingen??ã (confusion matrix)
table(sign(r2),sign(data[-train1, 'mobile_banking']) ,
      dnn =c("Predicted" , "Observed"))
r2_fact <- as.factor(r2)
M2 <- confusionMatrix(r2_fact, as.factor(data[-train1, 'mobile_banking']
                                         ), mode = "everything", positive="1")
M2

######### ARBORE DE DECIZIE################
Data <- read.csv('C:/Users/salav/OneDrive/Desktop/Disertatie/Data.csv')
Data <- Data[c(-1)]
#1. crearea seturilor de antrenare si testare
names(Data)
str(Data)
set.seed(123)
sample_data = sample.split(Data, SplitRatio = 0.80)
train_data <- subset(Data, sample_data == TRUE)
test_data <- subset(Data, sample_data == FALSE)
#rtree_1 <- rpart(mobile_banking ~ age + gender + marital_status + wages, train_data)#
#rpart.plot(rtree_1)
c_tree <- ctree(mobile_banking ~ ., train_data)
c_tree
# as.simpleparty(c_tree)
plot(c_tree)
pred_tree = predict(c_tree, test_data)
pred_tree = predict(c_tree, 
                        type = 'response') 

pred_tree<- ifelse(pred_tree > 0.5, 1, 0)

pred_tree_factor <- as.factor(pred_tree)
cm = confusionMatrix(as.factor(test_data$mobile_banking), pred_tree_factor)
print(cm)
r_tree <- rpart(mobile_banking ~ ., train_data)
rpart.plot(r_tree)
pred_tree_new = predict(r_tree, test_data)
pred_tree_new = predict(r_tree, 
                    type = 'response') 

pred_tree_new<- ifelse(pred_tree_new > 0.5, 1, 0)

pred_tree_new_factor <- as.factor(pred_tree_new)
cm_new = confusionMatrix(as.factor(test_data$mobile_banking), pred_tree_new_factor)
print(cm_new)
