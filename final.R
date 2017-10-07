###################################################################################################
# Business Model 
# How does the lending process work?

# Business Objective
# The company wants to understand the driving factors behind default. 
# If one is able to identify these risky loan applicants,
# then such loans can be reduced thereby cutting down the amount of credit loss. 

###################################################################################################

library("ggplot2")
library("dplyr")
library("tidyr")
library("readr")
ibrary("scales")

test_raw <- read.csv("raw_data_30_new.csv",header = TRUE,stringsAsFactors = FALSE)
test_acc <- read.csv("raw_account_30_new.csv",header = TRUE,stringsAsFactors = FALSE)
#shuffledata_test <- test_raw[sample(nrow(test_raw)), ]


train_raw <- read.csv("raw_data_70_new.csv",header = TRUE,stringsAsFactors = FALSE)
train_acc <- read.csv("raw_account_70_new.csv",header = TRUE,stringsAsFactors = FALSE)
#shuffledata_train <- train_raw[sample(nrow(train_raw)), ]
train_plot <- merge(x=train_raw , y=train_acc , by="customer_no" , all.x  = TRUE)


#####################################################################################################
######################### Univariate Analysis ########################################

# Function for distribution of categorical variables (plotting bar charts)

univariate_categorical <- function(dataset,var,var_name){
  
  dataset %>% ggplot(aes(x = as.factor(var))) +
    geom_bar(aes(y = (..count..)/sum(..count..))) +
    geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) + 
    scale_y_continuous(labels = percent) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(title = var_name, y = "Percent", x = var_name)+theme(
      axis.text.y=element_blank(), axis.ticks=element_blank(),
      axis.title.y=element_blank()
    ) 
}


train_plot$feature_1 <- as.factor(train_plot$feature_1)
univariate_categorical(train_plot,train_plot$feature_1,"Application feature_1")

train_plot$feature_32 <- as.factor(train_plot$feature_32)
univariate_categorical(train_plot,train_plot$feature_32,"Application feature_32")

train_plot$feature_36 <- as.factor(train_plot$feature_36)
univariate_categorical(train_plot,train_plot$feature_36,"Application feature_36")

train_plot$feature_46 <- as.factor(train_plot$feature_46)
univariate_categorical(train_plot,train_plot$feature_46,"Application feature_46")

train_plot$feature_28 <- as.factor(train_plot$feature_28)
univariate_categorical(train_plot,train_plot$feature_28,"Application feature_28")

#######################################################################################
# facet for categorical vars
categorical_bivariate <- function(dataset, var, var_name){
  plot_bi = ggplot(dataset, aes(x=var))+geom_bar()+facet_wrap(~train_plot$Bad_label) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
  return(plot_bi)
}

categorical_bivariate(train_plot,train_plot$feature_1,"feature_1")

categorical_bivariate(train_plot,train_plot$feature_32,"feature_32")

categorical_bivariate(train_plot,train_plot$feature_36,"feature_36")

categorical_bivariate(train_plot,train_plot$feature_46,"feature_46")

categorical_bivariate(train_plot,train_plot$feature_28,"feature_28")


change_data <- function(df1,df2,num_rows){
  
  #formatting Last payment and end date column of raw_account to find out whether he paid late and change to factor 
  df2$last_paymt_dt <- as.Date(df2$last_paymt_dt,format = "%d-%b-%y")
  df2$paymt_end_dt <- as.Date(df2$paymt_end_dt,format = "%d-%b-%y")
  df2$late_payment_status <- ifelse(df2$last_paymt_dt > df2$paymt_end_dt , 1,0)
  
  #transforming paymentHistory to categorical depending on payment status of last 18 months
  df2$paymnthiscategory1 <- ifelse(grepl("LSS", df2$paymenthistory1), 'loss' , ifelse(grepl("SUB", df2$paymenthistory1), 'risk', ifelse(grepl("DBT", df2$paymenthistory1), 'risk' , 'ok')))
  df2$paymnthiscategory2 <- ifelse(grepl("LSS", df2$paymenthistory2), 'loss' , ifelse(grepl("SUB", df2$paymenthistory2), 'risk', ifelse(grepl("DBT", df2$paymenthistory2), 'risk' , 'ok')))
  
  #extracting relevant columns from raw_account csv
  pretty_df2 <- df2[,c(2,4,5,10,11,12,17,18,19,20,21,22,23)]
  
  ##Customer Data
  
  #Since mobile is encoded not getting idea so tranforming it to whether present or not
  df1$is_mobile <- ifelse(df1$feature_77 == "XXXXX",0,1)
  #Extracting relevant information from customer data
  df1 <- df1[,c(2,4,6,7,8,9,10,12,14,22,26,28,30,31,35,36,37,38,39,40,49,52,53,55,58,60,62,63
                ,64,70,71,72,75,76,82,83,84)]
  
  
  ##Checking % of different categorical variables where there are 3-4 categorical variables and trying to club them
  #so as to reduce feature_set for analysing
  
  #feature_4
  df1 %>% group_by(feature_4) %>% summarise(count=n()*100/nrow(df1))
  #Since 2 and NA has very less than 10% value merging them to 1
  df1$feature_4 <- ifelse(is.na(df1$feature_4) | 
                            df1$feature_4 == "2", '1',df1$feature_4) 
  
  #feature_6
  df1 %>% group_by(feature_6) %>% summarise(count=n()*100/nrow(df1))
  #Since only 0.06% of row doesnt has values so removing this column
  df1 <- df1[,-6]
  
  #feature_11
  df1 %>% group_by(feature_11) %>% summarise(count=n()*100/nrow(df1))
  #since 0.06% of data doesnt have any value clubing it with N value
  df1$feature_11 <- ifelse(df1$feature_11 == "", 'N',df1$feature_11) 
  
  #feature_19
  df1 %>% group_by(feature_19) %>% summarise(count=n()*100/nrow(df1))
  #since 0.06% of data is NA clubbing it with 2
  df1$feature_19 <- ifelse(is.na(df1$feature_19), '2',df1$feature_19) 
  
  #feature_23
  df1 %>% group_by(feature_23) %>% summarise(count=n()*100/nrow(df1))
  #since 0.06% of data is Blank clubbing it with Y
  df1$feature_23 <- ifelse(df1$feature_23 == "", 'Y',df1$feature_23)   
  
  #feature_25
  df1 %>% group_by(feature_25) %>% summarise(count=n()*100/nrow(df1))
  #since 0.06% of data is NA clubbing it with 2
  df1$feature_25 <- ifelse(is.na(df1$feature_25), '2',df1$feature_25)   
  
  #feature_33
  df1 %>% group_by(feature_33) %>% summarise(count=n()*100/nrow(df1))
  #since 0.06% of data is blank clubbing it with N
  df1$feature_33 <- ifelse(df1$feature_33 == "", 'N',df1$feature_33)   
  
  
  #feature_34
  df1 %>% group_by(feature_34) %>% summarise(count=n()*100/nrow(df1))
  #clubbing 3 and NA value to 2
  df1$feature_34 <- ifelse(is.na(df1$feature_34) | df1$feature_34 == '3', '2',df1$feature_34) 
  
  #feature_50
  df1 %>% group_by(feature_50) %>% summarise(count=n()*100/nrow(df1))
  #clubbing blank value to N
  df1$feature_50 <- ifelse(df1$feature_50 == '', 'N',df1$feature_50) 
  
  #feature_55
  df1 %>% group_by(feature_55) %>% summarise(count=n()*100/nrow(df1))
  #clubbing 3,4 and NA value to 2
  df1$feature_55 <- ifelse(is.na(df1$feature_55) | df1$feature_55 == '3'
                           | df1$feature_55 == '4', '2',df1$feature_55) 
  
  
  #feature_57
  df1 %>% group_by(feature_57) %>% summarise(count=n()*100/nrow(df1))
  #clubbing N and Y value to Y
  df1$feature_57 <- ifelse(df1$feature_57 != '', 'Y',df1$feature_57) 
  
  #feature_59
  df1 %>% group_by(feature_59) %>% summarise(count=n()*100/nrow(df1))
  #clubbing blank to N
  df1$feature_59 <- ifelse(df1$feature_59 == '', 'N',df1$feature_59) 
  
  #feature_60
  df1 %>% group_by(feature_60) %>% summarise(count=n()*100/nrow(df1))
  #clubbing blank to Y
  df1$feature_60 <- ifelse(df1$feature_60 == '', 'Y',df1$feature_60) 
  
  #feature_67
  df1 %>% group_by(feature_67) %>% summarise(count=n()*100/nrow(df1))
  #clubbing NA to 1
  df1$feature_67 <- ifelse(is.na(df1$feature_67) , '1',df1$feature_67)
  
  #feature_68
  df1 %>% group_by(feature_68) %>% summarise(count=n()*100/nrow(df1))
  #clubbing 3 and NA value to 2
  df1$feature_68 <- ifelse(is.na(df1$feature_68) | 
                             df1$feature_68 == '3', '2',df1$feature_68) 
  
  #feature_72
  df1 %>% group_by(feature_72) %>% summarise(count=n()*100/nrow(df1))
  #clubbing blank to O
  df1$feature_72 <- ifelse(df1$feature_72 == '' , 'O',df1$feature_72)
  
  
  #feature_73
  df1 %>% group_by(feature_73) %>% summarise(count=n()*100/nrow(df1))
  #clubbing N value to Y
  df1$feature_73 <- ifelse(df1$feature_73 == 'N' , 'Y',df1$feature_73)
  
  
  #feature_79
  df1 %>% group_by(feature_79) %>% summarise(count=n()*100/nrow(df1))
  #clubbing blank value to Y
  df1$feature_79 <- ifelse(df1$feature_79 == '' , 'Y',df1$feature_79)
  
  #shuffle(df1)
  
  #Since data is large selecting less number of rows 
  limit_data <- df1[1:num_rows,]
  #Merging with Left Join
  merge1 <- merge(x=limit_data , y=pretty_df2 , by="customer_no" , all.x  = TRUE)
  
  #Transforming payment category
  merge1 %>% group_by(paymentfrequency) %>% summarise(count=n()*100/nrow(merge1))
  merge1$paymentfrequency <- ifelse(merge1$paymentfrequency == '1', '3',merge1$paymentfrequency)
  
  #Transforming payment status
  merge1 %>% group_by(late_payment_status) %>% summarise(count=n()*100/nrow(merge1))
  merge1$late_payment_status <- ifelse(is.na(merge1$late_payment_status), '0',merge1$late_payment_status)
  
  ########################################################################################
  ###  SELECTING THOSE COLUMNS WHICH I THINK IS IMPORTANT AND MANAGEABLE FOR ANALYSIS  ###
  ########################################################################################
  #Name of the factor variables
  #asFactorV <- c("feature_1","feature_4",
  #               "feature_9","feature_11","feature_19","feature_23","feature_25"
  #               ,"feature_27","feature_28","feature_32","feature_33","feature_34","feature_36","feature_37"
  #               ,"feature_46","feature_50","feature_55","feature_57","feature_59","feature_60","feature_67"
  #               ,"feature_68","feature_72","feature_73","feature_79","Bad_label","is_mobile","acct_type","owner_indic"
  #               ,"paymentfrequency","late_payment_status","paymnthiscategory1")
  
  asFactorV <- c(4,7,8,9,10,11,12,13,14,15,16,18,19,20,22,24,25,26,27,29,30,32,33,35,36,37,38,45,47,48)
  
  #Name of numeric variables
  #asNumber <- c("feature_3","feature_7","feature_35","feature_49","feature_52","feature_69","high_credit_amt"
  #              ,"cur_balance_amt","amt_past_due","creditlimit","cashlimit","rateofinterest","actualpaymentamount"
  #)
  
  asNumber <- c(3,6,17,21,23,31,39,40,41,42,43,44,46)
  
  # creating a dataframe of categorical features
  data_fact<- merge1[, asFactorV]
  data_fact <- as.data.frame(sapply(data_fact,as.factor))
  #sapply(data_fact,class)
  
  #Since categorical variable so converting blank data to 0 so as to be categorised
  data_fact <- as.data.frame(sapply(data_fact , function(x) ifelse(x == "" , '0',x)))
  
  #Changing it to 0 so as to categorise
  data_fact$paymentfrequency <- ifelse(is.na(data_fact$paymentfrequency),'0',data_fact$paymentfrequency)
  
  
  # creating dummy variables for factor attributes
  dummies<- data.frame(sapply(data_fact, 
                              function(x) data.frame(model.matrix(~x-1,data =data_fact))[,-1]))
  
  
  #INTEGER_VAR <- lapply(merge1, class) == "integer" | lapply(merge1, class) == "numeric"
  data_num <- merge1[, asNumber]
  #colMeans(is.na(data_num))
  
  #removing feature_49 column because 96% of data doesnt have value
  data_num <- data_num[,-4]
  
  #replacing NA values with 0
  data_num <- as.data.frame(sapply(data_num,function(x) ifelse(is.na(x),0,x)))
  
  #sapply(data_num,class)
  data_num <- as.data.frame(sapply(data_num,as.numeric))
  
  # Standardising all numerical features
  std_number <- as.data.frame(sapply(data_num,function(x) scale(x)))
  
  final_data <- cbind(dummies,std_number)
  
  
  return(final_data)
}



#write.csv(test,'test.csv')
#write.csv(train,'train.csv')
set.seed(100)
test <- change_data(test_raw,test_acc,800)
train <- change_data(train_raw,train_acc,800)


########################################################################
# Logistic Regression: 

#Initial model
model_1 = glm(Bad_label ~ ., data = train, family = "binomial",maxit = 800)
summary(model_1)

# Stepwise selection
library("MASS")
model_2<- stepAIC(model_1, direction="both")

summary(model_2)

# Removing multicollinearity through VIF check
library(car)
vif(model_2)


#Removinf feature_27.x10,feature_37.x9,feature_55,acct_type.x23,feature_68
model_3 <- glm(formula = Bad_label ~ feature_4 + feature_9.x2 + feature_9.x3 + 
                 feature_11 + feature_19 + feature_23 + feature_25 + 
                 feature_27.x2 + feature_27.x3 + feature_27.x4 + feature_27.x5 + 
                 feature_27.x6 + feature_27.x7 + feature_27.x8 + feature_27.x9 + 
                 feature_28.x10 + feature_28.x11 + feature_28.x12 + feature_28.x13 + 
                 feature_28.x14 + feature_28.x15 + feature_28.x16 + feature_28.x17 + 
                 feature_28.x18 + feature_28.x19 + feature_28.x2 + feature_28.x20 + 
                 feature_28.x21 + feature_28.x3 + feature_28.x4 + feature_28.x5 + 
                 feature_28.x6 + feature_28.x7 + feature_28.x8 + feature_28.x9 + 
                 feature_32.x2 + feature_32.x3 + feature_32.x4 + feature_33 + 
                 feature_34 + feature_36.x2 + feature_36.x3 + feature_36.x4 + 
                 feature_36.x5 + feature_36.x6 + feature_37.x2 + feature_37.x3 + 
                 feature_37.x4 + feature_37.x5 + feature_37.x6 + feature_37.x7 + 
                 feature_37.x8 + feature_46.x2 + feature_46.x3 + 
                 feature_46.x4 + feature_46.x5 + feature_46.x6 + feature_50 + 
                 feature_57 + feature_59 + feature_60 + feature_67 + 
                 feature_72 + feature_73 + is_mobile + acct_type.x10 + 
                 acct_type.x11 + acct_type.x12 + acct_type.x13 + acct_type.x14 + 
                 acct_type.x15 + acct_type.x16 + acct_type.x17 + acct_type.x18 + 
                 acct_type.x19 + acct_type.x2 + acct_type.x20 + acct_type.x21 + 
                 acct_type.x22 + acct_type.x3 + acct_type.x4 + 
                 acct_type.x5 + acct_type.x6 + acct_type.x7 + acct_type.x8 + 
                 acct_type.x9 + owner_indic.x2 + owner_indic.x3 + owner_indic.x4 + 
                 paymentfrequency + late_payment_status + paymnthiscategory1.x2 + 
                 paymnthiscategory1.x3 + feature_3 + feature_7 + feature_35 + 
                 feature_69 + high_credit_amt + cur_balance_amt + amt_past_due + 
                 creditlimit + cashlimit + rateofinterest, family = "binomial", 
               data = train, maxit = 800)
vif(model_3)
summary(model_3)


### Test Data ####

#predicted probabilities of Attrition for test data

test_pred = predict(model_3, type = "response", 
                    newdata = test)

summary(test_pred)


test_pred_attrition <- factor(ifelse(test_pred > 0.5,'Yes','No'))
test_actual_attrition <- factor(ifelse(test$Bad_label == 1 , 'Yes','No'))

library(caret)
test_conf <- confusionMatrix(test_pred_attrition, test_actual_attrition, positive = "Yes")
test_conf


#########################################################################################
# Sensitivity is very low. So let's choose a different cutoff value

# Let's find out the optimal probalility cutoff 
# First let's create a function to find the accuracy, sensitivity and specificity
# for a given cutoff

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

# Creating cutoff values from 0.01 to 0.80 for plotting and initiallizing a matrix of 100 X 3.

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

cutoff

# Let's choose a cutoff value of 0.1 for final model

test_cutoff_attrition <- factor(ifelse(test_pred >=0.1, "Yes", "No"))

conf_final <- confusionMatrix(test_cutoff_attrition, test_actual_attrition, positive = "Yes")

acc <- conf_final$overall[1]

sens <- conf_final$byClass[1]

spec <- conf_final$byClass[2]

acc

sens

spec





class(test_pred_attrition)
class(test_actual_attrition)

test_cutoff_attrition <- ifelse(test_pred_attrition=="Yes",1,0)
test_actual_attrition <- ifelse(test_actual_attrition=="Yes",1,0)



##################################################################################################
### Exporting Data for Excel Analysis (KS, Gain, Lift etc.) ######

myeval <- matrix(nrow = length(test_pred),ncol = 2)
myeval[,1] <- test_pred
myeval[,2] <- test_actual_attrition
colnames(myeval) <- c("Predicted_Prob","Actual_Labels")
write.csv(myeval,"myeval.csv")



##################################################################################################
### KS -statistic - Test Data ######
install.packages()
library(ROCR)
#on testing  data
pred_object_test<- prediction(test_cutoff_attrition, test_actual_attrition)

performance_measures_test<- performance(pred_object_test, "tpr", "fpr")

ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - 
  (attr(performance_measures_test, "x.values")[[1]])

max(ks_table_test)


