install.packages("readxl")
library(readxl)
df<- read_excel("C:/Users/aa/OneDrive/Documents/Data science 360/Assignments & Projects/R case study/4. R - Linear Regression case study/Linear Regression Case.xlsx")
View(df)


### Checking the data type ###
View(data.frame(sapply(df,class)))


#################################### DATA PREPARATION I ##############################################

### Converting into correct datatype ###
df$region<-as.character(df$region)
df$townsize<-as.character(df$townsize)
df$gender<-as.character(df$gender)
df$agecat<-as.character(df$agecat)
df$birthmonth<-as.character(df$birthmonth)
df$edcat<-as.character(df$edcat)
df$jobcat<-as.character(df$jobcat)
df$union<-as.character(df$union)
df$employ<-as.character(df$employ)
df$empcat<-as.character(df$empcat)
df$retire<-as.character(df$retire)
df$inccat<-as.character(df$inccat)
df$default<-as.character(df$default)
df$jobsat<-as.character(df$jobsat)
df$marital<-as.character(df$marital)
df$spousedcat<-as.character(df$spousedcat)
df$homeown<-as.character(df$homeown)
df$hometype<-as.character(df$hometype)
df$address<-as.character(df$address)
df$addresscat<-as.character(df$addresscat)
df$cars<-as.character(df$cars)
df$carown<-as.character(df$carown)
df$cartype<-as.character(df$cartype)
df$carcatvalue<-as.character(df$carcatvalue)
df$carbought<-as.character(df$carbought)
df$carbuy<-as.character(df$carbuy)
df$commute<-as.character(df$commute)
df$commutecat<-as.character(df$commutecat)
df$commutecar<-as.character(df$commutecar)
df$commutemotorcycle<-as.character(df$commutemotorcycle)
df$commutecarpool<-as.character(df$commutecarpool)
df$commutebus<-as.character(df$commutebus)
df$commuterail<-as.character(df$commuterail)
df$commutepublic<-as.character(df$commutepublic)
df$commutebike<-as.character(df$commutebike)
df$commutewalk<-as.character(df$commutewalk)
df$commutenonmotor<-as.character(df$commutenonmotor)
df$telecommute<-as.character(df$telecommute)
df$reason<-as.character(df$reason)
df$polview<-as.character(df$polview)
df$polparty<-as.character(df$polparty)
df$polcontrib<-as.character(df$polcontrib)
df$vote<-as.character(df$vote)
df$card<-as.character(df$card)
df$cardtype<-as.character(df$cardtype)
df$cardbenefit<-as.character(df$cardbenefit)
df$cardfee<-as.character(df$cardfee)
df$cardtenure<-as.character(df$cardtenure)
df$cardtenurecat<-as.character(df$cardtenurecat)
df$card2<-as.character(df$card2)
df$card2type<-as.character(df$card2type)
df$card2benefit<-as.character(df$card2benefit)
df$card2fee<-as.character(df$card2fee)
df$card2tenure<-as.character(df$card2tenure)
df$card2tenurecat<-as.character(df$card2tenurecat)
df$active<-as.character(df$active)
df$bfast<-as.character(df$bfast)
df$churn<-as.character(df$churn)
df$tollfree<-as.character(df$tollfree)
df$equip<-as.character(df$equip)
df$callcard<-as.character(df$callcard)
df$wireless<-as.character(df$wireless)
df$multline<-as.character(df$multline)
df$voice<-as.character(df$voice)
df$pager<-as.character(df$pager)
df$internet<-as.character(df$internet)
df$callid<-as.character(df$callid)
df$callwait<-as.character(df$callwait)
df$forward<-as.character(df$forward)
df$confer<-as.character(df$confer)
df$ebill<-as.character(df$ebill)
df$owntv<-as.character(df$owntv)
df$ownvcr<-as.character(df$ownvcr)
df$owndvd<-as.character(df$owndvd)
df$owncd<-as.character(df$owncd)
df$ownpda<-as.character(df$ownpda)
df$ownpc<-as.character(df$ownpc)
df$ownipod<-as.character(df$ownipod)
df$owngame<-as.character(df$owngame)
df$ownfax<-as.character(df$ownfax)
df$news<-as.character(df$news)
df$response_01<-as.character(df$response_01)
df$response_02<-as.character(df$response_02)
df$response_03<-as.character(df$response_03)

View(data.frame(sapply(df, class)))


### Total Credit Card Spend (Primary + Secondary Card)###
df$total_spend <- df$cardspent + df$card2spent


### Removing variables from dataframe ###
df$custid <- NULL 
df$birthmonth <- NULL 
df$carditems <- NULL
df$cardspent <- NULL
df$card2spent <- NULL
df$card2items <- NULL


### Splitting into Numerical and Categorical ###
isnumeric <- sapply(df, is.numeric)
df_numeric <- df[,isnumeric]

df_character <- df[,!isnumeric]


### Checking & removing variables with higher percentage of missing values ###
percent_missing_num <- as.data.frame(sort(apply(df_numeric, 2,function(col)sum(is.na(col))/length(col)*100)),descending=TRUE)
percent_missing_num

df_numeric$lncardmon<- NULL                                                                           
df_numeric$lncardten<- NULL                                                                           
df_numeric$lntollmon<- NULL                                                                           
df_numeric$lntollten<- NULL                                                                           
df_numeric$lnequipmon<-NULL                                                                           
df_numeric$lnequipten<-NULL                                                                           
df_numeric$lnwiremon<- NULL                                                                           
df_numeric$lnwireten<- NULL


### Creating a UDF to analyse the numeric variables ###
mystats <- function(x){
  n = length(x)
  nmiss = sum(is.na(x))
  nmiss_pct = mean(is.na(x))
  sum = sum(x, na.rm=T)
  mean = mean(x, na.rm=T)
  median = quantile(x, p=0.5, na.rm=T)
  std = sd(x, na.rm=T)
  var = var(x, na.rm=T)
  range = max(x, na.rm=T)-min(x, na.rm=T)
  pctl = quantile(x, p=c(0, 0.01, 0.05,0.1,0.25,0.5, 0.75,0.9,0.95,0.99,1), na.rm=T)
  return(c(N=n, Nmiss =nmiss, Nmiss_pct = nmiss_pct, sum=sum, avg=mean, meidan=median, std=std, var=var, range=range, pctl=pctl))
}

analysis.num.vars <- data.frame(t(apply(df_numeric, 2, mystats)))
View(analysis.cat.vars)
write.csv(analysis.num.vars, file = 'analysis.csv')

### Outlier and Missing value treatment (Numeric Variable) ###
outlier_treatment <- function(x){
  UC = quantile(x, p = 0.99, na.rm = T)
  LC = quantile(x, p = 0.01, na.rm = T)
  x = ifelse(x>UC, UC, x)
  x = ifelse(x<LC, LC, x)
  return(x)
}

missing_value_treatment <- function(x){
  x[is.na(x)] = mean(x, na.rm = T)
  return(x)
}

df_outlier_treated <- data.frame(apply(df_numeric, 2, FUN = outlier_treatment))

df_outlier_missing_treated <- data.frame(apply(df_outlier_treated, 2, FUN = missing_value_treatment))

analysis.num.vars.treated <- data.frame(t(apply(df_outlier_missing_treated, 2, mystats)))
View(analysis.num.vars.treated)
write.csv(analysis.num.vars.treated, file = 'analysis_treated.csv')



### Missing value teatment (Categorical variables) ###
mystats_cat=function(x){
  Var_Type=class(x)
  n=length(x)
  nmiss=sum(is.na(x))
  return( c(Var_Type=Var_Type, n=n,nmiss=nmiss))
}

analysis.cat.vars <-  data.frame(t(apply(df_character,2, mystats_cat)))
View(analysis.cat.vars)

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

missing_value_treatment_categorical <- function(x){
  x[is.na(x)] = Mode(na.omit(x))
  return(x)  
}


df_char_missing_treated <- data.frame(apply(df_character,2,FUN = missing_value_treatment_categorical))
View(df_char_missing_treated)
write.csv(df_char_missing_treated, file = 'dfchar.csv')


### Dummy Variable Creation ###
require(fastDummies)

df_char_missing_treated_with_dummies <- fastDummies::dummy_cols(df_char_missing_treated, remove_first_dummy = T)
View(df_char_missing_treated_with_dummies)

Vars_to_exclude <- names(df_character)

df_char_missing_treated_with_dummies <- dplyr::select(df_char_missing_treated_with_dummies, -Vars_to_exclude)
View(df_char_missing_treated_with_dummies)

df_clean <- cbind(df_outlier_missing_treated,df_char_missing_treated_with_dummies)
View(sapply(df_clean,class))



######################################## DATA PREPARATION II #############################################

# Assumption 1: Y being Normal
require(ggplot2)

df_clean$log_total_spend <- log(df_clean$total_spend)
ggplot(data = df_clean) + aes(log_total_spend) + geom_histogram(bins = 10, fill = 'blue', color = 'white')

df_clean$total_spend <- NULL

# Conclusion Assumption 1: log of Y is Normally distributed


# Assumption 2 and 3 : Correlation between X and Y and X and X should not be there
correlation <- data.frame(cor(df_outlier_missing_treated))
View(correlation)
require(writexl)
writexl::write_xlsx(correlation, 'correlation.xlsx')

# Conclusion Assumption 2 and 3: A number of X variables have a decent correlation with Y
# Final Conclusion: Assumptions are fulfilled & linear regression model seems to be fine on this data


######################################## DATA PREPARATION 3 ###########################################

# creating 2 objects, one with all the x vars and other with the Y variable
features <- dplyr::select(df_clean, -log_total_spend)
Y_var <- dplyr::select(df_clean, log_total_spend)

# for running some of the feature selection methods, we need the data in the form of a matrix
features <- data.matrix(features)
Y_var <- data.matrix(Y_var)


##### Stepwise Regression #####

# Creating a full and an empty model

# Full Model
modelF <- lm(log_total_spend~.,data=df_clean)
modelF

# Empty Model
modelnull <- lm(log_total_spend~1,data=df_clean)
modelnull

# Running the Stepwise Regression to find the imp variables
stepwise.model <- step(modelnull, scope = list(upper = modelF),   data = df_clean, direction = "both")

#Output-
#  lninc + reason_2 + card_4 + card_3 + card_2 + 
#  card_5 + card2_2 + card2_4 + card2_3 + card2_5 + reason_9 + 
#  age + gender_1 + cardtenure_39 + card2tenure_26 + address_32 + 
#  ed + region_5 + card2benefit_3 + internet_3 + cardtenure_23 + 
#  internet_4 + cardmon + employ_32 + polview_7 + employ_6 + 
#  card2tenure_23 + card2tenure_19 + card2tenure_20 + jobcat_6 + 
#  address_53 + response_03_1 + employ_17 + employ_26 + spousedcat_2 + 
#  address_19 + jobcat_4 + jobcat_5 + employ_25 + employ_39 + 
#  cars_1 + card2tenure_8 + card2tenure_15 + union_1 + voice_1 + 
#  employ_44 + polview_2 + cardtenure_17 + cardtenure_28 + pets_saltfish + 
#  address_51 + employ_43 + employ_13 + address_8 + cardtenurecat_2 + 
#  card2tenurecat_2 + carbuy_1 + employ_1 + cardtenure_38 + 
#  default_1 + carvalue + spousedcat_5 + address_28


#### Boruta #####


install.packages("Boruta")
library(Boruta)

set.seed(123)

boruta.train <- Boruta(log_total_spend~., data = df_clean, doTrace = 2)
print(boruta.train)

final.boruta <- TentativeRoughFix(boruta.train)
print(final.boruta)

getSelectedAttributes(final.boruta, withTentative = F)

#Final Result
# "age"         "income"       "lninc"            "debtinc"          "creddebt"        
# "lncreddebt"  "othdebt"      "lnothdebt"        "carvalue"         "tenure"          
# "longmon"     "lnlongmon"    "longten"          "lnlongten"        "tollmon"         
# "tollten"     "equipmon"     "equipten"         "cardmon"          "cardten"         
# "wiremon"     "wireten"      "agecat_6"         "retire_1"         "inccat_2"        
# "inccat_3"    "inccat_4"     "inccat_5"         "addresscat_5"     "carown_0"        
# "carown_1"    "carcatvalue_1" "carcatvalue_2"    "carcatvalue_3"    "reason_2"        
# "card_2"      "card_3"       "card_4"           "cardtenurecat_5"  "card2_2"         
# "card2_3"     "card2_4"      "card2tenurecat_5" "bfast_2"          "ownvcr_1"        
# "owndvd_1"    "owncd_1"        


Level1.Selected.Vars <- c(
  "lninc" , "reason_2" , "card_4" , "card_3", "card_2" , 
    "card_5" , "card2_2" , "card2_4" , "card2_3" , "card2_5" , "reason_9" , 
    "age" , "gender_1" , "cardtenure_39" , "card2tenure_26" , "address_32" , 
    "ed" , "region_5" , "card2benefit_3" , "internet_3" , "cardtenure_23" , 
    "internet_4" , "cardmon" , "employ_32" , "polview_7" , "employ_6" , 
    "card2tenure_23" , "card2tenure_19" , "card2tenure_20" , "jobcat_6" , 
    "address_53" , "response_03_1" , "employ_17" , "employ_26" , "spousedcat_2" , 
    "address_19" , "jobcat_4" , "jobcat_5" , "employ_25" , "employ_39" , 
    "cars_1" , "card2tenure_8" , "card2tenure_15" , "union_1" , "voice_1" , 
    "employ_44" , "polview_2" , "cardtenure_17" , "cardtenure_28" , "pets_saltfish" ,
    "address_51" , "employ_43" , "employ_13" , "address_8" , "cardtenurecat_2" , 
    "card2tenurecat_2" , "carbuy_1" , "employ_1" , "cardtenure_38" , 
    "default_1" , "carvalue" , "spousedcat_5" , "address_28",
  "age"        , "income"      , "lninc"         , "debtinc"       ,"creddebt" ,    
  "lncreddebt" , "othdebt"     , "lnothdebt"     , "carvalue"      ,"tenure"   ,    
  "longmon"    , "lnlongmon"   , "longten"       , "lnlongten"     ,"tollmon"  ,    
  "tollten"    , "equipmon"    , "equipten"      , "cardmon"       ,"cardten"  ,    
  "wiremon"    , "wireten"     , "agecat_6"      , "retire_1"      ,"inccat_2" ,    
  "inccat_3"   , "inccat_4"    , "inccat_5"      , "addresscat_5"  ,"carown_0" ,    
  "carown_1"   , "carcatvalue_1", "carcatvalue_2",  "carcatvalue_3", "reason_2",    
  "card_2"     , "card_3" , "card_4" ,"cardtenurecat_5",  "card2_2" ,     
  "card2_3"    , "card2_4","card2tenurecat_5", "bfast_2", "ownvcr_1",     
  "owndvd_1"   , "owncd_1"        

)

length(Level1.Selected.Vars)


df_selected_vars1 <- df_clean[Level1.Selected.Vars]

df_selected_vars1 <- cbind(df_selected_vars1,Y_var)
length(df_selected_vars1)


##### LASSO #####

lasso = train(log_total_spend~.,
              data=df_selected_vars1, method='glmnet',
              trControl = trainControl(method="none"),
              tuneGrid=expand.grid(alpha=1,lambda=0.03))

coef(lasso$finalModel, s = lasso$bestTune$lambda)


Excluded_Vars_Lasso <- c("card2_4","card2_5","reason_9","age","cardtenure_39","card2tenure_26","address_32",
"ed","region_5","card2benefit_3","internet_3","cardtenure_23","internet_4","cardmon","employ_32","polview_7","employ_6","card2tenure_23",
"card2tenure_19","card2tenure_20","jobcat_6","address_53","response_03_1","employ_17","employ_26","spousedcat_2","address_19","jobcat_4",
"jobcat_5","employ_25","employ_39","cars_1","card2tenure_8","card2tenure_15","union_1","voice_1","employ_44",
"polview_2","cardtenure_17","cardtenure_28","pets_saltfish","address_51","employ_43","employ_13","address_8",
"cardtenurecat_2","card2tenurecat_2","carbuy_1","employ_1","cardtenure_38","default_1","carvalue","spousedcat_5","address_28","age.1",
"income","debtinc","creddebt","lncreddebt","othdebt","lnothdebt","carvalue.1","tenure","longmon","lnlongmon",
"longten","lnlongten","tollmon","tollten","equipmon","equipten","cardmon.1","cardten","wiremon","wireten","agecat_6","inccat_2",
"inccat_3","inccat_4","inccat_5","addresscat_5","carown_0","carown_1","carcatvalue_1","carcatvalue_2",
"carcatvalue_3","cardtenurecat_5","card2_4.1","card2tenurecat_5","bfast_2","ownvcr_1","owndvd_1","owncd_1")


df_selected_vars2<-dplyr::select(df_selected_vars1,-Excluded_Vars_Lasso)
length(df_selected_vars2)

df_selected_vars2



##################################### Data Prep Level 4 #################################################

# Diving the data into train (development) and test (Validation)

selected_rows <- sample(1:nrow(df_selected_vars2), floor(nrow(df_selected_vars2)*0.7))

dev <- df_selected_vars2[selected_rows,]
val <- df_selected_vars2[-selected_rows,]

nrow(df_selected_vars2)
nrow(dev)
nrow(val)

nrow(dev) / nrow(df_selected_vars2)  # 70% in the train data (dev data)
nrow(val) / nrow(df_selected_vars2)  # 30% in the test data (val data)



##### FITTING THE MODEL #####

# Iteration 1
model1 <- lm(log_total_spend~lninc+
               reason_2+
               card_4+
               card_3+
               card_2+
               card_5+
               card2_2+
               card2_3+
               gender_1+
               lninc.1+
               retire_1+
               reason_2.1+
               card_2.1+
               card_3.1+
               card_4.1+
               card2_2.1+
               card2_3.1,
             data = dev)

summary(model1)

# Iteration 3
model3 <- lm(log_total_spend~lninc+
               reason_2+
               card_4+
               card_3+
               card_2+
               card_5+
               card2_2+
               card2_3+
               gender_1+
               retire_1+
               card_2.1+
               card_3.1+
               card_4.1+
               card2_2.1+
               card2_3.1,
             data = dev)

summary(model3)

# Iteration 4
model4 <- lm(log_total_spend~lninc+
               reason_2+
               card_4+
               card_3+
               card_2+
               card_5+
               card2_2+
               card2_3+
               gender_1+
               retire_1+
               card_3.1+
               card_4.1+
               card2_2.1+
               card2_3.1,
             data = dev)

summary(model4)

# Iteration 5
model5 <- lm(log_total_spend~lninc+
               reason_2+
               card_4+
               card_3+
               card_2+
               card_5+
               card2_2+
               card2_3+
               gender_1+
               retire_1+
               card_4.1+
               card2_2.1+
               card2_3.1,
             data = dev)

summary(model5)

# Iteration 6
model6 <- lm(log_total_spend~lninc+
               reason_2+
               card_4+
               card_3+
               card_2+
               card_5+
               card2_2+
               card2_3+
               gender_1+
               retire_1+
               card2_2.1+
               card2_3.1,
             data = dev)

summary(model6)

# Iteration 7
model7 <- lm(log_total_spend~lninc+
               reason_2+
               card_4+
               card_3+
               card_2+
               card_5+
               card2_2+
               card2_3+
               gender_1+
               retire_1+
               card2_3.1,
             data = dev)

summary(model7)

# Iteration 8
model8 <- lm(log_total_spend~lninc+
               reason_2+
               card_4+
               card_3+
               card_2+
               card_5+
               card2_2+
               card2_3+
               gender_1+
               retire_1,
             data = dev)

summary(model8)

################# POST MODELING ###################################
dev1 <- data.frame(cbind(dev,pred = predict(model8, newdata = dev)))
val1 <- data.frame(cbind(val,pred = predict(model8, newdata = val)))


## Running accuracy metrics on the dev and val dataset ##

# MAPE
require(Metrics)
Metrics::mape(dev1$log_total_spend, dev1$pred)
Metrics::mape(val1$log_total_spend, val1$pred)

# RMSE
Metrics::rmse(dev1$log_total_spend, dev1$pred)
Metrics::rmse(val1$log_total_spend, val1$pred)

# R2
install.packages("MLmetrics")
require(MLmetrics)
MLmetrics::R2_Score(dev1$pred, dev1$log_total_spend)
MLmetrics::R2_Score(val1$pred, val1$log_total_spend)

# Using the concept of Cook's distance to further reduce the error (increase the accuracy) #
cooksD <- cooks.distance(model8)
SampleSize <- nrow(dev)

par(mar=c(1,1,1,1))
plot(cooksD, pch = "*", cex = 2, main = "The data points that are influential as per Cooks Distance")
abline(h = 4/SampleSize, col = "red3")
text(x=1:length(cooksD)+1, y=cooksD, labels=ifelse(cooksD>4/SampleSize, names(cooksD),""), col="red3") 

# Removing those observations that are influencing the line of best fit

influential <- as.numeric(names(cooksD)[(cooksD > (4/SampleSize))])
dev.CooksD <- dev[-influential,]

View(dev.CooksD)

# Iteration 9
model9 <- lm(log_total_spend~lninc+
               reason_2+
               card_4+
               card_3+
               card_2+
               card_5+
               card2_2+
               card2_3+
               gender_1+
               retire_1,
             data = dev.CooksD)

summary(model9)

## Model Validation : Predict the Y for the train and test using new model ##
dev1 <- data.frame(cbind(dev,pred = predict(model9, newdata = dev)))
val1 <- data.frame(cbind(val,pred = predict(model9, newdata = val)))


## Running accuracy metrics on the dev and val dataset ##

# MAPE
require(Metrics)
Metrics::mape(dev1$log_total_spend, dev1$pred)
Metrics::mape(val1$log_total_spend, val1$pred)

# RMSE
Metrics::rmse(dev1$log_total_spend, dev1$pred)
Metrics::rmse(val1$log_total_spend, val1$pred)

# R2
install.packages("MLmetrics")
require(MLmetrics)
MLmetrics::R2_Score(dev1$pred, dev1$log_total_spend)
MLmetrics::R2_Score(val1$pred, val1$log_total_spend)


## DECILE ANALYSIS ##

# Decile Analysis of the Development Data 
dev11 <- dev1

# Calculating the deciles using the predicted Y
DecileValues <- quantile(dev11$pred, probs = seq(0.1,0.9, by = 0.1))

# Calculating the Decile Intervals
dev11$decile <- findInterval(dev11$pred, c(-Inf,DecileValues,Inf ))

# To check if the deciling has been done properly
summary(dev11$decile)

xtabs(~decile, dev11)

dev11 %>% dplyr::group_by(decile) %>% summarise(AvgPred = mean(pred))

dev111 <- dev11[,c("decile","log_total_spend","pred")]

colnames(dev111) <- c("decile","log_total_spend","pred")


require(sqldf)

DA.dev <- sqldf::sqldf("select decile,
                       count(decile) as count,
                       avg(pred) as avg_pred,
                       avg(log_total_spend) as avg_log_total_spend
                       from dev111
                       group by decile
                       order by decile")

View(DA.dev)


writexl::write_xlsx(DA.dev, "Decile_Analysis.xlsx")


# Decile Analysis of the validation Data 

# Created a copy of the val1 dataset
val11 <- val1

# Calculating the deciles using the predicted Y
DecileValues <- quantile(val11$pred, probs = seq(0.1,0.9, by = 0.1))


# Calculating the Decile Intervals
val11$decile <- findInterval(val11$pred, c(-Inf,DecileValues,Inf ))

# To check if the deciling has been done properly
summary(val11$decile)

xtabs(~decile, val11)

val11 %>% dplyr::group_by(decile) %>% summarise(AvgPred = mean(pred))

val111 <- val11[,c("decile","log_total_spend","pred")]

colnames(val111) <- c("decile","log_total_spend","pred")


require(sqldf)

DA.val <- sqldf::sqldf("select decile,
                       count(decile) as count,
                       avg(pred) as avg_pred,
                       avg(log_total_spend ) as avg_log_total_spend
                       from val111
                       group by decile
                       order by decile")
View(DA.val)


writexl::write_xlsx(DA.val, "Decile_Analysis_Validation.xlsx")

library(caret)
final=as.data.frame(varImp(model9))
View(final)
write.csv(final,"final.csv")

