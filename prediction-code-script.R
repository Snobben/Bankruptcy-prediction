#Setting working directory, where data file is located.
setwd("~/r-projects")
library(data.table)


#loading the dataset.

data = fread('data_aarsregnskaper.csv')

#initial simple investigation of the data.
dim(data)
str(data)

#Changing some variable data type to factor from character to be able to perform certain analysis.
data$fravalg_revisjon = as.factor(data$fravalg_revisjon) 
data$orgform = as.factor(data$orgform)
data$regler_smaa = as.factor(data$regler_smaa)
data$landkode = as.factor(data$landkode)
data$naeringskode = as.factor(data$naeringskode)

#Checking if theres too many or few accounts compared to organizations. Result: equal
library(sqldf)
table_1c = sqldf("SELECT 
                 regnaar, 
                 count (orgnr) AS antall_regnskap, 
                 count (DISTINCT orgnr) AS antall_org
                 FROM data 
                 GROUP BY regnaar")
#Checking the information on bankruptcies. 
summary(data$konkurs)

#Grouping bankruptices by organizational form
table_1d = sqldf('SELECT
                 orgform,
                 SUM(CASE WHEN konkurs = 1 THEN 1 ELSE 0 END) AS konk,
                 SUM(CASE WHEN konkurs = 0 THEN 1 ELSE 0 END) AS ikke_konk
                 FROM data
                 GROUP BY orgform')

#Loading plot library
library(ggplot2)

#Plotting bankruptices based on organizational form
plot_1e = 
  ggplot(table_1d, aes(x=orgform))+
  geom_col(aes(y = konk))
plot_1e

#The plot above is not very informative because of the vast amount of AS compared to other organizational forms
#therefore transforming to relative share of bankruptcies to number of organizations within respective form
table_1d$andel_konkurs = table_1d$konk/(table_1d$ikke_konk+table_1d$konk)

#Plotting with relative shares.
plot_1f = 
  ggplot(table_1d, aes(x=orgform))+
  geom_col(aes(y = andel_konkurs))
plot_1f

#Now, in plot f, we can actually see that the organizational form with the highest share of bankruptcies is SE.
#But from table 1d we read that it only has one bankruptcy, so this must be considered a random, not trustworthy statistic.
#Apart from that, AS is still highest, with NUF behind and all others quite far below. 

#Making a subset data set consisting only of AS and ASA org forms, because they are two of the most common and interesting ones.
#Especially ASA is of course by definition the only publicly traded form, and so it is relevant for investors to predict bankruptcy. 
#Of course, it can also be useful for other interests, such as banks wishing to evaluate risk of approving loans to businesses. 
data2 = subset(data, orgform=='AS' | orgform == 'ASA')

#Making a subset of the previous subset with only Norwegian companies; since none were excluded all were Norwegian from the start. 
data3 = subset(data2, landkode == 'NO')

#Making some new variables used to make Altmans Z-score formula for predicting bankrupty.
data3$x1 = (data3$Sum_omloepsmidler-data3$Sum_kortsiktig_gjeld)/data3$SUM_EIENDELER
data3$x2 = data3$Sum_opptjent_egenkapital/data3$SUM_EIENDELER
data3$x3 = data3$Driftsresultat/data3$SUM_EIENDELER
data3$x4 = data3$Sum_egenkapital/data3$Sum_gjeld
data3$x5 = data3$Sum_inntekter/data3$SUM_EIENDELER

#Making new variables based on the Norwegian central bank SEBRA model.
data3$v1 = (data3$Ordinaert_resultat_foer_skattekostnad-data3$Nedskrivning_av_varige_driftsmidler_og_immaterielle_eiendeler- data3$Nedskrivning_av_finansielle_eiendeler-data3$Avskrivning_paa_varige_driftsmidler_og_immaterielle_eiendeler)/data3$Sum_gjeld
data3$v2 = data3$Sum_egenkapital/data3$SUM_EIENDELER
data3$v3 = ifelse(data3$Sum_innskutt_egenkapital<data3$Sum_egenkapital,1,0)

data3$v4 = (data3$`Sum_bankinnskudd,_kontanter_og_lignende`-data3$Sum_kortsiktig_gjeld)/data3$Sum_inntekter
data3$v5 = data3$Leverandoergjeld/data3$SUM_EIENDELER
data3$v6 = data3$Skyldige_offentlige_avgifter/data3$SUM_EIENDELER

data3$alder_i_år = data3$alder_i_dager/365 #litt feil pga skuddår men la gå.
data3$a1 = ifelse(data3$alder_i_år>=1, 1,0)
data3$a2 = ifelse(data3$alder_i_år>=2, 1,0)
data3$a3 = ifelse(data3$alder_i_år>=3, 1,0)
data3$a4 = ifelse(data3$alder_i_år>=4, 1,0)
data3$a5 = ifelse(data3$alder_i_år>=5, 1,0)
data3$a6 = ifelse(data3$alder_i_år>=6, 1,0)
data3$a7 = ifelse(data3$alder_i_år>=7, 1,0)
data3$a8 = ifelse(data3$alder_i_år>=8, 1,0)


#Removing firms with less than NOK 500 000 in assets,
#and removing business codes not used in the norwegian bank model.
data_sebra = subset(data3, SUM_EIENDELER>= 500000)
data_sebra = data_sebra[!(naeringskode=="O"|naeringskode=="T"|naeringskode=="U"|naeringskode=="K"|naeringskode=="E"|naeringskode=="D"|naeringskode==0|naeringskode=="MISSING"),]


#Plotting age per business code to see what kinds of businesses last the longest.
plot_2d = ggplot(data_sebra, aes(x=naeringskode, y=alder_i_år)) + 
  geom_boxplot()
plot_2d 

#Plotting average age of bankrupt and not bankrupt businesses. 
plot_2d2 = ggplot(data_sebra, aes(x=konkurs, y=alder_i_år)) + 
  geom_boxplot()
plot_2d2   


#Since many of the variables created are relative, 
#We need to change some values who end up as infinity to 0.
data_sebra[which(is.infinite(data_sebra$x1)|is.na(data_sebra$x1)),'x1'] = 0
data_sebra[which(is.infinite(data_sebra$x2)|is.na(data_sebra$x2)),'x2'] = 0
data_sebra[which(is.infinite(data_sebra$x3)|is.na(data_sebra$x3)),'x3'] = 0
data_sebra[which(is.infinite(data_sebra$x4)|is.na(data_sebra$x4)),'x4'] = 0
data_sebra[which(is.infinite(data_sebra$x5)|is.na(data_sebra$x5)),'x5'] = 0
data_sebra[which(is.infinite(data_sebra$v1)|is.na(data_sebra$v1)),'v1'] = 0
data_sebra[which(is.infinite(data_sebra$v2)|is.na(data_sebra$v2)),'v2'] = 0
data_sebra[which(is.infinite(data_sebra$v4)|is.na(data_sebra$v4)),'v4'] = 0
data_sebra[which(is.infinite(data_sebra$v5)|is.na(data_sebra$v5)),'v5'] = 0
data_sebra[which(is.infinite(data_sebra$v6)|is.na(data_sebra$v6)),'v6'] = 0



library(ISLR)

#Checking the newest year of accounts.
max(data_sebra$regnaar)


#Making test set with the newest year, 2015, and using the rest as
#training data.
test = data_sebra[data_sebra$regnaar>=2015]
train = data_sebra[data_sebra$regnaar<2015]



#This forces R to not give output numbers in scientific notation
#Remove it if you want to.
options(scipen=999)

#Changing the factor values to numeric, to be able to perform 
#regression and other analysis. 
train$konkurs = as.numeric(train$konkurs)
test$konkurs = as.numeric(test$konkurs)

#Here we are performing a linear regression with the Altman Z-score variables as predictors
model_lm_altmann = lm(konkurs~x1+x2+x3+x4+x5,data = train)

#summary stats of the model

summary(model_lm_altmann)

#prediction on the test set with the fitted model from above
test$probs_konkurs_lm = predict(model_lm_altmann,test)
test$bin_konkurs_lm = ifelse(test$probs_konkurs_lm>=0.5,1,0)

#Confusion matrix too observe how many true positives and so on
table(test$konkurs,test$bin_konkurs_lm)

#Clearing up some memory space, our weak computer struggles(Is there a convention for doing this?)
data2=0
data=0
data3=0

#Making a logistic regression model, with the same procedure after fitting.
model_lr_altman = glm(konkurs~x1+x2+x3+x4+x5,data=train, family=binomial)
summary(model_lr_altman)


test$probs_konkurs_lr = predict(model_lr_altman,test,type="response")
test$bin_konkurs_lr = ifelse(test$probs_konkurs_lr>=0.5,1,0)
table(test$konkurs,test$bin_konkurs_lr)


#Now we are training a logistic model to predict using the SEBRA based model instead of the Altman Z. 
model_lr_sebra = glm(konkurs~v1+v2+v3+v4+v5+v6+a1+a2+a3+a4+a5+a6+a7+a8,data=train, family=binomial)
summary(model_lr_sebra)
test$probs_konkurs_lr_s = predict(model_lr_sebra,test,type="response")
test$bin_konkurs_lr_s = ifelse(test$probs_konkurs_lr_s>=0.5,1,0)
table(test$konkurs,test$bin_konkurs_lr_s)


library(pROC)
#Making ROC-plot with the altman model and SEBRA model, showing prediction accuracies.
roc(test$konkurs, test$probs_konkurs_lr)

#The SEBRA model is a bit better
roc(test$konkurs, test$probs_konkurs_lr_s)

#3h
library(class)
library(lattice)
library(ggplot2)
library(caret)
#Training a model using KNN with the altman variables. Using only one year as training data, because KNN requires a lot of computing power. 
train_2014 = train[train$regnaar == 2014]
train_x = cbind(train_2014$x1,train_2014$x2,train_2014$x3,train_2014$x4,train_2014$x5)
test_x = cbind(test$x1,test$x2,test$x3,test$x4,test$x5)
test$pred_knn = knn(train_x, test_x,train_2014$konkurs, k=2)
table(test$konkurs,test$pred_knn)
train_2014$konkurs



# Using k-fold crossvalidation and finding the average AUC across all folds.
#First with altman Z, then with SEBRA variables.
#Fitting a logistic model.
sum_auc = 0
data_sebra$konkurs=as.numeric(data_sebra$konkurs)
for (year in 2013:2015){
  train = data_sebra[data_sebra$regnaar<year,]
  train = data_sebra[data_sebra$regnaar>=year-2,]
  test = data_sebra[data_sebra$regnaar == year,]
  model_lr_kfold = glm(konkurs~x1+x2+x3+x4+x5,data = train, family = binomial)
  test$probs_konkurs_lr_kfold = predict(model_lr_kfold,test, type = "response")
  roc_ting = roc(test$konkurs, test$probs_konkurs_lr_kfold)
  sum_auc = sum_auc+roc_ting$auc
}
gjennom = sum_auc/3

sum_auc_s = 0
data_sebra$konkurs=as.numeric(data_sebra$konkurs)
for (year in 2013:2015){
  train = data_sebra[data_sebra$regnaar<year,]
  train = data_sebra[data_sebra$regnaar>=year-2,]
  test = data_sebra[data_sebra$regnaar == year,]
  model_lrs_kfold = glm(konkurs~v1+v2+v3+v4+v5+v6+a1+a2+a3+a4+a5+a6+a7+a8,data = train, family = binomial)
  test$probs_konkurs_lrs_kfold = predict(model_lrs_kfold,test, type = "response")
  roc_ting = roc(test$konkurs, test$probs_konkurs_lrs_kfold)
  sum_auc_s = sum_auc_s+roc_ting$auc
}
gjennom_s = sum_auc_s/3
