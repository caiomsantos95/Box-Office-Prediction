library(glmnet)
library(dplyr)
library(Hmisc)

library(ggplot2)
library(readr)
library(broom)
library(lattice)
library(Rcpp)
library(caret)


library(rpart)

library(tidyverse)
library(stringr)
library(skimr)
library(lubridate)

####DATASETS
web_scrapping <- readr::read_csv('webscrapping.csv')
title_basics <- read_tsv('title.basics.tsv')

###########################
##### DATA PREPARATION#####
###########################

### ADJUSTING SCRAPPING DATA
df <- web_scrapping
df$released <- str_extract(df$released,"(.*\\d)")
df$released <- gsub(",","", df$released)
df$released <- as.Date(df$released, "%B %d %Y")
df <- df %>% filter(released>='2000-01-01') 

#### DROPPING COLUMNS
### FILTERING OUT EVERY MOVIE WITHOUT BUDGET INFORMATION 
df <- df %>% filter(is.na(budget) == FALSE)

### FILTERING OUT EVERY MOVIE WITHOUT BOX OFFICE INFORMATION 
df <- df %>% filter(is.na(`gross value`) == FALSE)

### FILTERING TV MOVIES LAUNCHED MOVIE THEATER
df <- df %>% filter(is.element(rating,c('G','PG','PG-13','NC-17','R','Not Rated','Unrated'))|is.na(rating))

### REPLACING THE CURRENCY SIGNS WITH TEXT 
df$`budget currency`<- trimws(df$`budget currency`) 
unique(df$`budget currency`)
currency_symbol <- c('$','€','₹','£','CA$','A$')
currency_name <- c('USD','EUR','INR','GBP','CAD','AUD')
currency_cleanup <- data.frame(currency_symbol,currency_name)

df <- df %>% rename(budget_currency=`budget currency`)
df <- merge(x=df,y=currency_cleanup,by.x="budget_currency",by.y="currency_symbol")

### CREATE CURRENCY EXCHANGE RATE TABLE TO UPDATE VALUES FROM INTERNATIONAL MOVIES - REPLICATE FOR ALL CURRENCIES
##install.packages("priceR")
library(priceR)

exchange_currency <- NA
exchange_year <- NA
exchange_factor <- NA
currency_conv <- data.frame(exchange_currency,exchange_year,exchange_factor)
test <- rbind(currency_conv,c('EUR',2018,1))

##THIS TAKES TIME TO RUN BECAUSE IT USES INTERNET DATA
for(CUR in currency_cleanup$currency_name){
  for(year in unique(df$year)){
    year_exchange <- mean(historical_exchange_rates(CUR, to = "USD", start_date = make_date(year,1,1), end_date = make_date(year,12,31))[,2])
    currency_conv <- rbind(currency_conv,c(CUR,year,year_exchange))
  }
}

df <- merge(x=df,y=currency_conv,by.y=c('exchange_currency','exchange_year'),by.x=c('currency_name','year'))
df$exchange_factor <- as.numeric(df$exchange_factor)
df$budget_usd <- df$`budget value`*df$exchange_factor

df2<-df

### YEAR WIDELY RELEASED
df$year_widely_released <- year(df$released)
df$week_widely_released <- week(df$released)
df$weekday_widely_released <- weekdays(df$released)
df$quarter_widely_released <- quarter(df$released)

df2<-df

##ENRICHING DATA TABLE
df <- merge(x = df, y = title_basics, by.x = "movie_id", by.y = "tconst", all.x=TRUE)

### ADJUSTING GENRES
library(tm)
corpus <- Corpus(VectorSource(df$genres)) 
f <- content_transformer(function(doc, oldtext, newtext) gsub(oldtext, newtext, doc)) 
corpus <- tm_map(corpus, f, ",", " ")
corpus <- tm_map(corpus, removePunctuation)
frequencies = DocumentTermMatrix(corpus)
document_terms = as.data.frame(as.matrix(frequencies))
df <- merge(x = df, y = document_terms, by.x = 0, by.y = 0)

### ADJUSTING DIRECTORS, STAR, WRITERS AND COMPANIES
df <- data.frame(df)

df2 <- df

### DIRECTORS
director_count <- df %>% group_by(df$director) %>% summarize(director_count=n())
df <- merge(df,director_count,by.x="director",by.y="df$director")
#df <- df %>% filter(director_count >= "5")
df$director_adjusted <-ifelse(df$director_count < 9, "Other Director", df$director)
hist(df$director_count)
table(df$director)

### STARS
star_count <- df %>% group_by(df$star) %>% summarize(star_count=n())
df <- merge(df,star_count,by.x="star",by.y="df$star")
#df <- df %>% filter(director_count >= "8")
df$star_adjusted <-ifelse(df$star_count < 14, "Other Star", df$star)
hist(df$star_count)

### WRITERS
writer_count <- df %>% group_by(df$writer) %>% summarize(writer_count=n())
df <- merge(df,writer_count,by.x="writer",by.y="df$writer")
#df <- df %>% filter(director_count >= "8")
df$writer_adjusted <-ifelse(df$writer_count < 7, "Other Writer", df$writer)
hist(df$writer_count)

### COMPANIES
company_count <- df %>% group_by(df$company) %>% summarize(company_count=n())
df <- merge(df,company_count,by.x="company",by.y="df$company")
#df <- df %>% filter(director_count >= "8")
df$company_adjusted <-ifelse(df$company_count < 30, "Other Company", df$company)
hist(df$company_count)

### COUNTRIES
country_count <- df %>% group_by(df$country) %>% summarize(country_count=n())
df <- merge(df,country_count,by.x="country",by.y="df$country")
#df <- df %>% filter(director_count >= "8")
df$country <-ifelse(df$country_count < 50, "Other Country", df$country)
hist(df$country_count)


### ADJUSTIN RUNTIME
df$runtimeMinutes <- as.numeric(df$runtimeMinutes)

### ORDERING RATING
#unique(df$rating)
df$rating <- ifelse(df$rating == "Not Rated" | is.na(df$rating) == TRUE , "Unrated", df$rating)
rating.levels = c("G", "PG", "PG-13", "NC-17", "R", "Unrated")
df$rating <- factor(df$rating, ordered=TRUE, levels=rating.levels)

### DROPPING COLUMNS
df2 <- df
df <- subset(df, select = - c(movie_url,
                              movie_id,
                              budget, 
                              gross,
                              gross.currency,
                              runtime,
                              originalTitle,
                              titleType,
                              endYear,
                              Row.names, 
                              genres,
                              director,
                              writer,
                              star, 
                              budget_currency,
                              company,
                              startYear,
                              currency_name,
                              budget.value,
                              exchange_factor,
                              isAdult
                              ))

### FACTORIZATION
df$director <- factor(df$director_adjusted)
df$writer <- factor(df$writer_adjusted)
df$star <- factor(df$star_adjusted)
df$country <- factor(df$country)
df$company <- factor(df$company_adjusted)
df$weekday_widely_released <- factor(df$weekday_widely_released)

titles <- df$primaryTitle
df <- subset(df,select=-c(primaryTitle,company_adjusted,writer_adjusted,star_adjusted,director_adjusted))

### SOME OTHER ADJUSTEMENTS
skim(df)

######################################
##### DATA EXPLORATION ###############
######################################
hist_week <- hist(df$week_widely_released)
hist_year <- hist(df$year_widely_released)
df %>% group_by(weekday_widely_released) %>% summarize(n())
hist_quarter <-hist(df$quarter_widely_released)

hist(df$gross.value)

hist(df$budget_usd)

df$gross_log <- log10(df$gross.value+1)
df$budget_usd_log <- log10(df$budget_usd+1)

df <- subset(df,select=-c(gross.value,budget_usd))

###########################
##### SPLITTING DATASET#### 
###########################
#head(df_usd)
## splitting dataset
train <- filter(df, released <= "2017-12-31" & released >= "2000-01-01")
test <- filter(df, released >= "2018-01-01" & released <= "2020-03-15")
test_pandemic <- filter(df, released >= "2020-03-15")


#summary(train)
skim(train)
#view(test)
#view(train)

hist_year_train <- hist(train$year_widely_released)
hist_week_train <- hist(train$week_widely_released)

########################################################################################################################################################
##### MODEL ############################################################################################################################################
########################################################################################################################################################

#LINEAR REGRESSION

lin <- lm(gross_log~.-released-year,data=train)

pred_train = predict(lin, newdata=train)
pred_test <- predict(lin, newdata=test)

SSR_test = sum((test$gross_log - pred_test)^2)
baseline_train = mean(train$gross_log)
SST_test = sum((test$gross_log - baseline_train)^2)
OSR2_ = 1 - SSR_test / SST_test
OSR2_

########################################################################################################################################################################################################################

##################################
###### PENALIZED REGRESSION ######
##################################

library(glmnet)
# Prepare the data matrices for glmnet functions
x.train=model.matrix(gross_log~.-year-released,data=train)
y.train=train$gross_log
x.test=model.matrix(gross_log~.-year-released,data=test)
y.test=test$gross_log

x.train.scl = scale(x.train)
x.test.scl = x.test

for(i in 1:ncol(x.test)){
  x.test.scl[,i] <- (x.test[,i] - attr(x.train.scl, 'scaled:center')[i])/ attr(x.train.scl, 'scaled:scale')[i]
}

all.lambdas <- c(exp(seq(15, -10, -.1)))

### RIDGE REGRESSION
ridge.cv=cv.glmnet(x.train.scl, y.train, alpha=0, lambda=all.lambdas)
plot(ridge.cv, main="Ridge regression MSE\n")
best.lambda.ridge <- ridge.cv$lambda.min 
ridge.model=glmnet(x.train.scl,y.train,alpha=0,lambda=best.lambda.ridge)

r2_osr2_glmnet <- function(tree, trainData, testData, trainY, testY, lambda) {
  # Like we did for logistic regression, we use the predict() function on the model, 
  # indicating the data we should predict on with the newdata parameter.
  PredictTrain = predict(tree, newx = trainData, s=lambda)
  PredictTest = predict(tree, newx = testData, s=lambda)
  
  # Let us compute R2 for both the training and test set
  # First we create a baseline model, which is the average of the training set reimbursements.
  ymean = mean(trainY)
  
  # Then we can compute SSE and SST - the sum of square residuals between the
  # predictions and the truth vs the baseline and the truth
  SSETrain = sum((trainY - PredictTrain)^2)
  SSTTrain = sum((trainY - ymean)^2)
  # R2 is 1 minus the ratio of these terms
  R2 = 1 - SSETrain/SSTTrain
  print(paste0("R2=",R2))
  
  # We compute the out of sample R2 similarly, using predictTest and claimsTest
  # instead of the corresponding training sets
  # Remember that we keep the baseline the same as in the training set.
  # Using the testset true values would be cheating!
  SSETest = sum((testY - PredictTest)^2)
  SSTTest = sum((testY - ymean)^2)
  OSR2 = 1 - SSETest/SSTTest
  print(paste0("OSR2=",OSR2))
}

r2_osr2_glmnet(ridge.model,x.train.scl,x.test.scl,y.train,y.test,best.lambda.ridge)

### LASSO REGRESSION
lasso.cv=cv.glmnet(x.train.scl, y.train, alpha=1, lambda=all.lambdas)
plot(lasso.cv, main="Ridge regression MSE\n")
best.lambda.lasso <- lasso.cv$lambda.min 
lasso.model=glmnet(x.train.scl,y.train,alpha=0,lambda=best.lambda.lasso)
r2_osr2_glmnet(lasso.model,x.train.scl,x.test.scl,y.train,y.test,best.lambda.lasso)

#####################
### CART MODEL ######
#####################

library(rpart.plot)
cv.trees <- train(y = train$gross_log,
                  x = subset(train, select=-c(gross_log,released,year)),
                  method = "rpart", 
                  trControl = trainControl(method = "cv", number = 10), 
                  tuneGrid = data.frame(.cp = seq(.0001,.1,.0001)))
cv.trees$bestTune
cv.results = cv.trees$results
cv.results

cart.model <- rpart(gross_log ~. -gross_log-year-released, data = train, control = rpart.control(cp = 0.0019))
prp(cart.model, digits = 2, type = 2)


### R2
pred_train_cart = predict(cart.model, newdata=train)
pred_test_cart <- predict(cart.model, newdata=test)
pred_test_cart_pandemic <- predict(cart.model, newdata=test_pandemic)

###TEST
baseline_train_cart = mean(train$gross_log)

SSR_test_cart = sum((test$gross_log - pred_test_cart)^2)
SST_test_cart = sum((test$gross_log - baseline_train_cart)^2)
OSR2_cart = 1 - SSR_test_cart / SST_test_cart
OSR2_cart

###TEST PANDEMIC
SSR_test_cart_pandemic = sum((test_pandemic$gross_log - pred_test_cart_pandemic)^2)
SST_test_cart_pandemic = sum((test_pandemic$gross_log - baseline_train_cart)^2)
OSR2_cart_pandemic = 1 - SSR_test_cart_pandemic / SST_test_cart_pandemic
OSR2_cart_pandemic

CART_importance_scores = cart.model$variable.importance
n_variables = 20 # how many variables to display?
barplot( tail( sort(CART_importance_scores), n_variables ),
         beside = TRUE,
         horiz = TRUE,
         las=1,
         main = paste("CART - top", n_variables, "importance scores"),
         cex.names =.7)


### SIMPLIFIED CART MODEL
cart.model_s <- rpart(train$gross_log ~. -gross_log -year -released, data = train, control = rpart.control(cp = 0.005))
prp(cart.model_s, digits = 2, type = 2)

### R2
pred_train_cart_s = predict(cart.model_s, newdata=train)
pred_test_cart_s <- predict(cart.model_s, newdata=test)
pred_test_cart_pandemic_s <- predict(cart.model_s, newdata=test_pandemic)

###TEST
baseline_train_cart_s = mean(train$gross_log)

SSR_test_cart_s = sum((test$gross_log- pred_test_cart_s)^2)
SST_test_cart_s = sum((test$gross_log - baseline_train_cart_s)^2)
OSR2_cart_s = 1 - SSR_test_cart_s / SST_test_cart_s
OSR2_cart_s

###TEST PANDEMIC
SSR_test_cart_pandemic_s = sum((test_pandemic$gross.value - pred_test_cart_pandemic_s)^2)
SST_test_cart_pandemic_s = sum((test_pandemic$gross.value - baseline_train_cart_s)^2)
OSR2_cart_pandemic_s = 1 - SSR_test_cart_pandemic_s / SST_test_cart_pandemic_s
OSR2_cart_pandemic_s

#####################
### RANDOM FOREST####
#####################

skim(train)
rf_data <- subset(train, select=-c(released,year))
skim(rf_data)

train.rf.oob <- train(y = rf_data$gross_log,
                  x = subset(rf_data, select=-c(gross_log)),
                      method="rf",
                      ntree=500, nodesize=25,
                      tuneGrid=data.frame(mtry=seq(1,20,1)),
                      trControl=trainControl(method="oob"))
plot(train.rf.oob$results$mtry,train.rf.oob$results$Rsquared)
train.rf.oob$bestTune

library(randomForest)
rf.md1 <- randomForest(rf_data$gross_log~., data=subset(rf_data, select=-c(gross_log)), nodesize=25, ntree=500, mtry = 8)


### Variable importance with the random forest
# RF variable importance
RF_importance_scores <- rf.md1$importance[,1]
n_variables = 20 # how many variables to display?
barplot( tail( sort(RF_importance_scores), n_variables ),
         beside = TRUE,
         horiz = TRUE,
         las=1,
         main = paste("Random Forest - top", n_variables, "importance scores"),
         cex.names =.7)

### R2
pred_train_rf = predict(rf.md1, newdata=train)
pred_test_rf <- predict(rf.md1, newdata=test)
pred_test_rf_pandemic <- predict(rf.md1, newdata=test_pandemic)

###TEST
baseline_train_rf = mean(train$gross_log)

SSR_test_rf = sum((test$gross_log - pred_test_rf)^2)
SST_test_rf = sum((test$gross_log - baseline_train_rf)^2)
OSR2_rf = 1 - SSR_test_rf / SST_test_rf
OSR2_rf

###TEST PANDEMIC
SSR_test_rf_pandemic = sum((test_pandemic$gross_log - pred_test_rf_pandemic)^2)
SST_test_rf_pandemic = sum((test_pandemic$gross_log - baseline_train_rf)^2)
OSR2_rf_pandemic = 1 - SSR_test_rf_pandemic / SST_test_rf_pandemic
OSR2_rf_pandemic
