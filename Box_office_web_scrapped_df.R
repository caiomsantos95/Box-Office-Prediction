library(glmnet)
library(dplyr)
library(Hmisc)
library(lubridate)
library(ggplot2)
library(readr)
library(broom)
library(lattice)
library(Rcpp)
library(caret)
library(tidyverse)
library(skimr)
library(rpart)

####DATASETS
movies <- readr::read_csv("imdb_movies.csv")
#head(movies)
#tail(movies )
#head(web_scrapping)
df_2018_2019 <-readr::read_csv("movies_2018_2019.csv")
df_2016_17 <- readr::read_csv("movies_2016_2017.csv")
df_2015 <- readr::read_csv("movies_2015.csv")
df_2014 <-  readr::read_csv("movies_2014.csv")
df_2010_2013 <-  readr::read_csv("movies_2010_2013.csv")
df_2009 <-  readr::read_csv("movies_2009.csv")
df_2008 <-  readr::read_csv("movies_2008.csv")
df_2005_2007 <-  readr::read_csv("movies_2005_2008.csv")
df_2000_2004 <-  readr::read_csv("movies_2000_2005.csv")
df_2000_2004 <- df_2000_2004 %>% filter(year == "2000" | year == "2001"| year == "2002" | year == "2003" | year == "2004")
#view(df_2000_2004)
#view(df_2005_2007)

### STACKING ALL DATABASES TOGETHER
web_1 <- union(df_2018_2019, df_2016_17) 
web_2 <- union(web_1, df_2015)
web_3 <- union(web_2, df_2014)
web_4 <- union(web_3, df_2010_2013)
web_5 <- union(web_4, df_2009)
web_6 <- union(web_5, df_2008)
web_7 <- union (web_6, df_2000_2004)
web_scrapping <- union(web_7, df_2005_2007)

summary(web_scrapping)

###########################
##### DATA PREPARATION#####
###########################

### ADJUSTING THE DATA
df <- web_scrapping
df$released <- gsub(",","", df$released)
df$released <- gsub("(United States)","", df$released)
df$released <- substr(df$released,1,nchar(df$released)-2)
df$released <- as.Date(df$released, "%B %d %Y")

###ADJUSTING IDs
#df$URL_new <- str_match(df$movie_url, "title/\\s*(.*?)\\s*/?ref")
#df$URL_new <- str_extract_all(df$movie_url,"(?<=title/).+(?=/?ref)")
#df$id <- substr(df$URL_new,1,nchar(df$URL_new)-2)
#view(df)

#### DROPPING COLUMNS
### FILTERING OUT EVERY MOVIE WITHOUT BUDGET INFORMATION 
df <- df %>% filter(is.na(budget) == FALSE)

### FILTERING OUT EVERY MOVIE WITHOUT BOX OFFICE INFORMATION 
df <- df %>% filter(is.na(`gross value`) == FALSE)

### FILTERING TV MOVIES LAUNCHED MOVIE THEATER
df <- df %>% filter(rating == "PG-13" | rating == "R" | rating == "Not Rated" | rating == "PG" | is.na(rating) == TRUE | rating =="G" | rating == "Unrated" | rating == "NC-17")

### REPLACING THE CURRENCY SIGNES WITH TEXT 
unique(df$`budget currency`)
unique(df$`gross currency`)

### CHANGE THE CURRENCY SIGN
df$`budget currency` <-  ifelse(df$`budget currency` == "$", "USD",
                         ifelse(df$`budget currency` == "€","EUR",
                         ifelse(df$`budget currency` == "₹","INR", 
                         ifelse(df$`budget currency` == "NOK","NOK",
                         ifelse(df$`budget currency` == "£","GBP",
                         ifelse(df$`budget currency` == "SEK","SEK",
                         ifelse(df$`budget currency` == "MX$","MXN",
                         ifelse(df$`budget currency` == "ISK","ISK",
                         ifelse(df$`budget currency` == "CA$","CAD",
                         ifelse(df$`budget currency` == "RUR","RUB", "Other"
                         ))))))))))
df$`gross currency` <- ifelse(df$`gross currency` == "$", "USD", "BRL")

### YEAR WIDELY RELEASED
df$year_widely_released <- year(df$released)
df$week_widely_released <- week(df$released)

##ENRICHING DATA TABLE
df <- merge(x = df, y = movies, by.x = "movie_id", by.y = "tconst", all.x=TRUE)

### ADJUSTING GENRES
library(tm)
?tm
corpus <- Corpus(VectorSource(df$genres)) 
strwrap(corpus[[2]])
f <- content_transformer(function(doc, oldtext, newtext) gsub(oldtext, newtext, doc)) 
corpus <- tm_map(corpus, f, ",", " ")
corpus <- tm_map(corpus, removePunctuation)
frequencies = DocumentTermMatrix(corpus)
frequencies
document_terms = as.data.frame(as.matrix(frequencies))

df <- merge(x = df, y = document_terms, by.x = 0, by.y = 0)

### ADJUSTING DIRECTORS, STAR, WRITERS AND COMPANIES
df <- data.frame(df)

### DIRECTORS
df <- df %>% group_by(df$director) %>% mutate(director_count=n())
#df <- df %>% filter(director_count >= "5")
df$director_adjusted <-ifelse(df$director_count < 9, "Other Director", df$director)
hist(df$director_count)
table(df$director)

### STARS
df <- df %>% group_by(df$star) %>% mutate(star_count=n())
#df <- df %>% filter(director_count >= "8")
df$star_adjusted <-ifelse(df$star_count < 14, "Other Star", df$star)
hist(df$star_count)

### WRITERS
df <- df %>% group_by(df$writer) %>% mutate(writer_count=n())
#df <- df %>% filter(director_count >= "8")
df$writer_adjusted <-ifelse(df$writer_count < 7, "Other Writer", df$writer)
hist(df$writer_count)

### COMPANIES
df <- df %>% group_by(df$company) %>% mutate(company_count=n())
#df <- df %>% filter(director_count >= "8")
df$company_adjusted <-ifelse(df$company_count < 30, "Other Company", df$company)
hist(df$company_count)

### ADJUSTIN RUNTIME
df$runtimeMinutes <- as.numeric(df$runtimeMinutes)

### CREATE CURRENCY EXCHANGE RATE TABLE TO UPDATE VALUES FROM INTERNATIONAL MOVIES - REPLICATE FOR ALL CURRENCIES
#install.packages("priceR")
#library(priceR)
#USD_GBP <- historical_exchange_rates("USD", to = "GBP", start_date = "2010-01-01", end_date = "2010-12-31")
#USD_GBP$year <- year(USD_GBP$date)
#table(USD_GBP$year, mean(USD_GBP$one_USD_equivalent_to_x_GBP))

#summary <- summarise(USD_GBP, group_by(USD_GBP$year), avg_USD_GBP = mean(USD_GBP$one_USD_equivalent_to_x_GBP))

### ORDERING RATING
#unique(df$rating)
df$rating <- ifelse(df$rating == "Not Rated" | is.na(df$rating) == TRUE , "Unrated", df$rating)
rating.levels = c("G", "PG", "PG-13", "NC-17", "R", "Unrated")
df$rating <- factor(df$rating, ordered=TRUE, levels=rating.levels)

### TRAINING THE MODEL WITH USD BUDGET ONLY UNTIL CONVERSION IS OK
df_usd <- df %>% filter(budget.currency == "USD")
view(df_usd)

### DROPPING COLUMNS
df_usd <- subset(df_usd, select = - c(movie_url, budget, 
                                      gross, runtime, originalTitle, 
                                      titleType, endYear, Row.names, 
                                      genres, director, writer, star, 
                                      budget.currency, company, startYear))

df_usd <- subset(df_usd, select = - c(...1, `df$director`, `df$star`, `df$writer`, `df$company`))

### FACTORIZATION
df_usd$director <- factor(df_usd$director_adjusted)
df_usd$writer <- factor(df_usd$writer_adjusted)
df_usd$star <- factor(df_usd$star_adjusted)
df_usd$country <- factor(df_usd$country)
df_usd$company <- factor(df_usd$company_adjusted)

### SOME OTHER ADJUSTEMENTS
#df_usd <- subset(df_usd, select = - c(...1))
#df_usd <- subset(df_usd, select = - c(isAdult))

skim(df_usd)

### REMOVE LAST MISSING VALUES
df_usd <- df_usd %>% filter(is.na(released) == FALSE)
df_usd <- df_usd %>% filter(is.na(country) == FALSE)

######################################
##### DATA EXPLORATION ###############
######################################
hist_week <- hist(df_usd$week_widely_released)
hist_year <- hist(df_usd$year_widely_released)


###########################
##### SPLITTING DATASET#### 
###########################
#head(df_usd)
## splitting dataset
train <- filter(df_usd, released <= "2017-12-31" & released >= "2000-01-01")
test <- filter(df_usd, released >= "2018-01-01" & released <= "2019-12-31")
test_pandemic <- filter(df_usd, released >= "2020-01-01")
#summary(train)
skim(train)
#view(test)
#view(train)


hist_year_train <- hist(train$year_widely_released)
hist_week_train <- hist(train$week_widely_released)


########################################################################################################################################################
##### MODEL ############################################################################################################################################
########################################################################################################################################################

### LINEAR REGRESSION - NOT WORKING
lm.1 <- lm(
  train$gross.value ~ . - movie_id - gross.currency - primaryTitle - director_adjusted - star_adjusted - writer_adjusted - company_adjusted -isAdult,  data = train)
summary(lm.1)

### DEBUGGING
practice <- train
practice_1 <- sapply(lapply(practice, unique), length)
practice_1

lm.2 <- lm(
  train$gross.value ~ train$budget.value, train$week_widely_released, train$runtimeMinutes, train$comedy,
  data = train)
summary(lm.2)


#### R2
pred_train = predict(lm.2, newdata=train)
pred_test <- predict(lm.2, newdata=test)

SSR_test2 = sum((test$gross.value - pred_test)^2)
baseline_train = mean(train$gross.value)
SST_test2 = sum((test$gross.value - baseline_train)^2)
OSR2_2 = 1 - SSR_test2 / SST_test2
OSR2_2

########################################################################################################################################################################################################################

### CART MODEL
library(rpart.plot)
cv.trees <- train(y = train$gross.value,
                  x = subset(train, select=-c(gross.value, movie_id, primaryTitle, gross.currency, year_widely_released, year, director_adjusted, star_adjusted, writer_adjusted, company_adjusted)),
                  method = "rpart", 
                  trControl = trainControl(method = "cv", number = 10), 
                  tuneGrid = data.frame(.cp = seq(.0001,.1,.0001)))
cv.trees$bestTune
cv.results = cv.trees$results
cv.results

cart.model <- rpart(train$gross.value ~. -gross.value - movie_id - primaryTitle - gross.currency - year_widely_released - year - director_adjusted - star_adjusted - writer_adjusted - company_adjusted, data = train, control = rpart.control(cp = 0.0064))
prp(cart.model, digits = 2, type = 2)

### R2
pred_train_cart = predict(cart.model, newdata=train)
pred_test_cart <- predict(cart.model, newdata=test)
pred_test_cart_pandemic <- predict(cart.model, newdata=test_pandemic)

###TEST
baseline_train_cart = mean(train$gross.value)

SSR_test_cart = sum((test$gross.value - pred_test_cart)^2)
SST_test_cart = sum((test$gross.value - baseline_train_cart)^2)
OSR2_cart = 1 - SSR_test_cart / SST_test_cart
OSR2_cart

###TEST PANDEMIC
SSR_test_cart_pandemic = sum((test_pandemic$gross.value - pred_test_cart_pandemic)^2)
SST_test_cart_pandemic = sum((test_pandemic$gross.value - baseline_train_cart)^2)
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

#####################
### RANDOM FOREST####
#####################
skim(train)
rf_data <- subset(train, select=-c(movie_id, primaryTitle, gross.currency, year_widely_released, 
                                   year, director_adjusted, star_adjusted, writer_adjusted, company_adjusted, country))
#rf_data <- rf_data %>% filter(is.na(director) == FALSE)
#rf_data <- rf_data %>% filter(is.na(start) == FALSE)
#rf_data <- rf_data %>% filter(is.na(rating) == FALSE)
#rf_data <- rf_data %>% filter(is.na(company) == FALSE)
#rf_data <- rf_data %>% filter(is.na(writer) == FALSE)

skim(rf_data)

train.rf.oob <- train(y = rf_data$gross.value,
                  x = subset(rf_data, select=-c(gross.value)),
                      method="rf",
                      ntree=500, nodesize=25,
                      tuneGrid=data.frame(mtry=seq(1,20,1)),
                      trControl=trainControl(method="oob") )
plot(train.rf.oob$results$Rsquared, train.rf.oob$results$mtry)
train.rf.oob$bestTune

library(randomForest)
rf.md1 <- randomForest(rf_data$gross.value~., data=subset(rf_data, select=-c(gross.value)), nodesize=25, ntree=500, mtry = 10)


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
baseline_train_rf = mean(train$gross.value)

SSR_test_rf = sum((test$gross.value - pred_test_rf)^2)
SST_test_rf = sum((test$gross.value - baseline_train_rf)^2)
OSR2_rf = 1 - SSR_test_rf / SST_test_rf
OSR2_rf

###TEST PANDEMIC
SSR_test_rf_pandemic = sum((test_pandemic$gross.value - pred_test_rf_pandemic)^2)
SST_test_rf_pandemic = sum((test_pandemic$gross.value - baseline_train_rf)^2)
OSR2_rf_pandemic = 1 - SSR_test_rf_pandemic / SST_test_rf_pandemic
OSR2_rf_pandemic
