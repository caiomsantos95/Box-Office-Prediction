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
head(movies)
movies$movie_id <- movies$tconst
head(movies)
tail(movies )
head(web_scrapping)
skim(movies)
summary(web_scrapping)
summary(movies)
df_2016_17 <- readr::read_csv("movies_2016_2017.csv")
df_2015 <- readr::read_csv("movies_2015.csv")
web_scrapping <- union(df_2016_17, df_2015)
view(web_scrapping)

### DEBUGGING
#smallfoot <- movies %>% filter(primaryTitle == "Smallfoot")
#smallfoot

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
corpus <- Corpus(VectorSource(df$genres)) 
strwrap(corpus[[2]])
f <- content_transformer(function(doc, oldtext, newtext) gsub(oldtext, newtext, doc)) 
corpus <- tm_map(corpus, f, ",", " ")
corpus <- tm_map(corpus, removePunctuation)
frequencies = DocumentTermMatrix(corpus)
frequencies
document_terms = as.data.frame(as.matrix(frequencies))

df <- merge(x = df, y = document_terms, by.x = 0, by.y = 0)

### ADJUSTIN RUNTIME
df$runtimeMinutes <- as.numeric(df$runtimeMinutes)

### CREATE CURRENCY EXCHANGE RATE TABLE TO UPDATE VALUES FROM INTERNATIONAL MOVIES - REPLICATE FOR ALL CURRENCIES
install.packages("priceR")
library(priceR)
USD_GBP <- historical_exchange_rates("USD", to = "GBP", start_date = "2010-01-01", end_date = "2010-12-31")
USD_GBP$year <- year(USD_GBP$date)
table(USD_GBP$year, mean(USD_GBP$one_USD_equivalent_to_x_GBP))

summary <- summarise(USD_GBP, group_by(USD_GBP$year), avg_USD_GBP = mean(USD_GBP$one_USD_equivalent_to_x_GBP))

### TRAINING THE MODEL WITH USD BUDGET ONLY UNTIL CONVERSION IS OK
df_usd <- df %>% filter(df$`budget currency` == "USD")
view(df_usd)

### ORDERING RATING
unique(df$rating)
df$rating <- ifelse(df$rating == "Not Rated" | is.na(df$rating) == TRUE , "Unrated", df$rating)
rating.levels = c("G", "PG", "PG-13", "NC-17", "R", "Unrated")
df$rating <- factor(df$rating, ordered=TRUE, levels=rating.levels)

### DROPPING COLUMNS
df_usd <- subset(df_usd, select = - c(movie_url, budget, gross, runtime, originalTitle, titleType, endYear, movie_id.y, Row.names, genres))

### FACTORIZATION
df_usd$director <- factor(df_usd$director)
df_usd$writer <- factor(df_usd$writer)
df_usd$star <- factor(df_usd$star)
df_usd$country <- factor(df_usd$country)
df_usd$company <- factor(df_usd$company)

### SOME OTHER ADJUSTEMENTS
df_usd <- subset(df_usd, select = - c(...1))
df_usd <- subset(df_usd, select = - c(isAdult))


skim(df_usd)

######################################
##### DATA EXPLORATION ###############
######################################
hist <- hist(df_usd$released, "weeks")

###########################
##### SPLITTING DATASET#### CHANGE LATER TO MOVIES UP 2017 (TEST IS 2018 and 2019 - 2020 and 2021 are going to be tests)
###########################
head(df_usd)
## splitting dataset
train <- filter(df_usd, released <= "2016-12-31")
test <- filter(df_usd, released >= "2017-01-01")
view(train)
view(test)

######################################
##### MODEL ##########################
######################################

### LINEAR REGRESSION
lm.1 <- lm(
  train$`gross value` ~ . -movie_id - `budget currency` - `gross currency` - primaryTitle - startYear,
  data = train)
summary(lm.1)


#### OUT OF SAMPLE R2 SQUARED
pred_train = predict(lm.1, newdata=train)
pred_test <- predict(lm.1, newdata=test)

SSR_test2 = sum((test$`gross value` - pred_test)^2)
baseline_train = mean(train$`gross value`)
SST_test2 = sum((test$`gross value` - baseline_train)^2)
OSR2_2 = 1 - SSR_test2 / SST_test2
OSR2_2

### CART MODEL
library(rpart.plot)
cv.trees <- train(y = train$`gross value`,
                  x = subset(train, select=c(`gross value`, movie_id, primaryTitle, `budget currency`, `gross currency`, startYear)),
                  method = "rpart", 
                  trControl = trainControl(method = "cv", number = 10), 
                  tuneGrid = data.frame(.cp = seq(.00001,.0003,.000001)))
cv.trees$bestTune
cv.results = cv.trees$results
cv.results

cart.model <- rpart(train$`gross value` ~. - movie_id - primaryTitle, data = train, control = rpart.control(cp = 0.000054))
prp(cart.model, digits = 2, type = 2)

r2_osr2_glmnet(cart.model, train, test, train$`gross value`, test$`gross value`, )

#### FUNCTION FOR R2

#### FUNCTION FOR R2 - NOT WORKING
r2_osr2 <- function(tree, trainData, testData, yvar) {
  # Like we did for logistic regression, we use the predict() function on the model, 
  # indicating the data we should predict on with the newdata parameter.
  PredictTrain = predict(tree, newdata = trainData)
  PredictTest = predict(tree, newdata = testData)
  
  # Let us compute R2 for both the training and test set
  # First we create a baseline model, which is the average of the training set reimbursements.
  ymean = mean(trainData[,yvar])
  
  # Then we can compute SSE and SST - the sum of square residuals between the
  # predictions and the truth vs the baseline and the truth
  SSETrain = sum((trainData[,yvar] - PredictTrain)^2)
  SSTTrain = sum((trainData[,yvar] - ymean)^2)
  # R2 is 1 minus the ratio of these terms
  R2 = 1 - SSETrain/SSTTrain
  print(paste0("R2=",R2))
  
  # We compute the out of sample R2 similarly, using predictTest and claimsTest
  # instead of the corresponding training sets
  # Remember that we keep the baseline *the same* as in the training set.
  # Using the testset true values would be cheating!
  SSETest = sum((testData[,yvar] - PredictTest)^2)
  SSTTest = sum((testData[,yvar] - ymean)^2)
  OSR2 = 1 - SSETest/SSTTest
  print(paste0("OSR2=",OSR2))
}

r2_osr2(cart.model, train, test, `gross value`)
