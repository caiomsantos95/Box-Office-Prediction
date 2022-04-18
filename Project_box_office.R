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

####DATASETS
ratings = readr::read_tsv("data.tsv")
actor = readr::read_tsv("data-2.tsv")
movies = readr::read_tsv("data-3.tsv")
metadata = readr::read_tsv("data-4.tsv")
#kaggle_OG <- readr::read_csv("train.csv")
#kaggle <- readr::read_csv("train.csv")

library(skimr)
head(metadata)
head(actor)
head(ratings)
head(movies)
head(kaggle_train)
summary(kaggle_train$release_date)
summary(actor)

##################################
###### KAGGLE DATASET#############
##################################

##################################
########DATA PREPARATION##########
##################################


##### FIXING DATES IN THE DATASET
library(lubridate)
library(tidyverse)
kaggle$release_date <- as.Date(kaggle_OG$release_date, "%m/%d/%y")
kaggle$release_date <- format (kaggle$release_date, "%d-%m-%y")
kaggle$release_date <- parse_date_time2(kaggle$release_date, "dmy", cutoff_2000 = 20)

###HISTOGRAM
hist(kaggle$release_date, "years", freq = TRUE)
plot(ecdf(year(kaggle$release_date)))

head(kaggle$release_date)
summary(kaggle$release_date)

###MERGING DATA TABLES
enriched_dataset <- merge(x = kaggle, y = ratings, by.x = "imdb_id", by.y = "tconst")
head(enriched_dataset)

###FACTORIZATION
actor$knownForTitles <- as.factor(actor$knownForTitles)

######################################################################################################

########IMDB WEB SCRAPPED DATASET##################

df <- read.csv("movies.csv")
summary(df)
head(df)

###########################
##### DATA PREPARATION#####
###########################
df$released <- gsub(",","", df$released)
df$released <- gsub("(United States)","", df$released)
df$released <- substr(df$released,1,nchar(df$released)-2)
view(df)
df$released <- as.Date(df$released, "%B %d %Y")
summary(df) ##### WE NEED TO MAKE THIS BETTER - SOME NUMBERS ARE WRONG

### HISTOGRAM
hist <- hist(df$released, "years")



###########################
##### SPLITTING DATASET#### WE CAN IMPROVE THIS LATTER
###########################
splitter = runif(nrow(df))
head(df)
## splitting dataset
train = filter(df, splitter < 0.7)
test = filter(df, splitter >= 0.7)
view(train)
