library(glmnet)
library(dplyr)
library(Hmisc)
library(lubridate)
library(ggplot2)
library(readr)
library(broom)


####DATASETS
ratings = readr::read_tsv("data.tsv")
actor = readr::read_tsv("data-2.tsv")
movies = readr::read_tsv("data-3.tsv")
metadata = readr::read_tsv("data-4.tsv")

library(skimr)
head(d)

