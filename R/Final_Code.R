#load packages
library(here)
library(tidymodels)
library(tidyverse)
library(stats)
library(corrplot)

#load data
pva <- data.frame(read_csv(here::here('Data/qso_510_data_set_pva.csv')))

#descriptive statistics
gift_hist <- hist(pva$GIFTAMNT,
                  main = 'Frequency of Gift Amounts',
                  xlab = 'Gift Amount',
                  ylab = 'Frequency',
                  col = 'dodgerblue',
                  freq = TRUE,
                  breaks = c(0,10,20,30,40,50,100,200))

cor_pva <- cor(pva)
cor_pva

