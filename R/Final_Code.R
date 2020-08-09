#load packages
library(here)
library(tidymodels)
library(tidyverse)
library(stats)
library(corrplot)

#load data
#pg 685 in text for data definitions
#added LIFEVAL to csv to show life time donations by donor
pva <- data.frame(read_csv(here::here('Data/qso_510_data_set_pva.csv')))

#descriptive statistics
gift_hist <- hist(pva$GIFTAMNT,
                  main = 'Frequency of Gift Amounts',
                  xlab = 'Gift Amount',
                  ylab = 'Frequency',
                  col = 'dodgerblue',
                  freq = TRUE,
                  breaks = c(0,10,20,30,40,50,100,200))

gift_life_hist <- hist(pva$LIFEVAL,
                  main = 'Frequency of Life Time Value',
                  xlab = 'Total of all Donations',
                  ylab = 'Frequency',
                  col = 'limegreen',
                  freq = TRUE,
                  breaks = c(0,50,100,150,200,250,300,350,400,450,500,550,600,650,700,750,800,850,900,950,1000,1100,1200,1300,1400,1500,1600,1700,1800,1900,2000,2200))

n_gift_hist <- hist(pva$NGIFTALL,
                    main = 'Frequency of Total Number of Gifts',
                    xlab = 'Total Number of Gifts',
                    ylab = 'Frequency',
                    col = 'orangered',
                    freq = TRUE)

cor_pva <- cor(pva)
gift_corrplot <- corrplot(cor_pva, method = 'circle')

avg_gift_n_gift <- ggplot(data = pva, aes(x = NGIFTALL, y = AVGGIFT)) + geom_point() + geom_smooth(method=lm)
avg_gift_n_gift
avg_gift_ltv <- ggplot(data = pva, aes(x = AVGGIFT, y = LIFEVAL)) + geom_point() + geom_smooth(method=lm)
avg_gift_ltv
n_gift_ltv <- ggplot(data = pva, aes(x = NGIFTALL, y = LIFEVAL)) + geom_point() + geom_smooth(method=lm)
n_gift_ltv

n_gift_ltv <- plot(pva$NGIFTALL, pva$LIFEVAL)

