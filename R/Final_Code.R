#load packages
library(here)
library(tidymodels)
library(tidyverse)
library(stats)
library(corrplot)
library(wrapr)

#load data
#pg 685 in text for data definitions
#added LIFEVAL to csv to show life time donations by donor
pva <- data.frame(read_csv(here::here('Data/qso_510_data_set_pva.csv')))
colnames(pva)

#descriptive statistics
gift_hist <- hist(pva$GIFTAMNT,
                  main = 'Frequency of Gift Amounts',
                  xlab = 'Gift Amount',
                  ylab = 'Frequency',
                  col = 'dodgerblue',
                  freq = TRUE,
                  breaks = c(0,10,20,30,40,50,100,200))

gift_life_hist <- hist(pva$TOTALGIFTAMNT,
                  main = 'Frequency of Total',
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
avg_gift_t_gift <- ggplot(data = pva, aes(x = AVGGIFT, y = TOTALGIFTAMNT)) + geom_point() + geom_smooth(method=lm)
avg_gift_t_gift
n_gift_t_gift <- ggplot(data = pva, aes(x = NGIFTALL, y = TOTALGIFTAMNT)) + geom_point() + geom_smooth(method=lm)
n_gift_t_gift

n_gift_ltv <- plot(pva$NGIFTALL, pva$TOTALGIFTAMNT)

#model creation 
pva_t <- 
  pva %>% 
  dplyr::select(HOMEOWNER, HIT, MALEVET, VIETVETS, WWIIVETS, LOCALGOV, STATEGOV, FEDGOV, CARDPROM, NUMPROM, CARDPM12, NUMPRM12, NGIFTALL, CARDGIFT, MINRAMNT, MINRDATE_T, MAXRAMNT, MAXRDATE_T, LASTGIFT, AVGGIFT, CONTROLN, HPHONE_D, CLUSTER2, CHILDREN, AGE, GIFTAMNT, TOTALGIFTAMNT) %>% 
  initial_split()

#train and test
train <- training(pva_t) # training dataset
test <- testing(pva_t) # testing dataset

#recipes
mod_rec_gift_amt <- recipe(GIFTAMNT~ ., data = train) %>% 
  step_center(
    HOMEOWNER, HIT, MALEVET, VIETVETS, WWIIVETS, LOCALGOV, STATEGOV, FEDGOV, CARDPROM, NUMPROM, CARDPM12, NUMPRM12, NGIFTALL, CARDGIFT, MINRAMNT, MINRDATE_T, MAXRAMNT, MAXRDATE_T, LASTGIFT, AVGGIFT, CONTROLN, HPHONE_D, CLUSTER2, CHILDREN, AGE
  ) %>%
  step_scale(
    HOMEOWNER, HIT, MALEVET, VIETVETS, WWIIVETS, LOCALGOV, STATEGOV, FEDGOV, CARDPROM, NUMPROM, CARDPM12, NUMPRM12, NGIFTALL, CARDGIFT, MINRAMNT, MINRDATE_T, MAXRAMNT, MAXRDATE_T, LASTGIFT, AVGGIFT, CONTROLN, HPHONE_D, CLUSTER2, CHILDREN, AGE
  )

# training model
# all variables
gift_amt_lm_vars <- qc(HOMEOWNER, STATEGOV, NGIFTALL, LASTGIFT, AVGGIFT, CLUSTER2)
gift_amt_function_var <- "GIFTAMNT"
glm_formula <- as.formula(paste(sprintf("%s ~", gift_amt_function_var), paste(gift_amt_lm_vars [!gift_amt_lm_vars  %in% "y"], collapse = " + ")))

#bake data
train_prep <- prep(mod_rec_gift_amt, training = train)
train_data <- bake(train_prep, train)

# all variables function
gift_amount_lm <- lm(
  formula = glm_formula
  , data = train_data
)


# quick summary
summary(gift_amount_lm)

# test model
test_data <- bake(train_prep, test)

test_p <- predict(gift_amount_lm, test_data)
test_predict <- cbind(test_data, test_p)
test_sse <- (test_predict$GIFTAMNT - test_predict$test_p)^2
test_sst <- (test_predict$GIFTAMNT - mean(test_predict$GIFTAMNT))^2
test_r2 <- 1 - (sum(test_sse)/sum(test_sst))
test_r2


# k means model

#recipe

mod_rec_k_means <- recipe(TOTALGIFTAMNT ~ ., data = pva) %>% 
  step_center(
    HOMEOWNER, HIT, MALEVET, VIETVETS, WWIIVETS, LOCALGOV, STATEGOV, FEDGOV, CARDPROM, NUMPROM, CARDPM12, NUMPRM12, MINRAMNT, MINRDATE_T, MAXRAMNT, MAXRDATE_T, LASTGIFT, CONTROLN, HPHONE_D, CLUSTER2, CHILDREN, AGE, TOTALGIFTAMNT
  ) %>%
  step_scale(
    HOMEOWNER, HIT, MALEVET, VIETVETS, WWIIVETS, LOCALGOV, STATEGOV, FEDGOV, CARDPROM, NUMPROM, CARDPM12, NUMPRM12, MINRAMNT, MINRDATE_T, MAXRAMNT, MAXRDATE_T, LASTGIFT, CONTROLN, HPHONE_D, CLUSTER2, CHILDREN, AGE, TOTALGIFTAMNT
  )

k_prep <- prep(mod_rec_k_means, training = pva)
k_table <- bake(k_prep, pva)
k_table <- k_table %>% dplyr::select(HOMEOWNER, HIT, MALEVET, VIETVETS, WWIIVETS, LOCALGOV, STATEGOV, FEDGOV, CARDPROM, NUMPROM, CARDPM12, NUMPRM12, MINRAMNT, MINRDATE_T, MAXRAMNT, MAXRDATE_T, LASTGIFT, CONTROLN, HPHONE_D, CLUSTER2, CHILDREN, AGE, TOTALGIFTAMNT)
colnames(k_table)

# model creation

k_mod <- kmeans(k_table,centers = 4, nstart = 100)
k_mod

# build table

pva_clusters <- cbind(pva, k_mod$cluster)

