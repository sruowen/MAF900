###load the related packages
library(tidyverse)
library(RSQLite)
library(slider)
library(dbplyr)
library(furrr)
library(lubridate)

### create the function that calculate the beta the standard deviation of residuals
beta_residual <- function(data, min_obs = 1) {
    fit <- lm(ret ~ market_return, data = data)
    beta <- as.numeric(coefficients(fit)[2])
    std_residual <- as.numeric(sd(resid(fit)))
  
  results <- c(beta, std_residual)
  return(results)
}

### create the function of regression of panel A in table3
table3_panela <- function(data, min_obs = 1) {
  fit <- lm(retp ~ beta, data = data)
  gamma0 <- coefficients(fit)[1]
  gamma1 <- coefficients(fit)[2]
  rsquared <- summary(fit)$adj.r.squared
  results <- c(gamma0, gamma1, rsquared)
  return(results)
}

### create the function of regression of panel B in table3
table3_panelb <- function(data, min_obs = 1) {
  fit <- lm(retp ~ beta + beta_squared, data = data)
  gamma0 <- coefficients(fit)[1]
  gamma1 <- coefficients(fit)[2]
  gamma2 <- coefficients(fit)[3]
  rsquared <- summary(fit)$adj.r.squared
  results <- c(gamma0, gamma1, gamma2, rsquared)
  return(results)
}

### create the function of regression of panel C in table3
table3_panelc <- function(data, min_obs = 1) {
  fit <- lm(retp ~ beta + s_p_bar, data = data)
  gamma0 <- coefficients(fit)[1]
  gamma1 <- coefficients(fit)[2]
  gamma3 <- coefficients(fit)[3]
  rsquared <- summary(fit)$adj.r.squared
  results <- c(gamma0, gamma1, gamma3, rsquared)
  return(results)
}

### create the function of regression of panel D in table3
table3_paneld <- function(data, min_obs = 1) {
  fit <- lm(retp ~ beta + beta_squared + s_p_bar, data = data)
  gamma0 <- coefficients(fit)[1]
  gamma1 <- coefficients(fit)[2]
  gamma2 <- coefficients(fit)[3]
  gamma3 <- coefficients(fit)[4]
  rsquared <- summary(fit)$adj.r.squared
  results <- c(gamma0, gamma1, gamma2, gamma3, rsquared)
  return(results)
}


##### the following are a loop across the 22 periods
for (i in 1:22) {
##### for each loop, we first read the dataset of that period.
dataset_name <- paste("stock_beta_forming", i, sep = "")
stock_beta_forming <- get(dataset_name)

#### define a vector of time period that we have to update beta annually
beta_update_period <- c(table1_final$estimation_end[i],table1_final$testing_end[i]-365*3-1,table1_final$testing_end[i]-365*2,table1_final$testing_end[i]-365 )

#### the following loop are calculate the beta and save the residuals for four consecutive years in the testing periods
#### the saved beta and residuals are used for the testing regressions in the next stage
for (j in 1:4) {
stock_return <- market_return_monthly %>% 
  filter(date>=table1_final$estimation_start[i] & date<=beta_update_period[j]) %>% 
  ungroup() %>% 
  select(-year, -month) %>% 
  drop_na()

beta_residual_test <- stock_return %>% 
  nest(data = c(date, ret, market_return)) %>% 
  mutate(beta = map(
    data,
    ~ beta_residual(., )
  )) %>% 
  unnest(beta) %>% 
  select(permno, beta) 

temp <- beta_residual_test %>% 
  mutate(name = rep(c("beta","std_residual"), length.out = nrow(beta_residual_test))) %>% 
  pivot_wider(names_from = name, values_from = beta) 

assign(paste("beta_residual_test", j, sep = ""), temp)

}

#### the following the create a vector about the testing period
start_year = year(table1_final$testing_start[i])
beta_test_period <- c(start_year, start_year+1, start_year+2, start_year+3)
#### the following loop is about the run the four test regressions (panel A to D)
#### for four consecutive testing years, we first combine the return data 
#### with the beta - residual datasets
#### then, we run four regressions and save the key statistics of the regressions
for (j in 1:4) {
  #### prepare the dataset for testing
  dataset_name <- paste("beta_residual_test", j, sep = "")
  beta_residual_test <- get(dataset_name)

  test_data1 <- beta_residual_test %>% 
    left_join(stock_beta_forming %>% select(-beta)) %>% 
    drop_na() %>% 
    group_by(group) %>% 
    summarise(beta = mean(beta),
              s_p_bar = mean(std_residual)) %>% 
    mutate(beta_squared = beta*beta)
 
  test_data2 <- market_return_monthly %>% 
    filter(year==beta_test_period[j]) %>% 
    left_join(stock_beta_forming %>% select(-beta)) %>%
    drop_na() %>% 
    group_by(group, month) %>% 
    summarise(retp = mean(ret),
              mkt = mean(market_return))
  
  temp <- test_data2 %>% 
    left_join(test_data1)
  
  #### regression results for panel A table 3
  test_result_a <- temp %>% 
    nest(data = c(group, retp, mkt, beta, s_p_bar, beta_squared)) %>% 
    mutate(gamma = map(
      data,
      ~ table3_panela(., )
    )) %>% 
    unnest(gamma) %>% 
    select(month, gamma) %>% 
    mutate(name = rep(c("gamma0","gamma1","adj_r_squared"),12)) %>% 
    pivot_wider(names_from = name, values_from = gamma) %>% 
    mutate(year=beta_test_period[j])
  
  assign(paste("test_result_a", j, sep = ""), test_result_a)
  
  #### regression results for panel B table 3
  test_result_b <- temp %>% 
    nest(data = c(group, retp, mkt, beta, s_p_bar, beta_squared)) %>% 
    mutate(gamma = map(
      data,
      ~ table3_panelb(., )
    )) %>% 
    unnest(gamma) %>% 
    select(month, gamma) %>% 
    mutate(name = rep(c("gamma0","gamma1","gamma2", "adj_r_squared"),12)) %>% 
    pivot_wider(names_from = name, values_from = gamma) %>% 
    mutate(year=beta_test_period[j])
  
  assign(paste("test_result_b", j, sep = ""), test_result_b)

  #### regression results for panel C table 3
  test_result_c <- temp %>% 
    nest(data = c(group, retp, mkt, beta, s_p_bar, beta_squared)) %>% 
    mutate(gamma = map(
      data,
      ~ table3_panelc(., )
    )) %>% 
    unnest(gamma) %>% 
    select(month, gamma) %>% 
    mutate(name = rep(c("gamma0","gamma1", "gamma3" ,"adj_r_squared"),12)) %>% 
    pivot_wider(names_from = name, values_from = gamma) %>% 
    mutate(year=beta_test_period[j])
  
  assign(paste("test_result_c", j, sep = ""), test_result_c)

  #### regression results for panel D table 3
  test_result_d <- temp %>% 
    nest(data = c(group, retp, mkt, beta, s_p_bar, beta_squared)) %>% 
    mutate(gamma = map(
      data,
      ~ table3_paneld(., )
    )) %>% 
    unnest(gamma) %>% 
    select(month, gamma) %>% 
    mutate(name = rep(c("gamma0","gamma1", "gamma2", "gamma3" ,"adj_r_squared"),12)) %>% 
    pivot_wider(names_from = name, values_from = gamma) %>% 
    mutate(year=beta_test_period[j])
  
  assign(paste("test_result_d", j, sep = ""), test_result_d)
  
}

#### the following code is about the combine the time series of regression results
#### of each regression together as preparation of table 3.
panela_results <- rbind(test_result_a1,test_result_a2,test_result_a3,test_result_a4)
assign(paste("panela_results", i, sep = ""), panela_results)

panelb_results <- rbind(test_result_b1,test_result_b2,test_result_b3,test_result_b4)
assign(paste("panelb_results", i, sep = ""), panelb_results)

panelc_results <- rbind(test_result_c1,test_result_c2,test_result_c3,test_result_c4)
assign(paste("panelc_results", i, sep = ""), panelc_results)

paneld_results <- rbind(test_result_d1,test_result_d2,test_result_d3,test_result_d4)
assign(paste("paneld_results", i, sep = ""), paneld_results)


}


