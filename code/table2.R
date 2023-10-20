
library(tidyverse)
library(RSQLite)
library(slider)
library(dbplyr)
library(furrr)

# the following function is to report the relative statistics when regression about the portfolio
estimate_capm_full <- function(data, min_obs = 1) {
  fit <- lm(retp ~ market_return, data = data)
  beta <- as.numeric(coefficients(fit)[2])
  s_beta <- as.numeric(summary(fit)$coefficients[2,2])
  Rsquare <- as.numeric(summary(fit)$r.squared)
  s_ep <- sd(resid(fit))
  results <- c(beta, s_beta, Rsquare, s_ep)
  return(results)
}

# the following function is the find the standard deviation of the residual of a regression
estimate_residual <- function(data, min_obs = 1) {
  if (nrow(data) < min_obs) {
    std_residual <- as.numeric(NA)
  } else {
    fit <- lm(ret ~ market_return, data = data)
    std_residual <- as.numeric(sd(resid(fit)))
  }
  return(std_residual)
}


for (i in 1:22) {
stock_return <- market_return_monthly %>% 
  filter(date>=table1_final[i,"estimation_start"] & date<=table1_final[i,"estimation_end"])

dataset_name <- paste("stock_beta_forming", i, sep = "")
current_dataset <- get(dataset_name)

portfolio_estimation <- current_dataset %>%
  select(permno, group) %>% 
  left_join(stock_return, by = c("permno")) %>% 
  group_by(group, year, month) %>% 
  mutate(retp = mean(ret, na.rm = TRUE)) %>% 
  ungroup() %>% 
  select(group, date, retp, market_return) %>% 
  distinct()

stock_beta_estimation <- portfolio_estimation %>% 
  nest(data = c(date, retp, market_return)) %>% 
  mutate(beta = map(
    data,
    ~ estimate_capm_full(., )
  )) %>% 
  unnest(beta) %>% 
  select(group, beta) %>% 
  mutate(name = rep(c("beta","s_beta","Rsquared","s_ep"),20)) %>% 
  pivot_wider(names_from = name, values_from = beta)

s_Rp <- portfolio_estimation %>% 
  group_by(group) %>% 
  summarize(s_Rp = sd(retp))

portfolio_estimation_stock <- current_dataset %>%
  select(permno, group) %>% 
  left_join(stock_return, by = c("permno")) %>% 
  select(-year, -month)

sp_ei <- portfolio_estimation_stock %>% 
  nest(data = c(date, ret, market_return)) %>% 
  mutate(std_residual = map(
    data,
    ~ estimate_residual(., )
  )) %>% 
  unnest(std_residual) %>% 
  select(permno, group, std_residual) %>% 
  group_by(group) %>% 
  summarize(sp_ei = mean(std_residual))

temp <- full_join(stock_beta_estimation, s_Rp) %>% 
  full_join(sp_ei) %>% 
  mutate(ratio = s_ep/sp_ei) %>%
  select(group, beta, s_beta, Rsquared, s_Rp, s_ep, sp_ei, ratio)
  

assign(paste("portfolio_beta_estimation", i, sep = ""), temp)

file_name <- paste0("output/", "portfolio_beta_estimation",i, ".csv")
write_csv(temp, file_name)

}

## The CSV files for table2 that will be used in RMarkdown

