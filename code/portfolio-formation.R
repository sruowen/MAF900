##Important libraries##
library(tidyverse)
library(RSQLite)
library(slider)
library(dbplyr)
library(furrr)

####construct the Fisher's Arithmetic Index as the proxy for the market return
market_return_monthly <- crsp_monthly %>% 
  select(permno, date, ret) %>% 
  mutate(year = year(date), month = month(date))

market_return_monthly <- market_return_monthly %>% 
  group_by(year, month) %>% 
  mutate(market_return = mean(ret, na.rm = TRUE)) 

# estimate the betas in the portfolio forming period
# create a function
estimate_capm <- function(data, min_obs = 1) {
  if (nrow(data) < min_obs) {
    beta <- as.numeric(NA)
  } else {
    fit <- lm(ret ~ market_return, data = data)
    beta <- as.numeric(coefficients(fit)[2])
  }
  return(beta)
}


# create the dataset with stocks and market return in the specified period
for (i in 1:22) {
  stock_return <- market_return_monthly %>% 
    filter(date>=table1_final[i,"formation_start"] & date<=table1_final[i,"formation_end"]) 
 
   # Construct the dataset name dynamically
  dataset_name <- paste("stock_", i, sep = "")
  
  # Use get() to retrieve the dataset
  current_dataset <- get(dataset_name)
  
  stock_portfolio <- current_dataset %>% 
    left_join(stock_return, by = c("permno")) %>% 
    select(permno, date, ret, market_return)
  
  # estimate the beta in the portfolio forming period
  stock_beta_forming <- stock_portfolio %>% 
    nest(data = c(date, ret, market_return)) %>% 
    mutate(beta = map(
      data,
      ~ estimate_capm(., )
    )) %>% 
    unnest(beta) %>% 
    select(permno, beta) %>% 
    arrange(beta)
  
  # determine the number of the stocks in each portfolio
  
  
  N = nrow(current_dataset)
  group_size = round(N/20)
  first_size = group_size + round(0.5*(N - 20*group_size))
  last_size = N - group_size*18 - first_size
  stock_beta_forming$group <- rep(c(rep(1, first_size), rep(2:19, each = group_size), rep(20, last_size)))
  
  assign(paste("stock_beta_forming", i, sep = ""), stock_beta_forming)
}

