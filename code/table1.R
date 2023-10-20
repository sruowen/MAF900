
library(tidyverse)
library(RSQLite)
library(slider)
library(dbplyr)
library(furrr)

###### the following code is used to find the column one in table 1
# select the sample within the period 1
period1 <- crsp_monthly %>% 
  filter(date >= "1926-01-01"  & date <= "1938-12-31") %>% 
  filter(ret!="NA")
# count the number of stocks that in the first month of the testing period
stocks1 <- period1 %>% 
  filter(year(date)==1935 & month(date)==1) %>% 
  select(permno) %>% 
  unique()
# find and count the stocks that have complete 5 years of observations in estimation period
stocks2 <- period1 %>% 
  filter(date >= "1930-01-01"  & date <= "1934-12-31") %>% 
  select(permno, prc)  

stocks2_count <- stocks2 %>% 
  add_count(permno) %>% 
  select(permno, n) %>% 
  filter(n==60) %>% 
  select(permno) %>% 
  unique()
# find and count the stocks that have at least 4 years of observations in the portfolio formation period
stocks3 <- period1 %>% 
  filter(date >= "1926-01-01"  & date <= "1929-12-31") %>% 
  select(permno, prc)  

stocks3_count <- stocks3 %>% 
  add_count(permno) %>% 
  select(permno, n) %>% 
  filter(n>=48) %>% 
  select(permno) %>% 
  unique()
# find the stocks that meet the data requirements 
temp <- inner_join(stocks1, stocks2_count) %>% 
  inner_join(stocks3_count)
assign(paste("stock_", 1, sep = ""), temp)

# create a tibble to store the the dates of portfolio formation, estimation, and testing
key_dates <- tibble(
  formation_start = seq.Date(as.Date("1927-01-01"), as.Date("2007-01-01"), by = "4 years"),
  formation_end = seq.Date(as.Date("1933-12-31"), as.Date("2013-12-31"), by = "4 years"),
  estimation_start = seq.Date(as.Date("1934-01-01"), as.Date("2014-01-01"), by = "4 years"),
  estimation_end = seq.Date(as.Date("1938-12-31"), as.Date("2018-12-31"), by = "4 years"),
  testing_start = seq.Date(as.Date("1939-01-01"), as.Date("2019-01-01"), by = "4 years"),
  testing_end = seq.Date(as.Date("1942-12-31"), as.Date("2022-12-31"), by = "4 years")
)
# create a table to store the number of stocks in table 1
table1_num <- tibble(
  available_number = rep(NA, 21),
  final_number = rep(NA, 21)
)
# the following loop is used to find the information in table 1
# the logic is similar to the trial codes in the first period
for (i in 1:21) {
  period <- crsp_monthly %>% 
    filter(date >= key_dates$formation_start[i]  & date <= key_dates$testing_end[i]) %>% 
    filter(ret!="NA")
  
  stocks <- period %>% 
    filter(year(date)==year(key_dates$testing_start[i]) & month(date)==1) %>% 
    select(permno) %>% 
    unique()
  
  table1_num$avaiable_number[i]=nrow(stocks)
  
  stocks_estimation <- period %>% 
    filter(date >= key_dates$estimation_start[i]  & date <= key_dates$estimation_end[i]) %>% 
    select(permno, ret)  
  
  stocks_estimation_count <- stocks_estimation %>% 
    add_count(permno) %>% 
    select(permno, n) %>% 
    filter(n==60) %>% 
    select(permno) %>% 
    unique()
  
  stocks_formation <- period %>% 
    filter(date >= key_dates$formation_start[i]  & date <= key_dates$formation_end[i]) %>% 
    select(permno, ret)  
  
  stocks_formation_count <- stocks_formation %>% 
    add_count(permno) %>% 
    select(permno, n) %>% 
    filter(n>=48) %>% 
    select(permno) %>% 
    unique()
  
  temp <- inner_join(stocks, stocks_estimation_count) %>% 
    inner_join(stocks_formation_count)
  
  table1_num$final_number[i] = nrow(temp)
  
  assign(paste("stock_", i+1, sep = ""), temp)
}

# create the first row of table 1 with information from the trial
first_row <- tibble(
  formation_start = as.Date("1926-01-01"),
  formation_end = as.Date("1929-12-31"),
  estimation_start = as.Date("1930-01-01"),
  estimation_end = as.Date("1934-12-31"),
  testing_start = as.Date("1935-01-01"),
  testing_end = as.Date("1938-12-31"),
  available_number = nrow(stocks1),
  final_number = nrow(stock_1)
)
# create the table 1 by binding the existing tables together
table1_final <- rbind(first_row, bind_cols(key_dates, table1_num))
