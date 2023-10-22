
##### Function to calculate autocorrelation for each group
calculate_acf <- function(column) {
  acf_result <- acf(column, lag.max = 1)
  return(acf_result$acf[2])  # Extract autocorrelation at lag 1
}


#### the following code is the replication of the table 4 results for the whole sample period between 1935 to 2022
table4_whole <- market_return_monthly %>% 
  select(year, month, market_return) %>% 
  left_join(ff_rf) %>%
  drop_na() %>% 
  distinct() %>% 
  mutate(rm_rf = market_return - rf) 

table4_whole_result <- table4_whole %>%
  ungroup() %>% 
  mutate(rm_bar = mean(market_return),
         rm_sd = sd(market_return),
         rm_rf_bar = mean(rm_rf),
         rm_rf_sd = sd(rm_rf),
         rf_bar = mean(rf),
         rf_sd = sd(rf),
         acf_rm = calculate_acf(market_return),
         acf_rm_rf = calculate_acf(rm_rf),
         acf_rf = calculate_acf(rf),
         n = length(month),
         period = "whole"
  ) %>% 
  select(period, rm_bar, rm_rf_bar, rm_rf_sd, rf_bar, rm_sd, rf_sd, acf_rm, acf_rm_rf, acf_rf, n) %>% 
  distinct() %>% 
  mutate(rm_t = rm_bar/(rm_sd/sqrt(n)),
         rm_rf_t = rm_rf_bar/(rm_rf_sd/sqrt(n))) %>% 
  left_join(panela_first_row_table3 %>% select(period, gamma1_bar, gamma0_bar, gamma1_sd, gamma0_sd, gamma1_t, gamma0_t, acf_gamma1, acf_gamma0_rf)) %>% 
  rename(acf_gamma0 = acf_gamma0_rf)

table4_whole_final <- table4_whole_result %>% 
  mutate(rm_rf_srm = rm_rf_bar/rm_sd,
         gamma1_srm = gamma1_bar/rm_sd) %>% 
  mutate(year_min = 1935,
         year_max = 2022) %>% 
  select(year_min, year_max, rm_bar, rm_rf_bar, gamma1_bar, gamma0_bar, rf_bar, rm_rf_srm, gamma1_srm, rm_sd, gamma1_sd, gamma0_sd, rf_sd, rm_t, rm_rf_t, gamma1_t, gamma0_t, acf_rm, acf_rm_rf, acf_gamma1, acf_gamma0, acf_rf )


##### the following code is the replication of the table 4 results for the 9 longer testing periods between 1935 to 2022
table4_longer <- market_return_monthly %>% 
  select(year, month, market_return) %>% 
  left_join(ff_rf) %>%
  mutate(period = case_when(
    year >= 1935 & year <= 1945 ~ 1,
    year >= 1946 & year <= 1955 ~ 2,
    year >= 1956 & year <= 1965 ~ 3,
    year >= 1966 & year <= 1975 ~ 4,
    year >= 1976 & year <= 1985 ~ 5,
    year >= 1986 & year <= 1995 ~ 6,
    year >= 1996 & year <= 2005 ~ 7,
    year >= 2006 & year <= 2015 ~ 8,
    year >= 2016 & year <= 2022 ~ 9,
  )) %>% 
  drop_na() %>% 
  distinct()

table4_longer_result <- table4_longer %>% 
  mutate(rm_rf = market_return - rf) %>% 
  group_by(period) %>% 
  mutate(rm_bar = mean(market_return),
         rm_sd = sd(market_return),
         rm_rf_bar = mean(rm_rf),
         rm_rf_sd = sd(rm_rf),
         rf_bar = mean(rf),
         rf_sd = sd(rf),
         acf_rm = calculate_acf(market_return),
         acf_rm_rf = calculate_acf(rm_rf),
         acf_rf = calculate_acf(rf),
         n = length(month)
  ) %>% 
  select(period, rm_bar, rm_rf_bar, rm_rf_sd, rf_bar, rm_sd, rf_sd, acf_rm, acf_rm_rf, acf_rf, n) %>% 
  distinct() %>% 
  mutate(rm_t = rm_bar/(rm_sd/sqrt(n)),
         rm_rf_t = rm_rf_bar/(rm_rf_sd/sqrt(n))) %>% 
  left_join(panela_longer_table3 %>% select(period, gamma1_bar, gamma0_bar, gamma1_sd, gamma0_sd, gamma1_t, gamma0_t, acf_gamma1, acf_gamma0_rf)) %>% 
  rename(acf_gamma0 = acf_gamma0_rf)

table4_longer_final <- table4_longer_result %>% 
  mutate(rm_rf_srm = rm_rf_bar/rm_sd,
         gamma1_srm = gamma1_bar/rm_sd) %>%
  left_join(timeline_period_longer) %>% 
  ungroup() %>% 
  select(year_min, year_max, rm_bar, rm_rf_bar, gamma1_bar, gamma0_bar, rf_bar, rm_rf_srm, gamma1_srm, rm_sd, gamma1_sd, gamma0_sd, rf_sd, rm_t, rm_rf_t, gamma1_t, gamma0_t, acf_rm, acf_rm_rf, acf_gamma1, acf_gamma0, acf_rf )


##### the following code is the replication of the table 4 results for the 17 testing periods between 1935 to 2022
table4 <- market_return_monthly %>% 
  select(year, month, market_return) %>% 
  left_join(ff_rf) %>%
  mutate(period = case_when(
    year >= 1935 & year <= 1940 ~ 1,
    year >= 1941 & year <= 1945 ~ 2,
    year >= 1946 & year <= 1950 ~ 3,
    year >= 1951 & year <= 1955 ~ 4,
    year >= 1956 & year <= 1960 ~ 5,
    year >= 1961 & year <= 1965 ~ 6,
    year >= 1966 & year <= 1970 ~ 7,
    year >= 1971 & year <= 1975 ~ 8,
    year >= 1976 & year <= 1980 ~ 9,
    year >= 1981 & year <= 1985 ~ 10,
    year >= 1986 & year <= 1990 ~ 11,
    year >= 1991 & year <= 1995 ~ 12,
    year >= 1996 & year <= 2000 ~ 13,
    year >= 2001 & year <= 2005 ~ 14,
    year >= 2006 & year <= 2010 ~ 15,
    year >= 2011 & year <= 2015 ~ 16,
    year >= 2016 & year <= 2022 ~ 17,
  )) %>% 
  drop_na() %>% 
  distinct()

table4_result <- table4 %>% 
  mutate(rm_rf = market_return - rf) %>% 
  group_by(period) %>% 
  mutate(rm_bar = mean(market_return),
         rm_sd = sd(market_return),
         rm_rf_bar = mean(rm_rf),
         rm_rf_sd = sd(rm_rf),
         rf_bar = mean(rf),
         rf_sd = sd(rf),
         acf_rm = calculate_acf(market_return),
         acf_rm_rf = calculate_acf(rm_rf),
         acf_rf = calculate_acf(rf),
         n = length(month)
  ) %>% 
  select(period, rm_bar, rm_rf_bar, rm_rf_sd, rf_bar, rm_sd, rf_sd, acf_rm, acf_rm_rf, acf_rf, n) %>% 
  distinct() %>% 
  mutate(rm_t = rm_bar/(rm_sd/sqrt(n)),
         rm_rf_t = rm_rf_bar/(rm_rf_sd/sqrt(n))) %>% 
  left_join(panela_table3 %>% select(period, gamma1_bar, gamma0_bar, gamma1_sd, gamma0_sd, gamma1_t, gamma0_t, acf_gamma1, acf_gamma0_rf)) %>% 
  rename(acf_gamma0 = acf_gamma0_rf)

table4_final <- table4_result %>% 
  mutate(rm_rf_srm = rm_rf_bar/rm_sd,
         gamma1_srm = gamma1_bar/rm_sd) %>% 
  left_join(timeline_period) %>% 
  ungroup() %>% 
  select(year_min, year_max, rm_bar, rm_rf_bar, gamma1_bar, gamma0_bar, rf_bar, rm_rf_srm, gamma1_srm, rm_sd, gamma1_sd, gamma0_sd, rf_sd, rm_t, rm_rf_t, gamma1_t, gamma0_t, acf_rm, acf_rm_rf, acf_gamma1, acf_gamma0, acf_rf )

table4_final <- rbind(table4_whole_final, table4_longer_final, table4_final)

write_csv(table4_final, "output/table4.csv") 

