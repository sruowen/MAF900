

# Function to calculate autocorrelation for each group
calculate_acf <- function(column) {
  acf_result <- acf(column, lag.max = 1)
  return(acf_result$acf[2])  # Extract autocorrelation at lag 1
}

#### combine 22 datasets of time series results for regression of panel A, B, C and D
#### into one datasets
panela_raw <- bind_rows(mget(paste0("panela_results", 1:22)))
panelb_raw <- bind_rows(mget(paste0("panelb_results", 1:22)))
panelc_raw <- bind_rows(mget(paste0("panelc_results", 1:22)))
paneld_raw <- bind_rows(mget(paste0("paneld_results", 1:22)))

#### the following code is the replication of the summary results for the whole testing period from 1935 to 2022
panela_first_row_table3 <- panela_raw %>%
  left_join(ff_rf) %>% 
  mutate(gamma0_rf = gamma0 - rf) %>% 
  mutate(gamma0_bar = mean(gamma0),
         gamma0_sd = sd(gamma0),
         gamma1_bar = mean(gamma1),
         gamma1_sd = sd(gamma1),
         gamma0_rf_bar = mean(gamma0_rf),
         gamma0_rf_sd = sd(gamma0_rf),
         rsquared_bar = mean(adj_r_squared),
         rsquared_sd= sd(adj_r_squared),
         acf_gamma0_rf = calculate_acf(gamma0_rf),
         acf_gamma1 = calculate_acf(gamma1),
         n = length(month)) %>% 
  select(gamma0_bar, gamma0_sd, gamma1_bar, gamma1_sd, gamma0_rf_bar, gamma0_rf_sd, acf_gamma0_rf, acf_gamma1, rsquared_bar, rsquared_sd, n) %>% 
  distinct() 

panela_first_row_table3 <- panela_first_row_table3 %>% 
  mutate(gamma0_t = gamma0_bar/(gamma0_sd/sqrt(n)),
         gamma1_t = gamma1_bar/(gamma1_sd/sqrt(n)),
         gamma0_rf_t = gamma0_rf_bar/(gamma0_rf_sd/sqrt(n))) %>% 
  mutate(period = "whole",
         year_min = 1935,
         year_max = 2022) %>% 
  select(period, everything())

#### regression for panel B for the whole period
panelb_first_row_table3 <- panelb %>% 
  left_join(ff_rf) %>% 
  mutate(gamma0_rf = gamma0 - rf) %>% 
  mutate(gamma0_bar = mean(gamma0),
         gamma0_sd = sd(gamma0),
         gamma1_bar = mean(gamma1),
         gamma1_sd = sd(gamma1),
         gamma2_bar = mean(gamma2),
         gamma2_sd = sd(gamma2),
         gamma0_rf_bar = mean(gamma0_rf),
         gamma0_rf_sd = sd(gamma0_rf),
         rsquared_bar = mean(adj_r_squared),
         rsquared_sd= sd(adj_r_squared),
         acf_gamma0_rf = calculate_acf(gamma0_rf),
         acf_gamma1 = calculate_acf(gamma1),
         acf_gamma2 = calculate_acf(gamma2),
         n = length(month)) %>% 
  select(gamma0_bar, gamma0_sd, gamma1_bar, gamma1_sd,gamma2_bar, gamma2_sd, gamma0_rf_bar, gamma0_rf_sd, acf_gamma0_rf, acf_gamma1, acf_gamma2, rsquared_bar, rsquared_sd, n) %>% 
  distinct() 

panelb_first_row_table3 <- panelb_table3 %>% 
  mutate(gamma0_t = gamma0_bar/(gamma0_sd/sqrt(n)),
         gamma1_t = gamma1_bar/(gamma1_sd/sqrt(n)),
         gamma2_t = gamma2_bar/(gamma2_sd/sqrt(n)),
         gamma0_rf_t = gamma0_rf_bar/(gamma0_rf_sd/sqrt(n)))%>% 
  mutate(period = "whole",
         year_min = 1935,
         year_max = 2022) %>% 
  ungroup() %>% 
  select(period, everything())

#### regression for panel C for the whole period
panelc_first_row_table3 <- panelc_raw %>% 
  left_join(ff_rf) %>% 
  mutate(gamma0_rf = gamma0 - rf) %>% 
  mutate(gamma0_bar = mean(gamma0),
         gamma0_sd = sd(gamma0),
         gamma1_bar = mean(gamma1),
         gamma1_sd = sd(gamma1),
         gamma3_bar = mean(gamma3),
         gamma3_sd = sd(gamma3),
         gamma0_rf_bar = mean(gamma0_rf),
         gamma0_rf_sd = sd(gamma0_rf),
         rsquared_bar = mean(adj_r_squared),
         rsquared_sd= sd(adj_r_squared),
         acf_gamma0_rf = calculate_acf(gamma0_rf),
         acf_gamma1 = calculate_acf(gamma1),
         acf_gamma3 = calculate_acf(gamma3),
         n = length(month)) %>% 
  select(gamma0_bar, gamma0_sd, gamma1_bar, gamma1_sd, gamma3_bar, gamma3_sd, gamma0_rf_bar, gamma0_rf_sd, acf_gamma0_rf, acf_gamma1, acf_gamma3, rsquared_bar, rsquared_sd, n) %>% 
  distinct() 

panelc_first_row_table3 <- panelc_first_row_table3 %>% 
  mutate(gamma0_t = gamma0_bar/(gamma0_sd/sqrt(n)),
         gamma1_t = gamma1_bar/(gamma1_sd/sqrt(n)),
         gamma3_t = gamma3_bar/(gamma3_sd/sqrt(n)),
         gamma0_rf_t = gamma0_rf_bar/(gamma0_rf_sd/sqrt(n)))%>% 
  mutate(period = "whole",
         year_min = 1935,
         year_max = 2022) %>% 
  select(period, everything())

#### regression for panel D for the whole period
paneld_first_row_table3 <- paneld_raw %>%
  left_join(ff_rf) %>% 
  mutate(gamma0_rf = gamma0 - rf) %>% 
  mutate(gamma0_bar = mean(gamma0),
         gamma0_sd = sd(gamma0),
         gamma1_bar = mean(gamma1),
         gamma1_sd = sd(gamma1),
         gamma2_bar = mean(gamma2),
         gamma2_sd = sd(gamma2),
         gamma3_bar = mean(gamma3),
         gamma3_sd = sd(gamma3),
         gamma0_rf_bar = mean(gamma0_rf),
         gamma0_rf_sd = sd(gamma0_rf),
         rsquared_bar = mean(adj_r_squared),
         rsquared_sd= sd(adj_r_squared),
         acf_gamma0_rf = calculate_acf(gamma0_rf),
         acf_gamma1 = calculate_acf(gamma1),
         acf_gamma2 = calculate_acf(gamma2),
         acf_gamma3 = calculate_acf(gamma3),
         n = length(month)) %>% 
  select(gamma0_bar, gamma0_sd, gamma1_bar, gamma1_sd,gamma2_bar, gamma2_sd, gamma3_bar, gamma3_sd, gamma0_rf_bar, gamma0_rf_sd, acf_gamma0_rf, acf_gamma1, acf_gamma2, acf_gamma3, rsquared_bar, rsquared_sd, n) %>% 
  distinct() 

paneld_first_row_table3 <- paneld_first_row_table3 %>% 
  mutate(gamma0_t = gamma0_bar/(gamma0_sd/sqrt(n)),
         gamma1_t = gamma1_bar/(gamma1_sd/sqrt(n)),
         gamma2_t = gamma2_bar/(gamma2_sd/sqrt(n)),
         gamma3_t = gamma3_bar/(gamma3_sd/sqrt(n)),
         gamma0_rf_t = gamma0_rf_bar/(gamma0_rf_sd/sqrt(n)))%>% 
  mutate(period = "whole",
         year_min = 1935,
         year_max = 2022) %>% 
  select(period, everything())

#### the following code is the replication of the summary results for the longer testing period from 1935 to 2022
#### panel A for ten year period
panela_longer <- panela_raw %>%
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
  left_join(ff_rf) %>% 
  mutate(gamma0_rf = gamma0 - rf)

timeline_year_month <- panela_longer %>% 
  select(period, year, month)

timeline_period <- panela_longer %>% 
  select(period, year) %>% 
  group_by(period) %>% 
  mutate(year_min = min(year), 
         year_max = max(year)) %>% 
  select(-year) %>% 
  distinct()

panela_longer_table3 <- panela_longer %>% 
  group_by(period) %>% 
  mutate(gamma0_bar = mean(gamma0),
         gamma0_sd = sd(gamma0),
         gamma1_bar = mean(gamma1),
         gamma1_sd = sd(gamma1),
         gamma0_rf_bar = mean(gamma0_rf),
         gamma0_rf_sd = sd(gamma0_rf),
         rsquared_bar = mean(adj_r_squared),
         rsquared_sd= sd(adj_r_squared),
         acf_gamma0_rf = calculate_acf(gamma0_rf),
         acf_gamma1 = calculate_acf(gamma1),
         n = length(month)) %>% 
  select(gamma0_bar, gamma0_sd, gamma1_bar, gamma1_sd, gamma0_rf_bar, gamma0_rf_sd, acf_gamma0_rf, acf_gamma1, rsquared_bar, rsquared_sd, n) %>% 
  distinct()  %>% 
  mutate(gamma0_t = gamma0_bar/(gamma0_sd/sqrt(n)),
         gamma1_t = gamma1_bar/(gamma1_sd/sqrt(n)),
         gamma0_rf_t = gamma0_rf_bar/(gamma0_rf_sd/sqrt(n)))%>% 
  left_join(timeline_period)

#### panel B for ten year period
panelb_longer <- panelb_raw %>%
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
  left_join(ff_rf) %>% 
  mutate(gamma0_rf = gamma0 - rf)

timeline_year_month <- panelb_longer %>% 
  select(period, year, month)

timeline_period <- panelb_longer %>% 
  select(period, year) %>% 
  group_by(period) %>% 
  mutate(year_min = min(year), 
         year_max = max(year)) %>% 
  select(-year) %>% 
  distinct()

panelb_longer_table3 <- panelb_longer %>% 
  group_by(period) %>% 
  mutate(gamma0_bar = mean(gamma0),
         gamma0_sd = sd(gamma0),
         gamma1_bar = mean(gamma1),
         gamma1_sd = sd(gamma1),
         gamma2_bar = mean(gamma2),
         gamma2_sd = sd(gamma2),
         gamma0_rf_bar = mean(gamma0_rf),
         gamma0_rf_sd = sd(gamma0_rf),
         rsquared_bar = mean(adj_r_squared),
         rsquared_sd= sd(adj_r_squared),
         acf_gamma0_rf = calculate_acf(gamma0_rf),
         acf_gamma1 = calculate_acf(gamma1),
         acf_gamma2 = calculate_acf(gamma2),
         n = length(month)) %>% 
  select(gamma0_bar, gamma0_sd, gamma1_bar, gamma1_sd,gamma2_bar, gamma2_sd, gamma0_rf_bar, gamma0_rf_sd, acf_gamma0_rf, acf_gamma1, acf_gamma2, rsquared_bar, rsquared_sd, n) %>% 
  distinct() %>% 
  left_join(timeline_period)

panelb_longer_table3 <- panelb_longer_table3 %>% 
  mutate(gamma0_t = gamma0_bar/(gamma0_sd/sqrt(n)),
         gamma1_t = gamma1_bar/(gamma1_sd/sqrt(n)),
         gamma2_t = gamma2_bar/(gamma2_sd/sqrt(n)),
         gamma0_rf_t = gamma0_rf_bar/(gamma0_rf_sd/sqrt(n))) %>% 
  ungroup()


#### panel B for ten year period
panelc_longer <- panelc_raw %>%
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
  left_join(ff_rf) %>% 
  mutate(gamma0_rf = gamma0 - rf)

timeline_year_month <- panelc_longer %>% 
  select(period, year, month)

timeline_period <- panelc_longer %>% 
  select(period, year) %>% 
  group_by(period) %>% 
  mutate(year_min = min(year), 
         year_max = max(year)) %>% 
  select(-year) %>% 
  distinct()

panelc_longer_table3 <- panelc_longer %>% 
  group_by(period) %>% 
  mutate(gamma0_bar = mean(gamma0),
         gamma0_sd = sd(gamma0),
         gamma1_bar = mean(gamma1),
         gamma1_sd = sd(gamma1),
         gamma3_bar = mean(gamma3),
         gamma3_sd = sd(gamma3),
         gamma0_rf_bar = mean(gamma0_rf),
         gamma0_rf_sd = sd(gamma0_rf),
         rsquared_bar = mean(adj_r_squared),
         rsquared_sd= sd(adj_r_squared),
         acf_gamma0_rf = calculate_acf(gamma0_rf),
         acf_gamma1 = calculate_acf(gamma1),
         acf_gamma3 = calculate_acf(gamma3),
         n = length(month)) %>% 
  select(gamma0_bar, gamma0_sd, gamma1_bar, gamma1_sd, gamma3_bar, gamma3_sd, gamma0_rf_bar, gamma0_rf_sd, acf_gamma0_rf, acf_gamma1, acf_gamma3, rsquared_bar, rsquared_sd, n) %>% 
  distinct() %>% 
  left_join(timeline_period)

panelc_longer_table3 <- panelc_longer_table3 %>% 
  mutate(gamma0_t = gamma0_bar/(gamma0_sd/sqrt(n)),
         gamma1_t = gamma1_bar/(gamma1_sd/sqrt(n)),
         gamma3_t = gamma3_bar/(gamma3_sd/sqrt(n)),
         gamma0_rf_t = gamma0_rf_bar/(gamma0_rf_sd/sqrt(n)))

#### panel B for ten year period
paneld_longer <- paneld_raw %>%
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
  left_join(ff_rf) %>% 
  mutate(gamma0_rf = gamma0 - rf)

timeline_year_month <- paneld_longer %>% 
  select(period, year, month)

timeline_period_longer <- paneld_longer %>% 
  select(period, year) %>% 
  group_by(period) %>% 
  mutate(year_min = min(year), 
         year_max = max(year)) %>% 
  select(-year) %>% 
  distinct()

paneld_longer_table3 <- paneld_longer %>% 
  group_by(period) %>% 
  mutate(gamma0_bar = mean(gamma0),
         gamma0_sd = sd(gamma0),
         gamma1_bar = mean(gamma1),
         gamma1_sd = sd(gamma1),
         gamma2_bar = mean(gamma2),
         gamma2_sd = sd(gamma2),
         gamma3_bar = mean(gamma3),
         gamma3_sd = sd(gamma3),
         gamma0_rf_bar = mean(gamma0_rf),
         gamma0_rf_sd = sd(gamma0_rf),
         rsquared_bar = mean(adj_r_squared),
         rsquared_sd= sd(adj_r_squared),
         acf_gamma0_rf = calculate_acf(gamma0_rf),
         acf_gamma1 = calculate_acf(gamma1),
         acf_gamma2 = calculate_acf(gamma2),
         acf_gamma3 = calculate_acf(gamma3),
         n = length(month)) %>% 
  select(gamma0_bar, gamma0_sd, gamma1_bar, gamma1_sd,gamma2_bar, gamma2_sd, gamma3_bar, gamma3_sd, gamma0_rf_bar, gamma0_rf_sd, acf_gamma0_rf, acf_gamma1, acf_gamma2, acf_gamma3, rsquared_bar, rsquared_sd, n) %>% 
  distinct() %>% 
  left_join(timeline_period)

paneld_longer_table3 <- paneld_longer_table3 %>% 
  mutate(gamma0_t = gamma0_bar/(gamma0_sd/sqrt(n)),
         gamma1_t = gamma1_bar/(gamma1_sd/sqrt(n)),
         gamma2_t = gamma2_bar/(gamma2_sd/sqrt(n)),
         gamma3_t = gamma3_bar/(gamma3_sd/sqrt(n)),
         gamma0_rf_t = gamma0_rf_bar/(gamma0_rf_sd/sqrt(n)))

#### the following part is replication of summary results for the regression for each of 22 testing periods

panela <- panela_raw %>%
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
  left_join(ff_rf) %>% 
  mutate(gamma0_rf = gamma0 - rf)

timeline_year_month <- panela %>% 
  select(period, year, month)

timeline_period <- panela %>% 
  select(period, year) %>% 
  group_by(period) %>% 
  mutate(year_min = min(year), 
         year_max = max(year)) %>% 
  select(-year) %>% 
  distinct()

panela_table3 <- panela %>% 
  group_by(period) %>% 
  mutate(gamma0_bar = mean(gamma0),
         gamma0_sd = sd(gamma0),
         gamma1_bar = mean(gamma1),
         gamma1_sd = sd(gamma1),
         gamma0_rf_bar = mean(gamma0_rf),
         gamma0_rf_sd = sd(gamma0_rf),
         rsquared_bar = mean(adj_r_squared),
         rsquared_sd= sd(adj_r_squared),
         acf_gamma0_rf = calculate_acf(gamma0_rf),
         acf_gamma1 = calculate_acf(gamma1),
         n = length(month)) %>% 
  select(gamma0_bar, gamma0_sd, gamma1_bar, gamma1_sd, gamma0_rf_bar, gamma0_rf_sd, acf_gamma0_rf, acf_gamma1, rsquared_bar, rsquared_sd, n) %>% 
  distinct()  %>% 
  mutate(gamma0_t = gamma0_bar/(gamma0_sd/sqrt(n)),
         gamma1_t = gamma1_bar/(gamma1_sd/sqrt(n)),
         gamma0_rf_t = gamma0_rf_bar/(gamma0_rf_sd/sqrt(n)))%>% 
  left_join(timeline_period)

write_csv(panela_table3, "output/panela_table3.csv")

###table 3 for panel B regression
panelb <- panelb_raw %>%
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
  left_join(ff_rf) %>% 
  mutate(gamma0_rf = gamma0 - rf)

timeline_year_month <- panelb %>% 
  select(period, year, month)

timeline_period <- panelb %>% 
  select(period, year) %>% 
  group_by(period) %>% 
  mutate(year_min = min(year), 
         year_max = max(year)) %>% 
  select(-year) %>% 
  distinct()

panelb_table3 <- panelb %>% 
  group_by(period) %>% 
  mutate(gamma0_bar = mean(gamma0),
         gamma0_sd = sd(gamma0),
         gamma1_bar = mean(gamma1),
         gamma1_sd = sd(gamma1),
         gamma2_bar = mean(gamma2),
         gamma2_sd = sd(gamma2),
         gamma0_rf_bar = mean(gamma0_rf),
         gamma0_rf_sd = sd(gamma0_rf),
         rsquared_bar = mean(adj_r_squared),
         rsquared_sd= sd(adj_r_squared),
         acf_gamma0_rf = calculate_acf(gamma0_rf),
         acf_gamma1 = calculate_acf(gamma1),
         acf_gamma2 = calculate_acf(gamma2),
         n = length(month)) %>% 
  select(gamma0_bar, gamma0_sd, gamma1_bar, gamma1_sd,gamma2_bar, gamma2_sd, gamma0_rf_bar, gamma0_rf_sd, acf_gamma0_rf, acf_gamma1, acf_gamma2, rsquared_bar, rsquared_sd, n) %>% 
  distinct() %>% 
  left_join(timeline_period)

panelb_table3 <- panelb_table3 %>% 
  mutate(gamma0_t = gamma0_bar/(gamma0_sd/sqrt(n)),
         gamma1_t = gamma1_bar/(gamma1_sd/sqrt(n)),
         gamma2_t = gamma2_bar/(gamma2_sd/sqrt(n)),
         gamma0_rf_t = gamma0_rf_bar/(gamma0_rf_sd/sqrt(n))) %>% 
  ungroup()

###table 3 for panel C regression
panelc <- panelc_raw %>%
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
  left_join(ff_rf) %>% 
  mutate(gamma0_rf = gamma0 - rf)

timeline_year_month <- panelc %>% 
  select(period, year, month)

timeline_period <- panelc %>% 
  select(period, year) %>% 
  group_by(period) %>% 
  mutate(year_min = min(year), 
         year_max = max(year)) %>% 
  select(-year) %>% 
  distinct()

panelc_table3 <- panelc %>% 
  group_by(period) %>% 
  mutate(gamma0_bar = mean(gamma0),
         gamma0_sd = sd(gamma0),
         gamma1_bar = mean(gamma1),
         gamma1_sd = sd(gamma1),
         gamma3_bar = mean(gamma3),
         gamma3_sd = sd(gamma3),
         gamma0_rf_bar = mean(gamma0_rf),
         gamma0_rf_sd = sd(gamma0_rf),
         rsquared_bar = mean(adj_r_squared),
         rsquared_sd= sd(adj_r_squared),
         acf_gamma0_rf = calculate_acf(gamma0_rf),
         acf_gamma1 = calculate_acf(gamma1),
         acf_gamma3 = calculate_acf(gamma3),
         n = length(month)) %>% 
  select(gamma0_bar, gamma0_sd, gamma1_bar, gamma1_sd, gamma3_bar, gamma3_sd, gamma0_rf_bar, gamma0_rf_sd, acf_gamma0_rf, acf_gamma1, acf_gamma3, rsquared_bar, rsquared_sd, n) %>% 
  distinct() %>% 
  left_join(timeline_period)

panelc_table3 <- panelc_table3 %>% 
  mutate(gamma0_t = gamma0_bar/(gamma0_sd/sqrt(n)),
         gamma1_t = gamma1_bar/(gamma1_sd/sqrt(n)),
         gamma3_t = gamma3_bar/(gamma3_sd/sqrt(n)),
         gamma0_rf_t = gamma0_rf_bar/(gamma0_rf_sd/sqrt(n)))

write_csv(panelc_table3, "output/panelc_table3.csv")


###table 3 for panel D regression
paneld <- paneld_raw %>%
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
  left_join(ff_rf) %>% 
  mutate(gamma0_rf = gamma0 - rf)

timeline_year_month <- paneld %>% 
  select(period, year, month)

timeline_period <- paneld %>% 
  select(period, year) %>% 
  group_by(period) %>% 
  mutate(year_min = min(year), 
         year_max = max(year)) %>% 
  select(-year) %>% 
  distinct()

paneld_table3 <- paneld %>% 
  group_by(period) %>% 
  mutate(gamma0_bar = mean(gamma0),
         gamma0_sd = sd(gamma0),
         gamma1_bar = mean(gamma1),
         gamma1_sd = sd(gamma1),
         gamma2_bar = mean(gamma2),
         gamma2_sd = sd(gamma2),
         gamma3_bar = mean(gamma3),
         gamma3_sd = sd(gamma3),
         gamma0_rf_bar = mean(gamma0_rf),
         gamma0_rf_sd = sd(gamma0_rf),
         rsquared_bar = mean(adj_r_squared),
         rsquared_sd= sd(adj_r_squared),
         acf_gamma0_rf = calculate_acf(gamma0_rf),
         acf_gamma1 = calculate_acf(gamma1),
         acf_gamma2 = calculate_acf(gamma2),
         acf_gamma3 = calculate_acf(gamma3),
         n = length(month)) %>% 
  select(gamma0_bar, gamma0_sd, gamma1_bar, gamma1_sd,gamma2_bar, gamma2_sd, gamma3_bar, gamma3_sd, gamma0_rf_bar, gamma0_rf_sd, acf_gamma0_rf, acf_gamma1, acf_gamma2, acf_gamma3, rsquared_bar, rsquared_sd, n) %>% 
  distinct() %>% 
  left_join(timeline_period)

paneld_table3 <- paneld_table3 %>% 
  mutate(gamma0_t = gamma0_bar/(gamma0_sd/sqrt(n)),
         gamma1_t = gamma1_bar/(gamma1_sd/sqrt(n)),
         gamma2_t = gamma2_bar/(gamma2_sd/sqrt(n)),
         gamma3_t = gamma3_bar/(gamma3_sd/sqrt(n)),
         gamma0_rf_t = gamma0_rf_bar/(gamma0_rf_sd/sqrt(n)))

write_csv(paneld_table3, "output/paneld_table3.csv")

#### the following the combined results for the panel A to panel D of table 3
#### panel A
panela_table3_whole <- rbind(panela_first_row_table3, panela_longer_table3, panela_table3)
panela_table3_whole <- panela_table3_whole %>% 
  select(year_min, year_max, gamma0_bar, gamma1_bar, gamma0_rf_bar, gamma0_sd, gamma1_sd, acf_gamma0_rf, acf_gamma1, gamma0_t, gamma1_t, gamma0_rf_t, rsquared_bar, rsquared_sd) 
write_csv(panela_table3_whole, "output/panela_table3_complete.csv")         

#### panel B
panelb_table3_whole <- rbind(panelb_first_row_table3 %>% select(-period), panelb_longer_table3 %>% select(-period), panelb_table3 %>% select(-period))
panelb_table3_whole <- panelb_table3_whole %>% 
  select(year_min, year_max, gamma0_bar, gamma1_bar, gamma2_bar, gamma0_rf_bar, gamma0_sd, gamma1_sd, gamma2_sd, acf_gamma0_rf, acf_gamma1, acf_gamma2, gamma0_t, gamma1_t, gamma2_t, gamma0_rf_t, rsquared_bar, rsquared_sd) 
write_csv(panelb_table3_whole, "output/panelb_table3_complete.csv")         

#### panel C
panelc_table3_whole <- rbind(panelc_first_row_table3, panelc_longer_table3, panelc_table3)
panelc_table3_whole <- panelc_table3_whole %>% 
  select(year_min, year_max, gamma0_bar, gamma1_bar, gamma3_bar, gamma0_rf_bar, gamma0_sd, gamma1_sd, gamma3_sd, acf_gamma0_rf, acf_gamma1, acf_gamma3, gamma0_t, gamma1_t, gamma3_t, gamma0_rf_t, rsquared_bar, rsquared_sd) 
write_csv(panelc_table3_whole, "output/panelc_table3_complete.csv")         

#### panel D
paneld_table3_whole <- rbind(paneld_first_row_table3, paneld_longer_table3, paneld_table3)
paneld_table3_whole <- paneld_table3_whole %>% 
  select(year_min, year_max, gamma0_bar, gamma1_bar, gamma2_bar, gamma3_bar, gamma0_rf_bar, gamma0_sd, gamma1_sd, gamma2_sd, gamma3_sd, acf_gamma0_rf, acf_gamma1, acf_gamma2, acf_gamma3, gamma0_t, gamma1_t, gamma2_t, gamma3_t, gamma0_rf_t, rsquared_bar, rsquared_sd) 

write_csv(paneld_table3_whole, "output/paneld_table3_complete.csv")         

