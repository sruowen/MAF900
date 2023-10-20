install.packages("tidyverse")
install.packages("RSQLite")
install.packages("slider")
install.packages("dbplyr")
install.packages("furrr")

library(tidyverse)
library(RSQLite)
library(slider)
library(dbplyr)
library(furrr)

MAF900_final <- dbConnect(
  SQLite(),
  "data/MAF900_final.sqlite",
  extended_types = TRUE  
)

# prepare the data 
# Collect Data from CRSP 
start_date <- ymd("1926-01-01")
end_date <- ymd("2022-12-31")
crsp_monthly<- tbl(wrds, sql("select * from crsp_a_stock.msf")) |>
  filter(date >= start_date  & date <= end_date) |>
  select(permno,date,hexcd,ret,retx, prc, shrout,altprc, cfacpr) |> 
  filter(hexcd==1) |>  # select the NYSE stocks
  collect()

dbWriteTable(MAF900_final,
             "crsp_monthly",
             value = crsp_monthly,
             overwrite = TRUE 
)

# collect monthly data on fama-french 3 factors
temp <- tempfile(fileext = ".zip")
download.file("http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/F-F_Research_Data_Factors_CSV.zip",temp)
temp1<- unzip(temp, exdir = ".")
ff_3factors_monthly <- read_csv(temp1, skip=3) 
names(ff_3factors_monthly) <- c('dt', 'rmrf', 'smb', 'hml', 'rf')
unlink(temp)
unlink(temp1)

ff_rf <- ff_3factors_monthly |> 
  filter(nchar(dt) == 6) |> 
  mutate(yr = str_sub(dt,1,4), mon= str_sub(dt,-2,-1),  
         date = make_date(year= yr, month = mon, day = 01), 
         mkt_excess = as.numeric(rmrf), smb = as.numeric(smb),
         hml = as.numeric(hml), rf = as.numeric(rf)) |> 
  filter(date >=start_date & date <= end_date) |> 
  select(c('date','rf')) %>% 
  mutate(year = year(date),
         month = month(date),
         rf = rf/100)
dbWriteTable(MAF900_final,
             "ff_rf",
             value = ff_rf,
             overwrite = TRUE
)
