rm(list = ls())

dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(dir);

require(tidyverse)

set.seed(1)

num_accidents <- 30000

# Accidents between 2010 and 2019
accident_date <- as.Date('2010-01-01') + floor(runif(num_accidents) * as.numeric((as.Date('2021-01-01') - as.Date('2010-01-01'))))

# Reporting delay lognormally distributed
reporting_date <- accident_date + floor(rlnorm(accident_date, meanlog = 2, sdlog = 1))
reporting_delay <- as.numeric(reporting_date - accident_date)

settlement_date <- reporting_date + floor(rlnorm(accident_date, meanlog = 4, sdlog = 1))
settlement_delay <- as.numeric(settlement_date - accident_date)

reserving_data <- data.frame();

for(i in 1:num_accidents) {
  if(i %% 1000 == 0) {
    cat(i, '\n');
  }
  
  payment_delay <- ceiling(cumsum(rexp(100, rate = 1/(30*log(2:101)))))
  payment_size <- rgamma(100, shape = 5, scale = 50 * log(payment_delay))
  
  # Add a zero payment at reporting to preserve claims without payments
  claim_data <- data.frame(accident_number = i,
                           accident_date = accident_date[i],
                           reporting_date = reporting_date[i],
                           settlement_date = settlement_date[i],
                           reporting_delay = reporting_delay[i],
                           settlement_delay = settlement_delay[i],
                           payment_date = reporting_date[i] + c(0, payment_delay),
                           payment_size = c(0, payment_size)) %>%
    filter(payment_date <= settlement_date)
  
  reserving_data <- rbind(reserving_data,
                          claim_data)  
  
}

# COVID shock -- less claims in March, April, May 2020

claims <- reserving_data %>%
  group_by(accident_number) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(keep = runif(n()) > 0.5) 
claims$keep[claims$accident_date < as.Date('2020-03-01') | claims$accident_date >= as.Date('2020-06-01')] <- TRUE

selected_claims <- claims %>%
  filter(keep) %>%
  pull(accident_number)

reserving_data <- reserving_data %>% 
  filter(accident_number %in% selected_claims)

saveRDS(reserving_data, file = 'reserving_data_daily.rds')

# Daily to yearly data

# 1. Add yearly indices
reserving_data_daily<- reserving_data %>%
  mutate(accident_year = as.numeric(format(accident_date, '%Y')) - 2010,
         reporting_year = as.numeric(format(reporting_date, '%Y')) - 2010,
         reporting_delay_year = reporting_year - accident_year,
         payment_year = as.numeric(format(payment_date, '%Y')) - 2010,
         development_year = payment_year - accident_year + 1,
         observation_year = payment_year - reporting_year + 1,
         settlement_year = as.numeric(format(settlement_date, '%Y')) - 2010)

# 2. Group by year
aggregated_data <- reserving_data_daily %>%
  group_by(accident_number, development_year) %>%
  summarise(size = sum(payment_size),
            payment = (size > 0)*1) %>%
  ungroup()

# 3. Add missing years

max_dev_year <- max(reserving_data_daily$accident_year)+1

completed_data <- reserving_data_daily %>% 
  group_by(accident_number) %>%
  slice(1) %>%
  ungroup() %>%
  select(accident_number, accident_year, reporting_year, settlement_year) %>%
  left_join(expand.grid(accident_number = unique(reserving_data_daily$accident_number),
                        development_year = 1:max_dev_year),
            by = 'accident_number') %>%
  mutate(calendar_year = accident_year + development_year - 1) %>%
  left_join(aggregated_data,
            by = c('accident_number', 'development_year')) %>%
  mutate(size = replace_na(size, 0),
         payment = replace_na(payment, 0)) %>%
  mutate(close = (settlement_year == calendar_year)*1)

completed_data <- completed_data %>%
  group_by(accident_number) %>%
  mutate(is_closed = cumsum(close)) %>%
  ungroup() %>%
  filter(!((is_closed == 1) & (close == 0)))

saveRDS(completed_data, file = 'reserving_data_yearly.rds')




                       