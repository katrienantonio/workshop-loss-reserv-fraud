rm(list = ls())

dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(dir);

require(tidyverse)

set.seed(1)

num_accidents <- 30000

# Accidents between 2010 and 2019
accident_date <- as.Date('2010-01-01') + floor(runif(num_accidents) * as.numeric((as.Date('2020-01-01') - as.Date('2010-01-01'))))

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

save(reserving_data, file = 'reserving_data.RData')