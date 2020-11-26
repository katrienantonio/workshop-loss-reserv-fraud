rm(list = ls())

# Set working directory
dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(dir);

# Load required packages
require(tidyverse)

# Load the data 
load("data/reserving_data.RData")

# Inspect the data
head(reserving_data)

date_to_year <- function(date) {
  year <- as.numeric(format(date, '%Y'))
  
  return(year)
}

date_to_quarter <- function(date) {
  month <- as.numeric(format(date, '%m'))
  quarter <- floor((month-1)/3)
  
  return(quarter)
}

date_to_period <- function(date, base) {
  date_to_year(date) + date_to_quarter(date)/4 - base
}

reserving_data <- reserving_data %>%
  mutate(accident_period = date_to_period(accident_date, 2010),
         reporting_period = date_to_period(reporting_date, 2010),
         payment_period = date_to_period(payment_date, 2010),
         payment_period = date_to_period(payment_date, 2010),
         settlement_period = date_to_period(settlement_date, 2010)) %>% 
  mutate(development_period = payment_period - accident_period) 

# Calculate the reserve in the second quarter of 2019
# --> remove claims that occur after June 2019
reserving_data <- reserving_data %>%
  filter(accident_date < as.Date('2019-07-01'))

head(reserving_data)

# records:
max_period <- 9.25
accidents <- unique(reserving_data$accident_number)

records <- expand.grid(accident_number = accidents,
                       development_period = seq(from = 0, to = max_period, by = .25))

# claim data:
claim_data <- reserving_data %>% 
  group_by(accident_number) %>%
  slice(1) %>%
  ungroup() %>%
  select(accident_number, accident_period, 
         accident_date, reporting_period, 
         settlement_period, accident_period)

head(claim_data)

# payment data:
payment_data <- reserving_data %>%
  group_by(accident_number, development_period) %>%
  summarise(size = sum(payment_size),
            payment = size > 0)

# merge into individual data:
individual_data <- records %>%
  left_join(claim_data,
            by = 'accident_number') %>%
  left_join(payment_data, 
            by = c('accident_number', 'development_period')) %>%
  mutate(size = replace_na(size, 0),
         payment = replace_na(payment, FALSE)) %>%
  mutate(calendar_period = accident_period + development_period)

# censoring:
observed_data <- individual_data %>%
  filter(calendar_period >= reporting_period,
         calendar_period <= 9.25)

unobserved_data <- individual_data %>%
  filter(calendar_period > 9.25)

# IBNR and RBNS:
reserve_actual <- sum(unobserved_data$size)
reserve_actual

## The RBNS reserve is much larger than the IBNR reserve
unobserved_data %>%
  mutate(reported = (reporting_period <= 9.5)) %>%
  group_by(reported) %>%
  summarise(reserve = sum(size))

#### Reserving data structures - part 2 ####

# More sophisticated function to create incremental triangles:
## rows: aggregation variable for the rows
## columns: aggregation variable for the columns
## variable: variable that will be aggregated in the cells of the triangle
## lower_na: fill the lower triangle with NA's
incremental_triangle <- function(data, 
                                 rows = 'accident_period',
                                 columns = 'development_period',
                                 variable = 'size',
                                 lower_na = TRUE,
                                 step = .25) {
  
  data_triangle <- data %>%
    group_by(!!sym(rows), !!sym(columns)) %>%
    summarise(value = sum(!!sym(variable))) %>%
    ungroup()
  
  n <- max(data_triangle[, rows]) / step + 1
  
  triangle <- matrix(0, nrow = n, ncol = n)
  triangle[cbind(data_triangle[[rows]]/step+1, data_triangle[[columns]]/step+1)] <- data_triangle$value
  
  if(lower_na) {
    triangle[row(triangle) + col(triangle) > n+1] <- NA
  }
  
  return(triangle)
}

cumulative_triangle <- function(data, 
                                rows = 'accident_period',
                                columns = 'development_period',
                                variable = 'size',
                                lower_na = TRUE,
                                step = .25) {
  incremental <- incremental_triangle(data, rows, columns, variable, lower_na, step)
  
  t(apply(incremental, 1, cumsum))
} 

tri <- incremental_triangle(observed_data,
                     variable = 'payment',
                     lower_na = TRUE)

view(tri)

cumulative_triangle(observed_data, 
                    variable = 'payment')

#### Claims reserving with {ChainLadder} ####

require(ChainLadder)
triangle <- cumulative_triangle(observed_data, variable = 'size')
cl <- MackChainLadder(triangle)
cl
cl$f

ultimate <- sum(cum2incr(cl$FullTriangle))
already_paid <- sum(cum2incr(cl$Triangle), na.rm = TRUE)
reserve_cl <-  ultimate - already_paid
reserve_actual <- sum(unobserved_data_seasonal$size)

data.frame(reserve_actual, reserve_cl, pct_error = (reserve_cl / reserve_actual - 1) * 100)