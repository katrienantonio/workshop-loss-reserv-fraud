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

# Add yearly indices
date_to_year <- function(date, base_year) {
  year <- as.numeric(format(date, '%Y')) - base_year
}

reserving_data <- reserving_data %>%
  mutate(accident_year = date_to_year(accident_date, 2010),
         reporting_year = date_to_year(reporting_date, 2010),
         payment_year = date_to_year(payment_date, 2010),
         development_year = payment_year - accident_year + 1,
         settlement_year = date_to_year(settlement_date, 2010))

#### Your turn: exercise 1 ####

# Q1: visualize reporting and settlement delay.

# Q2: when was the last payment registered in the data set?

# Q3: what is the average number of payments per claim?

# Q4: calculate the number of claims per accident year.

#### Reserving data structures - part 1 ####

# records:
max_dev_year <- 10
accidents <- unique(reserving_data$accident_number)

records <- expand.grid(accident_number = accidents,
                       development_year = 1:max_dev_year)

head(records)

# claim data:
claim_data <- reserving_data %>% 
  group_by(accident_number) %>%
  slice(1) %>%
  ungroup() %>%
  select(accident_number, accident_year, 
         accident_date, reporting_year, 
         settlement_year)

head(claim_data)

# payment data:
payment_data <- reserving_data %>%
  group_by(accident_number, development_year) %>%
  summarise(size = sum(payment_size),
            payment = size > 0)

# merge into individual data:
individual_data <- records %>%
  left_join(claim_data,
            by = 'accident_number') %>%
  left_join(payment_data, 
            by = c('accident_number', 'development_year')) %>%
  mutate(size = replace_na(size, 0),
         payment = replace_na(payment, FALSE))

head(individual_data)

# 300 000 = 10 * 30 000 = 10 records per claim, one per development year.
dim(individual_data) 

# censoring:
observed_data <- individual_data %>%
  mutate(calendar_year = accident_year + development_year - 1) %>%
  filter(calendar_year >= reporting_year,
         calendar_year <= 9)

unobserved_data <- individual_data %>%
  mutate(calendar_year = accident_year + development_year - 1) %>%
  filter(calendar_year > 9)

# IBNR and RBNS:
reserve_actual <- sum(unobserved_data$size)
reserve_actual

## The RBNS reserve is much larger than the IBNR reserve
unobserved_data %>%
  mutate(reported = (reporting_year <= 9)) %>%
  group_by(reported) %>%
  summarise(reserve = sum(size))

#### Reserving data structures - part 2 ####

# Incremental triangle:
observed_data %>%
  group_by(accident_year, development_year) %>%
  summarise(value = sum(size)) %>%
  pivot_wider(values_from = value, 
              names_from = development_year, 
              names_prefix = 'DY.')

# More sophisticated function to create incremental triangles:
## rows: aggregation variable for the rows
## columns: aggregation variable for the columns
## variable: variable that will be aggregated in the cells of the triangle
## lower_na: fill the lower triangle with NA's
incremental_triangle <- function(data, 
                                 rows = 'accident_year',
                                 columns = 'development_year',
                                 variable = 'size',
                                 lower_na = TRUE) {
  data_triangle <- data %>%
    group_by(!!sym(rows), !!sym(columns)) %>%
    summarise(value = sum(!!sym(variable))) %>%
    ungroup()
  
  n <- max(data_triangle[, rows])+1
  
  triangle <- matrix(0, nrow = n, ncol = n)
  triangle[cbind(data_triangle[[rows]]+1, data_triangle[[columns]])] <- data_triangle$value
  
  if(lower_na) {
    triangle[row(triangle) + col(triangle) > n+1] <- NA
  }
  
  return(triangle)
}

cumulative_triangle <- function(data, 
                                rows = 'accident_year',
                                columns = 'development_year',
                                variable = 'size',
                                lower_na = TRUE) {
  incremental <- incremental_triangle(data, rows, columns, variable, lower_na)
  
  t(apply(incremental, 1, cumsum))
} 

incremental_triangle(observed_data,
                     variable = 'payment',
                     lower_na = TRUE)

cumulative_triangle(observed_data, 
                    variable = 'payment')

#### Claims reserving with triangles ####

# diy approach to chainladder:
triangle <- cumulative_triangle(observed_data, variable = 'size')
l <- nrow(triangle)

## compute development factors
f <- rep(0, l-1)
for(j in 1:(l-1)) {
  f[j] <- sum(triangle[1:(l-j), j+1]) / sum(triangle[1:(l-j), j])
}
f

## complete the triangle
triangle_completed <- triangle
for(j in 2:l) {
  triangle_completed[l:(l-j+2), j] <- triangle_completed[l:(l-j+2), j-1] * f[j-1]
}
triangle_completed

## cumulative to incremental triangle
cbind(triangle_completed[, 1],
      t(apply(triangle_completed, 1, diff)))

## cum2incr using the {ChainLadder} package
require(ChainLadder)
cum2incr(triangle_completed)

## calculating the reserve estimate
triangle_completed_incr <- cum2incr(triangle_completed)

lower_triangle <- row(triangle_completed_incr) + col(triangle_completed_incr) > l+1
lower_triangle

reserve_cl <- sum(triangle_completed_incr[lower_triangle])

data.frame(reserve_cl = reserve_cl,
           reserve_actual = reserve_actual,
           difference = reserve_cl - reserve_actual,
           relative_difference_pct = (reserve_cl - reserve_actual) / reserve_actual * 100)

# Using the {ChainLadder} package
require(ChainLadder)
triangle <- cumulative_triangle(observed_data, variable = 'size')
MackChainLadder(triangle)

# Using a GLM

triangle <- incremental_triangle(observed_data, 
                                 variable = 'size')
triangle_long <- data.frame(
  occ.year = as.numeric(row(triangle)),
  dev.year = as.numeric(col(triangle)),
  size = as.numeric(triangle))

head(triangle_long)

## fit the GLM
fit <- glm(size ~ factor(occ.year) + factor(dev.year),
           data = triangle_long,
           family = poisson(link = log))

summary(fit)

coef_cl <- coefficients(fit)
plot(coef_cl[2:10], main = 'coefficients accident year')
plot(coef_cl[11:18], main = 'coefficients development year')

## fill the lower triangle
lower_triangle <- triangle_long$occ.year + triangle_long$dev.year > l + 1

triangle_long$size[lower_triangle] <- predict(fit, newdata = triangle_long[lower_triangle, ], type = 'response')

triangle_long %>% 
  pivot_wider(values_from = size, 
              names_from = dev.year, 
              names_prefix = 'DY.')

reserve_glm <- sum(triangle_long$size[lower_triangle])
reserve_glm

#### your turn 2: implementing chain ladder on the Corona data set ####

# Creating the Corona data set
set.seed(1)

keep_claim <- rep(TRUE, nrow(claim_data))

reduction_period <- claim_data$accident_date >= as.Date('2019-03-01') & 
  claim_data$accident_date < as.Date('2019-06-01')

keep_claim[reduction_period] <- runif(sum(reduction_period)) > 0.5

claims <- claim_data$accident_number[keep_claim]

individual_data_covid <- individual_data %>%
  filter(accident_number %in% claims)

observed_data_covid <- individual_data_covid %>%
  mutate(calendar_year = accident_year + development_year - 1) %>%
  filter(calendar_year >= reporting_year,
         calendar_year <= 9)

unobserved_data_covid <- individual_data_covid %>%
  mutate(calendar_year = accident_year + development_year - 1) %>%
  filter(calendar_year > 9)

# Q1: Compute the actual reserve from the COVID-19 data set.

# Q2: Estimate the reserve using the {ChainLadder} package.

# Q3: Compute the difference between the actual and estimated reserve. 
#     Express this difference as a number of standard deviations.

#### When the chain ladder method fails ####

# inspecting a range of triangles to get insights in the underlying dynamics
triangle_open <- incremental_triangle(
  observed_data_covid %>%
    mutate(open = calendar_year <= settlement_year),
  variable = 'open')

triangle_open

triangle_settlement <- incremental_triangle(
  observed_data_covid %>%
    mutate(settlement = calendar_year == settlement_year),
  variable = 'settlement')

triangle_settlement / triangle_open

triangle_payment <- incremental_triangle(
  observed_data_covid,
  variable = 'payment')

triangle_payment / triangle_open

triangle_size <- incremental_triangle(
  observed_data_covid,
  variable = 'size')

triangle_size / triangle_payment

# inspecting evolutions in claim frequency:
claims_covid <- observed_data_covid %>%
  group_by(accident_number) %>%
  slice(1) %>%
  ungroup()

occ_intensity <- claims_covid %>%
  group_by(accident_date) %>%
  summarise(count = n())

require(zoo)
occ_intensity$moving_average <- 
  rollmean(occ_intensity$count, 30, na.pad = TRUE)

ggplot(occ_intensity) +
  theme_bw() +
  geom_point(aes(x = accident_date, y = count)) +
  geom_line(aes(x = accident_date, y = moving_average), 
            size = 1, color = 'blue') +
  ggtitle('Evolution of claim frequency')

# inspecting evolutions in the distribution of claims within an accident year:
require(lubridate)

claims_covid <- claims_covid %>%
  mutate(start_year = floor_date(accident_date, unit = 'year'),
         time = as.numeric(accident_date - start_year) / 366)

ggplot(claims_covid) +
  theme_bw() +
  geom_density(aes(x = time, 
                   group = factor(accident_year), 
                   color = factor(accident_year)))

#### Fixing the chain ladder method ####

observed_data_covid <- observed_data_covid %>%
  mutate(occurrence_month = as.numeric(format(accident_date, '%m')))

observed_data_covidA <- observed_data_covid %>%
  filter(occurrence_month %in% c(3, 4, 5))

observed_data_covidB <- observed_data_covid %>%
  filter(!(occurrence_month %in% c(3, 4, 5)))

#### your turn ####

# Q1: Analyse the stability of the data set covidA or covidB.

# Q2: Compute the chain ladder reserve for group A and B seperately. 
#     Combine these estiamtes to estimate the total reserve.

# Q3: Compare this new estimate with the previous estimate without splitting the data.

# Q4: Express this error as a number of standard deviations