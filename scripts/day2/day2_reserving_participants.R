rm(list = ls())

# Set working directory
dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(dir);

# Load required packages
require(tidyverse)

# Load the data 
reserving_daily <- readRDS("data/reserving_data_daily.rds")
reserving_yearly <- readRDS("data/reserving_data_yearly.rds")

# Inspect the data
head(reserving_daily)

# Yearly indicators are defined as the number of elapsed years since 2010
# Settlement year == 3 implies the claim settled in 2013 (=2010 + 3)
# Development year is defined as the number of years elapsed since the occurrence of the claim
# Development year 1 refers to the year in which the claim occurred
head(reserving_yearly)


#### Your turn: exercise 1 ####

# Q1: visualize reporting and settlement delay.

ggplot(data = reserving_daily) +
  theme_bw() +
  geom_density(aes(___, fill = 'reporting delay'),
               alpha = .5) +
  geom_density(aes(___, fill = 'settlement_delay'),
               alpha = .5) +
  xlab('delay in days') +
  xlim(c(0, 1000))

# Q2: when was the last payment registered in the data set?

max(reserving_daily$___)


# Q3: what is the average number of payments per claim?

reserving_daily %>%
  group_by(accident_number) %>%
  summarise(payments = sum(___)) %>%
  ungroup() %>%
  summarise(average = mean(payments))


# Q4: calculate the number of claims per accident year.

reserving_yearly %>%
  filter(development_year == 1) %>%
  group_by(accident_year) %>%
  summarise(num_claims = ___)

# censoring:
observed_daily <- reserving_daily %>%
  filter(payment_date <= as.Date('2020-12-31'))
unobserved_daily <- reserving_daily %>%
  filter(payment_date > as.Date('2020-12-31'))
observed_yearly <- reserving_yearly %>%
  filter(calendar_year <= 10,
         reporting_year <= 10)
unobserved_yearly <- reserving_yearly %>% 
  filter(calendar_year > 10 | reporting_year > 10)

# IBNR and RBNS:
reserve_actual <- sum(unobserved_yearly$size)
reserve_actual

# same result from daily data
sum(unobserved_daily$payment_size)

## The RBNS reserve is much larger than the IBNR reserve
unobserved_yearly %>%
  mutate(reported = (reporting_year <= 10)) %>%
  group_by(reported) %>%
  summarise(reserve = sum(size))

#### Reserving data structures - part 2 ####

# Incremental triangle:
observed_yearly %>%
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

incremental_triangle(observed_yearly,
                     variable = 'payment',
                     lower_na = TRUE)

cumulative_triangle(observed_yearly, 
                    variable = 'payment')

#### Claims reserving with triangles ####

# diy approach to chainladder:
triangle <- cumulative_triangle(observed_yearly, variable = 'size')
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
triangle <- cumulative_triangle(observed_yearly, variable = 'size')
MackChainLadder(triangle)

# Using a GLM

triangle <- incremental_triangle(observed_yearly, 
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

#### your turn 2: estimating the number of future payments ####

# Q1: Compute the actual number of future payments from the unobserved data set.



# Q2: Create a cumulative triangle containing the number of payments per accident and development year.

triangle <- cumulative_triangle(observed_yearly, 
                                variable = ___)

# Q3: Estimate the future number of payments using the chain ladder method from the {ChainLadder} package.

require(ChainLadder)
cl <- ___(triangle)
cl

# Q4: Compute the difference between the estimated and actual number of payments. 
#     Express this error in terms of standard deviations?

ultimate <- sum(cum2incr(cl$FullTriangle))
already_paid <- sum(cum2incr(cl$Triangle), na.rm = TRUE)
payment_cl <-  ___
sigma_cl <- as.numeric(cl$Total.Mack.S.E)
error = ___
round(c(error = error, 
        pct_error = error / payment_actual * 100,
        std.dev = error / sigma_cl),2)


#### When the chain ladder method fails ####

# inspecting a range of triangles to get insights in the underlying dynamics
triangle_open <- incremental_triangle(
  observed_yearly %>%
    mutate(open = calendar_year <= settlement_year),
  variable = 'open')
triangle_open

triangle_open_end <- incremental_triangle(
  observed_yearly %>%
    mutate(open_end = (calendar_year < settlement_year)),
  variable = 'open_end')
triangle_open_end

triangle_payment <- incremental_triangle(
  observed_yearly,
  variable = 'payment')
triangle_payment / triangle_open

triangle_size <- incremental_triangle(
  observed_yearly,
  variable = 'size')
triangle_size / triangle_payment

# inspecting evolutions in claim frequency:
claims<- observed_daily %>%
  group_by(accident_number) %>%
  slice(1) %>%
  ungroup()
occ_intensity <- claims %>%
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
claims <- claims %>%
  mutate(start_year = floor_date(accident_date, unit = 'year'),
         time = as.numeric(accident_date - start_year) / 366,
         accident_year = year(accident_date),
         reporting_year = year(reporting_date)) %>%
  filter(accident_year == reporting_year)
ggplot(claims) +
  theme_bw() +
  geom_density(aes(x = time, 
                   group = factor(accident_year), 
                   color = factor(accident_year)))

#### Fixing the chain ladder method ####

## Monthly chain ladder

require(lubridate)
claims <- observed_daily %>%
  group_by(accident_number) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(start_month = floor_date(accident_date, unit = 'month'),
         time = as.numeric(accident_date - start_month) / 31,
         accident_month = format(accident_date, '%Y%m'),
         reporting_month = format(reporting_date, '%Y%m')) %>%
  filter(accident_month == reporting_month)
ggplot(claims) +
  theme_bw() +
  geom_density(aes(x = time, 
                   group = factor(accident_month), 
                   color = factor(accident_month))) +
  theme(legend.position = 'none')

# Constructing a monthly triangle
triangle_month <- observed_daily %>%
  mutate(accident_month = year(accident_date)*12 + month(accident_date) - 2010*12,
         development_month = year(payment_date)*12 + month(payment_date) - 2010*12 - accident_month) %>%
  group_by(accident_month, development_month) %>%
  summarise(size = sum(payment_size)) %>%
  ungroup() %>%
  complete(expand.grid(accident_month = 1:132, development_month = 0:131), fill = list(size = 0)) %>%
  mutate(size = ifelse(accident_month + development_month > 132, NA, size)) %>%
  arrange(development_month) %>%
  pivot_wider(names_from = development_month, values_from = size) %>%
  arrange(accident_month)
triangle_month <- as.matrix(triangle_month[, 2:132])

cl <- MackChainLadder(incr2cum(triangle_month))
summary(cl)$Totals

## Chainladder by occurrence month

require(lubridate)
claims <- observed_daily %>%
  group_by(accident_number) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(start_month = floor_date(accident_date, unit = 'month'),
         time = as.numeric(accident_date - start_month) / 31,
         accident_year = format(accident_date, '%Y'),
         reporting_year = format(reporting_date, '%Y'),
         month = format(accident_date, '%B')) %>%
  filter(accident_year == reporting_year)
ggplot(claims) +
  facet_wrap( ~ month, ncol = 3) +
  theme_bw() +
  geom_density(aes(x = time, 
                   group = factor(accident_year), 
                   color = factor(accident_year)))

# Add accident date to reserving_yearly
reserving_yearly <- reserving_yearly %>%
  left_join(reserving_daily %>% 
              group_by(accident_number) %>%
              slice(1) %>%
              ungroup() %>%
              select(accident_number, accident_date))
reserving_yearly <- reserving_yearly %>%
  mutate(accident_month = format(accident_date, '%B'))

# Compute data for runoff triangles by month
triangles <- reserving_yearly %>%
  group_by(accident_month, accident_year, development_year) %>%
  summarise(size = sum(size)) %>%
  ungroup() %>%
  complete(expand.grid(accident_month = unique(accident_month), 
                       accident_year = 0:10, development_year = 1:11),
           fill = list(size = 0)) %>%
  mutate(size = ifelse(accident_year + development_year > 11, NA, size))

triangles %>%
  filter(accident_month == 'April') %>%
  arrange(development_year) %>%
  pivot_wider(names_from = development_year, values_from = size)

# Estimate chain ladder glm
fit <- glm(size ~ factor(development_year) * accident_month  + factor(accident_year) * accident_month,
           data = triangles,
           family = poisson(link = 'log'))

# compute reserve
reserve_group <- sum(predict(fit, newdata = triangles %>% filter(is.na(size)), type = 'response'))
reserve_group