library(tidyverse)
library(readr)

#https://www.census.gov/prod/cen2010/briefs/c2010br-08.pdf  

#https://www.brookings.edu/research/dividing-the-house-why-congress-should-reinstate-an-old-reapportionment-formula/

#https://isr.umich.edu/apportionment-calculator-for-us-census/

hist_pop <- "data/hist_pop_revised.csv"
df <- read_csv(hist_pop) %>% select(stcd, population =`2020`)

pull_maximum_priority <- function(df) {
  maxvalue <- df %>% 
    top_n(1, priority)
  return(maxvalue)
}

popsum_adj <- sum(df$population) - 1e6

webster_calc <- function(df, popsum = popsum, SEATS = 435) {
  df <- df %>%
    mutate(quota = population/popsum * SEATS,
           residual = quota - floor(quota),
           seats_to_allocate = case_when(
             quota <= .5 ~ 1,
             residual >= .5 ~ floor(quota) + 1,
             residual < .5 ~ floor(quota)
           )
    )
  return(df)
}

webster <- function(df, ADJUST = TRUE, TEST_CHANGE = 0) {
  
  df1 <- webster_calc(df, popsum = popsum_adj, SEATS = 435) 
  seat_sum <- sum(df1$seats_to_allocate)
  
    while(seat_sum < 435){
      popsum_adj <- popsum_adj - 1000
      df1 <- webster_calc(df1, popsum = popsum_adj)
      seat_sum <- sum(df1$seats_to_allocate)
    }
    
    while(seat_sum > 435){
      popsum_adj <- popsum_adj + 1000
      df1 <- webster_calc(df1, popsum = popsum_adj)
      seat_sum <- sum(df1$seats_to_allocate)
    }

  
  return(df1)
}    






