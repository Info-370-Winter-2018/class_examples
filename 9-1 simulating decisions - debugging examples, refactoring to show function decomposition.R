profit_monte_carlo_given_choice_debug <- function(ads_to_buy){
  # the price of ads fluctuates with the market
  # we model these price fluctuations with a random value
  browser()
  output <- tribble( ~ads_to_buy, ~ad_cost, ~number_of_ads, 
                     ~expected_sales_per_ad, ~profit)
  output <- output %>% add_row(ads_to_buy=ads_to_buy)
  output <- output %>%  mutate(
    ad_cost = 9 + runif(1, -2, 6),
    number_of_ads = ads_to_buy,
    expected_sales_per_ad = 12*number_of_ads,
    profit = expected_sales_per_ad - (ad_cost*number_of_ads)
  )
  
  # residual_term <- resample(1, your_models_residuals)
  # expected_sales_per_ad <- predict(model, 
  #             newdata=(tibble for this one simulation)) + residual_term
  
  return(output)
}

profit_monte_carlo_given_choice_debug(0)
profit_monte_carlo_given_choice_debug(1)


# you can refactor as a part of debugging;
# here we isolated one value
random_ad_cost_fun <- function(mean){
  mean + runif(1, -2, 6)
}

profit_monte_carlo_given_choice <- function(ads_to_buy){
  # the price of ads fluctuates with the market
  # we model these price fluctuations with a random value
  #browser()
  
  ad_cost <- random_ad_cost_fun(mean=9)
  # original ad_cost line
  # ad_cost <- 9 + runif(1, -2, 6)
  
  number_of_ads <- ads_to_buy
  
  expected_sales_per_ad <- 12*number_of_ads
  # residual_term <- resample(1, your_models_residuals)
  # expected_sales_per_ad <- predict(model, 
  #             newdata=(tibble for this one simulation)) + residual_term
  
  profit <- expected_sales_per_ad - (ad_cost*number_of_ads)
  return(profit)
}

#refactor to see that the profit is a factor of the sales
# this makes function composition more explicit
profit_monte_carlo_given_choice <- function(ads_to_buy){
  # the price of ads fluctuates with the market
  # we model these price fluctuations with a random value
  ad_cost <- 9 + runif(1, -2, 6)
  number_of_ads <- ads_to_buy
  # residual_term <- resample(1, your_models_residuals)
  # expected_sales_per_ad <- predict(model,
  #             newdata=(tibble for this one simulation)) + residual_term
  
  profit <- profit(sales(ads_to_buy), ad_cost*number_of_ads)
  return(profit)
}

sales <- function(ads_buy) {
  return(12 * ads_buy)
}

profit <- function(revenue, cost){
  revenue - cost
}

profit_monte_carlo_given_choice_buggy <- function(ads_to_buy){
  # the price of ads fluctuates with the market
  # we model these price fluctuations with a random value
  #browser()
  ad_cost <- 9 + runif(2, -2, 6)
  number_of_ads <- ads_to_buy
  expected_sales_per_ad <- 12*number_of_ads
  # residual_term <- resample(1, your_models_residuals)
  # expected_sales_per_ad <- predict(model, 
  #             newdata=(tibble for this one simulation)) + residual_term
  
  profit <- expected_sales_per_ad - (ad_cost*number_of_ads)
  return(profit)
}

profit_monte_carlo_given_choice_buggy(1)



results <- tribble (~choice, ~mean_profit)

for (ads_to_buy in seq(from=0, to=20, by=1)){
  
  simulate_1000_times <- do(1000)*profit_monte_carlo_given_choice(ads_to_buy)
  results <- add_row(results, 
          choice=ads_to_buy,
          mean_profit=mean(simulate_1000_times$profit_monte_carlo_given_choice)
          )
}

plot(results)


results <- tribble (~choice, ~profit, ~mean_profit)

for (ads_to_buy in seq(from=0, to=20, by=1)){
  simulate_1000_times <- do(1000)*profit_monte_carlo_given_choice(ads_to_buy)
  results <- add_row(results, 
                     choice=ads_to_buy,
                     profit=simulate_1000_times$profit_monte_carlo_given_choice,
                     mean_profit=mean(simulate_1000_times$profit_monte_carlo_given_choice)
  )
}

ggplot(data = results, aes(x=choice, y=profit, group=choice)) + geom_boxplot() + ylim(-100,100)




# use sales model fit on your simulated data
profit_monte_carlo_given_choice <- function(ads_to_buy){
  # the price of ads fluctuates with the market
  # we model these price fluctuations with a random value
  ad_cost <- 9 + runif(1, -2, 6)
  number_of_ads <- ads_to_buy
  expected_sales_per_ad <- 12*number_of_ads
  # residual_term <- resample(1, your_models_residuals)
  # expected_sales_per_ad <- predict(model, 
  #             newdata=(tibble for this one simulation)) + residual_term
  
  profit <- expected_sales_per_ad - (ad_cost*number_of_ads)
  return(profit)
}