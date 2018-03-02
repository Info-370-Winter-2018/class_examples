simulate_outcome(ads_to_buy){
  ad = 9 + runif(1,-2,6)
  if (person_buys){
    product_cost = rnorm(1, 5, .2)
    cost = ad + product_cost
    revenue = 20
  } else {
    cost = ad 
    revenue = 0
  }
  
  profit = revenue - cost
  
}


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
