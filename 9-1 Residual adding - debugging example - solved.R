library(tidyverse)
library(tidyr)
library(broom)
library(mosaic)
df <- tibble::data_frame(
  x = sort(runif(100)),
  y = 5 * x + 0.5 * x ^ 2 + 3 + rnorm(length(x))
)
plot(df)
m1 <- lm(y ~ x, data = df)
grid <- data.frame(x = seq(0, 1, length = 10))

# could use this table for debugging
# to help us know what the values "should" be
# by using it in place of actual residuals, since it is simpler
# eg r <- resample(fixed_residuals)
fixed_residuals <- tribble (~.resid,
                            1,
                            2)
ag <- augment(m1)
r <- resample(ag, 1)


to_predict <- tribble (~x, 
                    10)

prediction_plus_residual <- predict(m1, newdata=to_predict) + 
  r$.resid


