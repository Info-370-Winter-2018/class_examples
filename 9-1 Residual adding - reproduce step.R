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



ag <- augment(m1)
r <- resample(1, ag)

predition_plus_residual <- predict(m1, newdata=c(10)) + r
