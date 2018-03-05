
runningMedian <- function(x, length=3){   # bugged!
  #   x: a numeric vector
  #   length: the number of values for each running median, defaults to 3
  n <- length(x)
  X <- matrix(x, n, length)
  for (i in 1:length) X[1:(n - i + 1), i] <- x[-(1:(i - 1))]
  apply(X, 1, median)[1:(n - length + 1)]
}

runningMedian(c(3,4,5,6,7,8))