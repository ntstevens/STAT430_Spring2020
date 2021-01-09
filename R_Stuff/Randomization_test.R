## A function and example of the randomization test
rand_test <- function(x, y, MOI, alternative, N, p){
  n1 <- length(x)
  n2 <- length(y)
  if(MOI == "mean"){
    t <- mean(x) - mean(y)
  }else if(MOI == "median"){
    t <- median(x) - median(y)
  }else if(MOI == "quant"){
    t <- quantile(x, probs = p) - quantile(y, probs = p)
  }
  comb <- c(x, y)
  t_star <- rep(0, N)
  for(i in 1:N){
    s <- sample(x = comb, size = length(comb), replace = FALSE)
    x_star <- s[1:n1]
    y_star <- s[(n1+1):length(comb)]
    if(MOI == "mean"){
      t_star[i] <- mean(x_star) - mean(y_star)  
    }else if(MOI == "median"){
      t_star[i] <- median(x_star) - median(y_star)  
    }else if(MOI == "quant"){
      t_star[i] <- quantile(x_star, probs = p) - quantile(y_star, probs = p)  
    }
  }
  par(mfrow = c(1,1))
  h = hist(t_star)
  dev.off()
  if(alternative == "two.sided"){
    p_value = sum(t_star >= abs(t))/N + sum(t_star <= -abs(t))/N
    cols = ifelse(h$mids <= -abs(t) | h$mids >= abs(t), "deepskyblue", "white")
    plot(h, col = cols, border = "navyblue", main = "Null Distribution of t", xlab = "t")
    abline(v = -t, col = "red", lwd = 2, lty = 2)
  }else if(alternative == "less"){
    p_value = sum(t_star <= t)/N
    cols = ifelse(h$mids <= t, "deepskyblue", "white")
    plot(h, col = cols, border = "navyblue", main = "Null Distribution of t", xlab = "t")
  }else if(alternative == "greater"){
    p_value = sum(t_star >= t)/N
    cols = ifelse(h$mids >= t, "deepskyblue", "white")
    plot(h, col = cols, border = "navyblue", main = "Null Distribution of t", xlab = "t")
  }
  abline(v = t, col = "red", lwd = 2, lty = 2)
  print(paste("The p-value is ", p_value, ".", sep = ""))
  # return(p_value)
}

## Pokémon Go example:
## Read in the data
setwd("/Users/nstevens/Dropbox/Teaching/STAT_430/Spring_2020/R Stuff/")
pokemon <- read.csv(file = "pokemongo.csv", header = T)

## Look at the data
par(mfrow = c(3,1))
hist(pokemon$control, xlim = c(0,50), breaks = seq(0,50,5), xlab = "Dollars Spent", main = "Control Condition")
abline(v = c(mean(pokemon$control), median(pokemon$control)), col = c("red", "darkgreen"), lwd = 2)
legend("topright", legend = c("Mean", "Median"), lty = 1, lwd = 2, col = c("red", "darkgreen"), bty = "n")
hist(pokemon$freecoins, xlim = c(0,50), breaks = seq(0,50,5), xlab = "Dollars Spent", main = "Free Coin Condition")
abline(v = c(mean(pokemon$freecoins), median(pokemon$freecoins)), col = c("red", "darkgreen"), lwd = 2)
hist(pokemon$discount, xlim = c(0,50), breaks = seq(0,50,5), xlab = "Dollars Spent", main = "Discount Condition")
abline(v = c(mean(pokemon$discount), median(pokemon$discount)), col = c("red", "darkgreen"), lwd = 2)

############## Where the test statistic is the difference of means ##############
## Compare Conditions 1 and 2 
rand_test(x = pokemon$control, y = pokemon$freecoins, MOI = "mean", alternative = "two.sided", N = 10000)

## Compare Conditions 1 and 3 (where the test statistic is the difference of means)
rand_test(x = pokemon$control, y = pokemon$discount, MOI = "mean", alternative = "two.sided", N = 10000)
rand_test(x = pokemon$control, y = pokemon$discount, MOI = "mean", alternative = "less", N = 10000)

## Compare Conditions 2 and 3 (where the test statistic is the difference of means)
rand_test(x = pokemon$freecoins, y = pokemon$discount, MOI = "mean", alternative = "two.sided", N = 10000)
rand_test(x = pokemon$freecoins, y = pokemon$discount, MOI = "mean", alternative = "less", N = 10000)

############## Where the test statistic is the difference of medians ##############
## Compare Conditions 1 and 2 (where the test statistic is the difference of medians)
rand_test(x = pokemon$control, y = pokemon$freecoins, MOI = "median", alternative = "two.sided", N = 10000)

## Compare Conditions 1 and 3 (where the test statistic is the difference of medians)
rand_test(x = pokemon$control, y = pokemon$discount, MOI = "median", alternative = "two.sided", N = 10000)

## Compare Conditions 2 and 3 (where the test statistic is the difference of medians)
rand_test(x = pokemon$freecoins, y = pokemon$discount, MOI = "median", alternative = "two.sided", N = 10000)


