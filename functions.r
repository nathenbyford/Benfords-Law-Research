library(tidyverse)

## significand function

significand <- function(x) {
  s = 10 ^ (log10(abs(x)) - floor(log10(abs(x))))
  return(round(s, 4))
}

## My attempt to make a function for extracting the first digit 

getFirstDigit <- function(x) {
  fd = floor((10 ^ (log10(abs(x))%%1)))
  return(fd)
}

## function from benford.analysis package for extracting digits

extract.digits <- function(data, number.of.digits = 1, sign="positive", second.order = FALSE, discrete = TRUE, round = 3) {
  
  if (!is.numeric(data)) stop("Data must be a numeric vector")
  
  ## cleaning data for analysis - only > 0 and either only positive or only negative
  if (sign == "positive")  positives <- data[data > 0 & !is.na(data)]
  if (sign == "negative")  positives <- data[data < 0 & !is.na(data)]*(-1)
  if (sign == "both")      positives <- abs(data[data != 0 & !is.na(data)]) 
  
  if (second.order) {
    
    if (number.of.digits > 4) {
      warning("There might be some floating point precision issues on the Second Order distribution")
    }
    
    n <- length(positives)
    first <- sort(positives)[1:(n - 1)]
    second <- sort(positives)[2:n]
    
    positives <-  if (discrete) {
      round(second - first, number.of.digits + round)
    } else {
      round(second - first, 8)
    } 
    
    positives <- positives[positives > 0]
  }
  
  results <- trunc((10^((floor(log10(positives))*-1) + 
                          number.of.digits - 1))*positives)
  return(results)
}
### Neither of the first digits functions work with the significand


## function that returns a data frame with digit 1:9 and the proportion of the first digit vector of each

proportion_df <- function(v) {
  p_1 = length(which(v == 1)) / length(v)
  p_2 = length(which(v == 2)) / length(v)
  p_3 = length(which(v == 3)) / length(v)
  p_4 = length(which(v == 4)) / length(v)
  p_5 = length(which(v == 5)) / length(v)
  p_6 = length(which(v == 6)) / length(v)
  p_7 = length(which(v == 7)) / length(v)
  p_8 = length(which(v == 8)) / length(v)
  p_9 = length(which(v == 9)) / length(v)
  digit = 1:9
  prop = c(p_1, p_2, p_3, p_4, p_5, p_6, p_7, p_8, p_9)
  df = cbind(digit, prop)
  return(df)
}


## Function that will plot a histogram with the proportions from the proportions data frame

prop_graph <- function(prop_df, title = NULL) {
  if (is.null(title) == TRUE) {
    prop_df %>% as_tibble() %>% ggplot(aes(x = digit, y = prop)) +
      geom_col(fill = "skyblue3") + 
      geom_point(aes(y = benford_prop), color = "red", size = 4, shape = 3) +
      geom_point(aes(y = benford_prop), color = "red", size = 3, shape = 1) +
      geom_line(aes(y = benford_prop), color = "red") +
      scale_x_continuous(breaks = 1:9)+ 
      labs(x = "Digit", y = "Proportion") +
      theme_classic()
  } else {
    prop_df %>% as_tibble() %>% ggplot(aes(x = digit, y = prop)) +
      geom_col(fill = "skyblue3") + 
      geom_point(aes(y = benford_prop), color = "red", size = 4, shape = 3) +
      geom_point(aes(y = benford_prop), color = "red", size = 3, shape = 1) +
      geom_line(aes(y = benford_prop), color = "red") +
      scale_x_continuous(breaks = 1:9) + 
      labs(main = title, x = "Digit", y = "Proportion")
  }
}


####################### Testing Functions ################################

Zd <- function(x) {
    k = floor(log10(x)) + 1
    z = 10^(k - 1) * significand(x) * 
      (0 + (x <= 10^(k - 1) * significand(x) & 10^(k - 1) * significand(x) <= x+1))
    return(z)
}


Z_bar <- function(data, digit = NULL) {
  if (is.null(digit) == 1) {
    zbar = c()
    for (i in 1:9) {
      n = length(x[which(getFirstDigit(data) == i)])
      zbar[i] = 1/n * sum(Zd(data[which(getFirstDigit(data) == i)]))
    }
    return(zbar)
  }
  n = length(x[which(getFirstDigit(data) == digit)])
  zbar = 1/n * sum(Zd(data[which(getFirstDigit(data) == digit)]))
  return(zbar)
}


Td <- function(x) {
  C = log10(exp(1))
  t = c()
  for (i in 1:9) {
    n = length(x[which(getFirstDigit(x) == i)])
    t[i] = (Z_bar(x, i) - C) / sqrt((C * (i + .5 - C)) / n)
  }
  digit = 1:9
  return(rbind(digit, t))
}


## sup-norm Test

sup.norm <- function(dat) {
  m = which(Td(dat) == max(Td(dat)))
  return(m - 1)
}

## Figure out how the P-value for this test works so we can have the P-value shown and used as well


### Chi-square goodness of fit test

benford_chi <- function(data, alpha = 0.05) {
  obs = proportion_df(getFirstDigit(data))[ ,2]
  expected = c(.301, .176, .124, .096, .079, .066, .057, .051, .045)
  # get test statistic
  chi_square = sum((obs - expected) / expected)
  # find p-value
  pval = pchisq(chi_square, 9)
  # rejection region
  rr = c(qchisq(alpha/2, 9), qchisq(1 - (alpha/2), 9))
  
  return(chi_square)
}


## Min P-Value Test


