install.packages("poisson")
library(poisson)

install.packages("ggplot2")
library(ggplot2)

install.packages("dplyr")
library(dplyr)


## Question 3
### 3.1 Submitted hand written in the Report

### 3.2: Use R to confirm the result of Pr(X=5) for the die-roll example

P = 1/6
Success = 5
Trails = 8 

dbinom(Success, Trails, P) ## 0.0042

## 3.3 Plot the corresponding full probability mass function for X for this die-rolling example.
## (Hint: because of the discrete nature of X, it is easy to use the barplot function for this).

PrbDist <- data_frame(value = c(1, 2, 3, 4, 5, 6, 7, 8),
                      prob = c(1/6, 1/6, 1/6, 1/6, 1/6, 5/6, 5/6, 5/6))

PrbDist %>%   ggplot(aes(x = value, y = prob)) +
  geom_bar(stat = "identity") + 
  ylim(c(0, 1)) +  labs(title = "Probability mass function for Dice Distribution",
                        x = "X (number of times Dice is rolled)", y = "f(X) Probability") + 
  theme_minimal(base_size=9)


# Given lambda = mean = 3.22
## 4.1 Use R to find the probability you observe strictly 
## less than three defects
ppois(2, 3.22, lower.tail = TRUE, log.p = FALSE) # 0.3757454

# or 

ppois(3, 3.22, lower.tail = TRUE, log.p = FALSE) - 
  dpois(x = 3, lambda = 3.22)


## 4.2 Use R to find the probability you observe strictly 
## more than five defects. (x > 5)
ppois(6, 3.22, lower.tail = FALSE)


?options
options(scipen = 999, digits = 2) # sig digits


samp <- 0:10
dens <- dpois(x = samp, lambda = 3.22)
prob <- ppois(q = samp, lambda = 3.22, lower.tail = TRUE)
data <- data.frame(samp, dens, prob)

ggplot(data, aes(x = factor(samp), y = dens)) +
  geom_col() +
  geom_text(aes(label = round(dens,2), y = dens + 0.01), position = position_dodge(0.9), size = 3,vjust = 0) +
  labs(title = "Probability mass function and cumulative distribution function of Poisson Distribution",
       x = "samp (x)", y = "density") + geom_line(data = data, aes(x = samp, y = prob)) + theme_minimal(base_size=9)





