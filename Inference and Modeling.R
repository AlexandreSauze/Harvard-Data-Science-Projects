# INFERENCE AND MODELING




# Section 1: Parameters and Estimates


# Sampling Model Parameters and Estimates
library(tidyverse)
library(dslabs)
take_poll(25)    # draw 25 beads




# Section 2: The Central Limit Theorem in Practice


# The Central Limit Theorem in Practice

X_hat <- 0.48
se <- sqrt(X_hat*(1-X_hat)/25)
pnorm(0.01/se) - pnorm(-0.01/se)


# A Monte Carlo Simulation for the CLT

#Code: Monte Carlo simulation using a set value of p
p <- 0.45    # unknown p to estimate
N <- 1000
# simulate one poll of size N and determine x_hat
x <- sample(c(0,1), size = N, replace = TRUE, prob = c(1-p, p))
x_hat <- mean(x)
# simulate B polls of size N and determine average x_hat
B <- 10000    # number of replicates
N <- 1000    # sample size per replicate
x_hat <- replicate(B, {
  x <- sample(c(0,1), size = N, replace = TRUE, prob = c(1-p, p))
  mean(x)
})

# Code: Histogram and QQ-plot of Monte Carlo results
library(tidyverse)
library(gridExtra)
p1 <- data.frame(x_hat = x_hat) %>%
  ggplot(aes(x_hat)) +
  geom_histogram(binwidth = 0.005, color = "black")
p2 <- data.frame(x_hat = x_hat) %>%
  ggplot(aes(sample = x_hat)) +
  stat_qq(dparams = list(mean = mean(x_hat), sd = sd(x_hat))) +
  geom_abline() +
  ylab("X_hat") +
  xlab("Theoretical normal")
grid.arrange(p1, p2, nrow=1)


# Bias: Why Not Run a Very Large Poll?

library(tidyverse)
N <- 100000
p <- seq(0.35, 0.65, length = 100)
SE <- sapply(p, function(x) 2*sqrt(x*(1-x)/N))
data.frame(p = p, SE = SE) %>%
  ggplot(aes(p, SE)) +
  geom_line()




# Section 3: Confidence Intervals and p-Values


# Confidence Intervals

#Code: geom_smooth confidence interval example
#The shaded area around the curve is related to the concept of confidence intervals.
data("nhtemp")
data.frame(year = as.numeric(time(nhtemp)),temperature = as.numeric(nhtemp)) %>%
  ggplot(aes(year, temperature)) +
  geom_point() +
  geom_smooth() +
  ggtitle("Average Yearly Temperatures in New Haven")

#Code: Monte Carlo simulation of confidence intervals
#Note that to compute the exact 95% confidence interval, we would use qnorm(.975)*SE_hat instead of 2*SE_hat.
p <- 0.45
N <- 1000
X <- sample(c(0,1),size = N, replace= TRUE, prob = c(1-p,p))
X_hat <- mean(X)
SE_hat <- sqrt(X_hat*(1-X_hat)/N)
c(X_hat - 2*SE_hat, X_hat + 2*SE_hat)

#Code: Solving for z with qnorm
z <- qnorm(0.995) # we need q=99% confidence so, z=qnorm(1-(1-q)/2)
z # for 99% confidence int. we need z of 2.576
pnorm(qnorm(0.995)) # demostration that qnorm gives the zvalue for a given prob
pnorm(qnorm(1-0.995))
pnorm(z) - pnorm(-z)


# A Monte Carlo Simulation for Confidence Intervals

B <- 10000
N <- 1000
inside <- replicate(B, {
  X <- sample(c(0,1), size = N, replace = TRUE, prob = c(1-p,p))
  X_hat <- mean(X)
  SE_hat <- sqrt(X_hat*(1-X_hat)/N)
  between(p,X_hat - 2*SE_hat, X_hat + 2*SE_hat)
})
mean(inside)


# P-values

N <- 100    # sample size
z <- sqrt(N) * 0.02/0.5    # spread of 0.02
1 - (pnorm(z) - pnorm(-z))



# Section 4: Statistical Models


# Poll Aggregators

#Code: Simulating polls
d <- 0.039  #Actual spread of popular vote 2012
Ns <- c(1298, 533, 1342, 897, 774, 254, 812, 324, 1291, 1056, 2172, 516) # Mimic 
#of poll sizes
p <- (d+1)/2  #Proportion of democrats (Obama)
# Calculate confidence intervals of the spread
confidence_intervals <- sapply(Ns,function(N){
  X <- sample(c(0,1), size = N, replace = TRUE, prob = c(1-p,p))
  X_hat <- mean(X)
  SE_hat <- sqrt(X_hat*(1-X_hat)/N)
  2*c(X_hat, X_hat - qnorm(.975)*SE_hat, X_hat + qnorm(.975)*SE_hat) - 1
})
# generate a data frame storing results
polls <- data.frame(poll = 1:ncol(confidence_intervals),
                    t(confidence_intervals), sample_size = Ns)
names(polls) <- c("poll", "estimate", "low", "high", "sample_size")
polls

#Code: Calculating the spread of combined polls (Aggregation)
d_hat <- polls %>%
  summarize(avg = sum(estimate*sample_size)/sum(sample_size)) %>%
  .$avg #Estimated spread using various polls
p_hat <- (1+d_hat)/2  #Estimate proportion of Obama voters
moe <- 2*qnorm(0.975)*sqrt(p_hat*(1-p_hat)/sum(polls$sample_size))
round(d_hat*100,1)
round(moe*100,1)


# Poll Data and Pollster Bias

#Code: Generating simulated poll data
library(dslabs)
data("polls_us_election_2016")
names(polls_us_election_2016)
#keep only national polls from week before election with a reliable grade
polls <- polls_us_election_2016 %>%
  filter(state == "U.S." & enddate >= "2016-10-31" & 
           (grade %in% c("A+", "A", "A-", "B+") | is.na(grade)))
#add spread estimate
polls <- polls %>%
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)
#compute estimated spread for combined polls
d_hat <- polls %>%
  summarize(d_hat = sum(spread*samplesize)/sum(samplesize)) %>%
  .$d_hat
#compute margin of error
p_hat <- (d_hat+1)/2
moe <- qnorm(.975)*2*sqrt(p_hat*(1-p_hat)/sum(polls$samplesize))
#histogram of the spread
polls %>%
  ggplot(aes(spread)) +
  geom_histogram(color="black", binwidth = .01)

#Code: Investigating poll data and pollster bias
#number of polls per pollster in week before election
polls %>% group_by(pollster) %>% summarize(n())
# plot results by pollsters with at least 6 polls
polls %>% group_by(pollster) %>% filter(n() >= 6) %>%
  ggplot(aes(pollster, spread)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
#standard errors within each pollster
polls %>% group_by(pollster) %>%
  filter(n() >= 6) %>%
  summarize(se = 2*sqrt(p_hat*(1-p_hat)/median(samplesize)))


# Data-Driven Models
# Using an urn with various polls results instead of people

#Code
#collect last results before the election for each pollster
one_poll_per_pollster <- polls %>% group_by(pollster) %>% 
  filter(enddate == max(enddate)) %>%   #keep latest poll
  ungroup()
#histogram of spread estimates
one_poll_per_pollster %>%
  ggplot(aes(spread)) + geom_histogram(binwidth = 0.01)
#construct 95% confidence interval
results <- one_poll_per_pollster %>%
  summarize(avg = mean(spread), se = sd(spread)/sqrt(length(spread))) %>%
  mutate(start = avg - qnorm(.975)*se, end = avg + qnorm(.975)*se)
round(results*100,1)




# Section 5: Bayesian Statistics


# Bayes' Theorem

#Code: Monte Carlo simulation
prev <- 0.00025  # disease prevalence
N <- 100000   # number of tests
outcome <- sample(c("Disease", "Healthy"), N, replace = TRUE, prob = c(prev, 1-prev))
N_D <- sum(outcome == "Disease")  # number with disease
N_H <- sum(outcome == "Healthy")  # number healthy
# for each person, randomly determine if test is + or -
accuracy <- 0.99
test <- vector("character", N)
test[outcome == "Disease"] <- sample(c("+","-"),N_D, replace = TRUE, prob = c(accuracy, 1-accuracy))
test[outcome == "Healthy"] <- sample(c("-","+"),N_H, replace = TRUE, prob = c(accuracy, 1-accuracy))
table(outcome, test)




# Section 6: Election Forecasting


# Election Forecasting

#Code: Definition of results object
library(tidyverse)
library(dslabs)
polls <- polls_us_election_2016 %>%
  filter(state == "U.S." & enddate >= "2016-10-31" &
           (grade %in% c("A+", "A", "A-", "B+") | is.na(grade))) %>%
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)

one_poll_per_pollster <- polls %>% group_by(pollster) %>%
  filter(enddate == max(enddate)) %>%
  ungroup()

results <- one_poll_per_pollster %>%
  summarize(avg = mean(spread), se = sd(spread)/sqrt(length(spread))) %>%
  mutate(start = avg - 1.96*se, end = avg + 1.96*se)
#Code: Computing the posterior mean, standard error, credible interval and probability
mu <- 0
tau <- 0.035
sigma <- results$se
Y <- results$avg
B <- sigma^2 / (sigma^2 + tau^2)
posterior_mean <- B*mu + (1-B)*Y
posterior_se <- sqrt(1 / (1/sigma^2 + 1/tau^2))

posterior_mean
posterior_se
# 95% credible interval
posterior_mean + c(-qnorm(0.975), qnorm(0.975))*posterior_se
# probability of d > 0
1 - pnorm(0, posterior_mean, posterior_se)

































