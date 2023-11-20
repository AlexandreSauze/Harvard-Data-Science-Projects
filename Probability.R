# Probability




# Section 1



# Section 1: Discrete Probability / 1.1 Introduction to Discrete Probability


# Monte Carlo Simulations
beads <- rep( c("red", "blue"), times = c(2,3)) #List of 2 reds and 3 blues
sample(beads,1) #1 random value from our list

B <- 10000
events <- replicate(B, sample(beads, 1)) #B times random select
tab <- table(events)
tab
prop.table(tab)

sample(beads,5) #Random select withut repetition
sample(beads,6) #We run out of options
sample(beads, 6, replace = TRUE) #Repetition is available so we can select without end

events <- sample(beads, B, replace = TRUE)
prop.table(table(events))


# Setting the random seed
set.seed(1986) #Seed to generate random. 1986=2018-12-20 todays date

set.seed(1)
set.seed(1, sample.kind="Rounding")    # will make R 3.6 generate a seed as in R 3.5


# Using the mean function for probability
beads <- rep( c("red", "blue"), times = c(2,3)) #List of 2 reds and 3 blues
beads
mean(beads == "blue")



# Section 1: Discrete Probability / 1.2 Combinations and Permutations


# Combinations and permutations
# joining strings with paste
number <- "Three"
suit <- "Hearts"
paste(number, suit)

# joining vectors element-wise with paste
paste(letters[1:5], as.character(1:5))

# generating combinations of 2 vectors with expand.grid
expand.grid(pants = c("blue", "black"), shirt = c("white", "grey", "plaid"))

#Deck of cards
suits <- c("Diamonds", "Clubs", "Hearts", "Spades")
numbers <- c("Ace", "Deuce", "Three", "Four", "Five", "Six", "Seven", "Eight", "Nine", "Ten", "Jack", "Queen", "King")
deck <- expand.grid(number = numbers, suit = suits)
deck <- paste(deck$number, deck$suit)

# probability of drawing a king
kings <- paste("King", suits)
mean(deck %in% kings)
4/52

# Code: Permutations and combinations
library(gtools)
permutations(5,2)    # ways to choose 2 numbers in order from 1:5
all_phone_numbers <- permutations(10, 7, v = 0:9)    # v = 0.9 makes the 10 go from 0 to 9 instead of 1 to 10
n <- nrow(all_phone_numbers)
index <- sample(n, 5)
all_phone_numbers[index,]

permutations(3,2)    # order matters
combinations(3,2)    # order does not matter

# Code: Probability of drawing a second king given that one king is drawn
hands <- permutations(52,2, v = deck)
first_card <- hands[,1]
second_card <- hands[,2]
sum(first_card %in% kings)

sum(first_card %in% kings & second_card %in% kings) / sum(first_card %in% kings)

#Code: Probability of a natural 21 in blackjack
aces <- paste("Ace", suits)
facecard <- c("King", "Queen", "Jack", "Ten")
facecard <- expand.grid(number = facecard, suit = suits)
facecard <- paste(facecard$number, facecard$suit)

hands <- combinations(52, 2, v=deck) # all possible hands

# probability of a natural 21 given that the ace is listed first in `combinations`
mean(hands[,1] %in% aces & hands[,2] %in% facecard)

# probability of a natural 21 checking for both ace first and ace second
mean((hands[,1] %in% aces & hands[,2] %in% facecard)|(hands[,2] %in% aces & hands[,1] %in% facecard))

# Code: Monte Carlo simulation of natural 21 in blackjack
# code for one hand of blackjack
hand <- sample(deck, 2)
hand

# code for B=10,000 hands of blackjack
B <- 10000
results <- replicate(B, {
  hand <- sample(deck, 2)
  (hand[1] %in% aces & hand[2] %in% facecard) | (hand[2] %in% aces & hand[1] %in% facecard)
})
mean(results)



# Section 1: Discrete Probability / 1.3 Addition Rule and Monty Hall


#The Birthday Problem
# checking for duplicated bdays in one 50 person group
n <- 50
bdays <- sample(1:365, n, replace = TRUE)    # generate n random birthdays
any(duplicated(bdays))    # check if any birthdays are duplicated

# Monte Carlo simulation with B=10000 replicates
B <- 100000
results <- replicate(B, {    # returns vector of B logical values
  bdays <- sample(1:365, n, replace = TRUE)
  any(duplicated(bdays))
})
mean(results)    # calculates proportion of groups with duplicated bdays


#sapply
# function to calculate probability of shared bdays across n people
n <- 50
compute_prob <- function(n, B = 10000) {
  same_day <- replicate(B, {
    bdays <- sample(1:365, n, replace = TRUE)
    any(duplicated(bdays))
  })
  mean(same_day)
}

n <- seq(1, 60)

#Code: Element-wise operation over vectors and sapply
x <- 1:10
sqrt(x)    # sqrt operates on each element of the vector

y <- 1:10
x*y    # * operates element-wise on both vectors

compute_prob(n)    # does not iterate over the vector n without sapply

x <- 1:10
sapply(x, sqrt)    # this is equivalent to sqrt(x)

prob <- sapply(n, compute_prob)    # element-wise application of compute_prob to n
plot(n, prob)

#Code: Computing birthday problem probabilities with sapply
# function for computing exact probability of shared birthdays for any n
exact_prob <- function(n){
  prob_unique <- seq(365, 365-n+1)/365   # vector of fractions for mult. rule
  1 - prod(prob_unique)    # calculate prob of no shared birthdays and subtract from 1
}

# applying function element-wise to vector of n values
eprob <- sapply(n, exact_prob)

# plotting Monte Carlo results and exact probabilities on same graph
plot(n, prob)    # plot Monte Carlo results
lines(n, eprob, col = "red")    # add line for exact prob


#How Many Monte Carlo Experiments are Enough?
B <- 10^seq(1, 5, len = 100)    # defines vector of many B values
compute_prob <- function(B, n = 22){    # function to run Monte Carlo simulation with each B
  same_day <- replicate(B, {
    bdays <- sample(1:365, n, replace = TRUE)
    any(duplicated(bdays))
  })
  mean(same_day)
}

prob <- sapply(B, compute_prob)    # apply compute_prob to many values of B
plot(log10(B), prob, type = "l")    # plot a line graph of estimates 



celtics_win <- replicate(B, {
  simulated_games <- sample(c("lose","win"), 4, replace = TRUE, prob = c(0.6, 0.4))
  any(simulated_games == "win")
})


#The Monty Hall Problem
#Code: Monte Carlo simulation of stick strategy
B <- 10000
stick <- replicate(B, {
  doors <- as.character(1:3)
  prize <- sample(c("car","goat","goat"))    # puts prizes in random order
  prize_door <- doors[prize == "car"]    # note which door has prize
  my_pick  <- sample(doors, 1)    # note which door is chosen
  show <- sample(doors[!doors %in% c(my_pick, prize_door)],1)    # open door with no prize that isn't chosen
  stick <- my_pick    # stick with original door
  stick == prize_door    # test whether the original door has the prize
})
mean(stick)    # probability of choosing prize door when sticking

#Code: Monte Carlo simulation of switch strategy
switch <- replicate(B, {
  doors <- as.character(1:3)
  prize <- sample(c("car","goat","goat"))    # puts prizes in random order
  prize_door <- doors[prize == "car"]    # note which door has prize
  my_pick  <- sample(doors, 1)    # note which door is chosen first
  show <- sample(doors[!doors %in% c(my_pick, prize_door)], 1)    # open door with no prize that isn't chosen
  switch <- doors[!doors%in%c(my_pick, show)]    # switch to the door that wasn't chosen first or opened
  switch == prize_door    # test whether the switched door has the prize
})
mean(switch)    # probability of choosing prize door when switching



# Section 1: Discrete Probability / 1.4 Assessment: Discrete Probability


library(gtools)
library(tidyverse)

# Question 1: Olympic running
# 1.a How many different ways can the 3 medals be distributed across 8 runners?
nrow(permutations(8,3))

# 1.b How many different ways can the three medals be distributed among the 3 runners from Jamaica?
nrow(permutations(3,3))

# 1.c What is the probability that all 3 medals are won by Jamaica?
(3/8)*(2/7)*(1/6)

# 1.d Run a Monte Carlo simulation on this vector representing the countries of the 8 runners in this race:
runners <- c("Jamaica", "Jamaica", "Jamaica", "USA", "Ecuador", "Netherlands", "France", "South Africa")
# For each iteration of the Monte Carlo simulation, within a replicate() loop, select 3 runners representing 
# the 3 medalists and check whether they are all from Jamaica. Repeat this simulation 10,000 times. Set the 
# seed to 1 before running the loop.Calculate the probability that all the runners are from Jamaica.
set.seed(1)
results <- replicate(10000,{
  race <- sample(runners,3)
  yes <- sum(sum(race == "Jamaica") == 3)
  
})
mean(results)


# Question 2: Restaurant management
# 2.a How many meal combinations are possible with the current menu?
entrees <- paste("E",c(1:6))
combination_of_sides <- nrow(combinations(6,2))
sides <- paste("S",1:combination_of_sides)
drinks <- paste("D",c(1:2))
meals <- expand.grid(Entree = entrees, Sides = sides, Drink = drinks)
nrow(meals)

nrow(combinations(6,2))
6*15*2

# 2.b The manager has one additional drink he could add to the special.
# How many combinations are possible if he expands his original special to 3 drink options?
6*15*3

# 2.c The manager decides to add the third drink but needs to expand the number of options. 
#The manager would prefer not to change his menu further and wants to know if he can meet his 
#goal by letting customers choose more sides.How many meal combinations are there if customers 
#can choose from 6 entrees, 3 drinks, and select 3 sides from the current 6 options?
nrow(combinations(6,3))
6*20*3

# 2.d The manager is concerned that customers may not want 3 sides with their meal. He is willing 
#to increase the number of entree choices instead, but if he adds too many expensive options it 
#could eat into profits. He wants to know how many entree choices he would have to offer in order 
#to meet his goal. What is the minimum number of entree options required in order to generate more than 365 combinations?
num_entrees <- function(n) {
  n*15*3
}
n <- c(1:12)
data.frame(meals = sapply(n,num_entrees), entrees = n)

# 2.e The manager isn't sure he can afford to put that many entree choices on the lunch menu and 
#thinks it would be cheaper for him to expand the number of sides. He wants to know how many sides 
#he would have to offer to meet his goal of at least 365 combinations.
num_sides <- function(n) {
  no_comb <- nrow(combinations(n,2))
  6*no_comb*3
}
n <- c(2:12)
data.frame(meals = sapply(n,num_sides), sides = n)


#Questions 3 and 4: Esophageal cancer and alcohol/tobacco use, part 1
head(esoph)
library(tidyverse)
#3.a How many groups are in the study?
nrow(esoph)

#3.b How many cases are there?
all_cases <- sum(esoph$ncases)
all_cases

#3.c How many controls are there?
all_controls <- sum(esoph$ncontrols)
all_controls

#4.a What is the probability that a subject in the highest alcohol consumption group is a cancer case?
esoph %>% filter(alcgp == "120+") %>% summarize(Cases = sum(ncases), Total = sum(ncases+ncontrols), Prob = Cases/Total) %>% pull(Prob)

#4.b What is the probability that a subject in the lowest alcohol consumption group is a cancer case?
esoph %>% filter(alcgp == "0-39g/day") %>% summarize(Cases = sum(ncases), Total = sum(ncases+ncontrols), Prob = Cases/Total) %>% pull(Prob)

#4.c Given that a person is a case, what is the probability that they smoke 10g or more a day?
Smoke10 <- esoph %>% filter(ncases > 0 & tobgp != "0-9g/day") %>% summarize(sum(ncases))
Smoke10
Smoke10/all_cases

esoph %>% filter(tobgp != "0-9g/day" & ncases > 0) %>% summarize(Smoke = sum(ncases), total = Smoke/all_cases) %>% pull(total)

#4.d Given that a person is a control, what is the probability that they smoke 10g or more a day?
esoph %>% filter(tobgp != "0-9g/day" & ncontrols > 0) %>% summarize(Smoke = sum(ncontrols), total = Smoke/all_controls) %>% pull(total)


#Questions 5 and 6: Esophageal cancer and alcohol/tobacco use, part 2
#5.a For cases, what is the probability of being in the highest alcohol group?
a5 <- esoph %>% filter(alcgp == "120+" & ncases > 0) %>% summarize(Alcohol = sum(ncases), total = Alcohol/all_cases) %>% pull(total)
a5

#5.b For cases, what is the probability of being in the highest tobacco group?
esoph %>% filter(tobgp == "30+" & ncases > 0) %>% summarize(Smoke = sum(ncases), Tot = Smoke/all_cases) %>% pull(Tot)

#5.c For cases, what is the probability of being in the highest alcohol group and the highest tobacco group?
esoph %>% filter(alcgp == "120+" & tobgp == "30+" & ncases > 0) %>% summarize(Drink_Smoke = sum(ncases), Tot = Drink_Smoke/all_cases) %>% pull(Tot)

#5.d For cases, what is the probability of being in the highest alcohol group or the highest tobacco group?
d5 <- esoph %>% filter(alcgp == "120+" | tobgp == "30+" & ncases > 0) %>% summarize(DS = sum(ncases), Prob = DS/all_cases) %>% pull(Prob)
d5

#6.a For controls, what is the probability of being in the highest alcohol group?
a6 <- esoph %>% filter(alcgp == "120+" & ncontrols > 0) %>% summarize(Alcohol = sum(ncontrols), total = Alcohol/all_controls) %>% pull(total)
a6

#6.b How many times more likely are cases than controls to be in the highest alcohol group?
a5/a6

#6.c For controls, what is the probability of being in the highest tobacco group?
esoph %>% filter(tobgp == "30+" & ncontrols > 0) %>% summarize(Smoke = sum(ncontrols), Tot = Smoke/all_controls) %>% pull(Tot)

#6.d For controls, what is the probability of being in the highest alcohol group and the highest tobacco group?
esoph %>% filter(alcgp == "120+" & tobgp == "30+" & ncontrols > 0) %>% summarize(Drink_Smoke = sum(ncontrols), Tot = Drink_Smoke/all_controls) %>% pull(Tot)

#6.e For controls, what is the probability of being in the highest alcohol group or the highest tobacco group?
e6 <- esoph %>% filter(alcgp == "120+" | tobgp == "30+" & ncontrols > 0) %>% summarize(Drink_Smoke = sum(ncontrols), Tot = Drink_Smoke/all_controls) %>% pull(Tot)
e6

#6.f How many times more likely are cases than controls to be in the highest alcohol group or the highest tobacco group?
d5/e6




# Section 2: Continuous Probability



# Section 2: Continuous Probability/2.1 Continuous Probability


# Continuous Probability

library(tidyverse)
library(dslabs)
data(heights)
x <- heights %>% filter(sex=="Male") %>% pull(height)
#Given a vector x, we can define a function for computing the CDF of x using:
F <- function(a) mean(x <= a)
1 - F(70)    # probability of male taller than 70 inches


# Theoretical Distribution

#Code: Using pnorm() to calculate probabilities
#Given male heights x:
library(tidyverse)
library(dslabs)
data(heights)
x <- heights %>% filter(sex=="Male") %>% pull(height)
#We can estimate the probability that a male is taller than 70.5 inches using:
1 - pnorm(70.5, mean(x), sd(x))

#Code: Discretization and the normal approximation
#plot distribution of exact heights in data
plot(prop.table(table(x)), xlab = "a = Height in inches", ylab = "Pr(x = a)")

#probabilities in actual data over length 1 ranges containing an integer
mean(x <= 68.5) - mean(x <= 67.5)
mean(x <= 69.5) - mean(x <= 68.5)
mean(x <= 70.5) - mean(x <= 69.5)

#probabilities in normal approximation match well
pnorm(68.5, mean(x), sd(x)) - pnorm(67.5, mean(x), sd(x))
pnorm(69.5, mean(x), sd(x)) - pnorm(68.5, mean(x), sd(x))
pnorm(70.5, mean(x), sd(x)) - pnorm(69.5, mean(x), sd(x))

#probabilities in actual data over other ranges don't match normal approx as well
mean(x <= 70.9) - mean(x <= 70.1)
pnorm(70.9, mean(x), sd(x)) - pnorm(70.1, mean(x), sd(x))


# Plotting the Probability Density
library(tidyverse)
x <- seq(-4, 4, length = 100)
data.frame(x, f = dnorm(x)) %>%
  ggplot(aes(x, f)) +
  geom_line()


# Monte Carlo Simulations (Normal PDF)
#Code: Generating normally distributed random numbers
# define x as male heights from dslabs data
library(tidyverse)
library(dslabs)
data(heights)
x <- heights %>% filter(sex=="Male") %>% pull(height)
# generate simulated height data using normal distribution - both datasets should have n observations
n <- length(x)
avg <- mean(x)
s <- sd(x)
simulated_heights <- rnorm(n, avg, s)
# plot distribution of simulated_heights
data.frame(simulated_heights = simulated_heights) %>%
  ggplot(aes(simulated_heights)) +
  geom_histogram(color="black", binwidth = 2)

#Code: Monte Carlo simulation of tallest person over 7 feet
B <- 10000
tallest <- replicate(B, {
  simulated_data <- rnorm(800, avg, s)    # generate 800 normally distributed random heights
  max(simulated_data)    # determine the tallest height
})
mean(tallest >= 7*12)    # proportion of times that tallest person exceeded 7 feet (84 inches)

# DataCamp Assessment: Continuous Probability
highestIQ <- replicate(B,{
  simulated_data <- rnorm(10000,100,15)
  max(simulated_data)})
hist(highestIQ)



# Section 2: Continuous Probability/2.2 Assessment: Continuous Probability


# Questions 1 and 2: ACT scores, part 1

#Set the seed to 16, then use rnorm() to generate a normal distribution of 10000 
#tests with a mean of 20.9 and standard deviation of 5.7. Save these values as 
#act_scores. You'll be using this dataset throughout these four multi-part 
#questions.
set.seed(16, sample.kind = "Rounding")
act_scores <- rnorm(10000, 20.9, 5.7)

#1.a What is the mean of act_scores?
mean(act_scores)

#1.b What is the standard deviation of act_scores?
sd(act_scores)

#1.c 
#A perfect score is 36 or greater (the maximum reported score is 36).
#In act_scores, how many perfect scores are there out of 10,000 simulated tests?
sum(act_scores >= 36)

#1.d
#In act_scores, what is the probability of an ACT score greater than 30?
sum(act_scores >= 30)/10000

#1.e
#In act_scores, what is the probability of an ACT score less than or equal to 10?
sum(act_scores <= 10)/10000

#2
#Set x equal to the sequence of integers 1 to 36. Use dnorm to determine the 
#value of the probability density function over x given a mean of 20.9 and 
#standard deviation of 5.7; save the result as f_x. Plot x against f_x.
x <- seq(1,36)
f_x <- dnorm(x, 20.9, 5.7)
plot(x,f_x)

#3
Z <- (act_scores - mean(act_scores)) / sd(act_scores)

#3.a
#What is the probability of a Z-score greater than 2 (2 standard deviations above the mean)?
sum(Z > 2)/length(act_scores)

#3.b
#What ACT score value corresponds to 2 standard deviations above the mean (Z = 2)?
20.9 + 2*5.7

#3.c
#A Z-score of 2 corresponds roughly to the 97.5th percentile.
#Use qnorm() to determine the 97.5th percentile of normally distributed data 
#with the mean and standard deviation observed in act_scores.
#What is the 97.5th percentile of act_scores?
qnorm(0.975, mean(act_scores), sd(act_scores))

#4
#In this 4-part question, you will write a function to create a CDF for ACT scores.
#Write a function that takes a value and produces the probability of an ACT 
#score less than or equal to that value (the CDF). Apply this function to the range 1 to 36.
ACT_prob <- function(a){
  pnorm(a, mean(act_scores), sd(act_scores))
}
scores <- 1:36
results <- ACT_prob(scores)

#4.a
#What is the minimum integer score such that the probability of that score or lower is at least .95?
min(scores[(results >= 0.95)])
ceiling(qnorm(0.95, mean(act_scores), sd(act_scores)))

#4.b
#Use qnorm() to determine the expected 95th percentile, the value for which the 
#probability of receiving that score or lower is 0.95, given a mean score of 20.9 
#and standard deviation of 5.7.
#What is the expected 95th percentile of ACT scores?
qnorm(0.95, 20.9, 5.7)

#4.c
#As discussed in the Data Visualization course, we can use quantile() to
#determine sample quantiles from the data.
#Make a vector containing the quantiles for p <- seq(0.01, 0.99, 0.01), 
#the 1st through 99th percentiles of the act_scores data. Save these as 
#sample_quantiles.
#In what percentile is a score of 26?
p <- seq(0.01, 0.99, 0.01)
sample_quantiles <- quantile(act_scores, p)
sample_quantiles

#4.d
#Make a corresponding set of theoretical quantiles using qnorm() over the 
#interval p <- seq(0.01, 0.99, 0.01) with mean 20.9 and standard deviation 5.7. 
#Save these as theoretical_quantiles. Make a QQ-plot graphing sample_quantiles 
#on the y-axis versus theoretical_quantiles on the x-axis.
theoretical_quantiles <- qnorm(p, 20.9, 5.7)
plot(theoretical_quantiles,sample_quantiles)
abline(0,1)




# Section 3: Random Variables, Sampling Models, and the Central Limit Theorem



# 3.1 Random Variables and Sampling Models, Incomplete


# Random Variables

# define random variable x to be 1 if blue, 0 otherwise
beads <- rep(c("red", "blue"), times = c(2, 3))
x <- ifelse(sample(beads, 1) == "blue", 1, 0)
# demonstrate that the random variable is different every time
ifelse(sample(beads, 1) == "blue", 1, 0)
ifelse(sample(beads, 1) == "blue", 1, 0)
ifelse(sample(beads, 1) == "blue", 1, 0)


# Sampling Models

#Monte Carlo simulation: Chance of casino losing money on roulette
#S is the winnings of the casino
# sampling model 1: define urn, then sample
color <- rep(c("Black", "Red", "Green"), c(18, 18, 2)) # define the urn for the sampling model
n <- 1000
X <- sample(ifelse(color == "Red", -1, 1), n, replace = TRUE)
X[1:10]
# sampling model 2: define urn inside sample function by noting probabilities
x <- sample(c(-1, 1), n, replace = TRUE, prob = c(9/19, 10/19))    # 1000 independent draws
S <- sum(x)    # total winnings = sum of draws
S

n <- 1000    # number of roulette players
B <- 10000    # number of Monte Carlo experiments
S <- replicate(B, {
  X <- sample(c(-1,1), n, replace = TRUE, prob = c(9/19, 10/19))    # simulate 1000 spins
  sum(X)    # determine total profit
})
mean(S < 0)    # probability of the casino losing money

library(tidyverse)
s <- seq(min(S), max(S), length = 100)    # sequence of 100 values across range of S
normal_density <- data.frame(s = s, f = dnorm(s, mean(S), sd(S))) # generate normal density for S
data.frame (S = S) %>%    # make data frame of S for histogram
  ggplot(aes(S, ..density..)) +
  geom_histogram(color = "black", binwidth = 10) +
  ylab("Probability") +
  geom_line(data = normal_density, mapping = aes(s, f), color = "blue")


# Questions 1 and 2: SAT testing
#An old version of the SAT college entrance exam had a -0.25 point penalty for 
#every incorrect answer and awarded 1 point for a correct answer. The 
#quantitative test consisted of 44 multiple-choice questions each with 5 answer 
#choices. Suppose a student chooses answers by guessing for all questions on the test.

#1.a
#What is the probability of guessing correctly for one question?
1/5

#1.b
#What is the expected value of points for guessing on one question?
-0.25*(4/5) + 1/5

#1.c
#What is the expected score of guessing on all 44 questions?
AVG <- 44*(-0.25*(4/5) + 1/5)

#1.d
#What is the standard error of guessing on all 44 questions?
SE <- sqrt(44)*abs(-0.25-1)*sqrt(4/5 * 1/5)

#1.e
#Use the Central Limit Theorem to determine the probability that a guessing 
#student scores 8 points or higher on the test.
1 - pnorm(8, AVG, SE)

#1.f
set.seed(21, sample.kind = "Rounding")
#What is the probability that a guessing student scores 8 points or higher?
B <- 10000
n <- 44
SAT_sim <- replicate(B, {
  sum(sample(c(-0.25,1), n, replace = TRUE, prob = c(4/5, 1/5)))
})
mean(SAT_sim > 8)

#2
#The SAT was recently changed to reduce the number of multiple choice options 
#from 5 to 4 and also to eliminate the penalty for guessing.

#2.a
#Suppose that the number of multiple choice options is 4 and that there is no 
#penalty for guessing - that is, an incorrect question gives a score of 0.
#What is the expected value of the score when guessing on this new test?
44*(0*3/4 + 1*1/4)

#2.b
#Consider a range of correct answer probabilities p <- seq(0.25, 0.95, 0.05) 
#representing a range of student skills.
#What is the lowest p such that the probability of scoring over 35 exceeds 80%?
p <- seq(0.25, 0.95, 0.05)
ex_va <- 44*(1*p + 0*(1-p))
st_er <- sqrt(44)*abs(0-1)*sqrt(p*(1-p))
1 - pnorm(35,ex_va, st_er)

#3
#A casino offers a House Special bet on roulette, which is a bet on five pockets 
#(00, 0, 1, 2, 3) out of 38 total pockets. The bet pays out 6 to 1. In other 
#words, a losing bet yields -$1 and a successful bet yields $6. A gambler wants 
#to know the chance of losing money if he places 500 bets on the roulette House 
#Special.

#3.a
#What is the expected value of the payout for one bet?
-1*33/38 + 6*5/38

#3.b
#What is the standard error of the payout for one bet?
abs(-1-6)*sqrt(33/38*5/38)

#3.c
#What is the expected value of the average payout over 500 bets?
-1*33/38 + 6*5/38

#3.d
#What is the standard error of the average payout over 500 bets?
abs(-1-6)*sqrt(33/38*5/38)/sqrt(500)

#3.e
#What is the expected value of the sum of 500 bets?
avg <- 500*(-1*33/38 + 6*5/38)

#3.f
#What is the standard error of the sum of 500 bets?
se <- sqrt(500)*abs(-1-6)*sqrt(33/38*5/38)

#3.g
#Use pnorm() with the expected value of the sum and standard error of the sum to 
#calculate the probability of losing money over 500 bets, P<=0.
pnorm(0,avg,se)



# Section 4: The Big Short


# The Big Short: Interest Rates Explained

#Code: Interest rate sampling model
n <- 1000   #Loans
loss_per_foreclosure <- -200000
p <- 0.02   #Prob. that the clients will not pay up
defaults <- sample( c(0,1), n, prob=c(1-p, p), replace = TRUE)
sum(defaults * loss_per_foreclosure)

#Code: Interest rate Monte Carlo simulation
B <- 10000
losses <- replicate(B, {
  defaults <- sample( c(0,1), n, prob=c(1-p, p), replace = TRUE) 
  sum(defaults * loss_per_foreclosure)
})
#Code: Plotting expected losses
library(tidyverse)
data.frame(losses_in_millions = losses/10^6) %>%
  ggplot(aes(losses_in_millions)) +
  geom_histogram(binwidth = 0.6, col = "black")   #Seems Approx. Normal

#Code: Expected value and standard error of the sum of 1,000 loans
n*(p*loss_per_foreclosure + (1-p)*0)    # expected value
sqrt(n)*abs(loss_per_foreclosure)*sqrt(p*(1-p))    # standard error

#Code: Calculating interest rates for expected value of 0
x = - loss_per_foreclosure*p/(1-p)
x
x/180000    #Interest rate for a $180,000 loan

#Code: Calculating interest rate for 1% probability of losing money
l <- loss_per_foreclosure
z <- qnorm(0.01)
x <- -l*( n*p - z*sqrt(n*p*(1-p)))/ ( n*(1-p) + z*sqrt(n*p*(1-p)))
x/180000    # interest rate
loss_per_foreclosure*p + x*(1-p)    # expected value of the profit per loan
n*(loss_per_foreclosure*p + x*(1-p)) # expected value of the profit over n loans

#Code: Monte Carlo simulation for 1% probability of losing money
B <- 100000
profit <- replicate(B, {
  draws <- sample( c(x, loss_per_foreclosure), n, 
                   prob=c(1-p, p), replace = TRUE) 
  sum(draws)
})
mean(profit)    # expected value of the profit over n loans
mean(profit<0)    # probability of losing money

#Code: Expected value with higher default rate and interest rate
p <- .04
loss_per_foreclosure <- -200000
r <- 0.05
x <- r*180000
loss_per_foreclosure*p + x*(1-p)

#Code: Calculating number of loans for desired probability of losing money
z <- qnorm(0.01)
l <- loss_per_foreclosure
n <- ceiling((z^2*(x-l)^2*p*(1-p))/(l*p + x*(1-p))^2)
n    # number of loans required
n*(loss_per_foreclosure*p + x * (1-p))    # expected profit over n loans

#Code: Monte Carlo simulation with known default probability
B <- 10000
p <- 0.04
x <- 0.05 * 180000
profit <- replicate(B, {
  draws <- sample( c(x, loss_per_foreclosure), n, 
                   prob=c(1-p, p), replace = TRUE) 
  sum(draws)
})
mean(profit)
mean(profit<0)

#Code: Monte Carlo simulation with unknown default probability
#This Monte Carlo simulation estimates the expected profit given an unknown 
#probability of default "0.03 <= p <= 0.05", modeling the situation where an event changes the 
#probability of default for all borrowers simultaneously.
p <- 0.04
x <- 0.05*180000
profit <- replicate(B, {
  new_p <- 0.04 + sample(seq(-0.01, 0.01, length = 100), 1)
  draws <- sample( c(x, loss_per_foreclosure), n, 
                   prob=c(1-new_p, new_p), replace = TRUE)
  sum(draws)
})
mean(profit)    # expected profit
mean(profit < 0)    # probability of losing money
mean(profit < -10000000)    # probability of losing over $10 million




# 4.2 Assessment: The Big Short
library(tidyverse)
library(dslabs)
data(death_prob)
head(death_prob)
#An insurance company offers a one-year term life insurance policy that pays 
#$150,000 in the event of death within one year. The premium (annual cost) for 
#this policy for a 50 year old female is $1,150. Suppose that in the event of a 
#claim, the company forfeits the premium and loses a total of $150,000, and if 
#there is no claim the company gains the premium amount of $1,150. The company 
#plans to sell 1,000 policies to this demographic.

# 1.a
#The death_prob data frame from the dslabs package contains information about 
#the estimated probability of death within 1 year (prob) for different ages and 
#sexes.
#Use death_prob to determine the death probability of a 50 year old female, p.
p_fem50 <- death_prob %>% filter(age == 50, sex == "Female") %>% pull(prob)
p_fem50

# 1.b
#The loss in the event of the policy holder's death is -$150,000 and the gain if 
#the policy holder remains alive is the premium $1,150.
#What is the expected value of the company's net profit on one policy for a 50 
#year old female?
(1150*(1-p_fem50) + (-150000)*p_fem50)

# 1.c
#Calculate the standard error of the profit on one policy for a 50 year old female.
abs(1150 - -150000)*sqrt(p_fem50*(1-p_fem50))

# 1.d
#What is the expected value of the company's profit over all 1,000 policies for 50 year old females?
AVG <- 1000*(1150*(1-p_fem50) + (-150000)*p_fem50)

# 1.e
#What is the standard error of the sum of the expected value over all 1,000 policies for 50 year old females?
SE <- abs(1150 - -150000)*sqrt(1000*p_fem50*(1-p_fem50))

# 1.f
#Use the Central Limit Theorem to calculate the probability that the insurance company loses money on this set of 1,000 policies.
pnorm(0,AVG,SE)

# 2.a
#Use death_prob to determine the probability of death within one year for a 50 year old male.
p_male50 <- death_prob %>% filter(age == 50, sex == "Male") %>% pull(prob)
p_male50

# 2.b
#Suppose the company wants its expected profits from 1,000 50 year old males with 
#$150,000 life insurance policies to be $700,000. Use the formula for expected 
#value of the sum of draws with the following values and solve for the premium b:
AVG <- 700000
n <- 1000
a <- -150000
# AVG = (a*(p_male50) + b*(1-p_male50))*n
b <- (AVG/n - a*p_male50)/(1-p_male50)
b

# 2.c
#Using the new 50 year old male premium rate, calculate the standard error of the sum of 1,000 premiums.
SE <- abs(a-b)*sqrt(n*p_male50*(1-p_male50))

# 2.d
#What is the probability of losing money on a series of 1,000 policies to 50 year old males?
pnorm(0,AVG,SE)

# 3.a
#We'll look at a scenario in which a lethal pandemic disease increases the 
#probability of death within 1 year for a 50 year old to .015. Unable to predict 
#the outbreak, the company has sold 1,000 $150,000 life insurance policies for $1,150.
#What is the expected value of the company's profits over 1,000 policies?
1000*(a*(0.015)+1150*(1-0.015))

err <- sqrt(1000) * abs(-150000 - 1150) * sqrt(0.015*(1 - 0.015))
err

pnorm(0, mu, err)

pnorm(-1000000,mu,err)

p <- seq(0.01, 0.03, 0.001)
f <- function(p){
  mu <- 1000 * (-150000*p + 1150*(1-p))
  err <- sqrt(1000) * abs(-150000 - 1150) * sqrt(p*(1 - p))
  pnorm(0, mu, err)
}
sapply(p, FUN=f)

p <- seq(0.01, 0.03, 0.0025)
f1 <- function(p){
  mu <- 1000 * (-150000*p + 1150*(1-p))
  err <- sqrt(1000) * abs(-150000 - 1150) * sqrt(p*(1 - p))
  pnorm(-1000000, mu, err)
}
sapply(p, FUN=f1)

set.seed(25)
n <- 1000
p_loss <- 0.015
X <- sample(c(0,1), n, replace=TRUE, prob=c((1-p_loss),p_loss))
loss <- -150000*sum(X==1)/10^6 # in millions
profit <- 1150*sum(X==0)/10^6
loss+profit

set.seed(27)
S <- replicate(10000, {
  X <- sample(c(0, 1), 1000, replace=TRUE, prob=c((1-0.015), 0.015))
  loss <- -150000*sum(X==1)/10^6 # in millions
  profit <- 1150*sum(X==0)/10^6
  loss+profit
})
sum(S<=-1)/10000

p <- .015
n <- 1000
l <- -150000
z <- qnorm(.05)
x <- -l*( n*p - z*sqrt(n*p*(1-p)))/ ( n*(1-p) + z*sqrt(n*p*(1-p)))
x

l*p + x*(1-p)

n*(l*p + x*(1-p))

set.seed(28)
S <- replicate(10000, {
  X <- sample(c(0,1), n, replace = TRUE, prob=c((1-p), p))
  loss <- l*sum(X==1)/10^6 # in millions
  profit <- x*sum(X==0)/10^6
  loss+profit
})
sum(S<0)/10000

set.seed(29)
n <- 1000
B <- 10000
l <- -150000
p <- 0.015
x <- 3268
X <- replicate(B, {
  new_p <- p + sample(seq(-0.01, 0.01, length=100), 1)
  Y <- sample(c(x, l), n, replace=TRUE, prob=c(1-new_p, new_p))
  sum(Y)
})

mean(X)

sum(X<0)/B

mean(X< -1000000)


