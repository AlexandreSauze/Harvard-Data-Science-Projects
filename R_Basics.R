#Section 1

#Section 1: R Basics, Functions, and Data Types/1.1 Motivation and Getting Started/Running Commands While Editing Scripts
library(tidyverse)
library(dslabs)
data(murders)

murders %>% ggplot(aes(population, total, label=abb, color=region)) + geom_label()


#Section 1: R Basics, Functions, and Data Types/1.2 R Basics/R Basics
#To solve "x^2 + x - 1 = 0"
a <- 2
b <- -1
c <- -4
(-b + sqrt(b^2 - 4*a*c))/(2*a)
(-b - sqrt(b^2 - 4*a*c))/(2*a)


#Section 1: R Basics, Functions, and Data Types/1.3 Data Types/Data Types
library(dslabs)
data(murders)
class(murders) #Shows what type of information is the argument (numeric, character, integer, function, data frame, logical, factor, etc)
#In this case murders is a Data Frame which is a table with observations in the rows and variables in the columns
str(murders) #Shows the structure of the argument
head(murders) #Shows the first six rows of each column
murders$population #The "$" sign is called accessor and it shows the data of a column
names(murders) #Shows the name of the columns
length(murders$population) #Shows the number of entries in a vector

class(murders$region) #The region column is a factor because it is a category
levels(murders$region) #It shows the categories of a factor
#Also factors are more efficient and lighter than characters

z <- 3 == 2 # z is a logical vector because it contains a FALSE. All characters, numericals and logicals are vectors even if they have only one entry.

table(murders$region) #The table function shows the number of things inside a factor, vector or list
list <- c("a","a","a","b","b","c") #The c() function concatenates numbers, characters or logicals
table(list)


#Section 1: R Basics, Functions, and Data Types/Section 1 Assessment
#Question 3a-3d
library(dslabs)
data(movielens)
str(movielens)
#Question 4
levels(movielens$genres)
nlevels(movielens$genres)
length(levels(movielens$genres))




#Section 2

#Section 2: Vectors, Sorting/2.1 Vectors/Vectors
codes <- c(380,124,818) #Function "c()" concatenates things inside of it to create a vector
country <- c("Italy", "Canada", "Egypt")
codes <- c(Italy = 380, Canada = 124, Egypt = 818) #This asigns values to codes. This is still a numeric vector
class(codes) #To corroborate that it is a numeric vector
codes <- c("Italy" = 380, "Canada" = 124, "Egypt" = 818) #It makes no difference to use quotes or not

codes <- c(380,124,818)
country <- c("Italy", "Canada", "Egypt")
names(codes) <- country #The function "names()" assigns the names of one vector to the values of the vector inside the function.

seq(1,10) #Writes a sequence of numbers from the first entry to the last one by one. This is an integer vector
seq(0,10,2) #The first entry is the starting number, second is the last number and third is the rate of change. This is a numeric vector
1:10 #Same as seq but one by one default

#Subsetting lets us acces specific parts of a vector with "[]"
codes[2]
codes[c(1,3)] #It is necessary to create a vector of positions to acces many parts at once
codes[1:2]
codes["Canada"] #It is necessary to use quotes in this case
codes[c("Italy", "Egypt")]


#Section 2: Vectors, Sorting/2.1 Vectors/Vector Coercion
#Coercion is when R transforms something it doesn't understand or doesn't fit into something valid before throwing an error
x <- c(1, "Canada", 3) #1 and 3 become characters to fit the vector. This means R coerced 1 and 3 into characters
class(x)

x <- 1:5
x <- as.character(x) #Function "as.character()" coerces into characters. Aka typecasting.
x <- as.numeric(x) #Coerces into numeric

x <- c("1", "b", "3")
as.numeric(x) #It coerces "b" into "NA" as it can't be coerced into a number
#"NA" is a logical

seq(0, 100, length.out = 5)#The argument "length.out" inside sequence lets us generate sequences that are increasing by the same amount but are of the prespecified length


#Course/Section 2: Vectors, Sorting/2.2 Sorting
library(dslabs)
data(murders)
sort(murders$total) #This sorts the values from smallest to biggest

x <- c(31, 4, 15, 92, 65)
x
sort(x)    # puts elements in increasing order

index <- order(x)    #Creates a vector with the positions to the increasing number
x[index]    # rearranging by this index puts elements in order
order(x)

murders$state[1:10]
murders$abb[1:10]

index <- order(murders$total)
murders$abb[index]    # order abbreviations by total murders
murders$state[index]

max(murders$total)    # highest number of total murders
i_max <- which.max(murders$total)    # index with highest number of murders
i_max
murders$state[i_max]    # state name with highest number of total murders

x <- c(31, 4, 15, 92, 65)
x
rank(x)    # returns ranks (smallest to largest)


#Course/Section 2: Vectors, Sorting/2.2 Sorting/DataCamp Assessment: 2.2 Sorting
library(dslabs)
data(na_example)
str(na_example)
na_example
mean(na_example)
ind <- is.na(na_example)
ind
# Determine how many NA ind has using the sum function
sum(is.na(na_example)==TRUE)

x <- c(1, 2, 3)
ind <- c(FALSE, TRUE, FALSE)
x[!ind] #"!" means "NOT" which can turn "FALSE" into "TRUE" and vice versa


#Course/  Section 2: Vectors, Sorting/2.3 Vector Arithmetic
murders$state[which.max(murders$population)]
max(murders$population)
heights <- c(68, 94, 73, 57, 27, 84, 36)
heights*2.54 #The number is multiplied into each value of the vector
heights - 37 #Substracts the number to each number in the vector

murder_rate <- murders$total/murders$population*100000
murders$state[order(murder_rate, decreasing = TRUE)]

#Section 2: Vectors, Sorting/Section 2 Assessment
x <- c(2, 43, 27, 96, 18)
sort(x)
order(x)
rank(x)
min(x)
which.min(x)
max(x)
which.max(x)
name <- c("Mandi", "Amy", "Nicole", "Olivia")
distance <- c(0.8, 3.1, 2.8, 4.0)
time <- c(10, 30, 40, 50)
time <- time/60
speed <- distance/time
speed_person <- data.frame('name' = name, 'speed' = speed)
speed_person




#Section 3

#Section 3: Indexing, Data Wrangling, Plots/3.1 Indexing
murder_rate <- murders$total/murders$population*100000
index <- murder_rate <= 0.71
murders$state[index]
sum(index)
# "!=" not equal to
# "!" NOT
# "|" OR only one needs to be TRUE
# "&" AND both need to be TRUE
west <- murders$region == "West"
safe <- murder_rate <= 1
index <- safe & west
murders$state[index]


#Section 3: Indexing, Data Wrangling, Plots/3.1 Indexing
x <- c(FALSE, TRUE, FALSE, TRUE, TRUE, FALSE)
which(x)    # returns indices that are TRUE
index <- which(murders$state == "Massachusetts")
murder_rate[index]
# to obtain the indices and subsequent murder rates of New York, Florida, Texas, we do:
index <- match(c("New York", "Florida", "Texas"), murders$state)
murders$state[index]
murder_rate[index]

x <- c("a", "b", "c", "d", "e")
y <- c("a", "d", "f")
y %in% x
# to see if Boston, Dakota, and Washington are states
c("Boston", "Dakota", "Washington") %in% murders$state


#Section 3: Indexing, Data Wrangling, Plots/3.2 Basic Data Wrangling
library(dplyr)
murders <- mutate(murders, rate = total/population * 100000) # "mutate()" creates a new column
head(murders)

filter(murders, rate <= 0.71) #filters a data set

new_table <- select(murders, state, region, rate) #creates a new table with only the selected columns from a data set
filter(new_table, rate <= 0.71)

murders %>% select(state, region, rate) %>% filter(rate <= 0.71) # the pipe function like does different function for a data set at the same time


#Section 3: Indexing, Data Wrangling, Plots/3.2 Basic Data Wrangling
# creating a data frame with stringAsFactors = FALSE so that the string values don't become factors
grades <- data.frame(names = c("John", "Juan", "Jean", "Yao"),
                     exam_1 = c(95, 80, 90, 85), 
                     exam_2 = c(90, 85, 85, 90),
                     stringsAsFactors = FALSE)


#Section 3: Indexing, Data Wrangling, Plots/3.3 Basic Plots
population_in_millions <- murders$population /10^6
total_gun_murders <- murders$total
plot(population_in_millions, total_gun_murders) #simple plot
hist(murders$rate) #simple histogram
boxplot(rate ~ region, data = murders) #simple boxplots or strata


#Section 3: Indexing, Data Wrangling, Plots/Section 3 Assessment
library(dslabs)
data(heights)
options(digits = 3)    # report 3 significant digits for all answers
ind <- filter(heights, height > mean(height))
nrow(ind)
sum(ind$sex == "Female")
ind <- heights$sex == "Female"
mean(ind)

min(heights$height)
match(min(heights$height),heights$height)
which.min(heights$height)
heights$sex[which.min(heights$height)]

max(heights$height)
x <- 50:82
sum(!(x %in% heights$height))

heights_2 <- mutate(heights, ht_cm = heights$height*2.54)
heights_2$ht_cm[18]
mean(heights_2$ht_cm)
sum(heights_2$sex == "Female")
mean(heights_2$ht_cm[heights_2$sex == "Female"])
mean(heights_2$ht_cm[heights_2$sex == "Male"])

library(dslabs)
data(olive)
head(olive)
plot(olive$palmitic, olive$palmitoleic)
hist(olive$eicosenoic)
boxplot(olive$palmitic~region, data = olive)




#Section 4


#Section 4: Programming Basics/4.2 Conditionals
a <- 0
if(a!=0){
  print(1/a)
} else{
  print("No reciprocal for 0.")
}

ind <- which.min(murder_rate)
if(murder_rate[ind] < 0.5){
  print(murders$state[ind])
} else{
  print("No state has a murder rate that low")
}

a <- 0
ifelse(a > 0, 1/a, NA)

a <- c(0,1,2,-4,5)
result <- ifelse(a > 0, 1/a, NA)
result

data(na_example) #Inside dslabs package
sum(is.na(na_example)) #The data has 145 NA inside
no_nas <- ifelse(is.na(na_example), 0, na_example) 
sum(is.na(no_nas))

# the any() and all() functions evaluate logical vectors
z <- c(TRUE, TRUE, FALSE)
any(z)
all(z)


#Section 4: Programming Basics/4.3 Functions
# example of defining a function to compute the average of a vector x
avg <- function(x){
  s <- sum(x)
  n <- length(x)
  s/n
}

# we see that the above function and the pre-built R mean() function are identical
x <- 1:100
identical(mean(x), avg(x))

# variables inside a function are not defined in the workspace
s <- 3
avg(1:10)
s

# the general form of a function
#my_function <- function(VARIABLE_NAME){
#  perform operations on VARIABLE_NAME and calculate VALUE
#  VALUE
#}

# functions can have multiple arguments as well as default values
avg <- function(x, arithmetic = TRUE){
  n <- length(x)
  ifelse(arithmetic, sum(x)/n, prod(x)^(1/n))
}


#Section 4: Programming Basics/4.4 For Loops
compute_s_n <- function(n){
  x <- 1:n
  sum(x)
}

for (i in 1:5){
  print(i)
}

m <- 25
s_n <- vector(length = 25)
for (i in 1:m){
  s_n[i] <- compute_s_n(i)
}
n <- 1:m
plot(n,s_n)
lines(n,n*(n+1)/2)


#Section 4: Programming Basics/4.4 For Loops/DataCamp Assessment 4.0 Programming Basics
# Assign the state abbreviation when the state name is longer than 8 characters 
new_names <- ifelse(nchar(murders$state) > 8, murders$abb, murders$state)

# Create `altman_plot` 
altman_plot <- function(x,y){
  plot(x+y,y-x)
}



#Section 4: Programming Basics/Section 4 Assessment (Verified Learners Only)
library(dslabs)
data(heights)
sx12 <- ifelse(heights$sex == "Female", 1, 2)
sum(sx12)

ht72 <- ifelse(heights$height <= 72, 0, heights$height)
mean(ht72)

inches_to_ft <- function(x){
  y <- x/12
  y
}
inches_to_ft(144)
htft <- inches_to_ft(heights$height)
htftTF <- ifelse(htft < 5, TRUE, FALSE)
sum(htftTF)
