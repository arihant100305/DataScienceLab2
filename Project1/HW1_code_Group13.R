install.packages("rbechmark")
library(rbenchmark)
install.packages("randtests")
library(randtests)
install.packages("Rcpp")
library(Rcpp)

#######################################################################################################################
#######################################################################################################################

#question - 1 : Estimation of e

#note as found by scientists the computed value of e available on internet is e = 2.71828182...... 

#method 1 

ans1 <- 0  
val_vec <- numeric(1000)

for (j in 1:1000) {
  
  # Create a vector of 10,000 random values, each transformed using log10
  vec <- numeric(length = 10000)
  for (i in 1:10000) {
    vec[i] <- log10(runif(1, 0, 1)) 
  }
  
  # Calculate the mean of the transformed values
  sum <- 0
  for (i in 1:10000) {
    sum <- sum + vec[i]  
  }
  sum <- sum / 10000
  
  # Calculate the intermediate value for this iteration
  val <- 1 / (10^sum)
  
  ans1 <- ans1 + val
  
  val_vec[j] <- val
}

# Finalize the estimate of e by averaging over all iterations
ans1 <- ans1 / 1000

cat("Estimated value of e:", ans1, "\n")

# Create a histogram to visualize the intermediate values
hist(
  val_vec, breaks = 30, col = "pink",
  main = "Distribution of Intermediate Values",
  xlab = "Intermediate Values (val)", ylab = "Frequency", border = "white"
)

# Add a red dashed line showing the mean of the intermediate values
abline(v = mean(val_vec), col = "red", lwd = 2, lty = 2)

################################################################################################################


#Method 2

n <- 1000000  
count_vec <- numeric(n)  # Vector to store counts from each iteration

for (j in 1:n) {
  
  count <- 0 
  sum <- 0 
  
  # Add random numbers until the sum exceeds 1
  while (sum <= 1) {
    count <- count + 1
    sum <- sum + runif(1, 0, 1)
  }
  
  # Store the count for this iteration
  count_vec[j] <- count
}

estimated_e <- mean(count_vec)


cat("Estimated value of e:", estimated_e, "\n")

# Visualization: Histogram of counts
library(ggplot2)

plot_data <- data.frame(Count = count_vec)

# Plot the histogram
ggplot(plot_data, aes(x = Count)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  geom_vline(xintercept = estimated_e, color = "red", linetype = "dashed", size = 1) +
  labs(
    title = "Visualization of Count Distribution in Estimating e",
    x = "Count (Number of Random Values Summed)",
    y = "Frequency"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5)
  ) +
  annotate("text", x = estimated_e + 1, y = max(table(count_vec)) * 0.9, 
           label = paste("Estimated e =", round(estimated_e, 5)), 
           color = "red", size = 4)
#The histogram usually has a peak near the integer closest to the true value of 
#e (~2.718).
#This indicates that most iterations required a number of random values close to e.


####################################################################################################

#Method 3
n <- 2^52  
#note we do utilise the concept of maximum order statistic as the greater the value of n that we chose , the closer to the 
#actual value of the e do we arrive . 

ans3 <- (1 + 1/n)^n
ans3  

# Drawing graph to visualize the convergence
vec <- 2^(1:52)  
y_val <- numeric(length = 52)  

# Compute values of (1 + 1/n)^n for each n
for (i in 1:52) {
  n <- vec[i]
  y_val[i] <- (1 + 1/n)^n
}

# Plot the convergence
plot(
  1:52, y_val, cex = 0.5, pch = 16,
  xlab = "Index (Power of 2 for n)", 
  ylab = "Value of (1 + 1/n)^n", 
  main = "Convergence of (1 + 1/n)^n to e",
  col = "blue"
)

# Add a horizontal line at the final estimated value
abline(h = y_val[52], col = "red", lty = 1, lwd = 0.5)

#The graph illustrates how the value of (1+1/n)^n converges to e as n increases.

########################################################################################

#Method 4

sampling <- function(n) {
  zero_wins_count <- 0  
  for (sim in 1:n) {
    results <- rbinom(n, size = 1, prob = 1 / n) #Bernoulli Trial
    if (sum(results) == 0) {
      zero_wins_count <- zero_wins_count + 1
    }
  }
  e <- n/zero_wins_count #The probability of zero wins = 1/e
  return(e)
}

options(digits = 15)

n <- 1e3
n_trials <- 1e2
estimated_e <- 0

for(i in 1:n_trials){
  estimated_e  <- estimated_e + sampling(n)
} #Using this loop to get a more accurate e by taking mean of all estimated values of e

cat("Estimated value of e using Bernoulli sampling:",  estimated_e/n_trials, "\n")
cat("Actual value of e :", exp(1))

#############################################################################################

#method 5

# Function to check if a permutation is a derangement
is_derangement <- function(permutation) {
  all(permutation != seq_along(permutation))
}

estimate_e <- function(n, sim) {
  derangement_count <- 0
  for (i in 1:sim) {
    shuffled_cards <- sample(1:n)
    # Check if it is a derangement
    if (is_derangement(shuffled_cards)) {
      derangement_count <- derangement_count + 1
    }
  }
  # Estimate of e
  estimated_e <- simulations / derangement_count
  return(estimated_e)
}
n <- 1000         # Number of cards
simulations <- 100000  # Number of simulations
estimated_e <- estimate_e(n, simulations)
cat("Estimated value of Euler's number (e):", estimated_e, "\n")

################################################################################################

#comparing the different methods based on their rate of convergence :

e <- exp(1)


method1<- function(n){
  del <- 0.1
  
  
  while(TRUE){
  ans1 <- 0  
  val_vec <- numeric(n)
  
  for (j in 1:n) {
    
    # Create a vector of 10,000 random values, each transformed using log10
    vec <- numeric(length = n)
    for (i in 1:n) {
      vec[i] <- log10(runif(1, 0, 1)) 
    }
    
    # Calculate the mean of the transformed values
    sum <- 0
    for (i in 1:n) {
      sum <- sum + vec[i]  
    }
    sum <- sum /n
    
    # Calculate the intermediate value for this iteration
    val <- 1 / (10^sum)
    
    ans1 <- ans1 + val
    
    val_vec[j] <- val
  }
  
  # Finalize the estimate of e by averaging over all iterations
  ans1 <- ans1 / n
  
  if( is.infinite(n^del * (ans1 - e) ) == TRUE  ){
    return(del)
  }else{
    del<-del+0.1
  }
  }
}

method2<- function(n ){
  del <- 0.1
  
  while(TRUE){
    
    count_vec <- numeric(n)  # Vector to store counts from each iteration
    
    for (j in 1:n) {
      
      count <- 0 
      sum <- 0 
      
      # Add random numbers until the sum exceeds 1
      while (sum <= 1) {
        count <- count + 1
        sum <- sum + runif(1, 0, 1)
      }
      
      # Store the count for this iteration
      count_vec[j] <- count
    }
    
    ans2 <- mean(count_vec)
    
    if( is.infinite(n^del * (ans2 - e) ) == TRUE  ){
      return(del)
    }else{
      del<-del+0.1
    }
  }
}

method4<- function(n ){
  del <- 0.1
  
  while(TRUE){
    
    estimated_e <- 0
    
    for(i in 1:n){
      estimated_e  <- estimated_e + sampling(n)
    }
    ans4<- estimated_e / n
    
    
    if( is.infinite(n^del * (ans4 - e) ) == TRUE  ){
      return(del)
    }else{
      del<-del+0.1
    }
  }
}

method5<- function(n){
  del <- 0.1
  
  while(TRUE){
    
    ans5 <- estimate_e(n, n)
    
    if( is.infinite(n^del * (ans5 - e) ) == TRUE  ){
      return(del)
    }else{
      del<-del+0.1
    }
  }
}

set.seed(192)

method1(100)
method2(100)
method4(100)
method5(100)

## method 1 , method 2 , and method 4 are giving same value of delta of 154.2 for n=100 whereas 
#method 5 is giving a delta of 152.5 ; hence we can conclude that the methods 1,2,4 have similar rate 
#of convergence whereas the method 5 has lower rate of convergence than them . 





################################################################################################
##################################################

#Question 2 - Estimation of pi
#note as found by scientists the computed value of pi available on internet is pi = 3.141592653...... 

#method 1 - (2 dimensional)


n <-1000000

x <- runif(n , -1 , 1)
y <- runif(n , -1 , 1)

dis<- x^2 + y^2
dis


dis[dis <= 1] <- 1 
dis[dis > 1] <- 0
dis

p<-sum(dis)/n
p*4

###################################################################################################


#method 2 - (3 dimensional)


n<-1000000

x <- runif(n , -1 , 1)
y <- runif(n , -1 , 1)
z <- runif(n , -1 , 1)

dis<- x^2 + y^2 + z^2
dis


dis[dis <= 1] <- 1 
dis[dis > 1] <- 0
dis

p<-sum(dis)/n
p*6

##############################################################################################

#method 3 - (n dimensional where n is even) 


n<-100000000

dis<- numeric(length = n) #num of dimensions (max keep 20 otherwise would have memory issues because we would need to make 
# size of vector n larger because otherwise sum(dist) would become like around 5 or so and if it varies even a little value of
# pi we are estimating will change by a lot)

d<- 14    #num of dimensions

for(i in 1:d){
  x_i <- runif(n , -1 , 1)
  dis<- dis + x_i^2
}

dis[dis <= 1] <- 1 
dis[dis > 1] <- 0

p<-sum(dis)/n

pi<- 4*( (p*factorial(d/2) )^(2/d) )
pi

##############################################################################################

#method 4 - (n dimensional where n is odd)


n<-100000000

dis<- numeric(length = n)

d<- 15    #num of dimensions (max keep 19 otherwise would have memory issues because we would need to make 
# size of vector n larger because otherwise sum(dist) would become like around 5 or so and if it varies even a little value of
# pi we are estimating will change by a lot)


for(i in 1:d){
  x_i <- runif(n , -1 , 1)
  dis<- dis + x_i^2
}

dis[dis <= 1] <- 1 
dis[dis > 1] <- 0

p<-sum(dis)/n

fac <- 1

for(j in 1:((d-1)/2)){
  fac<- fac*(2*j+1)
}

pi<- 2*( (p*fac)^(2/(d-1)) )
pi

### generating graph by plotting the corresponding value that we achieve from different dimensions 2 to 20

even <- function(d) {
  n<-100000000
  
  dis<- numeric(length = n) 
  
  for(i in 1:d){
    x_i <- runif(n , -1 , 1)
    dis<- dis + x_i^2
  }
  
  dis[dis <= 1] <- 1 
  dis[dis > 1] <- 0
  
  p<-sum(dis)/n
  
  pi<- 4*( (p*factorial(d/2) )^(2/d) )
  return(pi)
}


odd <- function(d) {
  
  n<-100000000
  
  dis<- numeric(length = n)
  
  for(i in 1:d){
    x_i <- runif(n , -1 , 1)
    dis<- dis + x_i^2
  }
  
  dis[dis <= 1] <- 1 
  dis[dis > 1] <- 0
  
  p<-sum(dis)/n
  
  fac <- 1
  
  for(j in 1:((d-1)/2)){
    fac<- fac*(2*j+1)
  }
  
  pi<- 2*( (p*fac)^(2/(d-1)) )
  
  return(pi)
}



dim<- 2:20
pi_val <- numeric(length = 19)

for(i in 2:20){
  
  if(i%%2==0){
    pi_val[i-1] <- even(i)
  }else{
    pi_val[i-1] <- odd(i)
  }
  
}

pi_val

plot(dim, pi_val, type = "o", ylim = c(2.8, 3.5), col = "black", main = "graph of dimension vs the estimated value of pi " ,pch =16)
abline(h = 3.141592653, col = "red", lwd = 1, lty = 1)

# we note that for the first few dim approx(10-11) the estimated value of pi is close to the actual value of pi but afterwards 
#it deviates even though we were expecting that as dimensions increases the estimated value would converge to the actual value of
#pi , this happens so because we cant increase the number of data points n beyond a certain limit due to memory constraints and
# time limit constraints , because of which sum(dis) that is the points within the n - ball which we were calculating 
#becomes significantly less say around 5 or below due to which even slight variations in it causes significant changes in 
#the estimated value of pi

# to overcome this shortcoming we can make a approach in which memory required is 0(1) instead of o(n) , however it will 
#result in further increase in time complexity as we would make use of nested loops , and also increase the value of n.
# doing so will require the use of a supercomputer , as otherwise on a normal laptop it would take a lot of time because even this
# final code of plotting graph took around 15 mins so the other mentioned approch would take more than 150 mins at the very 
#least because of increasing the value of n by a factor of 10 . 

##############################################################################################

# let us try try doing that for one value of dim ,say d = 20
#method 5 - without using memory approach


n<-10000000

d<- 18

distance <- function(d) {
  
  dis<- 0
  for(i in 1:d){
    x_i <- runif(1 , -1 , 1)
    dis<- dis + x_i^2 
  }
  
  if(dis>1){
    return(0)
  }else{
    return(1)
  }
  
}

sum <- 0


for(i in 1:n){
  sum <- sum+ distance(d)
}


p<-sum/n

pi<- 4*( (p*factorial(d/2) )^(2/d) )
pi

## this takes a huge amount of time to run , and hence is 
# not a very good method unless doing on a supercomputer

###################################################################################################

#method 6 - can try using using rcpp package to reduce the time complexity of method 5

cppFunction('int summing(int n, int d) {

  int summ = 0 ; 
  
  for(int i=0 ; i<n ; i++){
  
  float dis = 0;
  int val = 1 ;
  
  for(int j=0 ; j<d ; j++){
    float x_i = R::runif(-1 , 1) ;
    dis= dis + pow(x_i, 2); ;
  }
  
  if(dis>1){
    val = 0 ;
  }
  
  summ = summ+val;
}
  return summ;
}')

n<-1000000000

d<- 18

p<-summing(n, d)/n  

pi<- 4*( (p*factorial(d/2) )^(2/d) )
pi

# this method is clearly much faster than using R but still it takes decent amount of time to extend n beyond 20

#################################################################################################################

#method 7 

pi
n<-1000000

#note that the following is not a cyclic argument as we can generate uniform values of degrees in 0 to 90 and take cos(degree)
# and taking cos of something does not utilise the value of pi anywhere 
#also this method involves statistics as we are generating the values of degrees at random from the runif function
vec <- runif(n , 0 , pi/2)
arr <- cos(vec)

pi_est <- 2/mean(arr)
pi_est

########################################################################################################################
########################################################################################################################


#question 3 - 

set.seed(123)
data <- rnorm(100, mean = 0, sd = 1)

# visualizing the data set generated

summary(data)

# Histogram
hist(data,  col = "lightblue", main = "Histogram of Generated Data",
     xlab = "Value", ylab = "Frequency")

# Density plot
plot(density(data), col = "red", lwd = 2, main = "Density Plot of Generated Data")


###################################################################################################

#method1 - using random runs test


runs.test(data)     

# Since the p-value is coming out to be greater than 0.05 we fail to reject the Null hypothesis and hence the data we 
# generated is evidently random (at least with the number of datapoints that we supplied)

###################################################################################################

#method 2 - autocorrelation test

acf(data, main = "Autocorrelation of Random Data")

#since the value of acf function at all lags except the first are within the confidence levels(-0.2,0.2) hence we fail 
#to reject the null hypothesis and therefore the data is random.

###################################################################################################

#method 3 - Ljung-Box Test

Box.test(data, lag=10, type="Ljung-Box")

#since the p-value is greater than 0.05 we fail to reject the null hypothesis
#and therefore the data is random.

###################################################################################################

#method 4 - Kolmogorov-Smirnov Test

ks_test_result <- ks.test(data, "pnorm", mean = 0, sd = 1)
print(ks_test_result)

#since the p-value is greater than 0.05 we fail to reject the null hypothesis
#and therefore the data is random.

##########################################################################
## method 5( random run tests - manually written code) - 1

# Generate 100 observations from a normal distribution 
data = rnorm(100, mean = 0, sd = 1)

# Function to perform the runs test
runs_test = function(data) {
  # Convert data into a sequence of "+" (above mean) and "-" (below mean)
  signs = ifelse(data > median(data), "+", "-")
  
  # Count the number of runs
  runs = 1
  for (i in 2:length(signs)) {
    if (signs[i] != signs[i - 1]) {
      runs = runs + 1
    }
  }
  
  # Calculate the expected number of runs and variance under null hypothesis
  n1 = sum(signs == "+")
  n2 = sum(signs == "-")
  N = n1 + n2
  expected_runs = 1 + (2 * n1 * n2) / N
  variance_runs = (2 * n1 * n2 * (2 * n1 * n2 - N)) / (N^2 * (N - 1))
  
  # Compute z-score
  z = (runs - expected_runs) / sqrt(variance_runs)
  
  # P-value from the z-score
  p_value = 2 * pnorm(-abs(z))  # Two-tailed test
  
  return(list(
    observed_runs = runs,
    expected_runs = expected_runs,
    z_score = z,
    p_value = p_value
  ))
}

# Perform the Runs Test
result = runs_test(data)

# Display results
cat("Observed Runs:", result$observed_runs, "\n")
cat("Expected Runs:", result$expected_runs, "\n")
cat("Z-Score:", result$z_score, "\n")
cat("P-Value:", result$p_value, "\n")

# Conclusion
if (result$p_value < 0.05) {
  cat("Conclusion: Reject the null hypothesis. The data is not random.\n")
} else {
  cat("Conclusion: Fail to reject the null hypothesis. The data appears random.\n")
}


##########################################################################
##method5 - 2(random run test using recursion)

recursive_runs_test = function(data, depth = 5) {
  if (depth == 0 || length(data) < 2) {
    # Base case: stop recursion if depth is 0 or data is too small
    return(NULL)
  }
  
  # Step 1: Calculate the median of the data
  data_median = median(data)
  
  # Step 2: Assign "+" and "-" based on median
  signs = ifelse(data > data_median, "+", "-")
  
  # Step 3: Perform the runs test
  runs = 1
  for (i in 2:length(signs)) {
    if (signs[i] != signs[i - 1]) {
      runs = runs + 1
    }
  }
  
  n1 = sum(signs == "+")
  n2 = sum(signs == "-")
  N = n1 + n2
  
  if (n1 == 0 || n2 == 0) {
    cat("Data is not random at this level (all points on one side of median).\n")
    return(NULL)
  }
  
  expected_runs = 1 + (2 * n1 * n2) / N
  variance_runs = (2 * n1 * n2 * (2 * n1 * n2 - N)) / (N^2 * (N - 1))
  z = (runs - expected_runs) / sqrt(variance_runs)
  p_value = 2 * pnorm(-abs(z))
  
  cat("Depth:", depth, "\n")
  cat("Observed Runs:", runs, "\n")
  cat("Expected Runs:", expected_runs, "\n")
  cat("Z-Score:", z, "\n")
  cat("P-Value:", p_value, "\n")
  
  if (p_value < 0.05) {
    cat("Conclusion: Data is not random at this level.\n")
  } else {
    cat("Conclusion: Data appears random at this level.\n")
  }
  cat("\n")
  
  # Step 4: Recursively analyze the "+" and "-" subsets
  recursive_runs_test(data[data > data_median], depth - 1)
  recursive_runs_test(data[data < data_median], depth - 1)
}

# Generate 100 observations from a normal distribution
set.seed(123)
data = rnorm(100, mean = 0, sd = 1)

# Run the recursive runs test
recursive_runs_test(data, depth = 5)