
install.packages("MASS")   # Needed for generating multivariate normal data
install.packages("ks")     # For 2D KDE estimation
library(MASS)
library(ks)

###############################################################################################
#Question 1 
#Generate data from bivariate unimodal and bimodal distributions and
#draw contour plots of them. From those contour plots, detect the number of
#modes of those data

#METHOD1

#unimodal when variances of X and Y are equal in bivariate normal dist

set.seed(42)  # For reproducibility
n <- 10^4  # Number of samples

# Mean vector and identity covariance matrix
mu <- c(0, 0)
sigma <- matrix(c(1, 0, 0, 1), nrow=2)  # Identity covariance matrix

# Generate data
data <- mvrnorm(n, mu, sigma)  # Generates (X, Y) pairs
x <- data[,1]  # Extract X values
y <- data[,2]  # Extract Y values


kde_est <- kde(x = data)  # Perform 2D KDE
kde_est <- kde(data, gridsize = c(300, 300))


evalu<-kde_est$eval.points
evalu
x_eval<-evalu[1]
y_eval<-evalu[2]
val<- max(kde_est$estimate)
val
kde_est$estimate
density_matrix <- kde_est$estimate
dim(density_matrix) 
options(max.print = 100000)  # Increase limit to 100,000 values
print(density_matrix)

contour_levels <- kde_est$cont
print(contour_levels)

# Contour Plot based on density percentile point capture
plot(kde_est, main="2D Kernel Density Estimation (Bivariate Normal)")

# Perspective 3D Plot
persp(kde_est$eval.points[[1]], kde_est$eval.points[[2]], kde_est$estimate, 
      theta=30, phi=30, col="lightblue", shade=0.5, border=NA,
      xlab="X", ylab="Y", zlab="f(x,y)", main="3D Kernel Density Estimate")


x_eval <- unlist(x_eval)  # Convert list to numeric vector
y_eval <- unlist(y_eval)





thresh_err <- 0.001

plot_density_points <- function(density_matrix, x_eval, y_eval, val, err) {
  
  # Find indices where density is within the given range
  matches <- which(abs(density_matrix - val) < err, arr.ind = TRUE)
  
  # Extract corresponding x and y values
  x_match <- x_eval[matches[, 1]]  # Get x-values from first index
  y_match <- y_eval[matches[, 2]]  # Get y-values from second index
  
  # Plot the selected points
  plot(x_match, y_match, col = "red", pch = 16, xlab = "X", ylab = "Y",
       main ="countour plots")
  
  # Add grid for better visualization
  grid()
}


plot_density_points(density_matrix, x_eval, y_eval, 0.01, thresh_err)



add_density_points <- function(density_matrix, x_eval, y_eval, val, err,co) {
  
  # Find indices where density is within the given range
  matches <- which(abs(density_matrix - val) < err, arr.ind = TRUE)
  
  # Extract corresponding x and y values
  x_match <- x_eval[matches[, 1]]  # Get x-values from first index
  y_match <- y_eval[matches[, 2]]  # Get y-values from second index
  
  # Plot the selected points
  points(x_match, y_match, col=co, pch = 16)
  
  # Add grid for better visualization
  grid()
}

my_colors <- colorRampPalette(c("red", "yellow", "green"))(16)

for(i in 2:16){
add_density_points(density_matrix, x_eval, y_eval, 0.01*i, thresh_err,my_colors[i])
}

#as we can observe we get approx circular contour plots centered at (0.0)

#####################################################################################################


# unimodal when variances of X and Y differ in bivariate normal dist


set.seed(42)  # For reproducibility
n <- 10^4  # Number of samples

# Mean vector and identity covariance matrix
mu <- c(0, 0)
sigma <- matrix(c(1, 0, 0, 4), nrow = 2)  # Identity covariance matrix

# Generate data
data <- mvrnorm(n, mu, sigma)  # Generates (X, Y) pairs
x <- data[,1]  # Extract X values
y <- data[,2]  # Extract Y values


kde_est <- kde(x = data)  # Perform 2D KDE
kde_est <- kde(data, gridsize = c(300, 300))

evalu<-kde_est$eval.points
evalu
x_eval<-evalu[1]
y_eval<-evalu[2]
val<- max(kde_est$estimate)
val
kde_est$estimate
density_matrix <- kde_est$estimate
dim(density_matrix) 
options(max.print = 100000)  # Increase limit to 100,000 values
print(density_matrix)

contour_levels <- kde_est$cont
print(contour_levels)
# Contour Plot based on density percentile point capture
plot(kde_est, main="2D Kernel Density Estimation (Bivariate Normal)")

# Perspective 3D Plot
persp(kde_est$eval.points[[1]], kde_est$eval.points[[2]], kde_est$estimate, 
      theta=30, phi=30, col="lightblue", shade=0.5, border=NA,
      xlab="X", ylab="Y", zlab="f(x,y)", main="3D Kernel Density Estimate")


x_eval <- unlist(x_eval)  # Convert list to numeric vector
y_eval <- unlist(y_eval)


thresh_err <- 0.002

plot_density_points <- function(density_matrix, x_eval, y_eval, val, err) {
  
  # Find indices where density is within the given range
  matches <- which(abs(density_matrix - val) < err, arr.ind = TRUE)
  
  # Extract corresponding x and y values
  x_match <- x_eval[matches[, 1]]  # Get x-values from first index
  y_match <- y_eval[matches[, 2]]  # Get y-values from second index
  
  # Plot the selected points
  plot(x_match, y_match, col = "red", pch = 16, xlab = "X", ylab = "Y",
       main ="countour plots")
  
  # Add grid for better visualization
  grid()
}


plot_density_points(density_matrix, x_eval, y_eval, 0.01, thresh_err)



add_density_points <- function(density_matrix, x_eval, y_eval, val, err,co) {
  
  # Find indices where density is within the given range
  matches <- which(abs(density_matrix - val) < err, arr.ind = TRUE)
  
  # Extract corresponding x and y values
  x_match <- x_eval[matches[, 1]]  # Get x-values from first index
  y_match <- y_eval[matches[, 2]]  # Get y-values from second index
  
  # Plot the selected points
  points(x_match, y_match, col=co, pch = 16)
  
  # Add grid for better visualization
  grid()
}

my_colors <- colorRampPalette(c("red", "yellow", "green"))(8)

for(i in 2:8){
  add_density_points(density_matrix, x_eval, y_eval, 0.01*i, thresh_err,my_colors[i])
}

#as we can observe we get approx ellipse countour plots centered at (0.0)

###################################################################################################



# unimodal when variances of X and Y differ in bivariate normal dist and correlation exists and 
# X and Y not centered at 0,0


set.seed(42)  # For reproducibility
n <- 10^4  # Number of samples

# Mean vector and identity covariance matrix
mu <- c(20, 30)
corr <- 0.7
sigma <- matrix(c(1, corr, corr, 4), nrow=2)  # Identity covariance matrix

# Generate data
data <- mvrnorm(n, mu, sigma)  # Generates (X, Y) pairs
x <- data[,1]  # Extract X values
y <- data[,2]  # Extract Y values


kde_est <- kde(x = data)  # Perform 2D KDE
kde_est <- kde(data, gridsize = c(300, 300))

evalu<-kde_est$eval.points
evalu
x_eval<-evalu[1]
y_eval<-evalu[2]
val<- max(kde_est$estimate)
val
kde_est$estimate
density_matrix <- kde_est$estimate
dim(density_matrix) 
options(max.print = 100000)  # Increase limit to 100,000 values
print(density_matrix)

contour_levels <- kde_est$cont
print(contour_levels)
# Contour Plot based on density percentile point capture
plot(kde_est, main="2D Kernel Density Estimation (Bivariate Normal)")

# Perspective 3D Plot
persp(kde_est$eval.points[[1]], kde_est$eval.points[[2]], kde_est$estimate, 
      theta=30, phi=30, col="lightblue", shade=0.5, border=NA,
      xlab="X", ylab="Y", zlab="f(x,y)", main="3D Kernel Density Estimate")


x_eval <- unlist(x_eval)  # Convert list to numeric vector
y_eval <- unlist(y_eval)


thresh_err <- 0.002

plot_density_points <- function(density_matrix, x_eval, y_eval, val, err) {
  
  # Find indices where density is within the given range
  matches <- which(abs(density_matrix - val) < err, arr.ind = TRUE)
  
  # Extract corresponding x and y values
  x_match <- x_eval[matches[, 1]]  # Get x-values from first index
  y_match <- y_eval[matches[, 2]]  # Get y-values from second index
  
  # Plot the selected points
  plot(x_match, y_match, col = "red", pch = 16, xlab = "X", ylab = "Y",
       main ="countour plots")
  
  # Add grid for better visualization
  grid()
}


plot_density_points(density_matrix, x_eval, y_eval, 0.01, thresh_err)



add_density_points <- function(density_matrix, x_eval, y_eval, val, err,co) {
  
  # Find indices where density is within the given range
  matches <- which(abs(density_matrix - val) < err, arr.ind = TRUE)
  
  # Extract corresponding x and y values
  x_match <- x_eval[matches[, 1]]  # Get x-values from first index
  y_match <- y_eval[matches[, 2]]  # Get y-values from second index
  
  # Plot the selected points
  points(x_match, y_match, col=co, pch = 16)
  
  # Add grid for better visualization
  grid()
}

my_colors <- colorRampPalette(c("red", "yellow", "green"))(8)

for(i in 2:8){
  add_density_points(density_matrix, x_eval, y_eval, 0.01*i, thresh_err,my_colors[i])
}

#as we can observe we get approx ellipse centered at approx(20,30) , with its axis tilted
# as countour plots

###################################################################################################



# bimodal in bivariate normal dist 
#considering dist as -> 0.7*bivariate normal centered at (0,0) + 0.3*bivariate normal centered at (10,10)


set.seed(42)  # For reproducibility
n <- 10^4  # Number of samples

# Mean vector and identity covariance matrix
mu1<-c(0,0)
mu2 <- c(10, 10)
corr <- 0
sigma1 <- matrix(c(1, corr, corr, 1), nrow=2)  # Identity covariance matrix
sigma2 <- matrix(c(1, corr, corr, 1), nrow=2) 

# Generate data
data<- matrix(0, nrow = n, ncol = 2)

for(i in 1:n){
  u<-runif(1,0,1)
  if(u<=0.7){
    data[i,]<- mvrnorm(1, mu1, sigma1) 
  }else{
    data[i,]<- mvrnorm(1, mu2, sigma2) 
  }
}


x <- data[,1]  # Extract X values
y <- data[,2]  # Extract Y values


kde_est <- kde(x = data)  # Perform 2D KDE
kde_est <- kde(data, gridsize = c(300, 300))

evalu<-kde_est$eval.points

x_eval<-evalu[1]
y_eval<-evalu[2]

val<- max(kde_est$estimate)

density_matrix <- kde_est$estimate
dim(density_matrix) 
options(max.print = 100000)  # Increase limit to 100,000 values
print(density_matrix)


# Perspective 3D Plot
persp(kde_est$eval.points[[1]], kde_est$eval.points[[2]], kde_est$estimate, 
      theta=30, phi=30, col="lightblue", shade=0.5, border=NA,
      xlab="X", ylab="Y", zlab="f(x,y)", main="3D Kernel Density Estimate")


x_eval <- unlist(x_eval)  # Convert list to numeric vector
y_eval <- unlist(y_eval)


thresh_err <- 0.002

plot_density_points <- function(density_matrix, x_eval, y_eval, val, err) {
  
  # Find indices where density is within the given range
  matches <- which(abs(density_matrix - val) < err, arr.ind = TRUE)
  
  # Extract corresponding x and y values
  x_match <- x_eval[matches[, 1]]  # Get x-values from first index
  y_match <- y_eval[matches[, 2]]  # Get y-values from second index
  
  # Plot the selected points
  plot(x_match, y_match, col = "red", pch = 16, xlab = "X", ylab = "Y",
       main ="countour plots")
  
  # Add grid for better visualization
  grid()
}


plot_density_points(density_matrix, x_eval, y_eval, 0.01, thresh_err)



add_density_points <- function(density_matrix, x_eval, y_eval, val, err,co) {
  
  # Find indices where density is within the given range
  matches <- which(abs(density_matrix - val) < err, arr.ind = TRUE)
  
  # Extract corresponding x and y values
  x_match <- x_eval[matches[, 1]]  # Get x-values from first index
  y_match <- y_eval[matches[, 2]]  # Get y-values from second index
  
  # Plot the selected points
  points(x_match, y_match, col=co, pch = 16 , cex = 0.5)
  
  # Add grid for better visualization
  grid()
}

my_colors <- colorRampPalette(c("red", "yellow", "green"))(10)

for(i in 2:10){
  add_density_points(density_matrix, x_eval, y_eval, 0.01*i, thresh_err,my_colors[i])
}

#########################################################
#METHOD 2
library(MASS)
library(mvtnorm)
set.seed(123)

n <- 100  # Sample size

# Kernel Density Estimation Function
kde_biv <- function(grid_x, grid_y, data, bandwidth) {
  # Create grid
  grid_points <- expand.grid(grid_x, grid_y)
  
  # Initialize kde matrix
  kde_values <- numeric(nrow(grid_points))
  
  # Compute KDE using a loop over grid points
  for (i in 1:nrow(grid_points)) {
    kde_values[i] <- sum(dmvnorm(data, mean = as.numeric(grid_points[i, ]), sigma = bandwidth * diag(2)))
  }
  
  # Normalize KDE
  kde_values <- kde_values / (n * bandwidth)
  
  # Convert to matrix
  list(x = grid_x, y = grid_y, z = matrix(kde_values, nrow = length(grid_x), byrow = TRUE))
}

# Function to plot contours
plot_kde_contour <- function(data, bandwidth, title, num_modes) {
  x_vals <- seq(min(data[,1]), max(data[,1]), length.out = 100)
  y_vals <- seq(min(data[,2]), max(data[,2]), length.out = 100)
  
  kde_result <- kde_biv(x_vals, y_vals, data, bandwidth)
  
  contour(kde_result$x, kde_result$y, kde_result$z, 
          main = title, col = c("blue", "red", "green"), lwd = 2, asp = 1)
  
  cat("Number of modes in", title, ":", num_modes, "\n")
}

# Generate Bivariate Normal Data
mu <- c(0, 0)
sigma <- matrix(c(1, 0, 0, 1), nrow = 2)
data <- mvrnorm(n, mu = mu, Sigma = sigma)

# Plot KDE
plot_kde_contour(data, bandwidth = 0.5, title = "Unimodal Distribution (Centered at 0,0)", num_modes = 1)



# Unimodal Distribution 1
mu <- c(0,0)
sigma <- matrix(c(1,0,0,1), nrow = 2)
data <- mvrnorm(n, mu = mu, Sigma = sigma)
plot_kde_contour(data, bandwidth = 5, title = "Unimodal Distribution (Centered at 0,0)", num_modes = 1)

# Unimodal Distribution 2
mu <- c(6,9)
data <- mvrnorm(n, mu = mu, Sigma = sigma)
plot_kde_contour(data, bandwidth = 5, title = "Unimodal Distribution (Shifted to 6,9)", num_modes = 1)


# Unimodal Distribution 3 (With Covariance)
mu <- c(2,3)
cov <- 0.5
sigma <- matrix(c(1, cov, cov, 1), nrow = 2)
data <- mvrnorm(n, mu = mu, Sigma = sigma)
plot_kde_contour(data, bandwidth = 5, title = "Unimodal Distribution (With Covariance)", num_modes = 1)

# Bimodal Distribution 1
mu1 <- c(0,0)
mu2 <- c(3,4)
data <- rbind(mvrnorm(n/2, mu = mu1, Sigma = sigma),
              mvrnorm(n/2, mu = mu2, Sigma = sigma))
plot_kde_contour(data, bandwidth = 5, title = "Bimodal Distribution", num_modes = 2)

# Bimodal Distribution 2 (With Different Variance)
sigma <- matrix(c(2, cov, cov, 2), nrow = 2)
data <- rbind(mvrnorm(n/2, mu = mu1, Sigma = sigma),
              mvrnorm(n/2, mu = mu2, Sigma = sigma))
plot_kde_contour(data, bandwidth = 5, title = "Bimodal Distribution (Higher Variance)", num_modes = 2)


#Note zoom the plot to see distinguished countours

#as we can observe we get two different approx circles cenetered at (0,0) and (10,10) ; if we note 
# carefully we can see that on increasing the height of plane of countour cut the circle centered 
# at (10,10) vanishes after a time because we can see red and orange countours for it but not green and 
# yellow ones which can be seen for the circle at (0,0) ; which makes sense as height of (10,10) peak is 
#lesser than the one ceneterd at (0,0) which can also be seen from the 3-D plot

# Note that both the density centers are quite far away that is 0,0 and 10,10 are quite far apart that 
#the densities fx1 and fx2 do not affect each other . 


#Using real dataset to plot contours
#Unimodal data

library(MASS)
data(airquality)

# Remove missing values
airquality <- na.omit(airquality)

# Standardize Wind and Temperature variables
airquality_std <- scale(airquality[, c("Wind", "Temp")])

# Define the KDE function
kde_biv <- function(grid_x, grid_y, data, bandwidth) {
  n <- nrow(data)
  kde_values <- matrix(0, nrow = length(grid_x), ncol = length(grid_y))
  
  for (i in 1:length(grid_x)) {
    for (j in 1:length(grid_y)) {
      x_point <- c(grid_x[i], grid_y[j])
      kernel_values <- apply((data - x_point) / bandwidth, 1, function(x) exp(-sum(x^2) / 2))
      kde_values[i, j] <- sum(kernel_values) / (n * bandwidth^2 * 2 * pi)
    }
  }
  
  list(x = grid_x, y = grid_y, z = kde_values)
}

# Function to count modes in KDE
count_modes <- function(kde_result) {
  z_matrix <- kde_result$z
  n_modes <- 0
  
  for (i in 2:(nrow(z_matrix) - 1)) {
    for (j in 2:(ncol(z_matrix) - 1)) {
      center <- z_matrix[i, j]
      neighbors <- c(
        z_matrix[i-1, j], z_matrix[i+1, j], 
        z_matrix[i, j-1], z_matrix[i, j+1],
        z_matrix[i-1, j-1], z_matrix[i-1, j+1], 
        z_matrix[i+1, j-1], z_matrix[i+1, j+1]
      )
      
      if (all(center > neighbors)) {
        n_modes <- n_modes + 1
      }
    }
  }
  
  return(n_modes)
}

# Function to plot KDE contour
plot_kde_contour <- function(data, bandwidth, title) {
  x_vals <- seq(min(data[,1]), max(data[,1]), length.out = 100)
  y_vals <- seq(min(data[,2]), max(data[,2]), length.out = 100)
  
  kde_result <- kde_biv(x_vals, y_vals, data, bandwidth)
  
  contour(kde_result$x, kde_result$y, kde_result$z, 
          main = title, col = terrain.colors(10), lwd = 2,asp = 1)
  
  
  
  num_modes <- count_modes(kde_result)
  cat("Number of modes in", title, ":", num_modes, "\n")
}

# Convert to matrix
data_matrix <- as.matrix(airquality_std)

# Plot contour and count modes
plot_kde_contour(data_matrix, bandwidth = 0.5, title = "KDE Contour: Wind vs Temperature")


#here the number of modes is 1




# 2.Bimodal data

install.packages("MASS")   # Needed for generating multivariate normal data
install.packages("ks")     # For 2D KDE estimation
library(MASS)
library(ks)
library(readr)

df <- read_csv("weight-height.csv")

df

data<- matrix(0, nrow = 10000, ncol = 2)

data[,1] <- df$Height
data[,2] <- df$Weight 

data


kde_est <- kde(x = data)  # Perform 2D KDE
kde_est <- kde(data, gridsize = c(300, 300))

evalu<-kde_est$eval.points
evalu
x_eval<-evalu[1]
y_eval<-evalu[2]
val<- max(kde_est$estimate)
val
kde_est$estimate
density_matrix <- kde_est$estimate
dim(density_matrix) 
options(max.print = 100000)  # Increase limit to 100,000 values
print(density_matrix)


contour_levels <- kde_est$cont
print(contour_levels)

# Perspective 3D Plot
persp(kde_est$eval.points[[1]], kde_est$eval.points[[2]], kde_est$estimate, 
      theta=30, phi=30, col="lightblue", shade=0.5, border=NA,
      xlab="X", ylab="Y", zlab="f(x,y)", main="3D Kernel Density Estimate")


x_eval <- unlist(x_eval)  # Convert list to numeric vector
y_eval <- unlist(y_eval)

selected_levels <- quantile(kde_est$cont, probs = seq(0.1, 0.9, by = 0.1))

contour(kde_est$eval.points[[1]], kde_est$eval.points[[2]], kde_est$estimate, 
        levels = selected_levels,  
        col = c("blue", "red", "green"), lwd = 2)


# we can observe the two modes appearing for male and female respectively , however since the peaks are very 
# close hence they are a bit merged and not completely separated

#also since there is positive correlation between weight and in general as can be expected hence we can 
#see that the ellipse is slanted



###################################################################################################
#QUESTION 2

# function for the integral
# Theoritically, the value of the integral is (355/113 - pi) ~ 10^-5. So, we can use this integral for the estimation of pi.
f = function(x){
  return((x^8 * (1 - x)^8 * (25 + 816 * x^2)) / (3164 * (1 + x^2)))
}
# Monte Carlo estimation of the integral(using weak law of large number, sample average converges to the expectation of the function)
n = 1e3
x_samples = runif(n,0,1) # Generate random samples from U(0,1)
estimated_integral = mean(f(x_samples))

cat("Estimated value of the integral:", estimated_integral, "\n")
#the estimated integral value is close to zero


#Checking the rate of convergence using delta method:-
integral_estimator <- function(n) {
  x <- runif(n, 0, 1)  
  fx <- (x^8 * (1 - x)^8 * (25 + 816 * x^2)) / (3164 * (1 + x^2))  # Compute function values
  mean(fx)  # Monte Carlo estimate
}

rate_of_convergence <- function(n) {
  del <- 0.1
  
  while(TRUE) {
    est <- integral_estimator(n)  # Estimate the integral
    
    if (is.infinite(n^del * abs(est))) {
      return(del)
    } else {
      del <- del + 0.1
    }
  }
}

# Compute rate of convergence for different values of n
n_values <- c(100, 1000, 10000, 100000)
delta_values <- sapply(n_values, rate_of_convergence)

# Print results
results <- data.frame(n = n_values, delta = delta_values)
print(results)


library(ggplot2)
estimate_pi_1 <- function(n) {
  355/113 - integral_estimator(n)
}

n_values <- seq(100, 10000, by = 100)
pi_estimates <- sapply(n_values, estimate_pi_1)
# Plot convergence of pi estimates
pi_df <- data.frame(n = n_values, pi_est = pi_estimates)
ggplot(pi_df, aes(x = n, y = pi_est)) +
  geom_line(color = "blue") +
  geom_hline(yintercept = pi, linetype = "dashed", color = "red") +
  labs(title = "Convergence of Monte Carlo Estimation of ",
       x = "Number of Samples (n)",
       y = "Estimated ") +
  theme_minimal()


# Comparision with estimator 2: pi_2 = mean(4/(1 + X^2)) 
# Function for estimator 1: pi_1 = 355/113 - I_hat
estimate_pi_1 <- function(n) {
  355/113 - integral_estimator(n)
}

# Function for estimator 2: pi_2 = mean(4/(1 + X^2))
estimate_pi_2 <- function(n) {
  x <- runif(n, 0, 1)
  mean(4 / (1 + x^2))
}

# Function to compute variance for an estimator
compute_variance <- function(n, estimator_func, reps = 100) {
  estimates <- replicate(reps, estimator_func(n))
  var(estimates)  # Empirical variance of the estimator
}

# Sample sizes
n_values <- c(100, 1000, 10000)

# Compute variances
variance_pi1 <- sapply(n_values, function(n) compute_variance(n, estimate_pi_1))
variance_pi2 <- sapply(n_values, function(n) compute_variance(n, estimate_pi_2))

# Store results in a data frame
results <- data.frame(
  n = n_values,
  Variance_Estimator1 = variance_pi1,
  Variance_Estimator2 = variance_pi2,
  Ratio = variance_pi1 / variance_pi2  # Ratio to compare efficiency
)

print(results)


# Comparision with estimation with unit circle method:-
# Estimation of π using the unit circle method
estimate_pi_standard <- function(N) {
  x <- runif(N, -1, 1)
  y <- runif(N, -1, 1)
  mean(x^2 + y^2 <= 1) * 4  # Ratio of points inside circle * 4
}

# Estimation of pi using that integral.
# Run Monte Carlo for different sample sizes
N_vals <- c(1000, 10000, 100000)  # Different sample sizes
results <- data.frame(N = N_vals, Integral_Estimate = NA, Pi_Estimate = NA, Pi_Standard = NA)

for (i in seq_along(N_vals)) {
  N <- N_vals[i]
  I_hat <- integral_estimator(N)  # Compute integral estimate
  pi_corrected <- 355 / 113 - I_hat  # Compute corrected π estimate
  pi_standard <- estimate_pi_standard(N)  # Standard Monte Carlo π estimate
  
  results[i, 2] <- I_hat
  results[i, 3] <- pi_corrected
  results[i, 4] <- pi_standard
}

# Print results
print(results)

# Compare variance
num_trials <- 100  # Number of repeated simulations to compute variance
var_corrected <- var(replicate(num_trials, 355 / 113 - integral_estimator(10000)))
var_standard <- var(replicate(num_trials, estimate_pi_standard(10000)))

cat("\nVariance of Corrected Estimator:", var_corrected, "\n")
cat("Variance of Standard Monte Carlo Estimator:", var_standard, "\n")

##for this method, variance of the estimator is much lower than the unit circle method, therefore it's a better estimator.

