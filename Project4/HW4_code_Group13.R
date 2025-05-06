##Question 1

set.seed(42)  

library(MASS)  # Needed for multivariate normal sampling

N <- 1000  
n <- 100    
rho <- 0   # Correlation parameter for dependence in Gaussian Copula

# Function to generate dependent uniform samples using Gaussian Copula
generate_gaussian_copula <- function(n, rho) {
  mu <- rep(0, n)
  
  # Create covariance matrix
  Sigma <- matrix(rho, n, n)
  diag(Sigma) <- 1
  
  # Generate correlated normal vector
  Z <- mvrnorm(n = 1, mu = mu, Sigma = Sigma)
  
  # Transform to Uniform(0,1)
  U <- pnorm(Z)
  return(U)
}

# Function to compute the statistic using copula-generated samples
compute_fraction_copula <- function() {
  U <- generate_gaussian_copula(n, rho)
  return(sum(U^101) / sum(U))
}

# Monte Carlo Simulation
results <- numeric(N)
for (i in 1:N) {
  results[i] <- compute_fraction_copula()
}

# Estimate expectation
estimated_value <- mean(results)

# Output
cat("Estimated value using Gaussian Copula (rho =", rho, "):", estimated_value, "\n")
cat("Theoretical value:", 1/51, "\n")

## the estimated value matches with the theoritical value, that is 1/51 = 0.01960784

###############################################################################

##Question 2

install.packages("L1pack")
library(L1pack)

load("Placement.RData")
x = df[,1]
y = df[,2]/1e5

plot(x,y)

lse = function(x,y){
  b1 = cor(x,y) * sd(y)/sd(x)
  b0 = mean(y) - b1*mean(x)
  mse = mean((y-b0-b1*x)^2)
  mae = mean(abs(y-b0-b1*x))
  mape = mean(abs(y-b0-b1*x)*100/y)
  cat("b1 = ",b1,"b0 = ",b0,"mse = ",mse,"mae = ",mae,"mape = ",mape)
}

lse(x,y)

lse_f = function(x,y){
  model <- lm(y ~ x, data = data.frame(x,y))
  b0 <- coef(model)[1]  
  b1 <- coef(model)[2] 
  mse = mean((y-b0-b1*x)^2)
  mae = mean(abs(y-b0-b1*x))
  mape = mean(abs(y-b0-b1*x)*100/y)
  cat("b1 = ",b1,"b0 = ",b0,"mse = ",mse,"mae = ",mae,"mape = ",mape)
}

lse_f(x,y)

lad_f = function(x,y){
  model <- lad(y ~ x, data = data.frame(x,y), method = "BR")
  b0 <- coef(model)[1]  
  b1 <- coef(model)[2]  
  mse = mean((y-b0-b1*x)^2)
  mae = mean(abs(y-b0-b1*x))
  mape = mean(abs(y-b0-b1*x)*100/y)
  cat("b1 = ",b1,"b0 = ",b0,"mse = ",mse,"mae = ",mae,"mape = ",mape)
}

lad_f(x,y)

########################################################################################

install.packages("plot3D")  
library(plot3D)

mse_function <- function(b0, b1, x, y) {
  mean((y-b0-b1*x)^2)  # MSE formula
}

# Define grid for b0 and b1
b0_seq <- seq(-100, -80, length.out = 50)  # Adjust range based on your data
b1_seq <- seq(12, 18, length.out = 50)

# Create MSE matrix
mse_values <- matrix(0, nrow = length(b0_seq), ncol = length(b1_seq))

# Compute MSE for each (b0, b1) combination
for (i in 1:length(b0_seq)) {
  for (j in 1:length(b1_seq)) {
    mse_values[i, j] <- mse_function(b0_seq[i], b1_seq[j], x, y)
  }
}

# Find the minimum MSE location
min_index <- which(mse_values == min(mse_values), arr.ind = TRUE)
best_b0 <- b0_seq[min_index[1]]
best_b1 <- b1_seq[min_index[2]]

# 3D Plot
persp3D(x = b0_seq, y = b1_seq, z = mse_values,
        xlab = "b0", ylab = "b1", zlab = "MSE",
        main = "MSE Surface Plot",
        col = "lightblue", border = "black")

# Highlight minimum point
points3D(x = best_b0, y = best_b1, z = min(mse_values),
         col = "red", pch = 19, add = TRUE)


#########################################################################################################


mad_function <- function(b0, b1, x, y) {
  mean(abs(y-b0-b1*x))  # MSE formula
}

# Define grid for b0 and b1
b0_seq <- seq(-50, -30, length.out = 50)  # Adjust range based on your data
b1_seq <- seq(5, 11, length.out = 50)

# Create MSE matrix
mse_values <- matrix(0, nrow = length(b0_seq), ncol = length(b1_seq))

# Compute MSE for each (b0, b1) combination
for (i in 1:length(b0_seq)) {
  for (j in 1:length(b1_seq)) {
    mse_values[i, j] <- mad_function(b0_seq[i], b1_seq[j], x, y)
  }
}

# Find the minimum MSE location
min_index <- which(mse_values == min(mse_values), arr.ind = TRUE)
best_b0 <- b0_seq[min_index[1]]
best_b1 <- b1_seq[min_index[2]]

# 3D Plot
persp3D(x = b0_seq, y = b1_seq, z = mse_values,
        xlab = "b0", ylab = "b1", zlab = "MSE",
        main = "MSE Surface Plot",
        col = "lightblue", border = "black")

# Highlight minimum point
points3D(x = best_b0, y = best_b1, z = min(mse_values),
         col = "red", pch = 19, add = TRUE)


#########################################################################################################

load("Bivariate_Data.RData")
x = dat$X1
y = dat$y

plot(x,y)
lse(x,y)
lad_f(x,y)

x = dat$X2
y = dat$y

plot(x,y)
lse(x,y)
lad_f(x,y)

x = dat$X3
y = dat$y

plot(x,y)
lse(x,y)
lad_f(x,y)

x = dat$X4
y = dat$y

plot(x,y)
lse(x,y)
lad_f(x,y)

##########################################################################

