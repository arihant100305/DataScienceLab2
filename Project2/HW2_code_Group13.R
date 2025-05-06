# Set the working directory to the uploaded folder for loading the dataset

# Install and load necessary libraries
# The goftest library is for goodness-of-fit tests like Cramer-von Mises

install.packages("goftest")
library(goftest)
library(ggplot2) # For plotting
library(readr)   # For reading CSV files
library(dplyr)   # For data manipulation

# Load the dataset
df <- read_csv("SOCR-HeightWeight.csv")

# Extract the weight data
data <- df$`Weight(Pounds)`
n <- length(data)  # Number of observations

# --- Initial Data Analysis ---
cat("--- Initial Data Analysis ---\n")
hist(data, main = "Histogram of Weight Data", xlab = "Weight (Pounds)")
sample_mean <- mean(data)
cat("Sample Mean:", sample_mean, "\n")
summary(data) # Print summary statistics

# --- Normalization using Mean and Variance ---
cat("--- Normalization using Mean and Variance ---\n")
# Adjust data by subtracting the mean
data_mean_adjusted <- data - sample_mean
cat("First 10 Adjusted Data Points (Mean):", head(data_mean_adjusted, 10), "\n")

# Calculate sample variance
sample_variance <- sum(data_mean_adjusted^2) / (n - 1)
cat("Sample Variance:", sample_variance, "\n")

# Normalize the data
data_normalised <- data_mean_adjusted / (sample_variance^0.5)
hist(data_normalised, main = "Histogram of Normalized Data (Mean/Variance)", xlab = "Normalized Weight")
summary(data_normalised)
cat("First 10 Normalized Data Points (Mean/Variance):", head(data_normalised, 10), "\n")

# Sort the normalized data
sorted_data <- sort(data_normalised)
cat("First 10 Sorted Normalized Data Points (Mean/Variance):", head(sorted_data, 10), "\n")
unique_vals <- unique(sorted_data)
cat("Number of Unique Values (Mean/Variance):", length(unique_vals), "\n")

# --- Normalization using Median and MAD ---
cat("--- Normalization using Median and MAD ---\n")
# Calculate median and MAD
sample_median <- median(data)
sample_MAD <- mad(data, constant = 1) #constant = 1 ensures that mad is statistically consistent for normal data

cat("Sample Median:", sample_median, "\n")
cat("Sample MAD:", sample_MAD, "\n")

# Normalize the data using median and MAD
data_normalised_median <- (data - sample_median) / sample_MAD
hist(data_normalised_median, main = "Histogram of Normalized Data (Median/MAD)", xlab = "Normalized Weight")
summary(data_normalised_median)

# Sort the normalized data
sorted_data_median <- sort(data_normalised_median)
cat("First 10 Sorted Normalized Data Points (Median/MAD):", head(sorted_data_median, 10), "\n")
unique_vals_median <- unique(sorted_data_median)
cat("Number of Unique Values (Median/MAD):", length(unique_vals_median), "\n")

# --- Kolmogorov-Smirnov Test Implementation (Continuous) ---
cat("--- Kolmogorov-Smirnov Test Implementation (Continuous) ---\n")

# KS Test for continuous distribution
Ks_test_c <- function(sorted_data) {
  max_diff <- 0
  for (i in 1:length(sorted_data)) {
    Fnx <- sum(sorted_data <= sorted_data[i]) / n  # Empirical CDF
    Fox <- pnorm(sorted_data[i], mean = 0, sd = 1)  # Theoretical CDF (Normal)
    
    if (abs(Fnx - Fox) > max_diff) {
      max_diff <- abs(Fnx - Fox)
    }
  }
  Tn_KS <- (n^0.5) * max_diff
  return(Tn_KS)
}

Tn_KS_continuous <- Ks_test_c(sorted_data)
cat("KS Test Statistic (Continuous):", Tn_KS_continuous, "\n")

# --- Kolmogorov-Smirnov Test Implementation (Discrete) ---
cat("--- Kolmogorov-Smirnov Test Implementation (Discrete) ---\n")
# KS Test for discrete distribution (adjust for unique values)
Ks_test_d <- function(sorted_data) {
  unique_vals <- unique(sorted_data)
  max_diff <- 0
  for (i in 1:length(unique_vals)) {
    Fnx <- sum(sorted_data <= unique_vals[i]) / n  # Empirical CDF
    Fox <- pnorm(unique_vals[i], mean = 0, sd = 1)  # Theoretical CDF (Normal)
    
    if (abs(Fnx - Fox) > max_diff) {
      max_diff <- abs(Fnx - Fox)
    }
  }
  Tn_KS <- (n^0.5) * max_diff
  return(Tn_KS)
}

Tn_KS_discrete <- Ks_test_d(sorted_data)
cat("KS Test Statistic (Discrete):", Tn_KS_discrete, "\n")

# --- Check for Duplicates ---
cat("--- Check for Duplicates ---\n")
# Check for duplicates in a large vector
n_large <- 1e5  # Reduced size for demonstration
vec <- rnorm(n_large, mean = 0, sd = 1)

if (anyDuplicated(vec) > 0) {
  print("Duplicates exist")
} else {
  print("No duplicates")
}

# --- Critical Value Estimation for KS Test ---
cat("--- Critical Value Estimation for KS Test ---\n")
# Estimate critical value for KS test using simulation
KS_Test_critical <- function() {
  alpha <- 0.05  # Significance level
  m <- 1e3      # Number of simulations
  
  vec <- numeric(m)
  
  for (i in 1:m) {
    dat <- rnorm(n_large, mean = 0, sd = 1)  # Generate random normal data
    sorted_dat <- sort(dat)
    
    Fnx <- 1:n_large / n_large  # Empirical CDF
    Fox <- pnorm(sorted_dat, mean = 0, sd = 1)  # Theoretical CDF
    
    max_diff <- max(abs(Fox - Fnx))
    vec[i] <- (n_large^0.5) * max_diff
  }
  
  vec <- sort(vec)
  c <- vec[(1 - alpha) * m]  # Critical value
  return(c)
}

c <- KS_Test_critical()
cat("Critical Value for KS Test:", c, "\n")

##Critical value = 1.327 and KS_Test value = 0.633, Hence our hypothesis is correct

# --- Kolmogorov-Smirnov Test (In-built) ---
cat("--- Kolmogorov-Smirnov Test (In-built) ---\n")
# Perform KS test using the built-in function
ks_test_result <- ks.test(data_normalised, "pnorm", mean = 0, sd = 1)
print(ks_test_result)

# --- Cramer-von Mises Test (In-built) ---
cat("--- Cramer-von Mises Test (In-built) ---\n")
# Perform Cramer-von Mises test using the built-in function
result <- cvm.test(data_normalised, "pnorm", mean = 0, sd = 1)
print(result)

# --- Cramer-von Mises Test Implementation ---
cat("--- Cramer-von Mises Test Implementation ---\n")
# Implement Cramer-von Mises test
cvm_test <- function(dat) {
  sorted_data <- sort(dat)
  Tn_cvm <- 0
  
  for (i in 1:length(sorted_data)) {
    Fnx <- sum(sorted_data <= sorted_data[i]) / n  # Empirical CDF
    Fox <- pnorm(sorted_data[i], mean = 0, sd = 1)  # Theoretical CDF
    
    Tn_cvm <- Tn_cvm + (Fnx - Fox)^2
  }
  
  return(Tn_cvm)
}

Tn_cvm <- cvm_test(sorted_data)
cat("CVM Test Statistic :", Tn_cvm, "\n")
##It comes out to be = 0.08523986

# --- Critical Value Calculation for Cramer-von Mises Test ---
cat("--- Critical Value Calculation for Cramer-von Mises Test ---\n")
# Calculate critical value for Cramer-von Mises test
cvm_test_c <- function() {
  alpha <- 0.05
  m <- 1e3
  
  vec <- numeric(m)
  for (i in 1:m) {
    dat <- rnorm(n_large, mean = 0, sd = 1)
    sorted_dat <- sort(dat)
    
    Fnx <- 1:n_large / n_large
    Fox <- pnorm(sorted_dat, mean = 0, sd = 1)
    
    vec[i] <- sum((Fox - Fnx)^2)
  }
  
  vec <- sort(vec)
  c <- vec[(1 - alpha) * m]
  return(c)
}

cvm_c <- cvm_test_c()
cat("CVM Critical Value:", cvm_c, "\n")
###critical value = 0.4579202, while our test gives 0.085, hence hypthesis correct

# --- Power of the Test ---
cat("--- Power of the Test ---\n")
# Calculate power of the test
func <- function(c, m) {
  accept <- 0
  n <- 25000  
  
  for (i in 1:m) {
    t_data <- rt(n = 25000, df = 20)
    sorted_data1 <- sort(t_data)  
    
    Fnx <- (1:n) / n
    Fox <- pnorm(sorted_data1, mean = 0, sd = 1)
    
    max_diff <- max(abs(Fox - Fnx))  
    Tn_KS <- (n^0.5) * max_diff  
    
    
    if (Tn_KS >= c) {  
      accept <- accept + 1
    }
  }
  
  return(accept)
}

total_accept <- func(c, m = 1e2)
power <- total_accept / 1e2

cat("Power of the Test:",power,"\n")

# --- P-value Estimation using Bootstrap Resampling ---
cat("--- P-value Estimation using Bootstrap Resampling ---\n")
# Estimate p-value using Bootstrap
options(digits = 9)
B <- 100  # Number of bootstrap samples
observed_Tn_KS <- Ks_test_d(sorted_data)
bootstrap_Tn_KS <- numeric(B)

for (i in 1:B) {
  # Generate a bootstrap sample by resampling with replacement
  boot_sample <- sample(sorted_data, length(sorted_data), replace = TRUE)
  
  # Compute KS statistic for bootstrap sample
  Ks_statistic <- Ks_test_d(boot_sample)
  bootstrap_Tn_KS[i] <- Ks_statistic
}

p_value_KS <- mean(bootstrap_Tn_KS >= observed_Tn_KS)
cat("P-value (KS Test, Bootstrap):", p_value_KS, "\n")

################################################################################

# --- QUESTION 2 ---
cat("--- QUESTION 2 ---\n")
# --- Kernel Density Estimation for Mode Estimation ---
get_mode <- function(data) {
  # Define the Uniform Kernel function
  uniform_kernel <- function(x) {
    if (abs(x) <= 1) return(0.5) else return(0)
  }
  
  # Kernel Density Estimation (Uniform Kernel)
  kde_uniform <- function(x, data, bandwidth) {
    n <- length(data)
    kde_values <- sapply(x, function(xi) {
      sum(sapply((xi - data) / bandwidth, uniform_kernel)) / (n * bandwidth)
    })
    return(kde_values)
  }
  
  # Define range for KDE estimation (before normalization)
  x_vals <- seq(min(data), max(data), length.out = 1000)
  
  # Compute KDE with Uniform Kernel
  bandwidth <- 5  # Adjust bandwidth based on data range
  density_vals <- kde_uniform(x_vals, data, bandwidth)
  
  # Estimate Mode: Find x corresponding to max KDE value
  mode_index <- which.max(density_vals)
  estimated_mode <- x_vals[mode_index]
  
  # Plot Kernel Density Estimate
  plot(x_vals, density_vals, type = "l", col = "blue", lwd = 2,
       main = "Kernel Density Estimation (Uniform Kernel)", xlab = "Weight (Pounds)", ylab = "Density")
  abline(v = estimated_mode, col = "red", lwd = 2, lty = 2)
  
  # Print Estimated Mode
  print(paste("Estimated Mode (Before Normalization):", estimated_mode))
  return(estimated_mode)
}

sample_mode = get_mode(data)

# --- Subsampling Methods ---
cat("--- Subsampling Methods ---\n")
# --- Method 1: Subsampling without Replacement ---
subsample_size <- 1e3
num_subsamples <- floor(n / subsample_size) # Ensure it's an integer

method1 <- function(data, p_mean, p_med, p_mode, p_sd) {
  means <- numeric(num_subsamples)
  medians <- numeric(num_subsamples)
  modes <- numeric(num_subsamples)
  
  # Subsampling loop
  for (i in 1:num_subsamples) {
    subsample <- sample(data, subsample_size, replace = FALSE)
    means[i] <- mean(subsample)
    medians[i] <- median(subsample)
    modes[i] <- get_mode(subsample)
  }
  
  means <- (means - p_mean) / (p_sd / sqrt(subsample_size))
  medians <- (medians - p_med) / (p_sd / sqrt(subsample_size))
  modes <- (modes - p_mode) / (p_sd / sqrt(subsample_size))
  
  return(list(means, medians, modes))
}

# --- Method 2: Bootstrap Sampling with Replacement ---
method2 <- function(data, p_mean, p_med, p_mode, p_sd) {
  means <- numeric(num_subsamples)
  medians <- numeric(num_subsamples)
  modes <- numeric(num_subsamples)
  
  # Subsampling loop
  for (i in 1:num_subsamples) {
    subsample <- sample(data, subsample_size, replace = TRUE)
    means[i] <- mean(subsample)
    medians[i] <- median(subsample)
    modes[i] <- get_mode(subsample)
  }
  
  means <- (means - p_mean) / (p_sd / sqrt(subsample_size))
  medians <- (medians - p_med) / (p_sd / sqrt(subsample_size))
  modes <- (modes - p_mode) / (p_sd / sqrt(subsample_size))
  
  return(list(means, medians, modes))
}

# --- Apply Subsampling Methods ---
cat("--- Apply Subsampling Methods ---\n")

result1 <- method1(data, sample_mean, sample_median, sample_mode, sqrt(sample_variance))

# --- Statistical Tests on Method 1 Results ---
cat("--- Statistical Tests on Method 1 Results ---\n")
# Perform statistical tests on Method 1 results
cvm_test_result1_mean <- cvm.test(result1[[1]], "pnorm", mean = 0, sd = 1)
ks_test_result1_mean <- ks.test(result1[[1]], "pnorm", mean = 0, sd = 1)
cvm_test_result1_med <- cvm.test(result1[[2]], "pnorm", mean = 0, sd = 1)
ks_test_result1_med <- ks.test(result1[[2]], "pnorm", mean = 0, sd = 1)
cvm_test_result1_mode <- cvm.test(result1[[3]], "pnorm", mean = 0, sd = 1)
ks_test_result1_mode <- ks.test(result1[[3]], "pnorm", mean = 0, sd = 1)

print("CVM test for method 1 mean")
print(cvm_test_result1_mean)
print("KS test for method 1 mean")
print(ks_test_result1_mean)
print("CVM test for method 1 median")
print(cvm_test_result1_med)
print("KS test for method 1 median")
print(ks_test_result1_med)
print("CVM test for method 1 mode")
print(cvm_test_result1_mode)
print("KS test for method 1 mode")
print(ks_test_result1_mode)

#critical values
KS_Test_critical()
cvm_test_c()

cat("KS Test Statistic (Continuous) for method 1 mean:",Ks_test_c(result1[[1]]), "\n")
cat("CVM Test Statistic for method 1 mean:",cvm_test(result1[[1]]), "\n")
cat("KS Test Statistic (Continuous) for method 1 median:",Ks_test_c(result1[[2]]), "\n")
cat("CVM Test Statistic for method 1 median:",cvm_test(result1[[2]]), "\n")
cat("KS Test Statistic (Continuous) for method 1 mode:",Ks_test_c(result1[[3]]), "\n")
cat("CVM Test Statistic for method 1 mode:",cvm_test(result1[[3]]), "\n")

result2 <- method2(data, sample_mean, sample_median, sample_mode, sqrt(sample_variance))

# --- Statistical Tests on Method 2 Results ---
cat("--- Statistical Tests on Method 2 Results ---\n")
# Perform statistical tests on Method 2 results
cvm_test_result2_mean <- cvm.test(result2[[1]], "pnorm", mean = 0, sd = 1)
ks_test_result2_mean <- ks.test(result2[[1]], "pnorm", mean = 0, sd = 1)
cvm_test_result2_med <- cvm.test(result2[[2]], "pnorm", mean = 0, sd = 1)
ks_test_result2_med <- ks.test(result2[[2]], "pnorm", mean = 0, sd = 1)
cvm_test_result2_mode <- cvm.test(result2[[3]], "pnorm", mean = 0, sd = 1)
ks_test_result2_mode <- ks.test(result2[[3]], "pnorm", mean = 0, sd = 1)

print("CVM test for method 2 mean")
print(cvm_test_result2_mean)
print("KS test for method 2 mean")
print(ks_test_result2_mean)
print("CVM test for method 2 median")
print(cvm_test_result2_med)
print("KS test for method 2 median")
print(ks_test_result2_med)
print("CVM test for method 2 mode")
print(cvm_test_result2_mode)
print("KS test for method 2 mode")
print(ks_test_result2_mode)

cat("KS Test Statistic (Continuous) for method 2 mean:",Ks_test_c(result2[[1]]), "\n")
cat("CVM Test Statistic for method 2 mean:",cvm_test(result2[[1]]), "\n")
cat("KS Test Statistic (Continuous) for method 2 median:",Ks_test_c(result2[[2]]), "\n")
cat("CVM Test Statistic for method 2 median:",cvm_test(result2[[2]]), "\n")
cat("KS Test Statistic (Continuous) for method 2 mode:",Ks_test_c(result2[[3]]), "\n")
cat("CVM Test Statistic for method 2 mode:",cvm_test(result2[[3]]), "\n")

