#-------------Question1----------------

library(parallel)
install.packages("doParallel")
install.packages("foreach")
library(doParallel)
library(foreach)

#Setup parallel backend
num_cores <- detectCores() - 1  # Leave one core free
cl <- makeCluster(num_cores)
registerDoParallel(cl)

#Parameters
total_numbers <- 1e8 # 10^10
chunk_size <- 1e7      # Numbers per chunk (adjust based on memory)
total_chunks <- ceiling(total_numbers / chunk_size)

#Process in parallel without storing all numbers 
#(Numbers more than 1e8 cannot be stored, can only be generated)

system.time(
  foreach(chunk = 1:total_chunks, .combine = c) %dopar% {
    # Calculate start and end for this chunk
    start <- (chunk - 1) * chunk_size + 1
    end <- min(chunk * chunk_size, total_numbers)
    
    # Generate numbers in this chunk
    numbers_in_chunk <- rnorm(end - start + 1 , 0 , 1)
    
    # Process these numbers immediately (example: calculate mean)
    result <- mean(numbers_in_chunk)
    
    # Return only the processed result, cannot store the numbers
    result
  } -> processed_results
)

# Clean up parallel backend
stopCluster(cl)

# processed_results now contains whatever summary (here, mean) you computed

#--------------------------------------
#-------------Question2----------------

##We try to find the empirical probability of ||X||>0.75

#n_sim is the number of times we conduct the experiment
n_sim = 10
#ans will contain the number of time our proposition ||X||>0.75 is true
ans <- 0

##Instead of sampling from X ~ MVN normal with mean = 0, var(Xt) = 1, cov(Xi,Xj) = 0.6
##We shall sample from AX, where A is an orthogonal matrix, such that A.t * X * A = D
##Thus we shall get n independent random variables with mean = 0 and var = eigen values
##For a covariance matrix K + 0.4*I, where K = all entries 0.6, eigen values come to be
##0.4 with multiplicity (n-1) and 0.6*n+0.4 with multiplicity 1
##Thus we need to generate Y ~ AX, which implies, Y1,...,Y(n-1) will have Var = 0.4
##Var(Yn) = 0.6*n+0.4
##Norm remains unchanged as ||AX|| = ||X||, A is orthogonal matrix

num_cores <- detectCores() - 1  
cl <- makeCluster(num_cores)
registerDoParallel(cl)

for(sim in 1:n_sim) {
  
  ##Computing the sum of sq for 10^8 samples, in each process and storing it as a vector
  ##total_numbers-1 samples generated with mean = 0, var = 0.4
  
  chunk_norm <- foreach(chunk = 1:total_chunks, .combine = c) %dopar% {

    start <- (chunk - 1) * chunk_size + 1
    end <- min(chunk * chunk_size, total_numbers-1)
   
    numbers_in_chunk <- rnorm(end - start + 1 , 0 , sqrt(0.4))
    
    result <- sum(numbers_in_chunk^2)
    
    result
  }
  
  ##One sample with var = 0.6*n+0.4
  xn = rnorm(1 , 0 , sqrt(0.6*total_numbers+0.4))
  
  ##sum of sq of all Xi = square of the norm of vector X
  sq_norm <- sum(chunk_norm) + xn^2
  
  # Compute norm
  norm <- sqrt(sq_norm)
  
  # Return 1 if ||X|| > 0.75, 0 otherwise
  ans <- ans + as.numeric(norm > 0.75)
  
}

# Compute empirical probability

probability <- ans / n_sim
cat("Estimated P(||X|| > 0.75) =", probability, "\n")

##Theoretically checking the probability
##Norm(X) follows weighted Chi-Squared(n) where n = number of samples
##Eigen values of the variance-covariance matrix are the weights

library(mgcv)

# Define weights
lb <- c(0.6*total_numbers + 0.4, rep(0.4, total_numbers-1))

# Define degrees of freedom
df <- rep(1, total_numbers)

# Compute P(||X||² ≤ 0.75^2)
p_less_than <- psum.chisq(0.75^2, lb, df, lower.tail=TRUE)

# The answer is P(||X|| > 0.75) = 1 - p_less_than
result <- 1 - p_less_than


