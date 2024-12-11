# Load required package
if (!require("pwr")) install.packages("pwr", dependencies = TRUE)
library(pwr)

# Function for power analysis
test_power_analysis <- function(group1, group2, alpha = 0.05, power = 0.8) {
  # Check input validity
  if (!is.numeric(group1) || !is.numeric(group2)) {
    stop("Both inputs must be numeric vectors.")
  }
  
  # Calculate means and standard deviations
  mean1 <- mean(group1)
  print(mean1)
  mean2 <- mean(group2)
  print(mean2)
  sd1 <- sd(group1)
  print(sd1)
  sd2 <- sd(group2)
  print(sd2)
  
  # Pooled standard deviation
  pooled_sd <- sqrt((((length(group1) - 1) * sd1^2) + ((length(group2) - 1) * sd2^2)) / (length(group1) + length(group2) - 2))
  
  # Effect size (Cohen's d)
  effect_size <- abs(mean1 - mean2) / pooled_sd
  
  # Calculate required sample size
  sample_size <- pwr.t.test(d = effect_size, sig.level = alpha, power = power, type = "two.sample", alternative = "two.sided")
  
  # Output results
  list(
    "Effect Size (Cohen's d)" = effect_size,
    "Required Sample Size per Group" = ceiling(sample_size$n),
    "Total Required Sample Size" =2*(ceiling(sample_size$n))
  )
}

# Example usage
# Replace these with user inputs
group1 <- c(10, 12, 15, 14, 13)  # Example data for group 1
group2 <- c(8, 9, 11, 10, 10)    # Example data for group 2

# Run power analysis
result <- test_power_analysis(group1, group2)

# Display results
print(result)
