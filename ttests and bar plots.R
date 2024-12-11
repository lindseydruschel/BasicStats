# Prompt the user for experiment details - uncomment if you want this
#experiment_title <- readline(prompt = "Enter the experiment title: ")
#group1_name <- readline(prompt = "Enter the name for Group 1: ")
#group2_name <- readline(prompt = "Enter the name for Group 2: ")

#OR

experiment_title <- "Monocyte Percentages"
group1_name <- "D2"
group2_name <- "D4"

# Define the two groups as vectors
group1 <- c(0.36292241,
            0.406113983,
            0.526412973,
            0.416356841
) # Replace with your data for Group 1

group2 <- c( 0.273373915,
             0.232485532,
             0.256767034,
             0.230079208,
             0.418961414) # Replace with your data for Group 2

# Perform a two-sample t-test
t_test_result <- t.test(group1, group2)

# Extract p-value and check for significance
p_value <- t_test_result$p.value
significance <- ifelse(p_value < 0.05, "*", ifelse(p_value < 0.01, "**", "ns"))

# Plot the t-distribution
x_vals <- seq(-4, 4, length = 1000)       # Range of t values
y_vals <- dt(x_vals, df = t_test_result$parameter) # T-distribution for the given degrees of freedom

plot(x_vals, y_vals, type = "l", lwd = 2, col = "blue",
     xlab = "t-statistic", ylab = "Density",
     main = experiment_title)
abline(h = 0, col = "gray")

# Add critical t-values
crit_t <- qt(0.975, t_test_result$parameter) # 95% confidence bounds
abline(v = c(-crit_t, crit_t), col = "red", lty = 2)

# Add observed t-statistic
points(t_test_result$statistic, 0, col = "darkgreen", pch = 19, cex = 1.5)
text(t_test_result$statistic, 0.02, labels = paste("Observed t =", round(t_test_result$statistic, 2)), pos = 4, col = "darkgreen")

# Add sample means in the top-left corner of the plot
text(x = par("usr")[1] + 0.5, y = par("usr")[4] - 0.1, 
     labels = paste("Mean Group 1 =", round(mean(group1), 2)), 
     col = "red", adj = 0, cex = 1.2)

text(x = par("usr")[1] + 0.5, y = par("usr")[4] - 0.2, 
     labels = paste("Mean Group 2 =", round(mean(group2), 2)), 
     col = "hotpink", adj = 0, cex = 1.2)

# Calculate group means and standard errors
group_means <- c(mean(group1), mean(group2))
group_se <- c(sd(group1) / sqrt(length(group1)), sd(group2) / sqrt(length(group2)))

# Bar plot with significance
group_means <- c(mean(group1), mean(group2))
group_labels <- c(group1_name, group2_name)

# Create bar plot
bar_positions <- barplot(group_means, names.arg = c(group1_name, group2_name), col = c("red", "blue"), 
                         ylim = c(0, max(group_means) + max(group_se) + 0.5),
                         main = paste(experiment_title, "- Bar Plot"), ylab = "Mean Value")

# Add error bars
arrows(x0 = bar_positions, y0 = group_means - group_se, 
       x1 = bar_positions, y1 = group_means + group_se, 
       angle = 90, code = 3, length = 0.1, col = "black")

# Add significance marker
if (t_test_result$p.value < 0.05) {
  segments(bar_positions[1], max(group_means + group_se) + 0.1, 
           bar_positions[2], max(group_means + group_se) + 0.1, lwd = 2)
  text(mean(bar_positions), max(group_means + group_se) + 0.2, labels = "*", cex = 1.5)
}

#### Repeat #####

# Prompt the user for experiment details - uncomment if you want this
#experiment_title <- readline(prompt = "Enter the experiment title: ")
#group1_name <- readline(prompt = "Enter the name for Group 1: ")
#group2_name <- readline(prompt = "Enter the name for Group 2: ")

#OR

experiment_title <- "M2 Percentages"
group1_name <- "D2"
group2_name <- "D4"

# Define the two groups as vectors
group1 <- c(0.245619577,
            0.21921781,
            0.100244975,
            0.064225817
            
) # Replace with your data for Group 1

group2 <- c(0.310025197,
             0.33370017,
             0.301145405,
             0.406525573,
             0.292734647
) # Replace with your data for Group 2

# Perform a two-sample t-test
t_test_result <- t.test(group1, group2)

# Extract p-value and check for significance
p_value <- t_test_result$p.value
significance <- ifelse(p_value < 0.05, "*", ifelse(p_value < 0.01, "**", "ns"))

# Plot the t-distribution
x_vals <- seq(-4, 4, length = 1000)       # Range of t values
y_vals <- dt(x_vals, df = t_test_result$parameter) # T-distribution for the given degrees of freedom

plot(x_vals, y_vals, type = "l", lwd = 2, col = "blue",
     xlab = "t-statistic", ylab = "Density",
     main = experiment_title)
abline(h = 0, col = "gray")

# Add critical t-values
crit_t <- qt(0.975, t_test_result$parameter) # 95% confidence bounds
abline(v = c(-crit_t, crit_t), col = "red", lty = 2)

# Add observed t-statistic
points(t_test_result$statistic, 0, col = "darkgreen", pch = 19, cex = 1.5)
text(t_test_result$statistic, 0.02, labels = paste("Observed t =", round(t_test_result$statistic, 2)), pos = 4, col = "darkgreen")

# Add sample means in the top-left corner of the plot
text(x = par("usr")[1] + 0.5, y = par("usr")[4] - 0.1, 
     labels = paste("Mean Group 1 =", round(mean(group1), 2)), 
     col = "red", adj = 0, cex = 1.2)

text(x = par("usr")[1] + 0.5, y = par("usr")[4] - 0.2, 
     labels = paste("Mean Group 2 =", round(mean(group2), 2)), 
     col = "hotpink", adj = 0, cex = 1.2)

# Calculate group means and standard errors
group_means <- c(mean(group1), mean(group2))
group_se <- c(sd(group1) / sqrt(length(group1)), sd(group2) / sqrt(length(group2)))

# Bar plot with significance
group_means <- c(mean(group1), mean(group2))
group_labels <- c(group1_name, group2_name)

# Create bar plot
bar_positions <- barplot(group_means, names.arg = c(group1_name, group2_name), col = c("red", "blue"), 
                         ylim = c(0, max(group_means) + max(group_se) + 0.5),
                         main = paste(experiment_title, "- Bar Plot"), ylab = "Mean Value")

# Add error bars
arrows(x0 = bar_positions, y0 = group_means - group_se, 
       x1 = bar_positions, y1 = group_means + group_se, 
       angle = 90, code = 3, length = 0.1, col = "black")

# Add significance marker
if (t_test_result$p.value < 0.05) {
  segments(bar_positions[1], max(group_means + group_se) + 0.1, 
           bar_positions[2], max(group_means + group_se) + 0.1, lwd = 2)
  text(mean(bar_positions), max(group_means + group_se) + 0.2, labels = "*", cex = 1.5)
}

# Add p-value to the plot
text(mean(bar_positions), max(group_means + group_se) + 0.4, 
     labels = paste0("p = ", signif(t_test_result$p.value, 3)), cex = 1.2)

#### Repeat 2 #####

# Prompt the user for experiment details - uncomment if you want this
#experiment_title <- readline(prompt = "Enter the experiment title: ")
#group1_name <- readline(prompt = "Enter the name for Group 1: ")
#group2_name <- readline(prompt = "Enter the name for Group 2: ")

#OR

experiment_title <- "DC Percentages"
group1_name <- "D2"
group2_name <- "D4"

# Define the two groups as vectors
group1 <- c(0.151612528,
            0.108940107,
            0.040017656,
            0.253478408

) # Replace with your data for Group 1

group2 <- c(0.025930335,
            0.272513161,
            0.280111109,
            0.029306035,
            0.035284201
) # Replace with your data for Group 2

# Perform a two-sample t-test
t_test_result <- t.test(group1, group2)

# Extract p-value and check for significance
p_value <- t_test_result$p.value
significance <- ifelse(p_value < 0.05, "*", ifelse(p_value < 0.01, "**", "ns"))

# Plot the t-distribution
x_vals <- seq(-4, 4, length = 1000)       # Range of t values
y_vals <- dt(x_vals, df = t_test_result$parameter) # T-distribution for the given degrees of freedom

plot(x_vals, y_vals, type = "l", lwd = 2, col = "blue",
     xlab = "t-statistic", ylab = "Density",
     main = experiment_title)
abline(h = 0, col = "gray")

# Add critical t-values
crit_t <- qt(0.975, t_test_result$parameter) # 95% confidence bounds
abline(v = c(-crit_t, crit_t), col = "red", lty = 2)

# Add observed t-statistic
points(t_test_result$statistic, 0, col = "darkgreen", pch = 19, cex = 1.5)
text(t_test_result$statistic, 0.02, labels = paste("Observed t =", round(t_test_result$statistic, 2)), pos = 4, col = "darkgreen")

# Add sample means in the top-left corner of the plot
text(x = par("usr")[1] + 0.5, y = par("usr")[4] - 0.1, 
     labels = paste("Mean Group 1 =", round(mean(group1), 2)), 
     col = "red", adj = 0, cex = 1.2)

text(x = par("usr")[1] + 0.5, y = par("usr")[4] - 0.2, 
     labels = paste("Mean Group 2 =", round(mean(group2), 2)), 
     col = "hotpink", adj = 0, cex = 1.2)

# Calculate group means and standard errors
group_means <- c(mean(group1), mean(group2))
group_se <- c(sd(group1) / sqrt(length(group1)), sd(group2) / sqrt(length(group2)))

# Bar plot with significance
group_means <- c(mean(group1), mean(group2))
group_labels <- c(group1_name, group2_name)

# Create bar plot
bar_positions <- barplot(group_means, names.arg = c(group1_name, group2_name), col = c("red", "blue"), 
                         ylim = c(0, max(group_means) + max(group_se) + 0.5),
                         main = paste(experiment_title, "- Bar Plot"), ylab = "Mean Value")

# Add error bars
arrows(x0 = bar_positions, y0 = group_means - group_se, 
       x1 = bar_positions, y1 = group_means + group_se, 
       angle = 90, code = 3, length = 0.1, col = "black")

# Add significance marker
if (t_test_result$p.value < 0.05) {
  segments(bar_positions[1], max(group_means + group_se) + 0.1, 
           bar_positions[2], max(group_means + group_se) + 0.1, lwd = 2)
  text(mean(bar_positions), max(group_means + group_se) + 0.2, labels = "*", cex = 1.5)
}

# Add p-value to the plot
text(mean(bar_positions), max(group_means + group_se) + 0.4, 
     labels = paste0("p = ", signif(t_test_result$p.value, 3)), cex = 1.2)