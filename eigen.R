# Load necessary libraries
library(MASS)
library(ggplot2)

# Generate synthetic 2D data
set.seed(0)
X <- mvrnorm(100, mu = c(0, 0), Sigma = matrix(c(4, 2, 2, 3), nrow = 2))

# Calculate the mean of the data
mean_X <- colMeans(X)

# Covariance matrix
cov_matrix <- cov(X)

# Eigenvalues and eigenvectors
eig <- eigen(cov_matrix)
eigenvalues <- eig$values
eigenvectors <- eig$vectors

# Sort eigenvalues and eigenvectors
idx <- order(eigenvalues, decreasing = TRUE)
eigenvalues <- eigenvalues[idx]
eigenvectors <- eigenvectors[, idx]

# Prepare data for plotting
df <- data.frame(X1 = X[,1], X2 = X[,2])
arrow1 <- data.frame(x1 = mean_X[1], y1 = mean_X[2], 
                     x2 = mean_X[1] + eigenvectors[1,1] * sqrt(eigenvalues[1]), 
                     y2 = mean_X[2] + eigenvectors[2,1] * sqrt(eigenvalues[1]))
arrow2 <- data.frame(x1 = mean_X[1], y1 = mean_X[2], 
                     x2 = mean_X[1] + eigenvectors[1,2] * sqrt(eigenvalues[2]), 
                     y2 = mean_X[2] + eigenvectors[2,2] * sqrt(eigenvalues[2]))

# Plot original data and eigenvectors
ggplot(df, aes(x = X1, y = X2)) +
  geom_point(alpha = 0.2) +
  geom_segment(data = arrow1, aes(x = x1, y = y1, xend = x2, yend = y2), color = 'red', arrow = arrow(length = unit(0.3, "cm"))) +
  geom_segment(data = arrow2, aes(x = x1, y = y1, xend = x2, yend = y2), color = 'green', arrow = arrow(length = unit(0.3, "cm"))) +
  labs(title = 'PCA Eigenvectors and Eigenvalues', x = 'X1', y = 'X2') +
  theme_minimal() +
  coord_fixed()