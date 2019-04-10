# Laplacian smoothed estimator for theta.hat (MAP)
pixel_means <- function(data) {
  m <- matrix(0, nrow=784, ncol=10)
  for (i in 0:9) {
    class_set <- data$x[data$y == i, ]
    theta.c <- (colSums(class_set) + 1) / (nrow(class_set) + 2)
    m[ ,i+1] <- theta.c
  }
  m
}

# Loglikelihood function for image belonging to certain class
loglik <- function(datapoint, prior, theta) {
  image <- datapoint$x; class <- datapoint$y
  r <- log(prior) + sum(image * log(theta[,class+1]) +
                          (1-image) * log(1-theta[,class+1]))
  r
}

# classification based on bayes probabilities
bayes_estimate <- function(image) {
  logliks <- c()
  for (i in 0:9) {
    datapoint <- list(x=image, y=i)
    logliks <- c(logliks, loglik(datapoint, 1/10, theta.hat))
  }
  which.max(logliks) - 1
}

# Determine the marginal distribution given top half of image
# and create concantenated image
marginal_means <- function(image) {
  marginal <- c()
  for (i in 393:784) {
    denom <- 0
    tmp <- numeric(10)
    for (j in 1:10) {
      bottom_pix <- dbinom(image[i], 1, theta.hat[i, j])
      top_probs <- prod(dbinom(image[1:392], 1, theta.hat[1:392,j]))
      denom <- denom + top_probs
      tmp[j] <- bottom_pix * top_probs
    }
    marginal <- c(marginal, 1-(sum(tmp) / denom))
  }
  new_image <- c(image[1:392], marginal)
  new_image
}