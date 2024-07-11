## Generating a bootstrap confidence interval for a regression
library(ggplot2)

# generate data
set.seed(123)
X <- runif(20,0,30)
epsilon <- rnorm(20)
Y <- 4*X+epsilon
data <- data.frame(X=X,Y=Y)

# Standard reg with confidence intervals from distributional assumption on epsilon/Asymptotics:
reg <- lm(Y ~ X,data=data)
summary(reg)

# Bootstrap
beta_2 <- 3.997
M <- 1000
n <- nrow(data)
beta_est <- matrix(numeric(M))
alpha <- 0.05

# Define bootstrap function that gives the estimate
bootstrap_func <- function(data){
        bootstrap_sample <- data[sample(1:n,n,replace=TRUE),]
        reg_m <- lm(Y ~ X, data=bootstrap_sample)
        beta_est <- coef(reg_m)["X"]
        return(beta_est)
}

for (i in 1:M){
    beta_est[i] <- bootstrap_func(data)
}

beta_ci <- quantile(beta_est,c(alpha/2,1-alpha/2))
print(beta_ci)
hist <- ggplot(data.frame(beta_est), aes(x = beta_est)) +
  geom_histogram(bins = 30, fill="red", alpha = 0.7) +
  geom_vline(xintercept = beta_ci, col = "red", lty = 2) +
  labs(title = "Bootstrap Estimates of beta_2",
       x = "beta_2 Estimates",
       y = "Frequency") +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white"),
    panel.grid.major = element_line(color = "grey80"),
    panel.grid.minor = element_line(color = "grey90")
  )  

print(hist)
ggsave("bootstrap_est_reg.png",plot=hist,path="C:/Users/jonas/Documents/Economics Bonn/Computational Statistics")
