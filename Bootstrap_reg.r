## Generating a bootstrap confidence interval for a regression
library(ggplot2)

# generate data
set.seed(123)
X <- runif(20,0,30)
epsilon <- rnorm(20)
beta <- 4
Y <- beta*X+epsilon
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

# Define bootstrap function for random design:
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
  labs(title = "Random Design Bootstrap Estimates of beta_2",
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
ggsave("bootstrap_est_reg_random.png",plot=hist,path="C:/Users/jonas/Documents/Economics_Bonn/Statistics/Computational Statistics")


# Bootstrap function for fixed design:
beta_est_f <- matrix(numeric(M))
reg <- lm(Y ~ X, data=data)
fitted_values <- fitted(reg)
residuals <- residuals(reg)
data_reg <- data.frame(fitted_values = fitted_values,residuals=residuals)
bootstrap_func_f <- function(data){
        bootstrap_res <- sample(data_reg$residuals,n,replace=TRUE)
        bootstrap_Y <- data_reg$fitted_values + bootstrap_res
        bootstrap_data <- data.frame(X=data$X,Y=bootstrap_Y)
        reg_f <- lm(Y ~ X, data=bootstrap_data)
        beta_est_f <- coef(reg_f)["X"]
        return(beta_est_f)
}

for (i in 1:M){
    beta_est_f[i] <- bootstrap_func_f(data)
}

CIf <- matrix(nrow=1,ncol=3)
beta_quant <- quantile(beta_est_f,c(1-alpha/2,alpha))
CIf[,2] <- 2*coef(reg)["X"]-beta_quant[1]
CIf[,3] <- 2*coef(reg)["X"]-beta_quant[2]
CIf[,1] <- CIf[,2]<=beta && CIf[,3]>=beta

hist <- ggplot(data.frame(beta_est_f), aes(x = beta_est_f)) +
  geom_histogram(bins = 30, fill="red", alpha = 0.7) +
  geom_vline(xintercept = beta_quant, col = "red", lty = 2) +
  labs(title = "Fixed Design Bootstrap Estimates of beta_2",
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
ggsave("bootstrap_est_reg_fixed.png",plot=hist,path="C:/Users/jonas/Documents/Economics_Bonn/Statistics/Computational Statistics")
