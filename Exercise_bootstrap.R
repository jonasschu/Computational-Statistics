library("tictoc")


## exercise 3 a. 1) classic confidence interval of mean with normal distribution
mu <- 1
B <- 10000
n <- 20
mu_cover <- matrix(NA, nrow=B, ncol=1)
contains_mu <- function(lower,upper,mu=1) {
  if (lower <= mu && upper >= mu)
    {return(1)
    } else{return(0)}
}

for(b in 1:B){
  sample <- rnorm(n,mean=1,sd=2)
  lower_bound <- mean(sample)+qnorm(0.025)*sd(sample)/sqrt(n)
  upper_bound <- mean(sample)+qnorm(0.975)*sd(sample)/sqrt(n)
  mu_cover[b,1] <- contains_mu(lower_bound, upper_bound,mu=1)
}
print(mean(mu_cover))

# 2) classic CI with chi-squared distribution
mu_cover <- matrix(NA, nrow=B, ncol=1)
contains_mu <- function(lower,upper,mu=1) {
  if (lower <= mu && upper >= mu)
    {return(1)
    } else{return(0)}
}

for(b in 1:B){
  sample <- rchisq(n=20,df=1)
  lower_bound <- mean(sample)+qnorm(0.025)*sd(sample)/sqrt(n)
  upper_bound <- mean(sample)+qnorm(0.975)*sd(sample)/sqrt(n)
  mu_cover[b,1] <- contains_mu(lower_bound, upper_bound,mu=1)
}
print(mean(mu_cover))

## The coverage probability is much worse, at only 0.873 and thus the confidence interval invalid

# b. Standard non-parametric bootstrap CI with chi-squared distribution
tic()
mu <- 1
M <- 1000
B <- 1000
n <- 20
alpha <- 0.05
bCI <- matrix(numeric(n),nrow=B,ncol=1)

bootstrap_func <- function(sample,m=M){
    bootstrap_est <- numeric(m)
    for (i in 1:m){
        bootstrap_sample <- sample(sample,n,replace=TRUE)
        bootstrap_est[i] <- mean(bootstrap_sample)
    }
    return(bootstrap_est)
}

for (b in 1:B){
    sample <- rchisq(n=20,df=1)
    mean_sample <- mean(sample)
    bootstrap_est <- bootstrap_func(sample)
    bootstrap_quant <- quantile(bootstrap_est, c(1-alpha/2, alpha/2))
  lower_bound <- 2*mean_sample-bootstrap_quant[1]
  upper_bound <- 2*mean_sample-bootstrap_quant[2]
  bCI[b,1] <- lower_bound<=mu && upper_bound>=mu
}

coverage_prob <- sum(bCI[,1])/B
coverage_prob
toc()

## The coverage prob is quite low with 0.855 and thus the CI invalid

# c. Bootstrap-t confidence interval 
library("tictoc")
tic()
mu <- 1
M <- 1000
B <- 1000
n <- 20
alpha <- 0.05
cCI <- matrix(numeric(n),nrow=B,ncol=1)

bootstrap_func_T <- function(sample,m=M){
    bootstrap_est_T <- numeric(m)
    for (i in 1:m){
        bootstrap_sample <- sample(sample,n,replace=TRUE)
        bootstrap_est_T[i] <- sqrt(n)*(mean(bootstrap_sample)-mean(sample))/var(bootstrap_sample)
    }
    return(bootstrap_est_T)
}

for (b in 1:B){
    sample <- rchisq(n=20,df=1)
    mean_sample <- mean(sample)
    bootstrap_est_T <- bootstrap_func_T(sample)
    bootstrap_quant_T <- quantile(bootstrap_est_T, c(1-alpha/2, alpha/2))
  lower_bound_T <- mean_sample-bootstrap_quant_T[1]*(var(sample)/sqrt(n))
  upper_bound_T <- mean_sample-bootstrap_quant_T[2]*(var(sample)/sqrt(n))
  cCI[b,1] <- lower_bound_T<=mu && upper_bound_T>=mu
}

coverage_prob_T <- sum(cCI[,1])/B
print(coverage_prob_T)
toc()

# Alternative: get CI as well
c1CI <- matrix(numeric(n),nrow=B,ncol=3)
tic()
for (b in 1:B){
    sample <- rchisq(n=20,df=1)
    mean_sample <- mean(sample)
    bootstrap_est_T <- bootstrap_func_T(sample)
    bootstrap_quant_T <- quantile(bootstrap_est_T, c(1-alpha/2, alpha/2))
  c1CI[b,2] <- mean_sample-bootstrap_quant_T[1]*(var(sample)/sqrt(n))
  c1CI[b,3] <- mean_sample-bootstrap_quant_T[2]*(var(sample)/sqrt(n))
  cCI[b,1] <- c1CI[b,2]<=mu && c1CI[b,3]>=mu
}

coverage_prob_T <- sum(cCI[,1])/B
print(coverage_prob_T)
toc()