theta_0 <- 0.4
epsilon <- 10^(-10)
n_head_obs <- 1
n <- 5
ll_der <- function(theta){return(n_head_obs/theta-(n-n_head_obs)/(1-theta))}
ll_der2 <- function(theta){return((-n_head_obs/(theta)^2 - (n-n_head_obs)/(1-theta)^2))}

m <- 1
theta <- c(theta_0)
while (abs(ll_der(theta[m]))>epsilon){
  theta[m+1]<-theta[m]-ll_der(theta[m])/ll_der2(theta[m])
  m<-m+1
}
  theta_ML <- theta[m+1]
  data.frame(m=seq(0:(m-1)), theta_m=theta)