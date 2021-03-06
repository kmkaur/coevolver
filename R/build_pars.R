#load require libraries
library(MASS)
set.seed(123)
   
#####These are functions for starting values#####

#by separating the values for i and j, it should fix the problem of 
#having the same carrying capacity per species for all 50 populations 
get_parameters <- function(f){
    N <- f$N()
    K_i <- round(f$K(N))
    K_j <- round(f$K(N))
    gamma_i <- f$gamma(N) 
    gamma_j <- f$gamma(N) 
    alpha_i <- f$alpha(N) 
    alpha_j <- f$alpha(N) 
    zeta_i <- f$zeta(N)
    zeta_j <- f$zeta(N) 
    m_i <- f$m(N) 
    m_j <- f$m(N)
    theta_i <- f$theta(N)[,1]
    theta_j <- f$theta(N)[,2]
    v_s <- f$v_s()

    list(N=N, K_i=K_i, K_j=K_j, gamma_i=gamma_i, gamma_j=gamma_j, 
         alpha_i=alpha_i, alpha_j=alpha_j, zeta_i=zeta_i, zeta_j=zeta_j,
         m_i=m_i, m_j=m_j, theta_i=theta_i, theta_j=theta_j, v_s=v_s)
}


#Defaults parameters (Yoder & Nuismer, 2010)
yoder_defaults <- function(){
    N <- function() 50
    K <- function(n) runif(n, 300, 2000)
    gamma <- function(n) runif(n, 0.005, 1)
    alpha <- function(n) runif(n, 1,10)
    zeta <- function(n) runif(n, 0.01, 5)
    m <- function(n) runif(n, 0, 0.1)
    mean <- c(runif(1, 0.4, 0.6), runif(1, 0.4, 0.6))
    var <- runif(1, 0, 1)
    sigma1 <- runif(1, -.1, .1)
    sigma2 <- runif(1, -.1, .1)
    sigma <- matrix(c(sigma1^2, sigma1*sigma2*var, sigma1*sigma2*var, sigma2^2),2)
    #sigma gives correlation structure to theta
    theta <- function(n) mvrnorm(n, mu=mean, Sigma=sigma, empirical = FALSE)
    v_s <- function() 0.01
    list(N=N, K=K, gamma=gamma, alpha=alpha, zeta=zeta, m=m,
         theta=theta, v_s=v_s)
}

test_parameters <- function(){
  N <- function() 10
  K <- function(n) runif(n, 2, 5)
  gamma <- function(n) runif(n, 0.005, 1)
  alpha <- function(n) runif(n, 1,10)
  zeta <- function(n) runif(n, 0.01, 5)
  m <- function(n) runif(n, 0, 0.1)
  theta <- function(n) rnorm(n, runif(1, 0.4, 0.6), runif(1, 0, 1))
  v_s <- function() 0.01
  list(N=N, K=K, gamma=gamma, alpha=alpha, zeta=zeta, m=m,
       theta=theta, v_s=v_s)
}

#####Build starting populations#####
get_starting_pop <- function(pars){
    mi <- lapply(c(1:pars$N), function(x) {rnorm(pars$K_i[x],
                                                 pars$theta_i[x], 0.05)})
    mj <- lapply(c(1:pars$N), function(x) {rnorm(pars$K_j[x], 
                                                 pars$theta_j[x], 0.05)})
    list(meta_i=mi, meta_j=mj)
}

