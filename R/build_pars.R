## Functions for drawing values
## N.B. Need to add correlation structure among optimum
get_parameters <- function(f){
    N <- f$N()
    K <- round(f$K(2)) #if you change 2 to N, there are 48 NA's, but keeping it at 2
                        #returns one carrying value for i and one for j, they dont vary
    gamma <- f$gamma(2) #this should also vary for each indidivual
    alpha <- f$alpha(2) #this should also vary for each indidivual
    zeta <- f$zeta(2) #this should also vary for each indidivual
    m <- f$m(2) #this should also vary for each indidivual
    names(m) <- names(zeta) <- names(alpha) <- names(gamma) <- names(K) <- c("i", "j")
    theta_i <- f$theta(N)
    theta_j <- f$theta(N)
    v_s <- f$v_s()

    list(N=N, K=K, gamma=gamma, alpha=alpha, zeta=zeta, m=m,
         theta_i=theta_i, theta_j=theta_j, v_s=v_s)
}


## Defaults (Yoder and Nuismer)

yoder_defaults <- function(){
    N <- function() 50
    K <- function(n) runif(n, 300, 2000)
    gamma <- function(n) runif(n, 0.005, 1)
    alpha <- function(n) runif(n, 1,10)
    zeta <- function(n) runif(n, 0.01, 5)
    m <- function(n) runif(n, 0, 0.1)
    theta <- function(n) rnorm(n, runif(1, 0.4, 0.6), runif(1, 0, 1))
    v_s <- function() 0.01
    list(N=N, K=K, gamma=gamma, alpha=alpha, zeta=zeta, m=m,
         theta=theta, v_s=v_s)
}

## Build starting values
get_starting_pop <- function(pars){
    mi <- lapply(c(1:pars$N), function(x) {rnorm(pars$K["i"], #this value needs to vary
                                                 pars$theta_i[x], 0.05)})
    mj <- lapply(c(1:pars$N), function(x) {rnorm(pars$K["j"], #this value needs to vary
                                                 pars$theta_j[x], 0.05)})
    list(meta_i=mi, meta_j=mj)
}
