#load required libraries
library(MASS)
set.seed(123)


#####These are functions to create starting values#####

#Note: separate values for i and j to prevent duplicating values

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
  m <- function(n) runif(n, 0, 0.01)
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


#####Build starting populations#####
get_starting_pop <- function(pars){
  mi <- lapply(c(1:pars$N), function(x) {rnorm(pars$K_i[x],
                                               pars$theta_i[x], 0.05)})
  mj <- lapply(c(1:pars$N), function(x) {rnorm(pars$K_j[x], 
                                               pars$theta_j[x], 0.05)})
  list(meta_i=mi, meta_j=mj)
}

build_starting_pop <- function(pars=NULL){
  if (is.null(pars)){
    pars <- yoder_defaults()
  }
  start_pars <- get_parameters(pars)
  start_pops <- get_starting_pop(start_pars)
  out <- list(pars=start_pars, pops=start_pops,
              fit=NA, part=NA)
  class(out) <- c("list", "metapop")
  out
}

print.metapop <- function(x){
  if (!inherits(x, "metapop")){
    stop("object needs to be of class 'metapop'")
  }
  print(paste0("There are ", length(x$pops), " populations in this community"))
}


#####Function for mating and reproduction#####
mate_repro <- function(x, k, v_s) 
  lapply(x, function(y) mate_repro_k(y, k, v_s))

mate_repro_k <- function(x, k, v_s) 
  sapply(c(1:k), function(y) {
    mean(sample(x, size=2, replace=TRUE)) + rnorm(1, sd=v_s)})


#####Function for migration#####
#note that km = K*m

migrate_m <- function(x, z, km){
  pool <- sapply(x, function(y) y[c(1:km)])
  mkk <- sample(seq_len(length(pool)), size=km)
  if(km > 0) {
  z[c(1:km)] <- pool[mkk] 
  }
  z
}
    
#####Function for abiotic selection#####
## Note: We ignore the possibility of local extinction
abiotic_sel <- function(x, theta, gamma){ 
  w <- lapply(seq_len(length(x)), function(y)
    fitness_f(x[[y]], theta[y], gamma))
  get_survivors(x, w)
}


#####Fitnesses function (quadratic stabilizing selection)#####
fitness_f <- function(x, theta, gamma)
  exp(-gamma * (x - theta)^2)

get_survivors <- function(x, w){
  surv <- lapply(w, surv_id)
  lapply(seq_len(length(x)), function(y) {x[[y]][surv[[y]]]})
}


#####Function to get survivors#####
surv_id <- function(x)
  which(x > runif(1))


#####Function to match up individuals (partnerships)#####
get_partners <- function(i,j){ 
  min_n <- min(length(i), length(j))
  ind_sp_i <- sample(seq_len(length(i)), min_n)
  ind_sp_j <- sample(seq_len(length(j)), min_n) 
  sp_i <- i[ind_sp_i]
  sp_j <- j[ind_sp_j]
  part <- data.frame(sp_i=sp_i, sp_j=sp_j)
  
  #these are the leftovers
  if (length(i) > length(j)){
    rem_i <- i[-ind_sp_i]
    rem_j <- NA
  } else if (length(i) < length(j)){
    rem_i <- NA
    rem_j <- j[-ind_sp_j]
  } else {
    rem_i <- NA
    rem_j <- NA
  }
  
  list(part = part, ind_sp_i=ind_sp_i, ind_sp_j=ind_sp_j,
       rem_i=rem_i, rem_j=rem_j) 
}


#####Biotic selection#####
#for Mutualism, use both beneficial eq'ns
#for Competition, use both costly eq'ns
#for Antagonism, use a beneficial eq'n for partner i and a costly eq'n for j

##Functions for biotic selection - fitness matching##

#costly interaction
fitness_f_match_cost <- function(zeta, alpha, fit_diff){ 
  w <- -zeta * exp(-alpha * fit_diff^2)
  out <- list(w)
}

#beneficial interaction
fitness_f_match_ben <- function(zeta, alpha, fit_diff){ 
  w <- zeta * exp(-alpha * fit_diff^2)
  out <- list(w)
}


##Functions for biotic selection - fitness differences##

#costly interaction
fitness_f_diff_cost <- function(zeta, alpha, fit_diff){ 
  w <- -zeta / (1 + alpha * fit_diff^2)
  out <- list(w)
}

#beneficial interaction
fitness_f_diff_ben <- function(zeta, alpha, fit_diff){ 
  w <- zeta / (1 + alpha * fit_diff^2)
  out <- list(w)
}