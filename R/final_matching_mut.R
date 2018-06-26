#Everything Needed for Matching Mutualism

##########PART ONE - SINGLE FUNCTIONS##########
#load required libraries
library(MASS) #for mvrnorm fxn
library(parallel) #for mclapply fxn
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

test_parameters <- function(){
  N <- function() 2
  K <- function(n) runif(n, 2, 4)
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

yoder_2_pop <- function(){
  N <- function() 2
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

##########PART TWO - MATCHING MUTUALISM SPECIFIC FUNCTION##########
coev_div_single_gen <- function(meta_i, meta_j, pars){
  
  ptm <- proc.time()
  newgen_i <- list()
  for(i in 1:pars$N){
    trial_pop_mr <- mate_repro(meta_i[i], pars$K_i[i], pars$v_s)
    newgen_i[i] <- trial_pop_mr
  }
  
  newgen_j <- list()
  for(i in 1:pars$N){
    trial_pop_mr <- mate_repro(meta_j[i], pars$K_j[i], pars$v_s)
    newgen_j[i] <- trial_pop_mr
  }
  proc.time()-ptm 
  
  #####migration#####
  
  ptm <- proc.time()
  remix_i <- list()
  for(i in 1:pars$N){
    remix <- migrate_m(newgen_i[i], newgen_i[[i]], round(pars$m_i[i]*pars$K_i[i]))
    remix_i[[i]] <- remix
  }
  
  remix_j <- list()
  for(i in 1:pars$N){
    remix <- migrate_m(newgen_j[i], newgen_j[[i]], round(pars$m_j[i]*pars$K_j[i]))
    remix_j[[i]] <- remix
  }
  proc.time()-ptm 
  
  #####match individuals#####
  
  ptm <- proc.time()
  partners <- list()
  for(i in 1:pars$N){
    gp <- get_partners(remix_i[[i]], remix_j[[i]])
    partners[[i]] <- gp
  }
  proc.time()-ptm 
  
  #####differences in trait values#####
  
  ptm <- proc.time()
  diff <- list()
  for(i in 1:pars$N){
    d <- partners[[i]]$part$sp_i-partners[[i]]$part$sp_j
    diff[[i]] <- d
  }
  proc.time()-ptm 
  
  #####fitness matching#####
  
  ptm <- proc.time()
  fit_diff <- list()
  w1 <- list()
  for(i in 1:pars$N){
    length_part <- nrow(partners[[i]]$part)
    diff_1 <- diff[[i]][1:length_part]
    fit_diff[[i]] <- diff_1}
  
  fit_match_ben_i <- list()
  for(i in 1:pars$N){
    fitness_matching_ben <- fitness_f_match_ben(pars$zeta_i[i], pars$alpha_i[i], fit_diff[[i]])
    fit_match_ben_i[[i]] <- fitness_matching_ben
  }
  
  fit_match_ben_j <- list()
  for(i in 1:pars$N){
    fitness_matching_ben <- fitness_f_match_ben(pars$zeta_j[i], pars$alpha_j[i], fit_diff[[i]])
    fit_match_ben_j[[i]] <- fitness_matching_ben
  }
  proc.time()-ptm 
  
  #####add remainders to have all phenotypes#####
  
  ptm <- proc.time()
  post_bio_i <- list()
  for(i in 1:pars$N){
    post_bio <- c(unlist(partners[[i]]$part$sp_i), partners[[i]]$rem_i)
    post_bio_i[[i]] <- post_bio[!is.na(post_bio)]
  }
  
  post_bio_j <- list()
  for(i in 1:pars$N){
    post_bio <- c(unlist(partners[[i]]$part$sp_i), partners[[i]]$rem_j)
    post_bio_j[[i]] <- post_bio[!is.na(post_bio)]
  }
  proc.time()-ptm
  
  list(pop_i = post_bio_i, pop_j = post_bio_j)
  
}
  
#####add remainders to have all fitnessess#####


  #####abiotic sel#####
  
  ptm <- proc.time()
  post_sel_i <- list()
  for(i in 1:pars$N){
    post_sel <- abiotic_sel(post_bio_i[i], pars$theta_i[i], pars$gamma_i[i])
    post_sel_i[i] <- post_sel
  }
  
  post_sel_j <- list()
  for(i in 1:pars$N){
    post_sel <- abiotic_sel(post_bio_j[i], pars$theta_j[i], pars$gamma_j[i])
    post_sel_j[i] <- post_sel
  }
  proc.time()-ptm 
  
  ###survivors and remainders###
  
  ptm <- proc.time()
  surv_match_ben_i <- list()
  for(i in 1:pars$N){
    survivors <- get_survivors(post_sel_i[i], fit_match_ben_i[[i]])
    surv_match_ben_i[[i]] <- survivors
  }
  
  surv_match_ben_j <- list()
  for(i in 1:pars$N){
    survivors <- get_survivors(post_sel_j[i], fit_match_ben_j[[i]])
    surv_match_ben_j[[i]] <- survivors
  }
  proc.time()-ptm
  


##########PART THREE - RUN 1000 GEN##########
coev_div <- function(all_pars=NULL, n.gen, burnin=FALSE, burnin.gen, print=FALSE){
  
  if (is.null(all_pars)){
    all_pars <- build_starting_pop(pars=yoder_defaults())
  }
  pars <- all_pars$pars
  meta_i <- all_pars$pops$meta_i
  meta_j <- all_pars$pops$meta_j
  
  ## Burnin -- ramp up selection pressures over time
  if (burnin){
    n.gen <- n.gen - burnin.gen
    
    #create empty matrices to store new selection values
    alpha_i_burn <- matrix(ncol=burnin.gen, nrow=pars$N)
    alpha_j_burn <- matrix(ncol=burnin.gen, nrow=pars$N)
    gamma_i_burn <- matrix(ncol=burnin.gen, nrow=pars$N)
    gamma_j_burn <- matrix(ncol=burnin.gen, nrow=pars$N)
    
    #fill in matrices
    for (m in 1:pars$N){
      alpha_i_burn[m,] <- seq(0, pars$alpha_i[m], by=pars$alpha_i[m]/(burnin.gen-1))
      alpha_j_burn[m,] <- seq(0, pars$alpha_j[m], by=pars$alpha_j[m]/(burnin.gen-1))
      gamma_i_burn[m,] <- seq(0, pars$gamma_i[m], by=pars$gamma_i[m]/(burnin.gen-1))
      gamma_j_burn[m,] <- seq(0, pars$gamma_j[m], by=pars$gamma_j[m]/(burnin.gen-1))
    }
    
    
    ##this is running the simulation for the burnin gen only
    meta_i_list <- list()
    meta_j_list <- list()
    for (p in 1:burnin.gen){
      pars$alpha_i <- alpha_i_burn[,p]
      pars$alpha_j <- alpha_j_burn[,p]
      pars$gamma_i <- gamma_i_burn[,p]
      pars$gamma_j <- gamma_j_burn[,p]
      run <- coev_div_single_gen(meta_i = meta_i, meta_j = meta_j, pars = pars)
      meta_ii <- run$pop_i
      meta_jj <- run$pop_j
      meta_i_list[[p]] <- meta_ii
      meta_j_list[[p]] <- meta_jj
    }
    
    ##run the simulation for the remaining gen (n.gen <- burnin.gen- n.gen)
    #final burnin.gen value is the value we want for the remaining generations
    pars$alpha_i <- alpha_i_burn[,burnin.gen] 
    pars$alpha_j <- alpha_j_burn[,burnin.gen] 
    pars$gamma_i <- gamma_i_burn[,burnin.gen]
    pars$gamma_j <- gamma_j_burn[,burnin.gen]
    
  }
  
  meta_i_list2 <- list()
  meta_j_list2 <- list()
  for (q in 1:n.gen){
    out <- coev_div_single_gen(meta_i, meta_j, pars)
    meta_ii <- out$pop_i
    meta_jj <- out$pop_j
    meta_i_list2[[q]] <- meta_ii
    meta_j_list2[[q]] <- meta_jj
  }
  
  #NOW, meta_i_list and meta_j_list have the generations 1:burnin.gen
  #AND, meta_i_list2 and meta_j_list2 have the generations n.gen (n.gen-burnin.gen)
  
  #combine lists so entire output has total gen information
  all_gens_i <- c(meta_i_list, meta_i_list2)
  all_gens_j <- c(meta_j_list, meta_j_list2)
  
  #create empty matrices to store means and variances
  #columns=gen number and rows=pop number
  pop_meansi <- matrix(ncol=length(all_gens_i), nrow=pars$N)
  pop_vari <- matrix(ncol=length(all_gens_i), nrow=pars$N)
  pop_meansj <- matrix(ncol=length(all_gens_j), nrow=pars$N)
  pop_varj <- matrix(ncol=length(all_gens_j), nrow=pars$N)
  
  #fill in matrices
  for(q in 1:length(all_gens_i)){
    for(r in 1:pars$N){
      pop_meansi[r,q] <- mean(all_gens_i[[q]][[r]])
      pop_vari[r,q] <- var(all_gens_i[[q]][[r]])
    }
  }
  
  #fill in matrices
  for(q in 1:length(all_gens_j)){
    for(r in 1:pars$N){
      pop_meansj[r,q] <- mean(all_gens_j[[q]][[r]])
      pop_varj[r,q] <- var(all_gens_j[[q]][[r]])
    }
  }
  
  #convert to data frames
  pop_vari <- as.data.frame(pop_vari) 
  pop_varj <- as.data.frame(pop_varj) 
  pop_meansi <- as.data.frame(pop_meansi)
  pop_meansj <- as.data.frame(pop_meansj)
  list(pars = pars,pop_var_i = pop_vari, pop_var_j = pop_varj,
       pop_means_i = pop_meansi, pop_means_j = pop_meansj )
}

##########PART FOUR - RUN 1000 TIMES##########
#####Function for running 1000 times#####
coev_div_wrapper <- function(iter, all_pars=NULL, n.gen = 1000, 
                             burnin = TRUE, burnin.gen = 200, print=FALSE){
  out <- coev_div(all_pars, n.gen = n.gen, burnin = burnin, 
                  burnin.gen = burnin.gen, print=print)
  saveRDS(out, paste0("out_mut_match/mutualism_matching_sim_", iter, ".rds"))
}

#run coev_div_wrapper 1000 times
mclapply(c(1:1000), function(x) coev_div_wrapper(x, n.gen = 1000, burnin = TRUE, burnin.gen = 200, 
                                              print = FALSE), mc.cores = 50)

##########PART 5 - AMMENDED FOR MEANS FIGURE##########
yoder_defaults_2 <- function(){
  N <- function() 100
  K <- function(n) runif(n, 1000, 1000)
  gamma <- function(n) runif(n, 0.2, 0.2)
  alpha <- function(n) runif(n, 4,4)
  zeta <- function(n) runif(n, 0.25, 0.25)
  m <- function(n) runif(n, 0.0001, 0.0001)
  theta <- function(n) rnorm(n, runif(1, 0.5, 0.5), runif(1, 0.5, 0.5))
  v_s <- function() 0.1
  list(N=N, K=K, gamma=gamma, alpha=alpha, zeta=zeta, m=m,
       theta=theta, v_s=v_s)
}

get_parameters_2 <- function(f){
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
  theta_i <- f$theta(N)
  theta_j <- f$theta(N)
  v_s <- f$v_s()
  
  list(N=N, K_i=K_i, K_j=K_j, gamma_i=gamma_i, gamma_j=gamma_j, 
       alpha_i=alpha_i, alpha_j=alpha_j, zeta_i=zeta_i, zeta_j=zeta_j,
       m_i=m_i, m_j=m_j, theta_i=theta_i, theta_j=theta_j, v_s=v_s)
}

build_starting_pop_2 <- function(pars=NULL){
  if (is.null(pars)){
    pars <- yoder_defaults_2()
  }
  start_pars <- get_parameters_2(pars)
  start_pops <- get_starting_pop(start_pars)
  out <- list(pars=start_pars, pops=start_pops,
              fit=NA, part=NA)
  class(out) <- c("list", "metapop")
  out
}

coev_div_2 <- function(all_pars=NULL, n.gen, burnin=FALSE, burnin.gen, print=FALSE){
  
  if (is.null(all_pars)){
    all_pars <- build_starting_pop_2(pars=yoder_defaults_2())
  }
  pars <- all_pars$pars
  meta_i <- all_pars$pops$meta_i
  meta_j <- all_pars$pops$meta_j
  
  ## Burnin -- ramp up selection pressures over time
  if (burnin){
    n.gen <- n.gen - burnin.gen
    
    #create empty matrices to store new selection values
    alpha_i_burn <- matrix(ncol=burnin.gen, nrow=pars$N)
    alpha_j_burn <- matrix(ncol=burnin.gen, nrow=pars$N)
    gamma_i_burn <- matrix(ncol=burnin.gen, nrow=pars$N)
    gamma_j_burn <- matrix(ncol=burnin.gen, nrow=pars$N)
    
    #fill in matrices
    for (m in 1:pars$N){
      alpha_i_burn[m,] <- seq(0, pars$alpha_i[m], by=pars$alpha_i[m]/(burnin.gen-1))
      alpha_j_burn[m,] <- seq(0, pars$alpha_j[m], by=pars$alpha_j[m]/(burnin.gen-1))
      gamma_i_burn[m,] <- seq(0, pars$gamma_i[m], by=pars$gamma_i[m]/(burnin.gen-1))
      gamma_j_burn[m,] <- seq(0, pars$gamma_j[m], by=pars$gamma_j[m]/(burnin.gen-1))
    }
    
    
    ##this is running the simulation for the burnin gen only
    meta_i_list <- list()
    meta_j_list <- list()
    for (p in 1:burnin.gen){
      pars$alpha_i <- alpha_i_burn[,p]
      pars$alpha_j <- alpha_j_burn[,p]
      pars$gamma_i <- gamma_i_burn[,p]
      pars$gamma_j <- gamma_j_burn[,p]
      run <- coev_div_single_gen(meta_i = meta_i, meta_j = meta_j, pars = pars)
      meta_ii <- run$pop_i
      meta_jj <- run$pop_j
      meta_i_list[[p]] <- meta_ii
      meta_j_list[[p]] <- meta_jj
    }
    
    ##run the simulation for the remaining gen (n.gen <- burnin.gen- n.gen)
    #final burnin.gen value is the value we want for the remaining generations
    pars$alpha_i <- alpha_i_burn[,burnin.gen] 
    pars$alpha_j <- alpha_j_burn[,burnin.gen] 
    pars$gamma_i <- gamma_i_burn[,burnin.gen]
    pars$gamma_j <- gamma_j_burn[,burnin.gen]
    
  }
  
  meta_i_list2 <- list()
  meta_j_list2 <- list()
  for (q in 1:n.gen){
    out <- coev_div_single_gen(meta_i, meta_j, pars)
    meta_ii <- out$pop_i
    meta_jj <- out$pop_j
    meta_i_list2[[q]] <- meta_ii
    meta_j_list2[[q]] <- meta_jj
  }
  
  #NOW, meta_i_list and meta_j_list have the generations 1:burnin.gen
  #AND, meta_i_list2 and meta_j_list2 have the generations n.gen (n.gen-burnin.gen)
  
  #combine lists so entire output has total gen information
  all_gens_i <- c(meta_i_list, meta_i_list2)
  all_gens_j <- c(meta_j_list, meta_j_list2)
  
  #create empty matrices to store means and variances
  #columns=gen number and rows=pop number
  pop_meansi <- matrix(ncol=length(all_gens_i), nrow=pars$N)
  #pop_vari <- matrix(ncol=length(all_gens_i), nrow=pars$N)
  pop_meansj <- matrix(ncol=length(all_gens_j), nrow=pars$N)
  #pop_varj <- matrix(ncol=length(all_gens_j), nrow=pars$N)
  
  #fill in matrices
  for(q in 1:length(all_gens_i)){
    for(r in 1:pars$N){
      pop_meansi[r,q] <- mean(all_gens_i[[q]][[r]])
      #pop_vari[r,q] <- var(all_gens_i[[q]][[r]])
    }
  }
  
  #fill in matrices
  for(q in 1:length(all_gens_j)){
    for(r in 1:pars$N){
      pop_meansj[r,q] <- mean(all_gens_j[[q]][[r]])
      #pop_varj[r,q] <- var(all_gens_j[[q]][[r]])
    }
  }
  
  #convert to data frames
  #pop_vari <- as.data.frame(pop_vari) 
  #pop_varj <- as.data.frame(pop_varj) 
  pop_meansi <- as.data.frame(pop_meansi)
  pop_meansj <- as.data.frame(pop_meansj)
  list(pars = pars, pop_means_i = pop_meansi, pop_means_j = pop_meansj )
}

#run the simulation with the different pars for 1000 generations
solo_sim_mm <- coev_div_2(all_pars = NULL, n.gen = 1000, burnin = TRUE, burnin.gen = 200, print = FALSE)
write.csv(solo_sim_mm, file = "solo_sim_mm.csv")
