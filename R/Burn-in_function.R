#####Function for simulating across multiple generations#####
#STILL working on this one!

#build starting pars
trial_pops <- build_starting_pop(pars=yoder_2_pop())
pars <- trial_pops$pars
meta_i <- trial_pops$pops$meta_i
meta_j <- trial_pops$pops$meta_j
out <- coev_div_single_gen(meta_i, meta_j, pars)

coev_div <- function(pars, n.gen, burnin=FALSE, burnin.gen=200, print=FALSE){
  
  ## Burnin -- ramp up selection pressures over time
  if (burnin){
    n.gen <- n.gen - burnin.gen
    
    alpha_i_burn <- matrix(ncol=burnin.gen, nrow=pars$N)
    alpha_j_burn <- matrix(ncol=burnin.gen, nrow=pars$N)
    gamma_i_burn <- matrix(ncol=burnin.gen, nrow=pars$N)
    gamma_j_burn <- matrix(ncol=burnin.gen, nrow=pars$N)
    
    
    for (m in 1:pars$N){
        alpha_i_burn[m,] <- seq(0, pars$alpha_i[m], by=pars$alpha_i[m]/(burnin.gen-1))
        alpha_j_burn[m,] <- seq(0, pars$alpha_j[m], by=pars$alpha_j[m]/(burnin.gen-1))
        gamma_i_burn[m,] <- seq(0, pars$gamma_i[m], by=pars$gamma_i[m]/(burnin.gen-1))
        gamma_j_burn[m,] <- seq(0, pars$gamma_j[m], by=pars$gamma_j[m]/(burnin.gen-1))
    }
    

    ##this is where I get stuck, the coev.div.single.gen doesnt
    #work with the new pars, might have to update the fxns##
    for (p in 1:burnin.gen){
      pars$alpha_i <- alpha_i_burn[,p]
      pars$alpha_j <- alpha_j_burn[,p]
      pars$gamma_i <- gamma_i_burn[,p]
      pars$gamma_j <- gamma_j_burn[,p]
      out <- coev_div_single_gen(meta_i, meta_j, pars)
      meta_i <- out$pop_i
      meta_j <- out$pop_j
    }
  }
  
  for (j in 1:n.gen){
  #need to define j
  #need to find the mean of each population under each generation?
  res_i <- c(0, sapply(meta_i, mean))
  res_j <- c(0, sapply(meta_j, mean))
  res   <- rbind(res_i, res_j)
  names(res) <- c("gen", sapply(seq_len(length(meta_i)), function(x)
    paste("pop", x, sep="_")))
  }
  
  for (i in seq_len(n.gen)){
  #need to define i
    out <- coev_div_single_gen(meta_i, meta_j, pars)
    meta_i <- out$pop_i
    meta_j <- out$pop_j
    res_i <- c(i, sapply(meta_i, mean))
    res_j <- c(i, sapply(meta_j, mean))
  }
  
}        













