#####Function for simulating across multiple generations#####
#STILL working on this one!

#build starting pars
trial_pops <- build_starting_pop(pars=yoder_defaults())
pars <- trial_pops$pars

coev_div <- function(pars, n.gen, burnin=FALSE, burnin.gen=200, print=FALSE){
  
  ## Burnin -- ramp up selection pressures over time
  if (burnin){
    n.gen <- n.gen - burnin.gen
    
    #abiotic selection
    alpha_i <- list()
    for(i in 1:50){
      alpha <- seq(0, pars$alpha_i[i], by=pars$alpha_i[i]/(burnin.gen-1))
      alpha_i[[i]] <- alpha
    }
    
    alpha_j <- list()
    for(i in 1:50){
      alpha <- seq(0, pars$alpha_j[i], by=pars$alpha_j[i]/(burnin.gen-1))
      alpha_j[[i]] <- alpha
    }
    
    #biotic selection
    gamma_i <- list()
    for(i in 1:50){
      gamma <- seq(0, pars$gamma_i[i], by=pars$gamma_i[i]/(burnin.gen-1))
      gamma_i[[i]] <- gamma
    }
    
    gamma_j <- list()
    for(i in 1:50){
    gamma <- seq(0, pars$gamma_j[i], by=pars$gamma_j[i]/(burnin.gen-1))
    gamma_j[[i]] <- gamma
    }
    
    #replace the pars with the new pars
    for (i in 1:50){
      pars$alpha_i[i] <- alpha_i[i]
      pars$alpha_j[i] <- alpha_j[i]
      pars$gamma_i[i] <- gamma_i[i]
      pars$gamma_j[i] <- gamma_j[i]
    }
    
    #this is where I get stuck, the coev.div.single.gen doesnt
    #work with the new pars, might have to update the fxns
    for (i in 1:burnin.gen){
      out <- coev_div_single_gen(meta_i, meta_j, pars)
      meta_i <- out$pop_i
      meta_j <- out$pop_j
    }
  }
  
  for (j in 1:n.gen){
  res_i <- c(0, sapply(meta_i, mean))
  res_j <- c(0, sapply(meta_j, mean))
  res   <- rbind(res_i, res_j)
  names(res) <- c("gen", sapply(seq_len(length(meta_i)), function(x)
    paste("pop", x, sep="_")))
  }
  
  for (i in seq_len(n.gen)){
    out <- coev_div_single_gen(meta_i, meta_j, pars)
    meta_i <- out$bio_sel_i
    meta_j <- out$bio_sel_j
    res_i <- c(i, sapply(meta_i, mean))
    res_j <- c(i, sapply(meta_j, mean))
  }
  
}        













