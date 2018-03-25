#####Function for simulating across multiple generations#####
#STILL working on this one!

coev_div <- function(pars, n.gen, burnin=FALSE, burnin.gen=200, print=FALSE){
  
  trial_pops <- build_starting_pop(pars=yoder_defaults())
  
  pars <- trial_pops$pars
  meta_i <- trial_pops$pops$meta_i
  meta_j <- trial_pops$pops$meta_j
  
  ## Burnin -- ramp up selection pressures over time
  if (burnin){
    n.gen <- n.gen - burnin.gen
    
    alpha_i <- seq(0, pars$alpha_i[i], by=pars$alpha_i[i]/(burnin.gen-1))
    alpha_j <- seq(0, pars$alpha_j[i], by=pars$alpha_j[i]/(burnin.gen-1))
    gamma_i <- seq(0, pars$gamma_i[i], pars$gamma_i[i]/(burnin.gen-1))
    gamma_j <- seq(0, pars$gamma_j[i], pars$gamma_j[i]/(burnin.gen-1))
    
    for (k in seq_len(burnin.gen)){
      pars$alpha_i[i] <- alpha_i[k]
      pars$alpha_j[i] <- alpha_j[k]
      pars$gamma_i[i] <- gamma_i[k]
      pars$gamma_j[i] <- gamma_j[k]
      
      out <- coev_div_single_gen(meta_i, meta_j, pars)
      meta_i <- out$bio_sel_i
      meta_j <- out$bio_sel_j
    }
  }
  
  res_i <- c(0, sapply(meta_i, mean))
  res_j <- c(0, sapply(meta_j, mean))
  res   <- rbind(res_i, res_j)
  names(res) <- c("gen", sapply(seq_len(length(meta_i)), function(x)
    paste("pop", x, sep="_")))
  
  
  for (i in seq_len(n.gen)){
    out <- coev_div_single_gen(meta_i, meta_j, pars)
    meta_i <- out$bio_sel_i
    meta_j <- out$bio_sel_j
    res_i <- c(i, sapply(meta_i, mean))
    res_j <- c(i, sapply(meta_j, mean))
  }
  
}        













