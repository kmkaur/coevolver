#####Function for simulating across multiple generations#####
#STILL working on this one!

#build starting pars
trial_pops <- build_starting_pop(pars=yoder_defaults())
pars <- trial_pops$pars
meta_i <- trial_pops$pops$meta_i
meta_j <- trial_pops$pops$meta_j

coev_div <- function(pars, n.gen, burnin=FALSE, burnin.gen, print=FALSE){
  
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
    list(all_gens_i = all_gens_i, all_gens_j = all_gens_j, 
         pop_var_i = pop_vari, pop_var_j = pop_varj,
         pop_means_i = pop_meansi, pop_means_j = pop_meansj )
}

ptm <- proc.time()
out <- coev_div(pars, n.gen = 1000, burnin = TRUE, burnin.gen = 200, print=FALSE)
proc.time()-ptm 

#run 1000 times







#     for (j in 1:n.gen){
#       res_i <- c(0, sapply(meta_i, mean))
#       res_j <- c(0, sapply(meta_j, mean))
#       res   <- rbind(res_i, res_j)
#       names(res) <- c("gen", sapply(seq_len(length(meta_i)), function(x)
#         paste("pop", x, sep="_")))
#     }
#     
#     for (i in seq_len(n.gen)){
#     #need to define i
#     out <- coev_div_single_gen(meta_i, meta_j, pars)
#     meta_i <- out$pop_i
#     meta_j <- out$pop_j
#     res_i <- c(i, sapply(meta_i, mean))
#     res_j <- c(i, sapply(meta_j, mean))
#     }










