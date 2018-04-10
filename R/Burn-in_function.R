#####Function for simulating across multiple generations#####
#STILL working on this one!

#build starting pars
require(gmodels)
require(ggplot2)
trial_pops <- build_starting_pop(pars=yoder_2_pop())
pars <- trial_pops$pars
meta_i <- trial_pops$pops$meta_i
meta_j <- trial_pops$pops$meta_j

coev_div <- function(pars, n.gen, burnin=FALSE, burnin.gen, print=FALSE){
  
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
    

    ##this is running the simulation for the burnin gen only
    meta_i_list <- list()
    meta_j_list <- list()
    for (p in 1:burnin.gen){
      pars$alpha_i <- alpha_i_burn[,p]
      pars$alpha_j <- alpha_j_burn[,p]
      pars$gamma_i <- gamma_i_burn[,p]
      pars$gamma_j <- gamma_j_burn[,p]
      run <- coev_div_single_gen(meta_i, meta_j, pars)
      meta_i <- run$pop_i
      meta_j <- run$pop_j
      meta_i_list[[p]] <- meta_i
      meta_j_list[[p]] <- meta_j
    }
  }
  
  ##run the simulation for the remaining gen (n.gen <- burnin.gen- n.gen)
  #final burnin.gen value is the value we want for the remaining generations
    pars$alpha_i <- alpha_i_burn[,burnin.gen] 
    pars$alpha_j <- alpha_j_burn[,burnin.gen] 
    pars$gamma_i <- gamma_i_burn[,burnin.gen]
    pars$gamma_j <- gamma_j_burn[,burnin.gen]
    
    meta_i_list2 <- list()
    meta_j_list2 <- list()
    for (q in length(n.gen)){
      out <- coev_div_single_gen(meta_i, meta_j, pars)
      meta_i <- out$pop_i
      meta_j <- out$pop_j
      meta_i_list2[[q]] <- meta_i
      meta_j_list2[[q]] <- meta_j
    }
    
    #NOW, meta_i_list and meta_j_list have the generations 1:burnin.gen
    #AND, meta_i_list2 and meta_j_list2 have the generations n.gen (n.gen-burnin.gen)
    
    #combine lists so entire output has total gen information
    all_gens_i <- c(meta_i_list, meta_i_list2)
    all_gens_j <- c(meta_j_list, meta_j_list2)
    
    #need to calculate 
    #1) x = generation #  and y = mean phenotype for i and for j and 95% CI
  
    pop_means <- matrix(ncol=length(all_gens), nrow=pars$N)
    pop_var <- matrix(ncol=length(all_gens), nrow=pars$N)
    #columns=gen# and rows=pop#
 
    for(q in 1:length(all_gens)){
        for(r in 1:pars$N){
          pop_means[r,q] <- mean(all_gens[[q]][[r]])
          pop_var[r,q] <- var(all_gens[[q]][[r]])
        }
    }
    end_var <- as.data.frame(pop_var[,5]) 
    colnames(end_var) <- c("Final Variance")
    pop_means <- as.data.frame(pop_means)
    list(all_gens = all_gens, final_variance = end_var, pop_means = pop_means )
}

#make final variance figure
out <- coev_div(pars, n.gen = 5, burnin = FALSE, burnin.gen = 3, print=FALSE)
var_graph <- qplot(out$final_variance$`Final Variance`, geom = "histogram", binwidth = 0.02, 
                   xlab = "Final Variance", ylab = "Simulations")




#     for (j in 1:n.gen){
#       #need to define j
#       #need to find the mean of each population under each generation?
#       res_i <- c(0, sapply(meta_i_list[[j]], mean))
#       res_j <- c(0, sapply(meta_j_list[[j]], mean))
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
#     
# }        
# 
# 
# 










