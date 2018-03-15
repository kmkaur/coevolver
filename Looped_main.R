
trial_pops <- build_starting_pop(pars=yoder_defaults())

meta_i <- trial_pops$pops$meta_i
meta_j <- trial_pops$pops$meta_j

######mating and reproduction#####
newgen_i <- list()
for(i in 1:50){
  
  trial_pop_mr <- mate_repro(meta_i[i], trial_pops$pars$K_i[i], trial_pops$pars$v_s)
  newgen_i[i] <- trial_pop_mr
}

newgen_j <- list()
for(i in 1:50){
  
  trial_pop_mr <- mate_repro(meta_j[i], trial_pops$pars$K_j[i], trial_pops$pars$v_s)
  newgen_j[i] <- trial_pop_mr
}

#####migration#####
remix_i <- list()
for(i in 1:50){
  
  remix_i <- migrate_m(newgen_i, round(trial_pops$pars$m_i[i]*trial_pops$pars$K_i[i]))
}

remix_j <- list()
for(i in 1:50){
  
  remix_j <- migrate_m(newgen_j, round(trial_pops$pars$m_j[i]*trial_pops$pars$K_j[i]))
} 

#####abiotic sel#####
post_sel_i <- list()
for(i in 1:50){
  post_sel <- abiotic_sel(remix_i[i], trial_pops$pars$theta_i[i], trial_pops$pars$gamma_i[i])
  post_sel_i[i] <- post_sel
}

post_sel_j <- list()
for(i in 1:50){
  post_sel <- abiotic_sel(remix_j[i], trial_pops$pars$theta_j[i], trial_pops$pars$gamma_j[i])
  post_sel_j[i] <- post_sel
}

#####match individuals#####
partners <- lapply(seq_len(length(meta_i)), function(x)
  get_partners(post_sel_i[[x]], post_sel_j[[x]]))

#####differences in trait values#####
diff <- list()
for(i in 1:50){
  d <- partners[[i]]$part$sp_i-partners[[i]]$part$sp_j
  diff[[i]] <- d
}

#####fitness matching#####
fit_match <- list()
w1 <- list()
for(i in 1:50){
  length_part <- nrow(partners[[i]]$part)
  diff_1 <- diff[[i]][1:length_part]
  fit_match[[i]] <- diff_1}

fitness_matching_output <- list()
for(i in 1:50){
  fitness_matching_out <- fitness_f_match(test_pars$zeta_i[i], test_pars$alpha_i[i], fit_match[[i]])
  fitness_matching_output[[i]] <- fitness_matching_out
}

#####fitness differences#####
fit_diff <- list()
w1 <- list()
for(i in 1:50){
  length_part <- nrow(partners[[i]]$part)
  diff_1 <- diff[[i]][1:length_part]
  fit_diff[[i]] <- diff_1}

fitness_diff_output <- list()
for(i in 1:50){
  fitness_diff_out <- fitness_f_match(test_pars$zeta_i[i], test_pars$alpha_i[i], fit_diff[[i]])
  fitness_diff_output[[i]] <- fitness_diff_out
}
  
#biotic_sel <- function(zeta, alpha, fit_match, fit_diff){
#      if (pars$type == "match"){ #something buggy here, always is 0, define pars$type
#       w  <- fitness_f_match(zeta, alpha, fit_match)
#      } else {
#       w <- fitness_f_diff(zeta, alpha, fit_diff)
#      }
#      get_survivors(x, w)
      
#    }

#####add remainders#####
    #get remainders that survived from partners and add them here



#####alternate way of getting partners and difference#####
#not looped yet
#currently not using this version
n <- min(length(meta_i[[1]]), length(meta_j[[1]]))
partner_i <- sample(meta_i[[1]], n, replace = FALSE)
partner_j <- sample(meta_j[[1]], n, replace = FALSE)
partners_list <- cbind.data.frame(partner_i, partner_j)
difference <- partners_list$partner_i-partners_list$partner_j


    
   

