#####create populations#####
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
  remix <- migrate_m(newgen_i[i], newgen_i[[i]], round(trial_pops$pars$m_i[i]*trial_pops$pars$K_i[i]))
  remix_i[[i]] <- remix
}

remix_j <- list()
for(i in 1:50){
  remix <- migrate_m(newgen_j[i], newgen_i[[i]], round(trial_pops$pars$m_j[i]*trial_pops$pars$K_j[i]))
  remix_j[[i]] <- remix
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
partners <- list()
for(i in 1:50){
  gp <- get_partners(post_sel_i[[i]], post_sel_j[[i]])
  partners[[i]] <- gp
}

#####differences in trait values#####
diff <- list()
for(i in 1:50){
  d <- partners[[i]]$part$sp_i-partners[[i]]$part$sp_j
  diff[[i]] <- d
}

#####fitness differences#####
fit_diff <- list()
w1 <- list()
for(i in 1:50){
  length_part <- nrow(partners[[i]]$part)
  diff_1 <- diff[[i]][1:length_part]
  fit_diff[[i]] <- diff_1}

fit_diff_cost_i <- list()
for(i in 1:50){
  fitness_difference_cost <- fitness_f_diff_cost(test_pars$zeta_i[i], test_pars$alpha_i[i], fit_diff[[i]])
  fit_diff_cost_i[[i]] <- fitness_difference_cost
}


fit_diff_ben_j <- list()
for(i in 1:50){
  fitness_difference_ben <- fitness_f_diff_ben(test_pars$zeta_j[i], test_pars$alpha_j[i], fit_diff[[i]])
  fit_diff_ben_j[[i]] <- fitness_difference_ben
}

###survivors and remainders###
surv_m_c_i <- list()
for(i in 1:50){
  survivors <- get_survivors(post_sel_i[i], fit_diff_cost_i[[i]])
  surv_m_c_i[[i]] <- survivors
}

surv_m_c_j <- list()
for(i in 1:50){
  survivors <- get_survivors(post_sel_j[i], fit_diff_ben_j[[i]])
  surv_m_c_j[[i]] <- survivors
}

#####add remainders#####
post_bio_i <- list()
for(i in 1:50){
  post_bio <- c(unlist(surv_m_c_i[[i]]), partners[[i]]$rem_i)
  post_bio_i[[i]] <- post_bio[!is.na(post_bio)]
}

post_bio_j <- list()
for(i in 1:50){
  post_bio <- c(unlist(surv_m_c_j[[i]]), partners[[i]]$rem_j)
  post_bio_j[[i]] <- post_bio[!is.na(post_bio)]
}

i_and_j <- list(post_bio_i, post_bio_j)
