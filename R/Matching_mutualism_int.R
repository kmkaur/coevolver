#####create populations#####

ptm <- proc.time()
trial_pops <- build_starting_pop(pars=yoder_defaults())
meta_i <- trial_pops$pops$meta_i
meta_j <- trial_pops$pops$meta_j
proc.time()-ptm #0.003

######mating and reproduction#####
ptm <- proc.time()
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
proc.time()-ptm #0.047

#####migration#####

ptm <- proc.time()
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
proc.time()-ptm #0.052

#####abiotic sel#####

ptm <- proc.time()
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
proc.time()-ptm #0.002

#####match individuals#####

ptm <- proc.time()
partners <- list()
for(i in 1:50){
  gp <- get_partners(post_sel_i[[i]], post_sel_j[[i]])
  partners[[i]] <- gp
}
proc.time()-ptm #0.001

#####differences in trait values#####

ptm <- proc.time()
diff <- list()
for(i in 1:50){
  d <- partners[[i]]$part$sp_i-partners[[i]]$part$sp_j
  diff[[i]] <- d
}
proc.time()-ptm #0.001

#####fitness matching#####

ptm <- proc.time()
fit_diff <- list()
w1 <- list()
for(i in 1:50){
  length_part <- nrow(partners[[i]]$part)
  diff_1 <- diff[[i]][1:length_part]
  fit_diff[[i]] <- diff_1}

fit_match_ben_i <- list()
for(i in 1:50){
  fitness_matching_ben <- fitness_f_match_ben(test_pars$zeta_i[i], test_pars$alpha_i[i], fit_diff[[i]])
  fit_match_ben_i[[i]] <- fitness_matching_ben
}

fit_match_ben_j <- list()
for(i in 1:50){
  fitness_matching_ben <- fitness_f_match_ben(test_pars$zeta_j[i], test_pars$alpha_j[i], fit_diff[[i]])
  fit_match_ben_j[[i]] <- fitness_matching_ben
}
proc.time()-ptm #0.002

###survivors and remainders###

ptm <- proc.time()
surv_match_ben_i <- list()
for(i in 1:50){
  survivors <- get_survivors(post_sel_i[i], fit_match_ben_i[[i]])
  surv_match_ben_i[[i]] <- survivors
}

surv_match_ben_j <- list()
for(i in 1:50){
  survivors <- get_survivors(post_sel_j[i], fit_match_ben_j[[i]])
  surv_match_ben_j[[i]] <- survivors
}
proc.time()-ptm #0.002

#####add remainders#####

ptm <- proc.time()
post_bio_i <- list()
for(i in 1:50){
  post_bio <- c(unlist(surv_match_ben_i[[i]]), partners[[i]]$rem_i)
  post_bio_i[[i]] <- post_bio[!is.na(post_bio)]
}

post_bio_j <- list()
for(i in 1:50){
  post_bio <- c(unlist(surv_match_ben_j[[i]]), partners[[i]]$rem_j)
  post_bio_j[[i]] <- post_bio[!is.na(post_bio)]
}
proc.time()-ptm #0.001

i_and_j <- list(post_bio_i, post_bio_j)