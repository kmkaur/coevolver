#####create populations#####

ptm <- proc.time()
trial_pops <- build_starting_pop(pars=yoder_defaults())
pars <- trial_pops$pars
meta_i <- trial_pops$pops$meta_i
meta_j <- trial_pops$pops$meta_j
proc.time()-ptm 

######mating and reproduction#####
ptm <- proc.time()
newgen_i <- list()
for(i in 1:50){
  trial_pop_mr <- mate_repro(meta_i[i], pars$K_i[i], pars$v_s)
  newgen_i[i] <- trial_pop_mr
}

newgen_j <- list()
for(i in 1:50){
  trial_pop_mr <- mate_repro(meta_j[i], pars$K_j[i], pars$v_s)
  newgen_j[i] <- trial_pop_mr
}
proc.time()-ptm 

#####migration#####

ptm <- proc.time()
remix_i <- list()
for(i in 1:50){
  remix <- migrate_m(newgen_i[i], newgen_i[[i]], round(pars$m_i[i]*pars$K_i[i]))
  remix_i[[i]] <- remix
}

remix_j <- list()
for(i in 1:50){
  remix <- migrate_m(newgen_j[i], newgen_j[[i]], round(pars$m_j[i]*pars$K_j[i]))
  remix_j[[i]] <- remix
}
proc.time()-ptm 

#####abiotic sel#####

ptm <- proc.time()
post_sel_i <- list()
for(i in 1:50){
  post_sel <- abiotic_sel(remix_i[i], pars$theta_i[i], pars$gamma_i[i])
  post_sel_i[i] <- post_sel
}

post_sel_j <- list()
for(i in 1:50){
  post_sel <- abiotic_sel(remix_j[i], pars$theta_j[i], pars$gamma_j[i])
  post_sel_j[i] <- post_sel
}
proc.time()-ptm 

#####match individuals#####

ptm <- proc.time()
partners <- list()
for(i in 1:50){
  gp <- get_partners(post_sel_i[[i]], post_sel_j[[i]])
  partners[[i]] <- gp
}
proc.time()-ptm 

#####differences in trait values#####

ptm <- proc.time()
diff <- list()
for(i in 1:50){
  d <- partners[[i]]$part$sp_i-partners[[i]]$part$sp_j
  diff[[i]] <- d
}
proc.time()-ptm 

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
  fitness_matching_ben <- fitness_f_match_ben(pars$zeta_i[i], pars$alpha_i[i], fit_diff[[i]])
  fit_match_ben_i[[i]] <- fitness_matching_ben
}

fit_match_ben_j <- list()
for(i in 1:50){
  fitness_matching_ben <- fitness_f_match_ben(pars$zeta_j[i], pars$alpha_j[i], fit_diff[[i]])
  fit_match_ben_j[[i]] <- fitness_matching_ben
}
proc.time()-ptm 

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
proc.time()-ptm

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
proc.time()-ptm

list(post_bio_i, post_bio_j)

