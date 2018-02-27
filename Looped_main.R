
test_pars = get_parameters(yoder_defaults())
trial_pops = get_starting_pop(pars=test_pars)

meta_i <- trial_pops$meta_i
meta_j <- trial_pops$meta_j

#mating and reproduction
newgen_i <- list()
for(i in 1:50){
  
  trial_pop_mr <- mate_repro(meta_i[i], test_pars$K_i[i], test_pars$v_s)
  newgen_i[i] <- trial_pop_mr
}

newgen_j <- list()
for(i in 1:50){
  
  trial_pop_mr <- mate_repro(meta_j[i], test_pars$K_j[i], test_pars$v_s)
  newgen_j[i] <- trial_pop_mr
}

#migration
remix_i <- list()
for(i in 1:50){
  
  remix_i <- migrate_m(newgen_i, round(test_pars$m_i[i]*test_pars$K_i[i]))
}

remix_j <- list()
for(i in 1:50){
  
  remix_j <- migrate_m(newgen_j, round(test_pars$m_j[i]*test_pars$K_j[i]))
} 

#abiotic sel
post_sel_i <- list()
for(i in 1:50){
  post_sel <- abiotic_sel(remix_i[i], test_pars$theta_i[i], test_pars$gamma_i[i])
  post_sel_i[i] <- post_sel
}

post_sel_j <- list()
for(i in 1:50){
  post_sel <- abiotic_sel(remix_j[i], test_pars$theta_j[i], test_pars$gamma_j[i])
  post_sel_j[i] <- post_sel
}

partners <- lapply(seq_len(length(meta_i)), function(x)
  get_partners(post_sel_i[[x]], post_sel_j[[x]]))

diff_i <- list()
diff_j <- list()
diff <- list()
for(i in 1:50){
  d <- lapply(partners[[i]]$part, function(x) {(x[1] - x[2])})
  diff_i[i] <- d$sp_i
  diff_j[i] <- d$sp_j
}

diff <- cbind(diff_i,diff_j)











Listkxm <- as.list(test_pars$K_i*test_pars$m_i)
as.numeric(Listkxm[1])


mylist2 <- list()

for(i in 1:2){
  
trial_test_migrate_i <- migrate_m(pops_i[i], round(pars_test$m_i*pars_test$K_i))
}

trial_test_sel_i <- abiotic_sel(trial_test_migrate_i, pars$theta_i[1], pars$gamma_i[1])
