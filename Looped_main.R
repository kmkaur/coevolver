
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

#####biotic selection#####

n <- min(length(meta_i[[1]]), length(meta_j[[1]]))
sample(meta_i[[1]], n, replace = FALSE)
sample(meta_j[[1]], n, replace = FALSE)

#first fix the fitness_f_match fxn:
  #change zeta to zeta_i[a] and alpha to alpha_i[a] and 'a' needs to go from 1:50
  #change diff to diff[[a]] and 'a' needs to go from 1:50
  #length of partners[[a]]$part is nrow(partners[[a]]$part)
  #define length_part <- nrow(partners[[a]]$part)
  #diff[[a]][b] 'b' needs to go from 1:length_part in partners[[a]]$part
  #so overall, diff[[a]][1:length_part], a goes from 1:50

#fitness_f_match <- function(diff, sp, pars){ 
    #decide what goes into the function
    fit_match <- list()
    w1 <- list()
  for(i in 1:50){
    length_part <- nrow(partners[[i]]$part)
    diff_1 <- diff[[i]][1:length_part]
    fit_match[[i]] <- diff_1
    #if (pars$int[sp] == "cost"){
        #define what pars$int is, currently NULL
    w <- -test_pars$zeta_i[i] * exp(-test_pars$alpha_i[i] * fit_match[[i]][1:length_part]^2)
    #} else {
    w <- test_pars$zeta_i[i] * exp(-test_pars$alpha_i[i] * fit_match[[i]][1:length_part]^2)
    w1[[i]] <- w
  }
#}  
  
#fitness_f_diff <- function(diff, sp, pars){
    #decide what goes into the function
    fit_diff <- list()
    w2 <- list()
  for(i in 1:50){
    length_part <- nrow(partners[[i]]$part)
    diff_1 <- diff[[i]][1:length_part]
    fit_diff[[i]] <- diff_1
    #if (pars$int[sp] == "cost"){ 
     #define what pars$int is, currently NULL
    w <- -test_pars$zeta_i[i] / (1 + test_pars$alpha_i[i] * fit_match[[i]][1:length_part]^2)
    #} else {
    w <- test_pars$zeta_i[i] / (1 + test_pars$alpha_i[i] * fit_match[[i]][1:length_part]^2)
    w2[[i]] <- w
  }
 #}
  
#biotic_sel <- function(x, diff, sp, pars){
#      if (pars$type == "match"){ #something buggy here, always is 0, define pars$type
#       w  <- fitness_f_match(diff, sp, pars)
#      } else {
#        w <- fitness_f_diff(diff, sp, pars)
#      }
#      get_survivors(x, w)
#    }

#####add remainders#####
    #need to define a function first



    
   

