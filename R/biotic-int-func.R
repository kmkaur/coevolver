## Match up individuals
#this works now
get_partners <- function(i,j){ 
  min_n <- min(length(i), length(j))
  ind_sp_i <- sample(seq_len(length(i)), min_n)
  ind_sp_j <- sample(seq_len(length(j)), min_n) 
  sp_i <- i[ind_sp_i]
  sp_j <- j[ind_sp_j]
  part <- data.frame(sp_i=sp_i, sp_j=sp_j)
  
  ## leftovers
  if (length(i) > length(j)){
    rem_i <- i[-ind_sp_i]
    rem_j <- NA
  } else if (length(i) < length(j)){
    rem_i <- NA
    rem_j <- j[-ind_sp_j]
  } else {
    rem_i <- NA
    rem_j <- NA
  }
  
  list(part = part, ind_sp_i=ind_sp_i, ind_sp_j=ind_sp_j,
       rem_i=rem_i, rem_j=rem_j) #fixing part, it now shows up
}


## Function for fitness matching
#need to figure out cost vs benefit
fitness_f_match <- function(zeta, alpha, fit_match){ 
  #if (interaction == "cost"){
  w <- -zeta * exp(-alpha * fit_match^2)
  #} else {
  w <- zeta * exp(-alpha * fit_match^2)
  out <- list(w)
}

## Function for fitness difference
#need to figure out cost vs benefit
fitness_f_diff <- function(zeta, alpha, fit_diff){ 
  #if (interaction == "cost"){
  w <- -pars$zeta[sp] / (1 + pars$alpha[sp] * fit_diff^2)
  #} else {
  w <- pars$zeta[sp] / (1 + pars$alpha[sp] * fit_diff^2)
  out <- list(w)
}

## Functions for evaluating fitnesses of partners
#I have started to re do this function
biotic_sel <- function(x, diff, sp, pars){

    if (pars$type == "match"){ #something buggy here, always is 0
         w  <- fitness_f_match(diff, sp, pars)
     } else {
         w <- fitness_f_diff(diff, sp, pars)
     }
    get_survivors(x, w)
}






####################

#first fix the fitness_f_match fxn:
#change zeta to zeta_i[a] and alpha to alpha_i[a] and 'a' needs to go from 1:50
#change diff to diff[[a]] and 'a' needs to go from 1:50
#length of partners[[a]]$part is nrow(partners[[a]]$part)
#define length_part <- nrow(partners[[a]]$part)
#diff[[a]][b] 'b' needs to go from 1:length_part in partners[[a]]$part
#so overall, diff[[a]][1:length_part], a goes from 1:50

#V2
#fitness_f_match <- function(diff, sp, pars){ 
#pars = pars, stays the same
#diff = diff, stays the same
#what would sp be?
fit_match <- list()
w1 <- list()
for(i in 1:50){
  length_part <- nrow(partners[[i]]$part)
  diff_1 <- diff[[i]][1:length_part]
  fit_match[[i]] <- diff_1
  #if (pars$int[sp] == "cost"){
  #define what pars$int is, currently NULL
  w <- pars$zeta_i[i] * exp(-pars$alpha_i[i] * fit_match[[i]][1:length_part]^2)
  #} else {
  w <- pars$zeta_i[i] * exp(-pars$alpha_i[i] * fit_match[[i]][1:length_part]^2)
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
  w <- -pars$zeta_i[i] / (1 + pars$alpha_i[i] * fit_diff[[i]][1:length_part]^2)
  #} else {
  w <- pars$zeta_i[i] / (1 + pars$alpha_i[i] * fit_diff[[i]][1:length_part]^2)
  w2[[i]] <- w
}
#}

#V1
## Matching fitness
#I have started to re do this function
fitness_f_match <- function(diff, sp, pars){
  if (pars$int[sp] == "cost"){ #something buggy here, always is 0
    w <- -pars$zeta[sp] * exp(-pars$alpha[sp] * diff^2)
  } else {
    w <-  pars$zeta[sp] * exp(-pars$alpha[sp] * diff^2)
  }
  w
}

## Difference fitness
#I have started to re do this function
fitness_f_diff <- function(diff, sp, pars){
  if (pars$int[sp] == "cost"){ #something buggy here, always is 0
    w <- -pars$zeta[sp] / (1 + pars$alpha[sp] * diff^2)
  } else {
    w <- pars$zeta[sp] / (1 + pars$alpha[sp] * diff^2)
  }
}


