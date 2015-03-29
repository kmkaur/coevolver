## Functions for evaluating fitnesses of partners
biotic_sel <- function(x, diff, sp, pars){

    if (pars$type == "match"){
        w  <- fitness_f_match(diff, sp, pars)
    } else {
        w <- fitness_f_diff(diff, sp, pars)
    }
    get_survivors(x, w)
}

## Matching fitness
fitness_f_match <- function(diff, sp, pars){
    if (pars$int[sp] == "cost"){
       w <- -pars$zeta[sp] * exp(-pars$alpha[sp] * diff^2)
    } else {
       w <-  pars$zeta[sp] * exp(-pars$alpha[sp] * diff^2)
    }
    w
}

## Difference fitness
fitness_f_diff <- function(diff, sp, pars){
    if (pars$int[sp] == "cost"){
        w <- -pars$zeta[sp] / (1 + pars$alpha[sp] * diff^2)
    } else {
        w <- pars$zeta[sp] / (1 + pars$alpha[sp] * diff^2)
    }
}


## Match up individuals
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
      
    list(part=part, ind_sp_i=ind_sp_i, ind_sp_j=ind_sp_j,
         rem_i=rem_i, rem_j=rem_j)
}





                  
                  
        
