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

## Functions for fitness matching
fitness_f_match_cost <- function(zeta, alpha, fit_diff){ 
  w <- -zeta * exp(-alpha * fit_diff^2)
  out <- list(w)
}

fitness_f_match_ben <- function(zeta, alpha, fit_diff){ 
  w <- zeta * exp(-alpha * fit_diff^2)
  out <- list(w)
}

## Functions for fitness difference
fitness_f_diff_cost <- function(zeta, alpha, fit_diff){ 
  w <- -zeta / (1 + alpha * fit_diff^2)
  out <- list(w)
}

fitness_f_diff_ben <- function(zeta, alpha, fit_diff){ 
  w <- zeta / (1 + alpha * fit_diff^2)
  out <- list(w)
}

