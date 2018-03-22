#####Function to match up individuals (partnerships)#####
get_partners <- function(i,j){ 
  min_n <- min(length(i), length(j))
  ind_sp_i <- sample(seq_len(length(i)), min_n)
  ind_sp_j <- sample(seq_len(length(j)), min_n) 
  sp_i <- i[ind_sp_i]
  sp_j <- j[ind_sp_j]
  part <- data.frame(sp_i=sp_i, sp_j=sp_j)
  
  ## these are the leftovers
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
       rem_i=rem_i, rem_j=rem_j) 
}

#####Functions for fitness matching#####

#costly interaction
fitness_f_match_cost <- function(zeta, alpha, fit_diff){ 
  w <- -zeta * exp(-alpha * fit_diff^2)
  out <- list(w)
}

#beneficial interaction
fitness_f_match_ben <- function(zeta, alpha, fit_diff){ 
  w <- zeta * exp(-alpha * fit_diff^2)
  out <- list(w)
}

#####Functions for fitness differences#####

#costly interaction
fitness_f_diff_cost <- function(zeta, alpha, fit_diff){ 
  w <- -zeta / (1 + alpha * fit_diff^2)
  out <- list(w)
}

#beneficial interaction
fitness_f_diff_ben <- function(zeta, alpha, fit_diff){ 
  w <- zeta / (1 + alpha * fit_diff^2)
  out <- list(w)
}

#for mutualists, use both beneficial eq'ns
#for competitors, use both costly eq'ns
#for antagonists, use a beneficial eq'n for partner i and a costly eq'n for j

