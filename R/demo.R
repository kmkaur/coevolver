#####Function for mating and reproduction#####
mate_repro <- function(x, k, v_s) 
    lapply(x, function(y) mate_repro_k(y, k, v_s))

mate_repro_k <- function(x, k, v_s) 
    sapply(c(1:k), function(y) {
        mean(sample(x, size=2, replace=TRUE)) + rnorm(1, sd=v_s)})


#####Function for migration#####
#note that km = K*m

migrate_m <- function(x, km){ 
    ## migrant pool
    pool <- sapply(x, function(y) y[c(1:km)])
    ## distribute among metapopulations
    for (i in 1:length(x)){
        mkk <- sample(seq_len(length(pool)), size=km)
        x[[i]][c(1:km)] <- pool[mkk]
        pool <- pool[-mkk]
    }
    x
}

#####Function for abiotic selection#####
## N.B.: We ignore the possibility of local extinction
abiotic_sel <- function(x, theta, gamma){ 
    w <- lapply(seq_len(length(x)), function(y)
                 fitness_f(x[[y]], theta[y], gamma))
    get_survivors(x, w)
}

#####Fitnesses function (quadratic stabilizing selection)#####
fitness_f <- function(x, theta, gamma)
    exp(-gamma * (x - theta)^2)

get_survivors <- function(x, w){
    surv <- lapply(w, surv_id)
    lapply(seq_len(length(x)), function(y) {x[[y]][surv[[y]]]})
}
     

#####Function to get survivors#####
surv_id <- function(x)
    which(x > runif(1))
