## Function for simulating change in a population over a single generation

coev_div_single_gen <- function(meta_i, meta_j, pars){
    
    ## Mating and reproduction
    newgen_i <- mate_repro(pops$meta_i, pars$K_i, pars$v_s) #this works for the first pop only
    newgen_j <- mate_repro(pops$meta_j, pars$K_j, pars$v_s)

    ## Movement between sites
    remix_i <- migrate_m(newgen_i, round(pars$m_i*pars$K_i))
    remix_j <- migrate_m(newgen_j, round(pars$m_j*pars$K_j))

    ## Abiotic selection
    post_sel_i <- abiotic_sel(remix_i, pars$theta_i, pars$gamma_i)
    post_sel_j <- abiotic_sel(remix_j, pars$theta_j, pars$gamma_j)

    ## Biotic selection (depends on form)
    ## Match individuals
    partners <- lapply(seq_len(length(meta_i)), function(x)
                       get_partners(post_sel_i[[x]], post_sel_j[[x]]))
    ## Calculate differences in trait values
    diff <- lapply(partners$part, function(x) {x$sp_i - x$sp_j})
    ## Selection
    bio_sel_i <- biotic_sel(post_sel_i, diff, "i", pars)
    bio_sel_j <- biotic_sel(post_sel_j, diff, "j", pars)
    ## Add remainders
    bio_sel_i <- add_remainders(bio_sel_i, partners, pars)
    bio_sel_j <- add_remainders(bio_sel_j, partners, pars)

    ## Return final
    list(bio_sel_i, bio_sel_j)
}


## Function for simulating across multiple generations

coev_div <- function(pars, n.gen, burnin=FALSE, burnin.gen=200, print=FALSE){

    sp <- get_starting_pop(pars)
    meta_i <- sp$meta_i
    meta_j <- sp$meta_j

    ## Burnin -- ramp up selection pressures over time
    if (burnin){
        n.gen <- n.gen - burnin.gen

        alpha_i <- seq(0, pars$alpha["i"], by=pars$alpha["i"]/(burnin.gen-1))
        alpha_j <- seq(0, pars$alpha["j"], by=pars$alpha["j"]/(burnin.gen-1))
        gamma_i <- seq(0, pars$gamma["i"], by=pars$gamma["i"]/(burnin.gen-1))
        gamma_j <- seq(0, pars$gamma["j"], by=pars$gamma["j"]/(burnin.gen-1))

        for (k in seq_len(burning.gen)){
            pars$alpha["i"] <- alpha_i[k]
            pars$alpha["j"] <- alpha_j[k]
            pars$gamma["i"] <- gamma_i[k]
            pars$gamma["j"] <- gamma_j[k]

            out <- coev_div_single_gen(meta_i, meta_j, pars)
            meta_i <- out$bio_sel_i
            meta_j <- out$bio_sel_j
        }
    }

    res_i <- c(0, sapply(meta_i, mean))
    res_j <- c(0, sapply(meta_j, mean))
    res   <- rbind(res_i, res_j)
    names(res) <- c("gen", sapply(seq_len(length(meta_i)), function(x)
        paste("pop", x, sep="_")))
    
    
    for (i in seq_len(n.gen)){
        out <- coev_div_single_gen(meta_i, meta_j, pars)
        meta_i <- out$bio_sel_i
        meta_j <- out$bio_sel_j
        res_i <- c(i, sapply(meta_i, mean))
        res_j <- c(i, sapply(meta_j, mean))
    }
                    
}        
    



    

