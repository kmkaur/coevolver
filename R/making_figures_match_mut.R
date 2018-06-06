#load packages
library(ggplot2)
library(cowplot)

#making data structure work for figure 3 eqiuvalent
setwd("~/simulation_output/out_mut_match")

list.filenames<-list.files(pattern=".rds")
list.data <-list()

#read rds files
for (i in 1:length(list.filenames)){
  list.data[[i]]<-readRDS(list.filenames[i])
}

#find mean end variance
end_variances_i <- list()
for(i in 1:length(list.data)){
  var_i <- as.data.frame(list.data[[i]]$pop_var_i)
  end_var <- mean(var_i[,1000], na.rm=TRUE)
  end_variances_i[[i]] <- end_var
}

end_variances_j <- list()
for(i in 1:length(list.data)){
  var_i <- as.data.frame(list.data[[i]]$pop_var_j)
  end_var <- mean(var_i[,1000], na.rm=TRUE)
  end_variances_j[[i]] <- end_var
}

end_var_i <- t(as.data.frame(end_variances_i))
end_var_j <- t(as.data.frame(end_variances_j))
var_i_fig_mm <- qplot(end_var_i[,1], geom="histogram", binwidth = 0.02, 
           xlab = "Final Variance", ylab = "Simulations", xlim = c(-0.02,0.4))
var_j_fig_mm <- qplot(end_var_j[,1], geom="histogram", binwidth = 0.02, 
                   xlab = "Final Variance", ylab = "Simulations", xlim = c(-0.02,0.4))
plot_grid(var_i_fig_mm, var_j_fig_mm)

end_var <- cbind(end_var_i, end_var_j)
write.csv(end_var, file = "matching_mutualism_variance.csv")



