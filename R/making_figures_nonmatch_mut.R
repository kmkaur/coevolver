#load packages
library(ggplot2)
library(cowplot)

#making data structure work figure 4 eqiuvalent
i_sim <- read.csv(file="match comp i.csv", row.names = 1)
j_sim <- read.csv(file="match comp j.csv", row.names = 1)
generations <- 1:1000
df_i <- data.frame(generations, i_sim$V1)
df_j <- data.frame(generations, j_sim$V1)
plot_i_nm <- ggplot(data=df_i, aes(x=generations, y=i_sim$V1)) +
  geom_point() + geom_line() + ylim(-2,2) + xlim(0, 1000) + xlab("Generations") + ylab("Mean Phenotype") + ggtitle("Mean Non-Matching Mutualism Species i")
plot_j_nm <- ggplot(data=df_j, aes(x=generations, y=j_sim$V1)) +
  geom_point() + geom_line() + ylim(-2,2) + xlim(0, 1000) + xlab("Generations") + ylab("Mean Phenotype") + ggtitle("Mean Non-Matching Mutualism Species i")

#making data structure work figure 3 eqiuvalent
setwd("~/simulation_output/out_mut_match/")
list.filenames<-list.files(pattern=".rds")
list.data.i<-list()
list.data.j<-list()

#read rds files
for (i in 1:length(list.filenames)){
  list.data.i[[i]]<-readRDS(list.filenames[i])
}

for (i in 1:length(list.filenames)){
  list.data.j[[i]]<-readRDS(list.filenames[i])
}





#find mean end variance
end_variances_i <- list()
for(i in 1:length(list.data.i)){
  var_i <- as.data.frame(list.data.i[[i]]$pop_var_i)
  end_var <- mean(var_i[,10], na.rm=TRUE)
  end_variances_i[[i]] <- end_var
}

end_variances_j <- list()
for(i in 1:length(list.data.j)){
  var_i <- as.data.frame(list.data.j[[i]]$pop_var_j)
  end_var <- mean(var_i[,10], na.rm=TRUE)
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
write.csv(end_var, file = "nonmatching_mutualism_variance.csv")
