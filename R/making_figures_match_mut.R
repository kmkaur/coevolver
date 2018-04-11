#load packages
library(ggplot2)

#making data structure work figure 4 eqiuvalent
i_sim <- read.csv(file="solo_sim_i.csv", row.names = 1)
j_sim <- read.csv(file="solo_sim_j.csv", row.names = 1)
averages_i <- i_sim[101,]
averages_i <- as.numeric(averages_i)
averages_j <- j_sim[101,] 
averages_j <- as.numeric(averages_j)
generations <- 1:1000
df_i <- data.frame(generations, averages_i)
df_j <- data.frame(generations, averages_j)
plot_i <- ggplot(data=df_i, aes(x=generations, y=averages_i)) +
  geom_point() + geom_line() + ylim(-2,2) + xlim(0, 1000) + xlab("Generations") + ylab("Mean Phenotype")

#making data structure work figure 3 eqiuvalent
setwd("/Users/Katrina/git_practice/coevolver/out")
list.filenames<-list.files(pattern=".rds")
list.data<-list()

#read rds files
for (i in 1:length(list.filenames)){
  list.data[[i]]<-readRDS(list.filenames[i])
}

#find mean end variance
end_variances <- list()
for(i in 1:length(list.data)){
  var_i <- as.data.frame(list.data[[i]]$pop_var_i)
  end_var <- mean(var_i[,10], na.rm=TRUE)
  end_variances[[i]] <- end_var
}

end_var <- t(as.data.frame(end_variances))
p <- qplot(end_var[,1], geom="histogram", binwidth = 0.02, 
           xlab = "Final Variance", ylab = "Simulations", xlim = c(-0.02,0.4))


