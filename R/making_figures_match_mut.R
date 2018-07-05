#load packages
library(ggplot2)
library(cowplot)

#making data structure work for figure 3 and 4 eqiuvalent
setwd("~/desktop/out_mut_match_new_var_try_1")

list.filenames<-list.files(pattern=".rds")
list.data <-list()

#read rds files
for (i in 1:length(list.filenames)){
  list.data[[i]]<-readRDS(list.filenames[i])
}

#####STEP 1:pooled variances#####

#import global variances over time
global_vari <- list()
for(i in 1:length(list.data)){
  var <- as.matrix(list.data[[i]]$global_var_i)
  global_vari[[i]] <- var
}

global_varj <- list()
for(i in 1:length(list.data)){
  var <- as.matrix(list.data[[i]]$global_var_j)
  global_varj[[i]] <- var
}

#try to plot global variances over time
global_all_runs_i <- list()
for(i in 1:10){
runs <- data.frame(x=1:1000, y=global_vari[[i]][1,], xlab = "Time", ylab = "Global Variance")
global_all_runs_i[[i]] <- runs
}

global_all_runs_j <- list()
for(i in 1:10){
  runs <- data.frame(x=1:1000, y=global_varj[[i]][1,], xlab = "Time", ylab = "Global Variance")
  global_all_runs_j[[i]] <- runs
}

gb_plots_i <- ggplot()
for (i in 1:10){
  gb_plots_i <- gb_plots_i + geom_point(data=global_all_runs_i[[i]], aes(x,y), size = 0.5)
}
gb_plots_i

gb_plots_j <- ggplot()
for (i in 1:10){
  gb_plots_j <- gb_plots_j + geom_point(data=global_all_runs_j[[i]], aes(x,y), size = 0.5)
}
gb_plots_j


#global end variances, not by time
global_end_var_i <- list()
for(i in 1:length(list.data)){
  var_i <- as.data.frame(list.data[[i]]$global_var_i)
  global_end_var <- mean(var_i[,1000], na.rm=TRUE)
  global_end_var_i[[i]] <- global_end_var
}

global_end_var_j <- list()
for(i in 1:length(list.data)){
  var_j <- as.data.frame(list.data[[i]]$global_var_j)
  global_end_var <- mean(var_j[,1000], na.rm=TRUE)
  global_end_var_j[[i]] <- global_end_var
}

global_end_var_i <- data.frame(unlist(global_end_var_i))
global_end_var_j <- data.frame(unlist(global_end_var_j))
mm_global_end_var_i <- qplot(global_end_var_i[,1], geom="histogram", binwidth = 0.02, 
                      xlab = "Final Variance 'i'", ylab = "Simulations", xlim = c(-0.02,0.4))
mm_global_end_var_j <- qplot(global_end_var_j[,1], geom="histogram", binwidth = 0.02, 
                      xlab = "Final Variance 'j'", ylab = "Simulations", xlim = c(-0.02,0.4))
plot_grid(mm_global_end_var_i, mm_global_end_var_j)

glob_end_var <- cbind(global_end_var_i, global_end_var_j)
write.csv(glob_end_var, file = "matching_mutualism_global_variance.csv")

#####Step 2: separated variances#####

#import variances over time
vari <- list()
for(i in 1:length(list.data)){
  var <- as.matrix(list.data[[i]]$pop_var_i)
  vari[[i]] <- var
}

varj <- list()
for(i in 1:length(list.data)){
  var <- as.matrix(list.data[[i]]$pop_var_j)
  varj[[i]] <- var
}

#try to plot global variances over time
all_runs_i <- list()
for(i in 1:10){
  runs <- data.frame(x=1:1000, y=vari[[i]][1,], xlab = "Time", ylab = "Global Variance")
  all_runs_i[[i]] <- runs
}

all_runs_j <- list()
for(i in 1:10){
  runs <- data.frame(x=1:1000, y=varj[[i]][1,], xlab = "Time", ylab = "Global Variance")
  all_runs_j[[i]] <- runs
}

plots_i <- ggplot()
for (i in 1:10){
  plots_i <- plots_i + geom_point(data=all_runs_i[[i]], aes(x,y), size = 0.5)
}
plots_i

plots_j <- ggplot()
for (i in 1:10){
  plots_j <- plots_j + geom_point(data=all_runs_j[[i]], aes(x,y), size = 0.5)
}
plots_j

#find mean end variance, not by time
end_variances_i <- list()
for(i in 1:length(list.data)){
  var_i <- as.data.frame(list.data[[i]]$pop_var_i)
  end_var <- mean(var_i[,1000], na.rm=TRUE)
  end_variances_i[[i]] <- end_var
}

end_variances_j <- list()
for(i in 1:length(list.data)){
  var_j <- as.data.frame(list.data[[i]]$pop_var_j)
  end_var <- mean(var_j[,1000], na.rm=TRUE)
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

#####STEP 3:pooled means#####

#import global means over time
global_meani <- list()
for(i in 1:length(list.data)){
  mean <- as.matrix(list.data[[i]]$global_mean_i)
  global_meani[[i]] <- mean
}

global_meanj <- list()
for(i in 1:length(list.data)){
  mean <- as.matrix(list.data[[i]]$global_mean_j)
  global_meanj[[i]] <- mean
}

#try to plot global means over time
mglobal_all_runs_i <- list()
for(i in 1:10){
  runs <- data.frame(x=1:1000, y=global_meani[[i]][1,], xlab = "Time", ylab = "Global Mean")
  mglobal_all_runs_i[[i]] <- runs
}

mglobal_all_runs_j <- list()
for(i in 1:10){
  runs <- data.frame(x=1:1000, y=global_meanj[[i]][1,], xlab = "Time", ylab = "Global Mean")
  mglobal_all_runs_j[[i]] <- runs
}

mgb_plots_i <- ggplot()
for (i in 1:10){
  mgb_plots_i <- mgb_plots_i + geom_point(data=mglobal_all_runs_i[[i]], aes(x,y), size = 0.5)
}
mgb_plots_i

mgb_plots_j <- ggplot()
for (i in 1:10){
  mgb_plots_j <- mgb_plots_j + geom_point(data=mglobal_all_runs_j[[i]], aes(x,y), size = 0.5)
}
mgb_plots_j


#global end means, not by time
global_end_mean_i <- list()
for(i in 1:length(list.data)){
  mean_i <- as.data.frame(list.data[[i]]$global_mean_i)
  global_end_mean <- mean(mean_i[,1000], na.rm=TRUE)
  global_end_mean_i[[i]] <- global_end_mean
}

global_end_mean_j <- list()
for(i in 1:length(list.data)){
  mean_j <- as.data.frame(list.data[[i]]$global_mean_j)
  global_end_mean <- mean(mean_j[,1000], na.rm=TRUE)
  global_end_mean_j[[i]] <- global_end_mean
}

global_end_mean_i <- data.frame(unlist(global_end_mean_i))
global_end_mean_j <- data.frame(unlist(global_end_mean_j))
mm_global_end_mean_i <- qplot(global_end_mean_i[,1], geom="histogram", binwidth = 0.02, 
                             xlab = "Final Mean 'i'", ylab = "Simulations")
mm_global_end_mean_j <- qplot(global_end_mean_j[,1], geom="histogram", binwidth = 0.02, 
                             xlab = "Final Mean 'j'", ylab = "Simulations")
plot_grid(mm_global_end_mean_i, mm_global_end_mean_j)

glob_end_mean <- cbind(global_end_mean_i, global_end_mean_j)
write.csv(glob_end_var, file = "matching_mutualism_global_mean.csv")

#####Step 4: separated means#####

#import variances over time
meani <- list()
for(i in 1:length(list.data)){
  m <- as.matrix(list.data[[i]]$pop_means_i)
  meani[[i]] <- m
}

meanj <- list()
for(i in 1:length(list.data)){
  m <- as.matrix(list.data[[i]]$pop_means_j)
  meanj[[i]] <- m
}

#try to plot global variances over time
mall_runs_i <- list()
for(i in 1:10){
  runs <- data.frame(x=1:1000, y=meani[[i]][1,], xlab = "Time", ylab = "Global Mean")
  mall_runs_i[[i]] <- runs
}

mall_runs_j <- list()
for(i in 1:10){
  runs <- data.frame(x=1:1000, y=meanj[[i]][1,], xlab = "Time", ylab = "Global Mean")
  mall_runs_j[[i]] <- runs
}

m_plots_i <- ggplot()
for (i in 1:10){
  m_plots_i <- m_plots_i + geom_point(data=mall_runs_i[[i]], aes(x,y), size = 0.5)
}
m_plots_i

m_plots_j <- ggplot()
for (i in 1:10){
  m_plots_j <- m_plots_j + geom_point(data=mall_runs_j[[i]], aes(x,y), size = 0.5)
}
m_plots_j

#find non global end mean, not by time
end_means_i <- list()
for(i in 1:length(list.data)){
  mean_i <- as.data.frame(list.data[[i]]$pop_means_i)
  end_meann <- mean(mean_i[,1000], na.rm=TRUE)
  end_means_i[[i]] <- end_meann
}

end_means_j <- list()
for(i in 1:length(list.data)){
  mean_j <- as.data.frame(list.data[[i]]$pop_means_j)
  end_mean <- mean(mean_j[,1000], na.rm=TRUE)
  end_means_j[[i]] <- end_mean
}

end_means_i <- t(as.data.frame(end_means_i))
end_means_j <- t(as.data.frame(end_means_j))
mean_i_fig_mm <- qplot(end_means_i[,1], geom="histogram", binwidth = 0.02, 
                      xlab = "Final Mean", ylab = "Simulations")
mean_j_fig_mm <- qplot(end_means_j[,1], geom="histogram", binwidth = 0.02, 
                      xlab = "Final Mean", ylab = "Simulations")
plot_grid(mean_i_fig_mm, mean_j_fig_mm)

end_means <- cbind(end_means_i, end_means_j)
write.csv(end_var, file = "matching_mutualism_means.csv")






