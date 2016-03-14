library(readr)
library(MASS)
library(ggplot2)
library(psych)
setwd("~/Scholars_Presentation")
rm(list=ls())

#Question 1A
raw <- read_csv("Prob_Set_2.csv")
raw <- raw/100
industry_returns <- as.matrix(raw[,2:11])

vcov_mat <- cov(industry_returns)
v_inv <- ginv(vcov_mat) 
mean_return <- sapply(as.list(as.data.frame(industry_returns+1)), geometric.mean) - 1
mean_rf <- geometric.mean(raw$`Risk-free rate` + 1) - 1
mean_return2 <- mean_return - mean_rf

one_v <- rep(1, 10)
mvp_weights <- (v_inv %*% one_v) / as.vector((t(one_v) %*% v_inv %*% one_v))
tan_weights <- (v_inv %*% mean_return2) / (as.vector(t(one_v) %*% v_inv %*% mean_return2))

mvp_return <- mean_return %*% mvp_weights
tan_return <- mean_return %*% tan_weights

mvp_sd <- (t(mvp_weights) %*% vcov_mat %*% mvp_weights)^(1/2)
tan_sd <- (t(tan_weights) %*% vcov_mat %*% tan_weights)^(1/2)

monthly_mvp <- industry_returns %*% mvp_weights
monthly_tan <- industry_returns %*% tan_weights
cov_two_port <- t(tan_weights) %*% vcov_mat %*% mvp_weights

weights <- seq(-5, 5, .01)
efficient_frontier <- data.frame(ret = numeric(), sd = numeric())
for (w in weights){
  multi_sd <- (w^2 * mvp_sd^2 + (1-w)^2 * tan_sd^2 + 2*w*(1-w)*cov_two_port)^(1/2)
  multi_return <- w*mvp_return + (1-w)*tan_return
  temp = data.frame(ret = multi_return, sd = multi_sd)
  efficient_frontier <- rbind(efficient_frontier, temp)
}

industry_sd1 <- sapply(as.list(as.data.frame(industry_returns)), sd)

industries <- data.frame(mean_r = mean_return, i_sd = industry_sd1)

first_plot <- ggplot(data = efficient_frontier, aes(x = sd, y = ret)) + 
              geom_point(color="firebrick") + 
              geom_point(aes(x=i_sd, y = mean_r), data = industries, color="blue") +
              labs(x="Standard Deviation", y="Return")

#For Questions 1D and 1E
num_months <- 60

#Question 1E
highest_weight2 <- numeric()
total_portfolio_boot <- data.frame(var_return = mvp_return, var_sd = mvp_sd, tangency_return = tan_return, tangency_sd = tan_sd)
for (i in 1:1000){
  indices <- sample(1:1069, num_months)
  predicted_IR <- industry_returns[indices,]
  vcov_mat_p <- cov(predicted_IR)
  v_inv_p <- ginv(vcov_mat_p) 
  mean_return_p <- sapply(as.list(as.data.frame(predicted_IR)), mean)
  mean_return_p2 <- mean_return_p - mean_rf
  
  mvp_weights_p <- (v_inv_p %*% one_v) / as.vector((t(one_v) %*% v_inv_p %*% one_v))
  tan_weights_p <- (v_inv_p %*% mean_return_p2) / (as.vector(t(one_v) %*% v_inv_p %*% mean_return_p2))
  
  mvp_return_p <- mean_return %*% mvp_weights_p
  tan_return_p <- mean_return %*% tan_weights_p
  
  mvp_sd_p <- (t(mvp_weights_p) %*% vcov_mat %*% mvp_weights_p)^(1/2)
  tan_sd_p <- (t(tan_weights_p) %*% vcov_mat %*% tan_weights_p)^(1/2)
  
  highest_weight2 <- c(highest_weight2, max(tan_weights_p))
  temp <- data.frame(var_return = mvp_return_p, var_sd = mvp_sd_p, tangency_return = tan_return_p, tangency_sd = tan_sd_p)
  total_portfolio_boot <- rbind(total_portfolio_boot, temp)
}

seventh_plot <- ggplot(data = total_portfolio_boot, aes(x=var_sd, y=var_return)) +
                geom_point(color = "firebrick") + labs(x="Standard Deviation", y="Return")
eigth_plot <- ggplot(data = total_portfolio_boot, aes(x=tangency_sd, y=tangency_return)) +
              geom_point(color = "firebrick") + labs(x="Standard Deviation", y="Return")
ninth_plot <- ggplot(data = total_portfolio_boot, aes(x=tangency_sd, y=tangency_return)) +
              geom_point(color = "firebrick") + xlim(0,.5) + ylim(-.025, .025) + labs(x="Standard Deviation", y="Return")

SR_tan <- (tan_return - mean_rf) / tan_sd

securities_line <- ggplot(data = efficient_frontier, aes(x = sd, y = ret)) + 
                   geom_point(color="firebrick") + 
                   geom_point(aes(x=i_sd, y = mean_r), data = industries, color="blue") +
                   geom_abline(intercept=mean_rf, slope = SR_tan) +
                   labs(x="Standard Deviation", y="Return")