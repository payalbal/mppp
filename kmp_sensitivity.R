

set.seed(10)
default_par <- par()


## Load data
library(readxl)    
source("./analysis/rcode/mppp_functios.R")
kmp_data <- lapply(excel_sheets("./data/kmp_data.xlsx"), read_excel, path = "./data/kmp_data.xlsx")
names(kmp_data) <- c("threats", "assests", "indicators", "persistence_probability", "management_data", "monitoring_data")


## Remove uninformative indicators
ind_delete <- kmp_data$monitoring_data[which(with(kmp_data$monitoring_data, c(detect_firegrazing == 0 & detect_catpredation == 0 & detect_weeds == 0))),]$indicators_i
kmp_data$monitoring_data <- kmp_data$monitoring_data[!kmp_data$monitoring_data$indicators_i %in% ind_delete,]
kmp_data$indicators <- kmp_data$indicators[!kmp_data$indicators$indicator_i %in% ind_delete,]
kmp_data$indicators$newID <- 1:dim(kmp_data$indicators)[1]


## Define variables
detect_p <- with(kmp_data$monitoring_data, cbind(detect_p_firegrazing, detect_p_catpredation, detect_p_weeds))
detect_q <- with(kmp_data$monitoring_data, cbind(type1_q_firegrazing, type1_q_catpredation, type1_q_weeds))
ind_feasibility <- with(kmp_data$monitoring_data, monit_feasibility_F)
ind_cost <- with(kmp_data$monitoring_data, monit_relcost_R)
prior_impact <- with(kmp_data$management_data, prior_d)
manage_feasibility <- with(kmp_data$management_data, manage_feasibility_G)
manage_cost <- with(kmp_data$management_data, manage_cost_M)
manage_benefit <- as.vector(with(kmp_data$persistence_probability, colSums(nspecies_n * (cbind(peristence_m_firegrazing, peristence_m_catpredation, peristence_m_weeds) - persistence_m0_noaction))))
threats <- with(kmp_data$threats, threat_j)
indicators <- with(kmp_data$indicators, indicator_label)
newID <- with(kmp_data$indicators, newID)


## Estimate cost-effectiveness
ce <- costeff(detect_p, detect_q, ind_feasibility, ind_cost, prior_impact,
              manage_feasibility, manage_cost, manage_benefit, threats, indicators, newID)

## Estimate cumulative benefit, cumulative cost and index for diminishing threshold
cost <- cumsum(ce$exp_cost)
benefit <- cumsum(ce$exp_benefit)
dimidx <- find.dim.point(ce$exp_cost, ce$exp_benefit, threshold = 0.2)$idx


## Cost-effectiveness and ranking for original data
# pdf(paste("Dim.returns_", MCruns, ".pdf", sep=""))
par(mfrow=c(1,1), las=1, mar=c(5, 7, 4, 2) + 0.1)
plot(cost, benefit, lwd = 2, type = "l", xlab = "Cumulative expected cost",
     ylab = "Cumulative expected benefit", xlim = c(-50, max(cost)), 
     ylim = c(min(benefit), ceiling(max(benefit)/50)*50))
points(cost, benefit, pch=19)
segments(cost[dimidx], 0, cost[dimidx],
         benefit[dimidx],lty = 2)
segments(-100,benefit[dimidx], cost[dimidx],
         benefit[dimidx],lty = 2)
text(cost-30,benefit+15,
     paste(ce$ind_ranks))
text(cost[dimidx]-20,130, "diminishing returns threshold", srt=90)


## Set scenario parameters
MCruns <- 1000 # runs\\increase to 1000 for figures
error <- seq(0,50,2) # seq(10,90,10) # error\\seq(0,50,2) for figures
absolute <- TRUE # use of absolute values of metrics
metrics <- c("mar.benefit", "rank.corr", "exp.benefit",
             "cum.exp.cost", "n.ind", "diff.ind") # metrics used in sensitivity analysis
## Mertic ID for identification
  ## 1 = diminishing returns cut-off value
  ## 2 = spearman rank correlation of new and orignal ranks of all indicators
  ## 3 = expected benefit (cumulative benefit) of indicators selected using specific cut-off
  ## 4 = cumulative cost of indicators selected using specific cut-off
  ## 5 = number of indicators selected using specific cut-off
  ## 6 = number of new indicators


## Specify parameters to vary
ppp_vars <- c("detect_p", "detect_q", "ind_feasibility", "ind_cost", "prior_impact",
              "manage_feasibility", "manage_cost", "manage_benefit")


## Specify output objects
metric1 <- matrix(NA, MCruns, length(ppp_vars))
colnames(metric1) <- ppp_vars
metric2 <- matrix(NA, MCruns, length(ppp_vars))
colnames(metric2) <- ppp_vars
metric3 <- matrix(NA, MCruns, length(ppp_vars))
colnames(metric3) <- ppp_vars
metric4 <- matrix(NA, MCruns, length(ppp_vars))
colnames(metric4) <- ppp_vars
metric5 <- matrix(NA, MCruns, length(ppp_vars))
colnames(metric5) <- ppp_vars
metric6 <- matrix(NA, MCruns, length(ppp_vars))
colnames(metric6) <- ppp_vars
error_outputs <- list()


## Start loop for error levels
for (E in 1:length(error)) {

  ## Start loop for parameters
  for (p in 1:length(ppp_vars)) {
    param <- get(ppp_vars[p])
    
    ## Start loop for MC runs
    for (run in 1:MCruns) {
      
      ## Sample parameter values for each MC run
      if(all(dplyr::between(range(param, na.rm = TRUE), 0, 1))) {
        param_type = "probability"
      } else if (all(dplyr::between(range(param, na.rm = TRUE), 0, 100))) {
        param_type = "percent"
      } else {
        param_type = "unconstrained"
      }
      
      new_var <- param
      new_var[which(!is.na(new_var))] <- NA
      for (i in which(!is.na(param))) {
        new_var[i] <- unif.param(param[i],error[E], type = param_type)
      }
      
      ## Estimate cost-effectiveness for new data
      if (ppp_vars[p] == "detect_p") {
        temp <- costeff(new_var, detect_q, ind_feasibility, ind_cost, 
                        prior_impact, manage_feasibility, manage_cost, 
                        manage_benefit, threats, indicators, newID)
        
      } else if (ppp_vars[p] == "detect_q") {
        temp <- costeff(detect_p, new_var, ind_feasibility, ind_cost, 
                        prior_impact, manage_feasibility, manage_cost, 
                        manage_benefit, threats, indicators, newID)
        
      } else if (ppp_vars[p] == "ind_feasibility") {
        temp <- costeff(detect_p, detect_q, new_var, ind_cost, 
                        prior_impact, manage_feasibility, manage_cost, 
                        manage_benefit, threats, indicators, newID)
        
      } else if (ppp_vars[p] == "ind_cost") {
        temp <- costeff(detect_p, detect_q, ind_feasibility, new_var, 
                        prior_impact, manage_feasibility, manage_cost, 
                        manage_benefit, threats, indicators, newID)
        
      } else if (ppp_vars[p] == "prior_impact") {
        temp <- costeff(detect_p, detect_q, ind_feasibility, ind_cost, 
                        new_var, manage_feasibility, manage_cost, 
                        manage_benefit, threats, indicators, newID)
        
      } else if (ppp_vars[p] == "manage_feasibility") {
        temp <- costeff(detect_p, detect_q, ind_feasibility, ind_cost, 
                        prior_impact, new_var, manage_cost, 
                        manage_benefit, threats, indicators, newID)
      } else if (ppp_vars[p] == "manage_cost") {
        temp <- costeff(detect_p, detect_q, ind_feasibility, ind_cost, 
                        prior_impact, manage_feasibility, new_var, 
                        manage_benefit, threats, indicators, newID)
      } else if (ppp_vars[p] == "manage_benefit") {
        temp <- costeff(detect_p, detect_q, ind_feasibility, ind_cost, 
                        prior_impact, manage_feasibility, manage_cost, 
                        new_var, threats, indicators, newID)
      }
      
      
      ## Specify cut-off
      dimidx_sens <- find.dim.point(temp$exp_cost, temp$exp_benefit, threshold = 0.2)$idx
      
      
      ## Caluclate PPP metrics 
      # Diminishing returns cut-off value (not used as a metric)
      metric1[run, p] <- find.dim.point(temp$exp_cost, temp$exp_benefit, threshold = 0.2)$val
      
      # Spearman rank correlationf or all indicators: ID = 2
      metric2[run, p] <- cor(ce$ID, temp$ID, method = "spearman")
      
      # Expected benefit (cumulative benefit): ID = 3; % change in 
      metric3[run, p] <- (sum(ce$exp_benefit[1:dimidx]) - sum(temp$exp_benefit[1:dimidx_sens])) / 
        sum(ce$exp_benefit[1:dimidx])
      if (absolute) metric3 <- abs(metric3)
      
      # Cumulative cost: ID = 4; % change in
      metric4[run, p] <- (sum(ce$exp_cost[1:dimidx]) - sum(temp$exp_cost[1:dimidx_sens])) / 
        sum(ce$exp_cost[1:dimidx])
      # (sum(ce$ind_cost[1:dimidx]) - sum(temp$ind_cost[1:dimidx_sens])) / sum(ce$ind_cost[1:dimidx])
      if (absolute) metric4 <- abs(metric4)
      
      # Number of indicators: ID = 5; % change in 
      metric5[run, p] <- (length(ce$ID[1:dimidx]) - length(temp$ID[1:dimidx_sens])) / 
        length(ce$ID[1:dimidx]) 
      if (absolute) metric5 <- abs(metric5)
      
      # Number of new indicators: ID = 6
      temp1 <- setdiff(ce$ID[1:dimidx], temp$ID[1:dimidx_sens])
      temp2 <- setdiff(temp$ID[1:dimidx_sens], ce$ID[1:dimidx])
      metric6[run, p] <- length(temp1) + length(temp2)
    } ## end MCruns loop
  } ## end ppp_vars loop
  
  error_outputs[[E]] <- list(metric1=metric1, metric2=metric2, metric3=metric3, 
                           metric4=metric4, metric5=metric5, metric6=metric6)
} ## end error loop

names(error_outputs) <- paste0("error", error)
saveRDS(error_outputs, "./output/error_output.rds")
error_outputs <- readRDS("./output/error_output.rds")

# PLOTTING
# Subset data according to metric and plot
# Estimate mean values for metrics per error and parameter 

metric2_val <- sapply(error_outputs, "[", 2)
metric3_val <- sapply(error_outputs, "[", 3)
metric4_val <- sapply(error_outputs, "[", 4)
metric5_val <- sapply(error_outputs, "[", 5)


pdf(file = "./output/boxplots.pdf", width=10, height=8)
par(mfrow=c(2,2), las=1, mar=c(7, 5, 2, 1), oma = c(0, 0, 2, 0))
for (E in seq(6,26,5)) {
  boxplot(metric2_val[[E]], xaxt='n', frame=FALSE,
          ylab="Correlation in selected indicators")
  axis(side = 1, at = 1:8, labels=FALSE, lwd.ticks = TRUE)
  text(x = seq(1, 8, by=1), y = -0.72, labels=colnames(metric2_val[[1]]), adj = 1, xpd = TRUE,cex=1,srt=60)
  
  boxplot(metric3_val[[E]], xaxt='n', frame=FALSE,
          ylab="Proportional change in expected benefit\nof selected indicators")
  axis(side = 1, at = 1:8, labels=FALSE, lwd.ticks = TRUE)
  text(x = seq(1, 8, by=1), y = -0.08, labels=colnames(metric2_val[[1]]), adj = 1, xpd = TRUE,cex=1,srt=60)
  
  boxplot(metric4_val[[E]], xaxt='n', frame=FALSE,
          ylab="Proportional change in expected cost of\nselected indicators")
  axis(side = 1, at = 1:8, labels=FALSE, lwd.ticks = TRUE)
  text(x = seq(1, 8, by=1), y = -0.085, labels=colnames(metric2_val[[1]]), adj = 1, xpd = TRUE,cex=1,srt=60)
  
  boxplot(metric5_val[[E]], xaxt='n', frame=FALSE,
          ylab="Proportional change in number\nof indicators selected")
  axis(side = 1, at = 1:8, labels=FALSE, lwd.ticks = TRUE)
  text(x = seq(1, 8, by=1), y = -0.07, labels=colnames(metric2_val[[1]]), adj = 1, xpd = TRUE,cex=1,srt=60)
  
  mtext(paste("Percentage error = ", error[E], "%",sep=""), outer = TRUE, cex = 1.2)
}
dev.off()

## Plot Error vs mean metric value per parameter
error_means <- data.frame()
for (E in 1:length(error)){
  for (m in 1:length(metrics)) {
    error_means <- rbind(error_means, c(error[E], m, colMeans(error_outputs[[E]][[m]])))
  }
}
colnames(error_means) <- c("error","metricID", ppp_vars)


par(mfrow=c(2,3), las=1, mar=c(5, 5, 4, 1) + 0.1, oma = c(0, 0, 2, 0))

temp <- error.dat[which(error.dat$metricID==2),]
plot(error, temp[,3], type="l", ylim=c(0,1), xlab="error", ylab="Mean value\n(Spearman rank correlation)")
lines(error, temp[,4],col=2)
lines(error, temp[,5], col=3)
lines(error, temp[,6], col=4)
lines(error, temp[,7], col=5)
lines(error, temp[,8], col=6)


temp <- error.dat[which(error.dat$metricID==3),]
plot(error, temp[,3], type="l", ylim = c(min(temp[,3:8]),max(temp[,3:8])), xlab="error", ylab="Mean value\n(Prop change in expected benefit)")
lines(error, temp[,4],col=2)
lines(error, temp[,5], col=3)
lines(error, temp[,6], col=4)
lines(error, temp[,7], col=5)
lines(error, temp[,8], col=6)

temp <- error.dat[which(error.dat$metricID==4),]
plot(error, temp[,3], type="l", ylim = c(min(temp[,3:8]),max(temp[,3:8])), xlab="error", ylab="Mean value\n(Prop change in cost)")
lines(error, temp[,4],col=2)
lines(error, temp[,5], col=3)
lines(error, temp[,6], col=4)
lines(error, temp[,7], col=5)
lines(error, temp[,8], col=6)

temp <- error.dat[which(error.dat$metricID==5),]
plot(error, temp[,3], type="l", ylim = c(min(temp[,3:8]),max(temp[,3:8])), xlab="error", ylab="Mean value\n(Prop change in total ind)")
lines(error, temp[,4],col=2)
lines(error, temp[,5], col=3)
lines(error, temp[,6], col=4)
lines(error, temp[,7], col=5)
lines(error, temp[,8], col=6)

temp <- error.dat[which(error.dat$metricID==6),]
plot(error, temp[,3], type="l", ylim = c(min(temp[,3:8]),max(temp[,3:8])), xlab="error", ylab="Mean value\n(Change in ind)")
lines(error, temp[,4],col=2)
lines(error, temp[,5], col=3)
lines(error, temp[,6], col=4)
lines(error, temp[,7], col=5)
lines(error, temp[,8], col=6)


op <- par(usr=c(0,1,0,1), # Reset the coordinates
          xpd=NA)         # Allow plotting outside the plot region
legend(2.2,1, # Find suitable coordinates by trial and error
       c("Fi", "Ci", "bj", "Fkj", "Bj", "pij"), lty=1, lwd=3, col=c(1:6), box.col=NA, cex=2)

# dev.off()



