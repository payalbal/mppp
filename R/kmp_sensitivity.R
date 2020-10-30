## Setup workspace ####
set.seed(10)
library(readxl)    
source("./R/mppp_functios.R")
default_par <- par()


## Load data ####
kmp_data <- lapply(excel_sheets("./data/kmp_data.xlsx"), read_excel, path = "./data/kmp_data.xlsx")
names(kmp_data) <- c("threats", "assests", "indicators", "persistence_probability", "management_data", "monitoring_data")


## Remove uninformative indicators ####
ind_delete <- kmp_data$monitoring_data[which(with(kmp_data$monitoring_data, c(detect_firegrazing == 0 & detect_catpredation == 0 & detect_weeds == 0))),]$indicators_i
kmp_data$monitoring_data <- kmp_data$monitoring_data[!kmp_data$monitoring_data$indicators_i %in% ind_delete,]
kmp_data$indicators <- kmp_data$indicators[!kmp_data$indicators$indicator_i %in% ind_delete,]
kmp_data$indicators$newID <- 1:dim(kmp_data$indicators)[1]


## Define variables ####
detect_p <- with(kmp_data$monitoring_data, cbind(detect_p_firegrazing, detect_p_catpredation, detect_p_weeds))
detect_q <- with(kmp_data$monitoring_data, cbind(type1_q_firegrazing, type1_q_catpredation, type1_q_weeds))
ind_feasibility <- with(kmp_data$monitoring_data, monit_feasibility_F)
ind_cost <- with(kmp_data$monitoring_data, monit_relcost_R)
prior_impact <- with(kmp_data$management_data, prior_d)
manage_feasibility <- with(kmp_data$management_data, manage_feasibility_G)
manage_cost <- with(kmp_data$management_data, manage_cost_M)
manage_benefit <- with(kmp_data$persistence_probability, colSums(nspecies_n * (cbind(peristence_m_firegrazing, peristence_m_catpredation, peristence_m_weeds) - persistence_m0_noaction)))
threats <- with(kmp_data$threats, threat_j)
indicators <- with(kmp_data$indicators, indicator_label)
newID <- with(kmp_data$indicators, newID)


## Specify diminishing returns threshold ####
dim.threshold <- 0.2 
## Other options: fix.budget <- c(200), fix.benefit <- 350


## CE estimates ####
ce <- costeff(detect_p, detect_q, ind_feasibility, ind_cost, prior_impact,
              manage_feasibility, manage_cost, manage_benefit, threats, indicators, newID)
dimidx <- find.dim.point(ce$exp_cost, ce$exp_benefit, threshold = dim.threshold)$idx


# ## CE plot
# cost <- cumsum(ce$exp_cost)
# benefit <- cumsum(ce$exp_benefit)
# dimidx <- find.dim.point(ce$exp_cost, ce$exp_benefit, threshold = dim.threshold)$idx
# par(mfrow=c(1,1), las=1, mar=c(5, 7, 4, 2) + 0.1, xpd = FALSE, cex = 1)
# plot(cost, benefit, lwd = 2, type = "l", xlab = "Cumulative expected cost",
#      ylab = "Cumulative expected benefit", xlim = c(-50, max(cost)), 
#      ylim = c(min(benefit), ceiling(max(benefit)/50)*50))
# points(cost, benefit, pch=19)
# segments(cost[dimidx], 0, cost[dimidx],
#          benefit[dimidx],lty = 2)
# segments(-100,benefit[dimidx], cost[dimidx],
#          benefit[dimidx],lty = 2)
# text(cost[1:dimidx]-30,benefit[1:dimidx],
#      paste(ce$ind_ranks[1:dimidx]))
# text(cost[dimidx]+30,130, "diminishing returns threshold", srt=90)
# text(600,cumsum(ce$exp_benefit)[1:dimidx], ce$indicators[1:dimidx], adj = 0)



## Set scenarios ####

## Specify parameter names to be tested
params <- c("detect_p", "detect_q", 
            "ind_feasibility", "ind_cost", "prior_impact", 
            "manage_feasibility", "manage_cost", "manage_benefit")

## Runs
MCruns = 1000

## Error values to be tested
error <- seq(0,90,2) 
# error <- 30

## Number of metrics used in sensitivity analysis
n.metrics <- 6 
metric.names <- c("change.mar.benefit", "rank.corr", "exp.benefit", 
                  "rel.cost", "num.ind", "changed.ind")
# Mertic ID for identification
# 1 = diminishing returns cut-off value
# 2 = spearman rank correlation of new and orignal ranks of all 18 indicators
# 3 = expected benefit (cumulative benefit) of indicators selected using specific cut-off
# 4 = cumulative cost of indicators selected using specific cut-off
# 5 = number of indicators selected using specific cut-off
# 6 = number of new indicators

## If absolute values to be returned for the metrics
absolute <- TRUE

## Specify where to store outputs
outmat <- matrix(NA, n.metrics * MCruns, length(params) + 2)
## outmat stores outputs from sensitivity analysis for 
## [rows] each metric (6) * each run (MCruns)
## [columns] for each paramerter (8) + counters to indicate run (1) and metricID (1)
error.mat <- matrix(NA, length(error) * (n.metrics - 1), length(params) + 2)
## error.mat stores mean outputs from sensitivity analysis 
## [rows] each error level tested * each metric excluding metric 1 (5)
## [columns] for each paramerter (8) + counters to indicate run (1) and metricID (1)

## Indexing for error.mat
error.ind <- seq(1, nrow(error.mat), 5) 
## increasing by 5 because we store 5 rows for the 5 metrics ata  time



## Run sensitivity analysis ####

pdf(paste("./output/sensitivityplots_", MCruns, "runs_", length(error), 
          "errors", ".pdf", sep=""))
# pdf(paste("./output/sensitivityplots_", MCruns, "runs_",
#           "error_30", ".pdf", sep=""))

for (E in 1:length(error)) {
  ctr <- 1
  
  for (run in 1:MCruns) {
    
    for (metric in 1:n.metrics) {
      outvec <- c()
      
      for (p in 1:length(params)) {
        
        ## Step 1- Sample parameter values
        param <- get(params[p])
        
        if (params[p] %in% c("detect_p", "detect_q", "ind_feasibility", 
                             "prior_impact", "manage_feasibility")) {
          
          if (params[p] %in% c("detect_p", "detect_q")) {
            
            new.param <- matrix(NA, dim(param)[1], dim(param)[2])
            
            for (i in 1:dim(param)[1]) {
              new.param[i,1] <- unif.param(param[i,1], error[E], type = "probability")
              new.param[i,2] <- unif.param(param[i,2], error[E], type = "probability")
              new.param[i,3] <- unif.param(param[i,3], error[E], type = "probability")
            }
          } else {
            
            new.param <- c()
            
            for (i in 1:length(param)) {
              new.param[i] <- unif.param(param[i], error[E], type = "probability")
            }
          }
        } else if (params[p] %in% "ind_cost") {
          
          new.param <- c()
          
          for (i in 1:length(param)) {
            new.param[i] <- unif.param(param[i], error[E], type = "percent")
          }
        } else if (params[p] %in% c("manage_cost", "manage_benefit")) {
          
          new.param <- c()
          
          for (i in 1:length(param)) {
            new.param[i] <- unif.param(param[i], error[E], type = "unconstrained")
          }
        }
        
        
        ## Step 2 - Estimate cost-effectiveness and ranking for resampled data
        if (params[p] == "detect_p") {
          temp <- costeff(new.param, detect_q, ind_feasibility, ind_cost, 
                          prior_impact, manage_feasibility, manage_cost, 
                          manage_benefit, threats, indicators, newID)
          
        } else if (params[p] == "detect_q") {
          temp <- costeff(detect_p, new.param, ind_feasibility, ind_cost, 
                          prior_impact, manage_feasibility, manage_cost, 
                          manage_benefit, threats, indicators, newID)
          
        } else if (params[p] == "ind_feasibility") {
          temp <- costeff(detect_p, detect_q, new.param, ind_cost, 
                          prior_impact, manage_feasibility, manage_cost, 
                          manage_benefit, threats, indicators, newID)
          
        } else if (params[p] == "ind_cost") {
          temp <- costeff(detect_p, detect_q, ind_feasibility, new.param, 
                          prior_impact, manage_feasibility, manage_cost, 
                          manage_benefit, threats, indicators, newID)
          
        } else if (params[p] == "prior_impact") {
          temp <- costeff(detect_p, detect_q, ind_feasibility, ind_cost, 
                          new.param, manage_feasibility, manage_cost, 
                          manage_benefit, threats, indicators, newID)
          
        } else if (params[p] == "manage_feasibility") {
          temp <- costeff(detect_p, detect_q, ind_feasibility, ind_cost, 
                          prior_impact, new.param, manage_cost, 
                          manage_benefit, threats, indicators, newID)
          
        } else if (params[p] == "manage_cost") {
          temp <- costeff(detect_p, detect_q, ind_feasibility, ind_cost, 
                          prior_impact, manage_feasibility, new.param, 
                          manage_benefit, threats, indicators, newID)
          
        } else if (params[p] == "manage_benefit") {
          temp <- costeff(detect_p, detect_q, ind_feasibility, ind_cost, 
                          prior_impact, manage_feasibility, manage_cost, 
                          new.param, threats, indicators, newID)
        }
        
        
        ## Step 3 - Specify cut-off
        dimidx.temp <- find.dim.point(temp$exp_cost, temp$exp_benefit, threshold = dim.threshold)$idx
        
        
        ## Step 4 - Caluclate sensitivity metrics 
        if (metric == 1) {
          ## Metric 1: Diminishing returns cut-off value (not used as a metric)
          outvec[p] <- find.dim.point(temp$exp_cost, temp$exp_benefit, threshold = dim.threshold)$val
          
        } else if (metric == 2) {
          ## Metric 2: Spearman rank correlationf or all 18 indicators
          outvec[p] <- cor(ce$ID, temp$ID, method = "spearman")
          
        } else if (metric == 3) {
          ## Metric 3: % change in expected benefit (cumulative benefit)
          outvec[p] <- (sum(ce$exp_benefit[1:dimidx]) - 
                          sum(temp$exp_benefit[1:dimidx.temp])) / 
            sum(ce$exp_benefit[1:dimidx])
          if (absolute) outvec[p] <- abs(outvec[p])
          
        } else if (metric == 4) {
          ## Metric 4: % change in expected cost (cumulative cost)
          outvec[p] <- (sum(ce$exp_cost[1:dimidx]) - 
                          sum(temp$exp_cost[1:dimidx.temp])) / 
            sum(ce$exp_cost[1:dimidx])
          if (absolute) outvec[p] <- abs(outvec[p])
          
        } else if (metric == 5) {
          ## Metric 5: % change in number of indicators selected 
          outvec[p] <- (dimidx - dimidx.temp) / dimidx
          if (absolute) outvec[p] <- abs(outvec[p])
          
        } else if (metric == 6) {
          ## Metric 6: number of new indicators
          temp1 <- setdiff(ce$ID[1:dimidx], temp$ID[1:dimidx.temp])
          temp2 <- setdiff(temp$ID[1:dimidx.temp], ce$ID[1:dimidx])
          outvec[p] <- length(temp1) + length(temp2)
          
        }
      }
      
      ## Store outputs for all metrics from all runs for a single error[E]
      outvec <- c(metric, outvec)
      outvec <- c(run, outvec)
      outmat[ctr,] <- outvec
      ctr <- ctr + 1
    }
  }
  
  ## Ploting by error ####
  outdat <- as.data.frame(outmat)
  colnames(outdat) <- c("run", "metricID", params)
  
  ## Temp matrix to store mean metric values for each error value
  temp.mat <- matrix(NA,(n.metrics - 1), length(params) + 2)
  
  ## Specify dimensiosn for multi-panel plot
  par(mfrow=c(2,3), las=1, mar=c(9, 5, 2, 1) + 0.1, oma = c(0, 0, 2, 0), xpd = NA)
  
  ## Metric 2
  temp <- outdat[which(outdat$metricID==2),] 
  temp.mat[1,] <- c(error[E], 2, colMeans(temp[,3:ncol(temp)]))
  if(error[E] %in% seq(10,90,10)) {
    boxplot(temp[,3:10], xaxt = "n", yaxt = "n", 
            ylab="Metric 1: Spearman rank correlation\nin indicator ranks") #, notch=TRUE)
    axis(side = 1, at = 1:length(params), labels = FALSE)
    axis(side = 2, las = 2)
    yticks <- axTicks(side = 2)
    y.interv <- (yticks[length(yticks)]-yticks[length(yticks)-1])/2
    text(x = 1:length(params),
         ## Move labels to just below bottom of chart.
         y = par("usr")[3] - y.interv,
         ## Use names from the data list.
         labels = params,
         ## Rotate the labels by 35 degrees.
         srt = 55,
         ## Adjust the labels to almost 100% right-justified.
         adj = 0.965,
         ## Increase label size.
         cex = 1)
    ## (ref: https://www.tenderisthebyte.com/blog/2019/04/25/rotating-axis-labels-in-r/)
  }
  
  ## Metric 3
  temp <- outdat[which(outdat$metricID==3),] 
  temp.mat[2,] <- c(error[E], 3, colMeans(temp[,3:ncol(temp)]))
  if(error[E] %in% seq(10,90,10)) {
    boxplot(temp[,3:ncol(temp)], xaxt = "n", yaxt = "n",
            ylab="Metric 2: Proportional change in\nexpected benefit of selected indicators")
    axis(side = 1, at = 1:length(params), labels = FALSE)
    axis(side = 2, las = 2)
    yticks <- axTicks(side = 2)
    y.interv <- (yticks[length(yticks)]-yticks[length(yticks)-1])/2
    text(x = 1:length(params),
         y = par("usr")[3] - y.interv,
         labels = params,
         srt = 55,
         adj = 0.965,
         cex = 1)
  }
  
  ## Metric 4
  temp <- outdat[which(outdat$metricID==4),] 
  temp.mat[3,] <- c(error[E], 4, colMeans(temp[,3:ncol(temp)]))
  if(error[E] %in% seq(10,90,10)) {
    boxplot(temp[,3:ncol(temp)], xaxt = "n", yaxt = "n", 
            ylab="Metric 3: Proportional change in cost\nof monitoring selected indicators")
    axis(side = 1, at = 1:length(params), labels = FALSE)
    axis(side = 2, las = 2)
    yticks <- axTicks(side = 2)
    y.interv <- (yticks[length(yticks)]-yticks[length(yticks)-1])/2
    text(x = 1:length(params),
         y = par("usr")[3] - y.interv,
         labels = params,
         srt = 55,
         adj = 0.965,
         cex = 1)
  }
  
  ## Metric 5
  temp <- outdat[which(outdat$metricID==5),] 
  temp.mat[4,] <- c(error[E], 5, colMeans(temp[,3:ncol(temp)]))
  if(error[E] %in% seq(10,90,10)) {
    boxplot(temp[,3:ncol(temp)], xaxt = "n", yaxt = "n", 
            ylab="Metric 4: Proportional change in\nnumber of indicators selected")
    axis(side = 1, at = 1:length(params), labels = FALSE)
    axis(side = 2, las = 2)
    yticks <- axTicks(side = 2)
    y.interv <- (yticks[length(yticks)]-yticks[length(yticks)-1])/2
    text(x = 1:length(params),
         y = par("usr")[3] - y.interv,
         labels = params,
         srt = 55,
         adj = 0.965,
         cex = 1)
  }
  
  ## Metric 6
  temp <- outdat[which(outdat$metricID==6),] 
  temp.mat[5,] <- c(error[E], 6, colMeans(temp[,3:ncol(temp)]))
  if(error[E] %in% seq(10,90,10)) {
    boxplot(temp[,3:ncol(temp)], xaxt = "n", yaxt = "n",
            ylab="Change in selected indicators\n i.e. included/excluded")
    axis(side = 1, at = 1:length(params), labels = FALSE)
    axis(side = 2, las = 2)
    yticks <- axTicks(side = 2)
    y.interv <- (yticks[length(yticks)]-yticks[length(yticks)-1])/2
    text(x = 1:length(params),
         y = par("usr")[3] - y.interv,
         labels = params,
         srt = 55,
         adj = 0.965,
         cex = 1)
    
    mtext(paste("error = ",error[E], sep=""), outer = TRUE, cex = 1.2) # title
  }
  
  ## Store mean values for all metrics from all runs for all error values
  error.mat[(error.ind[E]:(error.ind[E]+4)), ] <- temp.mat
  
}


## Plot Error vs mean metric value per parameter
error.dat <- as.data.frame(error.mat)
colnames(error.dat) <- c("error", "metricID", params)

# par(mfrow=c(2,3), las=1, mar=c(5, 5, 4, 1), oma = c(0, 0, 2, 0))
# comment out when not plotting metric 5
par(mfrow=c(2,2), las=1, mar=c(4.2, 5, 2, 1), oma = c(0, 0, 0, 4), xpd = FALSE, cex=1)

temp <- error.dat[which(error.dat$metricID==2),]
plot(error, temp[,3], type="l", ylim=c(0,1),
     xlab="Percentage error", ylab="Metric 1: Mean correlation\nin indicator ranks")
lines(error, temp[,4],col=2)
lines(error, temp[,5], col=3)
lines(error, temp[,6], col=4)
lines(error, temp[,7], col=5)
lines(error, temp[,8], col=6)
lines(error, temp[,9], col=7)
lines(error, temp[,10], col=8)
abline(v=30, lty=2, col="grey50")

temp <- error.dat[which(error.dat$metricID==3),]
plot(error, temp[,3], type="l", ylim = c(min(temp[,3:8]),max(temp[,3:8])),
     xlab="Percentage error", ylab="Metric 2: Mean prop change\nin expected benefit")
lines(error, temp[,4],col=2)
lines(error, temp[,5], col=3)
lines(error, temp[,6], col=4)
lines(error, temp[,7], col=5)
lines(error, temp[,8], col=6)
lines(error, temp[,9], col=7)
lines(error, temp[,10], col=8)
abline(v=30, lty=2, col="grey50")

temp <- error.dat[which(error.dat$metricID==4),]
plot(error, temp[,3], type="l", ylim = c(min(temp[,3:8]),max(temp[,3:8])),
     xlab="Percentage error", ylab="Metric 3: Mean prop change\nin cost")
lines(error, temp[,4],col=2)
lines(error, temp[,5], col=3)
lines(error, temp[,6], col=4)
lines(error, temp[,7], col=5)
lines(error, temp[,8], col=6)
lines(error, temp[,9], col=7)
lines(error, temp[,10], col=8)
abline(v=30, lty=2, col="grey50")

# temp <- error.dat[which(error.dat$metricID==5),]
# plot(error, temp[,3], type="l", ylim = c(min(temp[,3:8]),max(temp[,3:8])),
#      xlab="Percentage error", ylab="Mean value\n(Prop change in total ind)")
# lines(error, temp[,4],col=2)
# lines(error, temp[,5], col=3)
# lines(error, temp[,6], col=4)
# lines(error, temp[,7], col=5)
# lines(error, temp[,8], col=6)
# lines(error, temp[,9], col=7)
# lines(error, temp[,10], col=8)
# abline(v=30, lty=2, col="grey50")

temp <- error.dat[which(error.dat$metricID==6),]
plot(error, temp[,3], type="l", ylim = c(min(temp[,3:8]),max(temp[,3:8])),
     xlab="Percentage error", ylab="Metric 4: Mean change\nin selected indicators")
lines(error, temp[,4],col=2)
lines(error, temp[,5], col=3)
lines(error, temp[,6], col=4)
lines(error, temp[,7], col=5)
lines(error, temp[,8], col=6)
lines(error, temp[,9], col=7)
lines(error, temp[,10], col=8)
abline(v=30, lty=2, col="grey50")

op <- par(usr=c(0,1,0,1), # Reset the coordinates
          xpd=NA)         # Allow plotting outside the plot region
legend(1.05,2.4, # Find suitable coordinates by trial and error
       c("pij", "qij", "Fi", "Ri", "dj", "Gj", "Mj", "Aj"), 
       lty=1, lwd=1, col=c(1:8), box.col=NA, cex=1)

dev.off()

## legend only
plot.new()
legend("center", c(expression("p"[ij], "q"[ij], "F"[i], "R"[i], "d"[j], "G"[j], "M"[j], "A"[j])), 
       lty=1, lwd=2, col=c(1:8), box.col=NA, cex=2, y.intersp=1.3)

