
## Monitoring PPP functions
## ----------------------------------------------
#' @author Payal Bal


## ----------------------------------------------
## Estimate cost-effectiveness
## ----------------------------------------------

#'@description find cost-effectiveness of a list of indicators given the parameters describe below
#'@param detect_p (double matrix) p_ij: probability of indicator i detecting enough change in threat j to trigger a chosen management intervention given threat j is present
#'@param detect_q (double matrix) q_ij: probability of indicator i incorrectly detecting enough change in threat j to trigger a chosen management intervention given that threat j is absent
#'@param ind_feasibility (double vector) F_i: feasibility of monitoring indicator i
#'@param ind_cost (integer vector) R_i: relative cost of monitoring indicator i
#'@param prior_impact (double vector) d_j: prior belief in impact of threat i
#'@param manage_feasibility (double vector) G_j: feasibility of the chosen management intervention for threat j
#'@param manage_cost (double vector) M_j: cost of the chosen management intervention for threat j (in millions)
#'@param manage_benefit (double vector) A_j: expected benefit of management of threat for all species groups/assets 
#'@param threats (charcater vector) list of threats
#'@param indicators (charcater vector) list of indicators
#'#'@param newID (integer vector) Numeric ID associated with the indicators
#'@return (data frame) output data frame with 11 columns: "indicators", "benefit_firegrazing", "benefit_catpredation", "benefit_weeds", "cost_firegrazing", "cost_catpredation", "cost_weeds", "exp_benefit", "exp_cost", "cost_effectiveness", "ind_ranks" 

costeff <- function(detect_p, detect_q, ind_feasibility, ind_cost, 
                    prior_impact, manage_feasibility, manage_cost, 
                    manage_benefit, threats, indicators, newID) {
  
  monit_benefit <- matrix(NA,length(indicators),length(threats))
  for (j in 1:length(threats)) {
    monit_benefit[,j] <- (prior_impact[j] * ind_feasibility * detect_p[,j] * 
                            manage_feasibility[j] * manage_benefit[j])
  }
  exp_benefit <- rowSums(monit_benefit)
  
  monit_cost <- matrix(NA,length(indicators),length(threats))
  for (j in 1:length(threats)) {
    monit_cost[,j] <- (((detect_p[,j] * prior_impact[j]) + detect_q[,j] - (detect_q[,j] * prior_impact[j])) * 
                         (ind_feasibility * manage_feasibility[j] * manage_cost[j])) + ind_cost
  }
  exp_cost <- rowSums(monit_cost, na.rm = TRUE)
  
  effectiveness <- exp_benefit/exp_cost
  
  output <- as.data.frame(cbind(newID, ind_feasibility, ind_cost, monit_benefit, 
                                monit_cost, exp_benefit, exp_cost, effectiveness, 
                                detect_p, detect_q))
  output <- cbind(indicators, output)
  names(output)[2:10] <- c("ID", "ind_feasibility", "ind_cost", paste0("benefit_", threats), paste0("cost_", threats))
  output <- output[order(-effectiveness),]
  output$ind_ranks <- 1:dim(output)[1]
  return(output)
}



## ----------------------------------------------
## Find diminishing point of returns
## ----------------------------------------------

#'@description find cut-off point for prioritisation based on diminishing returns
#'@details To find the diminishing returns point we calculate the gradient of straight lines 
#' between pairs or successive points of the cumulative cost and cumulative benefit vectors
#' (obtained from costeff function), (x1,y1) and (x2,y2) using the formula : (y2-y1)/(x2-x1)
#' where x = cumulative cost (independent variable) and y = cumulative benefit (dependent variable)
#' This tells us how muvh the line changes relative to x and y.
#'@param expected_cost as obtained from cost-effectiveness calculation
#'@param expected_benefit as obtained from cost-effectiveness calculation. This variable
#'                         can also be substituted by (expected benefit * feasibility).
#'@param threshold to define cut-off for diminishing returns i.e slope.


find.dim.point <- function(expected_cost, expected_benefit, threshold) {
  output <- list()
  x <- cumsum(expected_cost)  # cost parameter
  y <- cumsum(expected_benefit)  # beneit parameter
  
  slopes <- c()
  for (i in 1:length(x)-1) {
    slopes[i] <- (y[i+1]-y[i]) / (x[i+1]-x[i])
  }
  
  if (length(slopes[slopes>=threshold]) == 0) {
    output$val = slopes[1] 
    output$idx = 1 
    ## if specified threshold is never reached, pick the first value of gradient vector and the corresponding index
    
  } else {
    
    output$val <- min(slopes[slopes>=threshold])
    output$idx <- max(which(slopes>=threshold))
    ## if specified threshold is reached, assign value and index at specified diminishing point
  }
  
  return(output)
}


## ----------------------------------------------
## Parameter sampling (for sensitivity analysis)
## ----------------------------------------------

#'@description sampling parameters from a specified (here, uniform) distribution for the sensitivity analysis
#'@param mean.est (double) mean estimate of parameters
#'@param percent.var (double) error
#'@param type (charcater) data type i.e. probability, percentage, unconstrained

unif.param <- function(mean.est, percent.var, type) {
  
  if(type=="probability") { 
    ## for probability parameters: "detect_p", "detect_q", "ind_feasibility", 
    ##  "prior_impact", "manage_feasibility"  
    ## probability parameters > 0 bound between 0.01 and 1
    
    if (is.na(mean.est)) {output <- NA}
    else if (mean.est > 0) {
      percent.var = percent.var/100 # not proportion of mean value
      min.val = mean.est - percent.var
      max.val = mean.est + percent.var
      output <- runif(1, min = min.val , max = max.val)
      if (output < 0) {output <- 0.01} else if (output > 1) {output <- 1}
    } else if (mean.est == 0) {output <- 0}
    
  } else if (type=="percent") { 
    ## for relative monitoring cost parameter: "ind_cost"
    min.val = mean.est - percent.var
    max.val = mean.est + percent.var
    output <- round(runif(1, min = min.val , max = max.val),0)
    if (output < 1 ) {output <- 1} else if (output > 100) {output <- 100}
    
  } else if (type=="unconstrained") { 
    ## for management benefit and management cost parameters: "manage_cost", "manage_benefit"
    # benefit is unconstrained variable; treating it the same as
    # cost parameter (i.e. a percentage value between 1 and 100)
    # eventually will remove benefit and replace with prob of persistence (+ 4 parameters)
    min.val = mean.est - ((percent.var/100)*137.55) ## 137.55 is the maximum possible benefit for each threat when all m_lj = 1
    max.val = mean.est + ((percent.var/100)*137.55)
    output <- runif(1, min = min.val , max = max.val)
    if (output < 0 ) {output <- 0.1}
  } 
  
  else {
    
    cat("\nERROR: unknown parameter type:", type, "\n")
    stop()
  }
  
  return(output)
}

