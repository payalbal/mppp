## ------------------------------------------------------
## Methods figure for diminishing returns
## ------------------------------------------------------
## Base scenario
cost <- cumsum(ce$exp_cost)
benefit <- cumsum(ce$exp_benefit)
dimidx <- find.dim.point(ce$exp_cost, ce$exp_benefit, threshold = 0.2)$idx
cl <- c("gold","sienna1","skyblue3","dimgray","darkolivegreen3",
        "orangered","purple4","firebrick3","royalblue4",
        "tomato4","dodgerblue","deeppink",
        "orchid4","seagreen" )

# pdf(file = "./output/method_plot.pdf", width=11, height=7)
par(mfrow=c(1,1), las=1, mar=c(5, 5, 2, 9) + 0.1, xpd = FALSE, cex=1.2)
plot(cost, benefit, lwd = 2, type = "l", xlab = "Cumulative expected cost",
     ylab = "Cumulative expected benefit", xlim = c(-50, max(cost)), 
     ylim = c(min(benefit), ceiling(max(benefit)/50)*50))
points(cost, benefit, pch=19, col=cl[ce$ind_ranks])
segments(cost[dimidx], 0, cost[dimidx],
         benefit[dimidx],lty = 2,col="dimgray")
segments(-100,benefit[dimidx], cost[dimidx],
         benefit[dimidx],lty = 2,col="dimgray")
# text(cost[1:dimidx]-30,benefit[1:dimidx],
#      paste(ce$ind_ranks[1:dimidx]))
# text(cost[dimidx]-20,130, "diminishing returns\n(base scenario)", srt=90)


## Randomised scenarios
error <- 50
set.seed(48)
new.param <- c()

for (i in 1:length(indicators)) {
  new.param[i] <- unif.param(ind_cost[i], error, "PERCENT")}
new.param <- round(new.param,0)
temp <- costeff(detect_p, detect_q, ind_feasibility, new.param, prior_impact,
                manage_feasibility, manage_cost, manage_benefit, threats, indicators)
dimidx <- find.dim.point(ce$exp_cost, ce$exp_benefit, threshold = 0.2)$idx
cost <- cumsum(temp$exp_cost)
benefit <- cumsum(temp$exp_benefit)
lines(cost,benefit, lwd=2, type="l", col="steelblue")
points(cost,benefit, pch=19, col=cl[temp$ind_ranks])
# text(cost[1:dimidx]+30, benefit[1:dimidx],
#      paste(temp$ind_ranks[1:dimidx]), col="royalblue")
segments(cost[dimidx], 0, cost[dimidx],
         benefit[dimidx],lty = 2, col="steelblue3")
segments(-100,benefit[dimidx], cost[dimidx],
         benefit[dimidx],lty = 2, col="steelblue3")


for (i in 1:length(indicators)) {
  new.param[i] <- unif.param(ind_feasibility[i], 0.2, "PROB")} ## NOT WORKING THIS...
new.param <- round(new.param,0)
temp <- costeff(detect_p, detect_q, ind_feasibility, new.param, prior_impact,
                manage_feasibility, manage_cost, manage_benefit, threats, indicators)
dimidx <- find.dim.point(ce$exp_cost, ce$exp_benefit, threshold = 0.2)$idx
cost <- cumsum(temp$exp_cost)
benefit <- cumsum(temp$exp_benefit)
lines(cost,benefit, lwd=2, type="l", col="steelblue")
points(cost,benefit, pch=19, col=cl[temp$ind_ranks])
# text(cost[1:dimidx]+30, benefit[1:dimidx],
#      paste(temp$ind_ranks[1:dimidx]), col="royalblue")
segments(cost[dimidx], 0, cost[dimidx],
         benefit[dimidx],lty = 2, col="steelblue3")
segments(-100,benefit[dimidx], cost[dimidx],
         benefit[dimidx],lty = 2, col="steelblue3")

# text(230,40,"A1",col="steelblue")
# text(345,40,"B1")
# text(525,40,"C1",col="steelblue")
# text(-65,308,"A2",col="steelblue")
# text(-65,338,"B2")
# text(-65,370,"C2",col="steelblue")



# Legend
legend(480,225, # Find suitable coordinates by trial and error
       c("Base scenario",
         "Randomised scenarios with\nuncertainty in paramters", 
         "Diminishing returns threshold\n(base scenario)",
         "Diminishing returns threshold\n(as per randomised scenario)"), y.intersp=2.7,
       lty=c(1,1,2,2), lwd=2, col=c("black","steelblue3","black","steelblue3"), box.col=NA, cex=1)
text(600, 220, "Prioritisation results", font=2)

op <- par(usr=c(0,1,0,1), # Reset the coordinates
          xpd=NA,font=1)         # Allow plotting outside the plot region
legend(1.05,0.9, # Find suitable coordinates by trial and error
       c(paste("IND", 1:18)), y.intersp=1.5,
       pch=rep(19,18), col=cl, box.col=NA, cex=1)
text(1.1, 0.95, "Indicators by ID", font=2)
