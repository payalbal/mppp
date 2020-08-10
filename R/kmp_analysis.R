
## Setup workspace
library(readxl)    
source("./R/mppp_functios.R")
default_par <- par()


## Load data 
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
manage_benefit <- with(kmp_data$persistence_probability, colSums(nspecies_n * (cbind(peristence_m_firegrazing, peristence_m_catpredation, peristence_m_weeds) - persistence_m0_noaction)))
threats <- with(kmp_data$threats, threat_j)
indicators <- with(kmp_data$indicators, indicator_label)
newID <- with(kmp_data$indicators, newID)

## CE estimates
ce <- costeff(detect_p, detect_q, ind_feasibility, ind_cost, prior_impact,
              manage_feasibility, manage_cost, manage_benefit, threats, indicators, newID)

write.csv(ce, "./output/ce_table.csv", na = "NA", row.names = FALSE)

## ------------------------------------------------------
## Cost-effectiveness barplot
## ------------------------------------------------------
pdf(file = "./output/ce_barplot.pdf", width=9, height=6.5)
par(las=1, mar=c(12,8,8,5), oma=c(0,0,0,0))
bplot <- barplot(ce$effectiveness, ylab="Cost-effectiveness", space=0.05, col = "grey", 
                 ylim = c(0,12), xlim = c(0, length(ce$effectiveness) + 1), xpd=FALSE)
text(x=bplot,y=-0.5, labels=ce$indicator, adj = 1, xpd = TRUE, cex=1, srt=60)
par(xpd=TRUE)
detect_threat <- cbind(ce$detect_p_firegrazing, ce$detect_p_catpredation, ce$detect_p_weeds)
for(m in 1:nrow(detect_threat)){
  for (n in 1:ncol(detect_threat)){
    if(detect_threat[m,n] == 0) {detect_threat[m,n] = 0} else{detect_threat[m,n] =1}
  }
}
# text(x=bplot,y=ce$effectiveness, labels=rowSums(detect_threat), pos = 3)
rect(xleft = -2.5, ybottom = 13.2, xright = 15, ytop = 16.8, col = "lightgrey", border = FALSE)
rect(xleft = -2.5, ybottom = 14.35, xright = 15, ytop = 15.5, col = "white", border = FALSE)
# rect(xleft = -2.5, ybottom = 13.2, xright = 15, ytop = 16.8, border = TRUE)
text(x=-1.2,y=c(15.4,14.3,13.2), labels=threats, pos = 3)
# mtext("Threats detected", side=3, line=5.3, las = 1, font = 2, adj = 0)
for (i in 1:length(indicators)) {
  if (detect_threat[i,1]==1) {
    points(bplot[i], 16, pch=4, cex=1)} else {NA}
  if (detect_threat[i,2]==1) {
    points(bplot[i], 16-1, pch=4, cex=1)} else {NA}
  if (detect_threat[i,3]==1) {
    points(bplot[i], 16-2.2, pch=4, cex=1)} else{NA}
}
  # ## And second y-axis for expected benefit
  # par(new=TRUE)
  # plot(x=bplot, y=ce$exp_benefit, type = "b", col="red",
  #      axes = FALSE,  xlab = "", ylab = "", xlim = c(0, length(ce$effectiveness) + 1))
  # mtext("Expected benefit",side=4,col="red",line=3, las = 3)
  # axis(4, ylim=c(0,7000), col="red",col.axis="red",las=1)
dev.off()



## ------------------------------------------------------
## Expected/Relative cost plots (Fig 4)
## ------------------------------------------------------
bplot <- barplot(ce$effectiveness, ylab="Cost-effectiveness", space=0.05, col = "grey", 
                 ylim = c(0,12), xlim = c(0, length(ce$effectiveness) + 1), xpd=FALSE)

pdf(file = "./output/expcost_plot.pdf", width=10, height=8)
par(las=1, mar=c(15,8,8,6), oma=c(0,0,0,0))
plot(x=bplot, y=ce$exp_benefit, type = "b", ylab="Expected benefit",
     ylim = c(0,ceiling(max(ce$exp_benefit)/50)*50), 
     xlim = c(0, length(ce$effectiveness) + 1), xpd=FALSE, bty = "n", xaxt="n", xlab = NA)
axis(side=1, at=bplot, labels = FALSE)
text(x=bplot, y=-5, labels=ce$indicator, adj = 1, xpd = TRUE, cex=1, srt=60)

# ## Add threats box
# par(xpd=TRUE)
# detect_threat <- with(kmp_data$monitoring_data, cbind(detect_firegrazing, detect_catpredation, detect_weeds))
# # text(x=bplot,y=ce$effectiveness, labels=rowSums(detect_threat), pos = 3)
# # rect(xleft = 0, ybottom = 60, xright = 15, ytop = 65, col = "lightgrey", border = FALSE)
# rect(xleft = 0, ybottom = 55, xright = 15, ytop = 70, border = TRUE)
# text(x=-1.5,y=c(65, 60, 55), labels=threats, pos = 3)
# mtext("Threats detected", side=3, line=6, las = 1, font = 2) 
# for (i in 1:length(indicators)) {
#   if (detect_threat[i,1]==1) {
#     points(bplot[i], 70-2.5, pch=4, cex=1)} else {NA}
#   if (detect_threat[i,2]==1) {
#     points(bplot[i], 70-7.5, pch=4, cex=1)} else {NA}
#   if (detect_threat[i,3]==1) {
#     points(bplot[i], 70-12.5, pch=4, cex=1)} else{NA}
# }

## And second y-axis for expected cost
par(new=TRUE, xpd = TRUE)
plot(x=bplot, y=ce$exp_cost, type = "b", col="blue",
     axes = FALSE,  xlab = "", ylab = "", xlim = c(0, length(ce$effectiveness) + 1))
## Add line for relative cost of monitoring
lines(x=bplot, y=ce$ind_cost, col="blue", type = "b", lwd = 1, lty =2)
mtext("Expected cost", side=4, col="blue", line=3, las = 3)
mtext("Relative cost", side=4, col="blue", line=4.2, las = 3)
axis(4, ylim=c(0,ceiling(max(ce$exp_benefit)/50)*50), col="blue", col.axis="blue",las=1)
segments(17.2,35,17.2,55, col = "blue", lty = 1, lwd = 2)
segments(17.7,35,17.7,55, col = "blue", lty = 2, lwd = 2)
shape::Arrows(2.5,-150,12.5,-150, lwd = 1.5, arr.type="triangle", arr.width=0.2)
mtext("Indicators arranged in decreasing order of cost-effectiveness", side=1, line = 13.5)
dev.off()
  # ## add images for points
  # x <- c('grid', 'XML', 'grImport')
  # lapply(x, require, character.only = TRUE)
  # PostScriptTrace("drawing.ps") # creates .xml in the working directory
  # spiral <- readPicture("drawing.ps.xml")
  # xx = grconvertX(x = bplot, from = 'user', to = 'ndc')
  # yy = grconvertY(y = c(15,14,13), from = 'user', to = 'ndc')
  # grid.symbols(spiral, x = xx, y = yy, size = 0.05)


## ------------------------------------------------------
## Detection probability plots (Fig 4)
## ------------------------------------------------------
bplot <- barplot(ce$effectiveness, ylab="Cost-effectiveness", space=0.05, col = "grey", 
                 ylim = c(0,12), xlim = c(0, length(ce$effectiveness) + 1), xpd=FALSE)

pdf(file = "./output/detectprob_plot.pdf", width=10, height=8)
par(las=1, mar=c(15,8,8,8), oma=c(0,0,0,0))
plot(x=bplot, y=ce$exp_benefit, type = "b", ylab="Expected benefit",
     ylim = c(0,ceiling(max(ce$exp_benefit)/50)*50), 
     xlim = c(0, length(ce$effectiveness) + 1), xpd=FALSE, bty = "n", xaxt="n", xlab = NA)
axis(side=1, at=bplot, labels = FALSE)
text(x=bplot, y=-5, labels=ce$indicator, adj = 1, xpd = TRUE, cex=1, srt=60)

## And second y-axis for expected cost
par(new=TRUE, xpd = TRUE)
plot(x=bplot, y=ce$detect_p_firegrazing, type = "b", col="blue",
     axes = FALSE,  xlab = "", ylab = "", xlim = c(0, length(ce$effectiveness) + 1))
## Add line for relative cost of monitoring
lines(x=bplot, y=ce$detect_p_catpredation, col="red", type = "b", lwd = 1, lty =2)
lines(x=bplot, y=ce$detect_p_weeds, col="darkgreen", type = "b", lwd = 1, lty =3)
mtext("Probability(detect firegrazing)", side=4, col="blue", line=3, las = 3, adj = 0.62)
mtext("Probability(detect catpredation)", side=4, col="red", line=4.2, las = 3, adj = 0.7)
mtext("Probability(detect weeds)", side=4, col="darkgreen", line=5.3, las = 3, adj = 0.5)
axis(4, ylim=c(0,ceiling(max(ce$exp_benefit)/50)*50), col="blue", col.axis="blue",las=1)
segments(17.3,0.03,17.3,0.14, col = "blue", lty = 1, lwd = 2)
segments(17.9,0.03,17.9,0.15, col = "red", lty = 2, lwd = 2)
segments(18.4,0.03,18.4,0.15, col = "darkgreen", lty = 3, lwd = 2)
shape::Arrows(2.5,-0.85,12.5,-0.85, lwd = 1.5, arr.type="triangle", arr.width=0.2)
mtext("Indicators arranged in decreasing order of cost-effectiveness", side=1, line = 13.5)
dev.off()


## ------------------------------------------------------
## FLIPPED: Expected/Relative cost plots (Fig 4b)
## ------------------------------------------------------
bplot <- barplot(ce$exp_cost)
## Plot line for expected cost
pdf(file = "./output/expcost_plot_flipped.pdf", width=10, height=8)
par(las=1, mar=c(15,6,8,8), oma=c(0,0,0,0))
plot(x=bplot, y=ce$exp_cost, type = "b", col = "purple",
     axes = TRUE, ylab = "Cost of monitoring",  
     ylim = c(0, ceiling(max(ce$exp_cost)/50)*50), 
     xlim = c(0, length(ce$exp_cost) + 2), 
     xpd=FALSE, bty = "n", xaxt="n", xlab = NA)
axis(side=1, at=bplot, labels = FALSE, pos = -20)
text(x=bplot, y=-30, labels=ce$indicator, adj = 1, xpd = TRUE, cex=1, srt=60)
## For purple LHS y_axis: set axes = FALSE above and...
# axis(side = 2, col = "purple", col.axis = "purple")
# mtext(side = 2, line = 4, "Cost of monitoring", col = "purple", las = 3)

## Add line for relative cost
lines(x=bplot, y=ce$ind_cost, type = "b", lwd = 1, lty = 1, col = "darkgreen")

## And second y-axis & line for expected benefit
par(new=TRUE, xpd = TRUE)
plot(x=bplot, y=ce$exp_benefit, type = "b", lty = 2,
     axes = FALSE,  xlab = "", ylab = "", xlim = c(0, length(ce$effectiveness) + 2), 
     ylim = c(0,ceiling(max(ce$exp_benefit))))
axis(4, seq(0,ceiling(max(ce$exp_benefit)), by = 10), las=1, pos = 17)
mtext("Expected benefit", side = 4, line = 3.5, las = 3)
# shape::Arrows(1.5,-39,14.5,-39, lwd = 1.5, arr.type="triangle", arr.width = 0.2)
# mtext("Indicators arranged in decreasing order of cost-effectiveness", side=1, line = 13.5)

## Legend
legend(12,52,
       c("Expected cost", "Relative cost", "Expected benefit"), 
       lty=c(1,1,2), col = c("purple", "darkgreen", "black"), lwd=2,  box.col=NA, cex=1)
dev.off()


## ------------------------------------------------------
## FLIPPED: Detection probability plots (Fig 4a)
## ------------------------------------------------------
bplot <- barplot(ce$detect_p_firegrazing)

pdf(file = "./output/detectprob_plot_flipped.pdf", width=10, height=8)
par(las=1, mar=c(15,6,8,8), oma=c(0,0,0,0))

## Plot detectprob line 1
plot(x=bplot, y=ce$detect_p_firegrazing, type = "b", col="purple", lty = 1,
     axes = TRUE, ylab = "Probability of detecting trigger point for threat", 
     ylim = c(0, 1), 
     xlim = c(0, length(ce$effectiveness) + 2), 
     xpd=FALSE, bty = "n", xaxt="n", xlab = NA)
axis(side=1, at=bplot, labels = FALSE, pos = - 0.1)
text(x=bplot, y= - 0.15, labels=ce$indicator, adj = 1, xpd = TRUE, cex=1, srt=60)

## Add lines for detectprob 2 & 3
lines(x=bplot, y=ce$detect_p_catpredation, col="darkgoldenrod4", 
      type = "b", lty = 1)
lines(x=bplot, y=ce$detect_p_weeds, col="darkgreen", type = "b", 
      lty = 1)

## And second y-axis & line for expected benefit
par(new=TRUE, xpd = TRUE)
plot(x=bplot, y=ce$exp_benefit, type = "b", lty = 2,
     axes = FALSE,  xlab = "", ylab = "", xlim = c(0, length(ce$effectiveness) + 2), 
     ylim = c(0,ceiling(max(ce$exp_benefit))))
axis(4, seq(0,ceiling(max(ce$exp_benefit)), by = 10), las=1, pos = 17)
mtext("Expected benefit", side = 4, line = 3.5, las = 3)
# shape::Arrows(1.5,-39,14.5,-39, lwd = 1.5, arr.type="triangle", arr.width = 0.2)
# mtext("Indicators arranged in decreasing order of cost-effectiveness", side=1, line = 13.5)

## Legend
legend(12,55,
       c("Pr(fire & grazing)", "Pr(cat predation)", "Pr(weeds)","Expected benefit"), 
       lty=c(1,1,1,2), col = c("purple", "darkgoldenrod4", "darkgreen", "black"), lwd=2,  box.col=NA, cex=1)
dev.off()


## ------------------------------------------------------
## Cumulative expected cost versus cumulative expected benefit plot
## ------------------------------------------------------
##  Note: Benefit of monitoring an indicator is represented as the overall increase in 
##  probability of persistence (on average) across all species under management of all threats
cost <- cumsum(ce$exp_cost)
benefit <- cumsum(ce$exp_benefit)
dimidx <- find.dim.point(ce$exp_cost, ce$exp_benefit, threshold = 0.2)$idx

pdf(file = "./output/dimreturns_plot.pdf", width=11, height=7)
par(mfrow = c(1,1), las = 0, mar = c(5, 7, 5, 7) + 0.1, xpd = FALSE, cex = 1)
plot(cost, benefit, lwd = 2, type = "l", xlab = "Cumulative expected cost",
     ylab = "Cumulative expected benefit", xlim = c(-50, max(cost)), 
     ylim = c(min(benefit), ceiling(max(benefit)/50)*50))
points(cost, benefit, pch=19)
segments(cost[dimidx], 0, cost[dimidx],
         benefit[dimidx],lty = 2)
segments(-100,benefit[dimidx], cost[dimidx],
         benefit[dimidx],lty = 2)
# text(cost[1:dimidx]-30,benefit[1:dimidx],
#      paste(ce$ind_ranks[1:dimidx]))
text(cost[dimidx]-20,130, "diminishing returns threshold", srt=90)
text(700,cumsum(ce$exp_benefit)[1:dimidx], ce$indicators[1:dimidx], adj = 0)

## Add benefit * feasibility line
benefit.feasibility <- cumsum(ce$exp_benefit * ce$ind_feasibility)
dimidx <- find.dim.point(ce$exp_cost, (ce$exp_benefit * ce$ind_feasibility), threshold = 0.2)$idx
lines(cost, benefit.feasibility, lwd=2, type="l", col="blue")
points(cost, benefit.feasibility,pch=19, col="blue")
segments(cost[dimidx], 0,cost[dimidx],
         benefit.feasibility[dimidx],lty = 2, col="blue")
segments(-100, benefit.feasibility[dimidx], cost[dimidx],
         benefit.feasibility[dimidx],lty = 2, col="blue")
# text(cost[1:dimidx]+30, benefit.feasibility[1:dimidx],
#      paste(ce$ind_ranks[1:dimidx]), col="blue")
axis(4, col="blue", col.axis = "blue")
mtext("Cumulative (benefit * feasibility)",side=4,line=3, col="blue")
dev.off()


## ------------------------------------------------------
## Prioritisation plot (Fig 5)
## ------------------------------------------------------
## Line for cumulative benefit
pdf(file = "./output/ppp_plot.pdf", width=11, height=7)

cost <- cumsum(ce$exp_cost)
benefit <- cumsum(ce$exp_benefit)
dimidx <- find.dim.point(ce$exp_cost, ce$exp_benefit, threshold = 0.2)$idx
off.x <- rep(0,length(cost))
off.x[1]<- cost[1]-20
off.x[2]<- cost[2]-20
off.x[3]<- cost[3]-20
off.x[4]<- cost[4]-20
off.x[5]<- cost[5]-20
off.x[6]<- cost[6]-20
off.x[7]<- cost[7]-20
off.x[8]<- cost[8]-20
off.x[9]<- cost[9]
off.x[10]<- cost[10]
off.x[11]<- cost[11]
off.x[12]<- cost[12]
off.x[13]<- cost[13]
off.x[14]<- cost[14]-5

off.y <- rep(0,length(benefit))
off.y[1]<- benefit[1]
off.y[2]<- benefit[2]
off.y[3]<- benefit[3]
off.y[4]<- benefit[4]+5
off.y[5]<- benefit[5]+10
off.y[6]<- benefit[6]+10
off.y[7]<- benefit[7]+10
off.y[8]<- benefit[8]+10
off.y[9]<- benefit[9]+10
off.y[10]<- benefit[10]+10
off.y[11]<- benefit[11]+10
off.y[12]<- benefit[12]+10
off.y[13]<- benefit[13]+10
off.y[14]<- benefit[14]+10

par(mfrow=c(1,1), las=0, mar=c(5, 5, 2, 15), xpd = FALSE, cex=1)
plot(cost,benefit, lwd=1, type="l", xlab="Cumulative expected cost",
     ylab="Cumulative expected benefit", xlim=c(-30,max(cost)+30), ylim=c(-10,400))
points(cost,benefit,pch=20)
segments(cost[dimidx], -20, cost[dimidx],
         benefit[dimidx],lty = 2)
segments(-100,benefit[dimidx], cost[dimidx],
         benefit[dimidx],lty = 2)
text(cost[dimidx]-20,110, "diminishing returns", srt=90)
text(cost[dimidx]+20,90, "         threshold", srt=90)
text(off.x,off.y, ce$ind_ranks)
# text(off.x,off.y, indicators, adj = 0)
par(xpd = TRUE)
text(x = rep(1180,14), y = seq(360, -30, length.out = 14), 
     labels = paste0(ce$ind_ranks, ": ", ce$indicators), adj = 0)
text(1180, 400, "Indicators ranked in decreasing\n order of cost-effectiveness:", adj = 0, font = 2) 

## Add line for cumulative relative cost of monitoring indicators
temp <- ce[order(ce$ind_cost),] # arranged according to increasing costs
cost <- cumsum(temp$exp_cost)
benefit <- cumsum(temp$exp_benefit)
off.x[1]<- cost[1]+30
off.x[2]<- cost[2]+30
off.x[3]<- cost[3]+30
off.x[4]<- cost[4]+30
off.x[5]<- cost[5]+20
off.x[6]<- cost[6]+20
off.x[7]<- cost[7]+30
off.x[8]<- cost[8]+30
off.x[9]<- cost[9]
off.x[10]<- cost[10]+20
off.x[11]<- cost[11]
off.x[12]<- cost[12]
off.x[13]<- cost[13]
off.x[14]<- cost[14]-5

off.y[1]<- benefit[1]
off.y[2]<- benefit[2]
off.y[3]<- benefit[3]
off.y[4]<- benefit[4]
off.y[5]<- benefit[5]-10
off.y[6]<- benefit[6]-10
off.y[7]<- benefit[7]-10
off.y[8]<- benefit[8]-5
off.y[9]<- benefit[9]-15
off.y[10]<- benefit[10]-10
off.y[11]<- benefit[11]-10
off.y[12]<- benefit[12]-10
off.y[13]<- benefit[13]-10
off.y[14]<- benefit[14]-10

lines(cost, benefit, lwd=1, lty = 2, col="purple")
points(cost, benefit, pch=20, col="purple")
text(off.x, off.y, temp$ind_ranks, col="purple")

## Add line for average probability of dectection across threats: sum of pij over j/length j
temp <- ce
temp$avg.p <- rowMeans(cbind(temp$detect_p_firegrazing, temp$detect_p_catpredation, temp$detect_p_weeds))
temp <- ce[order(temp$avg.p),] # arranged according to decreasing avg.pij
cost <- cumsum(temp$exp_cost)
benefit <- cumsum(temp$exp_benefit)
off.x <- rep(0,length(cost))
off.x[1]<- cost[1]+30
off.x[2]<- cost[2]+20
off.x[3]<- cost[3]+20
off.x[4]<- cost[4]+20
off.x[5]<- cost[5]+30
off.x[6]<- cost[6]+30
off.x[7]<- cost[7]+20
off.x[8]<- cost[8]+20
off.x[9]<- cost[9]+20
off.x[10]<- cost[10]+30
off.x[11]<- cost[11]+20
off.x[12]<- cost[12]+20
off.x[13]<- cost[13]+20
off.x[14]<- cost[14]+30
off.y <- rep(0,length(benefit))
off.y[1]<- benefit[1]-10
off.y[2]<- benefit[2]-10
off.y[3]<- benefit[3]-10
off.y[4]<- benefit[4]-10
off.y[5]<- benefit[5]
off.y[6]<- benefit[6]-5
off.y[7]<- benefit[7]-10
off.y[8]<- benefit[8]-10
off.y[9]<- benefit[9]-10
off.y[10]<- benefit[10]-5
off.y[11]<- benefit[11]-10
off.y[12]<- benefit[12]-10
off.y[13]<- benefit[13]-10
off.y[14]<- benefit[14]

lines(cost, benefit, lwd=1, lty = 2, type="l", col="darkgreen")
points(cost, benefit, pch=20,col="darkgreen")
text(off.x, off.y, temp$ind_ranks, col="darkgreen")

## Legend
legend(620,100,
       c("Cost effectiveness", "Relative cost",
         "Probability of detecting triggers"), y.intersp=2.2,
       lty=c(1,2,2), lwd=2, col=c("black", "purple", "darkgreen"), box.col=NA, cex=1)
text(770, 105, "Indicator prioritisation based on:", font=2)
dev.off()

## Ranking acc. to avg.q == CE ranking
temp <- ce
temp$avg.q <- rowMeans(cbind(temp$type1_q_firegrazing, temp$type1_q_catpredation, temp$type1_q_weeds))
temp <- ce[order(temp$avg.q),] # arranged according to decreasing avg.qij
temp$ind_ranks == ce$ind_ranks


