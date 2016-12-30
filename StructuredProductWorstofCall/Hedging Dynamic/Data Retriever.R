setwd("D:/Anmol/Workspace/Anmol Intern/StructuredProductWorstOfCall/Hedging Dynamic")
load("Simdata/cumulative_pnl.Rdata", envir = parent.frame(), verbose = FALSE)
load("Simdata/delta_interest.Rdata", envir = parent.frame(), verbose = FALSE)
load("Simdata/actual_payoff.Rdata", envir = parent.frame(), verbose = FALSE)
load("Simdata/bond.Rdata", envir = parent.frame(), verbose = FALSE)
load("Simdata/prices.Rdata", envir = parent.frame(), verbose = FALSE)
load("Simdata/roll_cost.Rdata", envir = parent.frame(), verbose = FALSE)
load("Simdata/delta_values.Rdata", envir = parent.frame(), verbose = FALSE)
load("Simdata/delta_pnl.Rdata", envir = parent.frame(), verbose = FALSE)
library(reshape2)
library(ggplot2)


scenario_number = 40
z = scenario_number
days = dim(cum_pnl)[2]
bond = yx
total_best = cum_pnl[z,] + daily_interest_delta_pnl[z,] + actual_payoff[z,] + bond[] + roll_cost[z,]*-1
sim_data <- cbind(prices[z,,], delta_all[z,,], delta_pnl_complete[z,], cum_pnl[z,], daily_interest_delta_pnl[z,], actual_payoff[z,], bond[], roll_cost[z,]*-1, total_best)
colnames(sim_data)<-c("Stk1","Stk2", "Stk3", "Delta1", "Delta2", "Delta3", "Daily_Hedge_MTM", "Cum_Hedge_MTM","Hedge_Int_MTM","OTC_MTM","Interest_MTM","Roll_MTM","Total_MTM")
del <- paste("sim_data-",z,".csv", sep = "")
delete_check <- paste("Output/","sim_data-",z,".csv", sep = "")
if(file.exists(delete_check)) file.remove(delete_check)
write.csv(sim_data, file.path('Output', del))
library(ggplot2)
x = 1:days
dat <- data.frame(x, cum_pnl[z,], daily_interest_delta_pnl[z,], actual_payoff[z,], bond[], roll_cost[z,]*-1, total_best)
dat_melt <- melt(dat, "x")
myPlot = ggplot(dat_melt, aes(x, value, colour = variable)) + geom_line() + facet_wrap(~ variable, ncol = 1, scales = "free_y") + scale_colour_discrete()
ggsave(filename="Output/simplot.jpg", width = 10, height = 6, plot=myPlot)
