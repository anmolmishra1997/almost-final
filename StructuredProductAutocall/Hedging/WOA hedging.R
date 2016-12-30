setwd("D:/Anmol/Workspace/Anmol Intern/StructuredProductAutocall/Hedging")
library('Rcpp')
library('RcppArmadillo')
library('abind')
library(scales)
Rcpp::sourceCpp('Rcpp_Functions/Minimum_3D.cpp')
Rcpp::sourceCpp('Rcpp_Functions/Cumulative_Hedge_Pnl.cpp')
Rcpp::sourceCpp('Rcpp_Functions/Delta_Pnl.cpp')
Rcpp::sourceCpp('Rcpp_Functions/Elementwise_Power.cpp')
Rcpp::sourceCpp('Rcpp_Functions/Roll_Cost.cpp')
Rcpp::sourceCpp('Rcpp_Functions/Mean_Scenarios.cpp')
Rcpp::sourceCpp('Rcpp_Functions/Minimum_Position_3D.cpp')
Rcpp::sourceCpp('Rcpp_Functions/Mean_2D_Scenarios.cpp')

input1 = read.table(file="Input/input for WOA.csv",header = TRUE,sep = "," , stringsAsFactors = FALSE)
valuation_date = strptime(input1[1,2],"%d-%b-%y")
initial_fixing_date <- strptime(input1[1,12],"%d-%b-%y")
final_fixing_date <- strptime(input1[1,3],"%d-%b-%y")
redemption_date <- strptime(input1[1,6],"%d-%b-%y")
observation_dates <- strptime(strsplit(input1[1,4], ";")[[1]],"%d-%b-%y")
autocall_level <- as.numeric(input1[1,8])
value_initial <- as.numeric(input1[1,7])
contingent_value <- as.numeric(input1[1,13])
scenarios <- as.numeric(input1[1,14])
assets <- as.numeric(input1[1,18])
hedge_scenarios = as.numeric(input1[1,19])
contingent_coupon <- as.numeric(input1[1,5])/100
ir <- as.numeric(input1[1,15])/100
payment_gap = as.numeric(input1[1,11])
days <-  as.numeric(as.Date(final_fixing_date) - as.Date(initial_fixing_date)) + 1
pp <- as.numeric(input1[1,17])
gamma = as.numeric(input1[1,20])
rate = ir
# pen <- as.numeric(input1[1,16])
# iv <-  as.numeric(input1[1,9])/100
# pr <-  as.numeric(input1[1,10])/100

input2 <- read.csv(file = "Input/sig.csv", header = FALSE)
mu <- input2[,2]
sigma <- input2[,1]
corr_coef <- t(chol(read.csv(file = "Input/mat.csv", header = FALSE)))
prices <- array(0, dim = c(scenarios,days,assets))
prices[,1,] = array(value_initial, dim = c(assets))
conster = ( mu-(sigma^2)/2 ) * (1.0/365)
conster = t(replicate(scenarios, conster))
var = (0.5 * sigma) * sqrt(1/365)  # Slight doubt over formula. There is a doubt whether 0.5 should be multiplied or not
var = t(replicate(scenarios, var))

"Bond Calculation"
# bond_days = as.numeric(as.Date(redemption_date) - as.Date(valuation_date)) + 1
step = as.numeric(as.Date(initial_fixing_date) - as.Date(valuation_date))
# bond = array(0, dim = c(bond_days))
bond = array(0, dim = c(days))
bond[days] = value_initial
# bond = value_initial/((1 + rate)^((bond_days - 1:bond_days)/365))
bond = value_initial/((1 + rate)^((days - 1:days)/365))
bond_all = t(replicate(scenarios, bond))
bond_save = bond[(step+1):(step+days)]
save( bond_save , file = "Simdata/bond.Rdata")

for (j in 2:days) #Could transfer this loop to RCpp
{
  ind_rand <- matrix( rnorm(assets*scenarios), assets, scenarios) 
  dep_rand <- t(corr_coef %*% ind_rand)
  varrer <- var * dep_rand
  prices[,j,] = prices[,j-1,]*exp(conster+varrer)
}
save(prices , file = "Simdata/prices.Rdata")

# Major time loss in printing. To be optimised, rather ideally to be done over RCpp
for (k in 1:assets)
{
  file_name<-paste("asset-",k,".csv", sep = "")
  delete_check <- paste("Output/","asset-",k,".csv", sep = "")
  if(file.exists(delete_check)) file.remove(delete_check)
  write.csv(prices[,,k], file.path('Output', file_name))
}

minimum_asset <- Minimum_3D(prices)

observation_days = c(0,as.numeric(observation_dates - initial_fixing_date) + 1)
observation_periods = diff(observation_days)
coupon_expired = array ( 0 ,dim = c(scenarios))
coupon_increment = array( 0 , dim = c(scenarios))
expiry_day = array(days, dim= c(scenarios))
actual_coupon = array(0, c(scenarios,days))

for (i in 1:(length(observation_dates)-1))
{
  coupon_increment = ifelse(minimum_asset[,observation_days[i+1]] > contingent_value & coupon_expired!= 1, contingent_coupon*observation_periods[i]/365,0)
  actual_coupon[,(observation_days[i+1]+1):days] = actual_coupon[,(observation_days[i+1]+1):days] + replicate(days - observation_days[i+1], coupon_increment)
  coupon_expired = ifelse(minimum_asset[,observation_days[i+1]] > autocall_level , 1,coupon_expired)
  expiry_day = ifelse( coupon_expired == 1 & expiry_day == days, observation_days[i+1], expiry_day)
}
actual_coupon = actual_coupon * value_initial

"Paramters for hedging simulation. Can be altered here, or even modified to be read 
from a file"
hedge_scenarios <- scenarios
hedge_prices <- array(0, dim = c(hedge_scenarios,days,assets))
hedge_prices[,1,] = array(value_initial, dim = c(assets))
hedge_conster = ( mu-(sigma^2)/2 ) * (1.0/365)
hedge_conster = t(replicate(hedge_scenarios, hedge_conster))
hedge_var = (0.5 * sigma) * sqrt(1/365)
hedge_var = t(replicate(hedge_scenarios, hedge_var))

"Hedging Simulation"
for (j in 2:days)
{
  ind_rand <- matrix( rnorm(assets*hedge_scenarios), assets, hedge_scenarios) 
  dep_rand <- t(corr_coef %*% ind_rand)
  hedge_varrer <- hedge_var * dep_rand
  hedge_prices[,j,] = hedge_prices[,j-1,]*exp(hedge_conster+ hedge_varrer)
}

payoff <- function( prices_part, hedge_final, initial_value, expiry, check_days, check_time, coupon_cutoff, coupon_rate, autocall_cutoff ) 
{
  simulations = dim(prices_part)[1]
  time = dim(prices_part)[2]
  total_coupon = array(0, dim = c(simulations, time))
  coupon_expired = array ( 0 ,dim = c(simulations, time))
  for ( i in 1:length(check_time))
  {
    temp = if (check_days[i+1] > expiry) expiry else check_days[i+1]
    hedge_multiple = aperm(replicate(temp, hedge_final[,check_days[i+1],] ), c(1,3,2))
    
    hedge_prices_new = prices_part[,1:temp,] * hedge_multiple/hedge_final[,1:temp,]
    
    min_stock <- Minimum_3D(hedge_prices_new)
    
    coupon_increment = ifelse( min_stock > coupon_cutoff & coupon_expired[,1:temp]!=1 , coupon_rate * check_time[i]/365, 0)
    total_coupon[,1:temp] = total_coupon[,1:temp] + coupon_increment
    coupon_expired[,1:temp] = ifelse( min_stock > autocall_cutoff, 1, coupon_expired)
  }
  initial_value * (total_coupon)
}

a = 20

delta_all = array(0, dim = c(scenarios,days,assets)) #Will store final delta values
actual_payoff = array(0, dim = c(scenarios,days))

if ( gamma == 0)
{
  for (a in 1:scenarios)
  {
    Rprof("reportanalytic")
    {"This block uses vectorization. Prices of all days in Actual simulation number 'a' have been
      replicated to form a 3D matrix again. The dimensions being 'simulations' * 'days' * 'assets'"}
    prices_sim <- aperm(replicate(scenarios, prices[a,,]), c(3,1,2))
    prices_part_sim <- aperm(replicate(scenarios, prices[a,1:expiry_day[a],]), c(3,1,2))
    payoff_temp = payoff(prices_part_sim, hedge_prices, value_initial ,expiry_day[a], observation_days, observation_periods, contingent_value, contingent_coupon  ,autocall_level)
    
    temp = array(0, c(scenarios, days - expiry_day[a]))
    payoff_a = abind(payoff_temp, temp, along = 2)
    actual_payoff[a,] = Mean_2D_Scenarios(payoff_a)
    {"IMPORTANT : Confirm if payoff and hedgepayoff served the same purpose"
      # Copy the same as above for hedgepayoff ( most probably same as payoff, raise a point)
    }
    
    payoff_increment = array(0, c(hedge_scenarios, days, assets))
    delta_hedge = array(0, c(hedge_scenarios, days, assets))
    
    
    {"Calculation of delta by incrementing each stock by 1% each time. The procedure for finding
      payoff remains the same as above."}
    for (k in 1:assets)
    {
      prices_part_increment = prices_part_sim
      prices_part_increment[,,k] = prices_part_sim[,,k] * 1.01
      payoff_increment_temp <- payoff(prices_part_increment, hedge_prices, value_initial,expiry_day[a], observation_days, observation_periods, contingent_value, contingent_coupon  ,autocall_level)
      payoff_increment[,,k] = abind(payoff_increment_temp, temp, along = 2)
      delta_hedge[,,k] = (payoff_increment[,,k] - payoff_a)/(prices_sim[,,k]*0.01)
    }
    {"Calculates mean of delta over all hedge simulations, returning a 'days' * 'assets' matrix"
      "Storing the delta values for all simulations"}
    delta_all[a,,] = Mean_Scenarios(delta_hedge)
    Rprof(NULL)
    summaryRprof("reportanalytic")
    
    }
}

delta_all_increment = array(0, dim = c(scenarios,days,assets))
delta_all_decrement = array(0, dim = c(scenarios,days,assets))
{"This block shall be run if gamma analysis also needs to be done. Delta calculation is repeated
  twice, using which gamma is easily calculated"}
if (gamma == 1)
{
  for (a in 1:scenarios)
  {
    
    prices_part_sim <- aperm(replicate(scenarios, prices[a,,]), c(3,1,2))
    hedge_final_day <- aperm(replicate(days, hedge_prices[,days,] ), c(1,3,2))
    hedge_prices_new <- prices_part_sim * hedge_final_day / hedge_prices
    
    payoff_a = payoff(hedge_prices_new, value_initial)
    actual_payoff[a,] = Mean_2D_Scenarios(payoff_a)
    {# Copy the same as above for hedgepayoff ( most probably same as payoff, raise a point)
    }
    
    payoff_increment = array(0, c(hedge_scenarios, days, assets))
    payoff_decrement = array(0, c(hedge_scenarios, days, assets))
    delta_hedge_increment = array(0, c(hedge_scenarios, days, assets))
    delta_hedge_decrement = array(0, c(hedge_scenarios, days, assets))
    hedge_prices_increment = hedge_prices_new
    hedge_prices_decrement = hedge_prices_new
    
    for (k in 1:assets)
    {
      hedge_prices_increment[,,k] = hedge_prices_new[,,k] * 1.01
      payoff_increment[,,k] <- payoff(hedge_prices_increment, value_initial)
      delta_hedge_increment[,,k] = (payoff_increment[,,k] - payoff_a)/(prices_part_sim[,,k]*0.01)
      
      hedge_prices_decrement[,,k] = hedge_prices_new[,,k] * 0.99
      payoff_decrement[,,k]<-payoff(hedge_prices_decrement, value_initial)
      delta_hedge_decrement[,,k] = (payoff_decrement[,,k] - payoff_a)/(prices_part_sim[,,k]*0.01)
    }
    
    delta_all_increment[a,,] = delta_all[a,,] = Mean_Scenarios(delta)
    delta_all_decrement[a,,] = Mean_Scenarios(delta)
  }
  gamma_final = delta_all_increment + delta_all_decrement
}
actual_payoff = actual_payoff + actual_coupon
save(delta_all, file = "Simdata/delta_values.Rdata")
save(actual_payoff , file = "Simdata/actual_payoff.Rdata")


"Printing the final values of delta for all assets"
for (k in 1:assets)
{
  del <- paste("finaldelta-",k,".csv", sep = "")
  delete_check <- paste("Output/","finaldelta-",k,".csv", sep = "")
  if(file.exists(delete_check)) file.remove(delete_check)
  write.csv(delta_all[,,k], file.path('Output', del))  
}

delta_pnl = Delta_Pnl_Function(prices, delta_all)
delta_pnl_complete = cbind(delta_pnl,delta_pnl[,days-1])
save(delta_pnl_complete , file = "Simdata/delta_pnl.Rdata")

roll_cost =  Roll_Cost(delta_all, mu)
save(roll_cost , file = "Simdata/roll_cost.Rdata")

final_roll_cost = rowSums(roll_cost) * -1

cum_pnl = Cumulative_Hedge_Pnl(delta_pnl)
cum_pnl[,days] = cum_pnl[,days-1]
save(cum_pnl , file = "Simdata/cumulative_pnl.Rdata")

hedge_pnl_int_rate = 0.075
daily_interest_delta_pnl = cum_pnl * hedge_pnl_int_rate * 1/365
save(daily_interest_delta_pnl , file = "Simdata/delta_interest.Rdata")

final_interest_delta_pnl = rowSums(daily_interest_delta_pnl)

total = cum_pnl[,days] + final_interest_delta_pnl + final_roll_cost - actual_payoff[,days]

output = cbind(1:scenarios, prices[,days,1],prices[,days,2],prices[,days,3], cum_pnl[,days], final_interest_delta_pnl, actual_payoff[,days],  final_roll_cost, total)
colnames(output) <- c("Sim Count","Stk1","Stk2","Stk3","Hedge_MTM","Hedge_Int_MTM","OTC_MTM","Roll_MTM","Total_MTM")

final_output <- "Output.csv"
delete_check <- paste("Output/", final_output)
if(file.exists(delete_check)) file.remove(delete_check)
write.csv(output, file.path('Output', final_output))  
{
  # total = delta_pnl_complete + daily_interest_delta_pnl +bond_all[,(step+1):(step+days)] - principal - actual_payoff 
  # client_payoff = Mean_2D_Scenarios(actual_payoff)
  # Rprof("reportanalytic")
  # Rprof(NULL)
  # summaryRprof("reportanalytic")
  # matplot(t(total), type = "l")
}
outputdf = as.data.frame.matrix(output) 
# outputdf = outputdf[order(outputdf$Total_MTM),]

summary <- cbind(percent(mean(outputdf$Total_MTM)/value_initial),percent(max(outputdf$Total_MTM)/value_initial),which(outputdf$Total_MTM == max(outputdf$Total_MTM), arr.ind = TRUE),percent(min(outputdf$Total_MTM)/value_initial),which(outputdf$Total_MTM == min(outputdf$Total_MTM), arr.ind = TRUE),percent(length(which(outputdf$Total_MTM>0))/scenarios),percent(length(which(outputdf$Total_MTM<0))/scenarios),percent(mean(outputdf$Total_MTM[outputdf$Total_MTM>=0])/value_initial),percent(mean(outputdf$Total_MTM[outputdf$Total_MTM<0])/value_initial),percent(length(which(outputdf$Total_MTM>10000))/length(which(outputdf$Total_MTM>=0))),percent(length(which(outputdf$Total_MTM<(-5000)))/length(which(outputdf$Total_MTM<0))))
colnames(summary)<-c("Average Return","Max Return","Max Return Sim", "Min Return","Min Return Sim","Percentage Wins"," Percentage Losses", "Avg Payoff if Win", "Avg Payoff if Loss","Percent Wins > 10 Percent","Percent Losses < 5 Percent")
write.csv(summary, file.path('Output', 'Summary.csv'))

z = which(outputdf$Total_MTM == max(outputdf$Total_MTM), arr.ind = TRUE)[1]
total_best = cum_pnl[z,] + daily_interest_delta_pnl[z,] + actual_payoff[z,] + bond[(step+1):(step+days)] + roll_cost[z,]*-1
best_hedge <- cbind(prices[z,,], delta_all[z,,], delta_pnl_complete[z,], cum_pnl[z,], daily_interest_delta_pnl[z,], actual_payoff[z,], bond[(step+1):(step+days)], roll_cost[z,]*-1, total_best)
colnames(best_hedge)<-c("Stk1","Stk2", "Stk3", "Delta1", "Delta2", "Delta3", "Daily_Hedge_MTM", "Cum_Hedge_MTM","Hedge_Int_MTM","OTC_MTM","Interest_MTM","Roll_MTM","Total_MTM")
write.csv(best_hedge, file.path('Output', 'Best.csv'))

z = which(outputdf$Total_MTM == min(outputdf$Total_MTM), arr.ind = TRUE)[1]
total_worst = cum_pnl[z,] + daily_interest_delta_pnl[z,] + actual_payoff[z,] + bond[(step+1):(step+days)] + roll_cost[z,]*-1
worst_hedge <- cbind(prices[z,,], delta_all[z,,], delta_pnl_complete[z,], cum_pnl[z,], daily_interest_delta_pnl[z,], actual_payoff[z,], bond[(step+1):(step+days)], roll_cost[z,]*-1, total_worst)
colnames(worst_hedge)<-c("Stk1","Stk2", "Stk3", "Delta1", "Delta2", "Delta3", "Daily_Hedge_MTM", "Cum_Hedge_MTM","Hedge_Int_MTM","OTC_MTM","Interest_MTM","Roll_MTM","Total_MTM")
write.csv(worst_hedge, file.path('Output', 'Worst.csv'))
