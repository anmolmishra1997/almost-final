  setwd("D:/Anmol/Workspace/Anmol Intern/StructuredProductWorstOfCall/Hedging Dynamic")
  library('Rcpp')
  library('RcppArmadillo')
  library(scales)
  Rcpp::sourceCpp('Rcpp_Functions/Cumulative_Hedge_Pnl.cpp')
  Rcpp::sourceCpp('Rcpp_Functions/Delta_Pnl.cpp')
  Rcpp::sourceCpp('Rcpp_Functions/Minimum_3D.cpp')
  Rcpp::sourceCpp('Rcpp_Functions/Elementwise_Power.cpp')
  Rcpp::sourceCpp('Rcpp_Functions/Roll_Cost.cpp')
  Rcpp::sourceCpp('Rcpp_Functions/Mean_Scenarios.cpp')
  Rcpp::sourceCpp('Rcpp_Functions/Minimum_Position_3D.cpp')
  Rcpp::sourceCpp('Rcpp_Functions/Mean_2D_Scenarios.cpp')
  Rcpp::sourceCpp('Rcpp_Functions/Payoff_Function.cpp')
  
  # Reading first set of input
  input1 = read.table(file="Input/input for WOC.csv",header = TRUE,sep = "," , stringsAsFactors = FALSE)
  valuation_date = strptime(input1[1,2],"%d-%b-%y")
  initial_fixing_date <- strptime(input1[1,9],"%d-%b-%y")
  final_fixing_date <- strptime(input1[1,3],"%d-%b-%y")
  redemption_date <- strptime(input1[1,5],"%d-%b-%y")
  strike <- as.numeric(input1[1,4])
  value_initial <- as.numeric(input1[1,6])
  scenarios <- as.numeric(input1[1,10])
  ir <- as.numeric(input1[1,11])/100
  assets <- as.numeric(input1[1,14])
  days <-  as.numeric(as.Date(final_fixing_date) - as.Date(initial_fixing_date)) + 1
  step2 <-  as.numeric(as.Date(redemption_date) - as.Date(final_fixing_date))
  gamma <- as.numeric(input1[1,15])
  pr = as.numeric(input1[1,16])
  built_in = as.numeric(input1[1,18])
  rate = ir

  "Bond Calculation"
  bond_days = as.numeric(as.Date(redemption_date) - as.Date(valuation_date)) + 1
  step = as.numeric(as.Date(initial_fixing_date) - as.Date(valuation_date))
  bond = array(0, dim = c(bond_days))
  bond = (value_initial*(1-built_in))*((1 + rate)^((0:(bond_days-1))/365)-1)
  bond_all = t(replicate(scenarios, bond))
  yx = bond[(step+1):(step+days)]
  save( yx , file = "Simdata/bond.Rdata")
  
  "Monte Carlo Simulator"
  simulator <- function( day_first, day_last, correlation, constant, vary, simulated )
  {
    stock = dim(simulated)[3]
    sims = dim(simulated)[1]
    for (j in day_first:day_last)
    {
      ind_rand <- matrix( rnorm(stock * sims), stock, sims) 
      dep_rand <- t(correlation %*% ind_rand)
      variable <- vary * dep_rand
      simulated[,j,] = simulated[,j-1,]*exp(constant + variable)
    }
    simulated
  }
  
  prices <- array(0, dim = c(scenarios,days,assets))
  prices[,1,] = array(strike, dim = c(assets))
  input4 <- read.csv(file = "Input/Generated Parameters.csv")
  "Initial fixing date must be the first date given in parameters"
  day_start = 2
  day_end = 0
  i = 1
  for ( i in 1:(dim(input4)[1]-1))
  {
    sigma = as.numeric(input4[i,2:(1 + assets)])
    mu = as.numeric(input4[1, (2+assets):(1 + 2*assets)])
    file_path = as.character(input4[i,2*(assets + 1)])
    corr_coef <- t(chol(read.csv(file = file_path, header = FALSE)))
    date1 = strptime(input4[i+1 ,1],"%d-%b-%y")
    date2 = strptime(input4[i ,1],"%d-%b-%y")
    day_end = day_end + as.numeric(as.Date(date1) - as.Date(date2))
    conster = ( mu-(sigma^2)/2 ) * (1.0/365)
    conster = t(replicate(scenarios, conster))
    var = (0.5 * sigma) * sqrt(1/365)
    var = t(replicate(scenarios, var))
    prices = simulator( day_start, day_end, corr_coef, conster, var, prices )
    day_start = day_end + 1
  }
  date2 = strptime(input4[dim(input4)[1] ,1],"%d-%b-%y")
  day_end = day_end + as.numeric(as.Date(final_fixing_date) - as.Date(date2)) + 1
  prices = simulator( day_start, day_end, corr_coef, conster, var, prices )
  save(prices , file = "Simdata/prices.Rdata")
  
  "Probably not required to print simulated values in hedging"  
  for (k in 1:assets)
  {
    file_name<-paste("asset-",k,".csv", sep = "")
    delete_check <- paste("Output/","asset-",k,".csv", sep = "")
    if(file.exists(delete_check)) file.remove(delete_check)
    write.csv(prices[,,k], file.path('Output', file_name))
  }
  
  "Paramters for hedging simulation. Can be altered here, or even modified to be read 
    from a file"
  "Hedging Simulation"
  hedge_scenarios <- scenarios
  hedge_prices <- array(0, dim = c(hedge_scenarios,days,assets))
  hedge_prices[,1,] = array(strike, dim = c(assets))
  input4 <- read.csv(file = "Input/Generated Parameters.csv")
  day_start = 2
  day_end = 0
  for ( i in 1:(dim(input4)[1]-1))
  {
    mu = as.numeric(input4[i,2:(1 + assets)])
    sigma = as.numeric(input4[1, (2+assets):(1 + 2*assets)])
    file_path = as.character(input4[i,2*(assets + 1)])
    corr_coef <- t(chol(read.csv(file = file_path, header = FALSE)))
    date1 = strptime(input4[i+1 ,1],"%d-%b-%y")
    date2 = strptime(input4[i ,1],"%d-%b-%y")
    day_end = day_end + as.numeric(as.Date(date1) - as.Date(date2))
    hedge_conster = ( mu-(sigma^2)/2 ) * (1.0/365)
    hedge_conster = t(replicate(hedge_scenarios, hedge_conster))
    hedge_var = (0.5 * sigma) * sqrt(1/365)
    hedge_var = t(replicate(hedge_scenarios, hedge_var))
    hedge_prices = simulator( day_start, day_end, corr_coef, conster, var, hedge_prices )
    day_start = day_end + 1
  }
  date2 = strptime(input4[dim(input4)[1] ,1],"%d-%b-%y")
  day_end = day_end + as.numeric(as.Date(final_fixing_date) - as.Date(date2)) + 1
  hedge_prices = simulator( day_start, day_end, corr_coef, conster, var, hedge_prices )
  
  # for (k in 1:assets)
  # {
  #   file_name<-paste("hedgeasset-",k,".csv", sep = "")
  #   delete_check <- paste("Output/","hedgeasset-",k,".csv", sep = "")
  #   if(file.exists(delete_check)) file.remove(delete_check)
  #   write.csv(hedge_prices[,,k], file.path('Output', file_name))
  # }
  
  payoff <- function( asset_prices, initial_value ) 
  {
    "Minimum_3D calculates the minimum along stock dimension, in the extrapolated prices."
    min_stock <- Minimum_3D(asset_prices)
    
    "Minimum_Position_3D calculates the index of the minimum priced stock"
    min_stock_id <- Minimum_Position_3D(asset_prices)
    
    simulations = dim(asset_prices)[1]
    time = dim(asset_prices)[2]
    
    "Returns the mean of the minimum stock, using the index given by Minimum_Position_3D"
    mu_min_stock_id <- matrix(mu[min_stock_id],c(nrow(min_stock_id),ncol( min_stock_id)))
    powerraised = t(replicate(simulations , 1:time))
    
    "Discounting of the future simulated values for actual payoff"
    "Same as Delta WOC. Same as OTC MTM"
    
    pmax(pr*pmin(min_stock/initial_value - 1, 1),0)/(Elementwise_Power(1 + mu_min_stock_id,(time - powerraised)/365))
  }
  
  delta_all = array(0, dim = c(scenarios,days,assets)) #Will store final delta values
  actual_payoff = array(0, dim = c(scenarios,days))
  a = 20
  if ( gamma == 0)
  {
    for (a in 1:scenarios)
    {
      prices_part_sim <- aperm(replicate(scenarios, prices[a,,]), c(3,1,2))
      
      hedge_final_day <- aperm(replicate(days, hedge_prices[,days,] ), c(1,3,2))
      
      hedge_prices_new <- prices_part_sim * hedge_final_day / hedge_prices
      
      payoff_a <- Payoff_Function(hedge_prices_new, mu, value_initial, pr)
      actual_payoff[a,] = -1 * Mean_2D_Scenarios(payoff_a)
      
      payoff_increment = array(0, c(hedge_scenarios, days, assets))
      delta_hedge = array(0, c(hedge_scenarios, days, assets))
      
      for (k in 1:assets)
      {
        hedge_prices_increment = hedge_prices_new
        hedge_prices_increment[,,k] = hedge_prices_new[,,k] * 1.01
        payoff_increment[,,k]<- Payoff_Function(hedge_prices_increment, mu,value_initial, pr)
        delta_hedge[,,k] = (payoff_increment[,,k] - payoff_a)/(prices_part_sim[,,k]*0.01)
      }
      
      delta_all[a,,] = Mean_Scenarios(delta_hedge)
      
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
      
      payoff_a = Payoff_Function(hedge_prices_new,mu, value_initial, pr)
      actual_payoff[a,] = -1 * Mean_2D_Scenarios(payoff_a)
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
        payoff_increment[,,k] <- Payoff_Function(hedge_prices_increment,mu, value_initial,pr)
        delta_hedge_increment[,,k] = (payoff_increment[,,k] - payoff_a)/(prices_part_sim[,,k]*0.01)
        
        hedge_prices_decrement[,,k] = hedge_prices_new[,,k] * 0.99
        payoff_decrement[,,k]<- Payoff_Function(hedge_prices_decrement,mu, value_initial, pr)
        delta_hedge_decrement[,,k] = (payoff_decrement[,,k] - payoff_a)/(prices_part_sim[,,k]*0.01)
      }
      
      delta_all_increment[a,,] = delta_all[a,,] = Mean_Scenarios(delta_hedge_increment)
      delta_all_decrement[a,,] = Mean_Scenarios(delta_hedge_decrement)
    }
    gamma_final = delta_all_increment + delta_all_decrement
  }
  
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
  # cum_pnl[,days] = cum_pnl[,days-1]
  save(cum_pnl , file = "Simdata/cumulative_pnl.Rdata")
  
  hedge_pnl_int_rate = as.numeric(input1[1,17])
  daily_interest_delta_pnl = cum_pnl * hedge_pnl_int_rate * 1/365
  save(daily_interest_delta_pnl , file = "Simdata/delta_interest.Rdata")
  
  final_interest_delta_pnl = rowSums(daily_interest_delta_pnl)
  
  total = cum_pnl[,days - 1] + final_interest_delta_pnl + final_roll_cost + actual_payoff[,days] + bond[bond_days]
  
  cum_pnl[,days] = cum_pnl[,days-1]
  
  stock_columns = NULL
  delta_columns = NULL
  gamma_columns = NULL 
  
  for ( i in 1:assets)
  {
    stock_columns = append(stock_columns, paste("Stk",i, sep = ""))
    delta_columns = append(delta_columns, paste("Delta",i,sep = ""))
    if (gamma == 1)
    {
      gamma_columns = append(gamma_columns, paste("Gamma",i, sep = ""))
    }
  }
  
  
  output = cbind(1:scenarios, prices[,days,1:assets], cum_pnl[,days], final_interest_delta_pnl, actual_payoff[,days], bond[bond_days], final_roll_cost, total)
  
  colnames(output) <- c("Sim Count",stock_columns,"Hedge_MTM","Hedge_Int_MTM","OTC_MTM","Interest_MTM", "Roll_MTM","Total_MTM")
  
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
  
  summary <- cbind ( percent(mean(outputdf$Total_MTM) / value_initial) , percent(max(outputdf$Total_MTM)/value_initial), which(outputdf$Total_MTM == max(outputdf$Total_MTM), arr.ind = TRUE), percent(min(outputdf$Total_MTM)/value_initial), which(outputdf$Total_MTM == min(outputdf$Total_MTM), arr.ind = TRUE) , percent(length(which(outputdf$Total_MTM>0))/scenarios), percent(length(which(outputdf$Total_MTM<0))/scenarios) , percent(mean(outputdf$Total_MTM[outputdf$Total_MTM>=0])/value_initial) , percent(mean(outputdf$Total_MTM[outputdf$Total_MTM<0])/value_initial) , percent(length(which(outputdf$Total_MTM> (value_initial * 1.1)))/length(which(outputdf$Total_MTM>= value_initial))),percent(length(which(outputdf$Total_MTM<(value_initial * 0.95)))/length(which(outputdf$Total_MTM<value_initial))))
  colnames(summary)<-c("Average Return","Max Return","Max Return Sim", "Min Return","Min Return Sim","Percentage Wins"," Percentage Losses", "Avg Payoff if Win", "Avg Payoff if Loss","Percent Wins > 10 Percent","Percent Losses < 5 Percent")
  write.csv(summary, file.path('Output', 'Summary.csv'))
  gamma_output = NULL
  z = which(outputdf$Total_MTM == max(outputdf$Total_MTM), arr.ind = TRUE)
  total_best = cum_pnl[z,] + daily_interest_delta_pnl[z,] + actual_payoff[z,] + roll_cost[z,]*-1
  if ( gamma == 1)
  {gamma_output = gamma_final[z,,]}
  
  best_hedge <- cbind(prices[z,,], delta_all[z,,], gamma_output, delta_pnl_complete[z,], cum_pnl[z,], daily_interest_delta_pnl[z,], actual_payoff[z,], bond[(step+1):(step+days)], roll_cost[z,]*-1, total_best)
  
  colnames(best_hedge)<-c(stock_columns, delta_columns , gamma_columns, "Daily_Hedge_MTM", "Cum_Hedge_MTM","Hedge_Int_MTM","OTC_MTM","Interest_MTM","Roll_MTM","Total_MTM")
  best_hedge = rbind(best_hedge, t(replicate(step2, best_hedge[days,])))
  best_hedge[,(dim(best_hedge)[2] - 2)] = bond[(step+1):bond_days]
  best_hedge[,(dim(best_hedge)[2])] = best_hedge[,(dim(best_hedge)[2])] + bond[(step+1):bond_days]
  write.csv(best_hedge, file.path('Output', 'Best.csv'))
  
  z = which(outputdf$Total_MTM == min(outputdf$Total_MTM), arr.ind = TRUE)
  total_worst = cum_pnl[z,] + daily_interest_delta_pnl[z,] + actual_payoff[z,] + roll_cost[z,]*-1
  if ( gamma == 1)
  {gamma_output = gamma_final[z,,]}
  
  worst_hedge <- cbind(prices[z,,], delta_all[z,,], gamma_output, delta_pnl_complete[z,], cum_pnl[z,], daily_interest_delta_pnl[z,], actual_payoff[z,], bond[(step+1):(step+days)], roll_cost[z,]*-1, total_worst)
  colnames(worst_hedge)<-c(stock_columns, delta_columns , gamma_columns, "Daily_Hedge_MTM", "Cum_Hedge_MTM","Hedge_Int_MTM","OTC_MTM","Interest_MTM","Roll_MTM","Total_MTM")
  worst_hedge = rbind(worst_hedge, t(replicate(step2, worst_hedge[days,])))
  worst_hedge[,(dim(worst_hedge)[2] - 2)] = bond[(step+1):bond_days]
  worst_hedge[,(dim(worst_hedge)[2])] = worst_hedge[,(dim(worst_hedge)[2])] + bond[(step+1):bond_days]
  write.csv(worst_hedge, file.path('Output', 'Worst.csv'))
  