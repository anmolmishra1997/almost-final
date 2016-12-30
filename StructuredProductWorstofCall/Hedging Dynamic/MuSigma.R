setwd("D:/Anmol/Workspace/Anmol Intern/StructuredProductWorstOfCall/Hedging Dynamic")

stock_prices = read.table(file="Input/Organized stock price data.csv",header = TRUE,sep = "," , stringsAsFactors = FALSE)
no_rows = nrow(stock_prices)
assets = (ncol(stock_prices)-1)/2
stock_prices = stock_prices[no_rows:1,]
dates = strptime(stock_prices[,1], "%m/%d/%Y")
dates = format(dates, "%d-%b-%y")

# Reading first set of input
input1 = read.table(file="Input/input for WOC.csv",header = TRUE,sep = "," , stringsAsFactors = FALSE)
valuation_date = strptime(input1[1,2],"%d-%b-%y")
initial_fixing_date <- strptime(input1[1,9],"%d-%b-%y")
final_fixing_date <- strptime(input1[1,3],"%d-%b-%y")
redemption_date <- strptime(input1[1,5],"%d-%b-%y")
start_index = match(format(initial_fixing_date, "%d-%b-%y"),dates)
end_index = match(format(final_fixing_date, "%d-%b-%y"),dates)

y = dates[seq(start_index,end_index,65)]
standard_deviations = y
mu = NULL

for ( i in seq(2,2*assets+1,2))   # seq(2, 3*assets +1, 3)
{
  data1 = stock_prices[,i]
  data2 = stock_prices[,i+1]
  # data3 = stock_prices[,i+2]
  x = NULL
  z = NULL
  returns = diff(log(data1))
  rolls = (data2 - data1)/data1 * 12
  # rolls = (data3 - data2)/data2 * 12
  for (j in seq(start_index,end_index,65) )
  {
    x = append(x, sd(returns[(j-504):j]))
    "In case the data needs to be augmented to the existing data ( bayesian style), following variant can be used" #append(x, sd(returns[start_index-504:j])))
    z = append(z, mean(rolls[(j-504):j]))
  }
  x = x *sqrt(252)
  standard_deviations = cbind(standard_deviations, x)
  mu = cbind(mu,z)
}
corr_names = NULL

prices = stock_prices[,seq(2,2*assets,2)]  
returns = prices[-1,]/prices[-no_rows,] - 1
for (j in seq(start_index,end_index,65) )
{
  file_name<-paste("correlation-",j,".csv", sep = "")
  delete_check <- paste("Correlation/","correlation-",j,".csv", sep = "")
  corr_names = append(corr_names, delete_check)
  if(file.exists(delete_check)) file.remove(delete_check)
  write.table(cor(returns[j:(j-504),]), file.path('Correlation', file_name), col.names = F, row.names = F, sep =',') 
}

output = cbind(standard_deviations,mu,corr_names)
file_name<- "Generated Parameters.csv"
delete_check <- paste("Input/","Generated Parameters.csv", sep = "")
if(file.exists(file_name)) file.remove(file_name)
write.csv(output, file.path('Input', file_name), row.names = F)
