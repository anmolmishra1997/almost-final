setwd("D:/Anmol/Workspace/Anmol Intern/StructuredProductWorstOfCall/Pricing")
library('Rcpp')
library('RcppArmadillo')
Rcpp::sourceCpp('Rcpp_Functions/Minimum_3D.cpp')

input1 = read.table(file="Input/input for WOC.csv",header = TRUE,sep = "," , stringsAsFactors = FALSE)
valuation_date = strptime(input1[1,2],"%d-%b-%y")
initial_fixing_date <- strptime(input1[1,9],"%d-%b-%y")
final_fixing_date <- strptime(input1[1,3],"%d-%b-%y")
redemption_date <- strptime(input1[1,5],"%d-%b-%y")
strike <- as.numeric(input1[1,4])
value_today <- as.numeric(input1[1,6])
iv <-  as.numeric(input1[1,7])/100
rfr  <- as.numeric(input1[1,8])/100
scenarios <- as.numeric(input1[1,10])
ir <- as.numeric(input1[1,11])/100
pen <- as.numeric(input1[1,12])
pp <- as.numeric(input1[1,13])
assets <- as.numeric(input1[1,14])
# Check if days are counted till redemption date or final fixing date
days <-  as.numeric(as.Date(final_fixing_date) - as.Date(initial_fixing_date))

input2 <- read.csv(file = "Input/sig.csv", header = FALSE)
mu <- input2[,2]
sigma <- input2[,1]
corr_coef <- t(chol(read.csv(file = "Input/mat.csv", header = FALSE)))
prices <- array(0, dim = c(scenarios,days,assets))
prices[,1,] = array(value_today, dim = c(assets))
conster = ( mu-(sigma^2)/2 ) * (1.0/365)
conster = t(replicate(scenarios, conster))
var = (0.5 * sigma) * sqrt(1/365)  # Slight doubt over formula. There is a doubt whether 0.5 should be multiplied or not
var = t(replicate(scenarios, var))

for (j in 2:days) #Could transfer this loop to RCpp
{
  ind_rand <- matrix( rnorm(assets*scenarios), assets, scenarios) 
  dep_rand <- t(corr_coef %*% ind_rand)
  varrer <- var * dep_rand
  prices[,j,] = prices[,j-1,]*exp(conster+varrer)
}

# Major time loss in printing. To be optimised, rather ideally to be done over RCpp
for (k in 1:assets)
{
  file_name<-paste("asset-",k,".csv")
  delete_check <- paste("Output/","asset-",k,".csv", sep = "")
  if(file.exists(delete_check)) file.remove(delete_check)
  write.csv(prices[,,k],file.path('Output', file_name))
}

minimum_asset <- Minimum_3D(prices)
payoff = pmax(0,1.77*pmin(minimum_asset[,days]/value_today-1,1)) * value_today

discount_days = as.numeric(as.Date(redemption_date) - as.Date(valuation_date))
interest_rates <- read.csv(file = "Input/ir.csv")
int_rate_index = which.min(interest_rates[,1] < discount_days) - 1
int_rate = interest_rates[int_rate_index,2]
discounted_payoff <- payoff/((1+int_rate)^(discount_days/365))
output = cbind(payoff, discounted_payoff)

file_name<- "Output.csv"
delete_check <- paste("Output/","Output.csv", sep = "")
if(file.exists(delete_check)) file.remove(delete_check)
write.csv(output, file.path('Output', file_name))
#Also add the code for average