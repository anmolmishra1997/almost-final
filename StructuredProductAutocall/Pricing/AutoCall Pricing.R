setwd("D:/Anmol/Workspace/Anmol Intern/StructuredProductAutocall/Pricing")
library('Rcpp')
library('RcppArmadillo')
Rcpp::sourceCpp('Rcpp_Functions/Minimum_3D.cpp')

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
total_coupon = array(0, c(scenarios))
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

observation_days = c(0,as.numeric(observation_dates - initial_fixing_date) + 1)
observation_periods = diff(observation_days)
coupon_expired = array ( 0 ,dim = c(scenarios))
expiry_day = array(days, dim= c(scenarios))
i = 1
for (i in 1:length(observation_dates))
{
  coupon_increment = ifelse(minimum_asset[,observation_days[i+1]] > contingent_value & coupon_expired!= 1, contingent_coupon*observation_periods[i]/365,0)
  total_coupon = total_coupon + coupon_increment
  coupon_expired = ifelse(minimum_asset[,observation_days[i+1]] > autocall_level , 1,coupon_expired)
  expiry_day = ifelse( coupon_expired == 1 & expiry_day == days, observation_days[i+1], expiry_day)
}

payout = value_initial*(total_coupon + 1)
discount_days = (expiry_day + 91)
irr = ((1 + total_coupon)^(365/discount_days)) - 1
discount_payout <- (payout/((1+ir)^(discount_days/365)))
output = cbind(total_coupon, irr, payout, discount_payout)
average = mean(discount_payout)

file_name<- "Output.csv"
delete_check <- paste("Output/","Output.csv", sep = "")
if(file.exists(delete_check)) file.remove(delete_check)
write.csv(output, file.path('Output', file_name))