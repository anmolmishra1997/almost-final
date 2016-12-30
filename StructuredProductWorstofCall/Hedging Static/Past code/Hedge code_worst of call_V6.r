#ActualSims: 

#input file

input1 = read.table(file="input for WOC.csv",header = TRUE,sep = "," , stringsAsFactors = FALSE)
product1 <- input1[1,1]
vdate1 <- strptime(input1[1,2],"%d-%b-%y")
fdate1 <- strptime(input1[1,3],"%d-%b-%y")
strike1 <- as.numeric(input1[1,4])
rdate1 <- strptime(input1[1,5],"%d-%b-%y")
vtoday1 <- as.numeric(input1[1,6])
iv1 <-  as.numeric(input1[1,7])/100
rfr1  <- as.numeric(input1[1,8])/100
idate1 <- strptime(input1[1,9],"%d-%b-%y")
noofscenarios1 <- as.numeric(input1[1,10])
ir1 <- as.numeric(input1[1,11])/100
pen1 <- as.numeric(input1[1,12])
pp1 <- as.numeric(input1[1,13])
noofassets1 <- as.numeric(input1[1,14])
noofhedgescenarios1 <- as.numeric(input1[1,15])
builtin <- as.numeric(input1[1,16])*vtoday1
bonddays <-  as.numeric(as.Date(rdate1) - as.Date(vdate1)) +1 
ir <- 0
rate <- 0.11
#bondvalue calculation

intrate <- read.csv(file = "ir.csv")
b <- NROW(intrate)
for (r in b:1)
{
  
  if (bonddays > intrate[r,1])
  {
    ir = intrate[r,2]
    if (ir >0)
    {
      break;
    }
  }
}

bondvalue <- (vtoday1 - builtin)*(1+ir)^(bonddays/365)

# Actual Simulations

days <-  as.numeric(as.Date(fdate1) - as.Date(idate1)) + 1
bdays <- as.numeric(as.Date(rdate1) - as.Date(fdate1))
sim <- noofscenarios1
total <- days*sim
n <- noofassets1
m <- matrix(1:total, nrow = days, ncol = sim)
r <- matrix(1:n, nrow = n, ncol = 1)
actual <- array(0,dim = c(days,sim,n))
actualc <- t(chol(read.csv(file = "actualmat.csv", header = FALSE)))
actualcr <- matrix(1:n, nrow = n, ncol = 1)
actualsig <- read.csv(file = "actualsig.csv", header = FALSE)
sig <- read.csv(file = "sig.csv", header = FALSE)


for (k in 1:n)
{
  filename<-paste("asset-",k,".csv")
  if(file.exists(filename)) file.remove(filename)
}

for(j in 1:sim)
{
  for (i in 1:days)
  {
    for (k in 1:n)
    {
      r[k,1] <- rnorm(1)
    }
    actualcr <- actualc %*% r
    for (k in 1:n)
    {	
      if ( i == 1 )
      {
        actual[1,j,k] <- vtoday1
      }
      else
      {
        old <- actual[i-1,j,k]
        actual[i,j,k] <- old*exp((actualsig[k,2] - (actualsig[k,1]^2)/2)*(1/365)+ actualsig[k,1]*sqrt(1/365)* actualcr[k,1])
      }
    }
  }
}
for (k in 1:n)
{
  filename<-paste("asset-",k,".csv") ;
  write.csv(actual[,,k],file = filename)
}




x <- matrix(0 , nrow = 10, ncol = n)
hedgesim <- noofhedgescenarios1
hedge <- array(0,dim = c(days,hedgesim,n))
ar <- matrix(0 , nrow = days, ncol =1)
#y1 <- matrix(0 , nrow = 10, ncol =2)
min1 <- matrix(0 , nrow = days, ncol = hedgesim)
min2 <- matrix(0 , nrow = days, ncol = hedgesim)
new <- array(0,dim = c(days,hedgesim,n))
temp <- matrix(0, nrow = days, ncol =n)
payoff <- matrix(0 , nrow = days, ncol = hedgesim)
newpayoff <- matrix(0, nrow = days, ncol =n)
newvalue <- array(0,dim = c(days,hedgesim,n))
delta1 <- matrix(0, nrow = days, ncol =n)
n <-noofassets1
sim <- noofscenarios1 
hedgepnl <- matrix(0, nrow = (days-1), ncol = sim)
averagesumhedgepnL <- 0
totalaveragepnl <- 0
hedgec <- t(chol(read.csv(file = "hedgemat.csv", header = FALSE)))
hedgecr <- matrix(1:n, nrow = n, ncol = 1)
hedgesig <- read.csv(file = "hedgesig.csv", header = FALSE)
deltatemp <- array(0,dim = c(days,hedgesim,n))
finaldelta <- array(0,dim = c(days,sim,n))
sumdelta <- matrix(0, nrow = days, ncol =n)
payout <-  matrix(0, nrow = sim, ncol = 1)
output <- matrix(0, nrow = sim, ncol = 7)
sumhedgepnl <- matrix(0 , nrow = sim, ncol =1)
bond <- matrix(0 , nrow = bonddays, ncol =1)
cum_hedge_pnl <-  matrix(0, nrow = (days-1), ncol = sim)
daily_int_hedge_pnl <- matrix(0, nrow = days, ncol = sim)
hedge_pnl_int_rate = 0.075
rollcost <- matrix(0, nrow = (days-1), ncol = sim)
apayoff <- matrix(0, nrow = days, ncol = sim)
client_payoff <- matrix(0 , nrow = days, ncol =1) 
hedgepayoff <-  matrix(0 , nrow = days, ncol =sim)
actualpayoff <- matrix(0 , nrow = days, ncol =sim)
totalactualpnl <- matrix(0, nrow = days, ncol = sim)


#generate hedge simulations

for (k in 1:n)
{
  filename<-paste("hedgesim-",k,".csv")
  if(file.exists(filename)) file.remove(filename)
}

for(j in 1:hedgesim)
{
  for (i in 1:days)
  {
    for (k in 1:n)
    {
      r[k,1] <- rnorm(1)
    }
    hedgecr <- hedgec %*% r
    for (k in 1:n)
    {	
      if ( i == 1 )
      {
        hedge[1,j,k] <- vtoday1
      }
      else
      {
        old <- hedge[i-1,j,k]
        hedge[i,j,k] <- old*exp((hedgesig[k,2] - (hedgesig[k,1]^2)/2)*(1/365)+ hedgesig[k,1]*sqrt(1/365)* hedgecr[k,1])
      }
    }
  }
}
for (k in 1:n)
{
  filename<-paste("hedgesim-",k,".csv") ;
  write.csv(hedge[,,k],file = filename)
}


#daily bond Calculation. this will remain same for all the actual sims. 

for (j in 1:bonddays)
{
  bond[j,1] <- vtoday1/((1+rate)^((bonddays-j)/365))
}

for (a in 1:sim)
{
  spot <-  actual[days,a,which.min(actual[days,a,])]
  print(paste(a))
  minspot_col <- which.min(actual[days,a,])
  output[a,6] <- max(0,1.77*(min((spot/vtoday1-1),1)))  
 # output[a,1] <- vtoday1/((1+rate)^((bdays)/365))
  output[a,1] <- vtoday1
  output[a,5] <- vtoday1
  
  for (j in 1:days)
  {
    #apayoff matrix is for actual sim payoff discouting. calculates payoff based actual sim and discounts it for each day.
    apayoff[j,a] <- max(0,1.77*(min((spot/vtoday1-1),1)))/((1+sig[minspot_col,2])^((days-j)/365))
  }
  for (h in 1:hedgesim)
  {
    for (k in 1:n)
    {
      for (j in 1:days)
      {
        new[j,h,k] <- actual[j,a,k]*hedge[days,h,k]/hedge[j,h,k]
      }
    }
    min1 <- apply(new,c(1,2),min) 
    ar <-  apply(new,c(1,2),which.min) 
    for (j in 1:days)
    {
      payoff[j,h] = (max(0,1.77*(min1[j,h]/vtoday1-1)))/((1+sig[ar[j,h],2])^((days-j)/365))
      hedgepayoff[j,h] = (max(0,1.77*min(min1[j,h]/vtoday1-1,1)))/((1+sig[ar[j,h],2])^((days-j)/365))
    }
    for (k in 1:n)
    {
      newvalue <- new 
      for (j in 1:days)
      {
        deltatemp[j,h,k] <- 0
        newvalue[j,h,k] = new[j,h,k]*1.01
        min2[j,h] = newvalue[j,h,which.min(newvalue[j,h,])]
        newpayoff[j,k] = (max(0,1.77*(min2[j,h]/vtoday1-1)))/((1+sig[ar[j,h],2])^((days-j)/365))
        delta1[j,k] = (newpayoff[j,k] - payoff[j,h])/(actual[j,a,k]*0.01)
        deltatemp[j,h,k] <- deltatemp[j,h,k]+ delta1[j,k]
      } 
    }
  }
  for (k in 1:n)
  {
    {        
      finaldelta[,a,k] <- apply(deltatemp[,,k],1,mean)
    } 
    del <- paste("finaldelta-",k,".csv")
    write.csv(finaldelta[,,k], file = del)
  }
  for (j in 1:(days-1))
  {  
     hedgepnl[j,a] <- 0
     rollcost[j,a] <- 0
     for (k in 1:n)
    {
      hedgepnl[j,a] <- hedgepnl[j,a] + finaldelta[j,a,k]*(1)*(actual[j+1,a,k]-actual[j,a,k])
      rollcost[j,a] <- rollcost[j,a]+ finaldelta[j,a,k]*sig[k,2]*(-1)*(1/365)
    } 
    
     if(j == 1)
     {   
     cum_hedge_pnl[j,a] <- hedgepnl[j,a]
     daily_int_hedge_pnl[j,a] <- cum_hedge_pnl[j,a] * (1/365) * hedge_pnl_int_rate
     }
     else
     {
       cum_hedge_pnl[j,a] <- hedgepnl[j,a] + cum_hedge_pnl[j-1,a]
       daily_int_hedge_pnl[j,a] <- cum_hedge_pnl[j,a] * (1/365) * hedge_pnl_int_rate
     }
    
  }
  daily_int_hedge_pnl[days,a] <- cum_hedge_pnl[days-1,a] * (1/365) * hedge_pnl_int_rate
  actualpayoff[,a] <- apply(hedgepayoff,1,mean)
}
client_payoff[,1]<- apply(actualpayoff,1,mean)
output[,2] <- apply(hedgepnl,2,sum)
output[,3] <- apply(rollcost,2,sum)
output[,4] <- apply(daily_int_hedge_pnl,2,sum)

output[,7] <- output[,1]+output[,2]+output[,3]+output[,4]-output[,5]-output[,6]

colnames(output) <- c("Bond value", "HedgePnL","Rollcost","Cumulative Interest","Principal","Payout","Total PnL")
write.csv(output, file = "Output.csv")
write.csv(hedgepnl, file = "Hedgepnl.csv")


