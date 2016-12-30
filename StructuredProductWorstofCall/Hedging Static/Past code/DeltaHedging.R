  #Delta Hedging of Worst Off
  #bi<-.04
  #irrelevant stuff
  termstructure<-matrix(c(182.5,365,547.5,730,.095,.105,.1075,.1125),nrow = 4,ncol = 2)
  #irrelevant stuff ends
  start_date<-as.Date("2016-08-08")
  end_date<-as.Date("2019-08-08")
  redemption_date<-as.Date("2020-02-06")
  observation_dates<-c(as.Date("2019-08-08"))
  built_in <- as.double(0.04)
  #irrelevant_stuff
  autocall_barrier<-as.double(0)
  interest_rate_hit<-0
  coupon_barrier<-as.double(0)
  coupon_rate<-0
  payment_lag<-0
  barrier<-as.double(0)
  #irrelevant_stuff_end
  participation <- 1.77
  participation_delta <- 1.9
  AUM <- 100000
  discount_rates <- c(0.1015,0.1088,0.1055)
  discount_rate<-min(discount_rates)
  rate1<-discount_rates[1]
  rate2<-discount_rates[2]
  rate3<-discount_rates[3]  
  output_folder <- "D:\\Output\\"
  output_path <- "D:\\Output"
  simcount<-2500
  Ncols<-1095*simcount
  dt<-1/365
  spot_bounce<-.01
  simcountd<-2500
  
  #Fixed Income Dynamics
  deployment_rate<-0.1125
  bond<-1/((1+deployment_rate)^(as.integer(redemption_date-start_date)/365))
  discount_factors<-as.double(1/((1+discount_rate)^(as.integer(observation_dates-start_date)/365)))
  interest_mtm <- unlist(lapply(0:(redemption_date-start_date),function(daycount){100000*(1-built_in)*(1+deployment_rate)^(daycount/365)-100000*(1-built_in)} ))
  #irrelevant_stuff 
  bond_deficiency<-as.double(unlist(lapply(observation_dates,function(x) (1-((1/((1+deployment_rate)^((as.integer(redemption_date-start_date)-payment_lag)/365)))*((1+max((termstructure[,1]<=as.integer(x-start_date))*termstructure[,2])-interest_rate_hit)^(as.integer(x-start_date)/365)))))))
  #irrelevant_stuff_end
  
  mu<-c(0.0813,0.0872,0.0845)
  sigma<-c(0.3095,0.338,0.4052)
  correlstocks<-c(0.49,0.38,0.53)
  correl<-chol(matrix(c(1,correlstocks[1],correlstocks[2],correlstocks[1],1,correlstocks[3],correlstocks[2],correlstocks[3],1),nrow = 3,ncol = 3))
  
  #hedge forward, vol and correlation
  muh<-c(0.1015,0.1088,0.1055)
  sigmah<-c(0.3254,0.3254,0.3254)
  correlstocksh<-c(0.6,0.5,0.7)
  correlh<-chol(matrix(c(1,correlstocksh[1],correlstocksh[2],correlstocksh[1],1,correlstocksh[3],correlstocksh[2],correlstocksh[3],1),nrow = 3,ncol = 3))
  
  #function(CS1,CS2,CS3,CS4,CSh1,CSh2,CSh3,CSh4,dayno)
  S1 <- matrix(rnorm(Ncols), ncol=Ncols, nrow=1)
  S2 <- matrix(rnorm(Ncols), ncol=Ncols, nrow=1)
  S3 <- matrix(rnorm(Ncols), ncol=Ncols, nrow=1)
  # S4 <- matrix(rnorm(Ncols), ncol=Ncols, nrow=1)
  stocks<-rbind(S1,S2,S3)
  
  correlrand<-apply(stocks,2,function(x) (x%*%correl))
  correlrandh<-apply(stocks,2,function(x) (x%*%correlh))
  
  #generating actual paths
  
  CS1<-t(apply(cbind(rep(1,simcount),matrix(as.double(exp((mu[1]-sigma[1]^2/2)*dt+sigma[1]*sqrt(dt)*correlrand[1,])),nrow=simcount,byrow = TRUE)),1,cumprod))
  CS2<-t(apply(cbind(rep(1,simcount),matrix(as.double(exp((mu[2]-sigma[2]^2/2)*dt+sigma[2]*sqrt(dt)*correlrand[2,])),nrow=simcount,byrow = TRUE)),1,cumprod))
  CS3<-t(apply(cbind(rep(1,simcount),matrix(as.double(exp((mu[3]-sigma[3]^2/2)*dt+sigma[3]*sqrt(dt)*correlrand[3,])),nrow=simcount,byrow = TRUE)),1,cumprod))
  # CS4<-t(apply(cbind(rep(1,simcount),matrix(as.double(exp((mu[4]-sigma[4]^2/2)*dt+sigma[4]*sqrt(dt)*correlrand[4,])),nrow=simcount,byrow = TRUE)),1,cumprod))
  #irrelevant_stuff
  CS4 <- CS3
  #irrelevant_stuff_ends
  observation_dates_tenor<-as.integer(observation_dates-start_date+1)
  #Creating Z's and factors
  otc_mtm <- matrix(nrow=nrow(CS1),ncol=ncol(CS1))
  path_mtm <- matrix(nrow=nrow(CS1),ncol=1)
  for (su in 1:nrow(CS1)){
  path_mtm[su,1] <- 100000*(1-built_in) - 100000*(1+participation*max(0,min(CS1[su,observation_dates-start_date]-1,CS2[su,observation_dates-start_date]-1,CS3[su,observation_dates-start_date]-1,2-1)))
  }
  otc_mtm <- cbind(replicate((observation_dates-start_date),path_mtm,simplify="matrix"))
  trim <- function (x) gsub("^\\s+|\\s+$", "", x)
  
  #irrelevant_stuff
  coupon_acrrual_ER<-as.double(unlist(lapply(1:length(observation_dates), function(x) (coupon_rate*(as.integer(observation_dates[x]-c(start_date,observation_dates)[x])/365))/((1+max((termstructure[,1]<=as.integer(observation_dates[x]-start_date))*termstructure[,2]))^(payment_lag/365)))))
  #irrelevant_stuff_ends
  
  CShz1<-t(apply(cbind(rep(1,simcount),matrix(as.double(exp((muh[1]-sigmah[1]^2/2)*dt+sigmah[1]*sqrt(dt)*correlrandh[1,])),nrow=simcount,byrow = TRUE)),1,cumprod))
  CShz2<-t(apply(cbind(rep(1,simcount),matrix(as.double(exp((muh[2]-sigmah[2]^2/2)*dt+sigmah[2]*sqrt(dt)*correlrandh[2,])),nrow=simcount,byrow = TRUE)),1,cumprod))
  CShz3<-t(apply(cbind(rep(1,simcount),matrix(as.double(exp((muh[3]-sigmah[3]^2/2)*dt+sigmah[3]*sqrt(dt)*correlrandh[3,])),nrow=simcount,byrow = TRUE)),1,cumprod))
  # CShz4<-1/t(apply(cbind(rep(1,simcount),matrix(as.double(exp((muh[4]-sigmah[4]^2/2)*dt+sigmah[4]*sqrt(dt)*correlrandh[4,])),nrow=simcount,byrow = TRUE)),1,cumprod))
  CShz4 <- CShz3
  
  subdir <- paste(mu[1],"_",mu[2],"_",mu[3],"_",sigma[1],"_",sigma[2],"_",sigma[3],"_",correlstocks[1],"_",correlstocks[2],"_",correlstocks[3],sep="")
  output_folders <- paste(output_folder,subdir,"\\",sep="")
  
  dir.create(file.path(output_path,subdir), showWarnings = FALSE)
  ########################################################################################################################################
  library(scales) 
  library(rJava)
  .jinit(classpath = "C:\\Javacodes")
  obj1<-new(J('DeltaCalc43Misc'))
  # obj2<-new(J('DeltaCalc3'))
  
  delta1<-list(NULL)
  delta2<-list(NULL)
  delta3<-list(NULL)
  delta4<-list(NULL)
  delta_test<-lapply(1:3, function(y) lapply(1:50, function(x) {
    gc()
    delta<-obj1$delta(.jarray(CS1[((x-1)*50+1):(x*50),],dispatch = TRUE),.jarray(CS2[((x-1)*50+1):(x*50),],dispatch = TRUE),.jarray(CS3[((x-1)*50+1):(x*50),],dispatch = TRUE),.jarray(CS4[((x-1)*50+1):(x*50),],dispatch = TRUE)
                      ,.jarray(CShz1,dispatch = TRUE),.jarray(CShz2,dispatch = TRUE),.jarray(CShz3,dispatch = TRUE),.jarray(CShz4,dispatch = TRUE),
                      as.integer(y),.jarray(observation_dates_tenor,dispatch = TRUE),.jarray(coupon_acrrual_ER,dispatch = TRUE),
                      coupon_barrier,autocall_barrier,barrier,.jarray(discount_factors,dispatch = TRUE),.jarray(bond_deficiency,dispatch = TRUE),as.double(discount_rate),as.double(rate1),as.double(rate2),as.double(rate3))
    result <- do.call(rbind, lapply(delta, .jevalArray))
    
    
  }))
  
  delta1<-do.call(rbind,delta_test[[1]])
  delta2<-do.call(rbind,delta_test[[2]])
  delta3<-do.call(rbind,delta_test[[3]])
  # delta4<-do.call(rbind,delta_test[[4]])
  delta_hedging_results_2.0<-array(c(delta1,delta2,delta3),dim=c(dim(delta1)[1],dim(delta1)[2],3))
  deltas1 <- matrix(nrow=nrow(delta1),ncol=ncol(delta1))
  deltas2 <- matrix(nrow=nrow(delta2),ncol=ncol(delta2))
  deltas3 <- matrix(nrow=nrow(delta3),ncol=ncol(delta3))
  deltas1 <- delta1
  deltas2 <- delta2
  deltas3 <- delta3
  price_test<- lapply(1:50, function(x) {
    gc()
    price<-obj1$price(.jarray(CS1[((x-1)*50+1):(x*50),],dispatch = TRUE),.jarray(CS2[((x-1)*50+1):(x*50),],dispatch = TRUE),.jarray(CS3[((x-1)*50+1):(x*50),],dispatch = TRUE),.jarray(CS4[((x-1)*50+1):(x*50),],dispatch = TRUE)
                      ,.jarray(CShz1,dispatch = TRUE),.jarray(CShz2,dispatch = TRUE),.jarray(CShz3,dispatch = TRUE),.jarray(CShz4,dispatch = TRUE),.jarray(observation_dates_tenor,dispatch = TRUE),.jarray(coupon_acrrual_ER,dispatch = TRUE),
                      coupon_barrier,autocall_barrier,barrier,.jarray(discount_factors,dispatch = TRUE),.jarray(bond_deficiency,dispatch = TRUE),as.double(rate1),as.double(rate2),as.double(rate3))
    result <- do.call(rbind, lapply(price, .jevalArray))
    
    
  })
  prices<-do.call(rbind,price_test)
  otc_mtms <- matrix(nrow=nrow(prices),ncol=ncol(prices))
  for (h in 1:ncol(prices)){
  otc_mtms[,h] <- AUM*(1-built_in)-AUM*((1/(1+0.1125)^((ncol(prices)-h)/365))+participation*prices[,h]/(1+discount_rate)^((ncol(prices)-h)/365))  
  }
  #otc_mtms <- AUM *(1-built_in) - AUM * (1+participation*prices) 
  #Roll MTM Calculation
  # average_delta1 <- matrix(nrow<-nrow(CS1) , ncol<-1 )
  # average_delta2 <- matrix(nrow<-nrow(CS1) , ncol<-1 )
  # average_delta3 <- matrix(nrow<-nrow(CS1) , ncol<-1 )
  # average_spot1 <- matrix(nrow<-nrow(CS1) , ncol<-1 )
  # average_spot2 <- matrix(nrow<-nrow(CS1) , ncol<-1 )
  # average_spot3 <- matrix(nrow<-nrow(CS1) , ncol<-1 )
  # average_delta1 <- apply(delta1,1,mean)
  # average_delta2 <- apply(delta2,1,mean)
  # average_delta3 <- apply(delta3,1,mean)
  # average_spot1 <- apply(CS1,1,mean)
  # average_spot2 <- apply(CS2,1,mean)
  # average_spot3 <- apply(CS3,1,mean)
  
  roll_mtm <- matrix(nrow=nrow(CS1),ncol=ncol(CS1)-1)
  roll_mtm1 <- matrix(nrow=nrow(CS1),ncol=ncol(CS1)-1)
  roll_mtm2 <- matrix(nrow=nrow(CS1),ncol=ncol(CS1)-1)
  roll_mtm3 <- matrix(nrow=nrow(CS1),ncol=ncol(CS1)-1)
  roll_mtm1 <- (participation_delta*AUM/100*mu[1]/365*100) * (CS1[,1:1095]*deltas1)
  roll_mtm2 <- (participation_delta*AUM/100*mu[2]/365*100) * (CS2[,1:1095]*deltas2)
  roll_mtm3 <- (participation_delta*AUM/100*mu[3]/365*100) * (CS3[,1:1095]*deltas3)
  roll_mtm <- roll_mtm1+roll_mtm2+roll_mtm3
  roll_mtms <- matrix(nrow<-nrow(CS1) , ncol<-ncol(CS1)-1)
  roll_mtms <- t(apply(roll_mtm,1,cumsum))
  #Roll MTM Calculation ends
   simcount<-2500
  ##########################################################################################################################################################  
  
  hedge_pnL<-matrix(sapply(1:simcount, function(simnum) {
    sapply(1:(observation_dates_tenor[length(observation_dates_tenor)]-1), function(s) {
      hedge_pnl_is<-(t(delta_hedging_results_2.0[simnum,s,1])*(CS1[simnum,s+1]-(CS1[simnum,s]*(1+0/365)))+
        t(delta_hedging_results_2.0[simnum,s,2])*(CS2[simnum,s+1]-(CS2[simnum,s]*(1+0/365)))+
        t(delta_hedging_results_2.0[simnum,s,3])*(CS3[simnum,s+1]-(CS3[simnum,s]*(1+0/365))))*participation_delta*AUM
      #t(delta_hedging_results_2.0[simnum,s,4])*(CS4[simnum,s+1]-(CS4[simnum,s]*(1+mu[4]/365)))
    })
  }),nrow =simcount ,byrow = TRUE)
  
  ############################################
  
  #hedge_pnL_autocalled<-matrix(sapply(1:simcount, function(simnum) {
  #  min_at_observation<-unlist(lapply(as.integer(observation_dates-start_date+1),function(x) min(CS1[simnum,x]/CS1[simnum,1],CS2[simnum,x]/CS2[simnum,1],CS3[simnum,x]/CS3[simnum,1])))
  #  autocalled<-unlist(lapply(1:length(observation_dates),function(x) (min_at_observation>=autocall_barrier)[x]*ifelse(x>1,(1-max((min_at_observation>=autocall_barrier)[1:(x-1)])),1)))
  #  hedge_pnL_simnum1<-(ifelse(sum(autocalled*observation_dates_tenor)==0,dim(hedge_pnL)[2],sum(autocalled*observation_dates_tenor)))
  #  hedge_pnL_simnum<-ifelse(hedge_pnL_simnum1>dim(hedge_pnL)[2],dim(hedge_pnL)[2],hedge_pnL_simnum1)
  #  hedge_pnL_adj<-c(unlist(hedge_pnL[simnum,(1:hedge_pnL_simnum)]),rep(0,observation_dates_tenor[length(observation_dates)]-1-hedge_pnL_simnum))
  #}),nrow = simcount, byrow = TRUE)
  
  ############################################
  
  
  pnl_track<-matrix(unlist(lapply(1:simcount,function(simnum){
    #  barrier<-.7
    min_at_observation<-unlist(lapply(as.integer(observation_dates-start_date+1),function(x) min(CS1[simnum,x]/CS1[simnum,1],CS2[simnum,x]/CS2[simnum,1],CS3[simnum,x]/CS3[simnum,1])))
    #   autocalled<-unlist(lapply(1:length(observation_dates),function(x) (min_at_observation>=autocall_barrier)[x]*ifelse(x>1,(1-max((min_at_observation>=autocall_barrier)[1:(x-1)])),1)))
    #    coupon_accrual_count<-unlist(lapply(1:length(observation_dates),function(x) (min_at_observation>=coupon_barrier)[x]*ifelse(x>1,(1-max(autocalled[1:(x-1)])),1)))
    #   coupon_accrued<-unlist(lapply(1:length(observation_dates),function(x) ((coupon_accrual_count[x]*coupon_rate*(as.integer(observation_dates[x]-c(start_date,observation_dates)[x])/365)))))
    #  payout<- (1+sum(coupon_accrued))#-((1-max(autocalled))*((min_at_observation[length(min_at_observation)]<=barrier)*(1-min_at_observation[length(min_at_observation)]))))
    # interest_accrued<-unlist(lapply(1:length(observation_dates),function(x) bi+(1-bi)*(1+max((termstructure[,1]<=as.integer(observation_dates[x]-start_date))*termstructure[,2]))^(as.integer(observation_dates[x]-start_date+payment_lag)/365)))
    #  hedge_pnL_simnum1<-(ifelse(sum(autocalled*observation_dates_tenor)==0,dim(hedge_pnL)[2],sum(autocalled*observation_dates_tenor)))
    # hedge_pnL_simnum<-ifelse(hedge_pnL_simnum1>dim(hedge_pnL)[2],dim(hedge_pnL)[2],hedge_pnL_simnum1)
    
    #  pnl<-(c(ifelse(sum(interest_accrued*autocalled)==0,interest_accrued[length(interest_accrued)],sum(interest_accrued*autocalled)),sum(hedge_pnL[simnum,1:hedge_pnL_simnum]),-payout))
  })),nrow = simcount,byrow = TRUE)
  
  
  #pnl_track_sorted<-cbind(c(1:simcount),pnl_track[,1]+pnl_track[,2]+pnl_track[,3])
  #pnl_track_sorted<-pnl_track_sorted[order(pnl_track_sorted[,2]),]
  
  
  
  ############################################  ############################################
  
  display_daily<-function(simnum) {
    int_hedges_mtm <- 0.075/365*cumsum(t(hedge_pnL[simnum,]))
    dim(int_hedges_mtm) <- c(ncol=observation_dates-start_date,nrow=1)
    cum_int_hedges_mtm <- matrix(nrow=1,ncol=observation_dates-start_date)
    cum_int_hedges_mtm <- cumsum(int_hedges_mtm)
    tester6<- cbind(format(100*CS1[simnum,],digits=2,nsmall=2,scientific=FALSE),format(100*CS2[simnum,],digits=2,nsmall=2,scientific=FALSE),format(100*CS3[simnum,],digits=2,nsmall=2,scientific=FALSE),percent(c(0,((CS1[simnum,2:ncol(CS1)]/CS1[simnum,1:(ncol(CS1)-1)])-1))),
                   percent(c(0,((CS2[simnum,2:ncol(CS2)]/CS2[simnum,1:(ncol(CS2)-1)])-1))),percent(c(0,((CS3[simnum,2:ncol(CS3)]/CS3[simnum,1:(ncol(CS3)-1)])-1)))
                   ,matrix(format(t(participation_delta*1000*delta_hedging_results_2.0[simnum,,]),digits=4,nsmall=2,scientific=FALSE), nrow = dim(delta_hedging_results_2.0)[2], byrow = TRUE  ),
                   matrix(format(t(delta_hedging_results_2.0[simnum,,]),digits=4,nsmall=2,scientific=FALSE), nrow = dim(delta_hedging_results_2.0)[2], byrow = TRUE  ),
                   c(0,format(t(hedge_pnL[simnum,]),digits=2,nsmall=2,scientific=FALSE)),(c(0,format(cumsum(t(hedge_pnL[simnum,])),digits=2,nsmall=2,scientific=FALSE))),c(0,format(cum_int_hedges_mtm,digits=2,nsmall=2,scientific=FALSE)),
                   format(otc_mtms[simnum,],digits=2, nsmall=2,scientific=FALSE), format(interest_mtm,digits=2, nsmall=2,scientific=FALSE),format(-1*roll_mtms[simnum,],digits=2, nsmall=2,scientific=FALSE),format((otc_mtms[simnum,]+interest_mtm-roll_mtms[simnum,]+c(0,cumsum(t(hedge_pnL[simnum,])))+c(0,cum_int_hedges_mtm)),digits=2,nsmall=2,scientific=FALSE)) 
    tester7 <- matrix(nrow=redemption_date-observation_dates,ncol=19)
    
    for(i in 1:(redemption_date-observation_dates)){
    tester7[i,] <- tester6[observation_dates-start_date,]
    tester7[i,17] <- format(interest_mtm[i+observation_dates-start_date],digits=4,nsmall=2,scientific=FALSE)
    tester7[i,19] <- as.double(tester6[observation_dates-start_date,19])+as.double(interest_mtm[i+observation_dates-start_date])
    tester7[i,19] <- format((as.double(tester7[i,19])-as.double(interest_mtm[observation_dates-start_date])),digits=4,nsmall=2,scientific=FALSE)
    }
    sim <- rbind(tester6,tester7)
    
    # colnames(tester)<-c("Stk1","Stk2","Stk3","Stk4","Perf1","Perf2","Perf3","Perf4","Delta1","Delta2","Delta3","Delta4","hedge_PnL","cumulative_PnL")
    colnames(sim)<-c("Stk1","Stk2","Stk3","Perf1","Perf2","Perf3","Qdelta1","Qdelta2","Qdelta3","Delta1","Delta2","Delta3","Daily_Hedge_MTM","Cum_Hedge_MTM","Hedge_Int_MTM","OTC_MTM", "Interest_MTM", "Roll_MTM", "Total_MTM")
    write.csv(sim, file = paste(output_folders,"SimResults_",simnum,"_",mu[1],"_",mu[2],"_",mu[3],"_",sigma[1],"_",sigma[2],"_",sigma[3],"_",correlstocks[1],"_",correlstocks[2],"_",correlstocks[3],".csv"))
    View(sim[278:1277,])
    }
  
  display_daily_worst<-function(simnum) {
    int_hedges_mtm <- 0.075/365*cumsum(t(hedge_pnL[simnum,]))
    dim(int_hedges_mtm) <- c(ncol=observation_dates-start_date,nrow=1)
    cum_int_hedges_mtm <- matrix(nrow=1,ncol=observation_dates-start_date)
    cum_int_hedges_mtm <- cumsum(int_hedges_mtm)
    tester6<- cbind(format(100*CS1[simnum,],digits=2,nsmall=2,scientific=FALSE),format(100*CS2[simnum,],digits=2,nsmall=2,scientific=FALSE),format(100*CS3[simnum,],digits=2,nsmall=2,scientific=FALSE),percent(c(0,((CS1[simnum,2:ncol(CS1)]/CS1[simnum,1:(ncol(CS1)-1)])-1))),
                    percent(c(0,((CS2[simnum,2:ncol(CS2)]/CS2[simnum,1:(ncol(CS2)-1)])-1))),percent(c(0,((CS3[simnum,2:ncol(CS3)]/CS3[simnum,1:(ncol(CS3)-1)])-1)))
                    ,matrix(format(t(participation_delta*1000*delta_hedging_results_2.0[simnum,,]),digits=4,nsmall=2,scientific=FALSE), nrow = dim(delta_hedging_results_2.0)[2], byrow = TRUE  ),
                    matrix(format(t(delta_hedging_results_2.0[simnum,,]),digits=4,nsmall=2,scientific=FALSE), nrow = dim(delta_hedging_results_2.0)[2], byrow = TRUE),
                    c(0,format(t(hedge_pnL[simnum,]),digits=2,nsmall=2,scientific=FALSE)),(c(0,format(cumsum(t(hedge_pnL[simnum,])),digits=2,nsmall=2,scientific=FALSE))),c(0,format(cum_int_hedges_mtm,digits=2,nsmall=2,scientific=FALSE)),
                    format(otc_mtms[simnum,],digits=2, nsmall=2,scientific=FALSE), format(interest_mtm,digits=2, nsmall=2,scientific=FALSE),format(-1*roll_mtms[simnum,],digits=2, nsmall=2,scientific=FALSE),format((otc_mtms[simnum,]+interest_mtm-roll_mtms[simnum,]+c(0,cumsum(t(hedge_pnL[simnum,])))+c(0,cum_int_hedges_mtm)),digits=2,nsmall=2,scientific=FALSE)) 
    tester7 <- matrix(nrow=redemption_date-observation_dates,ncol=19)
    
    for(i in 1:(redemption_date-observation_dates)){
      tester7[i,] <- tester6[observation_dates-start_date,]
      tester7[i,17] <- format(interest_mtm[i+observation_dates-start_date],digits=4,nsmall=2,scientific=FALSE)
      tester7[i,19] <- as.double(tester6[observation_dates-start_date,19])+as.double(interest_mtm[i+observation_dates-start_date])
      tester7[i,19] <- format((as.double(tester7[i,19])-as.double(interest_mtm[observation_dates-start_date])),digits=4,nsmall=2,scientific=FALSE)
    }
    worstpath <- rbind(tester6,tester7)
    
    # colnames(tester)<-c("Stk1","Stk2","Stk3","Stk4","Perf1","Perf2","Perf3","Perf4","Delta1","Delta2","Delta3","Delta4","hedge_PnL","cumulative_PnL")
    colnames(worstpath)<-c("Stk1","Stk2","Stk3","Perf1","Perf2","Perf3","Qdelta1","Qdelta2","Qdelta3","Delta1","Delta2","Delta3","Daily_Hedge_MTM","Cum_Hedge_MTM","Hedge_Int_MTM","OTC_MTM", "Interest_MTM", "Roll_MTM", "Total_MTM")
    write.csv(worstpath, file = paste(output_folders,"SimResults_Worst_",simnum,"_",mu[1],"_",mu[2],"_",mu[3],"_",sigma[1],"_",sigma[2],"_",sigma[3],"_",correlstocks[1],"_",correlstocks[2],"_",correlstocks[3],".csv"))
    View(worstpath[278:1277,])
  }
  
  
  display_daily_best<-function(simnum) {
    int_hedges_mtm <- 0.075/365*cumsum(t(hedge_pnL[simnum,]))
    dim(int_hedges_mtm) <- c(ncol=observation_dates-start_date,nrow=1)
    cum_int_hedges_mtm <- matrix(nrow=1,ncol=observation_dates-start_date)
    cum_int_hedges_mtm <- cumsum(int_hedges_mtm)
    tester6<- cbind(format(100*CS1[simnum,],digits=2,nsmall=2,scientific=FALSE),format(100*CS2[simnum,],digits=2,nsmall=2,scientific=FALSE),format(100*CS3[simnum,],digits=2,nsmall=2,scientific=FALSE),percent(c(0,((CS1[simnum,2:ncol(CS1)]/CS1[simnum,1:(ncol(CS1)-1)])-1))),
                    percent(c(0,((CS2[simnum,2:ncol(CS2)]/CS2[simnum,1:(ncol(CS2)-1)])-1))),percent(c(0,((CS3[simnum,2:ncol(CS3)]/CS3[simnum,1:(ncol(CS3)-1)])-1)))
                    ,matrix(format(t(participation_delta*1000*delta_hedging_results_2.0[simnum,,]),digits=4,nsmall=2,scientific=FALSE), nrow = dim(delta_hedging_results_2.0)[2], byrow = TRUE  ),
                    matrix(format(t(delta_hedging_results_2.0[simnum,,]),digits=4,nsmall=2,scientific=FALSE), nrow = dim(delta_hedging_results_2.0)[2], byrow = TRUE  ),
                    c(0,format(t(hedge_pnL[simnum,]),digits=2,nsmall=2,scientific=FALSE)),(c(0,format(cumsum(t(hedge_pnL[simnum,])),digits=2,nsmall=2,scientific=FALSE))),c(0,format(cum_int_hedges_mtm,digits=2,nsmall=2,scientific=FALSE)),
                    format(otc_mtms[simnum,],digits=2, nsmall=2,scientific=FALSE), format(interest_mtm,digits=2, nsmall=2,scientific=FALSE),format(-1*roll_mtms[simnum,],digits=2, nsmall=2,scientific=FALSE),format((otc_mtms[simnum,]+interest_mtm-roll_mtms[simnum,]+c(0,cumsum(t(hedge_pnL[simnum,])))+c(0,cum_int_hedges_mtm)),digits=2,nsmall=2,scientific=FALSE)) 
    tester7 <- matrix(nrow=redemption_date-observation_dates,ncol=19)
    
    for(i in 1:(redemption_date-observation_dates)){
      tester7[i,] <- tester6[observation_dates-start_date,]
      tester7[i,17] <- format(interest_mtm[i+observation_dates-start_date],digits=4,nsmall=2,scientific=FALSE)
      tester7[i,19] <- as.double(tester6[observation_dates-start_date,19])+as.double(interest_mtm[i+observation_dates-start_date])
      tester7[i,19] <- format((as.double(tester7[i,19])-as.double(interest_mtm[observation_dates-start_date])),digits=4,nsmall=2,scientific=FALSE)
    }
    bestpath <- rbind(tester6,tester7)
    
    # colnames(tester)<-c("Stk1","Stk2","Stk3","Stk4","Perf1","Perf2","Perf3","Perf4","Delta1","Delta2","Delta3","Delta4","hedge_PnL","cumulative_PnL")
    colnames(bestpath)<-c("Stk1","Stk2","Stk3","Perf1","Perf2","Perf3","Qdelta1","Qdelta2","Qdelta3","Delta1","Delta2","Delta3","Daily_Hedge_MTM","Cum_Hedge_MTM","Hedge_Int_MTM","OTC_MTM", "Interest_MTM", "Roll_MTM", "Total_MTM")
    write.csv(bestpath, file = paste(output_folders,"SimResults_Best_",simnum,"_",mu[1],"_",mu[2],"_",mu[3],"_",sigma[1],"_",sigma[2],"_",sigma[3],"_",correlstocks[1],"_",correlstocks[2],"_",correlstocks[3],".csv"))
    View(bestpath[278:1277,])
  }
  
  
  display<-function(simnum) {
    int_hedges_mtm <- 0.075/365*cumsum(t(hedge_pnL[simnum,]))
    y <- sum(int_hedges_mtm)-int_hedges_mtm[observation_dates-start_date]
    b <- observation_dates-start_date
    cum_int_hedges_mtm <- matrix(nrow=1,ncol=observation_dates-start_date)
    for (suk in 1:b){
      cum_int_hedges_mtm[1,suk] <- y
    }
    tester3<- cbind(format(100*CS1[simnum,],digits=2,nsmall=2,scientific=FALSE),format(100*CS2[simnum,],digits=2,nsmall=2,scientific=FALSE),format(100*CS3[simnum,],digits=2,nsmall=2,scientific=FALSE),percent(c(0,((CS1[simnum,2:ncol(CS1)]/CS1[simnum,1:(ncol(CS1)-1)])-1))),
                    percent(c(0,((CS2[simnum,2:ncol(CS2)]/CS2[simnum,1:(ncol(CS2)-1)])-1))),percent(c(0,((CS3[simnum,2:ncol(CS3)]/CS3[simnum,1:(ncol(CS3)-1)])-1)))
                    ,matrix(format(t(participation_delta*1000*delta_hedging_results_2.0[simnum,,]),digits=4,nsmall=2,scientific=FALSE), nrow = dim(delta_hedging_results_2.0)[2], byrow = TRUE  ),
                    c(0,format(t(hedge_pnL[simnum,]),digits=4,nsmall=2,scientific=FALSE)),(c(0,format(cumsum(t(hedge_pnL[simnum,])),digits=2,nsmall=2,scientific=FALSE))),c(0,format(cum_int_hedges_mtm,digits=4,nsmall=2,scientific=FALSE)),
                    format(otc_mtm[simnum,],digits=2, nsmall=2,scientific=FALSE), format(interest_mtm,digits=2, nsmall=2,scientific=FALSE),format(-1*roll_mtms[simnum,],digits=2, nsmall=2,scientific=FALSE),format((otc_mtm[simnum,]+interest_mtm-roll_mtms[simnum,]+c(0,cumsum(t(hedge_pnL[simnum,])))+c(0,cum_int_hedges_mtm)),digits=2,nsmall=2,scientific=FALSE)) 
    tester4 <- matrix(nrow=redemption_date-observation_dates,ncol=19)
    
    for(i in 1:(redemption_date-observation_dates)){
      tester4[i,] <- tester3[observation_dates-start_date,]
      tester4[i,17] <- format(interest_mtm[i+observation_dates-start_date],digits=4,nsmall=2,scientific=FALSE)
      tester4[i,19] <- as.double(tester3[observation_dates-start_date,19])+as.double(interest_mtm[i+observation_dates-start_date])
      tester4[i,19] <- format((as.double(tester4[i,19])-as.double(interest_mtm[observation_dates-start_date])),digits=4,nsmall=2,scientific=FALSE)
    }
    tester5 <- rbind(tester3,tester4)
    
    # colnames(tester)<-c("Stk1","Stk2","Stk3","Stk4","Perf1","Perf2","Perf3","Perf4","Delta1","Delta2","Delta3","Delta4","hedge_PnL","cumulative_PnL")
    colnames(tester5)<-c("Stk1","Stk2","Stk3","Perf1","Perf2","Perf3","Delta1","Delta2","Delta3","Daily_Hedge_MTM","Cum_Hedge_MTM","Hedge_Int_MTM","OTC_MTM", "Interest_MTM", "Roll_MTM", "Total_MTM")
    write.csv(tester5, file = paste(output_folders,"SimResults_",simnum,"_",mu[1],"_",mu[2],"_",mu[3],"_",sigma[1],"_",sigma[2],"_",sigma[3],"_",correlstocks[1],"_",correlstocks[2],"_",correlstocks[3],".csv"))
    View(tester5[278:1277,])
  }
  
  
  
  #########################################################################################
  
  display_stats_desc<-function() {
   
    int_hedges_mtm <- matrix(nrow=nrow(CS1),ncol=observation_dates-start_date)
    y <- matrix(nrow=nrow(CS1),ncol=1)
    for (i in 1:nrow(CS1)) {
      int_hedges_mtm[i,] <- 0.075/365*cumsum(t(hedge_pnL[i,]))
    }
    for (j in 1:nrow(CS1)) {
      y[j,] <- sum(int_hedges_mtm[j,])-int_hedges_mtm[j,observation_dates-start_date]
    }
    b <- observation_dates-start_date
    cum_int_hedges_mtm <- matrix(nrow=nrow(CS1),ncol=observation_dates-start_date)
    for (buk in 1:nrow(CS1)){ 
      for (suk in 1:b){
        cum_int_hedges_mtm[buk,suk] <- y[buk]
      }  
    } 
    
    hedges_mtm <- matrix(nrow=nrow(CS1),ncol=1)
    hedges_mtm <- apply(hedge_pnL,1,sum)
    hedges_mtm <- hedges_mtm - hedge_pnL[,1095]
    taster<- cbind(1:simcount,format(100*CS1[,observation_dates-start_date],digits=2,nsmall=2,scientific=FALSE),format(100*CS2[,observation_dates-start_date],digits=2,nsmall=2,scientific=FALSE),format(100*CS3[,observation_dates-start_date],digits=2,nsmall=2,scientific=FALSE),format(hedges_mtm,digits=2,nsmall=2,scientific=FALSE),format(cum_int_hedges_mtm[,1],digits=2,nsmall=2,scientific=FALSE),format(otc_mtm[,observation_dates-start_date],digits=2, nsmall=2,scientific=FALSE), format(interest_mtm[length(interest_mtm)],digits=2, nsmall=2,scientific=FALSE),format(-1*roll_mtms[,observation_dates-start_date],digits=2, nsmall=2,scientific=FALSE),format((otc_mtm[,observation_dates-start_date]+interest_mtm[length(interest_mtm)]-roll_mtms[,observation_dates-start_date]+hedges_mtm+cum_int_hedges_mtm[,1]),digits=2,nsmall=2,scientific=FALSE)) 
    colnames(taster)<-c("Sim Count", "Stk1","Stk2","Stk3","Hedge_MTM","Hedge_Int_MTM","OTC_MTM", "Interest_MTM", "Roll_MTM", "Total_MTM")
    # tasting <- order(taster$Total_MTM, decreasing=FALSE)
    AllPaths <- taster[order(taster[,"Total_MTM"],decreasing=TRUE),] 
    write.csv(AllPaths, file = paste(output_folders,"Stats_Desc_",mu[1],"_",mu[2],"_",mu[3],"_",sigma[1],"_",sigma[2],"_",sigma[3],"_",correlstocks[1],"_",correlstocks[2],"_",correlstocks[3],".csv"))
    View(AllPaths)
  }
  
  
  display_stats_asc<-function() {
    
    int_hedges_mtm <- matrix(nrow=nrow(CS1),ncol=observation_dates-start_date)
    y <- matrix(nrow=nrow(CS1),ncol=1)
    for (i in 1:nrow(CS1)) {
      int_hedges_mtm[i,] <- 0.075/365*cumsum(t(hedge_pnL[i,]))
    }
    for (j in 1:nrow(CS1)) {
      y[j,] <- sum(int_hedges_mtm[j,])-int_hedges_mtm[j,observation_dates-start_date]
    }
    b <- observation_dates-start_date
    cum_int_hedges_mtm <- matrix(nrow=nrow(CS1),ncol=observation_dates-start_date)
    for (buk in 1:nrow(CS1)){ 
      for (suk in 1:b){
        cum_int_hedges_mtm[buk,suk] <- y[buk]
      }  
    } 
    
    hedges_mtm <- matrix(nrow=nrow(CS1),ncol=1)
    hedges_mtm <- apply(hedge_pnL,1,sum)
    hedges_mtm <- hedges_mtm - hedge_pnL[,1095]
    taster<- cbind(1:simcount,format(100*CS1[,observation_dates-start_date],digits=2,nsmall=2,scientific=FALSE),format(100*CS2[,observation_dates-start_date],digits=2,nsmall=2,scientific=FALSE),format(100*CS3[,observation_dates-start_date],digits=2,nsmall=2,scientific=FALSE),format(hedges_mtm,digits=2,nsmall=2,scientific=FALSE),format(cum_int_hedges_mtm[,1],digits=2,nsmall=2,scientific=FALSE),format(otc_mtm[,observation_dates-start_date],digits=2, nsmall=2,scientific=FALSE), format(interest_mtm[length(interest_mtm)],digits=2, nsmall=2,scientific=FALSE),format(-1*roll_mtms[,observation_dates-start_date],digits=2, nsmall=2,scientific=FALSE),format((otc_mtm[,observation_dates-start_date]+interest_mtm[length(interest_mtm)]-roll_mtms[,observation_dates-start_date]+hedges_mtm+cum_int_hedges_mtm[,1]),digits=2,nsmall=2,scientific=FALSE)) 
    colnames(taster)<-c("Sim Count", "Stk1","Stk2","Stk3","Hedge_MTM","Hedge_Int_MTM","OTC_MTM", "Interest_MTM", "Roll_MTM", "Total_MTM")
    # tasting <- order(taster$Total_MTM, decreasing=FALSE)
    AllPaths <- taster[order(taster[,"Total_MTM"],decreasing=FALSE),] 
    write.csv(AllPaths, file = paste(output_folders,"Stats_Asc_",mu[1],"_",mu[2],"_",mu[3],"_",sigma[1],"_",sigma[2],"_",sigma[3],"_",correlstocks[1],"_",correlstocks[2],"_",correlstocks[3],".csv"))
    View(AllPaths)
  }
  
  display_paths<-function() {
  SimPaths <- rbind(cbind(1:simcount,CS1),cbind(1:simcount,CS2),cbind(1:simcount,CS3))
  write.csv(SimPaths, file = paste(output_folders,"Paths_",mu[1],"_",mu[2],"_",mu[3],"_",sigma[1],"_",sigma[2],"_",sigma[3],"_",correlstocks[1],"_",correlstocks[2],"_",correlstocks[3],".csv"))
  View(SimPaths)
  HedgeSimPaths <- rbind(cbind(1:simcount,CShz1),cbind(1:simcount,CShz2),cbind(1:simcount,CShz3))
  write.csv(HedgeSimPaths, file = paste(output_folders,"Hedge_Paths_",mu[1],"_",mu[2],"_",mu[3],"_",sigma[1],"_",sigma[2],"_",sigma[3],"_",correlstocks[1],"_",correlstocks[2],"_",correlstocks[3],".csv"))
  View(HedgeSimPaths)
  }
  
  
  
  display_summary<-function() {
    #  tester<- cbind(format(100*CS1[simnum,],digits=2,nsmall=2,scientific=FALSE),format(100*CS2[simnum,],digits=2,nsmall=2,scientific=FALSE),format(100*CS3[simnum,],digits=2,nsmall=2,scientific=FALSE),percent(c(0,((CS1[simnum,2:ncol(CS1)]/CS1[simnum,1:(ncol(CS1)-1)])-1))),
    #                 percent(c(0,((CS2[simnum,2:ncol(CS2)]/CS2[simnum,1:(ncol(CS2)-1)])-1))),percent(c(0,((CS3[simnum,2:ncol(CS3)]/CS3[simnum,1:(ncol(CS3)-1)])-1)))
    #                 ,rbind(rep(0,3),matrix(format(t(participation*1000*delta_hedging_results_2.0[simnum,,]),digits=4,nsmall=2,scientific=FALSE), nrow = dim(delta_hedging_results_2.0)[2], byrow = TRUE  )),
    #                 c(0,format(t(hedge_pnL[simnum,]),digits=4,nsmall=2,scientific=FALSE)),(c(0,format(cumsum(t(hedge_pnL[simnum,])),digits=2,nsmall=2,scientific=FALSE))),format(otc_mtm[simnum,],digits=2, nsmall=2,scientific=FALSE), format(interest_mtm,digits=2, nsmall=2,scientific=FALSE),format(-1*roll_mtms[simnum,],digits=2, nsmall=2,scientific=FALSE),format((otc_mtm[simnum,]+interest_mtm-roll_mtms[simnum,]+c(0,cumsum(t(hedge_pnL[simnum,])))),digits=2,nsmall=2,scientific=FALSE)) 
   
   
    int_hedges_mtm <- matrix(nrow=nrow(CS1),ncol=observation_dates-start_date)
    y <- matrix(nrow=nrow(CS1),ncol=1)
    for (i in 1:nrow(CS1)) {
      int_hedges_mtm[i,] <- 0.075/365*cumsum(t(hedge_pnL[i,]))
    }
    for (j in 1:nrow(CS1)) {
      y[j,] <- sum(int_hedges_mtm[j,])-int_hedges_mtm[j,observation_dates-start_date]
    }
    b <- observation_dates-start_date
    cum_int_hedges_mtm <- matrix(nrow=nrow(CS1),ncol=observation_dates-start_date)
    for (buk in 1:nrow(CS1)){ 
      for (suk in 1:b){
        cum_int_hedges_mtm[buk,suk] <- y[buk]
      }  
    }
    total_mtm <- matrix(nrow=nrow(CS1),ncol=1)
    hedges_mtm <- matrix(nrow=nrow(CS1),ncol=1)
    hedges_mtm <- apply(hedge_pnL,1,sum)
    hedges_mtm <- hedges_mtm - hedge_pnL[,observation_dates-start_date]
    total_mtm <- otc_mtm[,observation_dates-start_date]+interest_mtm[length(interest_mtm)]-roll_mtms[,observation_dates-start_date]+hedges_mtm+cum_int_hedges_mtm[,1]
    # colnames(tester)<-c("Stk1","Stk2","Stk3","Stk4","Perf1","Perf2","Perf3","Perf4","Delta1","Delta2","Delta3","Delta4","hedge_PnL","cumulative_PnL")
    #colnames(tester)<-c("Stk1","Stk2","Stk3","Perf1","Perf2","Perf3","Delta1","Delta2","Delta3","Daily_Hedge_MTM","Cum_Hedge_MTM","OTC_MTM", "Interest_MTM", "Roll_MTM", "Total_MTM")
    summary <- cbind(percent(mean(total_mtm)/AUM),percent(max(total_mtm)/AUM),which(total_mtm == max(total_mtm), arr.ind = TRUE),percent(min(total_mtm)/AUM),which(total_mtm == min(total_mtm), arr.ind = TRUE),percent(length(which(total_mtm>0))/simcount),percent(length(which(total_mtm<0))/simcount),percent(mean(total_mtm[total_mtm>=0])/AUM),percent(mean(total_mtm[total_mtm<0])/AUM),percent(length(which(total_mtm>10000))/length(which(total_mtm>=0))),percent(length(which(total_mtm<(-5000)))/length(which(total_mtm<0))))
    colnames(summary)<-c("Average Return","Max Return","Max Return Sim", "Min Return","Min Return Sim","Percentage Wins"," Percentage Losses", "Avg Payoff if Win", "Avg Payoff if Loss","Percent Wins > 10 Percent","Percent Losses < 5 Percent")
    #,"Avg Payoff if Win","Avg Payoff if Loss","Wins>10 Percent", "Losses<5 Percent"
    write.csv(summary, file = paste(output_folders,"Summary_",mu[1],"_",mu[2],"_",mu[3],"_",sigma[1],"_",sigma[2],"_",sigma[3],"_",correlstocks[1],"_",correlstocks[2],"_",correlstocks[3],".csv"))
    View(summary)
    #View(tester)
    # min_at_observation<-unlist(lapply(as.integer(observation_dates-start_date+1),function(x) min(CS1[simnum,x]/CS1[simnum,1],CS2[simnum,x]/CS2[simnum,1],CS3[simnum,x]/CS3[simnum,1])))
    #  autocalled<-unlist(lapply(1:length(observation_dates),function(x) (min_at_observation>=autocall_barrier)[x]*ifelse(x>1,(1-max((min_at_observation>=autocall_barrier)[1:(x-1)])),1)))
    # coupon_accrual_count<-unlist(lapply(1:length(observation_dates),function(x) (min_at_observation>=coupon_barrier)[x]*ifelse(x>1,(1-max(autocalled[1:(x-1)])),1)))
    #  coupon_accrued<-unlist(lapply(1:length(observation_dates),function(x) ((coupon_accrual_count[x]*coupon_rate*(as.integer(observation_dates[x]-c(start_date,observation_dates)[x])/365)))))
    #  data<-rbind(round(observation_dates_tenor,0), round(min_at_observation*100,2),round(autocalled,0),round(coupon_accrued*100,2))
    #  rownames(data)<-c("Obs Dates Tenor", "Min at Obs","Autocalled?","Coupon Accrued")
    # return (data)
    display_stats_asc()
    display_daily_worst(which(total_mtm == min(total_mtm)))
    display_daily_best(which(total_mtm == max(total_mtm)))
    display_paths()
  }
