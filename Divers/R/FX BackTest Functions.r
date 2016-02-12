
#########################Performance Test fucntions#################
{ 
	#fonction des quantiles	
	plotHisto=function(FX_Name,index,histoFX,dates)
	{
		rdtDates<-dates[-1]
		FxSpot=histoFX[,index]
		rdt<- diff(log(FxSpot),na.rm=FALSE)
		I<-!is.na(rdt)
		rdtDates<-rdtDates[I]
		rdt<-rdt[I]
		rdtTS <- timeSeries( rdt,rdtDates)
		spotTS<- timeSeries( FxSpot[I][-1],rdtDates)
		mytitle<-paste("Historique ",FX_Name)
		plot(rdtTS,type="l",col="black",main=mytitle,ylim=c(-0.15,0.15)) 
		par(new=TRUE)
		plot(spotTS,type="l",col="blue",xaxt="n",yaxt="n",xlab="",ylab="")
		axis(4)
		legend("topleft",col=c("black","blue"),lty=1,legend=c("Rdt","spot"))
	
	}
	Q01 <- function(e,i) 
	{
		quantile(e[i], probs = 0.01, na.rm = FALSE,type = 1)
	}
	Q90 <- function(e, i) 
	{
		quantile(e[i], probs = 0.99, na.rm = FALSE,type = 1)
	}
	
	#prix de call et puts non actualisés (fonctions utilisées pour le test des RCM)
	
	call_non_actua_bs =function(S_0,K,sigma,mat,r_d,r_f)
	{
		d1 = (log(S_0/K) + (r_d - r_f + 0.5*sigma*sigma)*mat)/(sigma*sqrt(mat))
		d2 = d1 - sigma*sqrt(mat)
		res = S_0*exp((r_d-r_f)*mat)*pnorm(d1) - K*pnorm(d2)
		return(res)
	}
	put_non_actua_bs =function(S_0,K,sigma,mat,r_d,r_f)
	{
		d1 = (log(S_0/K) + (r_d - r_f + 0.5*sigma*sigma)*mat)/(sigma*sqrt(mat))
		d2 = d1 - sigma*sqrt(mat)
		res=-S_0*exp((r_d-r_f)*mat)*pnorm(-d1) + K*pnorm(-d2)
		return(res)
	}
	
	#Intervalle de confiance des quantiles	
	
	borne_inf = function(n , p,confidence_level)
	{
		return(n*p-qnorm(confidence_level)*sqrt(n*p*(1-p)))
	
	}
	
	borne_sup = function(n , p,confidence_level)
	{
		return(n*p+qnorm(confidence_level)*sqrt(n*p*(1-p)))
	}
	
	getFX_Rate<-function(FX,spotParams,Tenor)
	{
		# FX=substr("FX", 3, 5)
		irFX=paste("IR",FX,"-Interbank",sep='')
		idFX=which(spotParams[,2]==irFX)
		if(Tenor=="1Y")
		{
			ir= as.double(toString( spotParams[idFX+3,10]))
		}else if(Tenor=="6M")
		{
			ir= as.double(toString( spotParams[idFX+2,10]))
		}
		return(ir)
	}
	
	garmanDrift <- function(currency,timeHorizon,spotParams)
	{
			
		fx_rate=getFX_Rate(currency,spotParams,timeHorizon)
		domestic_rate=getFX_Rate("USD",spotParams,timeHorizon)
		return((domestic_rate-fx_rate))
	
	}
	format_percentage <-function(x,digits)
	{
	  if(is.numeric(x))
	  {
	  	return (paste(100*signif(x, digits = digits),"\\%"))
	  }else{
	  	return (NA)
	  }
	}
	format_bp <-function(x,digits)
	{
	  if(is.numeric(x))
	  {
	  	return (paste(100*signif(x, digits = digits)))
	  }else{
	  	return (NA)
	  }
	}
	performanceTest_theo_sim  <-function(historicReturns,theoreticalReturns,T,confidence_level)
	{
			historicReturns<-sort(historicReturns)
			theoreticalReturns<-sort(theoreticalReturns)
			# q99_histo <- quantile(historicReturns,0.99,type=1)
			# q01_histo <- quantile(historicReturns,0.01,type=1)
			# q99_theo <- quantile(theoreticalReturns,0.99,type=1)
			# q01_theo <- quantile(theoreticalReturns,0.01,type=1)
			nb_data <- length(historicReturns)
			nb_scen <- length(theoreticalReturns)
			q99_histo <- historicReturns[nb_data*0.99]
			q01_histo <- historicReturns[nb_data*0.01]
			q99_theo <- theoreticalReturns[nb_scen*0.99]
			q01_theo <- theoreticalReturns[nb_scen*0.01]
			borne_inf_1 <- borne_inf(nb_data,0.01,confidence_level)
			borne_inf_99 <- borne_inf(nb_data,0.99,confidence_level)
			borne_sup_1 <- borne_sup(nb_data,0.01,confidence_level)                       
			borne_sup_99 <- borne_sup(nb_data,0.99,confidence_level)
			q99_histo_max<- historicReturns[borne_sup_99]
			q01_histo_min<- historicReturns[borne_inf_1]
			q99_histo_min<- historicReturns[borne_inf_99]
			q01_histo_max<- historicReturns[borne_sup_1]
			borne_inf_1  <- borne_inf(nb_scen,0.01,confidence_level)
			borne_inf_99 <- borne_inf(nb_scen ,0.99,confidence_level)
			borne_sup_1  <- borne_sup(nb_scen,0.01,confidence_level)                       
			borne_sup_99 <- borne_sup(nb_scen,0.99,confidence_level)
			q99_theo_max <- theoreticalReturns[borne_sup_99]
			q01_theo_min <- theoreticalReturns[borne_inf_1]
			q99_theo_min <- theoreticalReturns[borne_inf_99]
			q01_theo_max <- theoreticalReturns[borne_sup_1]
			test99 <- q99_theo_max > q99_histo_min
			test01 <- q01_theo_min < q01_histo_max
			###### Ecart en pts de vol 
			dt_Y<-T/260
			Delta <- qnorm(0.99)* qnorm(0.99)*dt_Y-2*dt_Y*(q01_histo)
			sigma_1=(- qnorm(0.99)*sqrt(dt_Y)+sqrt(Delta))/dt_Y
			Delta <- qnorm(0.99)* qnorm(0.99)*dt_Y-2*dt_Y*(q01_theo)
			sigma_1_theo=(- qnorm(0.99)*sqrt(dt_Y)+sqrt(Delta))/dt_Y
			delta_vol_01 <- sigma_1_theo-sigma_1
			Delta <- qnorm(0.99)* qnorm(0.99)*dt_Y-2*dt_Y*(q99_histo)
			sigma_2 <- (qnorm(0.99)*sqrt(dt_Y)-sqrt(Delta))/dt_Y
			Delta <- qnorm(0.99)* qnorm(0.99)*dt_Y-2*dt_Y*(q99_theo)
			sigma_2_theo <- (qnorm(0.99)*sqrt(dt_Y)-sqrt(Delta))/dt_Y
			delta_vol_99 <- sigma_2_theo-sigma_2
			
			return(list(q01_histo=format_percentage(q01_histo,3),q01_theo=format_percentage(q01_theo,3),test01=test01,delta_vol_01=format_bp(delta_vol_01,3),sigma_01_hist=format_percentage(sigma_1,3),sigma_01_theo=format_percentage(sigma_1_theo,3),q99_histo=format_percentage(q99_histo,3),q99_theo=format_percentage(q99_theo,3),test99=test99,delta_vol_99=format_bp(delta_vol_99,3),sigma_99_hist=format_percentage(sigma_2,3),sigma_99_theo=format_percentage(sigma_2_theo,3)))			
	}
	
	
	performanceTest_theo_exact<-function(historicReturns,drift,vol,confidence_level,T)
	{
			historicReturns<-sort(historicReturns)
			dt_Y = T/260
			q99_histo <- quantile(historicReturns,0.99,type=1)
			q01_histo <- quantile(historicReturns,0.01,type=1)
			nb_data   <- length(historicReturns)
			borne_inf_1   <- borne_inf(nb_data,0.01,confidence_level)
			borne_inf_99  <- borne_inf(nb_data,0.99,confidence_level)
			borne_sup_1   <- borne_sup(nb_data,0.01,confidence_level)                       
			borne_sup_99  <- borne_sup(nb_data,0.99,confidence_level)
			q99_histo_max <- historicReturns[borne_sup_99]
			q01_histo_min <- historicReturns[borne_inf_1]
			q99_histo_min <- historicReturns[borne_inf_99]
			q01_histo_max <- historicReturns[borne_sup_1]
			q99_theo <-drift + qnorm(0.99)*vol*sqrt(dt_Y)
			q01_theo <-drift + qnorm(0.01)*vol*sqrt(dt_Y)
			test99 <-   q99_theo > q99_histo_min
			test01 <-   q01_theo < q01_histo_max
			###### Ecart en pts de vol 
			Delta 	<- qnorm(0.99)* qnorm(0.99)*dt_Y-2*dt_Y*(q01_histo-drift)
			sigma_1	=(- qnorm(0.99)*sqrt(dt_Y)+sqrt(Delta))/dt_Y
			delta_vol_01 <- vol-sigma_1
			Delta <- qnorm(0.99)* qnorm(0.99)*dt_Y-2*dt_Y*(q99_histo-drift)
			sigma_2 <- (qnorm(0.99)*sqrt(dt_Y)-sqrt(Delta))/dt_Y
			delta_vol_99 <- vol-sigma_2
			return(list(q01_histo=format_percentage(q01_histo,3),q01_theo=format_percentage(q01_theo,3),test01=test01,delta_vol_01=format_bp(delta_vol_01,3),q99_histo=format_percentage(q99_histo,3),q99_theo=format_percentage(q99_theo,3),test99=test99,delta_vol_99=format_bp(delta_vol_99,3)))		
	}
	
	RCM_test_sim  <-function(historicReturns,theoreticalReturns,confidence_level)
	{
	
			St <- exp(historicReturns)
			moy <- mean(St)
			StCentre  <- St/moy -1
			RCM_histo <- sum(StCentre[StCentre > 0])/ length(historicReturns)
			sigma_histo <- sd(StCentre)/sqrt(length(historicReturns))
			
			RCM_histo_min <- RCM_histo - qnorm(confidence_level) * sigma_histo
			RCM_histo_max <- RCM_histo + qnorm(confidence_level) * sigma_histo
			#RCM theoique
			
			St <- exp(theoreticalReturns)
			moy <- mean(St)
			StCentre <- St / moy - 1
			
			RCM_theo      <- sum(StCentre[StCentre > 0]) / length(theoreticalReturns)
			sigma_theo    <- sd(StCentre)/sqrt(length(theoreticalReturns))
			
			RCM_theo_min = RCM_theo - qnorm(confidence_level) * sigma_theo
			RCM_theo_max = RCM_theo + qnorm(confidence_level) * sigma_theo
			test_rcm =  RCM_theo_max > RCM_histo_min
			sigma_histo   = 2*qnorm((RCM_histo+1)*0.5)
			sigma_theo    = 2*qnorm((RCM_theo+1)*0.5)
			delta_vol = sigma_theo - sigma_histo
			return(list(RCM_histo=format_percentage(RCM_histo,3),RCM_theo=format_percentage(RCM_theo,3),test_rcm=test_rcm,sigma_histo=format_percentage(sigma_histo,3),sigma_theo=format_percentage(sigma_theo,3),delta_vol=format_bp(delta_vol,3)))
	}
	RCM_test_exact  <-function(historicReturns,vol,confidence_level=0.995)
	{
		#RCM historique
		St=exp(historicReturns)
		moy <- mean(St)
		StCentreDev <- St/moy -1
		RCM_histo   <- sum(StCentreDev[StCentreDev > 0])/ length(historicReturns)
		sigma_histo <- sd(StCentreDev[StCentreDev > 0])/sqrt(length(historicReturns))
		
		RCM_histo_min<- RCM_histo - qnorm(confidence_level) * sigma_histo
		RCM_histo_max<- RCM_histo + qnorm(confidence_level) * sigma_histo	
		RCM_theo     <- call_non_actua_bs(1,1,vol,1,0,0)
		test_rcm =  RCM_theo > RCM_histo_min	
		############### Delta Vol ###################
		sigma   = 2*qnorm((RCM_histo+1)*0.5)
		delta_vol = vol - sigma
			return(list(RCM_histo=format_percentage(RCM_histo,3),RCM_theo=format_percentage(RCM_theo,3),test_rcm=test_rcm,sigma_histo=format_percentage(sigma,3),sigma_theo=format_percentage(vol,3),delta_vol=format_bp(delta_vol,3)))
	
	}
	
	bt_antithetique <- function(historicReturns,nb_bt,replace,T)
	{
		res <- rep(NA,nb_bt)
		for(i in 1:nb_bt)
		{
			#echantillon de variables de bernoulli 
			antithetique  <- sample(c(-1,1), T, prob=c(0.5,0.5), replace=TRUE)
			#echantillon de trajectoire
			trajectoire <- sample(historicReturns,T,replace=replace)
			trajectoire_antithetique = trajectoire* antithetique
			res[i] <- sum(trajectoire_antithetique)
		}
		return((res-mean(res)))
	}
	
	bootStrapp <- function(historicReturns,nb_bt,replace,T)
	{
		res <- rep(NA,nb_bt)
		for(i in 1:nb_bt)
		{
			trajectoire <- sample(historicReturns,T,replace=replace)
			res[i] <- sum(trajectoire)
		}
		return(res)
	}
		
	SimulateMSWRR=function(NbSimul,spotRating,diffuseRating,ratingPath,stationary,p0Sys,p1Sys,p001,p111,p000,p110,cumulTransitionMatrix,sigma0,cluster1,cluster2,cluster3,dt_,T)
	{  
	
		if(stationary)
		{
			QSys=matrix(c(p0Sys,1-p1Sys,1-p0Sys,p1Sys),2,2)
			pSys_inf<-(1-QSys[1,1])/(2-QSys[1,1]-QSys[2,2])
			p11_inf = p111 * pSys_inf + p110 * (1-pSys_inf)
			p00_inf = p001 * pSys_inf + p000 * (1-pSys_inf)
			p_inf<- (1-p00_inf)/(1-p11_inf+1-p00_inf)	
		}
		emat <- NULL
		rdt <- matrix(NA, T,NbSimul)
		r1<-NULL
		r2<-NULL
		ratings_vect	<- rep(0,NbSimul)
		crisis_vect 	<- rep(0,NbSimul)
		for(k in 1:NbSimul)
		{  
				if(stationary)
				{
							s0Sys	   <- (runif(1) <= pSys_inf) +1	
							s0Specific <- (runif(1) <= p_inf) +1	
							st         <- simulateMarkovStates(p0Sys,p1Sys,p001,p111,p000,p110,s0Sys,s0Specific,T)
							if(diffuseRating)
							{
									rating_stationnary <- SimulateRatingTransition(1,spotRating,cumulTransitionMatrix,T)
									ratings <- matrix(rating_stationnary,T,1)
							}else{
									ratings <- matrix(ratingPath,T,1)
							}
				}else
				{
							st         <- simulateMarkovStates(p0Sys,p1Sys,p001,p111,p000,p110,s0Sys=1,s0Specific=1,T)
							if(diffuseRating)
							{
								ratings <- SimulateRatingTransition(1,spotRating,cumulTransitionMatrix,T)
							}else{
								ratings <- matrix(sample(ratingPath, size=T, replace = TRUE, prob = NULL),T,1)
							}
				}
	
				emat <- NULL
				r1		<- rnorm(T, -0.5*sigma0^2*dt_, sigma0*sqrt(dt_))	
				r2      <- SimulateCrisisReturns(1,cluster1,cluster2,cluster3,T,ratings,dt_)$rdt
				emat <- rbind(emat,r1)
				emat <- rbind(emat,as.vector(r2))
				s <- st$specificStates[1]
				rdt[1,k] <- as.numeric(emat[s,1])			
				if(T>1)
				{
					for (i in 2:T) 
					{
						s <- st$specificStates[i]
						rdt[i,k] <- emat[s,i] 
					}	
				}	
				ratings_vect[k]<-ratings[T,1]
				crisis_vect[k] <-s 
		}	
		return(list(rdt=rdt,rdt_T=apply(rdt, 2, sum),crisis=crisis_vect,ratings=ratings_vect))
	}
	
    SimulateGarmanKohlhagen=function(NbSimul,sigma,dt_,T)
   {
	 rdt<-rnorm(NbSimul, -0.5*sigma^2*dt_*T, sigma*sqrt(dt_*T))		
     return(matrix(rdt,T,NbSimul))
	}

	SimulateMSWR=function(NbSimul,Q,sigma,mu,dt_,T,lambda,U)
	{  
		st<- rep(NA, NbSimul)
		p1_inf<-(1-Q[1,1])/(2-Q[1,1]-Q[2,2])
		if(is.na(p1_inf)) p1_inf<- 0
		st   <-1+rbinom(NbSimul,1, p1_inf)
		emat <- NULL
		rdt <- matrix(NA, T,NbSimul)
		r1<-NULL
		r2<-NULL
		for(k in 1:NbSimul)
		{
				
				st<- rep(NA,T)
				st[1] <-(runif(1) <= p1_inf) +1		
				if(T>1)
				{
					for (i in 2:T) {
						p.cumsum <- cumsum(Q[st[i - 1], ])
						u <- runif(1)	
						st[i] <-3 - sum(u <= p.cumsum) 
					}
				}			
				emat <- NULL
				r1<-rnorm(T, -0.5*sigma[1]^2*dt_, sigma[1]*sqrt(dt_))	
				r2<-rnorm(T, -0.5*sigma[2]^2*dt_, sigma[2]*sqrt(dt_)) +U*rpois(T,lambda*dt_)		
				emat <- rbind(emat,r1)
				emat <- rbind(emat,r2)
				s <- st[1]
				rdt[1,k] <- emat[s,1]			
				if(T>1)
				{
					for (i in 2:T) 
					{
						s <- st[i]
						rdt[i,k] <- emat[s,i] 
					}	
				}	
		}			
		return(rdt)
	}

	SimulateCrisisReturns =function(NbSimul,cluster1,cluster2,cluster3,T,ratings,dt_)
	{
		res<-matrix(0,T,NbSimul)
		for(k in c(1:NbSimul))
		{
			ratings5 = ratings[,k] - 5
			ratings15= ratings[,k] - 15
			ratings_cluster<-rep(0,length(ratings[,k]))
			ratings_cluster[which(ratings5<=0)] = 1
			ratings_cluster[intersect(which(ratings15<0),which(ratings5>0))] = 2
			ratings_cluster[which(ratings15 >= 0)] = 3
			jump_down_1=rpois(T,cluster1$lambda) *rgamma(T,shape=1, rate = cluster1$r_down)
			jump_down_2=rpois(T,cluster2$lambda) *rgamma(T,shape=1, rate = cluster2$r_down)
			jump_down_3=rpois(T,cluster3$lambda) *rgamma(T,shape=1, rate = cluster3$r_down)
			returnCluster1<-rnorm(T, -0.5*cluster1$sigma^2*dt_, cluster1$sigma*sqrt(dt_)) - jump_down_1
			returnCluster2<-rnorm(T, -0.5*cluster2$sigma^2*dt_, cluster2$sigma*sqrt(dt_)) - jump_down_2
			returnCluster3<-rnorm(T, -0.5*cluster3$sigma^2*dt_, cluster3$sigma*sqrt(dt_)) - jump_down_3
			returns       <-rbind(returnCluster1,returnCluster2,returnCluster3)
			res[,k]       =sapply(seq(1,T),function(i) as.numeric(returns[ratings_cluster[i],i]))
		}
		# tmp=apply(res, 2, cumsum)
			# if(T==1)
			# {
			# rdt_T=tmp
			# }else
			# {
			# rdt_T = tmp[T,]
		# }
		return(list(rdt=res,rdt_T=apply(res, 2, sum)))
	}
	
	SimulateRatingTransition = function(NbSimul,spotRating,cumulTransitionMatrix,T)
	{
		ratings = matrix(0,T+1,NbSimul)
		ratings[1,]=spotRating
		for(k in c(1:NbSimul))
		{
			u = runif(T,min=0,max=1)
			for (i in c(1:T))
			{
			j=1
			while( u[i] > cumulTransitionMatrix[ratings[i,k],j])
				j=j+1
			ratings[i+1,k]=j
			}
		}
		return(matrix(ratings[-1,],T,NbSimul))
	}

	simulateMarkovStates     = function(p0Sys,p1Sys,p001,p111,p000,p110,s0Sys,s0Specific,T)
	{
		Q0=matrix(c(p000,1-p110,1-p000,p110),2,2)
		Q1=matrix(c(p001,1-p111,1-p001,p111),2,2)
		Q<-list()
		Q[[1]]<-Q0
		Q[[2]]<-Q1
		QSys=matrix(c(p0Sys,1-p1Sys,1-p0Sys,p1Sys),2,2)
		st	 <- rep(NA,T)
		stSys<- rep(NA,T)
		st[1]    <- s0Sys     
		stSys[1] <- s0Specific
		if(T>1)
		{
			for (i in 2:T) {
				p.cumsum <- cumsum(QSys[stSys[i - 1], ])
				u <- runif(1)	
				stSys[i] <-3 - sum(u <= p.cumsum) 
				p.cumsum <- cumsum(Q[[stSys[i]]][st[i - 1], ])
				u <- runif(1)	
				st[i] <-3 - sum(u <= p.cumsum) 
			}
		}	
		return(list(SysStates=stSys,specificStates=st))
	}

	seqCumSum=function(x,period)
	{
	
			if(period==1){
				return(x)
			}else{
				sapply(seq(1,length(x)-period+1,by=period),function(i) {
					cumsum(x[i:(i+period-1)])[length(x[i:(i+period-1)])]
			})
		}
	}
		
	BackTest<-function(probs,BTestHorizon,calibration,spotDates,histoFX,ratingFX,developedCurrencies,EmergingCurrencies,FX_list)
	{
		res_dev_currencies      <-BackTest_dev_currencies(probs,BTestHorizon,calibration,spotDates,histoFX,developedCurrencies,FX_list)
		res_emerging_currencies <-BackTest_EmergingCurrencies(probs,BTestHorizon,calibration,spotDates,histoFX,ratingFX,EmergingCurrencies,FX_list)
		return(list(BTestResults_Dev_FX=res_dev_currencies$BTestResults,fractiles_Dev=res_dev_currencies$fractiles,recap_DEV=res_dev_currencies$recap,BTestResults_Emerg_FX=res_emerging_currencies$BTestResults,fractiles_Emerg=res_emerging_currencies$fractiles,recap_Emerg=res_emerging_currencies$recap))
	}

	BackTest_dev_currencies<-function(probs,BTestHorizon,calibration,spotDates,histoFX,developedCurrencies,FX_list)
	{
		nb_calib<-ncol(calibration)
		BTest<-c("FX","BackTest")
		probs_<- c(0,probs,1)
		labels<-NULL
		for(i in c(1:(length(probs_)-1)))
		{
			labels<-c(labels,paste("[",probs_[i]*100,"%,",probs_[i+1]*100,"%]",sep=""))
		}
		# BTest<-c(BTest,labels)
		BTest<-NULL
		fract<-c("FX",probs)
		n<-length(developedCurrencies)
		for (i in c(1:length(developedCurrencies)))
		{
			ModelRdt     <- NULL
			ModelSpot    <- NULL
			countMatrix  <- NULL
			ii           <- which(developedCurrencies[i]==FX_list)		   
			FxSpot_BTest <-histoFX[,ii]	
			I			 <-!is.na(FxSpot_BTest)
			FxSpot_BTest <-FxSpot_BTest[I]
			FxSpotDates  <-spotDates[I]
			#remove  NA values
			nb_observations<-0
			for (k in c(1:nb_calib))
			{ 
				I                    <- which((FxSpotDates <= calibration[,k]$date_fin) &(FxSpotDates >= calibration[,k]$date_debut)) 
				FxSpot_BTest_CQ      <-FxSpot_BTest[seq(I[1],I[length(I)],by=BTestHorizon)]
				nb_observations      <- nb_observations + length(FxSpot_BTest_CQ)
				if(I[1]-BTestHorizon<0)
				{
					e <- simpleError(paste("History not Deep Enough to perform BackTest .Spot Dates should at least begin at",FxSpotDates[I[1]]-BTestHorizon))
					stop(e)
				}
				FxSpot_BTest_CQ_prev <-c(FxSpot_BTest[(I[1]-BTestHorizon)],FxSpot_BTest_CQ[-length(FxSpot_BTest_CQ)])	
				BackTestDates        <-FxSpotDates[seq(I[1],I[length(I)],by=BTestHorizon)]
				fx_vols    		=calibration[,k]$param$normal_regime_vol
				sigma     		=as.numeric(fx_vols[which(developedCurrencies[i]==fx_vols[,1]),2])
				ModelRdt_CQ      =SimulateGarmanKohlhagen(1000,sigma,1/260,T=length(FxSpot_BTest_CQ)*BTestHorizon)
				ModelRdt_CQ      =apply(ModelRdt_CQ, 2, seqCumSum,period=BTestHorizon) 	  
				#####Spot Reconstitution
				ModelSpot_CQ     <-FxSpot_BTest_CQ_prev*exp(ModelRdt_CQ)
				fractRdtCQ       <-as.matrix(apply(ModelRdt_CQ,1,quantile,probs=probs,type=1))                
				fractCQ          <-rbind(rep(-Inf, nrow(ModelSpot_CQ)),as.matrix(apply(ModelSpot_CQ,1,quantile,probs=probs,type=1)),rep(Inf, nrow(ModelSpot_CQ)))                    
				# countMatrixCQ    <-apply(sapply(c(1: nrow(ModelSpot_CQ)),function(k){hist(FxSpot_BTest_CQ[k],as.vector(fractCQ[,k]),plot=FALSE)$counts}),1,sum)
				countMatrixCQ    <-apply(sapply(c(1: nrow(ModelSpot_CQ)),function(k){table(cut(FxSpot_BTest_CQ[k],breaks=as.vector(fractCQ[,k]),labels=labels))}),1,sum)		
				countMatrix      <-rbind(countMatrix,countMatrixCQ)
			}
			res<-apply(countMatrix,2,sum)
	
			freq<- res
			probWidth       <-diff(c(0,probs,1))
			threshold99<-as.numeric(qbinom(p=0.9999, size=nb_observations,probWidth,lower.tail = TRUE, log.p = FALSE))
			threshold95<-as.numeric(qbinom(p=0.95, size=nb_observations,probWidth,lower.tail = TRUE, log.p = FALSE))
			result <-rep("Ok",length(freq))
			result[freq>threshold95]<-"Echec 95\\%"
			result[freq>threshold99]<-"Echec 99\\%"
			
			res_i=as.vector(sapply( c(1:length(freq)),function(i){c(freq[i],threshold95[i],threshold99[i],result[i])}))
			BTest_i<-c(substring(developedCurrencies[i],3,5),nb_observations,res_i)
			BTest<-rbind(BTest,BTest_i)
		
			# BTest_i<-c(developedCurrencies[i],rbind(c("freq","threshold99","threshold95","result"),rbind(freq,threshold99,threshold95,result)))
			# fract_i<-c(developedCurrencies[i],apply(fractRdtCQ,1,mean))
			# fract <-rbind(fract,fract_i)	
		}	
		recap<- NULL
		# recap <- c("Recap",BTest[1,-c(1,2)])
		# recap <- rbind(recap,c("Echec 95%",sapply(c(3: ncol(BTest)),function(k){sum(ifelse(BTest[,k]=="Echec 95",1,0))/n})))
		# recap <- rbind(recap,c("Echec 99%",sapply(c(3: ncol(BTest)),function(k){sum(ifelse(BTest[,k]=="Echec 99",1,0))/n})))
		return(list(BTestResults=BTest,fractiles=fract,recap=recap))
	}
	

		####Hypotheses : on diffuse en régime stationnaire de la variable crise et en supposant que le rating spot est constant
	BackTest_EmergingCurrencies<-function(probs,BTestHorizon,calibration,spotDates,histoFX,ratingFX,EmergingCurrencies,FX_list)
	{
			nb_calib<-ncol(calibration)
			# BTest	<-c("FX","BackTest")
			probs_	<- c(0,probs,1)
			labels	<-NULL
			for(i in c(1:(length(probs_)-1)))
			{
				labels<-c(labels,paste("[",probs_[i]*100,"%,",probs_[i+1]*100,"%]",sep=""))
			}
			# BTest<-c(BTest,labels)
			BTest<-NULL
			fract<-c("FX",probs)
			n<-length(EmergingCurrencies)
			for (i in c(1:length(EmergingCurrencies)))
			{
				ModelRdt     <- NULL
				ModelSpot    <- NULL
				countMatrix  <- NULL
				ii           <- which(EmergingCurrencies[i]==FX_list)		   
				FxSpot_BTest <- histoFX[,ii]	
				I			 <- !is.na(FxSpot_BTest)
				FxSpot_BTest <- FxSpot_BTest[I]
				FxSpotDates  <- spotDates[I]
				fx_rating	 <- getRating(ratingFX,substring(EmergingCurrencies[i],3,6))
				ratings_i=data.frame(dates=fx_rating$dates,rating=fx_rating$ratings)
				# krw_TS=timeSeries(krw_rating$ratings,krw_rating$dates)
				FX_i <-  data.frame(dates=FxSpotDates,spot=FxSpot_BTest)
				FX_ratings_i <-merge(FX_i,ratings_i,by="dates")
		
				nb_observations      <-0
				for (k in c(1:nb_calib))
				{ 
				
					I                    <- which((FX_ratings_i$dates <= calibration[,k]$date_fin) &(FX_ratings_i$dates >= calibration[,k]$date_debut)) 
					FxSpot_BTest_CQ      <-FX_ratings_i$spot[seq(I[1],I[length(I)],by=BTestHorizon)]
					FxSpot_BTest_CQ_prev <-c(FX_ratings_i$spot[(I[1]-BTestHorizon)],FxSpot_BTest_CQ[-length(FxSpot_BTest_CQ)])
					FxRating_CQ          <-FX_ratings_i$rating[seq(I[1],I[length(I)],by=BTestHorizon)]
					nb_observations      <- nb_observations + length(FxSpot_BTest_CQ)
				#diffuser les trajectoires en utilisant les différents paramètres de calibration
					ModelRdt_CQ	     =SimulateMSWRR(NbSimul=1000,NULL,FALSE,FxRating_CQ,stationary=TRUE,calibration[,k]$param$tp$p0Sys,calibration[,k]$param$tp$p1Sys,calibration[,k]$param$tp$p001,calibration[,k]$param$tp$p111,calibration[,k]$param$tp$p000,calibration[,k]$param$tp$p110,cumulativeTransitionMatrix,as.numeric(calibration[,k]$param$normal_regime_vol[match(EmergingCurrencies[i],calibration[,k]$param$normal_regime_vol[,1]),2]),calibration[,k]$param$cluster_params$cluster1,calibration[,k]$param$cluster_params$cluster2,calibration[,k]$param$cluster_params$cluster3,dt_=1/260,T=length(FxSpot_BTest_CQ)*BTestHorizon)$rdt	
					ModelRdt_CQ      =apply(ModelRdt_CQ, 2, seqCumSum,period=BTestHorizon) 	  
					#####Spot Reconstitution
					ModelSpot_CQ     <-FxSpot_BTest_CQ_prev*exp(ModelRdt_CQ)
					fractRdtCQ       <-as.matrix(apply(ModelRdt_CQ,1,quantile,probs=probs,type=1))                
					fractCQ          <-rbind(rep(-Inf, nrow(ModelSpot_CQ)),as.matrix(apply(ModelSpot_CQ,1,quantile,probs=probs,type=1)),rep(Inf, nrow(ModelSpot_CQ)))                    
					# countMatrixCQ <-apply(sapply(c(1: nrow(ModelSpot_CQ)),function(k){hist(FxSpot_BTest_CQ[k],as.vector(fractCQ[,k]),plot=FALSE)$counts}),1,sum)
					countMatrixCQ    <-apply(sapply(c(1: nrow(ModelSpot_CQ)),function(k){table(cut(FxSpot_BTest_CQ[k],breaks=as.vector(fractCQ[,k]),labels=labels))}),1,sum)		
					countMatrix		 <-rbind(countMatrix,countMatrixCQ)
				}
					res				<-apply(countMatrix,2,sum)
					freq		    <- res
					probWidth       <-diff(c(0,probs,1))
					threshold99	    <-as.numeric(qbinom(p=0.9999, size=nb_observations,probWidth,lower.tail = TRUE, log.p = FALSE))
					threshold95		<-as.numeric(qbinom(p=0.95, size=nb_observations,probWidth,lower.tail = TRUE, log.p = FALSE))
					result 			<-rep("Ok",length(freq))
					result[freq>threshold95]<-"Echec 95"
					result[freq>threshold99]<-"Echec 99"
					res_i=as.vector(sapply( c(1:length(freq)),function(i){c(freq[i],threshold95[i],threshold99[i],result[i])}))
					BTest_i<-c(substring(EmergingCurrencies[i],3,5),nb_observations,res_i)
					BTest<-rbind(BTest,BTest_i)
		
					# BTest_i<-cbind(EmergingCurrencies[i],c("freq","threshold99","threshold95","result"),rbind(freq,threshold99,threshold95,result))
					# BTest<-rbind(BTest,BTest_i)
					# 
				# fract_i<-c(EmergingCurrencies[i],apply(fractRdtCQ,1,mean))
				# fract <-rbind(fract,fract_i)
		}
		recap <- NULL
		# recap <- c("Recap",BTest[1,-c(1,2)])
		# recap <- rbind(recap,c("Echec 95%",sapply(c(3: ncol(BTest)),function(k){sum(ifelse(BTest[,k]=="Echec 95",1,0))/n})))
		# recap <- rbind(recap,c("Echec 99%",sapply(c(3: ncol(BTest)),function(k){sum(ifelse(BTest[,k]=="Echec 99",1,0))/n})))
		return(list(BTestResults=BTest,fractiles=fract,recap=recap))
	}
In_sample_backtest_FX_Model<-function(results_Calibration)
{

# Performance Tests 
{
   #Emerging Floating currencies 
	# Performance Tests **** Crisis Regime *******
	developed_Currencies <-results_Calibration$Developed_currencies_list
	emerging_currencies  <-results_Calibration$Emerging_currencies_list
	peggCurrencies       <-results_Calibration$pegg_currencies_list
	clusters_perf_test_1Day 	<- NULL
	clusters_perf_test_6M   	<- NULL
	clusters_perf_test_1Y   	<- NULL
	cluster_rcm_test_1Y     	<- NULL
	
	NbSimul=10000
	T=1
	rdtClusters	     		   =results_Calibration$Emerging_currencies$rdtClusters
	cluster_1_5      		   =results_Calibration$Emerging_currencies$cluster_1_5
	cluster_6_14     		   =results_Calibration$Emerging_currencies$cluster_6_14
	cluster_15_20 	 		   =results_Calibration$Emerging_currencies$cluster_15_20
	tp            	 		   =results_Calibration$Emerging_currencies$tp
	vol_regime_normal		   =results_Calibration$Emerging_currencies$vol_regime_normal
	currency_crisis  		   =results_Calibration$Emerging_currencies$currency_crisis
	cluster_1_5_returns_theo   = SimulateCrisisReturns(NbSimul,cluster_1_5,cluster_6_14,cluster_15_20,T,matrix(1,T,NbSimul),dt_=1/260)$rdt_T
	cluster_6_14_returns_theo  = SimulateCrisisReturns(NbSimul,cluster_1_5,cluster_6_14,cluster_15_20,T,matrix(10,T,NbSimul),dt_=1/260)$rdt_T
	cluster_15_20_returns_theo = SimulateCrisisReturns(NbSimul,cluster_1_5,cluster_6_14,cluster_15_20,T,matrix(20,T,NbSimul),dt_=1/260)$rdt_T
    clusters_perf_test_1Day    = rbind(clusters_perf_test_1Day,c("[1-5] ",performanceTest_theo_sim(rdtClusters$rdt_cluster_1_5,cluster_1_5_returns_theo,T,confidence_level=0.975)))
	clusters_perf_test_1Day    = rbind(clusters_perf_test_1Day, c("[6-14] ",performanceTest_theo_sim(rdtClusters$rdt_cluster_6_14,cluster_6_14_returns_theo,T,confidence_level=0.975)))
	clusters_perf_test_1Day    = rbind(clusters_perf_test_1Day, c("[15-20] ",performanceTest_theo_sim(rdtClusters$rdt_cluster_15_20,cluster_15_20_returns_theo,T,confidence_level=0.975)))
	clusters_perf_test_1Day
	T=120
	cluster_1_5_returns_theo     =  SimulateCrisisReturns(NbSimul,cluster_1_5,cluster_6_14,cluster_15_20,T,matrix(1,T,NbSimul),dt_=1/260)$rdt_T
    historicReturns_6M		     <-  bootStrapp(rdtClusters$rdt_cluster_1_5,10000,replace=FALSE,T)	   	   
	clusters_perf_test_6M	     =  rbind(clusters_perf_test_6M,c("[1-5] ", performanceTest_theo_sim(historicReturns_6M,cluster_1_5_returns_theo,T,confidence_level=0.975)))
	cluster_6_14_returns_theo    =  SimulateCrisisReturns(NbSimul,cluster_1_5,cluster_6_14,cluster_15_20,T,matrix(10,T,NbSimul),dt_=1/260)$rdt_T
	historicReturns_6M           <- bootStrapp(rdtClusters$rdt_cluster_6_14,10000,replace=FALSE,T)	   	   
	clusters_perf_test_6M        =  rbind(clusters_perf_test_6M,c("[6-14] ",performanceTest_theo_sim(historicReturns_6M,cluster_6_14_returns_theo,T,confidence_level=0.975)))
	cluster_15_20_returns_theo   =  SimulateCrisisReturns(NbSimul,cluster_1_5,cluster_6_14,cluster_15_20,T,matrix(20,T,NbSimul),dt_=1/260)$rdt_T
	historicReturns_6M           <- bootStrapp(rdtClusters$rdt_cluster_15_20,10000,replace=FALSE,T)	   	   
	clusters_perf_test_6M        =  rbind(clusters_perf_test_6M,c("[15-20] ",performanceTest_theo_sim(historicReturns_6M,cluster_15_20_returns_theo,T,confidence_level=0.975)))
	clusters_perf_test_6M
	T=260
	cluster_1_5_returns_theo      =  SimulateCrisisReturns(NbSimul,cluster_1_5,cluster_6_14,cluster_15_20,T,matrix(1,T,NbSimul),dt_=1/260)$rdt_T
 	historicReturns_1Y           <- bootStrapp(rdtClusters$rdt_cluster_1_5,10000,replace=FALSE,T)	   	   
	clusters_perf_test_1Y         =  rbind(clusters_perf_test_1Y,c("[1-5] ",performanceTest_theo_sim(historicReturns_1Y,cluster_1_5_returns_theo,T,confidence_level=0.975)))
    cluster_rcm_test_1Y           =  rbind(cluster_rcm_test_1Y,c("[1-5] ",RCM_test_sim(historicReturns_1Y,cluster_1_5_returns_theo,confidence_level=0.995)))
	cluster_6_14_returns_theo     =   SimulateCrisisReturns(NbSimul,cluster_1_5,cluster_6_14,cluster_15_20,T,matrix(10,T,NbSimul),dt_=1/260)$rdt_T	
	historicReturns_1Y	          <-  bootStrapp(rdtClusters$rdt_cluster_6_14,10000,replace=FALSE,T)	   	   
	clusters_perf_test_1Y	      = rbind(clusters_perf_test_1Y,c("[6-14] ",performanceTest_theo_sim(historicReturns_1Y,cluster_6_14_returns_theo,T,confidence_level=0.975)))
	cluster_rcm_test_1Y			  = rbind(cluster_rcm_test_1Y,c("[6-14] ",RCM_test_sim(historicReturns_1Y,cluster_6_14_returns_theo,confidence_level=0.995)))
	cluster_15_20_returns_theo	  =   SimulateCrisisReturns(NbSimul,cluster_1_5,cluster_6_14,cluster_15_20,T,matrix(20,T,NbSimul),dt_=1/260)$rdt_T
	historicReturns_1Y	          <-  bootStrapp(rdtClusters$rdt_cluster_15_20,10000,replace=FALSE,T)	
	clusters_perf_test_1Y 		  =   rbind(clusters_perf_test_1Y,c("[15-20] ",performanceTest_theo_sim(historicReturns_1Y,cluster_15_20_returns_theo,T,confidence_level=0.975)))
	cluster_rcm_test_1Y	          =   rbind(cluster_rcm_test_1Y,c("[15-20] ", RCM_test_sim(historicReturns_1Y,cluster_15_20_returns_theo,confidence_level=0.995)))
    clusters_perf_test_1Y
	
   #Normal regime and stationary regime performance tests 
	normal_regime_perf_test_1Day        <- NULL
	normal_regime_perf_test_6M          <- NULL
	normal_regime_perf_test_1Y          <- NULL
    normal_regime_perf_test_RCM         <- NULL
	stationnary_regime_perf_test_1Day   <- NULL
	stationnary_regime_perf_test_6M     <- NULL
	stationnary_regime_perf_test_1Y     <- NULL
	stationnary_regime_perf_test_RCM_1Y <- NULL
	# stationnary_regime_perf_test_1Day_1_5  <-NULL
    # stationnary_regime_perf_test_1Day_6_14 <-NULL
	# stationnary_regime_perf_test_1_5_6M  <-NULL
    # stationnary_regime_perf_test_6_14_6M <-NULL
	# stationnary_regime_perf_test_1_5_1Y  <-NULL
    # stationnary_regime_perf_test_6_14_1Y <-NULL
	max_draw_down_1_year  <- NULL
	max_draw_down_6M     <- NULL
    max_draw_up_1_year  <- NULL
	max_draw_up_6M     <- NULL
	for (k in c(1:length(emerging_currencies)))
	{
	
	    # Performance Tests **** Normal Regime *******
		historicReturns        =currency_crisis[[k]]$returns[which(currency_crisis[[k]]$states==0)]  
    	historicReturns_6M    <- bt_antithetique(historicReturns,10000,replace=FALSE,T=120)	   	   
        historicReturns_1Y    <- bt_antithetique(historicReturns,10000,replace=FALSE,T=260)

	    normal_regime_perf_test_1Day    = rbind(normal_regime_perf_test_1Day,c(emerging_currencies[k],performanceTest_theo_exact(historicReturns,0,vol_regime_normal[k,2],confidence_level=0.975,1)))
     	normal_regime_perf_test_6M      = rbind(normal_regime_perf_test_6M,  c(emerging_currencies[k], performanceTest_theo_exact(historicReturns_6M,0,vol_regime_normal[k,2],confidence_level=0.975,120)))
		normal_regime_perf_test_1Y      = rbind(normal_regime_perf_test_1Y,  c(emerging_currencies[k],performanceTest_theo_exact(historicReturns_1Y,0,vol_regime_normal[k,2],confidence_level=0.975,260)))
	    normal_regime_perf_test_RCM     = rbind(normal_regime_perf_test_RCM, c(emerging_currencies[k],RCM_test_exact(historicReturns_1Y,vol_regime_normal[k,2],0.995) ))
    	# Performance Tests **** Stationnary Regime *******
		historicReturns		  <- currency_crisis[[k]]$returns
		historicReturns_6M    <- bt_antithetique(historicReturns,10000,replace=FALSE,T=120)	   	   
        historicReturns_1Y    <- bt_antithetique(historicReturns,10000,replace=FALSE,T=260)
		max_draw_down_1_year <-  rbind(max_draw_down_1_year,c(emerging_currencies[k],format_percentage(max_drop_down_returns(historicReturns,260),3)))
		max_draw_down_6M     <-  rbind(max_draw_down_6M,c(emerging_currencies[k],format_percentage(max_drop_down_returns(historicReturns,120),3)))
		max_draw_up_1_year   <-  rbind(max_draw_down_1_year,c(emerging_currencies[k],format_percentage(max_drop_up_returns(historicReturns,260),3)))
		max_draw_up_6M       <-  rbind(max_draw_down_6M,c(emerging_currencies[k],format_percentage(max_drop_up_returns(historicReturns,120),3)))

		if(length(which(currency_crisis[[k]]$states==1)))
		{
			    ratingPath   =currency_crisis[[k]]$rating[which(currency_crisis[[k]]$states==1)]
		}else
		{
				ratingPath   =currency_crisis[[k]]$rating
		}
		#Staionnary 1 day returns 
        T=1
	    theoretical_returns	     =SimulateMSWRR(NbSimul=10000,20,FALSE,20,stationary=TRUE,tp$p0Sys,tp$p1Sys,tp$p_0_0_1,tp$p_1_1_1,tp$p_0_0_0,tp$p_1_1_0,NULL,as.numeric(vol_regime_normal[k,2]),cluster_1_5,cluster_6_14,cluster_15_20,dt_=1/260,T)$rdt	  
    	theoretical_returns_1	 =SimulateMSWRR(NbSimul=10000,1 ,FALSE,1,stationary=TRUE,tp$p0Sys,tp$p1Sys,tp$p_0_0_1,tp$p_1_1_1,tp$p_0_0_0,tp$p_1_1_0,NULL,as.numeric(vol_regime_normal[k,2]),cluster_1_5,cluster_6_14,cluster_15_20,dt_=1/260,T)$rdt	  
	    theoretical_returns_2	 =SimulateMSWRR(NbSimul=10000,10,FALSE,10,stationary=TRUE,tp$p0Sys,tp$p1Sys,tp$p_0_0_1,tp$p_1_1_1,tp$p_0_0_0,tp$p_1_1_0,NULL,as.numeric(vol_regime_normal[k,2]),cluster_1_5,cluster_6_14,cluster_15_20,dt_=1/260,T)$rdt	      
	    stationnary_regime_perf_test_1Day      <-rbind(stationnary_regime_perf_test_1Day,c(emerging_currencies[k],performanceTest_theo_sim(historicReturns,theoretical_returns,T,confidence_level=0.975)))
     	# stationnary_regime_perf_test_1Day_1_5  <-rbind(stationnary_regime_perf_test_1Day_1_5,c(emerging_currencies[k],performanceTest_theo_sim(historicReturns,theoretical_returns_1,T,confidence_level=0.975)))
	    # stationnary_regime_perf_test_1Day_6_14 <-rbind(stationnary_regime_perf_test_1Day_6_14,c(emerging_currencies[k],performanceTest_theo_sim(historicReturns,theoretical_returns_2,T,confidence_level=0.975)))
        
	    # 6M performance test
		T=120
		theoretical_returns	   	   = SimulateMSWRR(NbSimul=10000,20,FALSE,20,stationary=TRUE,tp$p0Sys,tp$p1Sys,tp$p_0_0_1,tp$p_1_1_1,tp$p_0_0_0,tp$p_1_1_0,NULL,as.numeric(vol_regime_normal[k,2]),cluster_1_5,cluster_6_14,cluster_15_20,dt_=1/260,T)$rdt_T  
		theoretical_returns_1	   = SimulateMSWRR(NbSimul=10000,1,FALSE,1,stationary=TRUE,tp$p0Sys,tp$p1Sys,tp$p_0_0_1,tp$p_1_1_1,tp$p_0_0_0,tp$p_1_1_0,NULL,as.numeric(vol_regime_normal[k,2]),cluster_1_5,cluster_6_14,cluster_15_20,dt_=1/260,T)$rdt_T  
		theoretical_returns_2	   = SimulateMSWRR(NbSimul=10000,10,FALSE,10,stationary=TRUE,tp$p0Sys,tp$p1Sys,tp$p_0_0_1,tp$p_1_1_1,tp$p_0_0_0,tp$p_1_1_0,NULL,as.numeric(vol_regime_normal[k,2]),cluster_1_5,cluster_6_14,cluster_15_20,dt_=1/260,T)$rdt_T  
		stationnary_regime_perf_test_6M       = rbind(stationnary_regime_perf_test_6M,c(emerging_currencies[k],performanceTest_theo_sim(historicReturns_6M,theoretical_returns-mean(theoretical_returns),T,confidence_level=0.975)))
        # stationnary_regime_perf_test_1_5_6M  <- rbind(stationnary_regime_perf_test_1_5_6M,c(emerging_currencies[k],performanceTest_theo_sim(historicReturns,theoretical_returns_1-mean(theoretical_returns_1),T,confidence_level=0.975)))
	    # stationnary_regime_perf_test_6_14_6M <- rbind(stationnary_regime_perf_test_6_14_6M,c(emerging_currencies[k],performanceTest_theo_sim(historicReturns,theoretical_returns_2-mean(theoretical_returns_2),T,confidence_level=0.975)))
   
		T=260 
		theoretical_returns	       = SimulateMSWRR(NbSimul=10000,20,FALSE,20,stationary=TRUE,tp$p0Sys,tp$p1Sys,tp$p_0_0_1,tp$p_1_1_1,tp$p_0_0_0,tp$p_1_1_0,NULL,as.numeric(vol_regime_normal[k,2]),cluster_1_5,cluster_6_14,cluster_15_20,dt_=1/260,T)$rdt_T  
		theoretical_returns_1	   = SimulateMSWRR(NbSimul=10000,1,FALSE,1,stationary=TRUE,tp$p0Sys,tp$p1Sys,tp$p_0_0_1,tp$p_1_1_1,tp$p_0_0_0,tp$p_1_1_0,NULL,as.numeric(vol_regime_normal[k,2]),cluster_1_5,cluster_6_14,cluster_15_20,dt_=1/260,T)$rdt_T  
		theoretical_returns_2	   = SimulateMSWRR(NbSimul=10000,10,FALSE,10,stationary=TRUE,tp$p0Sys,tp$p1Sys,tp$p_0_0_1,tp$p_1_1_1,tp$p_0_0_0,tp$p_1_1_0,NULL,as.numeric(vol_regime_normal[k,2]),cluster_1_5,cluster_6_14,cluster_15_20,dt_=1/260,T)$rdt_T  
	    stationnary_regime_perf_test_1Y       = rbind(stationnary_regime_perf_test_1Y,c(emerging_currencies[k],performanceTest_theo_sim(historicReturns_1Y,theoretical_returns-mean(theoretical_returns),T,confidence_level=0.975)))   
  	    # stationnary_regime_perf_test_1_5_1Y  <-rbind(stationnary_regime_perf_test_1_5_1Y,c(emerging_currencies[k],performanceTest_theo_sim(historicReturns,theoretical_returns_1-mean(theoretical_returns_1),T,confidence_level=0.975)))
	    # stationnary_regime_perf_test_6_14_1Y <-rbind(stationnary_regime_perf_test_6_14_1Y,c(emerging_currencies[k],performanceTest_theo_sim(historicReturns,theoretical_returns_2-mean(theoretical_returns_2),T,confidence_level=0.975)))
	    stationnary_regime_perf_test_RCM_1Y   = rbind(stationnary_regime_perf_test_RCM_1Y,c(emerging_currencies[k],RCM_test_sim(historicReturns_1Y,theoretical_returns-mean(theoretical_returns),0.995)))
	}
	#Peg currencies
     # stationnary_regime_perf_test_1Day_comparaison <- cbind(stationnary_regime_perf_test_1Day_1_5[-25,c(1,2,3)],stationnary_regime_perf_test_1Day_6_14[-25,c(3)],stationnary_regime_perf_test_1Day[-25,c(3)],stationnary_regime_perf_test_1Day_1_5[-25,c(8,9)],stationnary_regime_perf_test_1Day_6_14[-25,c(9)],stationnary_regime_perf_test_1Day[-25,c(9)])
	 # stationnary_regime_perf_test_6M_comparaison <- cbind(stationnary_regime_perf_test_6M[-c(25:32),c(1,2)],stationnary_regime_perf_test_1_5_6M[-c(25:32),c(3)],stationnary_regime_perf_test_6_14_6M[-c(25:32),c(3)],stationnary_regime_perf_test_6M[-c(25:32),c(3,8)],stationnary_regime_perf_test_1_5_6M[-c(25:32),c(9)],stationnary_regime_perf_test_6_14_6M[-c(25:32),c(9)],stationnary_regime_perf_test_6M[-c(25:32),c(9)])
     # stationnary_regime_perf_test_1Y_comparaison <- cbind(stationnary_regime_perf_test_1Y[-c(25:32),c(1,2)],stationnary_regime_perf_test_1_5_1Y[-c(25:32),c(3)],stationnary_regime_perf_test_6_14_1Y[-c(25:32),c(3)],stationnary_regime_perf_test_1Y[-c(25:32),c(3,8)],stationnary_regime_perf_test_1_5_1Y[-c(25:32),c(9)],stationnary_regime_perf_test_6_14_1Y[-c(25:32),c(9)],stationnary_regime_perf_test_1Y[-c(25:32),c(9)])
	 # stationnary_regime_perf_test_6M_comparaison   <- cbind(stationnary_regime_perf_test_6M[-c(25:32),c(1)],max_draw_down_6M[-c(25),c(2)],stationnary_regime_perf_test_1_5_6M[-c(25:32),c(3)],stationnary_regime_perf_test_6_14_6M[-c(25:32),c(3)],stationnary_regime_perf_test_6M[-c(25:32),c(3)],max_draw_up_6M[-c(25),c(2)],stationnary_regime_perf_test_1_5_6M[-c(25:32),c(9)],stationnary_regime_perf_test_6_14_6M[-c(25:32),c(9)],stationnary_regime_perf_test_6M[-c(25:32),c(9)])
     # stationnary_regime_perf_test_1Y_comparaison   <- cbind(stationnary_regime_perf_test_1Y[-c(25:32),c(1)],max_draw_down_1_year[-c(25),c(2)],stationnary_regime_perf_test_1_5_1Y[-c(25:32),c(3)],stationnary_regime_perf_test_6_14_1Y[-c(25:32),c(3)],stationnary_regime_perf_test_1Y[-c(25:32),c(3)],max_draw_up_1_year[-c(25),c(2)],stationnary_regime_perf_test_1_5_1Y[-c(25:32),c(9)],stationnary_regime_perf_test_6_14_1Y[-c(25:32),c(9)],stationnary_regime_perf_test_1Y[-c(25:32),c(9)])

	for (k in c(1:length(peggCurrencies))){
		ratingPath=results_Calibration$Pegg_Currencies$peggCurrencies_rdt_rating[[k]]$rating
		historicReturns=results_Calibration$Pegg_Currencies$peggCurrencies_rdt_rating[[k]]$rdt
        historicReturns_6M<-bt_antithetique(historicReturns,10000,replace=FALSE,T=120)	   	   
        historicReturns_1Y<-bt_antithetique(historicReturns,10000,replace=FALSE,T=260)
		normal_regime_perf_test_1Day    = rbind(normal_regime_perf_test_1Day,c(peggCurrencies[k],performanceTest_theo_exact(historicReturns,0,as.numeric(results_Calibration$Pegg_Currencies$peggCurrencies_vol[k]),confidence_level=0.975,1)))
     	normal_regime_perf_test_6M      = rbind(normal_regime_perf_test_6M,  c(peggCurrencies[k], performanceTest_theo_exact(historicReturns_6M,0,as.numeric(results_Calibration$Pegg_Currencies$peggCurrencies_vol[k]),confidence_level=0.975,120)))
		normal_regime_perf_test_1Y      = rbind(normal_regime_perf_test_1Y,  c(peggCurrencies[k],performanceTest_theo_exact(historicReturns_1Y,0,as.numeric(results_Calibration$Pegg_Currencies$peggCurrencies_vol[k]),confidence_level=0.975,260)))
	    normal_regime_perf_test_RCM     = rbind(normal_regime_perf_test_RCM, c(peggCurrencies[k],RCM_test_exact(historicReturns_1Y,as.numeric(results_Calibration$Pegg_Currencies$peggCurrencies_vol[k]),0.995) ))
		#############Staionnary 1 day returns 
        T=1
		theoreticalReturns<- SimulateMSWRR(NbSimul=10000,20,FALSE,20,stationary=TRUE,tp$p0Sys,tp$p1Sys,tp$p_0_0_1,tp$p_1_1_1,tp$p_0_0_0,tp$p_1_1_0,NULL,as.numeric(results_Calibration$Pegg_Currencies$peggCurrencies_vol[k]),cluster_1_5,cluster_6_14,cluster_15_20,dt_=1/260,T)$rdt	  
		stationnary_regime_perf_test_1Day = rbind(stationnary_regime_perf_test_1Day,c(peggCurrencies[k],performanceTest_theo_sim(historicReturns,theoreticalReturns,T,confidence_level=0.975)))

		################## 6M performance test
		T=120
 		theoretical_returns	 =SimulateMSWRR(NbSimul=10000,20,FALSE,20,stationary=TRUE,tp$p0Sys,tp$p1Sys,tp$p_0_0_1,tp$p_1_1_1,tp$p_0_0_0,tp$p_1_1_0,NULL,as.numeric(results_Calibration$Pegg_Currencies$peggCurrencies_vol[k]),cluster_1_5,cluster_6_14,cluster_15_20,dt_=1/260,T)$rdt_T   	   
		stationnary_regime_perf_test_6M     = rbind( stationnary_regime_perf_test_6M,c(peggCurrencies[k],performanceTest_theo_sim(historicReturns_6M,theoretical_returns-mean(theoretical_returns),T=120,confidence_level=0.975)))     
	    # ################## 1 Year performance test	
	    # #############Staionnary 1Y returns
		T=260 		
	    theoretical_returns	 =SimulateMSWRR(NbSimul=10000,20,FALSE,20,stationary=TRUE,tp$p0Sys,tp$p1Sys,tp$p_0_0_1,tp$p_1_1_1,tp$p_0_0_0,tp$p_1_1_0,NULL,as.numeric(results_Calibration$Pegg_Currencies$peggCurrencies_vol[k]),cluster_1_5,cluster_6_14,cluster_15_20,dt_=1/260,T)$rdt_T   	   
		stationnary_regime_perf_test_1Y	         = rbind(stationnary_regime_perf_test_1Y,c(peggCurrencies[k],performanceTest_theo_sim(historicReturns_1Y,theoretical_returns-mean(theoretical_returns),T,confidence_level=0.975) ))  
	    stationnary_regime_perf_test_RCM_1Y      = rbind(stationnary_regime_perf_test_RCM_1Y,c(peggCurrencies[k],RCM_test_sim(historicReturns_1Y,theoretical_returns-mean(theoretical_returns),0.995) ))
	}
    # Dev Currencies performance test
    dev_currencies_perf_test_1Day<-NULL
	dev_currencies_perf_test_6M <-NULL
	dev_currencies_perf_test_1Y <-NULL
	dev_currencies_perf_test_RCM<-NULL
	vol = Calibration_resutls$Developed_currencies$vol
	diff_log_fx=Calibration_resutls$Developed_currencies$diff_log_fx
	for(i in c(1:length(developed_Currencies)))
	{
	    ii=match(developed_Currencies[i],vol$name)
		historic_returns=diff_log_fx[,ii]
		historicReturns_6M<-bt_antithetique(historic_returns,10000,replace=FALSE,T=120)	   	   
        historicReturns_1Y<-bt_antithetique(historic_returns,10000,replace=FALSE,T=260)
		# performance Tests 
		dev_currencies_perf_test_1Day  = rbind(dev_currencies_perf_test_1Day,c(developed_Currencies[i],performanceTest_theo_exact(historic_returns,0,vol$fract[ii],confidence_level=0.975,1)))
		dev_currencies_perf_test_6M    = rbind(dev_currencies_perf_test_6M, c(developed_Currencies[i],performanceTest_theo_exact(historicReturns_6M,0,vol$fract[ii],confidence_level=0.975,120)))
		dev_currencies_perf_test_1Y    = rbind(dev_currencies_perf_test_1Y, c(developed_Currencies[i],performanceTest_theo_exact(historicReturns_1Y,0,vol$fract[ii],confidence_level=0.975,260)))
		dev_currencies_perf_test_RCM   = rbind(dev_currencies_perf_test_RCM,c(developed_Currencies[i],RCM_test_exact(historicReturns_1Y,vol$fract[ii],0.995) ))
	}
  
  }
    Emerging_currencies=list(
 	clusters_perf_test_1Day 	   = clusters_perf_test_1Day                              ,
	clusters_perf_test_6M   	   = clusters_perf_test_6M                                ,
	clusters_perf_test_1Y   	   = clusters_perf_test_1Y                                ,
	cluster_rcm_test_1Y     	   = cluster_rcm_test_1Y                                  ,
 	normal_regime_perf_test_1Day        = normal_regime_perf_test_1Day                    ,
	normal_regime_perf_test_6M          = normal_regime_perf_test_6M                      ,
	normal_regime_perf_test_1Y          = normal_regime_perf_test_1Y                      ,
    normal_regime_perf_test_RCM         = normal_regime_perf_test_RCM                     ,
	stationnary_regime_perf_test_1Day   = stationnary_regime_perf_test_1Day               ,
	stationnary_regime_perf_test_6M     = stationnary_regime_perf_test_6M                 ,
	stationnary_regime_perf_test_1Y     = stationnary_regime_perf_test_1Y                 ,
	stationnary_regime_perf_test_RCM_1Y = stationnary_regime_perf_test_RCM_1Y         )
   Developed_currencies=list(  dev_currencies_perf_test_1Day = dev_currencies_perf_test_1Day                         ,
	dev_currencies_perf_test_6M   = dev_currencies_perf_test_6M                           ,
	dev_currencies_perf_test_1Y   = dev_currencies_perf_test_1Y                           ,
	dev_currencies_perf_test_RCM  = dev_currencies_perf_test_RCM )
	return(list(Emerging_currencies=Emerging_currencies,Developed_currencies=Developed_currencies))					
}

Out_of_Sample_backtest_FX_Model<-function(BackTestpath,BackTestWindow)
{
	# Back Test Out Of Sample 
{

    cat("Loading BackTest params \n")
	developed_Currencies    <- c("FXAUD","FXCAD","FXCHF","FXDKK","FXEUR","FXGBP","FXJPY","FXNOK","FXNZD","FXSEK","FXSGD","FXXAG","FXXAU")
	emerging_Currencies     <- c("FXARS","FXBRL","FXCLP","FXCOP","FXCZK","FXEGP","FXHUF","FXIDR","FXILS","FXINR","FXISK","FXKRW","FXMAD","FXMXN","FXMYR","FXPHP","FXPLN","FXRON","FXRUB","FXTHB","FXTND","FXTRY","FXTWD","FXZAR","FXCNY","FXCNH","FXAED","FXHKD","FXSAR","FXKWD","FXBGN")
	# chargement des historiques des devises	
	HistoPath <-gsub("Back Test Out Of Sample","data",BackTestpath)
	
	histoFX 	    <- read.csv(file = paste(HistoPath,"FX_data_calib.csv",sep=""),sep=";",dec = ".", header = TRUE)
	spotDates   	<- as.Date(histoFX[,1],format="%d/%m/%Y")
	CountryRating   <-  read.csv(paste(HistoPath,"Histo_Country_Rating_Table_Emerging.csv",sep=""),sep=";",header=TRUE)
	
	FX_list <- paste("FX",names(histoFX)[-1],sep="")
	histoFX <- as.matrix(histoFX[,-1])	
	
	########################Param to change every Backtest exercice###################
	
	cluster_param_CQ1 <- as.matrix(read.csv(file = paste(BackTestpath,"/param/CQ1/SG.SG.Clusters.param.BT.csv",sep=""),sep=",",dec = ".", header = FALSE))
	cluster_param_CQ2 <- as.matrix(read.csv(file = paste(BackTestpath,"/param/CQ2/SG.SG.Clusters.param.BT.csv",sep=""),sep=",",dec = ".", header = FALSE))
	cluster_param_CQ3 <- as.matrix(read.csv(file = paste(BackTestpath,"/param/CQ3/SG.SG.Clusters.param.BT.csv",sep=""),sep=";",dec = ".", header = FALSE))
	cluster_param_CQ4 <- as.matrix(read.csv(file = paste(BackTestpath,"/param/CQ4/SG.SG.Clusters.param.BT.csv",sep=""),sep=";",dec = ".", header = FALSE))
	
	
	normal_regime_vol_CQ1  		 <-  as.matrix(read.csv(file = paste(BackTestpath,"/param/CQ1/SG.SG.NormalRegime.Volatilities.BT.csv",sep=""),sep=",",dec = ".", header = FALSE))
	normal_regime_vol_CQ2  		 <-  as.matrix(read.csv(file = paste(BackTestpath,"/param/CQ2/SG.SG.NormalRegime.Volatilities.BT.csv",sep=""),sep=",",dec = ".", header = FALSE))
	normal_regime_vol_CQ3  		 <-  as.matrix(read.csv(file = paste(BackTestpath,"/param/CQ3/SG.SG.NormalRegime.Volatilities.BT.csv",sep=""),sep=",",dec = ".", header = FALSE))
	normal_regime_vol_CQ4  		 <-  as.matrix(read.csv(file = paste(BackTestpath,"/param/CQ4/SG.SG.NormalRegime.Volatilities.BT.csv",sep=""),sep=",",dec = ".", header = FALSE))
	
	transition_probabilities_CQ1  <- as.matrix(read.csv(file = paste(BackTestpath,"/param/CQ1/SG.SG.TransitionProbabilities.BT.csv",sep=""),sep=",",dec = ".", header = FALSE))
	transition_probabilities_CQ2  <- as.matrix(read.csv(file = paste(BackTestpath,"/param/CQ2/SG.SG.TransitionProbabilities.BT.csv",sep=""),sep=",",dec = ".", header = FALSE))
	transition_probabilities_CQ3  <- as.matrix(read.csv(file = paste(BackTestpath,"/param/CQ3/SG.SG.TransitionProbabilities.BT.csv",sep=""),sep=",",dec = ".", header = FALSE))
	transition_probabilities_CQ4  <- as.matrix(read.csv(file = paste(BackTestpath,"/param/CQ4/SG.SG.TransitionProbabilities.BT.csv",sep=""),sep=",",dec = ".", header = FALSE))
	
	dev_FX_vol_CQ1  		     <-  as.matrix(read.csv(file = paste(BackTestpath,"/param/CQ1/SG.SG.Dev.Volatility.BT.csv",sep=""),sep=",",dec = ".", header = FALSE))
	dev_FX_vol_CQ2  		     <-  as.matrix(read.csv(file = paste(BackTestpath,"/param/CQ2/SG.SG.Dev.Volatility.BT.csv",sep=""),sep=",",dec = ".", header = FALSE))
	dev_FX_vol_CQ3  		     <-  as.matrix(read.csv(file = paste(BackTestpath,"/param/CQ3/SG.SG.Dev.Volatility.BT.csv",sep=""),sep=",",dec = ".", header = FALSE))
	dev_FX_vol_CQ4  		     <-  as.matrix(read.csv(file = paste(BackTestpath,"/param/CQ4/SG.SG.Dev.Volatility.BT.csv",sep=""),sep=",",dec = ".", header = FALSE))
	
	calibration_param_CQ1  <- list(normal_regime_vol=data.frame(FX=c(normal_regime_vol_CQ1[,1],dev_FX_vol_CQ1[,1]),Vol=c(as.numeric(normal_regime_vol_CQ1[,2]),as.numeric(dev_FX_vol_CQ1[,2]))) ,cluster_params=list(cluster1=list(sigma=as.numeric(cluster_param_CQ1[2,1]),r_down=as.numeric(cluster_param_CQ1[2,2]),lambda=as.numeric(cluster_param_CQ1[2,3])),cluster2=list(sigma=as.numeric(cluster_param_CQ1[3,1]),r_down=as.numeric(cluster_param_CQ1[3,2]),lambda=as.numeric(cluster_param_CQ1[3,3])),cluster3=list(sigma=as.numeric(cluster_param_CQ1[4,1]),r_down=as.numeric(cluster_param_CQ1[4,2]),lambda=as.numeric(cluster_param_CQ1[4,3]))),tp=list(p0Sys=as.numeric(transition_probabilities_CQ1[2,1]),p1Sys=as.numeric(transition_probabilities_CQ1[2,2]),p001=as.numeric(transition_probabilities_CQ1[2,3]),p111=as.numeric(transition_probabilities_CQ1[2,4]),p000=as.numeric(transition_probabilities_CQ1[2,5]),p110=as.numeric(transition_probabilities_CQ1[2,6])))
	calibration_param_CQ2  <- list(normal_regime_vol=data.frame(FX=c(normal_regime_vol_CQ1[,1],dev_FX_vol_CQ1[,1]),Vol=c(as.numeric(normal_regime_vol_CQ1[,2]),as.numeric(dev_FX_vol_CQ1[,2]))) ,cluster_params=list(cluster1=list(sigma=as.numeric(cluster_param_CQ1[2,1]),r_down=as.numeric(cluster_param_CQ1[2,2]),lambda=as.numeric(cluster_param_CQ1[2,3])),cluster2=list(sigma=as.numeric(cluster_param_CQ1[3,1]),r_down=as.numeric(cluster_param_CQ1[3,2]),lambda=as.numeric(cluster_param_CQ1[3,3])),cluster3=list(sigma=as.numeric(cluster_param_CQ1[4,1]),r_down=as.numeric(cluster_param_CQ1[4,2]),lambda=as.numeric(cluster_param_CQ1[4,3]))),tp=list(p0Sys=as.numeric(transition_probabilities_CQ1[2,1]),p1Sys=as.numeric(transition_probabilities_CQ1[2,2]),p001=as.numeric(transition_probabilities_CQ1[2,3]),p111=as.numeric(transition_probabilities_CQ1[2,4]),p000=as.numeric(transition_probabilities_CQ1[2,5]),p110=as.numeric(transition_probabilities_CQ1[2,6])))
	calibration_param_CQ3  <- list(normal_regime_vol=data.frame(FX=c(normal_regime_vol_CQ1[,1],dev_FX_vol_CQ1[,1]),Vol=c(as.numeric(normal_regime_vol_CQ1[,2]),as.numeric(dev_FX_vol_CQ1[,2]))) ,cluster_params=list(cluster1=list(sigma=as.numeric(cluster_param_CQ1[2,1]),r_down=as.numeric(cluster_param_CQ1[2,2]),lambda=as.numeric(cluster_param_CQ1[2,3])),cluster2=list(sigma=as.numeric(cluster_param_CQ1[3,1]),r_down=as.numeric(cluster_param_CQ1[3,2]),lambda=as.numeric(cluster_param_CQ1[3,3])),cluster3=list(sigma=as.numeric(cluster_param_CQ1[4,1]),r_down=as.numeric(cluster_param_CQ1[4,2]),lambda=as.numeric(cluster_param_CQ1[4,3]))),tp=list(p0Sys=as.numeric(transition_probabilities_CQ1[2,1]),p1Sys=as.numeric(transition_probabilities_CQ1[2,2]),p001=as.numeric(transition_probabilities_CQ1[2,3]),p111=as.numeric(transition_probabilities_CQ1[2,4]),p000=as.numeric(transition_probabilities_CQ1[2,5]),p110=as.numeric(transition_probabilities_CQ1[2,6])))
	calibration_param_CQ4  <- list(normal_regime_vol=data.frame(FX=c(normal_regime_vol_CQ1[,1],dev_FX_vol_CQ1[,1]),Vol=c(as.numeric(normal_regime_vol_CQ1[,2]),as.numeric(dev_FX_vol_CQ1[,2]))) ,cluster_params=list(cluster1=list(sigma=as.numeric(cluster_param_CQ1[2,1]),r_down=as.numeric(cluster_param_CQ1[2,2]),lambda=as.numeric(cluster_param_CQ1[2,3])),cluster2=list(sigma=as.numeric(cluster_param_CQ1[3,1]),r_down=as.numeric(cluster_param_CQ1[3,2]),lambda=as.numeric(cluster_param_CQ1[3,3])),cluster3=list(sigma=as.numeric(cluster_param_CQ1[4,1]),r_down=as.numeric(cluster_param_CQ1[4,2]),lambda=as.numeric(cluster_param_CQ1[4,3]))),tp=list(p0Sys=as.numeric(transition_probabilities_CQ1[2,1]),p1Sys=as.numeric(transition_probabilities_CQ1[2,2]),p001=as.numeric(transition_probabilities_CQ1[2,3]),p111=as.numeric(transition_probabilities_CQ1[2,4]),p000=as.numeric(transition_probabilities_CQ1[2,5]),p110=as.numeric(transition_probabilities_CQ1[2,6])))
	
	######################################Calibrations###############################
	CQ1<-list(date_debut=as.Date(BackTestWindow$date_debut),date_fin=as.Date(BackTestWindow$date_debut) %m+% months(3),param=calibration_param_CQ1)
	CQ2<-list(date_debut=as.Date(BackTestWindow$date_debut) %m+% months(3)+days(1),date_fin=as.Date(BackTestWindow$date_debut) %m+% months(6),param=calibration_param_CQ2)
	CQ3<-list(date_debut=as.Date(BackTestWindow$date_debut) %m+% months(6)+days(1),date_fin=as.Date(BackTestWindow$date_debut) %m+% months(9),param=calibration_param_CQ3)
	CQ4<-list(date_debut=as.Date(BackTestWindow$date_debut) %m+% months(9)+days(1),date_fin=BackTestWindow$date_fin,param=calibration_param_CQ4)

	calibration<-cbind(CQ1,CQ2,CQ3,CQ4)
	cat("BackTest params loaded with succes\n")
	####################End Param to change every Backtest exercice###################
	
	##############################Perform BackTest 1 day 9 day#############################
	probs       <-	c(0.01,0.05,0.1,0.5,0.9,0.95,0.99)
	cat("BackTesting one day returns \n")
	BTest1J		<-	BackTest(probs,1,calibration,spotDates,histoFX,CountryRating,developed_Currencies,emerging_Currencies,FX_list)
	cat("BackTesting 9 days returns \n")
	BTest9J		<-	BackTest(probs,9,calibration,spotDates,histoFX,CountryRating,developed_Currencies,emerging_Currencies,FX_list)
	return(list(BTest1J=BTest1J,BTest9J=BTest9J))
}
	
}


}


