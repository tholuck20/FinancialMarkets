################################ fucntions #####################
{
	######################## Calibration Functions #########################################
	{
	dst_<- function (x, location = 0, scale = 1, shape = 0, df = Inf, dp = NULL, 
		log = FALSE) 
	{
		if (!is.null(dp)) {
			if (!missing(shape)) 
				stop("You cannot set both component parameters and dp")
			location <- dp[1]
			scale <- dp[2]
			shape <- dp[3]
			df <- dp[4]
		}
		if (df == Inf) 
			return(dsn(x, location, scale, shape, log = log))
		z <- (x - location)/scale
		pdf <- dt(z, df = df, log = log)
		cdf <- pt(shape * z * sqrt((df + 1)/(z^2 + df)), df = df + 
			1, log = log)
		if (log) 
			log(2) + pdf + cdf - logb(scale)
		else 2 * pdf * cdf/scale
	}
	
	is.weekend <- function(x) {
	x <- as.POSIXlt(x)
	x$wday == 7 | x$wday == 6
	}
		paste_function<- function(v,sep)
	{
			res<-""
			for(i in c(1:length(v)))
			{
				res<-paste(res,v[i],sep=sep)
			}
			return(res)
	}
	
	contagionRiskTestFunction<-function(EM_basket_states,currency_states,contagion)
	{
	
		if(contagion)
		{
			T_1_1_all<-0
			nb_1_1_period<-0
			T_x_1_all<-0
			T_1_0_all<-0
			T_x_0_all<-0
			nb_1_0_period<-0
			emerging_global_crisis<-EM_basket_states
	
			global_and_specific <-EM_basket_states+currency_states
			crisis_1_1<-rep(0,length(global_and_specific))
	
			II11<-which((global_and_specific==2))
				if(length(II11)>0)
					crisis_1_1[II11]=1
				
				
			T_1_1_all<-length(which(crisis_1_1==1))+T_1_1_all
			T_x_1_all<-T_x_1_all+length(which(emerging_global_crisis==1))
			nb_1_1_period<-nb_1_1_period+length(which(diff(crisis_1_1)==1))
			
			just_specific <-2*emerging_global_crisis+currency_states
			crisis_1_0	  <-rep(0,length(global_and_specific))
			II10<-which((just_specific==1))
			if(length(II10)>0)
				crisis_1_0[II10]=1
				
			T_1_0_all<-length(which(crisis_1_0==1))+T_1_0_all
			T_x_0_all<-T_x_0_all+length(which(emerging_global_crisis==0))
			nb_1_0_period<-length(which(diff(crisis_1_0)==1))+nb_1_0_period
	
			T_1_1_average<-T_1_1_all/nb_1_1_period * 365/250
			p_1_1_1<-1-1/T_1_1_average
			global_and_specific_frequency   <-T_1_1_all/T_x_1_all
			p_0_0_1<-1- (1-p_1_1_1)*(global_and_specific_frequency/(1-global_and_specific_frequency))
			T_1_0_average<-T_1_0_all/nb_1_0_period * 365/250
			p_1_1_0<-1-1/T_1_0_average
			just_specific_frequency   <-T_1_0_all/T_x_0_all
			p_0_0_0<-1- (1-p_1_1_0)*(just_specific_frequency/(1-just_specific_frequency))
			#######################Systemic Emerging Crisis#################
			if(	length(which(diff(EM_basket$EM_basket_crisis)==1)) > 0)
			{
				T_EM_1 <-length(which(EM_basket$EM_basket_crisis==1))/length(which(diff(EM_basket$EM_basket_crisis)==1))*365/250
			}else
			{
				T_EM_1<-1
			}	
			crisis_frequency_EM         <-length(which(EM_basket$EM_basket_crisis==1))/length(EM_basket$EM_basket_crisis)
			p_EM_1 <-1 - 1/T_EM_1
			p_EM_0 <- 1- (1-p_EM_1)*(crisis_frequency_EM/(1-crisis_frequency_EM))
			res=list()
			res$p_1_1_1=p_1_1_1
			res$p_0_0_1=p_0_0_1
			res$p_1_1_0=p_1_1_0
			res$p_0_0_0=p_0_0_0
			res$p0Sys=p_EM_0
			res$p1Sys=p_EM_1
			}else{
				T_1_x_all<-0
				nb_1_x_period<-0
				T_x_x_all<-0
	
					T_x_x_all<-T_x_x_all+length(currency_states)
					T_1_x_all<-length(which(currency_states==1))+T_1_x_all
					nb_1_x_period<-nb_1_x_period+length(which(diff(currency_states)==1))
	
			T_1_x_average <-T_1_x_all/nb_1_x_period * 365/250
			p_1_1<-1-1/T_1_x_average
			specific_frequency   <-T_1_x_all/T_x_x_all
			p_0_0<-1- (1-p_1_1)*(specific_frequency/(1-specific_frequency))
			res=list()
			res$p_1_1=p_1_1
			res$p_0_0=p_0_0
			}
			return(res)
			
	} 
	contagionRisk<-function(EM_basket,currency_crisis,contagion)
	{
	
		if(contagion)
		{
			T_1_1_all<-0
			nb_1_1_period<-0
			T_x_1_all<-0
			T_1_0_all<-0
			T_x_0_all<-0
			nb_1_0_period<-0
			emerging_global_crisis<-EM_basket$EM_basket_crisis
			for (i in c(1:length(currency_crisis)))
			{
				II<-match(as.character(currency_crisis[[i]]$dates),as.character(EM_basket$EM_basket_dates))
				global_and_specific <-emerging_global_crisis[II]+currency_crisis[[i]]$states
				crisis_1_1<-rep(0,length(global_and_specific))
	
				II11<-which((global_and_specific==2))
				if(length(II11)>0)
					crisis_1_1[II11]=1
				
				
				T_1_1_all<-length(which(crisis_1_1==1))+T_1_1_all
				T_x_1_all<-T_x_1_all+length(which(emerging_global_crisis[II]==1))
				nb_1_1_period<-nb_1_1_period+length(which(diff(crisis_1_1)==1))
			
				just_specific <-2*emerging_global_crisis[II]+currency_crisis[[i]]$states
				crisis_1_0	  <-rep(0,length(global_and_specific))
				II10<-which((just_specific==1))
				if(length(II10)>0)
					crisis_1_0[II10]=1
					
				T_1_0_all<-length(which(crisis_1_0==1))+T_1_0_all
				T_x_0_all<-T_x_0_all+length(which(emerging_global_crisis[II]==0))
				nb_1_0_period<-length(which(diff(crisis_1_0)==1))+nb_1_0_period
			}
			T_1_1_average<-T_1_1_all/nb_1_1_period * 365/250
			p_1_1_1<-1-1/T_1_1_average
			global_and_specific_frequency   <-T_1_1_all/T_x_1_all
			p_0_0_1<-1- (1-p_1_1_1)*(global_and_specific_frequency/(1-global_and_specific_frequency))
			T_1_0_average<-T_1_0_all/nb_1_0_period * 365/250
			p_1_1_0<-1-1/T_1_0_average
			just_specific_frequency   <-T_1_0_all/T_x_0_all
			p_0_0_0<-1- (1-p_1_1_0)*(just_specific_frequency/(1-just_specific_frequency))
			#######################Systemic Emerging Crisis#################
			if(	length(which(diff(EM_basket$EM_basket_crisis)==1)) > 0)
			{
				T_EM_1 <-length(which(EM_basket$EM_basket_crisis==1))/length(which(diff(EM_basket$EM_basket_crisis)==1))*365/250
			}else
			{
				T_EM_1<-1
			}	
			crisis_frequency_EM         <-length(which(EM_basket$EM_basket_crisis==1))/length(EM_basket$EM_basket_crisis)
			p_EM_1 <-1 - 1/T_EM_1
			p_EM_0 <- 1- (1-p_EM_1)*(crisis_frequency_EM/(1-crisis_frequency_EM))
			res=list()
			res$p_1_1_1=p_1_1_1
			res$p_0_0_1=p_0_0_1
			res$p_1_1_0=p_1_1_0
			res$p_0_0_0=p_0_0_0
			res$p0Sys=p_EM_0
			res$p1Sys=p_EM_1
			}else{
				T_1_x_all<-0
				nb_1_x_period<-0
				T_x_x_all<-0
				for (i in c(1:length(currency_crisis)))
			{
					T_x_x_all<-T_x_x_all+length(currency_crisis[[i]]$states)
					T_1_x_all<-length(which(currency_crisis[[i]]$states==1))+T_1_x_all
					nb_1_x_period<-nb_1_x_period+length(which(diff(currency_crisis[[i]]$states)==1))
			}
			T_1_x_average <-T_1_x_all/nb_1_x_period * 365/250
			p_1_1<-1-1/T_1_x_average
			specific_frequency   <-T_1_x_all/T_x_x_all
			p_0_0<-1- (1-p_1_1)*(specific_frequency/(1-specific_frequency))
			res=list()
			res$p_1_1=p_1_1
			res$p_0_0=p_0_0
			}
			return(res)
			
	}
	
	transitionProb<-function(currency_crisis)
	{
			crisis_length<-0
			nb_crisis	 <-0
			serie_length <-0	
			for (i in c(1:length(currency_crisis)))
			{
				crisis_length       <- crisis_length+length(which(currency_crisis[[i]]$states==1))
				nb_crisis	        <- nb_crisis+length(which(diff(currency_crisis[[i]]$states)==1))
				serie_length        <- serie_length+length(currency_crisis[[i]]$states)
				
			}	
			average_crisis_length    <-crisis_length/nb_crisis * 365/250
			crisis_frequency         <-crisis_length/serie_length
			p11      <- 1 - 1/average_crisis_length
			p00      <- 1- (1-p11)*(crisis_frequency/(1-crisis_frequency))
			res=list()
			res$p_1_1=p11
			res$p_0_0=p00
			return(res)	
	}
	
	vol_fractile = function (x)
	{
	b = mean(x,na.rm = T)
	n = length(x[!is.na(x)])
	#vol_std = sd(x,na.rm = T)*sqrt(1-1/n)
	vol_std = sd(x,na.rm = T)
	vol_1_pct = (quantile(x,0.01,na.rm=T) - b) /qnorm(0.01)
	vol_99_pct = (quantile(x,0.99,na.rm=T) - b) /qnorm(0.99)
	res = max(vol_std,vol_1_pct,vol_99_pct)
	return(list(volStd=vol_std*sqrt(260),volFract=res*sqrt(260)))
	}
	
	fit_jump_down = function(rdt,randomStart=FALSE,trace)
	{
		
		ll_jump=function(x,rdt){
			log_likelihood<-0
			for (i in 1:length(rdt)){
				mu         =x[1]
				sig2       =x[2]
				lambda	   =0.03287671 
				r_down     =x[3] 
				p1_down	   =dpois(1,lambda)
				integration_bound=30*sqrt(sig2)
				f_down =as.double(integrate(function(x) dgamma(-x,shape=1,rate=r_down)*dnorm(rdt[i],mean=(mu+x) ,sd=sqrt(sig2)), -1, 0)[1])		
				likelihood	   <-(1-p1_down)*dnorm(rdt[i],mean=mu ,sd=sqrt(sig2))+p1_down*f_down
				log_likelihood <- log_likelihood + log(likelihood)
			}
			return(-log_likelihood)
		}
	
		LB=c(-1,10^-5,0)
		x0=c(mean(rdt),var(rdt),15)
		UB=c(1,0.005,100)
		if(randomStart){
			Optim <- gosolnp(pars = x0, fixed = NULL, fun = ll_jump, LB = LB, UB = UB,rdt=rdt, control = list(outer.iter = 100, trace = trace),
			distr = rep(1, length(LB)), distr.opt = list(), n.restarts = 2, n.sim = 500, rseed = 443)
		}else{
	
			Optim <- solnp(pars=x0, fun=ll_jump,  UB=UB,LB=LB,rdt=rdt,control=list(trace=trace, tol=1e-12))
		}
			
		res <- list(mu=Optim$pars[1],sig2=Optim$pars[2],r_down=Optim$pars[3])
		return (res)
	}	
	
	max_drop_down=function(spot,period)
	{
	
	FX_i_spot=spot[seq(1+period,length(spot),by=1)]
	FX_i_spot_lag=spot[seq(1,length(spot)-period,by=1)]
	FX_i_rdt=log(FX_i_spot)-log(FX_i_spot_lag)
	rdt_sort=sort(FX_i_rdt)
	return (rdt_sort[1])
	
	}
	max_drop_down_returns=function(rdt,period)
	{
	rdt_lag  =sapply(seq(1,length(rdt)-period+1,by=1),function(k){ tail(cumsum(rdt[k:(k+period-1)]),1)})
	rdt_lag  = rdt_lag[!is.na(rdt_lag)]
	rdt_sort=sort(rdt_lag)
	return (rdt_sort[1])
	
	}
	
	max_drop_up=function(spot,period)
	{
	
	FX_i_spot=spot[seq(1+period,length(spot),by=1)]
	FX_i_spot_lag=spot[seq(1,length(spot)-period,by=1)]
	FX_i_rdt=log(FX_i_spot)-log(FX_i_spot_lag)
	rdt_sort=sort(FX_i_rdt)
	return (tail(rdt_sort,n=1))
	
	}
	
	max_drop_up_returns=function(rdt,period)
	{
		    rdt_lag  =sapply(seq(1,length(rdt)-period+1,by=1),function(k){ tail(cumsum(rdt[k:(k+period-1)]),1)})
			rdt_lag  = rdt_lag[!is.na(rdt_lag)]
			rdt_sort=sort(rdt_lag)
			return (tail(rdt_sort,n=1))
	}
	

	min_max_average_return=function(mu,sigma,lambda_up,lambda_down,r_down,r_up,T,qtl)
	{
		nb_simul=1000
		jump_up=rpois(nb_simul*T,lambda_up) *rgamma(nb_simul*T,shape=1, rate = r_up)
		jump_up=sapply(seq(1,length(jump_up),by=T),function(i) sum(jump_up[c(i:(i+T-1))]))
		jump_down=rpois(nb_simul*T,lambda_down) *rgamma(nb_simul*T,shape=1, rate = r_down)
		jump_down=sapply(seq(1,length(jump_down),by=T),function(i) sum(jump_down[c(i:(i+T-1))]))
		rdt=rnorm(nb_simul,mean = mu*T, sd = sigma*sqrt(T/250)) +jump_up-jump_down
		return(c(quantile(rdt,qtl),mean(rdt),quantile(rdt,1-qtl)))
	}
	
	min_max_average_return2=function(mu,sigma,lambda_down,r_down,T,qtl)
	{
		nb_simul=1000
		jump_down=rpois(nb_simul*T,lambda_down) *rgamma(nb_simul*T,shape=1, rate = r_down)
		jump_down=sapply(seq(1,length(jump_down),by=T),function(i) sum(jump_down[c(i:(i+T-1))]))
		rdt=rnorm(nb_simul,mean = mu*T, sd = sigma*sqrt(T/250)) -jump_down
		return(c(quantile(rdt,qtl),mean(rdt),quantile(rdt,1-qtl)))
	}
	
	var_skew=function(mu,sigma,lambda_up,lambda_down,r_down,r_up)
	{
		nb_simul=1000
		jump_up=rpois(nb_simul*T,lambda_up) *rgamma(nb_simul*T,shape=1, rate = r_up)
		jump_up=sapply(seq(1,length(jump_up),by=T),function(i) sum(jump_up[c(i:(i+T-1))]))
		jump_down=rpois(nb_simul*T,lambda_down) *rgamma(nb_simul*T,shape=1, rate = r_down)
		jump_down=sapply(seq(1,length(jump_down),by=T),function(i) sum(jump_down[c(i:(i+T-1))]))
		rdt=rnorm(nb_simul,mean = mu*T, sd = sigma*sqrt(T/250)) +jump_up-jump_down
		return(c(sd(rdt)*sqrt(250),skewness(rdt)))
	}
	
	var_skew2=function(mu,sigma,lambda_down,r_down)
	{
		nb_simul=1000
		jump_down=rpois(nb_simul*T,lambda_down) *rgamma(nb_simul*T,shape=1, rate = r_down)
		jump_down=sapply(seq(1,length(jump_down),by=T),function(i) sum(jump_down[c(i:(i+T-1))]))
		rdt=rnorm(nb_simul,mean = mu*T, sd = sigma*sqrt(T/250)) -jump_down
		return(c(sd(rdt)*sqrt(250),skewness(rdt)))
	}
	
	getRating=function (CountryRatingTable,FX)
	{
		if(FX == "CNH")
		{
			FX="CNY"
		}
		country_dates=as.Date(as.vector(CountryRatingTable$X),format="%d/%m/%y")
		Country_names=names(CountryRatingTable)
		i_ctry=which(Country_names == FX)
		country_dates_ext<-NULL
		ratings<-NULL
		for(i in 1:(length(country_dates)-1))
		{
			country_dates_ext_i<-  as.character(seq(country_dates[i],country_dates[i+1]-1,1))
			country_dates_ext<- c(country_dates_ext,country_dates_ext_i)
			ratings<-c(ratings,rep(CountryRatingTable[i,i_ctry],length(country_dates_ext_i)))
		}
		ratings			 <- c(ratings,CountryRatingTable[length(country_dates),i_ctry])
		country_dates_ext<- c(country_dates_ext,as.character(country_dates[length(country_dates)]))
		I=!is.na(ratings)
		ratings=ratings[I]
		dates=as.Date(country_dates_ext)[I]
		return(list(dates=dates,ratings=ratings))
	}
	
	compare_distribution=function(rdt)
	{
		#fit theoretical model
		JumpModel <-fit_jump_down(rdt,randomStart=TRUE,trace=1)
		mu     	  <- JumpModel$mu
		sigma     <- sqrt(JumpModel$sig2*250)
		r_down      <- JumpModel$r_down
		lambda_down <- 0.03287671
		#simulate theoretical model			
		nb_simul=10000
		jump_down=rpois(nb_simul*T,lambda_down) *rgamma(nb_simul*T,shape=1, rate = r_down)
		jump_down=sapply(seq(1,length(jump_down),by=T),function(i) sum(jump_down[c(i:(i+T-1))]))
		rdt_theo=rnorm(nb_simul,mean = mu*T, sd = sigma*sqrt(T/250)) -jump_down
		#plot distribution
		estHisto <- bkde(rdt, bandwidth=0.008)
		estTheo <- bkde(rdt_theo, bandwidth=0.008)
		mytitle="Comparaison des distributions des rdts de crises Historiques et Theoriques [KRW]"
		ylim=c(0,25)
		xlim=c(min(estTheo$x,estHisto$x),max(estTheo$x,estHisto$x))
		plot(estHisto$x,estHisto$y, type="l",col="blue",main=mytitle,xlim=xlim,ylim=ylim,xlab = "Return", ylab = "Density",cex.main=0.8)
	
		par(new=TRUE)
		plot(estTheo$x,estTheo$y, type="l",col="red",xlim=xlim,ylim=ylim,xlab = "Return", ylab = "Density")
	
		legend("topleft",col=c("blue","red"),lty=1,legend=c("Historique","Theorique"))
	
	}
	
	setClass("RS_Model",representation(
						# filtProb="matrix"   ,   # filtered probabilities (t¦t)
						smoothProb="matrix" ,   # smoothed prob
						# p_o="matrix" ,   # smoothed prob
						# Coeff="list"        ,   # all coefficients
						# condMean="matrix"   ,   # conditional Mean
						# condStd="matrix"    ,   # conditional standard deviation
						# Coeff_Std="list"    ,   # Standard errors for coefficients
						LL="numeric"        ,   # final log likelihood
						# k="numeric"         ,   # number of states
						# paramVec="numeric"  ,   # vector of parameters (has the same values as Coeff)
						# stateDur="numeric"  ,   # The expected duration of each state 
						# nParameter="numeric",   # number of parameters in the model
						# sizeModel="list"    ,   # a list with the size of the model (number of indep var, etc)
						viterbiStates="matrix",
						Coeff="list" 
						))  # Assumed distribution for ML estimation
						
	print.MS_Model<-function(MS_Model_In)
	{
	
	n_S      =MS_Model_In@sizeModel$n_S
	n_nS     =MS_Model_In@sizeModel$n_nS
	nIndep   =MS_Model_In@sizeModel$nIndep
	S_S      =MS_Model_In@sizeModel$S_S
	S_nS     =MS_Model_In@sizeModel$S_nS
	Coeff    =MS_Model_In@Coeff
	Coeff_Std=MS_Model_In@Coeff_Std
	k        =MS_Model_In@k
	distrib  =MS_Model_In@distrib
	stateDur =MS_Model_In@stateDur
	nr	   =MS_Model_In@sizeModel$nr
	
	
	
	# Sending output to R
	
	cat('\n\n***** Numerical Optimization for MS Model Converged *****\n\n')
	cat('Final log Likelihood:',MS_Model_In@LL,'\n')
	cat('Number of parameters:',MS_Model_In@nParameter,'\n')
	cat('Distribution Assumption ->',MS_Model_In@distrib,'\n')
	
	#cat('Method for standard error calculation -> ',num2str(std_method),'\n']); IMPLEMENT LATER OTHER STD CALC
	
	cat('\n***** Final Parameters *****\n');
	cat('\n---> Non Switching Parameters <---\n');
	
	if (n_nS==0)
		cat('\nThere was no Non Switching Parameters. Skipping this result')
	else
	{
		for (i in 1:n_nS)
			{
			cat('\n Non Switching Parameter at Indep  Column ', S_nS[i]);
			cat('\n      Value:    ', sprintf("%4.4f",Coeff$indep_nS[i]))
			cat('\n      Std error:', sprintf("%4.4f (%4.2f)",Coeff_Std$indep_nS[i],2*(1-pt(abs(Coeff$indep_nS[i]/Coeff_Std$indep_nS[i]),nr-MS_Model_In@nParameter)) ) )
			}
	}
	
	cat('\n\n--->   Switching Parameters   <---\n');
	
	for (i in 1:k)
	{
		cat('\n  State', i);
		cat('\n      mu   :				  ', sprintf("%4.8f ",as.double(Coeff$u[i])))
		cat('\n      omega:				  ', sprintf("%4.8f",Coeff$omega[i]))
		cat('\n      alpha:               ', sprintf("%4.8f ",Coeff$alpha[i]))
		cat('\n      beta :               ', sprintf("%4.8f ",Coeff$beta[i]))
	
	
	}
	
	for (i in 1:n_S)
	{
		cat('\n\n  Switching Parameters for Indep  Column ', S_S[i],'\n');
	
		for (j in 1:k)
		{
			cat('\n  State ', j);
			cat('\n     Value:     ',sprintf("%4.4f",Coeff$indep_S[i,j]))
			cat('\n     Std error: ',sprintf("%4.4f (%4.2f)",Coeff_Std$indep_S[i,j],2*(1-pt(abs(Coeff$indep_S[i,j]/Coeff_Std$indep_S[i,j]),nr-MS_Model_In@nParameter)) ))
		}
	}
	
	cat('\n\n---> Transition Probabilities Matrix <---\n');
	
	for (i in 1:k)
	{
		cat('\n      ')
		for (j in 1:k)
		{
			str<-sprintf('%4.8f   ',Coeff$P[i,j])
			cat(str)
		}
	}
	
	cat('\n\n---> Expected Duration of Regimes <---\n\n');
	
	
	for (i in 1:k)
	{
		str<-sprintf('     Expected duration of Regime #%i: %4.2f time periods\n',i,stateDur[i])
		cat(str)
	}
	
	}
	# FUNCTION FOR SIZE OF MS MODEL
	
	dim.MS_Model<-function(MS_Model_In)
	{
	nr        =MS_Model_In@sizeModel$nr
	nParameter=MS_Model_In@nParameter
	n_S       =MS_Model_In@sizeModel$n_S
	n_nS      =MS_Model_In@sizeModel$n_nS
	nIndep    =MS_Model_In@sizeModel$nIndep
	S_S       =MS_Model_In@sizeModel$S_S
	S_nS      =MS_Model_In@sizeModel$S_nS
	k         =MS_Model_In@k
	distrib   =MS_Model_In@distrib
	
	cat('\n***** Dimension of Markov Switching Model *****\n\n')
	
	cat('Number of Observations:',nr,'\n')
	cat('Number of Parameters:',nParameter,'\n')
	cat('Number of States:',k,'\n')
	cat('Fitted Distribution:',distrib,'\n')
	cat('Number of Independent Variables:\n')
	cat('     Switching States:',n_S,'\n')
	cat('     Non-Switching States:',n_nS,'\n')
	cat('     Total:',(n_S+n_nS),'\n')
	
	return()
	}
	
	# PLOTTING FUNCTION FOR MARKOV SWITCHING MODEL
	
	plot.MS_Model<-function(MS_Model)
	{
	MS_Model_In<-MS_Model$model
	k<-MS_Model_In@k    
	
	# fixing up the colors
	
	niceColors<-c("red","blue","green","black")
	allColors<-colors()
	
	for (i in 1:length(niceColors))
	{
		idx<-which(niceColors[i]==allColors)
		allColors=allColors[-idx]
	}
	
	if (k>4)
		myColors<-c(niceColors,sample(allColors,k-length(niceColors)))
	else
		myColors<-niceColors[1:k]
	
	col<-myColors 
	
	# Fixing up the strings in the legend
	
	myStr<-c("State 1")
	for (i in 2:k)
	{   
		str<-sprintf("State %i",i)
		myStr<-c(myStr,str)
	}
	
	# 4 plots in one window
		
	par(mfrow=c(2,2))   
	
	matplot(MS_Model_In@filtProb,
			xlab="Time",
			ylab="Filtered Probabilities",
			type='l',
			col=col)
	title("Filtered Probabilities for each State")
	legend('topright',myStr,col=col,lty=1)
	
	matplot(MS_Model_In@smoothProb,
			xlab="Time",
			ylab="Smoothed Probabilities for each State",
			type='l',
			col=c('red','blue'))
	title("Smoothed Probabilities")
	legend('topright',myStr,col=col,lty=1)
	
	# plot(MS_Model_In@condMean,
		# xlab="Time",
		# ylab="Fitted Conditional Mean",
		# type='l')
	# title("Fitted Conditional Mean")
	
	# plot(MS_Model_In@condStd,
		# xlab="Time",
		# ylab="Conditional Standard Deviation",
		# type='l')
	# title("Fitted Conditional Standard Deviation")
	
	}
	
	RSGARCH_Fit<-function(logR,n_regime=2,equal_by_reg,mean_incl,skew_incl,pars0,distIn,garch_variance,Underlying)
	{
	
	if(mean_incl) {
	
		u <- pars0[1]; u <- rep(u, length(u)+(!equal_by_reg[1]))
		uDown<--0.1 ;uDown<-rep(-0.1,length(uDown)+(!equal_by_reg[1]))
		uDown<- uDown[1:((!equal_by_reg[1])*(n_regime-1)+1)]
		uUp<-0.1    ;uUp<-rep(0.1,length(uUp)+(!equal_by_reg[1]))
		uUp<- uUp[1:((!equal_by_reg[1])*(n_regime-1)+1)]
		
	}else 
	{
		u    <- 0
	}
	
	h_0<-as.double(var(logR))
	if(n_regime==2)
	{
		a0 <- c(pars0[2]/2, pars0[2]*2); if(equal_by_reg[2]) a0 <- pars0[2];
	}else if(n_regime==1)
	{
		a0 <- pars0[2]
	}
	a0Up <- c(Inf, Inf); if(equal_by_reg[2]) a0Up <- Inf;
	a0Up <- ( a0Up[1:((!equal_by_reg[2])*(n_regime-1)+1)] )
	a0Down <- c(0, 0); if(equal_by_reg[2]) a0Down <- 0;
	a0Down <- ( a0Down[1:((!equal_by_reg[2])*(n_regime-1)+1)] )
	lower<-a0Down
	upper<-a0Up
	if(garch_variance)
	{
		a1 <- pars0[3]; a1 <- rep(a1, length(a1)+(!equal_by_reg[3]));
		b  <- pars0[4]; b  <- rep(b , length(b) +(!equal_by_reg[4]));
		a1Up <-Inf  ; a1Up <- rep(a1Up, length(a1Up)+(!equal_by_reg[3]));
		a1Down <- 0 ; a1Down <- rep(a1Down, length(a1Down)+(!equal_by_reg[3]));
		a1Up<- ( a1Up[1:((!equal_by_reg[3])*(n_regime-1)+1)] )
		a1Down <- ( a1Down[1:((!equal_by_reg[3])*(n_regime-1)+1)] )
		bUp  <- Inf;  bUp  <- rep(bUp , length(bUp) +(!equal_by_reg[4]));
		bDown  <- 0;  bDown  <- rep(bDown , length(bDown) +(!equal_by_reg[4]));
		bUp <- (  bUp[1:((!equal_by_reg[4])*(n_regime-1)+1)] )
		bDown  <- (  bDown[1:((!equal_by_reg[4])*(n_regime-1)+1)] )
		lower<-c(lower,a1Down,bDown)
		upper<-c(upper,a1Up,bUp)
	}
	pDown<-rep(0,n_regime)
	pUp  <-rep(1,n_regime)
	
	
	if(mean_incl) {
	lower=c(uDown,lower)
	upper=c(uUp,upper)
	}
	if(n_regime==2)
	{
			P  <- matrix(c(0.99, 0.01, 0.1, 0.9), ncol=2)
			lower=c(lower,pDown)
			upper=c(upper,pUp)
	
	}
	if(n_regime==1)
	{
		P  <- matrix(c(1), ncol=1)
	}
	
	if(distIn=="Student")
	{
		df_  <-rep(10,1+(!equal_by_reg[5])*(n_regime-1))
		lower<-c(lower,rep(0,1+(!equal_by_reg[5])*(n_regime-1)))
		upper<-c(upper,rep(Inf,1+(!equal_by_reg[5])*(n_regime-1)))
	if(skew_incl){
		shape<-rep(0,1+(!equal_by_reg[5])*(n_regime-1))
		lower<-c(lower,rep(-1, 1+(!equal_by_reg[6])*(n_regime-1)))
		upper<-c(upper,rep(1,1+(!equal_by_reg[6])*(n_regime-1)))
	}
	all_vars <- real_to_working__(u, a0, a1, b, P,df_,shape, n_regime=n_regime,
								equal_by_reg=equal_by_reg, mean_incl=mean_incl,skew_incl=skew_incl,distIn=distIn,garch_variance=garch_variance)
	
								
	}else if(distIn=="Normal"){
	
	all_vars <- real_to_working__(u, a0, a1, b, P, n_regime=n_regime,
								equal_by_reg=equal_by_reg, mean_incl=mean_incl,skew_incl=skew_incl,distIn=distIn,garch_variance=garch_variance)
								
	}else if(distIn=="gaussJumpDown"){
		lambda_down  <-rep(0.01,1+(!equal_by_reg[7])*(n_regime-1))
		r_down <- rep(10,1+(!equal_by_reg[7])*(n_regime-1))
		lower<-c(lower,rep(0.000000001,1+(!equal_by_reg[7])*(n_regime-1)),rep(0.00000001,1+(!equal_by_reg[7])*(n_regime-1)))
		upper<-c(upper,rep(Inf,1+(!equal_by_reg[7])*(n_regime-1)),rep(Inf,1+(!equal_by_reg[7])*(n_regime-1)))	  
		all_vars <- real_to_working__(u, a0, a1, b, P,lambda_down=lambda_down,r_down=r_down, n_regime=n_regime,
								equal_by_reg=equal_by_reg, mean_incl=mean_incl,skew_incl=skew_incl,distIn=distIn,garch_variance=garch_variance)
	
	}else if(distIn=="gaussJump"){
		lambda_down  <-rep(0.01,1+(!equal_by_reg[7])*(n_regime-1))
		r_down       <- rep(5,1+(!equal_by_reg[7])*(n_regime-1))
		lambda_up    <-rep(0.01,1+(!equal_by_reg[7])*(n_regime-1))
		r_up         <- rep(5,1+(!equal_by_reg[7])*(n_regime-1))
		lower<-c(lower,rep(0.000000001,1+(!equal_by_reg[7])*(n_regime-1)),rep(0.000000001,1+(!equal_by_reg[7])*(n_regime-1)),rep(0.000000001,1+(!equal_by_reg[7])*(n_regime-1)),rep(0.00000001,1+(!equal_by_reg[7])*(n_regime-1)))
		upper<-c(upper,rep(Inf,1+(!equal_by_reg[7])*(n_regime-1)),rep(100,1+(!equal_by_reg[7])*(n_regime-1)),rep(Inf,1+(!equal_by_reg[7])*(n_regime-1)),rep(100,1+(!equal_by_reg[7])*(n_regime-1)))	  
		all_vars <- real_to_working__(u, a0, a1, b, P,lambda_down=lambda_down,r_down=r_down,lambda_up=lambda_up,r_up=r_up, n_regime=n_regime,
								equal_by_reg=equal_by_reg, mean_incl=mean_incl,skew_incl=skew_incl,distIn=distIn,garch_variance=garch_variance)
	
	}
	
	cat(Underlying,"::Parameters Optimisation... \n")
	rel_tol  <- 1e-12      #optimization parameter 
	Optim <- solnp(pars=all_vars, fun=RSGARCH,  UB=upper,LB=lower,n_regime=n_regime, equal_by_reg=equal_by_reg,
										var_upd="gray", mean_incl=mean_incl,skew_incl=skew_incl, init=init, h_0=as.double(var(logR)),
										logR=logR,distIn=distIn, control=list(trace=0, tol=rel_tol),garch_variance=garch_variance)
										
	outputFull <- RSGARCH_full(all_vars, n_regime=n_regime, equal_by_reg=equal_by_reg,
							var_upd="gray", mean_incl=mean_incl,skew_incl=skew_incl, init=init, h_0=h_0,
								logR=logR,distIn=distIn,garch_variance=garch_variance)
	
	
	all_vars	<-Optim$pars						  
	params <- working_to_real__(all_vars, n_regime=n_regime, equal_by_reg=equal_by_reg,
								mean_incl=mean_incl,skew_incl=skew_incl,distIn=distIn,garch_variance=garch_variance)
								
	if(n_regime==2)
	{
		cat(Underlying,"::Viterbi States... \n")
		vb_States<-Viterbi(params$pi_0,params$P,outputFull$eta)
	}else{
		vb_States<-rep(1,length(outputFull$eta))
	}
	
	RS_Model_Out<-new("RS_Model",
				# filtProb=specOut$filtProb  ,
				smoothProb=as.matrix(outputFull$p.smooth ),  		   
				# Coeff=specOut$Coeff        ,
				viterbiStates=as.matrix(vb_States)       ,
				# # condMean=specOut$condMean  ,
				# # condStd=specOut$condStd    ,
				# Coeff_Std=Coeff_Std        ,
				LL=-Optim$values[length(Optim$values)],
				# k=k                        ,
				# paramVec=fittedParam$par   ,
				# stateDur=stateDur          ,
				# nParameter=length(fittedParam$par) ,
				# sizeModel=sizeModel        ,
				Coeff=params )
	
	return(RS_Model_Out)
	}
	
	real_to_working__ <- function(u, a0, a1, b, P,df_,shape,lambda_down,r_down,lambda_up,r_up, n_regime=2, equal_by_reg=rep(FALSE, 4),
								mean_incl=TRUE,skew_incl=FALSE, var_upd="gray",distIn,garch_variance){
	
	u        <-   u[1:((!equal_by_reg[1])*(n_regime-1)+1)]
	a0.tr    <-   ( a0[1:((!equal_by_reg[2])*(n_regime-1)+1)] )
	all_vars <-   a0.tr 
	if(garch_variance){
		a1.tr <- ( a1[1:((!equal_by_reg[3])*(n_regime-1)+1)] )
		b.tr  <- (  b[1:((!equal_by_reg[4])*(n_regime-1)+1)] )
		all_vars<-c(as.vector(all_vars),a1.tr, b.tr)
	}
	if(n_regime==2)
	{
	P.tr <- as.vector(c(P[1,1],P[2,2]))
	all_vars<-c(as.vector(all_vars), P.tr) 
	}
	if(mean_incl==TRUE) all_vars <- c(u, all_vars)
	
	if(distIn=="Student") {
		df.tr <-    df_[1:((!equal_by_reg[5])*(n_regime-1)+1)]
			all_vars<-c(as.vector(all_vars),df.tr)
		if(skew_incl==TRUE){
		shape.tr <-    shape[1:((!equal_by_reg[6])*(n_regime-1)+1)]
		all_vars<-c(as.vector(all_vars),shape.tr)	
		}
	}else if(distIn=="gaussJumpDown"){
	
		lambda_down.tr <-    lambda_down[1:((!equal_by_reg[7])*(n_regime-1)+1)]
		r_down.tr <-    r_down[1:((!equal_by_reg[7])*(n_regime-1)+1)]
		all_vars<-c(as.vector(all_vars),lambda_down.tr,r_down.tr)	
	}else if(distIn=="gaussJump"){
		lambda_down.tr <-    lambda_down[1:((!equal_by_reg[7])*(n_regime-1)+1)]
		r_down.tr <-    r_down[1:((!equal_by_reg[7])*(n_regime-1)+1)]
		lambda_up.tr <-    lambda_up[1:((!equal_by_reg[7])*(n_regime-1)+1)]
		r_up.tr <-    r_up[1:((!equal_by_reg[7])*(n_regime-1)+1)]
		all_vars<-c(as.vector(all_vars),lambda_down.tr,r_down.tr,lambda_up.tr,r_up.tr)	
	}
	
	return(as.vector(all_vars))
	}
	
	working_to_real__ <- function(all_vars, n_regime=2, equal_by_reg=rep(FALSE, 4),
								mean_incl=TRUE,skew_incl=FALSE, var_upd="gray",distIn,garch_variance){
	
	if(mean_incl==TRUE){
		u <- all_vars[1:((!equal_by_reg[1])*(n_regime-1)+1)]
		dummy <- all_vars[-(1:length(u))]
	} else { u <- 0; dummy <- all_vars; }
	
	
	a0 <- dummy[1:((!equal_by_reg[2])*(n_regime-1)+1)]
	dummy <- dummy[-(1:length(a0))]
	if(garch_variance){
		a1 <- dummy[1:((!equal_by_reg[3])*(n_regime-1)+1)]
		dummy <- dummy[-(1:length(a1))]
		b <- dummy[1:((!equal_by_reg[4])*(n_regime-1)+1)]
		dummy <- dummy[-(1:length(b))]
	}else{
			a1 <-NULL
			b<-NULL
	}
	P<-matrix(0,n_regime,n_regime)
	if (n_regime == 2) {
		P[1,1]<-dummy[1]
		P[1,2]<-1-dummy[1]
		P[2,1]<-1-dummy[2]
		P[2,2]<-dummy[2]
		pi_0 <- c(P[2,1] / (P[1,2] + P[2,1]),P[1,2] / (P[1,2] + P[2,1]))
		dummy <- dummy[-(1:n_regime)]
		if(distIn=="Student"){
				df=dummy[1:((!equal_by_reg[5])*(n_regime-1)+1)]
				dummy <- dummy[-(1:length(df))]
				if(skew_incl==TRUE) shape=dummy[1:((!equal_by_reg[6])*(n_regime-1)+1)] else shape<-rep(0,n_regime)
				dummy <- dummy[-(1:length(shape))]	
				if(garch_variance){
				if(skew_incl==TRUE)
					return(list(u=u, a0=a0, a1=a1, b=b, P=P, pi_0=pi_0,df=df,shape=shape))	
					else
					return(list(u=u, a0=a0, a1=a1, b=b, P=P, pi_0=pi_0,df=df))	
				}else{
				if(skew_incl==TRUE)
					return(list(u=u, a0=a0, P=P, pi_0=pi_0,df=df,shape=shape))	
					else
					return(list(u=u, a0=a0, P=P, pi_0=pi_0,df=df))	
				}			
			}else if(distIn=="gaussJumpDown"){	  
				lambda_down=dummy[1:((!equal_by_reg[7])*(n_regime-1)+1)]
				dummy <-dummy[-(1:length(lambda_down))]
				r_down=dummy[1:((!equal_by_reg[7])*(n_regime-1)+1)]
				if(garch_variance){
					return(list(u=u, a0=a0, a1=a1, b=b, P=P, pi_0=pi_0,lambda_down=lambda_down,r_down=r_down))		
				}else{
					return(list(u=u, a0=a0, P=P, pi_0=pi_0,lambda_down=lambda_down,r_down=r_down))
				}
			}else if(distIn=="gaussJump"){	  
				lambda_down=dummy[1:((!equal_by_reg[7])*(n_regime-1)+1)]
				dummy <-dummy[-(1:length(lambda_down))]
				r_down=dummy[1:((!equal_by_reg[7])*(n_regime-1)+1)]
				dummy <-dummy[-(1:length(r_down))]
				lambda_up=dummy[1:((!equal_by_reg[7])*(n_regime-1)+1)]
				dummy <-dummy[-(1:length(lambda_up))]
				r_up=dummy[1:((!equal_by_reg[7])*(n_regime-1)+1)]
				if(garch_variance){
					return(list(u=u, a0=a0, a1=a1, b=b, P=P, pi_0=pi_0,lambda_down=lambda_down,r_down=r_down,lambda_up=lambda_up,r_up=r_up))		
				}else{
					return(list(u=u, a0=a0, P=P, pi_0=pi_0,lambda_down=lambda_down,r_down=r_down,lambda_up=lambda_up,r_up=r_up))
				}
			}else if(distIn=="Normal"){
			if(garch_variance){
			return(list(u=u, a0=a0, a1=a1, b=b, P=P, pi_0=pi_0))	
			}else{	  
			return(list(u=u, a0=a0, P=P, pi_0=pi_0))	
			}		 
			}
	
			
	
						
	}
	if (n_regime == 1) {
			P<-matrix(1,n_regime,n_regime)
			P=P
			pi_0=P
			if(distIn=="Student"){
					df=dummy[1:((!equal_by_reg[5])*(n_regime-1)+1)]	
					dummy <- dummy[-(1:length(df))]
					if(skew_incl==TRUE)  shape<-dummy[1:((!equal_by_reg[6])*(n_regime-1)+1)] else shape<-0
					dummy <- dummy[-(1:length(shape))]
					return(list(u=u, a0=a0, a1=a1, b=b, P=P, pi_0=P,df=df,shape=shape))
			}else if(distIn=="gaussJumpDown"){
					lambda_down=dummy[1:((!equal_by_reg[7])*(n_regime-1)+1)]
					dummy <-dummy[-(1:length(lambda_down))]
					r_down=dummy[1:((!equal_by_reg[7])*(n_regime-1)+1)]
					if(garch_variance)
					{
					return(list(u=u, a0=a0, a1=a1, b=b, P=P, pi_0=P,lambda_down=lambda_down,r_down=r_down))
					}
					else
					{
					return(list(u=u, a0=a0 , P=P,pi_0=P,lambda_down=lambda_down,r_down=r_down))
					}
					
			}else if(distIn=="gaussJump"){
					lambda_down=dummy[1:((!equal_by_reg[7])*(n_regime-1)+1)]
					dummy <-dummy[-(1:length(lambda_down))]
					r_down=dummy[1:((!equal_by_reg[7])*(n_regime-1)+1)]
					dummy <-dummy[-(1:length(r_down))]
					lambda_up=dummy[1:((!equal_by_reg[7])*(n_regime-1)+1)]
					dummy <-dummy[-(1:length(lambda_up))]
					r_up=dummy[1:((!equal_by_reg[7])*(n_regime-1)+1)]
					if(garch_variance)
					{
					return(list(u=u, a0=a0, a1=a1, b=b, P=P, pi_0=P,lambda_down=lambda_down,r_down=r_down,lambda_up=lambda_up,r_up=r_up))
					}
					else
					{
					return(list(u=u, a0=a0 , P=P,pi_0=P,lambda_down=lambda_down,r_down=r_down,lambda_up=lambda_up,r_up=r_up))
					}
					
			}else if(distIn=="Normal"){
					return(list(u=u, a0=a0, a1=a1, b=b, P=P, pi_0=P))
			}
			
			
	}
	
	
	}
	
	RSGARCH <- function(all_vars, n_regime=2, equal_by_reg=c(FALSE,FALSE,FALSE,FALSE),
						var_upd = c("gray", "klaassen", "rs"), mean_incl=TRUE,skew_incl=FALSE, init, h_0, logR,distIn="Normal",garch_variance=FALSE){
	
	n <- length(logR)
	params <- working_to_real__(all_vars, n_regime=n_regime, equal_by_reg=equal_by_reg, mean_incl=mean_incl,skew_incl=skew_incl, var_upd=var_upd,distIn=distIn,garch_variance=garch_variance)
	u <- params$u; a0 <- params$a0; P_ <- params$P; pi_0 <- params$pi_0;
	lambda_down<-params$lambda_down;r_down<-params$r_down;lambda_up<-params$lambda_up;r_up<-params$r_up;
	if(garch_variance)
	{
		a1 <- params$a1; b <- params$b
		sig2 <- a0 + a1*h_0 + b*h_0
	
	}else
	{
		sig2 <- a0 
	}
	
		nmax=4
		if(distIn=="Normal")
		{
			f_r_o <- pi_0 * dnorm(logR[1], mean = u, sd = sqrt(sig2))
		}else if(distIn=="Student"){
					df_<-params$df
					if(skew_incl)
					shape<-params$shape
					else
					shape<-rep(0,n_regime)
					f_r_o <-c(dst_((logR[1]-u[1])/sqrt(sig2[1]),shape=shape[1], df=df_[1]),
							dst_((logR[1]-u[2])/sqrt(sig2[2]),shape=shape[2], df=df_[2]))
					f_r_o <- 1/sqrt(sig2)* pi_0 * f_r_o
	
	}else if(distIn=="gaussJumpDown"){
	
			p1_down=dpois(1,lambda_down)
			integration_bound=30*sqrt(sig2)
			if(n_regime==1)
			{
					f_down =as.double(integrate(function(x) r_down*exp(r_down*x)*dnorm(logR[1],mean=(u+x) ,sd=sqrt(sig2)), -integration_bound, 0)[1])
			}else if(n_regime==2)
			{
					f_down =c(as.double(integrate(function(x) r_down[1]*exp(r_down[1]*x)*dnorm(logR[1],mean=(u[1]+x) ,sd=sqrt(sig2[1])), -integration_bound[1], 0)[1]),as.double(integrate(function(x) r_down[2]*exp(r_down[2]*x)*dnorm(logR[1],mean=(u[2]+x) ,sd=sqrt(sig2[2])), -integration_bound[2], 0)[1]))
			}
			
			p<-(1-p1_down)*dnorm(logR[1],mean=u ,sd=sqrt(sig2))+ p1_down*f_down
			f_r_o<-pi_0*p
	}else if(distIn=="gaussJump"){
	
			p1_down=dpois(1,lambda_down)
			p1_up  =dpois(1,lambda_up)
			integration_bound=30*sqrt(sig2)
			if(n_regime==1)
			{
					f_up   =as.double(integrate(function(x) r_up*exp(-r_up*x)*dnorm(logR[1],mean=(u+x) ,sd=sqrt(sig2)),0, integration_bound)[1])
					f_down =as.double(integrate(function(x) r_down*exp(r_down*x)*dnorm(logR[1],mean=(u+x) ,sd=sqrt(sig2)), -integration_bound, 0)[1])
			}else if(n_regime==2)
			{
					f_up =c(as.double(integrate(function(x) r_up[1]*exp(-r_up[1]*x)*dnorm(logR[1],mean=(u[1]+x) ,sd=sqrt(sig2[1])), 0,integration_bound[1])[1]),as.double(integrate(function(x) r_up[2]*exp(-r_up[2]*x)*dnorm(logR[1],mean=(u[2]+x) ,sd=sqrt(sig2[2])), 0,integration_bound[2])[1]))
					f_down =c(as.double(integrate(function(x) r_down[1]*exp(r_down[1]*x)*dnorm(logR[1],mean=(u[1]+x) ,sd=sqrt(sig2[1])), -integration_bound[1], 0)[1]),as.double(integrate(function(x) r_down[2]*exp(r_down[2]*x)*dnorm(logR[1],mean=(u[2]+x) ,sd=sqrt(sig2[2])), -integration_bound[2], 0)[1]))
			}
			
			p<-(1-p1_down-p1_up)*dnorm(logR[1],mean=u ,sd=sqrt(sig2))+ p1_down*f_down+p1_up*f_up
			f_r_o<-pi_0*p
	}
	
	
	likelihood <- sum(f_r_o)
	xi <- f_r_o / likelihood
	log_likelihood <- log(likelihood)
	
	#E is the expected return at t given information up to t-1
	E <- sum(pi_0 * u)
	#h is the "averaged" variance used to update GARCH variances in each of the regime
	h <- sum(pi_0 * (u^2 + sig2)) - E^2
	#eps is the "averaged" error from the mean used to update GARCH variances in each of the regime
	eps <- logR[1] - E
	#xi_ex P(S(t)|y(1)...y(t-1);theta)
	#xi P(S(t-1)|y(1)...y(t-1);theta)
	#f_r_o P(y(t)|y(1)...y(t-1);theta)=xi_ex * f_r_o
	for (i in 2:length(logR)){
			if(garch_variance)
			{
				sig2 <- a0 + a1*eps^2 + b*h
			}else{
				sig2 <- a0
			}
	
		if(distIn=="Normal")
		{		
			xi_ex <- xi %*% P_
			E <- sum(xi_ex * u)
			h <- sum(xi_ex * (u^2 + sig2)) - E^2
			eps <- logR[i] - E
			f_r_o <- xi_ex * dnorm(logR[i], mean = u, sd = sqrt(sig2))
		}else if(distIn=="Student"){
				xi_ex <- xi %*% P_
				E <- sum(xi_ex * u)
				h <- sum(xi_ex * (u^2 + sig2)) - E^2
				eps <- logR[i] - E
			f_r_o <-c(dst_((logR[i]-u[1])/sqrt(sig2[1]),shape=shape[1], df=df_[1]),
					dst_((logR[i]-u[2])/sqrt(sig2[2]),shape=shape[2], df=df_[2]))
			f_r_o <- 1/sqrt(sig2)*xi_ex * f_r_o
			}else if(distIn=="gaussJumpDown"){
				xi_ex <- xi %*% P_
				E <- sum(xi_ex * u)
				h <- sum(xi_ex * (u^2 + sig2) )- E^2
				eps <- logR[i] - E
				integration_bound=30*sqrt(sig2)	
				p1_down=dpois(1,lambda_down)
				if(n_regime==1)
				{
					f_down =as.double(integrate(function(x) r_down*exp(r_down*x)*dnorm(logR[i],mean=(u+x) ,sd=sqrt(sig2)), -integration_bound, 0)[1])
				}else if(n_regime==2)
				{
					f_down =c(as.double(integrate(function(x) r_down[1]*exp(r_down[1]*x)*dnorm(logR[i],mean=(u[1]+x) ,sd=sqrt(sig2[1])), -integration_bound[1], 0)[1]),as.double(integrate(function(x) r_down[2]*exp(r_down[2]*x)*dnorm(logR[i],mean=(u[2]+x) ,sd=sqrt(sig2[2])), -integration_bound[2], 0)[1]))
				} 
				p<-(1-p1_down)*dnorm(logR[i],mean=u ,sd=sqrt(sig2))+p1_down*f_down
				f_r_o<-pi_0*p
			}else if(distIn=="gaussJump"){
				xi_ex <- xi %*% P_
				E <- sum(xi_ex * u)
				h <- sum(xi_ex * (u^2 + sig2) )- E^2
				eps <- logR[i] - E
				p1_down=dpois(1,lambda_down)
				p1_up  =dpois(1,lambda_up)
				integration_bound=30*sqrt(sig2)
				if(n_regime==1)
				{
					f_up   =as.double(integrate(function(x) r_up*exp(-r_up*x)*dnorm(logR[i],mean=(u+x) ,sd=sqrt(sig2)),0, integration_bound)[1])
					f_down =as.double(integrate(function(x) r_down*exp(r_down*x)*dnorm(logR[i],mean=(u+x) ,sd=sqrt(sig2)), -integration_bound, 0)[1])
				}else if(n_regime==2)
				{
					f_up   =c(as.double(integrate(function(x) r_up[1]*exp(-r_up[1]*x)*dnorm(logR[i],mean=(u[1]+x) ,sd=sqrt(sig2[1])), 0,integration_bound[1])[1]),as.double(integrate(function(x) r_up[2]*exp(r_up[2]*x)*dnorm(logR[i],mean=(u[2]+x) ,sd=sqrt(sig2[2])), 0,integration_bound[2])[1]))
					f_down =c(as.double(integrate(function(x) r_down[1]*exp(r_down[1]*x)*dnorm(logR[i],mean=(u[1]+x) ,sd=sqrt(sig2[1])), -integration_bound[1], 0)[1]),as.double(integrate(function(x) r_down[2]*exp(r_down[2]*x)*dnorm(logR[i],mean=(u[2]+x) ,sd=sqrt(sig2[2])), -integration_bound[2], 0)[1]))
				} 
				p<-(1-p1_down-p1_up)*dnorm(logR[i],mean=u ,sd=sqrt(sig2))+ p1_down*f_down+p1_up*f_up
				f_r_o<-pi_0*p
			}
		
			likelihood <-  sum(f_r_o)
			xi <- f_r_o / likelihood
			log_likelihood <- log_likelihood + log(likelihood)
	
	}
	return(-log_likelihood)
	
	
	}
	
	#full output
	RSGARCH_full <- function(all_vars, n_regime=2, equal_by_reg=rep(FALSE,4),
							var_upd = c("gray", "klaassen", "rs"), mean_incl=TRUE,skew_incl=FALSE, init,
								h_0, logR,distIn="Normal",garch_variance=FALSE){
	
	n <- length(logR)
	params <- working_to_real__(all_vars, n_regime=n_regime, equal_by_reg=equal_by_reg, mean_incl=mean_incl,skew_incl=skew_incl, var_upd=var_upd,distIn=distIn,garch_variance=garch_variance)
	u <- params$u; a0 <- params$a0; P <- params$P; pi_0 <- params$pi_0;
	lambda_down<-params$lambda_down;r_down<-params$r_down;r_up<-params$r_up;lambda_up<-params$lambda_up
	if(garch_variance)
	{
	a1 <- params$a1; b <- params$b;
	a1 <- rep(a1, n_regime-length(a1)+1)
	b <- rep(b, n_regime-length(b)+1)
	sig2 <- rbind(a0 + a1*h_0 + b*h_0, matrix(0,nrow=n-1,ncol=n_regime))
	
	}
	else{
	sig2 <- rbind(a0 , matrix(0,nrow=n-1,ncol=n_regime))
	}
	# if(init!="stationary") pi_0 <- P[init,]
	
	u <- rep(u, n_regime-length(u)+1)
	a0 <- rep(a0, n_regime-length(a0)+1)
	
	eta<-matrix(0,nrow=n,ncol=n_regime)
	#eta contains the density of the observation for every given regime
	if(distIn=="Normal")
		{
		eta [1,]<- dnorm(logR[1], mean = u, sd = sqrt(sig2[1,]))
		}	else if(distIn=="Student"){
		
		df_<-params$df
		if(skew_incl)
					shape<-params$shape
		else
					shape<-rep(0,n_regime)
	
		
		eta [1,]<- c( dst_((logR[1]-u[1])/sqrt(sig2[1,1]),shape=shape[1], df=df_[1]) ,
						dst_((logR[1]-u[2])/sqrt(sig2[1,2]),shape=shape[2], df=df_[2]) )
		eta [1,]<- 1/sqrt(sig2[1,])*eta [1,]
	
		}else if(distIn=="gaussJumpDown"){
			p1_down=dpois(1,lambda_down)
			integration_bound=30*sqrt(sig2)
			if(n_regime==1)
			{
				f_down =as.double(integrate(function(x) r_down*exp(r_down*x)*dnorm(logR[1],mean=(u+x) ,sd=sqrt(sig2[1,1])), -integration_bound, 0)[1])
			}else if(n_regime==2)
			{		    
				f_down =c(as.double(integrate(function(x) r_down[1]*exp(r_down[1]*x)*dnorm(logR[1],mean=(u[1]+x) ,sd=sqrt(sig2[1,1])), -integration_bound[1], 0)[1]),as.double(integrate(function(x) r_down[2]*exp(r_down[2]*x)*dnorm(logR[1],mean=(u[2]+x) ,sd=sqrt(sig2[1,2])), -integration_bound[2], 0)[1]))
			}		 
			eta [1,]<-(1-p1_down)*dnorm(logR[1],mean=u ,sd=sqrt(sig2[1,]))+ p1_down*f_down		  
		}else if(distIn=="gaussJump"){
			p1_down=dpois(1,lambda_down)
			p1_up  =dpois(1,lambda_up)
			integration_bound=30*sqrt(sig2)
			if(n_regime==1)
			{
				f_up   =as.double(integrate(function(x) r_up*exp(-r_up*x)*dnorm(logR[1],mean=(u+x) ,sd=sqrt(sig2[1,1])),0, integration_bound)[1])
				f_down =as.double(integrate(function(x) r_down*exp(r_down*x)*dnorm(logR[1],mean=(u+x) ,sd=sqrt(sig2[1,1])), -integration_bound, 0)[1])
			}else if(n_regime==2)
			{
				f_up =c(as.double(integrate(function(x) r_up[1]*exp(-r_up[1]*x)*dnorm(logR[1],mean=(u[1]+x) ,sd=sqrt(sig2[1,1])), 0,integration_bound[1])[1]),as.double(integrate(function(x) r_up[2]*exp(-r_up[2]*x)*dnorm(logR[1],mean=(u[2]+x) ,sd=sqrt(sig2[1,2])),0, integration_bound[2])[1]))		 
				f_down =c(as.double(integrate(function(x) r_down[1]*exp(r_down[1]*x)*dnorm(logR[1],mean=(u[1]+x) ,sd=sqrt(sig2[1,1])), -integration_bound[1], 0)[1]),as.double(integrate(function(x) r_down[2]*exp(r_down[2]*x)*dnorm(logR[1],mean=(u[2]+x) ,sd=sqrt(sig2[1,2])), -integration_bound[2], 0)[1]))
			}		 
			eta [1,]<-(1-p1_down-p1_up)*dnorm(logR[1],mean=u ,sd=sqrt(sig2[1,]))+ p1_down*f_down+ p1_up*f_up		  
		}
	
		
		
	#f_r_o denotes the density of regime and observation at t given
	#data up to t-1 (this is a vector of length n_regime)
	#i.e. P(C[t] = i ; x[t] | information up to t-1)
	f_r_o <- pi_0 * eta [1,]
	
	#likelihood_c contains the components of the likelihood
	#i.e. likelihood_c[i] = f(x[t] | information up to t-1)
	likelihood_c <- c(sum(f_r_o), rep(0,n-1))
	
	#xi contains the filter probability i.e. probability of being in a regime at t
	#given data up to t
	xi <- rbind(f_r_o / likelihood_c[1], matrix(0,nrow=n-1,ncol=n_regime))
	
	#xi_ex is the ex-ante probability i.e. probability of being in a regime at t
	#given data up to t-1; P(C[t] = i | information up to t-1)
	xi_ex <- rbind(c(pi_0), matrix(0,nrow=n-1,ncol=n_regime))
	
	if(var_upd=="gray" | var_upd=="rs"){
		#h is the "averaged" variance used to update GARCH variances in each of the regime
		h <- numeric(n)
		E <- sum(xi_ex[1,] * u) #E is the expected return at t given information up to t-1
		h[1] <- sum(xi_ex[1,] * (u^2 + sig2[1,])) - E^2
		eps <- logR[1] - E  #eps is the "averaged" error from the mean used to update GARCH variances in each of the regime
	
		for (i in 2:n){
		
				if(garch_variance)
				{
				sig2[i,] <- a0 + a1*eps^2 + b*h[i-1]
				}else{
				sig2[i,] <- a0 
				}
	
				xi_ex[i,] <- xi[i-1,] %*% P
				
					if(distIn=="Normal")
					{
						eta [i,] <- dnorm(logR[i], mean = u, sd = sqrt(sig2[i,]))				
						E <- sum(xi_ex[i,] * u)
						h[i] <- sum(xi_ex[i,] * (u^2 + sig2[i,])) - E^2
						eps <- logR[i] - E
					}else if(distIn=="Student"){
	
						eta [i,]<- c( dst_((logR[i]-u[1])/sqrt(sig2[i,1]),shape=shape[1], df=df_[1]) ,
						dst_((logR[i]-u[2])/sqrt(sig2[i,2]),shape=shape[2], df=df_[2]) )
						eta [i,]<- 1/sqrt(sig2[i,])*eta [i,]
	
						E <- sum(xi_ex[i,] * u)
						h[i] <- sum(xi_ex[i,] * (u^2 + sig2[i,])) - E^2
						eps <- logR[i] - E
						
					}else if(distIn=="gaussJumpDown"){
						p1_down=dpois(1,lambda_down)
						integration_bound=30*sqrt(sig2)
						if(n_regime==1)
						{
							f_down =as.double(integrate(function(x) r_down*exp(r_down*x)*dnorm(logR[i],mean=(u+x) ,sd=sqrt(sig2[i,1])), -integration_bound, 0)[1])
						}else if(n_regime==2)
						{
							f_down =c(as.double(integrate(function(x) r_down[1]*exp(r_down[1]*x)*dnorm(logR[i],mean=(u[1]+x) ,sd=sqrt(sig2[i,1])), -integration_bound[1], 0)[1]),as.double(integrate(function(x) r_down[2]*exp(r_down[2]*x)*dnorm(logR[i],mean=(u[2]+x) ,sd=sqrt(sig2[i,2])), -integration_bound[2], 0)[1]))
						} 
						eta [i,] <-(1-p1_down)*dnorm(logR[i],mean=u ,sd=sqrt(sig2[i,]))+ p1_down*f_down
	
						# E <- sum(xi_ex[i,] * (u+lambda*theta))
						# h[i] <- sum(xi_ex[i,] * (u^2 + sig2[i,]+lambda*delta^2+lambda^2*delta^2+lambda*theta^2)) - E^2
						E <- sum(xi_ex[i,] * u)
						h[i] <- sum(xi_ex[i,] * (u^2 + sig2[i,]) )- E^2
						eps <- logR[i] - E	  
					}else if(distIn=="gaussJump"){
							p1_down=dpois(1,lambda_down)
							p1_up  =dpois(1,lambda_up)
							integration_bound=30*sqrt(sig2)
							if(n_regime==1)
							{
								f_up   =as.double(integrate(function(x) r_up*exp(-r_up*x)*dnorm(logR[i],mean=(u+x) ,sd=sqrt(sig2[i,1])),0, integration_bound)[1])
								f_down =as.double(integrate(function(x) r_down*exp(r_down*x)*dnorm(logR[i],mean=(u+x) ,sd=sqrt(sig2[i,1])), -integration_bound, 0)[1])
							}else if(n_regime==2)
							{
								f_up =c(as.double(integrate(function(x) r_up[1]*exp(-r_up[1]*x)*dnorm(logR[i],mean=(u[1]+x) ,sd=sqrt(sig2[i,1])), 0,integration_bound[1])[1]),as.double(integrate(function(x) r_up[2]*exp(-r_up[2]*x)*dnorm(logR[i],mean=(u[2]+x) ,sd=sqrt(sig2[1,2])),0, integration_bound[2])[1]))		 
								f_down =c(as.double(integrate(function(x) r_down[1]*exp(r_down[1]*x)*dnorm(logR[i],mean=(u[1]+x) ,sd=sqrt(sig2[i,1])), -integration_bound[1], 0)[1]),as.double(integrate(function(x) r_down[2]*exp(r_down[2]*x)*dnorm(logR[i],mean=(u[2]+x) ,sd=sqrt(sig2[1,2])), -integration_bound[2], 0)[1]))
							}		 
							eta [i,]<-(1-p1_down-p1_up)*dnorm(logR[i],mean=u ,sd=sqrt(sig2[i,]))+ p1_down*f_down+ p1_up*f_up		  
					}
					f_r_o <- xi_ex[i,] * eta [i,]
					likelihood_c[i] <-  sum(f_r_o)
					xi[i,] <- f_r_o / likelihood_c[i]
	
		}
	}
	log_likelihood <- sum(log(likelihood_c))
	
	#we now compute the smoothed probabilities by the algorithm developed by Kim
	#see Hamilton (1994). Time Series Analysis. Chapter 22 - p. 694
	#see also Hidden Markov Models for Time Series: An Introduction Using R p. 81
	
	xi_smooth <- rbind(matrix(0,nrow=n-1,ncol=n_regime), xi[n,])
	for(i in (n-1):1) xi_smooth[i,] <- xi[i,] * (P %*% (xi_smooth[i+1,] / xi_ex[i+1,]))
	
	#analysis of residuals
	#residuals by regime
	res_by_reg <- matrix((rep(logR, each=n_regime) - u) / t(sqrt(sig2)), ncol=n_regime, byrow=TRUE)
	#weighted residuals
	res_weighted <- apply(res_by_reg * xi_smooth, MARGIN=1, FUN=sum)
	#0-1 residuals
	res_01 <- sapply(1:n, function(z) res_by_reg[z, which.max(xi_smooth[z,])])
	#F residuals
	res_F <- qnorm(apply(pnorm(res_by_reg) * xi_ex, MARGIN=1, FUN=sum))
	res_F[res_F==Inf] <- 8 #to avoid Inf
	res_F[res_F==-Inf] <- -8
	
	return(list(loglik = log_likelihood,
				params = params,
				h = h,
				sig2 = sig2,
				lik_c = likelihood_c,
				p.ex_ante = xi_ex,
				p.filter = xi,
				p.smooth = xi_smooth,
				res.by.reg = res_by_reg,
				res.weighted = res_weighted,
				res.01 = res_01,
				res.F = res_F,eta=eta)
				)
	}
	
	Viterbi <- function (Pi0,P,eta){
		# dfunc <- makedensity(object$distn)
		#initialisation
		n <- length(eta[,1])
		m <- nrow(P)
		nu <- matrix(NA, nrow = n, ncol = m)
		y <- rep(NA, n)
	
		nu[1, ] <- log(Pi0)+ log(eta[1,])
		logPi <- log(t(P))
		#recursion
		for (i in 2:n) {
	
			matrixnu <- matrix(nu[i - 1, ], nrow = m, ncol = m)
			nu[i, ] <- apply(matrixnu + logPi, 2, max) +
						log(eta[i,])
		}
		if (any(nu[n, ] == -Inf)) 
			stop("Problems With Underflow")		
		#evaluation du meilleur score (chemin le plus probable)
		y[n] <- which.max(nu[n, ])
		for (i in seq(n - 1, 1, -1)) y[i] <- which.max(logPi[, y[i + 1]] + nu[i, ])
		return(y-1)
	}
	
	
	EmergingCrisisList<-function(EmergingBasket,maxDropDownTresh,minimumDuration)
	{
			EmergingCrisisList <- NULL
			crisisStates =EmergingBasket$EM_basket_crisis
			returns      =EmergingBasket$EM_basket_returns
			dates        =EmergingBasket$EM_basket_dates
			xx           =diff(crisisStates)
			crisis_begin = which(xx==1)+1
			crisis_end   = which(xx==-1)
			if(crisisStates[1]==1) crisis_begin<-c(1,crisis_begin)
			if(crisisStates[length(crisisStates)]==1) crisis_end <-c(crisis_end,length(crisisStates))
			nb_crisis <-0
			crisis_length <-0
			serie_length<-length(crisisStates)
			for(i in c(1:length(crisis_begin)))
			{
				I=c(crisis_begin[i]:crisis_end[i])
				if((length(I)>= minimumDuration))
				{
					twenty_day_max_drop_down <- max_drop_down_returns(returns[I],20)
					if(twenty_day_max_drop_down< maxDropDownTresh)
					{
						EmergingCrisis<-data.frame(FX="Emerging Basket",crisis_dates=paste(dates[I[1]],dates[I[length(I)]],sep='->'),
						twenty_day_max_drop_down=twenty_day_max_drop_down,sigma=sd(returns[I])*sqrt(250),skew=skewness(returns[I]))				
						EmergingCrisisList<-rbind(EmergingCrisisList,  EmergingCrisis)			
					}else
					{
						EmergingBasket$EM_basket_crisis[I] <- 0
					}
				}else
				{
					EmergingBasket$EM_basket_crisis[I] <- 0
				}				
			}
			return(list(EmergingCrisisList=EmergingCrisisList,states=EmergingBasket$EM_basket_crisis))
		
	}
	
	CrisisFiltering_and_Statisitics<-function(FX,FX_ratings_i,udlName,minimumDuration,rdtClusters,trace=1)
	{
			crisisStates 	=FX_ratings_i$crisis
			spot        	=FX_ratings_i$spot
			crisis_theo_list =list()
			crisis_histo_list=list()
			xx           	=diff(crisisStates)
			crisis_begin = which(xx==1)+1
			crisis_end   = which(xx==-1)
			if(crisisStates[1]==1) crisis_begin<-c(1,crisis_begin)
			if(crisisStates[length(crisisStates)]==1) crisis_end <-c(crisis_end,length(crisisStates))
			nb_crisis     <-0
			crisis_length <-0
			serie_length  <-length(crisisStates)
			for(i in c(1:length(crisis_begin)))
			{
				I=c(crisis_begin[i]:crisis_end[i])
				if((length(I)>= minimumDuration))
				{
				
					rdt_crisis= FX_ratings_i$rdt[I]
					spot      = FX_ratings_i$spot[c(I,tail(I,n=1)+1)]
					twenty_day_max_drop_down <- max_drop_down(spot,20)
					twenty_day_max_drop_up   <- max_drop_up(spot,20)
					ratings <- max(FX_ratings_i$rating[I[1]],FX_ratings_i$rating[I[length(I)]])
					drop_down_threshold <- if(ratings>5) -0.1 else -0.05	
					if(twenty_day_max_drop_down < drop_down_threshold)
					{
	
						# historical Statistics
						crisis_histo<-data.frame(FX=FX,crisis_dates=paste(FX_ratings_i$dates[I[1]],FX_ratings_i$dates[I[length(I)]],sep='->'),ratings= ratings ,
						twenty_day_max_drop_down=twenty_day_max_drop_down,twenty_day_max_drop_up=twenty_day_max_drop_up,sigma=sd(rdt_crisis)*sqrt(250),skew=skewness(rdt_crisis))
						# theoretical Statistics
						JumpModel   <-fit_jump_down(rdt_crisis,randomStart=TRUE,trace=1)		
						mu     	    <- JumpModel$mu 
						sigma       <- sqrt(JumpModel$sig2*250)
						r_down      <- JumpModel$r_down
						lambda_down <- 0.03287671
						theo_quantiles        <- min_max_average_return2(mu,sigma,lambda_down,r_down,20,0.01)
						var_skew_res           <-var_skew2(mu,sigma,lambda_down,r_down)
						q01_theo              <-theo_quantiles[1]
						average_theo          <-theo_quantiles[2]
						q99_theo              <-theo_quantiles[3]
						crisis_theo<-data.frame(FX=FX,crisis_dates=paste(FX_ratings_i$dates[I[1]],FX_ratings_i$dates[I[length(I)]],sep='->'),ratings= ratings ,mu=mu,sigma=sigma,lambda_down=lambda_down,r_down=r_down,
						q01_theo=q01_theo,q99_theo=q99_theo,average_theo=average_theo,sigma=var_skew_res[1],skew=var_skew_res[2])
						#update cluster returns 	   
						if( ratings <= 5 )
							rdtClusters$rdt_cluster_1_5  <- c(rdtClusters$rdt_cluster_1_5,rdt_crisis)
						else if ( (ratings >= 6) & (ratings <= 14))
							rdtClusters$rdt_cluster_6_14  <- c(rdtClusters$rdt_cluster_6_14,rdt_crisis)
						else if (ratings >= 15 )
							rdtClusters$rdt_cluster_15_20 <- c(rdtClusters$rdt_cluster_15_20,rdt_crisis)
					if(trace)cat(udlName,"::crisis",i,"kept\n")
					
					}
					else
					{
						
						if(trace)cat(udlName,"::crisis",i,"discarded, drawDown Filter::twenty_day_max_drop_down",twenty_day_max_drop_down,"drop_down_threshold:",drop_down_threshold,"\n")
						FX_ratings_i$crisis[I] <- 0
						crisis_histo<-NULL
						crisis_theo<-NULL
					}
				}
				else
				{
						if(trace)cat(udlName,"::crisis",i,"discarded, duration Filter::crisis duration",length(I),"minimumDuration",minimumDuration,"\n")
						FX_ratings_i$crisis[I] <- 0
						crisis_histo<-NULL
						crisis_theo<-NULL
				}	
				crisis_theo_list<-rbind(crisis_theo_list,crisis_theo)
				crisis_histo_list<-rbind(crisis_histo_list,crisis_histo)
				
			}
			return(list(EmergingUnderlying=FX_ratings_i,rdtClusters=rdtClusters,crisis_theo=crisis_theo_list,crisis_histo=crisis_histo_list))
		
	}
	
	CrisisFiltering_and_Statisitics_RecordAll <-function(FX,FX_ratings_i,udlName,minimumDuration,rdtClusters,trace=1)
	{
	  crisisStates 	=FX_ratings_i$crisis
	  spot        	=FX_ratings_i$spot
	  crisis_theo_list =list()
	  crisis_histo_list=list()
    keep_all <- data.frame() # MODIF PELDV
	  xx           	=diff(crisisStates)
	  crisis_begin = which(xx==1)+1
	  crisis_end   = which(xx==-1)
	  if(crisisStates[1]==1) crisis_begin<-c(1,crisis_begin)
	  if(crisisStates[length(crisisStates)]==1) crisis_end <-c(crisis_end,length(crisisStates))
	  nb_crisis     <-0
	  crisis_length <-0
	  serie_length  <-length(crisisStates)
	  for(i in c(1:length(crisis_begin)))
	  {
	    I=c(crisis_begin[i]:crisis_end[i])
	    if((length(I)>= minimumDuration))
	    {
	      
	      rdt_crisis= FX_ratings_i$rdt[I]
	      spot      = FX_ratings_i$spot[c(I,tail(I,n=1)+1)]
	      twenty_day_max_drop_down <- max_drop_down(spot,20)
	      twenty_day_max_drop_up   <- max_drop_up(spot,20)
	      ratings <- max(FX_ratings_i$rating[I[1]],FX_ratings_i$rating[I[length(I)]])
	      drop_down_threshold <- if(ratings>5) -0.1 else -0.05	
	      if(twenty_day_max_drop_down < drop_down_threshold)
	      {
	        
	        # historical Statistics
	        crisis_histo<-data.frame(FX=FX,crisis_dates=paste(FX_ratings_i$dates[I[1]],FX_ratings_i$dates[I[length(I)]],sep='->'),ratings= ratings ,
	                                 twenty_day_max_drop_down=twenty_day_max_drop_down,twenty_day_max_drop_up=twenty_day_max_drop_up,sigma=sd(rdt_crisis)*sqrt(250),skew=skewness(rdt_crisis))
	        # theoretical Statistics
	        JumpModel   <-fit_jump_down(rdt_crisis,randomStart=TRUE,trace=1)		
	        mu     	    <- JumpModel$mu 
	        sigma       <- sqrt(JumpModel$sig2*250)
	        r_down      <- JumpModel$r_down
	        lambda_down <- 0.03287671
	        theo_quantiles        <- min_max_average_return2(mu,sigma,lambda_down,r_down,20,0.01)
	        var_skew_res           <-var_skew2(mu,sigma,lambda_down,r_down)
	        q01_theo              <-theo_quantiles[1]
	        average_theo          <-theo_quantiles[2]
	        q99_theo              <-theo_quantiles[3]
	        crisis_theo<-data.frame(FX=FX,crisis_dates=paste(FX_ratings_i$dates[I[1]],FX_ratings_i$dates[I[length(I)]],sep='->'),ratings= ratings ,mu=mu,sigma=sigma,lambda_down=lambda_down,r_down=r_down,
	                                q01_theo=q01_theo,q99_theo=q99_theo,average_theo=average_theo,sigma=var_skew_res[1],skew=var_skew_res[2])
	        #update cluster returns 	   
	        if( ratings <= 5 )
	          rdtClusters$rdt_cluster_1_5  <- c(rdtClusters$rdt_cluster_1_5,rdt_crisis)
	        else if ( (ratings >= 6) & (ratings <= 14))
	          rdtClusters$rdt_cluster_6_14  <- c(rdtClusters$rdt_cluster_6_14,rdt_crisis)
	        else if (ratings >= 15 )
	          rdtClusters$rdt_cluster_15_20 <- c(rdtClusters$rdt_cluster_15_20,rdt_crisis)
	        if(trace)cat(udlName,"::crisis",i,"kept\n")
	        
	      }
	      else
	      {
	        
	        if(trace)cat(udlName,"::crisis",i,"discarded, drawDown Filter::twenty_day_max_drop_down",twenty_day_max_drop_down,"drop_down_threshold:",drop_down_threshold,"\n")
	        FX_ratings_i$crisis[I] <- 0
	        crisis_histo<-NULL
	        crisis_theo<-NULL
	      }
	    }
	    else
	    {
	      if(trace)cat(udlName,"::crisis",i,"discarded, duration Filter::crisis duration",length(I),"minimumDuration",minimumDuration,"\n")
	      FX_ratings_i$crisis[I] <- 0
	      crisis_histo<-NULL
	      crisis_theo<-NULL
	    }	
	    crisis_theo_list<-rbind(crisis_theo_list,crisis_theo)
	    crisis_histo_list<-rbind(crisis_histo_list,crisis_histo)
	    keep_all <- rbind(keep_all, 
                        data.frame(
	      ccy=udlName,
        start=crisis_begin[i],
        end=crisis_end[i],
        duration=length(I),
        maxDropDown=twenty_day_max_drop_down,
        maxDropUp=twenty_day_max_drop_up,
        kept=((length(I)>= minimumDuration) & 
          (twenty_day_max_drop_down < drop_down_threshold))))
	  }
	  return(list(EmergingUnderlying=FX_ratings_i,
                rdtClusters=rdtClusters,
                crisis_theo=crisis_theo_list,
                crisis_histo=crisis_histo_list,
                keep_all=keep_all))
	  
	}
	
	FilterCrisis<-function(EmergingUnderlying,maxDropDownTresh,minimumDuration)
	{
			EmergingCrisisList <- NULL
			crisisStates =EmergingUnderlying$crisis
			returns      =EmergingUnderlying$returns
			dates        =EmergingUnderlying$dates
			xx           =diff(crisisStates)
			crisis_begin = which(xx==1)+1
			crisis_end   = which(xx==-1)
			if(crisisStates[1]==1) crisis_begin<-c(1,crisis_begin)
			if(crisisStates[length(crisisStates)]==1) crisis_end <-c(crisis_end,length(crisisStates))
			nb_crisis <-0
			crisis_length <-0
			serie_length<-length(crisisStates)
			for(i in c(1:length(crisis_begin)))
			{
				I=c(crisis_begin[i]:crisis_end[i])
				if((length(I)>= minimumDuration))
				{
					twenty_day_max_drop_down <- max_drop_down_returns(returns[I],20)
					if(twenty_day_max_drop_down< maxDropDownTresh)
					{
						EmergingCrisis<-data.frame(FX="Emerging Basket",crisis_dates=paste(dates[I[1]],dates[I[length(I)]],sep='->'),
						twenty_day_max_drop_down=twenty_day_max_drop_down,sigma=sd(returns[I])*sqrt(250),skew=skewness(returns[I]))				
						EmergingCrisisList<-rbind(EmergingCrisisList,  EmergingCrisis)			
					}else
					{
						EmergingUnderlying$crisis[I] <- 0
					}
				}else
				{
					EmergingUnderlying$crisis[I] <- 0
				}				
			}
			return(list(EmergingCrisisList=EmergingCrisisList,states=EmergingUnderlying$crisis))
		
	}
	
	crisisDetection<-function(underlying,histo)
	{
		spot     <- histo$spot
		dates    <- histo$Date
		dates    <- dates[!is.na(spot)]
		spot     <- spot[!is.na(spot)]
		rdt       =diff(log(spot),na.rm=FALSE)
		#compute returns 
		dates   <-dates[!is.na(rdt)]
		rdt     <-rdt[!is.na(rdt)]
		
		dep=as.matrix(rdt)
		tol <- 1e-8  
		rel_tol  <- 1e-12	  
		myModel_student<-RSGARCH_Fit(dep,n_regime=2,equal_by_reg = c(FALSE,FALSE,TRUE,TRUE,FALSE,FALSE,FALSE),mean_incl=TRUE,skew_incl=FALSE,pars0=c(0,var(rdt),0.001,0.0001),distIn="Student",garch_variance=FALSE,underlying)
		coefStudent<- myModel_student@Coeff
		coeff<-coefStudent	  
		if(	coeff$a0[1] >  coeff$a0[2])
		{        
			crisisStates=1-myModel_student@viterbiStates
		}else{
			crisisStates=myModel_student@viterbiStates
		}
		EmergingUnderlying=list(crisis=crisisStates,returns=rdt,dates=dates[-1])
		EM_crisis=FilterCrisis(EmergingUnderlying,maxDropDownTresh=-0.1,minimumDuration=20)$states
		mytitle=paste(underlying,":Detection des régimes de crises")  
		rdtTS <- timeSeries( rdt,dates[-1])
		spotTS<- timeSeries( spot[-1],dates[-1])
		StateTS <- timeSeries(EM_crisis,dates[-1])
		################ plot crisis detection output ##########################
		par(mar=c(2,2,2,2)+.1)	 
		plot(rdtTS,type="l",col="black",main=mytitle,ylim=c(-0.15,0.15)) 
		par(new=TRUE)
		plot(spotTS,type="l",col="blue",xaxt="n",yaxt="n",xlab="",ylab="")
		par(new=TRUE)
		plot(StateTS,type="l",col="red",xaxt="n",yaxt="n",xlab="",ylab="")
		axis(4)
		mtext("state",side=5,line=1)
		legend("topright",col=c("black","red","blue"),lty=1,legend=c("Rdt","state","spot"))
	
	}
	}
	
	format_Correl_Matrix<-function(underlyings,correlationMatrix)
{
	corr_udl <- NULL
	for(i in c(1:length(underlyings)))
	{
		for(j in c(1:length(underlyings)))
		{
			corr_udl   <-rbind(corr_udl,paste(underlyings[i],",",underlyings[j],",",correlationMatrix[i,j],sep=""))
		}	
	}
	return (corr_udl)	
}

calibrate_FX_Model_old <-function(resultPath,dataPath,basePath,CalibrationName,CalibrationWindow)
{
#Variable initialisation
{
        nb_data<-NULL
		T_ = 1
		histoDev = read.csv(paste(dataPath,"FX_data_calib.csv",sep=""), sep=";" , header=T)
		devises_all= names(histoDev)[-1]
		dates_dev  <- as.Date(as.character(histoDev$date),format = "%d/%m/%y")
		fx_1J_EMERGING      =	read.csv(paste(dataPath,"Histo_FX_Table_Emerging.csv",sep=""),sep=";",header=TRUE)
		CountryRatingTable  =   read.csv(paste(dataPath,"Histo_Country_Rating_Table_Emerging.csv",sep=""),sep=";",header=TRUE)
		dates_debut=read.csv(paste(dataPath,"FX_Dates.csv",sep=""),sep=";",header=TRUE)
		dates_debut <-dates_debut$Date
		paramStudent<- c("nom devise","mu0","Vol0","df0","mu1","vol1","df1","p0","p1","LogLik")
		filename="CrisisDetectionOutput.pdf"
		mypath=file.path(resultPath, filename)
		pdf(file = mypath)
		peggCurrencies<-c("CNY","CNH","AED","HKD","SAR","KWD","BGN")
		par(mfrow=c(2,1))
		dates  <- as.Date(as.character(fx_1J_EMERGING$Date),format = "%d/%m/%y")
		histo <- as.matrix(fx_1J_EMERGING[,-1])
		devises <-names(fx_1J_EMERGING)[-1]
		fxNames=names(fx_1J_EMERGING[1,-1])
		mean_incl <- TRUE
		skew_incl <- FALSE
		equal_by_reg <- c(FALSE,FALSE,TRUE,TRUE,FALSE,FALSE,FALSE)
		init_reg <- init <- 1 
		params_init <- "gray" 
		sign_loglik <- -1
		do_loglik <- TRUE
		n_regime <- 2  
		tol 	 <- 1e-8       #Gibbs sampler tolerance for stopping computation of sig2
		emerging_currencies=names(fx_1J_EMERGING)[-1]
		crisis_histo_list <-list()
		crisis_histo_transition_prob_list<-list()
		crisis_theo_list  <-list()
		nb_crisis_all <-0
		crisis_length_all <-0
		serie_length_all <-0
		EM_basket   <-list(EM_basket_dates=NULL,EM_basket_crisis=NULL,EM_basket_returns=NULL,EM_basket_composition=NULL)
		set.seed(4)
		
}			
#Crisis Detection Emerging Currencies  
{ 

	date_end       <- as.Date(CalibrationWindow$date_fin)
	date_init_all  <- as.Date(CalibrationWindow$date_debut)
    #Contagion risk  and Emerging Crisis 
	EM_basket$EM_basket_dates  <- as.character(seq(as.Date(CalibrationWindow$date_debut),as.Date(CalibrationWindow$date_fin),1))
	iWE<-isWeekday( EM_basket$EM_basket_dates,wday = 1:5)
	EM_basket$EM_basket_dates		<-EM_basket$EM_basket_dates [iWE]
	EM_basket$EM_basket_crisis 		<- rep(0,length(EM_basket$EM_basket_dates))
	EM_basket$EM_basket_returns 	<- rep(0,length(EM_basket$EM_basket_dates))
	EM_basket$EM_basket_composition <- rep(0,length(EM_basket$EM_basket_dates))
	currency_crisis<-list()
	FX_InCrisis    <-list()
	FX_InCrisis$FxNames<-emerging_currencies
	FX_InCrisis$states <-rep(0,length(emerging_currencies))
	rdtClusters<-list()
	crisis_results<-NULL
	vol_regime_normal<-NULL
	vol_fract_std<-NULL
   for (k in c(1:length(emerging_currencies)))
   {
	  histo_i <- histo[,k]
	  dates_I<-dates[!is.na(histo_i)]
	  histo_i<- histo_i[!is.na(histo_i)]
	  nb_data<-c(nb_data,length(histo_i))
	  I_T=seq(1,length(histo_i),T_)
	  histo_i_T<-histo_i[I_T]
	  dates_I<-dates_I[I_T]
	  rdtALL     <-  diff(log(histo_i_T),na.rm=FALSE)
	  rdtDates<-dates_I[-1]
	  rdtDates<-rdtDates[!is.na(rdtALL)]
	  rdtALL <-rdtALL[!is.na(rdtALL)]
	  if( !is.na(dates_debut[k]))
	  {
		date_init <- as.Date(as.character(dates_debut[k]),format = "%d/%m/%Y")
	  }else{ 
	    date_init <- CalibrationWindow$date_debut
	  }
	  I <- which((rdtDates >= date_init) & (rdtDates <= date_end) )
	  FxSpot <- histo_i[I]
	  spotDates<-dates_I[I]
	  dates_calibration <- rdtDates[I]
	  y=rdtALL[I]
	  nomDevise=fxNames[k]
      ###################### EM Basket returns #######################
	  currency_i_crisis<-list()
	  currency_i_crisis$dates<-as.character(seq(as.Date(date_init),as.Date(date_end),1))
	  currency_i_crisis$dates<-currency_i_crisis$dates[isWeekday(currency_i_crisis$dates,wday = 1:5)]
	  currency_i_crisis$states<-rep(0,length(currency_i_crisis$dates))
	  II<-match(as.character(rdtDates[I]),as.character(EM_basket$EM_basket_dates))
	  EM_basket$EM_basket_returns[II]     <- EM_basket$EM_basket_returns[II] + rdtALL[I]
	  EM_basket$EM_basket_composition[II] <- EM_basket$EM_basket_composition[II] +1
	 
	 ######################historical graphic########################
	  par(mfcol=c(2,1))
	  fenetre_calibration <- rep(0,length(rdtALL))
	  fenetre_calibration[I]<-1
	  rdtTS <- timeSeries( rdtALL,rdtDates)
	  mytitle=paste(fxNames[k],":Graphe des rdts et Fenetre de calibration ")
	  plot(rdtTS,type="l",col="blue",main=mytitle,cex.main=0.7)
	  par(new=TRUE)
	  plot(rdtDates, fenetre_calibration,type="l",col="black",xaxt="n",yaxt="n",xlab="",ylab="", ylim=c(0,1))
	  axis(4,ylim=c(0,1))
	  ##################Crisis Detection Algorithm##############	 
	  dep=as.matrix(y)
	  logR <- y
	  pars0<-c(0,var(logR),0.001,0.0001)
	  cat(format(Sys.time(), "%a %b %d %X %Y"),"::",round(100*k/length(emerging_currencies)),"% complete... \n")
      myModel_student<-RSGARCH_Fit(dep,n_regime=2,equal_by_reg,mean_incl,skew_incl,pars0,distIn="Student",garch_variance=FALSE,nomDevise)
	  coefStudent<- myModel_student@Coeff
	  coeff<-coefStudent	  
	  if(	coeff$a0[1] >  coeff$a0[2]){        
		  paramStudent=rbind(paramStudent,c(nomDevise,coeff$u[2],sqrt(coeff$a0[2]*250/T_),coeff$df[2],
								         coeff$u[1],sqrt(coeff$a0[1]*250/T_),coeff$df[1],
										 coeff$P[2,2],coeff$P[1,1],myModel_student@LL))

		   crisisStates_Student=1-myModel_student@viterbiStates
		   probCrisStates_Student<-myModel_student@smoothProb[,1]
	    }else{k
		  paramStudent=rbind(paramStudent,c(nomDevise,coeff$u[1],sqrt(coeff$a0[1]*250/T_),coeff$df[1],
													  coeff$u[2],sqrt(coeff$a0[2]*250/T_),coeff$df[2],
													  coeff$P[1,1],coeff$P[2,2],myModel_student@LL))
         # probCrisStates<-myModel@smoothProb[,2]
		 crisisStates_Student=myModel_student@viterbiStates
		 probCrisStates_Student<-myModel_student@smoothProb[,2]
	   }
		############################ Crisis rating #######################
		FX=emerging_currencies[k]
		fx_rating=getRating(CountryRatingTable,FX)
		ratings_i=data.frame(dates=fx_rating$dates,rating=fx_rating$ratings)
		# krw_TS=timeSeries(krw_rating$ratings,krw_rating$dates)
		FX_i <-  data.frame(dates=dates_calibration,rdt = y,crisis=crisisStates_Student,spot=FxSpot)
		FX_ratings_i <-merge(FX_i,ratings_i,by="dates")
        ##################### crisis list ##########################
		crisis_results=CrisisFiltering_and_Statisitics(FX,FX_ratings_i,nomDevise,minimumDuration=20,rdtClusters,trace=1)				
		###########################retrieve and bind results#################
		currency_crisis[[k]]<-list()
		currency_crisis[[k]]$states<-crisis_results$EmergingUnderlying$crisis
		currency_crisis[[k]]$dates <-crisis_results$EmergingUnderlying$dates
		currency_crisis[[k]]$returns<-crisis_results$EmergingUnderlying$rdt
		currency_crisis[[k]]$rating <-crisis_results$EmergingUnderlying$rating
		FX_InCrisis$states[k]=tail(currency_crisis[[k]]$states,1)
		crisis_histo_list <-rbind(crisis_histo_list,crisis_results$crisis_histo)
		crisis_theo_list  <-rbind(crisis_theo_list,crisis_results$crisis_theo)
		rdtClusters<-crisis_results$rdtClusters
		#############################Regime Normal Volatilities#########
		NoCrisisVol<-vol_fractile(currency_crisis[[k]]$returns[which(currency_crisis[[k]]$states==0)])
		GlobalVol  <-vol_fractile(currency_crisis[[k]]$returns)
		vol_i<-data.frame(FX=FX,NoCrisVolStd=NoCrisisVol$volStd,NoCrisVolFractile=NoCrisisVol$volFract,GlobalVolStd=GlobalVol$volStd,GlobalVolFract=GlobalVol$volFract)					
		vol_fract_std<-rbind(vol_fract_std,vol_i)
		######################################################################
		mytitle=paste(fxNames[k],":Detection des régimes de crises")  
		rdtTS   <- timeSeries( crisis_results$EmergingUnderlying$rdt,crisis_results$EmergingUnderlying$dates)
		spotTS  <- timeSeries( crisis_results$EmergingUnderlying$spot,crisis_results$EmergingUnderlying$dates)
		StateTS <- timeSeries(crisis_results$EmergingUnderlying$crisis,crisis_results$EmergingUnderlying$dates)
		################ plot crisis detection output ##########################
		par(mar=c(2,2,2,2)+.1)	 
		plot(rdtTS,type="l",col="black",main=mytitle,ylim=c(-0.15,0.15),cex.main=0.7) 
		par(new=TRUE)
		plot(spotTS,type="l",col="blue",xaxt="n",yaxt="n",xlab="",ylab="")
		par(new=TRUE)
		plot(StateTS,type="l",col="red",xaxt="n",yaxt="n",xlab="",ylab="")
	    axis(4)
		mtext("state",side=5,line=1)
        legend("topright",col=c("black","red","blue"),lty=1,legend=c("Rdt","state","spot"))
       
	
  }  
    cat("emerging currencies end calibrataion. Pegg currencies calibration in progress ... \n")
    vol_regime_normal<-vol_fract_std[,c(1,3)]
	###### Pegg Currencies calibration
    peggCurrencies_rdt_rating <- list()
	peggCurrencies_vol<-rep(0,length(peggCurrencies))
	for (k in c(1:length(peggCurrencies)))
	{
	    ii=match(peggCurrencies[k],devises_all)
		histo_i=as.vector(histoDev[,ii+1])
		dates_I<-dates_dev[!is.na(histo_i)]
		histo_i<- histo_i[!is.na(histo_i)]
		nb_data<-c(nb_data,length(histo_i))
		I_T=seq(1,length(histo_i),T_)
		histo_i_T<-histo_i[I_T]
		dates_I<-dates_I[I_T]
		rdtALL     <-  diff(log(histo_i_T),na.rm=FALSE)
		rdtDates<-dates_I[-1]
		rdtDates<-rdtDates[!is.na(rdtALL)]
		rdtALL <-rdtALL[!is.na(rdtALL)]
		date_init <- date_init_all
		I	 <- which((rdtDates >= date_init) & (rdtDates <= date_end) )
		dates_calibration <- rdtDates[I]
		y=rdtALL[I]
		############################ Crisis rating #######################
		FX=peggCurrencies[k]
		fx_rating=getRating(CountryRatingTable,FX)
		ratings_i=data.frame(dates=fx_rating$dates,rating=fx_rating$ratings)
		FX_i <-  data.frame(dates=dates_calibration,rdt = y)
		FX_ratings_i <-merge(FX_i,ratings_i,by="dates")
		peggCurrencies_rdt_rating[[k]]<-FX_ratings_i
		vol_i     = vol_fractile(y)
		peggCurrencies_vol[k]=max(vol_i$volFract,0.05)
	}
  
  } 
#currencies in crisis 
 {
	cat("Currencies in crisis at spot date: ",emerging_currencies[FX_InCrisis$states==1],"\n")
	initialState=cbind(emerging_currencies,FX_InCrisis$states)
  }
#Emerging Basket 
 {
    cat("emerging Basket calibration ... \n")
	EM_basket$EM_basket_dates  <- as.character(seq(as.Date(date_init_all),as.Date(date_end),1))
	iWE<-isWeekday( EM_basket$EM_basket_dates,wday = 1:5)
	EM_basket$EM_basket_dates<-EM_basket$EM_basket_dates [iWE]
	EM_basket$EM_basket_crisis <- rep(0,length(EM_basket$EM_basket_dates))
	EM_basket$EM_basket_returns <- rep(0,length(EM_basket$EM_basket_dates))
	EM_basket$EM_basket_composition <- rep(0,length(EM_basket$EM_basket_dates))
	EM_basket$EM_basket_composition_states<- rep(0,length(EM_basket$EM_basket_dates))
	EM_basket$NbFXInCrisis <- rep(0,length(EM_basket$EM_basket_dates))
	for (k in c(1:length(emerging_currencies)))
	{
	    cat(k,"\n")
		histo_i <- histo[,k]
		dates_I<-dates[!is.na(histo_i)]
		histo_i<- histo_i[!is.na(histo_i)]
		I_T=seq(1,length(histo_i),T)
		histo_i_T<-histo_i[I_T]
		
		dates_I<-dates_I[I_T]
		rdtALL     <-  diff(log(histo_i_T),na.rm=FALSE)
		rdtDates<-dates_I[-1]
		rdtDates<-rdtDates[!is.na(rdtALL)]
		rdtALL <-rdtALL[!is.na(rdtALL)]
		
		if( !is.na(dates_debut[k]))
		{
			date_init <- as.Date(dates_debut[k],format = "%d/%m/%Y")
		}else{ 
			date_init <- date_init_all
		}
		I <- which((rdtDates >= date_init) & (rdtDates <= date_end) )
		FxSpot <- histo_i[I]
		spotDates<-dates_I[I]
		dates_calibration <- rdtDates[I]
		y=rdtALL[I]
		depFrame <- data.frame(y=c(y))
		nomDevise=fxNames[k]
		logR <- y
		l_sim <- length(logR) #length of your data set
		h_0 <- var(logR)  
		###################### EM Basket returns #######################
		II<-match(as.character(rdtDates[I]),as.character(EM_basket$EM_basket_dates))
		EM_basket$EM_basket_returns[II]     <- EM_basket$EM_basket_returns[II] + rdtALL[I]
		EM_basket$EM_basket_composition[II] <- EM_basket$EM_basket_composition[II] +1
		II<-match(as.character(currency_crisis[[k]]$dates ),as.character(EM_basket$EM_basket_dates))
		EM_basket$NbFXInCrisis[II]          <- EM_basket$NbFXInCrisis[II] + currency_crisis[[k]]$states
		EM_basket$EM_basket_composition_states[II]<-EM_basket$EM_basket_composition_states[II]+1
	}
	#################### Compute EM_Basket return ############################
	
	EM_basket$EM_basket_returns <- EM_basket$EM_basket_returns / EM_basket$EM_basket_composition
	EM_basket$NbFXInCrisis      <- EM_basket$NbFXInCrisis /EM_basket$EM_basket_composition_states
	################### find EM_Basket Crisis   ############################
	dep=as.matrix(EM_basket$EM_basket_returns)
	logR <- dep
	pars0<-c(0,var(logR),0.001,0.0001)
    myModel_student<-RSGARCH_Fit(dep,n_regime=2,equal_by_reg,mean_incl,skew_incl,pars0,distIn="Student",garch_variance=FALSE,"Emerging Basket")
	coefStudent<- myModel_student@Coeff
	coeff<-coefStudent	  
	if(	coeff$a0[1] >  coeff$a0[2]){        
	  paramStudent=rbind(paramStudent,c("Emerging Basket",coeff$u[2],sqrt(coeff$a0[2]*250/T),coeff$df[2],
							         coeff$u[1],sqrt(coeff$a0[1]*250/T),coeff$df[1],
									 coeff$P[2,2],coeff$P[1,1],myModel_student@LL))
    
	  crisisStates_Student=1-myModel_student@viterbiStates
	  probCrisStates_Student<-myModel_student@smoothProb[,1]
	 }else{
	  paramStudent=rbind(paramStudent,c("Emerging Basket",coeff$u[1],sqrt(coeff$a0[1]*250/T),coeff$df[1],
												  coeff$u[2],sqrt(coeff$a0[2]*250/T),coeff$df[2],
												  coeff$P[1,1],coeff$P[2,2],myModel_student@LL))
	  crisisStates_Student=myModel_student@viterbiStates
	  probCrisStates_Student<-myModel_student@smoothProb[,2]
	}
	mytitle			  ="Emerging Basket:Detection des régimes de crises"  
	dates_calibration = EM_basket$EM_basket_dates
	Rdt	              = EM_basket$EM_basket_returns
	rdtTS             <- timeSeries( Rdt,dates_calibration)
	crisisStates      = crisisStates_Student
	EM_basket$EM_basket_crisis<-crisisStates
	EM_crisis	      =  EmergingCrisisList(EM_basket,-0.1,20)
	EM_basket$EM_basket_crisis=EM_crisis$states
	StateTS           <- timeSeries(EM_crisis$states,dates_calibration)
	NbFXInCrisis      <- timeSeries(EM_basket$NbFXInCrisis,dates_calibration)
    par(mar=c(2,2,2,2)+.1)	 
	plot(rdtTS,type="l",col="black",main=mytitle,ylim=c(-0.15,0.15),cex.main=0.7) 
	par(new=TRUE)
	plot(StateTS,type="l",col="red",xaxt="n",yaxt="n",xlab="",ylab="")
	par(new=TRUE)
	plot(NbFXInCrisis,type="l",col="blue",xaxt="n",yaxt="n",xlab="",ylab="")
	axis(4)
	mtext("state",side=5,line=1)
    legend("topright",col=c("black","red","blue"),lty=1,legend=c("Rdt","state","% FX in crisis"))
    #####################Calibration of Global emerging crisis probability##################
	dev.off()
}
#Transition Probabilities
{  
   #transitionProb(currency_crisis)
    ####################Contagion Risk   ###########################
    tansitionProbabilities=contagionRisk(EM_basket,currency_crisis,TRUE)
	tp = tansitionProbabilities
    #contagionRisk(EM_basket,currency_crisis,FALSE)
    ######################End of Calibration of Global emerging crisis probability##################
    ########################Calibration of Contagion Risq Probabil##################
}
#Cluster calibration 
{
   cat("Rating clusters calibration... \n")
    crisis_cluster_2 <- list()
  	JumpModel_1_5   <-fit_jump_down(rdtClusters$rdt_cluster_1_5,randomStart=TRUE,trace=1)
	mu     	  <- JumpModel_1_5$mu
	sigma     <- sqrt(JumpModel_1_5$sig2*260)
	r_down      <- JumpModel_1_5$r_down
	lambda_down <- 0.03287671
	theo_quantiles        <- min_max_average_return2(mu,sigma,lambda_down,r_down,20,0.01)
	q01_theo              <-theo_quantiles[1]
	average_theo          <-theo_quantiles[2]
	q99_theo              <-theo_quantiles[3]
	var_skew_res           <-var_skew2(mu,sigma,lambda_down,r_down)
	cluster_1_5<-data.frame(rating_Cluster="[1-5]",mu=mu,sigma=sigma,lambda_down=lambda_down,r_down=r_down,
			q01_theo=q01_theo,q99_theo=q99_theo,average_theo=average_theo,sigmaSimu=var_skew_res[1],skew=var_skew_res[2])
	crisis_cluster_2<-rbind(crisis_cluster_2,  cluster_1_5)
	JumpModel_6_14  <-fit_jump_down(rdtClusters$rdt_cluster_6_14,randomStart=TRUE,trace=1)
	mu     	  <- JumpModel_6_14$mu
	sigma     <- sqrt(JumpModel_6_14$sig2*260)
	r_down      <- JumpModel_6_14$r_down
	theo_quantiles        <- min_max_average_return2(mu,sigma,lambda_down,r_down,20,0.01)
	q01_theo              <-theo_quantiles[1]
	average_theo          <-theo_quantiles[2]
	q99_theo              <-theo_quantiles[3]
	var_skew_res           <-var_skew2(mu,sigma,lambda_down,r_down)
	cluster_6_14<-data.frame(rating_Cluster="[6-14]",mu=mu,sigma=sigma,lambda_down=lambda_down,r_down=r_down,
	q01_theo=q01_theo,q99_theo=q99_theo,average_theo=average_theo,sigmaSimu=var_skew_res[1],skew=var_skew_res[2])
	crisis_cluster_2<-rbind(crisis_cluster_2,  cluster_6_14)
	JumpModel_15_20 <-fit_jump_down(rdtClusters$rdt_cluster_15_20,randomStart=TRUE,trace=1)		    
	mu     	  <- JumpModel_15_20$mu
	sigma     <- sqrt(JumpModel_15_20$sig2*260)
	r_down      <- JumpModel_15_20$r_down
	theo_quantiles        <- min_max_average_return2(mu,sigma,lambda_down,r_down,20,0.01)
	q01_theo              <-theo_quantiles[1]
	average_theo          <-theo_quantiles[2]
	q99_theo              <-theo_quantiles[3]
	var_skew_res           <-var_skew2(mu,sigma,lambda_down,r_down)
	cluster_15_20<-data.frame(rating_Cluster="[15-20]",mu=mu,sigma=sigma,lambda_down=lambda_down,r_down=r_down,
			q01_theo=q01_theo,q99_theo=q99_theo,average_theo=average_theo,sigmaSimu=var_skew_res[1],skew=var_skew_res[2])
	crisis_cluster_2<-rbind(crisis_cluster_2,  cluster_15_20)
}
#Developed Currencies calibration 
{
    cat("Developed currencies calibration ... \n")
	developed_Currencies<-c("AUD","CAD","CHF","DKK","EUR","GBP","JPY","NOK","NZD","SEK","SGD","XAG","XAU")
	nb_devises =length(devises_all)
	vol<-list()
	# date_init	<- "1997-01-01"
	# date_end 	<- "2013-12-31"
	# I <- which((dates_dev >= date_init) & (dates_dev <= date_end) )
	for(i in 1:nb_devises)
	{
		#recuperer les rendements historiques 
		#et calculer les log_rdt
		histo_i=as.vector(histoDev[,i+1])
		log_histo_i=log(histo_i)
		log_rdmt_histo_i = diff(log(histo_i))
		nb_data<-c(nb_data,length(histo_i[!is.na(histo_i)]))
		#construction du vecteur de log_rdt
		if( i==1 )
		{
			diff_log_fx = log_rdmt_histo_i
			log_fx      = log_histo_i
		}else{
			diff_log_fx = cbind(diff_log_fx,log_rdmt_histo_i)
			log_fx      = cbind(log_fx,log_histo_i)
		}
		# log_rdmt_histo_i = log_rdmt_histo_i[!is.na(log_rdmt_histo_i)]
		#calibration
		vol_i     	= vol_fractile(log_rdmt_histo_i)
		vol$std		= c(vol$std,vol_i$volStd)
		vol$fract	= c(vol$fract,vol_i$volFract)
		vol$name	= c(vol$name,devises_all[i])
	}
	dev_FX_Vol <-NULL
	dev_FX_Vol_num<-NULL
	for(i in c(1:length(developed_Currencies)))
	{
		ii=match(developed_Currencies[i],vol$name)
		dev_FX_Vol   <-rbind(dev_FX_Vol,paste("FX",developed_Currencies[i],",",max(vol$fract[ii],0.11),sep=""))
		dev_FX_Vol_num <- rbind(dev_FX_Vol_num,max(vol$fract[ii],0.11))
	}
	##################Correlation Matrix 10 years############################
	matrice_correlation = cor(diff_log_fx, use = "pairwise.complete.obs")
	######## Modify CNH CNY correlation (5 days Correlation) #############
	iCNH 	<- match("CNH",devises_all)
	iCNY 	<- match("CNY",devises_all)
	matrice_correlation[iCNH,]=matrice_correlation[iCNY,]
	matrice_correlation[,iCNH]=matrice_correlation[,iCNY]
	matrice_correlation[iCNH,iCNH]=1
	diff_CNH_10=log_fx[seq(11,length(log_fx[,iCNH])),iCNH]-log_fx[seq(1,(length(log_fx[,iCNH])-10)),iCNH]
	diff_CNY_10=log_fx[seq(11,length(log_fx[,iCNY])),iCNY]-log_fx[seq(1,(length(log_fx[,iCNY])-10)),iCNY]
	corrCNHCNY=cor(diff_CNH_10,diff_CNY_10,use = "pairwise.complete.obs")
	matrice_correlation[iCNH,iCNY]=corrCNHCNY
	matrice_correlation[iCNY,iCNH]=corrCNHCNY
	corr_FX<-format_Correl_Matrix(paste("FX",devises_all,sep=""),matrice_correlation)
	
}
data_count<-list(dev_names=c(emerging_currencies,
                             peggCurrencies,
                             developed_Currencies),nb_data=nb_data)

Emerging_currencies  =list(cluster_1_5=cluster_1_5,
                           cluster_6_14=cluster_6_14,
                           cluster_15_20=cluster_15_20,
                           rdtClusters=rdtClusters,
                           vol_regime_normal=vol_regime_normal,
                           tp=tp,
                           currency_crisis=currency_crisis)
Developed_currencies =list(dev_FX_Vol=dev_FX_Vol,
                           vol=vol,
                           diff_log_fx=diff_log_fx)
Pegg_Currencies      =list(peggCurrencies_vol=peggCurrencies_vol,
                           peggCurrencies_rdt_rating=peggCurrencies_rdt_rating)

return(list(CalibrationName=CalibrationName,
            Emerging_currencies=Emerging_currencies,
            Developed_currencies=Developed_currencies,
            Pegg_Currencies=Pegg_Currencies,
            corr_FX=corr_FX,
            Emerging_currencies_list=emerging_currencies,
            Developed_currencies_list=developed_Currencies,
            pegg_currencies_list=peggCurrencies,data_count=data_count
))
}

calibrate_FX_Model <-function(resultPath,dataPath,basePath,CalibrationName,CalibrationWindow,nb_Currencies)
{
  #Variable initialisation
{
  nb_data<-NULL
  T_ = 1
  histoDev = read.csv(paste(dataPath,"FX_data_calib.csv",sep=""), sep=";" , header=T)
  devises_all= names(histoDev)[1+1:nb_Currencies]
  dates_dev  <- as.Date(as.character(histoDev$date),format = "%d/%m/%Y")
  fx_1J_EMERGING      =	read.csv(paste(dataPath,"Histo_FX_Table_Emerging.csv",sep=""),sep=";",header=TRUE)
  CountryRatingTable  =   read.csv(paste(dataPath,"Histo_Country_Rating_Table_Emerging.csv",sep=""),sep=";",header=TRUE)
  dates_debut=read.csv(paste(dataPath,"FX_Dates.csv",sep=""),sep=";",header=TRUE)
  dates_debut <-dates_debut$Date
  paramStudent<- c("nom devise","mu0","Vol0","df0","mu1","vol1","df1","p0","p1","LogLik")
  filename="CrisisDetectionOutput.pdf"
  mypath=file.path(resultPath, filename)
  pdf(file = mypath)
  peggCurrencies<-c("CNY","CNH","AED","HKD","SAR","KWD","BGN")
  par(mfrow=c(2,1))
  dates  <- as.Date(as.character(fx_1J_EMERGING$Date),format = "%d/%m/%y")
  histo <- as.matrix(fx_1J_EMERGING[,-1])
  devises <-names(fx_1J_EMERGING)[-1]
  fxNames=names(fx_1J_EMERGING[1,-1])
  mean_incl <- TRUE
  skew_incl <- FALSE
  equal_by_reg <- c(FALSE,FALSE,TRUE,TRUE,FALSE,FALSE,FALSE)
  init_reg <- init <- 1 
  params_init <- "gray" 
  sign_loglik <- -1
  do_loglik <- TRUE
  n_regime <- 2  
  tol 	 <- 1e-8       #Gibbs sampler tolerance for stopping computation of sig2
  emerging_currencies=names(fx_1J_EMERGING)[-1]
  crisis_histo_list <-list()
  crisis_histo_transition_prob_list<-list()
  crisis_theo_list  <-list()
  nb_crisis_all <-0
  crisis_length_all <-0
  serie_length_all <-0
  EM_basket   <-list(EM_basket_dates=NULL,EM_basket_crisis=NULL,EM_basket_returns=NULL,EM_basket_composition=NULL)
  set.seed(4)
  
}			
#Crisis Detection Emerging Currencies  
{ 
  
  date_end       <- as.Date(CalibrationWindow$date_fin)
  date_init_all  <- as.Date(CalibrationWindow$date_debut)
  #Contagion risk  and Emerging Crisis 
  EM_basket$EM_basket_dates  <- as.character(seq(as.Date(CalibrationWindow$date_debut),as.Date(CalibrationWindow$date_fin),1))
  iWE<-isWeekday( EM_basket$EM_basket_dates,wday = 1:5)
  EM_basket$EM_basket_dates		<-EM_basket$EM_basket_dates [iWE]
  EM_basket$EM_basket_crisis 		<- rep(0,length(EM_basket$EM_basket_dates))
  EM_basket$EM_basket_returns 	<- rep(0,length(EM_basket$EM_basket_dates))
  EM_basket$EM_basket_composition <- rep(0,length(EM_basket$EM_basket_dates))
  currency_crisis<-list()
  FX_InCrisis    <-list()
  FX_InCrisis$FxNames<-emerging_currencies
  FX_InCrisis$states <-rep(0,length(emerging_currencies))
  rdtClusters<-list()
  crisis_results<-NULL
  vol_regime_normal<-NULL
  vol_fract_std<-NULL
  crisis_res_keepall <- data.frame()
  for (k in c(1:length(emerging_currencies)))
  {
    histo_i <- histo[,k]
    dates_I<-dates[!is.na(histo_i)]
    histo_i<- histo_i[!is.na(histo_i)]
    nb_data<-c(nb_data,length(histo_i))
    I_T=seq(1,length(histo_i),T_)
    histo_i_T<-histo_i[I_T]
    dates_I<-dates_I[I_T]
    rdtALL     <-  diff(log(histo_i_T),na.rm=FALSE)
    rdtDates<-dates_I[-1]
    rdtDates<-rdtDates[!is.na(rdtALL)]
    rdtALL <-rdtALL[!is.na(rdtALL)]
    if( !is.na(dates_debut[k]))
    {
      date_init <- as.Date(as.character(dates_debut[k]),format = "%d/%m/%Y")
    }else{ 
      date_init <- CalibrationWindow$date_debut
    }
    I <- which((rdtDates >= date_init) & (rdtDates <= date_end) )
    FxSpot <- histo_i[I]
    spotDates<-dates_I[I]
    dates_calibration <- rdtDates[I]
    y=rdtALL[I]
    nomDevise=fxNames[k]
    ###################### EM Basket returns #######################
    currency_i_crisis<-list()
    currency_i_crisis$dates<-as.character(seq(as.Date(date_init),as.Date(date_end),1))
    currency_i_crisis$dates<-currency_i_crisis$dates[isWeekday(currency_i_crisis$dates,wday = 1:5)]
    currency_i_crisis$states<-rep(0,length(currency_i_crisis$dates))
    II<-match(as.character(rdtDates[I]),as.character(EM_basket$EM_basket_dates))
    EM_basket$EM_basket_returns[II]     <- EM_basket$EM_basket_returns[II] + rdtALL[I]
    EM_basket$EM_basket_composition[II] <- EM_basket$EM_basket_composition[II] +1
    
    ######################historical graphic########################
    par(mfcol=c(2,1))
    fenetre_calibration <- rep(0,length(rdtALL))
    fenetre_calibration[I]<-1
    rdtTS <- timeSeries( rdtALL,rdtDates)
    mytitle=paste(fxNames[k],":Graphe des rdts et Fenetre de calibration ")
    plot(rdtTS,type="l",col="blue",main=mytitle,cex.main=0.7)
    par(new=TRUE)
    plot(rdtDates, fenetre_calibration,type="l",col="black",xaxt="n",yaxt="n",xlab="",ylab="", ylim=c(0,1))
    axis(4,ylim=c(0,1))
    ##################Crisis Detection Algorithm##############	 
    dep=as.matrix(y)
    logR <- y
    pars0<-c(0,var(logR),0.001,0.0001)
    cat(format(Sys.time(), "%a %b %d %X %Y"),"::",round(100*k/length(emerging_currencies)),"% complete... \n")
    myModel_student<-RSGARCH_Fit(dep,n_regime=2,equal_by_reg,mean_incl,skew_incl,pars0,distIn="Student",garch_variance=FALSE,nomDevise)
    coefStudent<- myModel_student@Coeff
    coeff<-coefStudent	  
    if(	coeff$a0[1] >  coeff$a0[2]){        
      paramStudent=rbind(paramStudent,c(nomDevise,coeff$u[2],sqrt(coeff$a0[2]*250/T_),coeff$df[2],
                                        coeff$u[1],sqrt(coeff$a0[1]*250/T_),coeff$df[1],
                                        coeff$P[2,2],coeff$P[1,1],myModel_student@LL))
      
      crisisStates_Student=1-myModel_student@viterbiStates
      probCrisStates_Student<-myModel_student@smoothProb[,1]
    }else{k
          paramStudent=rbind(paramStudent,c(nomDevise,coeff$u[1],sqrt(coeff$a0[1]*250/T_),coeff$df[1],
                                            coeff$u[2],sqrt(coeff$a0[2]*250/T_),coeff$df[2],
                                            coeff$P[1,1],coeff$P[2,2],myModel_student@LL))
          # probCrisStates<-myModel@smoothProb[,2]
          crisisStates_Student=myModel_student@viterbiStates
          probCrisStates_Student<-myModel_student@smoothProb[,2]
    }
    ############################ Crisis rating #######################
    FX=emerging_currencies[k]
    fx_rating=getRating(CountryRatingTable,FX)
    ratings_i=data.frame(dates=fx_rating$dates,rating=fx_rating$ratings)
    # krw_TS=timeSeries(krw_rating$ratings,krw_rating$dates)
    FX_i <-  data.frame(dates=dates_calibration,rdt = y,crisis=crisisStates_Student,spot=FxSpot)
    FX_ratings_i <-merge(FX_i,ratings_i,by="dates")
    ##################### crisis list ##########################
    crisis_results=CrisisFiltering_and_Statisitics(FX,
            FX_ratings_i,nomDevise,minimumDuration=20,rdtClusters,trace=1)				    
    crisis_res_keepall=rbind(crisis_res_keepall, crisis_results$keep_all)
    ###########################retrieve and bind results#################
    currency_crisis[[k]]<-list()
    currency_crisis[[k]]$states<-crisis_results$EmergingUnderlying$crisis
    currency_crisis[[k]]$dates <-crisis_results$EmergingUnderlying$dates
    currency_crisis[[k]]$returns<-crisis_results$EmergingUnderlying$rdt
    currency_crisis[[k]]$rating <-crisis_results$EmergingUnderlying$rating
    FX_InCrisis$states[k]=tail(currency_crisis[[k]]$states,1)
    crisis_histo_list <-rbind(crisis_histo_list,crisis_results$crisis_histo)
    crisis_theo_list  <-rbind(crisis_theo_list,crisis_results$crisis_theo)
    rdtClusters<-crisis_results$rdtClusters
    #############################Regime Normal Volatilities#########
    NoCrisisVol<-vol_fractile(currency_crisis[[k]]$returns[which(currency_crisis[[k]]$states==0)])
    GlobalVol  <-vol_fractile(currency_crisis[[k]]$returns)
    vol_i<-data.frame(FX=FX,NoCrisVolStd=NoCrisisVol$volStd,NoCrisVolFractile=NoCrisisVol$volFract,GlobalVolStd=GlobalVol$volStd,GlobalVolFract=GlobalVol$volFract)					
    vol_fract_std<-rbind(vol_fract_std,vol_i)
    ######################################################################
    mytitle=paste(fxNames[k],":Detection des régimes de crises")  
    rdtTS   <- timeSeries( crisis_results$EmergingUnderlying$rdt,crisis_results$EmergingUnderlying$dates)
    spotTS  <- timeSeries( crisis_results$EmergingUnderlying$spot,crisis_results$EmergingUnderlying$dates)
    StateTS <- timeSeries(crisis_results$EmergingUnderlying$crisis,crisis_results$EmergingUnderlying$dates)
    ################ plot crisis detection output ##########################
    par(mar=c(2,2,2,2)+.1)	 
    plot(rdtTS,type="l",col="black",main=mytitle,ylim=c(-0.15,0.15),cex.main=0.7) 
    par(new=TRUE)
    plot(spotTS,type="l",col="blue",xaxt="n",yaxt="n",xlab="",ylab="")
    par(new=TRUE)
    plot(StateTS,type="l",col="red",xaxt="n",yaxt="n",xlab="",ylab="")
    axis(4)
    mtext("state",side=5,line=1)
    legend("topright",col=c("black","red","blue"),lty=1,legend=c("Rdt","state","spot"))
    
    
  }  
  cat("emerging currencies end calibrataion. Pegg currencies calibration in progress ... \n")
  vol_regime_normal<-vol_fract_std[,c(1,3)]
  ###### Pegg Currencies calibration
  peggCurrencies_rdt_rating <- list()
  peggCurrencies_vol<-rep(0,length(peggCurrencies))
  for (k in c(1:length(peggCurrencies)))
  {
    ii=match(peggCurrencies[k],devises_all)
    histo_i=as.vector(histoDev[,ii+1])
    dates_I<-dates_dev[!is.na(histo_i)]
    histo_i<- histo_i[!is.na(histo_i)]
    nb_data<-c(nb_data,length(histo_i))
    I_T=seq(1,length(histo_i),T_)
    histo_i_T<-histo_i[I_T]
    dates_I<-dates_I[I_T]
    rdtALL     <-  diff(log(histo_i_T),na.rm=FALSE)
    rdtDates<-dates_I[-1]
    rdtDates<-rdtDates[!is.na(rdtALL)]
    rdtALL <-rdtALL[!is.na(rdtALL)]
    date_init <- date_init_all
    I	 <- which((rdtDates >= date_init) & (rdtDates <= date_end) )
    dates_calibration <- rdtDates[I]
    y=rdtALL[I]
    ############################ Crisis rating #######################
    FX=peggCurrencies[k]
    fx_rating=getRating(CountryRatingTable,FX)
    ratings_i=data.frame(dates=fx_rating$dates,rating=fx_rating$ratings)
    FX_i <-  data.frame(dates=dates_calibration,rdt = y)
    FX_ratings_i <-merge(FX_i,ratings_i,by="dates")
    peggCurrencies_rdt_rating[[k]]<-FX_ratings_i
    vol_i     = vol_fractile(y)
    peggCurrencies_vol[k]=max(vol_i$volFract,0.05)
  }
  
} 
#currencies in crisis 
{
  cat("Currencies in crisis at spot date: ",emerging_currencies[FX_InCrisis$states==1],"\n")
  initialState=cbind(emerging_currencies,FX_InCrisis$states)
}
#Emerging Basket 
{
  cat("emerging Basket calibration ... \n")
  EM_basket$EM_basket_dates  <- as.character(seq(as.Date(date_init_all),as.Date(date_end),1))
  iWE<-isWeekday( EM_basket$EM_basket_dates,wday = 1:5)
  EM_basket$EM_basket_dates<-EM_basket$EM_basket_dates [iWE]
  EM_basket$EM_basket_crisis <- rep(0,length(EM_basket$EM_basket_dates))
  EM_basket$EM_basket_returns <- rep(0,length(EM_basket$EM_basket_dates))
  EM_basket$EM_basket_composition <- rep(0,length(EM_basket$EM_basket_dates))
  EM_basket$EM_basket_composition_states<- rep(0,length(EM_basket$EM_basket_dates))
  EM_basket$NbFXInCrisis <- rep(0,length(EM_basket$EM_basket_dates))
  for (k in c(1:length(emerging_currencies)))
  {
    cat(k,"\n")
    histo_i <- histo[,k]
    dates_I<-dates[!is.na(histo_i)]
    histo_i<- histo_i[!is.na(histo_i)]
    I_T=seq(1,length(histo_i),T)
    histo_i_T<-histo_i[I_T]
    
    dates_I<-dates_I[I_T]
    rdtALL     <-  diff(log(histo_i_T),na.rm=FALSE)
    rdtDates<-dates_I[-1]
    rdtDates<-rdtDates[!is.na(rdtALL)]
    rdtALL <-rdtALL[!is.na(rdtALL)]
    
    if( !is.na(dates_debut[k]))
    {
      date_init <- as.Date(dates_debut[k],format = "%d/%m/%Y")
    }else{ 
      date_init <- date_init_all
    }
    I <- which((rdtDates >= date_init) & (rdtDates <= date_end) )
    FxSpot <- histo_i[I]
    spotDates<-dates_I[I]
    dates_calibration <- rdtDates[I]
    y=rdtALL[I]
    depFrame <- data.frame(y=c(y))
    nomDevise=fxNames[k]
    logR <- y
    l_sim <- length(logR) #length of your data set
    h_0 <- var(logR)  
    ###################### EM Basket returns #######################
    II<-match(as.character(rdtDates[I]),as.character(EM_basket$EM_basket_dates))
    EM_basket$EM_basket_returns[II]     <- EM_basket$EM_basket_returns[II] + rdtALL[I]
    EM_basket$EM_basket_composition[II] <- EM_basket$EM_basket_composition[II] +1
    II<-match(as.character(currency_crisis[[k]]$dates ),as.character(EM_basket$EM_basket_dates))
    EM_basket$NbFXInCrisis[II]          <- EM_basket$NbFXInCrisis[II] + currency_crisis[[k]]$states
    EM_basket$EM_basket_composition_states[II]<-EM_basket$EM_basket_composition_states[II]+1
  }
  #################### Compute EM_Basket return ############################
  
  EM_basket$EM_basket_returns <- EM_basket$EM_basket_returns / EM_basket$EM_basket_composition
  EM_basket$NbFXInCrisis      <- EM_basket$NbFXInCrisis /EM_basket$EM_basket_composition_states
  ################### find EM_Basket Crisis   ############################
  dep=as.matrix(EM_basket$EM_basket_returns)
  logR <- dep
  pars0<-c(0,var(logR),0.001,0.0001)
  myModel_student<-RSGARCH_Fit(dep,n_regime=2,equal_by_reg,mean_incl,skew_incl,pars0,distIn="Student",garch_variance=FALSE,"Emerging Basket")
  coefStudent<- myModel_student@Coeff
  coeff<-coefStudent	  
  if(	coeff$a0[1] >  coeff$a0[2]){        
    paramStudent=rbind(paramStudent,c("Emerging Basket",coeff$u[2],sqrt(coeff$a0[2]*250/T),coeff$df[2],
                                      coeff$u[1],sqrt(coeff$a0[1]*250/T),coeff$df[1],
                                      coeff$P[2,2],coeff$P[1,1],myModel_student@LL))
    
    crisisStates_Student=1-myModel_student@viterbiStates
    probCrisStates_Student<-myModel_student@smoothProb[,1]
  }else{
    paramStudent=rbind(paramStudent,c("Emerging Basket",coeff$u[1],sqrt(coeff$a0[1]*250/T),coeff$df[1],
                                      coeff$u[2],sqrt(coeff$a0[2]*250/T),coeff$df[2],
                                      coeff$P[1,1],coeff$P[2,2],myModel_student@LL))
    crisisStates_Student=myModel_student@viterbiStates
    probCrisStates_Student<-myModel_student@smoothProb[,2]
  }
  mytitle			  ="Emerging Basket:Detection des régimes de crises"  
  dates_calibration = EM_basket$EM_basket_dates
  Rdt	              = EM_basket$EM_basket_returns
  rdtTS             <- timeSeries( Rdt,dates_calibration)
  crisisStates      = crisisStates_Student
  EM_basket$EM_basket_crisis<-crisisStates
  EM_crisis	      =  EmergingCrisisList(EM_basket,-0.1,20)
  EM_basket$EM_basket_crisis=EM_crisis$states
  StateTS           <- timeSeries(EM_crisis$states,dates_calibration)
  NbFXInCrisis      <- timeSeries(EM_basket$NbFXInCrisis,dates_calibration)
  par(mar=c(2,2,2,2)+.1)	 
  plot(rdtTS,type="l",col="black",main=mytitle,ylim=c(-0.15,0.15),cex.main=0.7) 
  par(new=TRUE)
  plot(StateTS,type="l",col="red",xaxt="n",yaxt="n",xlab="",ylab="")
  par(new=TRUE)
  plot(NbFXInCrisis,type="l",col="blue",xaxt="n",yaxt="n",xlab="",ylab="")
  axis(4)
  mtext("state",side=5,line=1)
  legend("topright",col=c("black","red","blue"),lty=1,legend=c("Rdt","state","% FX in crisis"))
  #####################Calibration of Global emerging crisis probability##################
  dev.off()
}
#Transition Probabilities
{  
  #transitionProb(currency_crisis)
  ####################Contagion Risk   ###########################
  tansitionProbabilities=contagionRisk(EM_basket,currency_crisis,TRUE)
  tp = tansitionProbabilities
  #contagionRisk(EM_basket,currency_crisis,FALSE)
  ######################End of Calibration of Global emerging crisis probability##################
  ########################Calibration of Contagion Risq Probabil##################
}
#Cluster calibration 
{
  cat("Rating clusters calibration... \n")
  crisis_cluster_2 <- list()
  JumpModel_1_5   <-fit_jump_down(rdtClusters$rdt_cluster_1_5,randomStart=TRUE,trace=1)
  mu     	  <- JumpModel_1_5$mu
  sigma     <- sqrt(JumpModel_1_5$sig2*260)
  r_down      <- JumpModel_1_5$r_down
  lambda_down <- 0.03287671
  theo_quantiles        <- min_max_average_return2(mu,sigma,lambda_down,r_down,20,0.01)
  q01_theo              <-theo_quantiles[1]
  average_theo          <-theo_quantiles[2]
  q99_theo              <-theo_quantiles[3]
  var_skew_res           <-var_skew2(mu,sigma,lambda_down,r_down)
  cluster_1_5<-data.frame(rating_Cluster="[1-5]",mu=mu,sigma=sigma,lambda_down=lambda_down,r_down=r_down,
                          q01_theo=q01_theo,q99_theo=q99_theo,average_theo=average_theo,sigmaSimu=var_skew_res[1],skew=var_skew_res[2])
  crisis_cluster_2<-rbind(crisis_cluster_2,  cluster_1_5)
  JumpModel_6_14  <-fit_jump_down(rdtClusters$rdt_cluster_6_14,randomStart=TRUE,trace=1)
  mu     	  <- JumpModel_6_14$mu
  sigma     <- sqrt(JumpModel_6_14$sig2*260)
  r_down      <- JumpModel_6_14$r_down
  theo_quantiles        <- min_max_average_return2(mu,sigma,lambda_down,r_down,20,0.01)
  q01_theo              <-theo_quantiles[1]
  average_theo          <-theo_quantiles[2]
  q99_theo              <-theo_quantiles[3]
  var_skew_res           <-var_skew2(mu,sigma,lambda_down,r_down)
  cluster_6_14<-data.frame(rating_Cluster="[6-14]",mu=mu,sigma=sigma,lambda_down=lambda_down,r_down=r_down,
                           q01_theo=q01_theo,q99_theo=q99_theo,average_theo=average_theo,sigmaSimu=var_skew_res[1],skew=var_skew_res[2])
  crisis_cluster_2<-rbind(crisis_cluster_2,  cluster_6_14)
  JumpModel_15_20 <-fit_jump_down(rdtClusters$rdt_cluster_15_20,randomStart=TRUE,trace=1)		    
  mu     	  <- JumpModel_15_20$mu
  sigma     <- sqrt(JumpModel_15_20$sig2*260)
  r_down      <- JumpModel_15_20$r_down
  theo_quantiles        <- min_max_average_return2(mu,sigma,lambda_down,r_down,20,0.01)
  q01_theo              <-theo_quantiles[1]
  average_theo          <-theo_quantiles[2]
  q99_theo              <-theo_quantiles[3]
  var_skew_res           <-var_skew2(mu,sigma,lambda_down,r_down)
  cluster_15_20<-data.frame(rating_Cluster="[15-20]",mu=mu,sigma=sigma,lambda_down=lambda_down,r_down=r_down,
                            q01_theo=q01_theo,q99_theo=q99_theo,average_theo=average_theo,sigmaSimu=var_skew_res[1],skew=var_skew_res[2])
  crisis_cluster_2<-rbind(crisis_cluster_2,  cluster_15_20)
}
#Developed Currencies calibration 
{
  cat("Developed currencies calibration ... \n")
  developed_Currencies<-c("AUD","CAD","CHF","DKK","EUR","GBP","JPY","NOK","NZD","SEK","SGD","XAG","XAU")
  nb_devises =length(devises_all)
  vol<-list()
  # date_init	<- "1997-01-01"
  # date_end 	<- "2013-12-31"
  # I <- which((dates_dev >= date_init) & (dates_dev <= date_end) )
  for(i in 1:nb_devises)
  {
    #recuperer les rendements historiques 
    #et calculer les log_rdt
    histo_i=as.vector(histoDev[,i+1])
    log_histo_i=log(histo_i)
    log_rdmt_histo_i = diff(log(histo_i))
    nb_data<-c(nb_data,length(histo_i[!is.na(histo_i)]))
    #construction du vecteur de log_rdt
    if( i==1 )
    {
      diff_log_fx = log_rdmt_histo_i
      log_fx      = log_histo_i
    }else{
      diff_log_fx = cbind(diff_log_fx,log_rdmt_histo_i)
      log_fx      = cbind(log_fx,log_histo_i)
    }
    # log_rdmt_histo_i = log_rdmt_histo_i[!is.na(log_rdmt_histo_i)]
    #calibration
    vol_i     	= vol_fractile(log_rdmt_histo_i)
    vol$std		= c(vol$std,vol_i$volStd)
    vol$fract	= c(vol$fract,vol_i$volFract)
    vol$name	= c(vol$name,devises_all[i])
  }
  dev_FX_Vol <-NULL
  dev_FX_Vol_num<-NULL
  for(i in c(1:length(developed_Currencies)))
  {
    ii=match(developed_Currencies[i],vol$name)
    dev_FX_Vol   <-rbind(dev_FX_Vol,paste("FX",developed_Currencies[i],",",max(vol$fract[ii],0.11),sep=""))
    dev_FX_Vol_num <- rbind(dev_FX_Vol_num,max(vol$fract[ii],0.11))
  }
  ##################Correlation Matrix 10 years############################
  matrice_correlation = cor(diff_log_fx, use = "pairwise.complete.obs")
  ######## Modify CNH CNY correlation (5 days Correlation) #############
  iCNH 	<- match("CNH",devises_all)
  iCNY 	<- match("CNY",devises_all)
  matrice_correlation[iCNH,]=matrice_correlation[iCNY,]
  matrice_correlation[,iCNH]=matrice_correlation[,iCNY]
  matrice_correlation[iCNH,iCNH]=1
  diff_CNH_10=log_fx[seq(11,length(log_fx[,iCNH])),iCNH]-log_fx[seq(1,(length(log_fx[,iCNH])-10)),iCNH]
  diff_CNY_10=log_fx[seq(11,length(log_fx[,iCNY])),iCNY]-log_fx[seq(1,(length(log_fx[,iCNY])-10)),iCNY]
  corrCNHCNY=cor(diff_CNH_10,diff_CNY_10,use = "pairwise.complete.obs")
  matrice_correlation[iCNH,iCNY]=corrCNHCNY
  matrice_correlation[iCNY,iCNH]=corrCNHCNY
  corr_FX<-format_Correl_Matrix(paste("FX",devises_all,sep=""),matrice_correlation)
  
}
#NEW 2015 : Floor on cross volatilities
{
  cat("Imposing floors on cross volatilities ... \n")
  # STEP 0 preliminary steps
  XFloor_FX_names <- c(developed_Currencies,
                       emerging_currencies,
                       peggCurrencies)
  XFloor_FX_VOL   <-  c(dev_FX_Vol_num,
                        vol_regime_normal$NoCrisVolFractile,
                        peggCurrencies_vol)
  XFloor_Emerging <- c(emerging_currencies,
                       peggCurrencies)
  XFloor_devises_all <- devises_all
  
  # STEP 1 implied cross vol matrix construction
  XFloor_correlMatrix_initial  <- matrice_correlation
  XFloor_nb_FX <- nrow(XFloor_correlMatrix_initial)
  XFloor_cross_vol     <- matrix(0,XFloor_nb_FX,XFloor_nb_FX)
  for(i in (1:(XFloor_nb_FX-1)))
  {
    for(j in ((i+1):XFloor_nb_FX))
    {
      XFloor_vol_i <- XFloor_FX_VOL[match(XFloor_devises_all[i],
                                          XFloor_FX_names)]
      XFloor_vol_j <- XFloor_FX_VOL[match(XFloor_devises_all[j],
                                          XFloor_FX_names)]
      XFloor_cross_vol[i,j] <-sqrt(XFloor_vol_i^2+
                                     XFloor_vol_j^2-
                              2*XFloor_correlMatrix_initial[i,j]*
                                XFloor_vol_i*XFloor_vol_j) 
      XFloor_cross_vol[j,i] <- XFloor_cross_vol[i,j]  
    }    
  }
  
  # STEP 2 loop on currency pairs to floor all cross vols
  XFloor_cross_vol_floor     <- matrix(0,XFloor_nb_FX,XFloor_nb_FX)  
  XFloor_floor_DEV               <- 0.11
  XFloor_floor_EME               <- 0.05  
  for(i in 1:XFloor_nb_FX)
  {
    for(j in c(1:XFloor_nb_FX)[-i])
    {
      if(is.na(match(XFloor_devises_all[i],XFloor_Emerging))&
           is.na(match(XFloor_devises_all[j],XFloor_Emerging)))
      {
        XFloor_cross_vol_floor[i,j] <-max(XFloor_cross_vol[i,j],
                                          XFloor_floor_DEV)        
      }
      else
      {
        XFloor_cross_vol_floor[i,j] <-max(XFloor_cross_vol[i,j],
                                          XFloor_floor_EME)      
      }
    }  
  }
  
  # STEP 3 imply the new correlation matrix from floored cross vols
  XFloor_correlMatrix_modified <- matrix(0,XFloor_nb_FX,XFloor_nb_FX)
  for(i in 1:XFloor_nb_FX)
  {
    XFloor_vol_i <-XFloor_FX_VOL[match(XFloor_devises_all[i],
                                       XFloor_FX_names)]
    for(j in i:XFloor_nb_FX)
    {
      XFloor_vol_j <-XFloor_FX_VOL[match(XFloor_devises_all[j],
                                         XFloor_FX_names)]
      XFloor_correlMatrix_modified[i,j] <- 
        (XFloor_vol_i^2 + XFloor_vol_j^2 - XFloor_cross_vol_floor[i,j]^2) /
        (2 *XFloor_vol_i * XFloor_vol_j)
      XFloor_correlMatrix_modified[j,i] <-XFloor_correlMatrix_modified[i,j]
    }
  }
  
  # STEP 4 if implied correl is not positive definite, 
  #        project to the nearest correl
  if(min(eigen(XFloor_correlMatrix_modified)$values)<0)
  {
    #Find near pd
    require(sfsmisc)
    cat(" --  Resulting correlation matrix is not positive definite ==> projecting to the nearest correlation -- \n\n")
    XFloor_correlMatrix_PD <- nearcor(XFloor_correlMatrix_modified)$cor  
  }
  else
  {
    XFloor_correlMatrix_PD <- XFloor_correlMatrix_modified
  }  
  
  # STEP 5 formatting of output correlation matrix 
  corr_FX_XFloor <- format_Correl_Matrix(paste("FX",devises_all,sep=""),
                                         XFloor_correlMatrix_PD)
}

data_count<-list(dev_names=c(emerging_currencies,
                             peggCurrencies,
                             developed_Currencies),nb_data=nb_data)

Emerging_currencies  =list(cluster_1_5=cluster_1_5,
                           cluster_6_14=cluster_6_14,
                           cluster_15_20=cluster_15_20,
                           rdtClusters=rdtClusters,
                           vol_regime_normal=vol_regime_normal,
                           tp=tp,
                           currency_crisis=currency_crisis)
Developed_currencies =list(dev_FX_Vol=dev_FX_Vol,
                           vol=vol,
                           diff_log_fx=diff_log_fx)
Pegg_Currencies      =list(peggCurrencies_vol=peggCurrencies_vol,
                           peggCurrencies_rdt_rating=peggCurrencies_rdt_rating)

cat("\n",format(Sys.time(), "%a %b %d %X %Y"), 
    "End of FX Model Calibration\n")

return(list(CalibrationName=CalibrationName,
            Emerging_currencies=Emerging_currencies,
            Developed_currencies=Developed_currencies,
            Pegg_Currencies=Pegg_Currencies,
            corr_FX=corr_FX,
            corr_FX_XFloor=corr_FX_XFloor,
            Emerging_currencies_list=emerging_currencies,
            Developed_currencies_list=developed_Currencies,
            pegg_currencies_list=peggCurrencies,data_count=data_count,
            crisis_res_keepall=crisis_res_keepall
))
}

	}
