
######################################### IR #########################################
quantiles_moyennes_diffusion_exp_vasicek = function(a,b,sigma,N){
	liste_pas_temps = c(1,3,5,10,30)
	moy = rep(0,5)
	q1 = rep(0,5)
	q99 = rep(0,5)
	
	pas_temps = 1
	nb_pas_temps = 30 / pas_temps
	x = b + rnorm(N) * sigma / sqrt(2*a)
	y = 0
	ti = 0
	for(i in c(1:nb_pas_temps)) # On diffuse 30 pas de temps...
	{
		ti = i * pas_temps
		alea = rnorm(N)
		x = (x-b) * exp(-a*pas_temps) + b + sigma * alea * sqrt( (1- exp(-2*a*pas_temps)) / (2*a) )
		y = y + exp(x)
		ind = match(ti,liste_pas_temps) # ... mais on ne garde que les pas de temps suivants (1, 3, 5, 10, 30)
		if(!is.na(ind))
		{
			T_i = liste_pas_temps[ind]
			moy[ind] = mean(y/T_i)
			q1[ind] = quantile(y/T_i,0.01)
			q99[ind] = quantile(y/T_i,0.99)
		}
	}
	res = 0
	res$moy = moy
	res$q1 = q1
	res$q99 = q99
	return(res)
}
QuantMoy_Theo = function(CI,a_infla,b_infla,sigma_infla,N=1e5){ #
	for(i in 1:CI$nbInfla) #1:4
	{
		ind_deb = 5 * (i-1) + 1
		a = a_infla[ind_deb]
		b = b_infla[ind_deb]
		sigma = sigma_infla[ind_deb]
		res = quantiles_moyennes_diffusion_exp_vasicek(a,b,sigma,N) #
		
		if(i==1)
		{
			moy_infla = res$moy
			q1_infla = res$q1
			q99_infla = res$q99
		}else{
			moy_infla= c(moy_infla, res$moy)
			q1_infla= c(q1_infla, res$q1)
			q99_infla= c(q99_infla, res$q99)
		}
	}
	return(list(moy_infla=moy_infla,q1_infla=q1_infla,q99_infla=q99_infla))
}






######################################### DIV ########################################
TruncateHistoDatas = function(dates,Histo,Begin,End,Spot=NULL){
	dates = as.Date(dates)
	idx_dates = which(dates>=as.Date(Begin) & dates <=as.Date(End))
	dates = dates[idx_dates]
	Histo = Histo[idx_dates,]
	if(!is.null(Spot)){
		Spot = Spot[idx_dates,]
		return(list(dates=dates,Histo=Histo,Spot=Spot))
	}else{
		return(list(dates=dates,Histo=Histo))
	}
}
Days = function(Begin,End){
	dates = seq(from=Begin,to=End,by=1)
	dates = sort(dates,decreasing = FALSE)
	n = length(dates)
	return(list(dates=dates,n=n))
}	
NetWorkingDays = function(dates,h,Calendar){ #
	dates_new = dates[isBusinessDay(Calendar,dates)] #
	idx_dates = match(dates_new,dates)
	h = h[idx_dates,]
	return(list(dates=dates_new,h=h))
}





####################################### Global #######################################
dirCreate = function(CI,path_dir){
	dir.create(CI$path_dir,showWarnings=FALSE,recursive=FALSE)
	
	dir.create(CI$ResPath,showWarnings=FALSE,recursive=FALSE)
	dir.create(CI$RefResPath,showWarnings=FALSE,recursive=FALSE)
	dir.create(CI$ProjResPath,showWarnings=FALSE,recursive=FALSE)
	
	dir.create(paste(CI$path_dir,"ASE",sep=""),showWarnings=FALSE,recursive=FALSE)
	dir.create(paste(CI$path_dir,"TESTS IN SAMPLE",sep=""),showWarnings=FALSE,recursive=FALSE)
	dir.create(paste(CI$path_dir,"TESTS OUT OF SAMPLE",sep=""),showWarnings=FALSE,recursive=FALSE)
}
ComputeReturns = function(dates,Histo,nbFacteurs){

	#dates = as.Date(as.character(dates))
	#nb_dates = length(dates)
	log_Histo = log(Histo) #cbind(as.data.frame(dates),log(Histo))
	d_log_Histo = matrix(NA,nrow(log_Histo),ncol(log_Histo))
	d_log_Histo = apply(log_Histo,2,diff)
	#colnames(d_log_Histo) = c("dates",colnames(d_log_Histo)[2:ncol(d_log_Histo)])
	
	return(list(log_datas=log_Histo,d_log_datas=d_log_Histo))
}
ComputeYieldReturns = function(dates,Histo){ #???? appliquer à tous les risk factors?
	dates = as.Date(as.character(dates))
	log_Histo = log(Histo)
	years = unique(format(dates,"%Y"))
	d_log_Histo = matrix(NA,nrow(log_Histo),ncol(log_Histo))
	# CALCUL DES LOG RENDEMENTS EXCEPTE DU PREMIER JOUR DE CHAQUE ANNEE
	for(year in years){
		idx = which(format(dates,"%Y") == year)
		d_log_Histo[idx[-1],] = apply(log_Histo[idx,],2,diff)
	}
	return(d_log_Histo)
}

vol_fractile = function(x,minus_moy=FALSE,type=1){

	if(!is.null(ncol(x))){
		vol_sd = as.double(apply(x,2,sd,na.rm=TRUE))
		m = as.double(apply(x,2,mean,na.rm=TRUE))
		if(minus_moy){x = t( t(x) - m )}
		q1 = as.double(apply(x,2,quantile,probs=0.01,type=type,na.rm=TRUE))
		q99 = as.double(apply(x,2,quantile,probs=0.99,type=type,na.rm=TRUE))
	}else{
		vol_sd = as.double(sd(x,na.rm=TRUE))
		m = as.double(mean(x,na.rm=TRUE))
		if(minus_moy){x = x - m}
		q1 = as.double(quantile(x,probs=0.01,type=type,na.rm=TRUE))
		q99 = as.double(quantile(x,probs=0.99,type=type,na.rm=TRUE))
	}
	
	vol_q1 = (q1 - m) / qnorm(0.01)
	vol_q99 = (q99 - m) / qnorm(0.99)
	
	vol_f = pmax(vol_sd,vol_q1,vol_q99)
	
	return(list(vol_sd=vol_sd,q1=q1,vol_q1=vol_q1,q99=q99,vol_q99=vol_q99,vol_f=vol_f))
}
vol_fractile2 = function(x){
	x = x[!is.na(x)]
	b = mean(x,na.rm=TRUE)
	n = length(x)
	vol_sd = sqrt(mean(x^2,na.rm=TRUE))
	vol_1p = (quantile(x,0.01) - b) / qnorm(0.01)
	vol_99p = (quantile(x,0.99) - b) / qnorm(0.99)
	res = max(vol_sd,vol_1p,vol_99p)
	return(res)
}
vol_fractile3 = function(x){ #LT_CRE #CT_IR
	x = x[!is.na(x)]
	b = mean(x)
	n = length(x)
	vol_std = sd(x,na.rm=TRUE) * sqrt(1-1/n)
	vol_1_pct = (quantile(x,0.01) - b) / qnorm(0.01)
	vol_99_pct = (quantile(x,0.99) - b) / qnorm(0.99)
	res = max(vol_std,vol_1_pct,vol_99_pct)
	return(res)
}
vol_standard = function (x){
	x = x[!is.na(x)]
	b = mean(x)
	n = length(x)
	vol_std = sd(x,na.rm = T)*sqrt(1-1/n)
	return(vol_std)
}

vol_fractile_ct_CRE = function (x){
  x = x[!is.na(x)]
  b = 0
  n = length(x)
  vol_std = sqrt(mean(x^2,na.rm = T))
  vol_1_pct = (quantile(x,0.01) - b) /qnorm(0.01)
  vol_99_pct = (quantile(x,0.99) - b) /qnorm(0.99)
  res = max(vol_std,vol_1_pct,vol_99_pct)
  return(res)
}
vol_fractile_lt_CRE = function (x){ #=vol_fractile_ct_IR
  x = x[!is.na(x)]
  b = mean(x)
  n = length(x)
  vol_std = sd(x,na.rm=TRUE) * sqrt(1-1/n) 
  vol_1_pct = (quantile(x,0.01) - b) / qnorm(0.01)
  vol_99_pct = (quantile(x,0.99) - b) / qnorm(0.99)
  res = max(vol_std,vol_1_pct,vol_99_pct)
  return(res)
}
