
# IR: au premier passage le code récupère bien l'historique total MULTI_COURBES (17 devises + inflation)
# IR: il effectue ensuite la calibration MONO_COURBES et va à ce moment-là boucler sur les historiques

rm(list=ls())
#rm(list=setdiff(ls(), "HISTO"))
T1 <- Sys.time()

Desktop		 = "D:/mneri092115/Modelisation/calibration/"
Script_Path  = paste(Desktop,"Script/",sep="")

source(paste(Script_Path,"Lib.r",sep=""))

Name = "CQ1_2016"
DateRef = as.Date("1899-12-30",format="%Y-%m-%d")
Begin_STD = as.Date("2006-01-01",format="%Y-%m-%d")
End_STD = as.Date("2016-01-01",format="%Y-%m-%d")
Begin_STR = as.Date("2008-09-02",format="%Y-%m-%d")
End_STR = as.Date("2011-09-02",format="%Y-%m-%d")
export=TRUE

nettoyage_mode = 7

Base_IR = c("HKD","USD","DKK","NZD","EUR","SGD","GBP","CHF","CZK","SEK","NOK","JPY","CAD","AUD","KRW","PLN","HUF",  #MULTI_COURBES
			"XAU","XAG","SAR","ZAR","THB","IDR","MYR","PHP","EGP",													#MONO_COURBES
			"TWD","TRY","ILS","ISK","RUB","RON","ARS","CNY","MXN",													#MONO_COURBES
			"BGN","AED","COP","KWD","TND","MAD","CLP","BRL","INR","PEN")											#MONO_COURBES

#IR_STD = StructureRF(Begin=Begin_STD,End=End_STD,DateRef=DateRef,Name=Name,Regime="STD",Base=Base_IR,Type="IR",nettoyage_mode=nettoyage_mode)
#IR_STR = StructureRF(Begin=Begin_STR,End=End_STR,DateRef=DateRef,Name=Name,Regime="STR",Base="",Type="IR",nettoyage_mode=nettoyage_mode)





StructureRF = function(Begin,End,DateRef,Name,Regime,Base,Type,nettoyage_mode=7){
	if(Type=="IR"){
		return(Structure_IR(Begin=Begin,End=End,DateRef=DateRef,Name=Name,Regime=Regime,Base=Base,Type=Type,nettoyage_mode=nettoyage_mode))
	}else if(Type==""){
		
	}
}
Structure_IR = function(Begin,End,DateRef,Name,Regime,Base,Type,nettoyage_mode=7){
	CI = list()
	CI$Type = Type #"IR"
	CI$Name = Name
	CI$Begin = Begin
	CI$End = End
	CI$DateRef = DateRef
	CI$Regime = Regime
	CI$Mode = 10
	if(Regime=="STR"){CI$Mode=3}
	
	CI$DataPath			= paste("D:/mneri092115/Modelisation/calibration/",Type,"/Data/",sep="")
	CI$ParamPath		= paste("D:/mneri092115/Modelisation/calibration/",Type,"/Parametres/",sep="")
	CI$RefDataPath 		= paste(CI$DataPath,"/Historiques_Ref/",sep="")
	CI$ProjDataPath 	= paste(CI$DataPath,"/Historiques_Proj/",sep="")
	
	CI$ResPath			= paste("D:/mneri092115/Modelisation/calibration/",Type,"/",Name,"/RESULTS/",sep="")
	CI$RefResPath		= paste(CI$ResPath,"REF/",sep="")
	CI$RefMultiResPath 	= paste(CI$RefResPath,"MULTI_COURBES/",sep="")
	CI$RefMonoResPath 	= paste(CI$RefResPath,"MONO_COURBES/",sep="")
	CI$ProjResPath		= paste(CI$ResPath,"PROJ/",sep="")
	
	CI$Tenors = c("1M","3M","6M","1Y","3Y","5Y","10Y","30Y")
	CI$TenorsInfla = c("1Y","3Y","5Y","10Y","30Y")
	CI$Base = Base
	CI$BaseInfla = c("inflaEUR","inflaUSD","inflaGBP","inflaJPY")
	CI$ProjInfla = c("inflaEUR HT","inflaFRF","inflaITL","inflaESP","inflaEUR AT","inflaBEL")
	
	CI$nbDevisesMulti = 17
	CI$nbDevisesMono = 28
	CI$nbInfla = 4
	
	CI$nbFacteurs = 156 #CI$NbDevisesMulti * length(CI$Tenors) + length(CI$TenorsInfla) * length(CI$ListeInflaRef)
	CI$nbFacteursMulti = 136 #CI$NbDevisesMulti * length(CI$Tenors)
	CI$nbFacteursInfla = 20 #CI$NbInfla * length(CI$TenorsInfla)
	CI$nbFacteursMono = 224 #CI$NbDevisesMono * length(CI$Tenors)
	CI$Shift = as.vector(read.csv(paste(CI$ParamPath,"liste_devises.csv",sep=""),sep=";",header=F)[,2])
	CI$ShiftInfla = as.vector(read.csv(paste(CI$ParamPath,"parametres_inflation.csv",sep=""),sep=";",header=T)[,2])
	CI$TypeCalib = as.vector(read.csv(paste(CI$ParamPath,"type_calibration.csv",sep=""),sep=";",header=F)[,2])
	
	CI$typeVol = 2
	CI$p_ct_opt = 1e-2 #nearcor
	CI$p_lt_opt = 1e-4 #nearcor
	
	CI$lambdaMulti = 90
	CI$lambdaMonoCorrelHisto = 15
	CI$lambdaMonoOther = 10
	
	CI$clean_data = 1
	CI$nettoyage_mode = nettoyage_mode
	
	CI$path_dir = paste("D:/mneri092115/Modelisation/calibration/",Type,"/",Name,"/",sep="") #calibration/IR/Q1-16/
	dir.create(CI$path_dir,showWarnings=FALSE,recursive=FALSE)
	dir.create(CI$ResPath,showWarnings=FALSE,recursive=FALSE)
	dir.create(CI$RefResPath,showWarnings=FALSE,recursive=FALSE)
	dir.create(CI$RefMultiResPath,showWarnings=FALSE,recursive=FALSE)
	dir.create(CI$RefMonoResPath,showWarnings=FALSE,recursive=FALSE)
	dir.create(CI$ProjResPath,showWarnings=FALSE,recursive=FALSE)
	
	return(CI)
}

GetHisto_Ref = function(CI){
	
	if(CI$Type == "IR"){
		HISTO = GetHisto_Ref_IR_MULTI(CI)
	}else if(CI$Type == ""){
		HISTO = "..."
	}
	
}
GetHisto_Ref_IR_MULTI_OLD = function(CI){

	for(i in 1:CI$nbDevisesMulti)
	{
		h = read.csv(paste(CI$RefDataPath, CI$Base[i], ".csv", sep=""), sep=";", header=TRUE) # On ne garde pas le header
		dates = h$dates = as.Date(h$dates, origin=CI$DateRef,format="%Y-%m-%d")
		idx_dates = which(dates >= CI$Begin & dates <= CI$End)
		h = cbind(h[idx_dates,1], (h[idx_dates,2:(length(CI$Tenors)+1)] + CI$Shift[i])) # On ne garde que les dates voulues et on shift
		colnames(h) = c("dates", paste(CI$Base[i],CI$Tenors,sep="_"))
		rownames(h) = NULL
		
		if(i==1){
			Histo = h
		}else{
			Histo = merge(Histo,h,by="dates",all.x=TRUE,all.y=TRUE)
		}
	}
	
	h = read.csv(paste(CI$RefDataPath, "inflation.csv", sep=""), sep=";", header=TRUE)
	dates = h$dates = as.Date(h$dates, origin=CI$DateRef, format="%Y-%m-%d")
	idx_dates = which(dates >= CI$Begin & dates <= CI$End)
	h = h[idx_dates,2:length(h)]
	for(i in 1:length(h)) { h[,i] = h[,i] + as.vector(CI$ShiftInfla)[i] } #On shift l'infla
	h = cbind(dates[idx_dates], h)
	colnames(h) = c("dates",paste(expand.grid(CI$TenorsInfla,CI$BaseInfla)[,2],expand.grid(CI$TenorsInfla,CI$BaseInfla)[,1],sep=" "))
	rownames(h) = NULL
	
	Histo = merge(Histo,h,by="dates",all.x=TRUE,all.y=TRUE)
	
	res = ComputeReturns(Histo$dates,Histo[,-1],CI$nbFacteurs)
	
	if(CI$clean_data == 1){
		res$log_datas = CleanConstData(res$log_datas, seuil_cleaning_data=CI$nettoyage_mode)
		res$d_log_datas = CleanConstData(res$d_log_datas, seuil_cleaning_data=CI$nettoyage_mode)
	}
	
	write.table(Histo,paste(CI$RefMultiResPath,"histo_shifte_all.csv",sep=""),sep=";",row.names=F)
	write.table(res$log_datas,paste(CI$RefMultiResPath,"log_histo_all.csv",sep=""),sep=";",row.names=F)
	write.table(res$d_log_datas,paste(CI$RefMultiResPath,"d_log_histo_all.csv",sep=""),sep=";",row.names=F)
	
	return(list(datas=Histo,log_datas=res$log_datas,d_log_datas=res$d_log_datas))
}
GetHisto_Ref_IR_MULTI = function(CI,i){

	h = read.csv(paste(CI$RefDataPath, CI$Base[i], ".csv", sep=""), sep=";", header=TRUE) # On ne garde pas le header
	dates = h$dates = as.Date(h$dates, origin=CI$DateRef,format="%Y-%m-%d")
	idx_dates = which(dates >= CI$Begin & dates <= CI$End)
	h = cbind(h[idx_dates,1], (h[idx_dates,2:(length(CI$Tenors)+1)] + CI$Shift[i])) # On ne garde que les dates voulues et on shift
	colnames(h) = c("dates", paste(CI$Base[i],CI$Tenors,sep="_"))
	rownames(h) = NULL
	
	res = ComputeReturns(h$dates,h[,-1],length(CI$Tenors))
	
	if(CI$clean_data == 1){
		res$log_datas = CleanConstData(res$log_datas, seuil_cleaning_data=CI$nettoyage_mode)
		res$d_log_datas = CleanConstData(res$d_log_datas, seuil_cleaning_data=CI$nettoyage_mode)
	}
	
	return(list(datas=h,log_datas=res$log_datas,d_log_datas=res$d_log_datas))
}
GetHisto_Ref_IR_MULTI_Infla = function(CI){

	h = read.csv(paste(CI$RefDataPath, "inflation.csv", sep=""), sep=";", header=TRUE)
	dates = h$dates = as.Date(h$dates, origin=CI$DateRef, format="%Y-%m-%d")
	idx_dates = which(dates >= CI$Begin & dates <= CI$End)
	h = h[idx_dates,2:length(h)]
	for(i in 1:length(h)) { h[,i] = h[,i] + as.vector(CI$ShiftInfla)[i] } #On shift l'infla
	h = cbind(dates[idx_dates], h)
	colnames(h) = c("dates",paste(expand.grid(CI$TenorsInfla,CI$BaseInfla)[,2],expand.grid(CI$TenorsInfla,CI$BaseInfla)[,1],sep=" "))
	rownames(h) = NULL
	
	res = ComputeReturns(h$dates,h[,-1],length(CI$nbFacteursInfla))
	
	if(CI$clean_data == 1){
		res$log_datas = CleanConstData(res$log_datas, seuil_cleaning_data=CI$nettoyage_mode)
		res$d_log_datas = CleanConstData(res$d_log_datas, seuil_cleaning_data=CI$nettoyage_mode)
	}
	
	return(list(datas=h,log_datas=res$log_datas,d_log_datas=res$d_log_datas))
}
GetHisto_Ref_IR_MONO = function(CI,i){
	
	ind = i + 17 # Les 17 premières devises sont calibrées conjointement
	h = read.csv(paste(CI$RefDataPath, CI$Base[ind], ".csv", sep=""), sep=";", header=TRUE) # On ne garde pas le header
	dates = h$dates = as.Date(h$dates, origin=CI$DateRef,format="%Y-%m-%d")
	idx_dates = which(dates >= CI$Begin & dates <= CI$End)
	h = cbind(h[idx_dates,1], (h[idx_dates,2:(length(CI$Tenors)+1)] + CI$Shift[ind])) # On ne garde que les dates voulues et on shift
	colnames(h) = c("dates", paste(CI$Base[ind],CI$Tenors,sep="_"))
	rownames(h) = NULL
	
	res = ComputeReturns(h$dates,h[,-1],length(CI$Tenors))
	
	if(CI$clean_data == 1)
	{
		res$log_datas = CleanConstData(res$log_datas, seuil_cleaning_data=CI$nettoyage_mode)
		res$d_log_datas = CleanConstData(res$d_log_datas, seuil_cleaning_data=CI$nettoyage_mode)
	}
	
	return(list(datas=h,log_datas=res$log_datas,d_log_datas=res$d_log_datas))
}

CalibrationRef = function(CI){
	if(CI$Type!="IR"){
		params = VasicekMultiFactorFractileModel(CI)
	}else{
		Vol = RefVolatility_IR(CI)
		HISTO = Vol$MULTI$HISTO
		params = VarianceCov_IR(CI,HISTO,Vol)
	}
	return(params)
}

RefVolatility_IR = function(CI){
	
	MULTI = RefVol_IR_MULTI(CI)
	MONO = RefVol_IR_MONO(CI)
	
	return(list(MULTI=MULTI,MONO=MONO))
}
RefVol_IR_MULTI_OLD = function(CI,HISTO){
	
	x = HISTO$d_log_datas[,-1]
	y = HISTO$log_datas[,-1]
	
	nbTenors = length(CI$Tenors)
	
	Q_LT_99p 	= read.csv(paste(CI$ParamPath,"Q_LT_99p.csv",sep=""),sep=";",header=F)
	Q_LT_1p 	= read.csv(paste(CI$ParamPath,"Q_LT_1p.csv",sep=""),sep=";",header=F)
	Moy_LT	 	= read.csv(paste(CI$ParamPath,"Moy_LT_v2.csv",sep=""),sep=";",header=F)
	
	Vol_CT 		= rep(0,CI$nbFacteurs)
	vol_sd		= rep(0,CI$nbFacteurs)
	Vol_LT 		= rep(0,CI$nbFacteurs)
	R_LT 		= rep(0,CI$nbFacteurs)
	
	Vol_CT 		= sapply(x,vol_fractile3) #2 et 3 OK	# Calcul pour tous les facteurs (infla inclue)
	vol_sd[1:CI$nbFacteursMulti] = sapply(y[,1:CI$nbFacteursMulti],vol_fractile3) #3 OK, 2 NOK
	vol_sd[(CI$nbFacteursMulti+1):CI$nbFacteurs] = sapply(y[,(CI$nbFacteursMulti+1):CI$nbFacteurs],vol_standard)
	
	for(i in 1:CI$nbDevisesMulti)
	{
		ind_start_i = (i-1) * nbTenors + 1
		ind_end_i = i * nbTenors
		
		q99 = Q_LT_99p[i,-1] + CI$Shift[i]
		q1 = Q_LT_1p[i,-1] + CI$Shift[i]
		m = Moy_LT[i,-1] + CI$Shift[i]
		
		R_LT[ind_start_i:ind_end_i] = m
		
		vol_LT_1p 	= pmax( qnorm(0.01) - sqrt(qnorm(0.01)^2-2*log(q1 /m)),qnorm(0.01) + sqrt(qnorm(0.01)^2-2*log(q1 /m)) )
		vol_LT_99p 	= pmin( qnorm(0.99) - sqrt(qnorm(0.99)^2-2*log(q99/m)),qnorm(0.99) + sqrt(qnorm(0.99)^2-2*log(q99/m)) )
		
		for (v in 1:nbTenors)
		{
			u = ind_start_i+v-1
			Vol_LT[u] = pmax( vol_sd[u],vol_LT_1p[v],vol_LT_99p[v] )
		}
	}
	
	parametres_inflation = read.csv(paste(CI$ParamPath,"parametres_inflation.csv",sep=""),sep=";",header=T)
	
	Q_LT_99p_infla 	= as.data.frame(parametres_inflation[,5])
	Q_LT_1p_infla 	= as.data.frame(parametres_inflation[,3])
	Moy_LT_infla 	= as.data.frame(parametres_inflation[,4])
	
	ind_start_infla = CI$nbDevisesMulti * length(CI$Tenors) + 1
	ind_end_infla 	= ind_start_infla + CI$nbFacteursInfla - 1
	
	q99 = unlist(Q_LT_99p_infla + CI$ShiftInfla)
	q1 = unlist(Q_LT_1p_infla + CI$ShiftInfla)
	m = unlist(Moy_LT_infla + CI$ShiftInfla)
	
	R_LT[ind_start_infla:ind_end_infla] = m
	
	vol_LT_1p 	= pmax( qnorm(0.01) - sqrt(qnorm(0.01)^2-2*log(q1 /m)),qnorm(0.01) + sqrt(qnorm(0.01)^2-2*log(q1 /m)) )
	vol_LT_99p 	= pmin( qnorm(0.99) - sqrt(qnorm(0.99)^2-2*log(q99/m)),qnorm(0.99) + sqrt(qnorm(0.99)^2-2*log(q99/m)) )
	
	for(v in 1:CI$nbFacteursInfla) #1:20
	{
		u = ind_start_infla+v-1
		Vol_LT[u] =  pmax( vol_sd[u],vol_LT_1p[v],vol_LT_99p[v] )
	}
	
	a_infla 	= 260 * Vol_CT[ind_start_infla:ind_end_infla]^2 / (2 * Vol_LT[ind_start_infla:ind_end_infla]^2)
	b_infla 	= log(m) - (0.5 * Vol_LT[ind_start_infla:ind_end_infla]^2)
	sigma_infla = sqrt(260) * Vol_CT[ind_start_infla:ind_end_infla]
	
	N_infla=1e5
	
	res = QuantMoy_Theo(CI,a_infla,b_infla,sigma_infla,N_infla)
	
	moy_infla = res$moy_infla - CI$ShiftInfla; 	m = res$moy_infla
	q1_infla = res$q1_infla - CI$ShiftInfla; 	q1 = res$q1_infla
	q99_infla = res$q99_infla - CI$ShiftInfla; 	q99 = res$q99_infla
	
	parametres_eco_infla = data.frame(a_infla,moy_infla,q1_infla,q99_infla)
	write.table(parametres_eco_infla,paste(CI$RefMultiResPath,"parametres_eco_infla.csv",sep=""),sep=";",row.names=T)
	
	################ On recalcule la vol à partir des quantiles et moyenne théoriques ################
	vol_LT_1p 	= pmax( qnorm(0.01) - sqrt(qnorm(0.01)^2-2*log(q1 /m)),qnorm(0.01) + sqrt(qnorm(0.01)^2-2*log(q1 /m)) )
	vol_LT_99p 	= pmin( qnorm(0.99) - sqrt(qnorm(0.99)^2-2*log(q99/m)),qnorm(0.99) + sqrt(qnorm(0.99)^2-2*log(q99/m)) )
	
	for(v in 1:CI$nbFacteursInfla) #1:20
	{
		u = ind_start_infla+v-1
		Vol_LT[u] =  pmax( vol_sd[u],vol_LT_1p[v],vol_LT_99p[v] )
	}
	
	return(list(CT=Vol_CT,LT=Vol_LT,SD=vol_sd))
}
RefVol_IR_MULTI = function(CI){
	
	nbTenors = length(CI$Tenors)
	
	Q_LT_99p 	= read.csv(paste(CI$ParamPath,"Q_LT_99p.csv",sep=""),sep=";",header=F)
	Q_LT_1p 	= read.csv(paste(CI$ParamPath,"Q_LT_1p.csv",sep=""),sep=";",header=F)
	Moy_LT	 	= read.csv(paste(CI$ParamPath,"Moy_LT_v2.csv",sep=""),sep=";",header=F)
	
	Vol_CT 		= rep(0,CI$nbFacteurs)
	vol_sd		= rep(0,CI$nbFacteurs)
	Vol_LT 		= rep(0,CI$nbFacteurs)
	R_LT 		= rep(0,CI$nbFacteurs)
	
	
	for(i in 1:CI$nbDevisesMulti)
	{
		
		HISTO_MULTI = GetHisto_Ref_IR_MULTI(CI,i)
		x = HISTO_MULTI$d_log_datas[,-1]
		y = HISTO_MULTI$log_datas[,-1]
		
		ind_start_i = (i-1) * nbTenors + 1
		ind_end_i = i * nbTenors
		
		Vol_CT[ind_start_i:ind_end_i] = sapply(x,vol_fractile3)
		vol_sd[ind_start_i:ind_end_i] = sapply(y,vol_standard)
		
		q99 = Q_LT_99p[i,-1] + CI$Shift[i]
		q1 = Q_LT_1p[i,-1] + CI$Shift[i]
		m = Moy_LT[i,-1] + CI$Shift[i]
		
		R_LT[ind_start_i:ind_end_i] = m
		
		vol_LT_1p 	= pmax( qnorm(0.01) - sqrt(qnorm(0.01)^2-2*log(q1 /m)),qnorm(0.01) + sqrt(qnorm(0.01)^2-2*log(q1 /m)) )
		vol_LT_99p 	= pmin( qnorm(0.99) - sqrt(qnorm(0.99)^2-2*log(q99/m)),qnorm(0.99) + sqrt(qnorm(0.99)^2-2*log(q99/m)) )
		
		for (v in 1:nbTenors)
		{
			u = ind_start_i+v-1
			Vol_LT[u] = pmax( vol_sd[u],vol_LT_1p[v],vol_LT_99p[v] )
		}
		
		if(i==1){
			histo_shifte_all = HISTO_MULTI$datas
			log_histo_all 	 = HISTO_MULTI$log_datas
			d_log_histo_all  = HISTO_MULTI$d_log_datas
		}else{
			histo_shifte_all = merge(histo_shifte_all,	HISTO_MULTI$datas,		by ="dates",all.x = T, all.y = T)
			log_histo_all 	 = merge(log_histo_all,		HISTO_MULTI$log_datas,	by ="dates",all.x = T, all.y = T)
			d_log_histo_all  = merge(d_log_histo_all,	HISTO_MULTI$d_log_datas,by ="dates",all.x = T, all.y = T)
		}
		
	}
	
	parametres_inflation = read.csv(paste(CI$ParamPath,"parametres_inflation.csv",sep=""),sep=";",header=T)
	
	Q_LT_99p_infla 	= as.data.frame(parametres_inflation[,5])
	Q_LT_1p_infla 	= as.data.frame(parametres_inflation[,3])
	Moy_LT_infla 	= as.data.frame(parametres_inflation[,4])
	
	HISTO_MULTI_Infla = GetHisto_Ref_IR_MULTI_Infla(CI)
	x = HISTO_MULTI_Infla$d_log_datas[,-1]
	y = HISTO_MULTI_Infla$log_datas[,-1]
	
	ind_start_infla = CI$nbDevisesMulti * length(CI$Tenors) + 1
	ind_end_infla 	= ind_start_infla + CI$nbFacteursInfla - 1
	
	Vol_CT[ind_start_infla:ind_end_infla] = sapply(x,vol_fractile3)
	vol_sd[ind_start_infla:ind_end_infla] = sapply(y,vol_standard)
	
	q99 = unlist(Q_LT_99p_infla + CI$ShiftInfla)
	q1 = unlist(Q_LT_1p_infla + CI$ShiftInfla)
	m = unlist(Moy_LT_infla + CI$ShiftInfla)
	
	R_LT[ind_start_infla:ind_end_infla] = m
	
	vol_LT_1p 	= pmax( qnorm(0.01) - sqrt(qnorm(0.01)^2-2*log(q1 /m)),qnorm(0.01) + sqrt(qnorm(0.01)^2-2*log(q1 /m)) )
	vol_LT_99p 	= pmin( qnorm(0.99) - sqrt(qnorm(0.99)^2-2*log(q99/m)),qnorm(0.99) + sqrt(qnorm(0.99)^2-2*log(q99/m)) )
	
	for(v in 1:CI$nbFacteursInfla) #1:20
	{
		u = ind_start_infla+v-1
		Vol_LT[u] =  pmax( vol_sd[u],vol_LT_1p[v],vol_LT_99p[v] )
	}
	
	a_infla 	= 260 * Vol_CT[ind_start_infla:ind_end_infla]^2 / (2 * Vol_LT[ind_start_infla:ind_end_infla]^2)
	b_infla 	= log(m) - (0.5 * Vol_LT[ind_start_infla:ind_end_infla]^2)
	sigma_infla = sqrt(260) * Vol_CT[ind_start_infla:ind_end_infla]
	
	N_infla=1e5
	
	res = QuantMoy_Theo(CI,a_infla,b_infla,sigma_infla,N_infla)
	
	moy_infla = res$moy_infla - CI$ShiftInfla; 	m = res$moy_infla
	q1_infla = res$q1_infla - CI$ShiftInfla; 	q1 = res$q1_infla
	q99_infla = res$q99_infla - CI$ShiftInfla; 	q99 = res$q99_infla
	
	parametres_eco_infla = data.frame(a_infla,moy_infla,q1_infla,q99_infla)
	write.table(parametres_eco_infla,paste(CI$RefMultiResPath,"parametres_eco_infla.csv",sep=""),sep=";",row.names=T)
	
	################ On recalcule la vol à partir des quantiles et moyenne théoriques ################
	vol_LT_1p 	= pmax( qnorm(0.01) - sqrt(qnorm(0.01)^2-2*log(q1 /m)),qnorm(0.01) + sqrt(qnorm(0.01)^2-2*log(q1 /m)) )
	vol_LT_99p 	= pmin( qnorm(0.99) - sqrt(qnorm(0.99)^2-2*log(q99/m)),qnorm(0.99) + sqrt(qnorm(0.99)^2-2*log(q99/m)) )
	
	for(v in 1:CI$nbFacteursInfla) #1:20
	{
		u = ind_start_infla+v-1
		Vol_LT[u] =  pmax( vol_sd[u],vol_LT_1p[v],vol_LT_99p[v] )
	}
	
	histo_shifte_all 	= merge(histo_shifte_all,HISTO_MULTI_Infla$datas,by ="dates",all.x = T, all.y = T)  
	log_histo_all 		= merge(log_histo_all,HISTO_MULTI_Infla$log_datas,by ="dates",all.x = T, all.y = T,) 
	d_log_histo_all	 	= merge(d_log_histo_all,HISTO_MULTI_Infla$d_log_datas,by ="dates",all.x = T, all.y = T) 
	names_facteurs 		= names(histo_shifte_all)[-1] 
	HISTO = list(datas=histo_shifte_all,log_datas=log_histo_all,d_log_datas=d_log_histo_all)
	
	return(list(CT=Vol_CT,LT=Vol_LT,SD=vol_sd,HISTO=HISTO))
}
RefVol_IR_MONO = function(CI){
	
	nbTenors = length(CI$Tenors)
	
	Q_LT_99p 	= read.csv(paste(CI$ParamPath,"Q_LT_99p.csv",sep=""),sep=";",header=F)
	Q_LT_1p 	= read.csv(paste(CI$ParamPath,"Q_LT_1p.csv",sep=""),sep=";",header=F)
	Moy_LT	 	= read.csv(paste(CI$ParamPath,"Moy_LT_v2.csv",sep=""),sep=";",header=F)
	
	nbFacteurs = CI$nbFacteursMono
	nbDevises = CI$nbDevisesMono
	
	Vol_CT 		= rep(0,nbFacteurs)
	vol_sd		= rep(0,nbFacteurs)
	Vol_LT 		= rep(0,nbFacteurs)
	R_LT 		= rep(0,nbFacteurs)
	
	for(i in 1:nbDevises){
		
		HISTO_MONO = GetHisto_Ref_IR_MONO(CI,i)
		x = HISTO_MONO$d_log_datas[,-1]
		y = HISTO_MONO$log_datas[,-1]
		
		ind_start_i = (i-1) * nbTenors + 1
		ind_end_i = i * nbTenors
		ind = i + 17
		
		Vol_CT[ind_start_i:ind_end_i] = sapply(x,vol_fractile3)
		vol_sd[ind_start_i:ind_end_i] = sapply(y,vol_standard)
		
		q99 = Q_LT_99p[ind,-1] + CI$Shift[ind]
		q1 = Q_LT_1p[ind,-1] + CI$Shift[ind]
		m = Moy_LT[ind,-1] + CI$Shift[ind]
		
		R_LT[ind_start_i:ind_end_i] = m
		
		vol_LT_1p 	= pmax( qnorm(0.01) - sqrt(qnorm(0.01)^2-2*log(q1 /m)),qnorm(0.01) + sqrt(qnorm(0.01)^2-2*log(q1 /m)) )
		vol_LT_99p 	= pmin( qnorm(0.99) - sqrt(qnorm(0.99)^2-2*log(q99/m)),qnorm(0.99) + sqrt(qnorm(0.99)^2-2*log(q99/m)) )
		
		for (v in 1:nbTenors){
			u = ind_start_i+v-1
			Vol_LT[u] = pmax( vol_sd[u],vol_LT_1p[v],vol_LT_99p[v] )
		}
	}
	
	return(list(CT=Vol_CT,LT=Vol_LT,SD=vol_sd,R_LT=R_LT))
}

VarianceCov_IR = function(CI,HISTO,Vol){

	MULTI = VarianceCov_IR_MULTI(CI,HISTO,Vol)
	MONO = VarianceCov_IR_MONO(CI,HISTO,Vol)
	
	return(list(MULTI=MULTI,MONO=MONO))
	#return(MONO)
}
VarianceCov_IR_MULTI_OLD = function(CI,HISTO,Vol){
	
	x = HISTO$d_log_datas[,-1]
	y = HISTO$log_datas[,-1]
	
	Vol_CT = Vol$MULTI$CT
	Vol_LT = Vol$MULTI$LT
	m	   = as.numeric(Vol$MULTI$R_LT)
	
	COR_CT = cor(x,use="pairwise.complete.obs")
	COR_LT = cor(y,use="pairwise.complete.obs")
	
	COR_CT_adj = nearcor(COR_CT,maxit=100000,posd.tol=CI$p_ct_opt)$cor
	COR_LT_adj = nearcor(COR_LT,maxit=100000,posd.tol=CI$p_lt_opt)$cor
	
	VCV_CT_adj = 260 * diag(Vol_CT) %*% COR_CT_adj %*% diag(Vol_CT)
	VCV_LT_adj = diag(Vol_LT) %*% COR_LT_adj %*% diag(Vol_LT)
	
	VCV_CT = 260 * diag(Vol_CT) %*% COR_CT %*% diag(Vol_CT)
	VCV_LT = diag(Vol_LT) %*% COR_LT %*% diag(Vol_LT)
	
	#Majoration des VCV
	ecart_CT_0 = VCV_CT - VCV_CT_adj
	ecart_LT_0 = VCV_LT - VCV_LT_adj
	
	P_CT_delta_0 = eigen(ecart_CT_0)$vectors
	D_CT_plus_delta_0 = pmax( eigen(ecart_CT_0)$values, 1e-7 )
	
	P_LT_delta_0 = eigen(ecart_LT_0)$vectors
	D_LT_plus_delta_0 = pmax( eigen(ecart_LT_0)$values, 1e-7 )
	
	ecart_CT_1 = P_CT_delta_0 %*% diag(D_CT_plus_delta_0) %*% P_CT_delta_0
	ecart_LT_1 = P_LT_delta_0 %*% diag(D_LT_plus_delta_0) %*% P_LT_delta_0
	
	VCV_CT = VCV_CT + ecart_CT_1 #VCV_CT_fin
	VCV_LT = VCV_LT + ecart_LT_1 #VCV_LT_fin
	
	lambda = CI$lambdaMulti
	dd = DoubleDiagonalisation(VCV_CT,VCV_LT,m,lambda,SVD=TRUE)
	
	resParam = data.frame(res$a,res$b,res$sigma)
	
	write.table(resParam,paste(CI$RefMultiResPath,"parametres_calibration_multi_courbes.csv" ,sep=""),sep=";",row.names=F)
	write.table(res$P,	 paste(CI$RefMultiResPath,"matrice_passage_multi_courbes.csv"		 ,sep=""),sep=";",row.names=F)
	write.table(res$invP,paste(CI$RefMultiResPath,"matrice_passage_inverse_multi_courbes.csv",sep=""),sep=";",row.names=F)
	
	sigma_CT = sqrt((P*P) %*% (sigma*sigma))
	sigma_LT = sqrt((0.5 * P * P) %*% (sigma*sigma/a))
	
	names_factors = HISTO$datas[-1,]
	res_final_CT = data.frame(names_factors,data.frame(sigma_CT)) 
	res_final_LT = data.frame(names_factors,data.frame(sigma_LT))
	
	write.table(res_final_CT,paste(CI$RefMultiResPath,"parametres_calibration_global_CT.csv",sep=""),sep=";",row.names =F)
	write.table(res_final_LT,paste(CI$RefMultiResPath,"parametres_calibration_global_LT.csv",sep=""),sep=";",row.names =F)
	
	#return(list(VCV_CT=VCV_CT,VCV_LT=VCV_LT,VCV_CT_adj=VCV_CT_adj,VCV_LT_adj=VCV_LT_adj))
}
VarianceCov_IR_MULTI = function(CI,HISTO,Vol){
	
	x = HISTO$d_log_datas[,-1]
	y = HISTO$log_datas[,-1]
	
	Vol_CT = Vol$MULTI$CT
	Vol_LT = Vol$MULTI$LT
	m	   = as.numeric(Vol$MULTI$R_LT)
	
	COR_CT = cor(x,use="pairwise.complete.obs")
	COR_LT = cor(y,use="pairwise.complete.obs")
	
	COR_CT_adj = nearcor(COR_CT,maxit=100000,posd.tol=CI$p_ct_opt)$cor
	COR_LT_adj = nearcor(COR_LT,maxit=100000,posd.tol=CI$p_lt_opt)$cor
	
	VCV_CT_adj = 260 * diag(Vol_CT) %*% COR_CT_adj %*% diag(Vol_CT)
	VCV_LT_adj = diag(Vol_LT) %*% COR_LT_adj %*% diag(Vol_LT)
	
	VCV_CT = 260 * diag(Vol_CT) %*% COR_CT %*% diag(Vol_CT)
	VCV_LT = diag(Vol_LT) %*% COR_LT %*% diag(Vol_LT)
	
	#Majoration des VCV
	ecart_CT_0 = VCV_CT - VCV_CT_adj
	ecart_LT_0 = VCV_LT - VCV_LT_adj
	
	P_CT_delta_0 = eigen(ecart_CT_0)$vectors
	D_CT_plus_delta_0 = pmax( eigen(ecart_CT_0)$values, 1e-7 )
	
	P_LT_delta_0 = eigen(ecart_LT_0)$vectors
	D_LT_plus_delta_0 = pmax( eigen(ecart_LT_0)$values, 1e-7 )
	
	ecart_CT_1 = P_CT_delta_0 %*% diag(D_CT_plus_delta_0) %*% P_CT_delta_0
	ecart_LT_1 = P_LT_delta_0 %*% diag(D_LT_plus_delta_0) %*% P_LT_delta_0
	
	VCV_CT = VCV_CT + ecart_CT_1 #VCV_CT_fin
	VCV_LT = VCV_LT + ecart_LT_1 #VCV_LT_fin
	
	lambda = CI$lambdaMulti
	dd = DoubleDiagonalisation(VCV_CT,VCV_LT,m,lambda,SVD=TRUE)
	
	resParam = data.frame(res$a,res$b,res$sigma)
	
	write.table(resParam,paste(CI$RefMultiResPath,"parametres_calibration_multi_courbes.csv" ,sep=""),sep=";",row.names=F)
	write.table(res$P,	 paste(CI$RefMultiResPath,"matrice_passage_multi_courbes.csv"		 ,sep=""),sep=";",row.names=F)
	write.table(res$invP,paste(CI$RefMultiResPath,"matrice_passage_inverse_multi_courbes.csv",sep=""),sep=";",row.names=F)
	
	sigma_CT = sqrt((P*P) %*% (sigma*sigma))
	sigma_LT = sqrt((0.5 * P * P) %*% (sigma*sigma/a))
	
	names_factors = HISTO$datas[-1,]
	res_final_CT = data.frame(names_factors,data.frame(sigma_CT)) 
	res_final_LT = data.frame(names_factors,data.frame(sigma_LT))
	
	write.table(res_final_CT,paste(CI$RefMultiResPath,"parametres_calibration_global_CT.csv",sep=""),sep=";",row.names =F)
	write.table(res_final_LT,paste(CI$RefMultiResPath,"parametres_calibration_global_LT.csv",sep=""),sep=";",row.names =F)
	
	#return(list(VCV_CT=VCV_CT,VCV_LT=VCV_LT,VCV_CT_adj=VCV_CT_adj,VCV_LT_adj=VCV_LT_adj))
}

VarianceCov_IR_MONO = function(CI,HISTO,Vol){
	
	Vol_CT 	= Vol$MONO$CT
	Vol_LT 	= Vol$MONO$LT
	R_LT 	= Vol$MONO$R_LT
	
	nbTenors = length(CI$Tenors)
	
	names_factors = rep(0,nbTenors*CI$nbDevisesMono) 

	parametres_factors = cbind(names_factors,names_factors,names_factors) 
	names(parametres_factors) = c("a" , "b" , "sigma") 
	parametres_factors=as.data.frame(parametres_factors) 

	parametres_factors_CT = cbind(names_factors) 
	names(parametres_factors_CT) = c("Vol_CT") 
	parametres_factors_CT=as.data.frame(parametres_factors_CT) 

	parametres_factors_LT = cbind(names_factors) 
	names(parametres_factors_LT) = c("Vol_LT") 
	parametres_factors_LT=as.data.frame(parametres_factors_LT) 
	
	idx = which(CI$Base == "USD")
	idx = (idx-1) * nbTenors + 2
	x = as.matrix(HISTO$d_log_datas[,idx:(idx+nbTenors-1)])
	y = as.matrix(HISTO$log_datas[,idx:(idx+nbTenors-1)])
	
	COR_USD_CT = cor(x,use = "pairwise.complete.obs")
	COR_USD_LT = cor(y,use = "pairwise.complete.obs")
	
	COR_USD_CT_adj = nearcor(COR_USD_CT,maxit=100000,posd.tol=CI$p_ct_opt)$cor
	COR_USD_LT_adj = nearcor(COR_USD_LT,maxit=100000,posd.tol=CI$p_lt_opt)$cor
	
	for(i in 1:CI$nbDevisesMono){
		
		ind_start_i = (i-1) * nbTenors + 1
		ind_end_i = i * nbTenors
		ind = i + 17
		
		type_calibration_i = CI$TypeCalib[ind]
		
		if(type_calibration_i == "correl_historique"){
		
			HISTO_MONO = GetHisto_Ref_IR_MONO(CI,i)
			x = HISTO_MONO$d_log_datas[,-1]
			y = HISTO_MONO$log_datas[,-1]
			
			COR_CT = cor(x,use = "pairwise.complete.obs")
			COR_LT = cor(y,use = "pairwise.complete.obs")
			
			COR_CT_adj = nearcor(COR_CT,maxit=100000,posd.tol=CI$p_ct_opt)$cor
			COR_LT_adj = nearcor(COR_LT,maxit=100000,posd.tol=CI$p_lt_opt)$cor
			
			VCV_CT_adj = 260 * diag(Vol_CT[ind_start_i:ind_end_i]) %*% COR_CT_adj %*% diag(Vol_CT[ind_start_i:ind_end_i])
			VCV_LT_adj = diag(Vol_LT[ind_start_i:ind_end_i]) %*% COR_LT_adj %*% diag(Vol_LT[ind_start_i:ind_end_i])
			
			VCV_CT = 260 * diag(Vol_CT[ind_start_i:ind_end_i]) %*% COR_CT %*% diag(Vol_CT[ind_start_i:ind_end_i])
			VCV_LT = diag(Vol_LT[ind_start_i:ind_end_i]) %*% COR_LT %*% diag(Vol_LT[ind_start_i:ind_end_i])
			
		}else{
			
			VCV_CT_adj = 260 * diag(Vol_CT[ind_start_i:ind_end_i]) %*% COR_USD_CT_adj %*% diag(Vol_CT[ind_start_i:ind_end_i])
			VCV_LT_adj = diag(Vol_LT[ind_start_i:ind_end_i]) %*% COR_USD_LT_adj %*% diag(Vol_LT[ind_start_i:ind_end_i])
			
			VCV_CT = 260 * diag(Vol_CT[ind_start_i:ind_end_i]) %*% COR_USD_CT %*% diag(Vol_CT[ind_start_i:ind_end_i])
			VCV_LT = diag(Vol_LT[ind_start_i:ind_end_i]) %*% COR_USD_LT %*% diag(Vol_LT[ind_start_i:ind_end_i])
			
		}
		
		ecart_CT_0 = VCV_CT - VCV_CT_adj
		ecart_LT_0 = VCV_LT - VCV_LT_adj
		
		P_CT_delta_0 = eigen(ecart_CT_0)$vectors
		D_CT_plus_delta_0 = pmax( eigen(ecart_CT_0)$values, 1e-7 )
		
		P_LT_delta_0 = eigen(ecart_LT_0)$vectors
		D_LT_plus_delta_0 = pmax( eigen(ecart_LT_0)$values, 1e-7 )
		
		ecart_CT_1 = P_CT_delta_0 %*% diag(D_CT_plus_delta_0) %*% P_CT_delta_0
		ecart_LT_1 = P_LT_delta_0 %*% diag(D_LT_plus_delta_0) %*% P_LT_delta_0
		
		VCV_CT = VCV_CT + ecart_CT_1 #VCV_CT_fin
		VCV_LT = VCV_LT + ecart_LT_1 #VCV_LT_fin
		
		m = as.numeric(R_LT[ind_start_i:ind_end_i])
		
		if(type_calibration_i=="correl_historique"){
			res = DoubleDiagonalisation(VCV_CT,VCV_LT,m,lambda=CI$lambdaMonoCorrelHisto,SVD=TRUE)
		}else{
			res = DoubleDiagonalisation(VCV_CT,VCV_LT,m,lambda=CI$lambdaMonoOther,SVD=TRUE)
		}
		
		resParam = data.frame(res$a,res$b,res$sigma)
		
		write.table(resParam,paste(CI$RefMonoResPath,CI$Base[ind],"_parametres_calibration.csv" ,sep=""),sep=";",row.names=F)
		write.table(res$P,	 paste(CI$RefMonoResPath,CI$Base[ind],"_matrice_passage.csv"		  ,sep=""),sep=";",row.names=F)
		write.table(res$invP,paste(CI$RefMonoResPath,CI$Base[ind],"_matrice_passage_inverse.csv",sep=""),sep=";",row.names=F)
		
		sigma_CT = sqrt((res$P*res$P) %*% (res$sigma*res$sigma))
		sigma_LT = sqrt((0.5 * res$P * res$P) %*% (res$sigma*res$sigma/res$a))
		
		names_factors[ind_start_i:ind_end_i] 			= paste(CI$Base[ind],CI$Tenors)
		parametres_factors[ind_start_i:ind_end_i]		= resParam
		parametres_factors_CT[ind_start_i:ind_end_i,1] 	= data.frame(sigma_CT)
		parametres_factors_LT[ind_start_i:ind_end_i,1] 	= data.frame(sigma_LT)
		
	}
	
	res_final = data.frame(names_factors,parametres_factors)
	res_final_CT = data.frame(names_factors,parametres_factors_CT)
	res_final_LT = data.frame(names_factors,parametres_factors_LT)
	
	write.table(res_final,   paste(CI$RefMonoResPath,"parametres_calibration_global.csv"   ,sep=""),sep=";",row.names =F) 
	write.table(res_final_CT,paste(CI$RefMonoResPath,"parametres_calibration_global_CT.csv",sep=""),sep=";",row.names =F) 
	write.table(res_final_LT,paste(CI$RefMonoResPath,"parametres_calibration_global_LT.csv",sep=""),sep=";",row.names =F) 
	
}
DoubleDiagonalisation_Rotation = function(CI,VCV_CT,VCV_LT,m,lambda,SVD=FALSE,type_calib=""){
	eigen_VCV_CT = eigen(VCV_CT)
	Q = eigen_VCV_CT$vectors
	D = sqrt(diag(eigen_VCV_CT$values))
	
	H = Q %*% D
	invH = solve(H)
	B = invH %*% VCV_LT %*% t(invH)
	eigen_VCV_LT = eigen(B)
	
	Phat = eigen_VCV_LT$vectors
	Dhat = eigen_VCV_LT$values # Var_LT
	
	P = H %*% Phat
	invP = solve(P)
	
	if(SVD){
	
		singular_value_decomposition = svd(P)
		SingVal = singular_value_decomposition$d
		MinS = min(SingVal)
		MaxS = max(SingVal)
		
		test = MinS/MaxS
		test_SVD = TRUE
		if(test < 1e-12)
		{
			MinS
			MaxS
			test_SVD = FALSE
		}
	}
	
	if(type_calib == "correl_historique"){ lambda = CI$lambdaMonoCorrelHisto } #Cas particulier dans calib IR MONO_COURBES
	
	Dhat = pmax(Dhat,lambda/260)
	a = -(260/2) * log(1 - 1 / (260 * Dhat))
	
	I = (Dhat == lambda/260)
	Sf = sum(I)
	if(Sf >= 1){ a = pmax(a, 0.1) } #Seuil minimum pour a
	
	sigma = sqrt(2 * a * Dhat)
	
	b = invP %*% (log(m) - 0.25 * (P*P) %*% (sigma*sigma / a))
	
	return(list(P=P,invP=invP,Dhat=Dhat,a=a,b=b,sigma=sigma,test_SVD=test_SVD))
}
DoubleDiagonalisation = function(VCV_CT,VCV_LT,m,lambda,SVD=FALSE){
	n = ncol(VCV_CT)
	eigen_VCV_CT = eigen(VCV_CT)
	Q = eigen_VCV_CT$vectors
	D = sqrt(diag(eigen_VCV_CT$values))
	H = Q %*% D
	invH = solve(H)
	B = invH %*% VCV_LT %*% t(invH)
	eigen_VCV_LT = eigen(B)
	Phat = eigen_VCV_LT$vectors
	Dhat = diag(eigen_VCV_LT$values) # Var_LT
	P = H %*% Phat
	invP = solve(P)
	PP =P^2
	
	if(SVD){
		singular_value_decomposition = svd(P)
		SingVal = singular_value_decomposition$d
		MinS = min(SingVal)
		MaxS = max(SingVal)
		
		test = MinS/MaxS
		test_SVD = TRUE
		if(test < 1e-12)
		{
			MinS
			MaxS
			test_SVD = FALSE
		}
	}
	
	a = numeric(n)
	b = numeric(n)
	sigma = numeric(n)
	for(i in seq(1,n,1)){
		d = max(lambda/260,Dhat[i,i])
		a[i] = -260*0.5*log(1-1/(260*d))
		Dhat[i,i] = d
		sigma[i] = sqrt(2*a[i]*d)
	}
	for(i in seq(1,n,1)){
		b[i]=log(m[i])-0.25*PP[i,]%*%((sigma^2)/a)
	}
	b=invP%*%b
	return(list(P=P,invP=invP,Dhat=Dhat,a=a,b=as.double(b),sigma=sigma,B=B))
}

VasicekMultiFactorFractileModel = function(datas,log_datas,returns,type=1){
	
}


IR_STD = StructureRF(Begin=Begin_STD,End=End_STD,DateRef=DateRef,Name=Name,Regime="STD",Base=Base_IR,Type="IR",nettoyage_mode=nettoyage_mode)

#HISTO = GetHisto_Ref(CI=IR_STD)

T2 <- Sys.time(); cat(paste("Durée récupération HISTO : ",round(T2-T1,digits=4))," secondes \n",sep="")

Vol = RefVolatility_IR(CI=IR_STD)

T3 <- Sys.time(); cat(paste("Durée calcul des volatilités : ",round(T3-T2,digits=4))," secondes \n",sep="")

HISTO = Vol$MULTI$HISTO
VCV = VarianceCov_IR(CI=IR_STD,HISTO,Vol)

T4 <- Sys.time(); cat(paste("Durée calcul des VCV : ",round(T4-T3,digits=4))," secondes \n",sep="")




cat(paste("Durée totale : ",round(Sys.time()-T1,digits=4))," secondes \n",sep="")



