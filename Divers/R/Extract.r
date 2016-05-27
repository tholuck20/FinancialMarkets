

####################################### Global #######################################
GetHisto_Ref = function(CI){ # Extract.r
	if(CI$Type == "IR"){
		#HISTO = GetHisto_Ref_IR_MULTI(CI)
	}else if(CI$Type == "VolATM"){
		HISTO = GetHisto_Ref_VolATM(CI=CI)
	}else if(CI$Type == "CRE"){
		HISTO = GetHisto_Ref_CRE(CI=CI)
	}else if(CI$Type == "BaseCorrel"){
		HISTO = GetHisto_Ref_BaseCorrel(CI=CI)
	}else if(CI$Type == "DIV"){
		HISTO = GetHisto_Ref_YIELD(CI=CI)
	}else if(CI$Type == ""){
		HISTO = "..."
	}
	
	#return(HISTO)
}
GetHisto_Ref_IR_MULTI = function(CI,i){ # Extract.r
	h = read.csv(paste(CI$RefDataPath, CI$Base[i], ".csv", sep=""), sep=";", header=TRUE) # On ne garde pas le header
	dates = h$dates = as.Date(h$dates, origin=CI$DateRef,format="%Y-%m-%d")
	idx_dates = which(dates >= CI$Begin & dates <= CI$End) #TruncateHistoDatas
	h = cbind(h[idx_dates,1], (h[idx_dates,2:(length(CI$Tenors)+1)] + CI$Shift[i])) # On ne garde que les dates voulues et on shift
	colnames(h) = c("dates", paste(CI$Base[i],CI$Tenors,sep="_"))
	rownames(h) = NULL
	
	log_Histo = cbind(dates=h$dates,log(h[,-1]))
	d_log_Histo = data.frame(dates=h$dates[-1],sapply(log_Histo[,-1],diff))
	colnames(d_log_Histo) = c("dates",colnames(d_log_Histo)[2:ncol(d_log_Histo)])
	
	if(CI$clean_data == 1){
		log_datas = CleanConstData(log_Histo,seuil_cleaning_data=CI$nettoyage_mode)
		d_log_datas = CleanConstData(d_log_Histo,seuil_cleaning_data=CI$nettoyage_mode)
	}
	
	return(list(datas=h,log_datas=log_datas,d_log_datas=d_log_datas))
}
GetHisto_Ref_IR_MULTI_Infla = function(CI){ # Extract.r
	h = read.csv(paste(CI$RefDataPath, "inflation.csv", sep=""), sep=";", header=TRUE)
	dates = h$dates = as.Date(h$dates, origin=CI$DateRef, format="%Y-%m-%d")
	idx_dates = which(dates >= CI$Begin & dates <= CI$End) #TruncateHistoDatas
	h = h[idx_dates,2:length(h)]
	for(i in 1:length(h)) { h[,i] = h[,i] + as.vector(CI$ShiftInfla)[i] } #On shift l'infla
	h = cbind(dates[idx_dates], h)
	colnames(h) = c("dates",paste(expand.grid(CI$TenorsInfla,CI$BaseInfla)[,2],expand.grid(CI$TenorsInfla,CI$BaseInfla)[,1],sep=" "))
	rownames(h) = NULL
	
	log_Histo = cbind(dates=h$dates,log(h[,-1]))
	d_log_Histo = data.frame(dates=h$dates[-1],sapply(log_Histo[,-1],diff))
	colnames(d_log_Histo) = c("dates",colnames(d_log_Histo)[2:ncol(d_log_Histo)])
	
	if(CI$clean_data == 1){
		log_datas = CleanConstData(log_Histo,seuil_cleaning_data=CI$nettoyage_mode)
		d_log_datas = CleanConstData(d_log_Histo,seuil_cleaning_data=CI$nettoyage_mode)
	}
	
	return(list(datas=h,log_datas=log_datas,d_log_datas=d_log_datas))
}
GetHisto_Ref_IR_MONO = function(CI,i){ # Extract.r
	ind = i + 17 # Les 17 premières devises sont calibrées conjointement
	h = read.csv(paste(CI$RefDataPath, CI$Base[ind], ".csv", sep=""), sep=";", header=TRUE) # On ne garde pas le header
	dates = h$dates = as.Date(h$dates, origin=CI$DateRef,format="%Y-%m-%d")
	idx_dates = which(dates >= CI$Begin & dates <= CI$End) #TruncateHistoDatas
	h = cbind(h[idx_dates,1], (h[idx_dates,2:(length(CI$Tenors)+1)] + CI$Shift[ind])) # On ne garde que les dates voulues et on shift
	colnames(h) = c("dates", paste(CI$Base[ind],CI$Tenors,sep="_"))
	rownames(h) = NULL
	
	log_Histo = cbind(dates=h$dates,log(h[,-1]))
	d_log_Histo = data.frame(dates=h$dates[-1],sapply(log_Histo[,-1],diff))
	colnames(d_log_Histo) = c("dates",colnames(d_log_Histo)[2:ncol(d_log_Histo)])
	
	if(CI$clean_data == 1)
	{
		log_datas = CleanConstData(log_Histo,seuil_cleaning_data=CI$nettoyage_mode)
		d_log_datas = CleanConstData(d_log_Histo,seuil_cleaning_data=CI$nettoyage_mode)
	}
	
	return(list(datas=h,log_datas=log_datas,d_log_datas=d_log_datas))
}

GetHisto_Ref_VolATM = function(CI){ # Extract.r
	h = read.csv(paste(CI$DataPath,"HistoIndex.csv",sep = ""),sep=";", header = TRUE)
	dates = as.Date(h$Date,format="%d/%m/%Y")
	histo = as.matrix(h[,-1])
	
	res = ComputeReturns(dates,histo,CI$nbFacteursRef)
	
	return(list(dates=dates,datas=histo,log_datas=res$log_datas,d_log_datas=res$d_log_datas))
}
GetHisto_Ref_CRE = function(CI){ # Extract.r
	h = read.csv(paste(CI$RefDataPath,"Spreads_histo.csv",sep=""),sep=",")
	h = cbind(Dates=as.Date(h$Dates,format="%d/%m/%Y"),h[,-1]/10000) #h = h / 10000 #l'historique est en bps, à remettre en niveaux de spreads
	histo = as.matrix(h[,-1])
	
	res = ComputeReturns(h$Dates,histo,CI$nbFacteursRef)
	
	return(list(dates=h$Dates,datas=histo,log_datas=res$log_datas,d_log_datas=res$d_log_datas))
}
GetHisto_Ref_BaseCorrel = function(CI){ # Extract.r
	h = read.csv(paste(CI$RefDataPath,"histo_Correl.csv",sep=""),sep=";",header=FALSE) # colonnes 2 à 16 = CDX, colonnes 17 à 31 = iTraxx
	h[,2:16] = h[,2:16] + CI$shiftCDX
	h[,17:length(h[1,])] = h[,17:length(h[1,])] + CI$shiftItraxx
	histo = as.matrix(h[,-1])
	
	dates = h[,1] = as.Date(h[,1], origin=CI$DateRef,format="%Y-%m-%d")
	
	res = ComputeReturns(dates,histo,CI$nbFacteursRef)
	
	return(list(dates=dates,datas=histo,log_datas=res$log_datas,d_log_datas=res$d_log_datas))
}
GetHisto_Ref_YIELD = function(CI){ # Extract.r 		#(GetYieldBaseHisto)
	n_base = length(CI$Base)
	Histo = c()
	spot = c()
	for(i in seq(1,n_base,1)){
		h = read.table(paste(CI$RefDataPath,CI$Base[i],"_YIELD.csv",sep=""),sep=",",header=TRUE)
		if(i==1){dates = as.character(h$Date)}
		h = data.frame(Date=h$Date,Spot=h$Spot,Y0=h$Y1,Y1=h$Y1,Y2=h$Y2,Y3=h$Y3,Y4=h$Y4,row.names=NULL)
		spot = cbind(spot,h$Spot,h$Spot,h$Spot,h$Spot,h$Spot)
		Histo = cbind(Histo,as.matrix(h[,-c(1,2)]))
	}
	res = TruncateHistoDatas(dates=dates,Histo=Histo,Begin=CI$Begin,End=CI$End,Spot=spot)
	dates = res$dates
	Histo = res$Histo + CI$ShiftIndex
	returns = ComputeYieldReturns(dates=dates,Histo=Histo)
	return(list(dates=dates,datas=as.matrix(Histo),log_datas=log(Histo),d_log_datas=returns,spot=res$Spot))
}



GetHisto_Proj_YIELD = function(CI,stock){
	h = read.table(paste(CI$ProjDataPath,stock,"_YIELD.csv",sep=""),sep=",",header=TRUE)
	dates = as.Date(h$Date)
	h = data.frame(Date=h$Date,Spot=h$Spot,Y0=h$Y1,Y1=h$Y1,Y2=h$Y2,Y3=h$Y3,Y4=h$Y4,row.names=NULL)
	spot = cbind(h$Spot,h$Spot,h$Spot,h$Spot,h$Spot)
	Histo = as.matrix(h[,-c(1,2)])
	res = TruncateHistoDatas(dates=dates,Histo=Histo,Begin=CI$Begin,End=CI$End,Spot=spot)
	dates = res$dates
	Histo = res$Histo + CI$ShiftStock
	returns = ComputeYieldReturns(dates=dates,Histo=Histo)
	return(list(dates=dates,datas=as.matrix(Histo),log_datas=log(Histo),returns=returns,spot=res$Spot))
}

######################################### DIV ########################################
BuildYieldFromDiv = function(y){
	return(data.frame(Date=y$Date,Spot=y$Spot,Y1=y$Y1/y$Spot,Y2=y$Y2/y$Spot,Y3=y$Y3/y$Spot,Y4=y$Y4/y$Spot,row.names=NULL))
}
GetYieldForAllDates = function(y,Begin,End){ #
	m = Days(Begin=as.Date(Begin),End=as.Date(End)) #
	Date = as.Date(m$dates)
	idx_dates = match(as.Date(y$Date),Date)
	Spot = rep(NA,m$n)
	Y1 = rep(NA,m$n)
	Y2 = rep(NA,m$n)
	Y3 = rep(NA,m$n)
	Y4 = rep(NA,m$n)
	Spot[idx_dates] = as.double(y$Spot)
	Y1[idx_dates] = as.double(y$Y1)
	Y2[idx_dates] = as.double(y$Y2)
	Y3[idx_dates] = as.double(y$Y3)
	Y4[idx_dates] = as.double(y$Y4)
	return(data.frame(Date=Date,Spot=Spot,Y1=Y1,Y2=Y2,Y3=Y3,Y4=Y4,row.names=NULL))
}
GetOneYield = function(CI,type,stock,export=FALSE){ #
	path = paste(CI$DataPath,type,"/",stock, sep="") 
	y = read.table(paste(path,"_DIV.csv",sep=""),header=TRUE,sep=",") #ouvre chaque fichier "..._DIV.csv" dans ACTIONS
	
	# RESTRICTION AUX DATES DE CALIBRATION
	H = TruncateHistoDatas(dates=as.Date(y$Date,format="%m/%d/%Y"),Histo=as.matrix(y[,-1]),Begin=CI$Begin,End=CI$End) #
	y = data.frame(H$dates,H$Histo,row.names=NULL)
	names(y) = c("Date","Spot","Y1","Y2","Y3","Y4")
	
	# SUPPRESSION DES PLAGES CONSTANTES HISTORIQUE DATES DECROISSANTES
	y = RemoveConstantRange(y=y,nettoyage_mode=CI$nettoyage_mode) #
	
	# MATCHING SUR L'ENSEMBLE DES DATES DISPONIBLES
	y = GetYieldForAllDates(y=y$z,Begin=CI$Begin,End=CI$End) #
	
	# CALCUL DES YIELDS
	y = BuildYieldFromDiv(y) #
	
	# TRONCATURE AUX JOURS OUVRES
	truncate_data = NetWorkingDays(dates=y$Date,h=as.matrix(y[,-1]),Calendar="UnitedKingdom") #
	dates = truncate_data$dates
	spot = as.double(truncate_data$h[,1])
	Y1 = as.double(truncate_data$h[,2])
	Y2 = as.double(truncate_data$h[,3])
	Y3 = as.double(truncate_data$h[,4])
	Y4 = as.double(truncate_data$h[,5])
	result=data.frame(Date=dates,Spot=spot,Y1=Y1,Y2=Y2,Y3=Y3,Y4=Y4,row.names=NULL)		
	if(export){write.table(result,paste(path,"_YIELD.csv",sep=""),sep=",",row.names=FALSE)}
	return(result)
}
ComputeAllYield = function(CI,type,export=FALSE){ #
	path = paste(CI$DataPath,type,"/",sep="")
	liste = list.files(path)
	suppressWarnings(liste <- as.double(unique(unlist(strsplit(liste,"_")))))
	liste = liste[!is.na(liste)]
	n = length(liste)
	for(i in seq(1,n,1)){ stock_yield = GetOneYield(CI=CI,type=type,stock=liste[i],export=export)} #
}
ComputeYield = function(CI,export=FALSE){ #
	ComputeAllYield(CI=CI,type="ACTIONS",export=export)
	ComputeAllYield(CI=CI,type="INDICES",export=export)
}


####################################### EQUITY #######################################
ExtractHistoricalPricesEqtyIndex = function(CI,Begin=NULL,End=NULL,clean_type=FALSE){
	if(is.null(Begin)){Begin = CI$Begin}
	if(is.null(End)){End = CI$End}
	if(clean_type){
		Data = read.table(file=paste(CI$DataPath,"Histo_CLEAN.csv",sep=""),sep=";",header = TRUE)
		dates = as.Date(as.character(Data$Date))
	}else{
		Data = read.table(file=paste(CI$DataPath,"Histo.csv",sep=""),sep=";",header = TRUE)
		dates = as.Date(as.character(Data$Date),format = "%d/%m/%Y")
	}
	I = which(dates >= as.Date(Begin) & dates <= as.Date(End))
	return(list(name=CI$Base,h=as.matrix(Data[I,-1]),dates=dates[I]))
}






