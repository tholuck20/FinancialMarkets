
StructureRF = function(Begin,End,DateRef,Name,Regime,Base_Name,Base_Code,Type,nettoyage_mode=7){
	if(Type=="IR"){
		return(Structure_IR(Begin=Begin,End=End,DateRef=DateRef,Name=Name,Regime=Regime,Base_Name=Base_Name,Type=Type,nettoyage_mode=nettoyage_mode))
	}else if(Type=="VolATM"){
		return(Structure_VolATM(Begin=Begin,End=End,DateRef=DateRef,Name=Name,Regime=Regime,Base_Name=Base_Name,Base_Code=Base_Code,Type=Type,nettoyage_mode=nettoyage_mode))
	}else if(Type=="CRE"){
		return(Structure_CRE(Begin=Begin,End=End,DateRef=DateRef,Name=Name,Regime=Regime,Base_Name=Base_Name,Type=Type,nettoyage_mode=nettoyage_mode))
	}else if(Type=="BaseCorrel"){
		return(Structure_BaseCorrel(Begin=Begin,End=End,DateRef=DateRef,Name=Name,Regime="STD",Base_Name=Base_Name,Type=Type,nettoyage_mode=nettoyage_mode))
	}else if(Type=="DIV"){
		return(Structure_Yield(Begin=Begin,End=End,DateRef=DateRef,Name=Name,Regime="STD",Base_Name=Base_Name,Base_Code=Base_Code,Type=Type,nettoyage_mode=nettoyage_mode))
	}
}
Structure_IR = function(Begin,End,DateRef,Name,Regime,Base_Name,Type="IR",nettoyage_mode=7){
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
	CI$Base = Base_Name
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
	CI$maxit = 100000 #nearcor
	CI$dd = "rot"
	CI$lambdaMulti = 90
	CI$lambdaMonoCorrelHisto = 15
	CI$lambdaMonoOther = 10
	
	CI$QuantType = 7 ############### ????
	CI$VolMinusMoy = TRUE
	
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
Structure_VolATM = function(Begin,End,DateRef,Name,Regime,Base_Name,Base_Code,Type="VolATM",nettoyage_mode=7){
	CI = list()
	CI$Type = Type #"VolATM"
	CI$Name = Name
	CI$Begin = Begin
	CI$End = End
	CI$DateRef = DateRef
	CI$Regime = Regime
	CI$Mode = 10
	if(Regime=="STR"){CI$Mode=3}
	
	CI$DataPath			= paste("D:/mneri092115/Modelisation/calibration/",Type,"/Data/",sep="")
	CI$ParamPath		= paste("D:/mneri092115/Modelisation/calibration/",Type,"/Parametres/",sep="")
	
	CI$ResPath			= paste("D:/mneri092115/Modelisation/calibration/",Type,"/",Name,"/RESULTS/",sep="")
	CI$RefResPath		= paste(CI$ResPath,"REF/",sep="")
	CI$ProjResPath		= paste(CI$ResPath,"PROJ/",sep="")
	
	CI$Tenors = c("1M","1Y","2Y","3Y")
	CI$Tenorsj = c(30,365,730,1096)
	CI$Base_Name = Base_Name
	CI$Base_Code = Base_Code
	
	CI$nbIndex = length(Base_Name)
	#CI$nbEquity = 
	
	CI$nbFacteursRef = 52 #13 Indexes * 4 Tenors
	
	CI$typeVol = 2
	CI$p_ct_opt = 1e-08 #nearcor (default value)
	CI$p_lt_opt = 1e-08 #nearcor (default value)
	CI$maxit = 100 #nearcor (default value)
	CI$dd = "sqrtm"
	CI$lambda = 2
	
	CI$QuantType = 7
	CI$VolMinusMoy = FALSE
	
	CI$clean_data = 1
	CI$nettoyage_mode = nettoyage_mode
	
	CI$path_dir = paste("D:/mneri092115/Modelisation/calibration/",Type,"/",Name,"/",sep="") #calibration/IR/Q1-16/
	dirCreate(CI=CI,path_dir=path_dir)
	
	return(CI)
}
Structure_CRE = function(Begin,End,DateRef,Name,Regime,Base_Name,Type="CRE",nettoyage_mode=7){
	CI = list()
	CI$Type = Type #"CRE"
	CI$Name = Name
	CI$Begin = Begin
	CI$End = End
	CI$DateRef = DateRef
	CI$Regime = Regime
	CI$Mode = 10
	if(Regime=="STR"){CI$Mode=3}
	
	CI$DataPath			= paste("D:/mneri092115/Modelisation/calibration/",Type,"/Data/",sep="")
	CI$ParamPath		= paste("D:/mneri092115/Modelisation/calibration/",Type,"/Parametres/",sep="")
	CI$RefDataPath 		= paste(CI$DataPath,"Historiques_Ref/",sep="")
	CI$ProjDataPath 	= paste(CI$DataPath,"Historiques_Proj/",sep="")
	
	CI$ResPath			= paste("D:/mneri092115/Modelisation/calibration/",Type,"/",Name,"/RESULTS/",sep="")
	CI$RefResPath		= paste(CI$ResPath,"REF/",sep="")
	CI$ProjResPath		= paste(CI$ResPath,"PROJ/",sep="")
	
	CI$Tenors = c("1Y","3Y","5Y","10Y")
	CI$Tenorsj = c(365,1096,1826,3653)
	CI$Ratings = c("AAA","AA","A","BBB","BB","B","CCC")
	CI$Sectors = c("Corporates","Financials","Government")
	CI$ZoneGeo = c("ASI","EUR","AME","OTH")
	CI$Base = Base_Name ################### ????
	
	CI$nbFacteursRef = 336 # 4*7*3*4
	
	CI$typeVol = 1
	CI$p_ct_opt = 1e-6 #nearcor
	CI$p_lt_opt = 1e-6 #nearcor
	CI$maxit = 100000 #nearcor
	CI$dd = "rot"
	CI$aMin = 0.138
	CI$lambda = 57
	
	CI$QuantType = 7 #default
	CI$Qt = 0.80
	CI$VolMinusMoy = FALSE
	
	CI$clean_data = 1
	CI$nettoyage_mode = nettoyage_mode
	
	CI$path_dir = paste("D:/mneri092115/Modelisation/calibration/",Type,"/",Name,"/",sep="") #calibration/IR/Q1-16/
	dirCreate(CI=CI,path_dir=path_dir)
	
	return(CI)
}
Structure_BaseCorrel = function(Begin,End,DateRef,Name,Regime,Base_Name,Type="BaseCorrel",nettoyage_mode=7){
	CI = list()
	CI$Type = Type #"BaseCorrel"
	CI$Name = Name
	CI$Begin = Begin
	CI$End = End
	CI$DateRef = DateRef
	CI$Regime = Regime
	CI$Mode = 10
	if(Regime=="STR"){CI$Mode=3}
	
	CI$DataPath			= paste("D:/mneri092115/Modelisation/calibration/",Type,"/Data/",sep="")
	CI$ParamPath		= paste("D:/mneri092115/Modelisation/calibration/",Type,"/Parametres/",sep="")
	CI$RefDataPath 		= paste(CI$DataPath,"Historiques_Ref/",sep="")
	CI$ProjDataPath 	= paste(CI$DataPath,"Historiques_Proj/",sep="")
	
	CI$ResPath			= paste("D:/mneri092115/Modelisation/calibration/",Type,"/",Name,"/RESULTS/",sep="")
	CI$RefResPath		= paste(CI$ResPath,"REF/",sep="")
	CI$ProjResPath		= paste(CI$ResPath,"PROJ/",sep="")
	
	CI$Tenors = c("1Y","3Y","5Y","10Y")
	CI$Ratings = c("AAA","AA","A","BBB","BB","B","CCC")
	#CI$Sectors = c("Corporates","Financials","Government")
	#CI$ZoneGeo = c("ASI","EUR","AME","OTH")
	CI$Base = Base_Name
	
	CI$nbFacteursRef = 30 # 
	
	CI$shiftCDX = 0.045
	CI$shiftItraxx = 0.02
	
	CI$typeVol = 1
	CI$p_ct_opt = 1e-6 #nearcor
	CI$p_lt_opt = 1e-6 #nearcor
	CI$maxit = 100000 #nearcor
	CI$dd = "rot"
	#CI$aMin = 0.138
	CI$lambda = 90
	
	CI$QuantType = 7 #default
	CI$VolMinusMoy = FALSE
	
	CI$clean_data = 1
	CI$nettoyage_mode = nettoyage_mode
	
	CI$path_dir = paste("D:/mneri092115/Modelisation/calibration/",Type,"/",Name,"/",sep="") #calibration/IR/Q1-16/
	dirCreate(CI=CI,path_dir=path_dir)
	
	return(CI)
}
Structure_Yield = function(Begin,End,DateRef,Name,Regime,Base_Name="",Base_Code,Type="DIV",nettoyage_mode=7){
	CI = list()
	CI$Type = Type #"DIV"
	CI$Name = Name
	CI$Begin = Begin
	CI$End = End
	CI$DateRef = DateRef
	CI$Regime = Regime
	CI$Mode = 10
	if(Regime=="STR"){CI$Mode=3}
	
	CI$DataPath			= paste("D:/mneri092115/Modelisation/calibration/",Type,"/Data/",sep="")
	CI$ParamPath		= paste("D:/mneri092115/Modelisation/calibration/",Type,"/Parametres/",sep="")
	CI$RefDataPath 		= paste(CI$DataPath,"Indices/",sep="")
	CI$ProjDataPath 	= paste(CI$DataPath,"Actions/",sep="")
	
	CI$ResPath			= paste("D:/mneri092115/Modelisation/calibration/",Type,"/",Name,"/RESULTS/",sep="")
	CI$RefResPath		= paste(CI$ResPath,"REF/",sep="")
	CI$ProjResPath		= paste(CI$ResPath,"PROJ/",sep="")
	
	CI$Tenors = c("Y0","Y1","Y2","Y3","Y4")
	CI$Base = Base_Code
	CI$Base_Name = Base_Name
	
	CI$nbFacteursRef = 30 # 
	CI$Bandwidth = 260
	CI$ShiftIndex = 0.001
	CI$ShiftStock = 0.005
	
	CI$typeVol = 2
	CI$p_ct_opt = 1e-08 #nearcor (default value)
	CI$p_lt_opt = 1e-08 #nearcor (default value)
	CI$maxit = 100 #nearcor (default value)
	CI$dd = "sqrtm"
	#CI$aMin = 0.138
	CI$lambda = 10
	
	CI$QuantType = 1
	CI$Qt = 0.66
	CI$VolMinusMoy = FALSE
	
	CI$clean_data = 1
	CI$nettoyage_mode = nettoyage_mode
	
	CI$path_dir = paste("D:/mneri092115/Modelisation/calibration/",Type,"/",Name,"/",sep="") #calibration/IR/Q1-16/
	dirCreate(CI=CI,path_dir=path_dir)
	
	return(CI)
}

