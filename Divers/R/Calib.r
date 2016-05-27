
rm(list=ls())
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
export = FALSE #TRUE

nettoyage_mode = 7

Base_IR = c("HKD","USD","DKK","NZD","EUR","SGD","GBP","CHF","CZK","SEK","NOK","JPY","CAD","AUD","KRW","PLN","HUF",  #MULTI_COURBES
			"XAU","XAG","SAR","ZAR","THB","IDR","MYR","PHP","EGP",													#MONO_COURBES
			"TWD","TRY","ILS","ISK","RUB","RON","ARS","CNY","MXN",													#MONO_COURBES
			"BGN","AED","COP","KWD","TND","MAD","CLP","BRL","INR","PEN")											#MONO_COURBES

Base_Name_VolATM = c("NIKKEI","CAC40_X","FTSE","SP500","HANGSENG","OMX","SMI","BEL20_X","DAX_X","IBEX_X","EOE_X","STOX5E_X","DJGT")
Base_Code_VolATM = c(24062,220056,24056,24066,24057,25905,24065,220054,220057,220075,220058,220102,1450985)

Base_Name_CRE = ""

Base_Name_BaseCorrel=c("CDX_5Y_3","CDX_5Y_7","CDX_5Y_10","CDX_5Y_15","CDX_5Y_30",
					   "CDX_7Y_3","CDX_7Y_7","CDX_7Y_10","CDX_7Y_15","CDX_7Y_30",
					   "CDX_10Y_3","CDX_10Y_7","CDX_10Y_10","CDX_10Y_15","CDX_10Y_30",
					   "Itraxx_5Y_3","Itraxx_5Y_6","Itraxx_5Y_9","Itraxx_5Y_12","Itraxx_5Y_22",
					   "Itraxx_7Y_3","Itraxx_7Y_6","Itraxx_7Y_9","Itraxx_7Y_12","Itraxx_7Y_22",
					   "Itraxx_10Y_3","Itraxx_10Y_6","Itraxx_10Y_9","Itraxx_10Y_12","Itraxx_10Y_22")

Base_Name_Div = c("SP500","STOX5E_X","NIKKEI","FTSE","CAC40_X","DJGT")
Base_Code_Div = c(24066,220102,24062,24056,220056,1450985)


IR_STD 			= StructureRF(Begin=Begin_STD,End=End_STD,DateRef=DateRef,Name=Name,Regime="STD",Base_Name=Base_IR,Base_Code="",Type="IR",nettoyage_mode=nettoyage_mode)
VolATM_STD 		= StructureRF(Begin=Begin_STD,End=End_STD,DateRef=DateRef,Name=Name,Regime="STD",Base_Name=Base_Name_VolATM,Base_Code=Base_Code_VolATM,Type="VolATM",nettoyage_mode=nettoyage_mode)
CRE_STD 		= StructureRF(Begin=Begin_STD,End=End_STD,DateRef=DateRef,Name=Name,Regime="STD",Base_Name=Base_Name_CRE,Base_Code="",Type="CRE",nettoyage_mode=nettoyage_mode)
BaseCorrel_STD 	= StructureRF(Begin=Begin_STD,End=End_STD,DateRef=DateRef,Name=Name,Regime="STD",Base_Name=Base_Name_BaseCorrel,Base_Code="",Type="BaseCorrel",nettoyage_mode=nettoyage_mode)
YIELD_STD 		= StructureRF(Begin=Begin_STD,End=End_STD,DateRef=DateRef,Name=Name,Regime="STD",Base_Name=Base_Name_Div,Base_Code=Base_Code_Div,Type="DIV",nettoyage_mode=nettoyage_mode)

####HISTO_IR = GetHisto_Ref(CI=IR_STD) #### not used anymore ####
HISTO_VolATM 		= GetHisto_Ref(CI=VolATM_STD)
HISTO_CRE 			= GetHisto_Ref(CI=CRE_STD)
HISTO_BaseCorrel 	= GetHisto_Ref(CI=BaseCorrel_STD)
HISTO_YIELD 		= GetHisto_Ref(CI=YIELD_STD)

ComputeYield(CI=YIELD_STD,export=TRUE)
report_cleaning = CleanData(CI=list(YIELD_STD),export=export)

T2 <- Sys.time(); cat(paste("Durée récupération HISTO : ",round(T2-T1,digits=4)," secondes \n",sep=""))

#Ref_Params_IR 			= Calibration_Ref(CI=IR_STD,			HISTO="",				export=export) #valeurs propres et vecteurs propres complexes
Ref_Params_VolATM 		= Calibration_Ref(CI=VolATM_STD,		HISTO=HISTO_VolATM,		export=export)
Ref_Params_CRE 			= Calibration_Ref(CI=CRE_STD,			HISTO=HISTO_CRE,		export=export)
Ref_Params_BaseCorrel 	= Calibration_Ref(CI=BaseCorrel_STD,	HISTO=HISTO_BaseCorrel,	export=export)
Ref_Params_YIELD 		= Calibration_Ref(CI=YIELD_STD,			HISTO=HISTO_YIELD,		export=export)

T3 <- Sys.time(); cat(paste("Durée Calib Ref : ",round(T3-T2,digits=4)," secondes \n",sep=""))

Proj_Params_IR 			= Calibration_Proj(CI=IR_STD,			Ref_Params=Ref_Params_IR,			export=export)
Proj_Params_VolATM 		= Calibration_Proj(CI=VolATM_STD,		Ref_Params=Ref_Params_VolATM,		export=export)
Proj_Params_CRE 		= Calibration_Proj(CI=CRE_STD,			Ref_Params=Ref_Params_CRE,			export=export)
####Proj_Params_BaseCorrel 	= Calibration_Proj(CI=BaseCorrel_STD,	Ref_Params=Ref_Params_BaseCorrel,	export=export)
Proj_Params_YIELD 		= Calibration_Proj(CI=YIELD_STD,		Ref_Params=Ref_Params_YIELD,		export=export)

T4 <- Sys.time(); cat(paste("Durée Calib Proj : ",round(T4-T3,digits=4)," secondes \n",sep=""))

cat(paste("Durée totale : ",round(Sys.time()-T1,digits=4)," secondes \n",sep=""))




