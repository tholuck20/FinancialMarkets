
Export_Base_ASE = function(CI,Base){ # Export.r
	if(CI$Type == "IR"){
		#HISTO = Export_Base_ASE_IR(CI,Base=Base)
	}else if(CI$Type == "VolATM"){
		HISTO = Export_Base_ASE_VolATM(CI=CI,Base=Base)
	}else if(CI$Type == "CRE"){
		HISTO = Export_Base_ASE_CRE(CI=CI,Base=Base)
	}else if(CI$Type == "BaseCorrel"){
		HISTO = Export_Base_ASE_BaseCorrel(CI=CI,Base=Base)
	}else if(CI$Type == "DIV"){
		HISTO = Export_Base_ASE_YIELD(CI=CI,Base=Base)
	}else if(CI$Type == ""){
		HISTO = "..."
	}
}
Export_Base_ASE_IR = function(CI,Base){ # Export.r
}
Export_Base_ASE_VolATM = function(CI,Base){ # Export.r
	coeff_ASE = 165/sqrt(365)
	path_ASE = paste(CI$path_dir,"ASE/",sep="")
	
	Index_ASE=c()
	Index_ASE1=c()
	Index_ASE2=c()
	P_ASE=c()
	
	for(i in 1:length(CI$Base_Name)){
		for(j in 1:length(CI$Tenors)){
			Index_ASE=c(Index_ASE,paste("Vol_Eqty_",CI$Base_Code[i],"_NADM(M1;O",CI$Tenorsj[j],")",sep=""))
		}
	}
	
	for(i in 1:length(Index_ASE)){
		for(j in 1:length(Index_ASE)){
			Index_ASE1=c(Index_ASE1,Index_ASE[i])
			Index_ASE2=c(Index_ASE2,Index_ASE[j])
			P_ASE=c(P_ASE,Base$P[i,j])
		}
	}
	
	a = data.frame(Index_ASE,Base$a)
	b = data.frame(Index_ASE,Base$b)
	sigma = data.frame(Index_ASE,Base$sigma*coeff_ASE)
	P = data.frame(Index_ASE1,Index_ASE2,P_ASE)
	
	write.table(a,paste(path_ASE,"SG.PCANORMAL.VolNADM.MRR.csv",sep=""),sep=",",row.names=FALSE,col.names=FALSE,quote=FALSE)
	write.table(b,paste(path_ASE,"SG.PCANORMAL.VolNADM.MRL.csv",sep=""),sep=",",row.names=FALSE,col.names=FALSE,quote=FALSE)
	write.table(sigma,paste(path_ASE,"SG.PCANORMAL.VolNADM.Volatility.csv",sep=""),sep=",",row.names=FALSE,col.names=FALSE,quote=FALSE)
	write.table(P,paste(path_ASE,"SG.VolNADM.TiltMatrix.csv",sep=""),sep=",",row.names=FALSE,col.names=FALSE,quote=FALSE)
	
	#### Prez
	ParamIndex = data.frame(a=Base$a,b=Base$b,sigma=Base$sigma)
	write.table(ParamIndex,paste(CI$ResPath,"paramsIndex ",CI$Name,".csv",sep=""),sep=",",row.names=FALSE,col.names=FALSE,quote=FALSE)
	write.table(Base$P,paste(CI$ResPath,"Matrice de Passage ",CI$Name,".csv",sep=""),sep=",",row.names=FALSE,col.names=FALSE,quote=FALSE)
	
	VolsIndex = data.frame(Vol_CT=Base$Vol_CT,Vol_LT=Base$Vol_LT)
	write.table(VolsIndex,paste(CI$ResPath,"VolsIndex ",CI$Name,".csv",sep=""),sep=",",row.names=TRUE,col.names=TRUE,quote=FALSE)
}
Export_Base_ASE_CRE = function(CI,Base){ # Export.r
	coeff_ASE = 165/sqrt(365)
	path_ASE = paste(CI$path_dir,"ASE/",sep="")
	
	liste_entete = NULL
	liste_entete2 = NULL
	liste_entete_final = NULL
	vect_matrix = NULL
	vect_matrix2 = NULL
	
	for (i in 1:length(CI$ZoneGeo))
	{
		for (l in 1:length(CI$Sectors))
		{
			for (j in 1:length(CI$Ratings))
			{
				for (k in 1:length(CI$Tenorsj))
				{
					liste_entete = c(liste_entete,paste("GenTreSpd--",CI$ZoneGeo[i],"_",CI$Sectors[l],"(C",CI$Ratings[j],";T",CI$Tenorsj[k],")",sep=""))
					for (m in 1:CI$nbFacteursRef)
					{
						liste_entete2 = c(liste_entete2,paste("GenTreSpd--",CI$ZoneGeo[i],"_",CI$Sectors[l],"(C",CI$Ratings[j],";T",CI$Tenorsj[k],")",sep=""))
					}
				}
			}
		}
	}
	
	a = data.frame(liste_entete, Base$a)
	b = data.frame(liste_entete, Base$b)
	write.table(a,paste(path_ASE,"SG.PCANORMAL.GENSPD.MRR.csv",sep=""),sep=",",row.names=FALSE,col.names=FALSE)
	write.table(b,paste(path_ASE,"SG.PCANORMAL.GENSPD.MRL.csv",sep=""),sep=",",row.names=FALSE,col.names=FALSE)
	
	# Pour la matrice de passage
	for (i in 1:CI$nbFacteursRef)
	{
		for (j in 1:CI$nbFacteursRef)
		{
			vect_matrix = c(vect_matrix, Base$P[i,j])
		}
		liste_entete_final = c(liste_entete_final, liste_entete)
	}
	aa = paste(liste_entete2,liste_entete_final,sep=",")
	res_ASE_MP = data.frame(aa, vect_matrix)
	write.table(res_ASE_MP,paste(path_ASE,"SG.GENSPD.TiltMatrix.csv",sep=""),sep=",",row.names=FALSE,col.names=FALSE)
	
	exist = file.exists(paste(path_ASE,"SG.SG.Volatility.csv",sep=""))
	
	if(exist){
		# Ecriture fichier SG.SG.Volatility.csv : on reprend l'ancien et on écrase avec les nouveaux paramètres
		SG_Vol = read.csv(paste(path_ASE,"SG.SG.Volatility.csv",sep=""),header=FALSE)
		# USD
		pos = which(SG_Vol=="GenTreSpd--ASI_Corporates(CAAA;T365)")
		for (i in 1:(CI$nbFacteursRef))
		{
			SG_Vol[pos+i-1,2] = coeff_ASE * Base$sigma[i]
		}
	}else{
		SG_Vol = coeff_ASE * Base$sigma
	}
	
	res_SG_Vol = data.frame(SG_Vol)
	write.table(res_SG_Vol,paste(path_ASE,"SG.SG.Volatility.csv",sep=""),sep=",",row.names=FALSE,col.names=FALSE)
}
Export_Base_ASE_BaseCorrel = function(CI,Base){ # Export.r
}
Export_Base_ASE_YIELD = function(CI,Base){ # Export.r
	coeff_ASE = 165/sqrt(365)
	path_ASE = paste(CI$path_dir,"ASE/",sep="")
	
	n = length(CI$Base)
	tenors = c("0Y","1Y","2Y","3Y","4Y")
	m = length(tenors)
	row_name = character(m*n)
	for(i in seq(1,n,1)){
		row_name[(m*(i-1)+1):(m*i)] = paste("YieldSpd_",CI$Base[i],"_",tenors,"(T365)",sep="")
	}
	
	a = data.frame(row_name,Base$a,row.names=NULL)
	b = data.frame(row_name,Base$b,row.names=NULL)	
	sigma = data.frame(row_name,Base$sigma*coeff_ASE,row.names=NULL)	
	P = data.frame(cbind(as.character(apply(t(row_name),2,paste,row_name,sep=","))),as.double(t(Base$P)),row.names=NULL)
	Vol_CT = data.frame(row_name,Base$Vol_CT*sqrt(260),row.names=NULL)
	Vol_LT = data.frame(row_name,Base$Vol_LT,row.names=NULL)
	
	write.table(a,paste(path_ASE,"SG.PCANORMAL.DIVYIELD.MRR.csv",sep=""),col.names=FALSE,row.names=FALSE,quote=FALSE,sep=",")
	write.table(b,paste(path_ASE,"SG.PCANORMAL.DIVYIELD.MRL.csv",sep=""),col.names=FALSE,row.names=FALSE,quote=FALSE,sep=",")
	write.table(sigma,paste(path_ASE,"SG.SG.Volatility.csv",sep=""),col.names=FALSE,row.names=FALSE,quote=FALSE,sep=",")
	write.table(P,paste(path_ASE,"SG.DIVYIELD.TiltMatrix.csv",sep=""),col.names=FALSE,row.names=FALSE,quote=FALSE,sep=",")
	write.table(Vol_CT,paste(path_ASE,"SG.PCANORMAL.DIVYIELD.VOL_CT.csv",sep=""),col.names=FALSE,row.names=FALSE,quote=FALSE,sep=",")
	write.table(Vol_LT,paste(path_ASE,"SG.PCANORMAL.DIVYIELD.VOL_LT.csv",sep=""),col.names=FALSE,row.names=FALSE,quote=FALSE,sep=",")
	write.table(Base$VCV_CT,paste(path_ASE,"SG.PCANORMAL.DIVYIELD.VCV_CT.csv",sep=""),col.names=FALSE,row.names=FALSE,quote=FALSE,sep=",")
	write.table(Base$VCV_LT,paste(path_ASE,"SG.PCANORMAL.DIVYIELD.VCV_LT.csv",sep=""),col.names=FALSE,row.names=FALSE,quote=FALSE,sep=",")
	write.table(Base$COR_CT,paste(path_ASE,"SG.PCANORMAL.DIVYIELD.COR_CT.csv",sep=""),col.names=FALSE,row.names=FALSE,quote=FALSE,sep=",")
	write.table(Base$COR_CT,paste(path_ASE,"SG.PCANORMAL.DIVYIELD.COR_LT.csv",sep=""),col.names=FALSE,row.names=FALSE,quote=FALSE,sep=",")
	write.table(Base$COR_CT_adj,paste(path_ASE,"SG.PCANORMAL.DIVYIELD.COR_CT_RED.csv",sep=""),col.names=FALSE,row.names=FALSE,quote=FALSE,sep=",")
	write.table(Base$COR_CT_adj,paste(path_ASE,"SG.PCANORMAL.DIVYIELD.COR_LT_RED.csv",sep=""),col.names=FALSE,row.names=FALSE,quote=FALSE,sep=",")
	write.table(Base$P,paste(path_ASE,"P.csv",sep=""),col.names=FALSE,row.names=FALSE,quote=FALSE,sep=";")	
	
	d_f = data.frame(Vol_LT=Vol_LT,Vol_LT_New = sqrt(diag(Base$VCV_LT_New)), Vol_CT=Vol_CT,Vol_CT_New = sqrt(diag(Base$VCV_CT_New)))
	
	write.table(d_f,paste(path_ASE,"d_f.csv",sep=""),col.names=FALSE,row.names=FALSE,quote=FALSE,sep=";")	
}

