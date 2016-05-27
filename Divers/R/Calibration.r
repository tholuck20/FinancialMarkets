

######################################### REF ########################################
Calibration_Ref = function(CI,HISTO="",export=FALSE){
	if(CI$Type!="IR"){
		res = VasicekMultiFactorFractileModel(CI=CI,HISTO=HISTO)
		if(export){ Export_Base_ASE(CI=CI,Base=res) }
	}else{
		Vol = RefVolatility_IR(CI)
		HISTO = Vol$MULTI$HISTO
		res = VarianceCov_IR(CI,HISTO,Vol)
	}
	return(res)
}

RefVolatility_IR = function(CI){
	
	MULTI = RefVol_IR_MULTI(CI)
	MONO = RefVol_IR_MONO(CI)
	
	return(list(MULTI=MULTI,MONO=MONO))
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
	#???? Erreur pour MULTI et pour MONO (!correl_histo, ie. COR_USD) ===>>> valeurs propres et vecteurs propres complexes)
	#MULTI = VarianceCov_IR_MULTI(CI,HISTO,Vol)
	MONO = VarianceCov_IR_MONO(CI,HISTO,Vol)
	
	#return(list(MULTI=MULTI,MONO=MONO))
	return(MONO=MONO)
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
	dd = DoubleDiagonalisation(CI,VCV_CT,VCV_LT,m,lambda,SVD=TRUE)
	
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
			res = DoubleDiagonalisation(CI,VCV_CT,VCV_LT,m,lambda=CI$lambdaMonoCorrelHisto,SVD=TRUE)
		}else{
			res = DoubleDiagonalisation(CI,VCV_CT,VCV_LT,m,lambda=CI$lambdaMonoOther,SVD=TRUE)
		}
		
		resParam = data.frame(a=res$a,b=res$b,sigma=res$sigma)
		
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

DoubleDiagonalisation = function(CI,VCV_CT,VCV_LT,m,lambda,SVD=FALSE){
	if(CI$dd == "rot"){
		dd = DoubleDiagonalisation_Rotation(CI=CI,VCV_CT=VCV_CT,VCV_LT=VCV_LT,m=m,lambda=lambda,SVD=SVD)
	}else{
		dd = DoubleDiagonalisation_SQRTM(CI=CI,VCV_CT=VCV_CT,VCV_LT=VCV_LT,m=m,lambda=lambda,SVD=SVD)
	}
}
DoubleDiagonalisation_Rotation = function(CI,VCV_CT,VCV_LT,m,lambda,SVD=FALSE){ ############## ????
	n = ncol(VCV_CT)
	eigen_VCV_CT = eigen(VCV_CT) ############### Pour IR MUTLI => valeurs propres et vecteurs propres complexes
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
	PP = P^2
	
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
		a[i] = -260 * 0.5 * log(1 - 1 / (260 * d))
		if(is.numeric(CI$aMin)){ a = pmax(a,CI$aMin) }
		Dhat[i,i] = d
		sigma[i] = sqrt(2 * a[i] * d)
	}
	for(i in seq(1,n,1)){
		b[i] = log(m[i]) - 0.25 * PP[i,] %*% ((sigma^2)/a)
	}
	b = invP%*%b
	return(list(P=P,invP=invP,Dhat=Dhat,a=a,b=as.double(b),sigma=sigma,B=B))
}
DoubleDiagonalisation_SQRTM = function(CI,VCV_CT,VCV_LT,m,lambda,SVD=FALSE){
	n = ncol(VCV_CT)
	H = sqrtm(VCV_CT)
	invH = solve(H)
	B = invH%*%VCV_LT%*%t(invH)
	BB = eigen(B,symmetric=TRUE)
	Phat = BB$vectors
	Dhat = diag(BB$values)
	P = H%*%Phat
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
		a[i] = -260 * 0.5 * log(1 - 1 / (260 * d))
		if(is.numeric(CI$aMin)){ a = pmax(a,CI$aMin) }
		Dhat[i,i] = d
		sigma[i] = sqrt(2 * a[i] * d)
	}
	for(i in seq(1,n,1)){
		b[i] = log(m[i]) - 0.25 * PP[i,] %*% ((sigma^2)/a)
	}
	b = invP%*%b
	return(list(P=P,invP=invP,Dhat=Dhat,a=a,b=as.double(b),sigma=sigma,B=B))
}

VasicekMultiFactorFractileModel = function(CI,HISTO){

	type = CI$QuantType
	minus_moy = CI$VolMinusMoy
	
	datas 		= as.matrix(HISTO$datas)
	log_datas 	= as.matrix(HISTO$log_datas)
	returns 	= as.matrix(HISTO$d_log_datas)
	
	m = as.double(apply(datas,2,mean,na.rm=TRUE))
	
	if(CI$typeVol == 1){
		#Vol_LT = vol_fractile(log_datas,minus_moy,type)$vol_f
		Vol_LT = apply(log_datas,2,vol_fractile3) # OK pour CRE
	}else{
		#d = vol_fractile(datas,minus_moy,type)
		q1 	= as.double(apply(datas,2,quantile,na.rm=TRUE,type=type,probs=0.01))
		q99 = as.double(apply(datas,2,quantile,na.rm=TRUE,type=type,probs=0.99))
		vol_log_x = as.double(apply(log_datas,2,sd,na.rm=TRUE))
		vol_sys_x = pmax( pmax(qnorm(0.01) - sqrt(qnorm(0.01)^2-2*log(q1 /m)),qnorm(0.01) + sqrt(qnorm(0.01)^2-2*log(q1/m))),
						  pmin(qnorm(0.99) - sqrt(qnorm(0.99)^2-2*log(q99/m)),qnorm(0.99) + sqrt(qnorm(0.99)^2-2*log(q99/m))) )
		
		Vol_LT  = pmax(vol_log_x,vol_sys_x)
	}
	
	#Vol_CT = vol_fractile(returns,minus_moy,type)$vol_f
	Vol_CT = apply(returns,2,vol_fractile_ct_CRE)
	
	#CORREL
	COR_CT = cor(returns,use ="pairwise.complete.obs")
	COR_LT = cor(log_datas,use ="pairwise.complete.obs")
	
	#REDRESSEMENT
	COR_CT_adj = nearcor(COR_CT,maxit=CI$maxit,posd.tol=CI$p_ct_opt)$cor
	COR_LT_adj = nearcor(COR_LT,maxit=CI$maxit,posd.tol=CI$p_lt_opt)$cor
	
	VCV_CT = 260*diag(Vol_CT) %*% COR_CT_adj %*% diag(Vol_CT)
	VCV_LT = diag(Vol_LT) %*% COR_LT_adj %*% diag(Vol_LT)
	
	dd = DoubleDiagonalisation(CI,VCV_CT,VCV_LT,m,lambda=CI$lambda)
	
	VCV_CT_New = dd$P %*% t(dd$P)
	VCV_LT_New = dd$P %*% dd$Dhat %*% t(dd$P)
	
	return(list(a=dd$a,b=dd$b,sigma=dd$sigma,P=dd$P,invP=dd$invP,COR_LT=COR_LT,COR_LT_adj=COR_LT_adj,COR_CT=COR_CT,COR_CT_adj=COR_CT_adj,
				  VCV_LT=VCV_LT,VCV_CT=VCV_CT,Vol_LT=Vol_LT,Vol_CT=Vol_CT,
				  VCV_LT_New = VCV_LT_New,VCV_CT_New = VCV_CT_New))
}

######################################## PROJ ########################################
Calibration_Proj = function(CI,params_base,export=FALSE){
	if(CI$Type == "DIV"){
		res = list()
		res$projetes = CalibrationAll_Proj_Yield(CI=CI,params_base=params_base,export=export)
		res$default = CalibrationAll_Proj_Yield_Default(CI=CI,projection=res$projetes,export=export)
	}else{
		res = ""
	}
	return(res)
}
CalibrationAll_Proj_Yield = function(CI,params_base,export=FALSE){
	liste = list.files(CI$ProjDataPath)
	suppressWarnings(liste <- as.double(unique(unlist(strsplit(liste,"_")))))
	liste = liste[!is.na(liste)]
	n = length(liste)
	z = data.frame()
	for(i in seq(1,n,1)){ #Pour chaque action
		
		h = GetHisto_Proj_YIELD(CI=CI,stock=liste[i]) # On récupère l'histo de l'action testée
		projection = GetProjectionIndex(CI=CI,returns_stock=h$returns) # On choisi l'indice de projection adapté
		res = VasicekMultiFactorFractileProjModel_Yield(datas_stock=h$datas,log_datas_stock=h$log_datas,returns_stock=h$returns,projection,params_base=params_base) # On projette
		
		z = rbind(z,data.frame(Stock=liste[i],ProjectionIndex=projection$ProjOnIndex,Correlation=projection$correlation,
								a_0Y=res$a_offset[1],b_0Y=res$b_offset[1],sigma_0Y=res$sigma_offset[1],
								a_1Y=res$a_offset[2],b_1Y=res$b_offset[2],sigma_1Y=res$sigma_offset[2],
								a_2Y=res$a_offset[3],b_2Y=res$b_offset[3],sigma_2Y=res$sigma_offset[3],
								a_3Y=res$a_offset[4],b_3Y=res$b_offset[4],sigma_3Y=res$sigma_offset[4],
								a_4Y=res$a_offset[5],b_4Y=res$b_offset[5],sigma_4Y=res$sigma_offset[5],row.names=NULL))
	}
	if(export){write.table(z,paste(CI$PathOutput,"parametres_projetes.csv",sep=""),sep=";",row.names=FALSE,quote=FALSE)}
	return(z)
}
GetProjectionIndex = function(CI,returns_stock){ # On garde l'indice qui a la plus forte corrélation avec l'action testée (à 1Y)
	HISTO_Ref = GetHisto_Ref(CI=CI)
	returns_base = HISTO_Ref$returns
	
	n_tenors = length(CI$Tenors)
	correlation_CT = rep(NA,n_tenors)
	
	# CALCUL DES CORRELATIONS DES RENDEMENTS DE L'ACTION ET DES INDICES
	cpt=1
	for(i in seq(2,ncol(returns_base),n_tenors)){
		correlation_CT[cpt] = as.double(cor(returns_stock[,2],returns_base[,i],use = "pairwise.complete.obs"))
		cpt = cpt+1
	}
	max_correl = max(correlation_CT)
	id = match(max_correl,correlation_CT)
	ProjOnIndex = CI$Base[id]
	ProjOnIndexRange = seq((n_tenors*(id-1)+1),(n_tenors*id),1)
	return(list(ProjOnIndex=ProjOnIndex,ProjOnIndexRange=ProjOnIndexRange,CorStockIndex=max_correl))	
}
VasicekMultiFactorFractileProjModel_Yield = function(datas,log_datas,returns,projection,params_base,eps=1e-5){
	n = ncol(returns)
	dln = vol_fractile(returns)
	Vol_CT = sqrt(260) * pmax(dln$vol_q1,dln$vol_q99)
	
	a_offset = rep(NA,n)
	b_offset = rep(NA,n)
	sigma_offset = rep(NA,n)
	
	if(!(TRUE %in% is.na(Vol_CT)) & !is.na(projection$CorStockIndex)){ #????
		d = vol_fractile(datas,type=CI$QuantType) # ???? pourquoi pas log_datas ?
		m_lt = params_base$P[projection$ProjOnIndexRange,] %*% params_base$b
		sig_lt = sqrt( params_base$P[projection$ProjOnIndexRange,]^2  %*% (0.5 * params_base$sigma^2 / params_base$a) )
		sig_ct = sqrt(260) * sqrt( params_base$P[projection$ProjOnIndexRange,]^2 %*% (params_base$sigma^2 * (1-exp(-2*params_base$a /260)) / (2*params_base$a)) )
		
		Y1 = pmax(qnorm(0.01) - sqrt(qnorm(0.01)^2-2*log(d$q1 /d$m)),qnorm(0.01) + sqrt(qnorm(0.01)^2-2*log(d$q1/d$m)))
		Y2 = pmin(qnorm(0.99) - sqrt(qnorm(0.99)^2-2*log(d$q99/d$m)),qnorm(0.99) + sqrt(qnorm(0.99)^2-2*log(d$q99/d$m)))
		X1 = pmax(Y1^2 - sig_lt^2,0.005)
		X2 = pmax(Y2^2 - sig_lt^2,0.005)
		
		b1_off_af = log(d$m) - m_lt - 0.5*sig_lt^2 - 0.5*X1
		b2_off_af = log(d$m) - m_lt - 0.5*sig_lt^2 - 0.5*X2
		
		suppressWarnings(abb <- log(1- pmax(Vol_CT^2 - sig_ct^2 ,0.005)/(X1*260)))
		if(!(TRUE %in% is.na(abb))){
			a1_off_af = -0.5*log(1- pmax(Vol_CT^2 - sig_ct^2 ,0.005) / (X1*260) )*260
			a2_off_af = -0.5*log(1- pmax(Vol_CT^2 - sig_ct^2 ,0.005) / (X2*260) )*260
			sigma1_off_af = sqrt(X1 * 2 * a1_off_af)
			sigma2_off_af = sqrt(X2 * 2 * a2_off_af)
			
			test2 = m_lt + b2_off_af + sqrt(X2+ sig_lt^2) * qnorm(0.01,0,1) - log(d$q1)
			test3 = m_lt + b1_off_af + sqrt(X1+ sig_lt^2) * qnorm(0.99,0,1) - log(d$q99)
			for(l in seq(1,n,1)){
				if(test2[l] <= eps & test3[l] >= -eps ){
					if(max(abs(test2[l]),abs(test3[l])) == abs(test2[l])){
						a_offset[l] = a1_off_af[l]
						b_offset[l] = b1_off_af[l]
						sigma_offset[l] = sigma1_off_af[l]
					}else{
						a_offset[l] = a2_off_af[l]
						b_offset[l] = b2_off_af[l]
						sigma_offset[l] = sigma2_off_af[l]
					}
				}else if(test2[l] <= eps & test3[l] < -eps ){
					a_offset[l] = a2_off_af[l]
					b_offset[l] = b2_off_af[l]
					sigma_offset[l] = sigma2_off_af[l]
				}else if(test2[l] > eps & test3[l] >= -eps){
					a_offset[l] = a1_off_af[l]
					b_offset[l] = b1_off_af[l]
					sigma_offset[l] = sigma1_off_af[l]
				}else{
					if(max(abs(test2[l]),abs(test3[l])) == abs(test2[l])){
						a_offset[l] = a1_off_af[l]
						b_offset[l] = b1_off_af[l]
						sigma_offset[l] = sigma1_off_af[l]
					}else{
						a_offset[l] = a2_off_af[l]
						b_offset[l] = b2_off_af[l]
						sigma_offset[l] = sigma2_off_af[l]
					}
				}
			}
		}
	}
	return(list(a_offset=a_offset,b_offset=b_offset,sigma_offset=sigma_offset))
}
VasicekMultiFactorFractileProjModel = function(CI,datas,log_datas,returns,projection,params_base,eps=1e-5){
	n = ncol(returns)
	dln = vol_fractile(returns)
	Vol_CT = sqrt(260) * pmax(dln$vol_sd,dln$vol_q1,dln$vol_q99)
	
	a_offset = rep(NA,n)
	b_offset = rep(NA,n)
	sigma_offset = rep(NA,n)
	
	#if(!(TRUE %in% is.na(Vol_CT)) & !is.na(projection$CorStockIndex)){ #????
	
	d = vol_fractile(log_datas)
	b = (d$q1 + d$q99)/2
	Q1_LT = quantile(d$q1,probs=1-CI$Qt,type=1)
	Q99_LT = quantile(d$q99,probs=CI$Qt,type=1)
	b = (Q1_LT + Q99_LT)/2
	
	
	Vol_CT = max( (quantile(dln$q1,probs=1-CI$Qt,type=1))/qnorm(0.01) , (quantile(dln$q99,probs=CI$Qt,type=1))/qnorm(0.99) )
	Vol_LT = max( (Q1_LT - b) / qnorm(0.01) , (Q99_LT - b) / qnorm(0.99) )
	
	
	
	
	
	m_lt = params_base$P %*% params_base$b
	#m_Index = CalibIndex$P %*% CalibIndex$b
	Vol_CT_Index = sqrt(params_base$P^2 %*% params_base$sigma^2)	
	Vol_LT_Index = sqrt(params_base$P^2 %*% params_base$VolLT^2)
	
	b_LogOffset = m_Equity - m_lt
	sigma_LogOffset = sqrt( pmax(0,Vol_CT_Equity^2 - Vol_CT_Index^2 - 0.1^2) + rep(0.1^2,n) )
	Vol_LT_LogOffset = sqrt( pmax(0,Vol_LT_Equity^2 - Vol_LT_Index^2 - 0.1^2) + rep(0.1^2,n) )
	
	a_LogOffset = 0.5 * (sigma_LogOffset / Vol_LT_LogOffset)^2
	
	
	
}
CalibrationAll_Proj_Yield_Default = function(CI,projection=NULL,export=FALSE){ # ???? à tester
	if(is.null(projection)){projection = read.table(paste(CI$PathOutput,"parametres_projetes.csv",sep=""),sep=";",header=TRUE)}
	
	for(i in (1:length(CI$Base))){ #CI$Base = c(24066,220102,24062,24056,220056,1450985)
		
		ind = which(projection$ProjectionIndex == CI$Base[i])
		if(CI$Base[i] == 1450985) { ind = which(projection$ProjectionIndex == 220102) } # On prend le DJGT egal au SX5E:220102 car peu de projetes
		
		a = cbind(projection$a_0Y[ind],projection$a_1Y[ind],projection$a_2Y[ind],projection$a_3Y[ind],projection$a_4Y[ind])
		b = cbind(projection$b_0Y[ind],projection$b_1Y[ind],projection$b_2Y[ind],projection$b_3Y[ind],projection$b_4Y[ind])
		sigma = cbind(projection$sigma_0Y[ind],projection$sigma_1Y[ind],projection$sigma_2Y[ind],projection$sigma_3Y[ind],projection$sigma_4Y[ind])
		sigma_66 = as.double(apply(sigma,2,quantile,na.rm=TRUE,type=1,probs=CI$Qt))
		
		idx = numeric(5)
		a_66 = rep(NA,5)
		b_66 = rep(NA,5)
		
		for(j in (1:5)){
			idx[j] = match(sigma_66[j],sigma[,j])
			a_66 = a[idx,j]
			b_66 = b[idx,j]
		}
		
		a_0Y = c(a_0Y,a_66[1])				;		a_1Y = c(a_1Y,a_66[2])
		b_0Y = c(b_0Y,b_66[1])				;		b_1Y = c(b_1Y,b_66[2])
		sigma_0Y = c(sigma_0Y,sigma_66[1]) 	; 		sigma_1Y = c(sigma_1Y,sigma_66[2])	
		
		a_2Y = c(a_2Y,a_66[3])				;		a_3Y = c(a_3Y,a_66[4])
		b_2Y = c(b_2Y,b_66[3])				;		b_3Y = c(b_3Y,b_66[4])
		sigma_2Y = c(sigma_2Y,sigma_66[3]) 	; 		sigma_3Y = c(sigma_3Y,sigma_66[4])		
		
		a_4Y = c(a_4Y,a_66[5])
		b_4Y = c(b_4Y,b_66[5])
		sigma_4Y = c(sigma_4Y,sigma_66[5])
	}
	
	res = data.frame(CI$Base_Name,a_0Y,b_0Y,sigma_0Y,
								  a_1Y,b_1Y,sigma_1Y,
								  a_2Y,b_2Y,sigma_2Y,
								  a_3Y,b_3Y,sigma_3Y,
								  a_4Y,b_4Y,sigma_4Y)	
	
	if(export){write.table(res,paste(CI$PathOutput,"parametres_projetes_defauts.csv",sep=""),sep=";",row.names=FALSE,quote=FALSE)}
	return(res)
}






