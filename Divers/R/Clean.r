

######################################### IR #########################################
nb_numeric_data = function(histo_data){

	n=length(histo_data)
	index = 0;
	for(i in 1:n)
	{
		if( !is.na(histo_data[i]) ){
			index = index +1
		}
	}
	return(index)
}
CleanConstData = function(histo_data,seuil_cleaning_data){

	data_sub = histo_data
	
	for(i in 1:length(histo_data[1,])) # On traite colonne par colonne (9 = dates + 8 tenors)
	{									
		n = length(data_sub[,i]) # Longueur de la colonne i
		test_egalite = (data_sub[-n,i] == data_sub[-1,i]) # On compare tout l'historique -dernière date avec tout l'historique décalé d'une date (ie. comparaison de tous les row_i et row_i-1)
		test_egalite[is.na(test_egalite)] = F
		
		compteur = 0
		ligne = 1
		
		while (ligne < n ) # On commence à la ligne 1 et on fini à la dernière ligne -1
		{
			if(test_egalite[ligne] == TRUE)
			{
				compteur = compteur + 1 # On compte le nombre de fois où les valeurs du tableau sont constantes
			}
			else # Dés qu'on repasse sur une valeur non constante, on test:
			{
				if(compteur < seuil_cleaning_data) # Soit le nombre de données constantes est inférieur au seuil...
				{
					compteur = 0 # ... et on réinitialise,
				}
				else # Soit on effectue:
				{
					ind_deb = ligne - compteur +1
					ind_fin = ligne
					data_sub[c(ind_deb:ind_fin),i] = NA # On définit les valeurs commme NA si le seuil est dépassé
					
					compteur = 0
				}
			}
			ligne = ligne +1
		}
	}
	
	return(data_sub)
}


######################################### DIV ########################################
CountConstantRange = function(y,nettoyage_mode=7,type="DIV"){
	dates = as.Date(y$Date,format="%Y-%m-%d")
	n_dates = length(dates)
	bad = c()
	k = 1
	while( k <= (n_dates-nettoyage_mode)){
		test = TRUE
		l = 1
		if(type == "DIV"){
			y_k = as.double(c(y$Y1[k],y$Y2[k],y$Y3[k],y$Y4[k]))
			while(((k + l) <= n_dates) & !(FALSE %in% test) & !(NA %in% test) ){
				test = as.logical(y_k == as.double(c(y$Y1[(k+l)],y$Y2[(k+l)],y$Y3[(k+l)],y$Y4[(k+l)])))
				l = l+1
			}
			if(l >= nettoyage_mode){
				# Historique des dividendes implicites du plus recent au plus ancien
				# Conservation de la valeur en k+l-1
				bad = c(bad,seq(k,(k+l-2),1))
				k = k + l - 2
			}
		}else{
			y_k = y[k,2]
			if(!is.na(y_k)){
				while(((k + l) <= n_dates) & !(FALSE %in% test) & !is.na(test)){
					test = as.logical(y_k == y[(k+l),2] )
					l = l + 1
				}	
			}
			if((l-1) >= nettoyage_mode){
				# Historique des actions du plus ancien au plus recent
				# Conservation de la valeur a la date k
				bad = c(bad,seq((k+1),(k+l-2),1))
				k = k + l - 2
			}
		}
		k = k + 1
	}
	return(list(n_bad=length(bad),bad=bad))
}
RemoveConstantRange = function(y,nettoyage_mode=7,type="DIV"){ #
	PlageConstantes = CountConstantRange(y=y,nettoyage_mode=nettoyage_mode,type=type) #
	if(PlageConstantes$n_bad > 0){
		if(type == "DIV"){
			y = y[-PlageConstantes$bad,]
		}else{
			y[PlageConstantes$bad,2] = NA
		}
	}
	return(list(n_bad=PlageConstantes$n_bad,z = data.frame(y,row.names=NULL)))
}
CountDivAberrant = function(y,stock){
	Spot = as.double(y$Spot)
	n_dates = length(y$Date)
	cpt = 0
	dates = c()
	values = c()
	for(k in seq(1,n_dates,1)){
		y_k = as.double(c(y$Y1[k],y$Y2[k],y$Y3[k],y$Y4[k]))
		if( TRUE %in% unique(y_k < 0 | y_k >= Spot[k])){
			dates = c(dates,as.character(y$Date[k]))
			values = rbind(values,c(Spot[k],y_k))
			cpt = cpt + 1
		}
	}
	z = data.frame()
	if(cpt > 0){
		z = data.frame(Date=as.character(dates),rep(stock,cpt),values,row.names=NULL)
		names(z) = c("Date","Stock","Spot","Div 1Y","Div 2Y","Div 3Y","Div 4Y")	
	}
	return(list(nb_MisData=cpt,Synthese=z))
}


CleanOne = function(name,histo,rdt,dates,CI){
	dates = as.Date(as.character(dates))
	id_d = match(max(CI$Begin,dates[1]),dates)
	id_f = match(min(CI$End,dates[length(dates)]),dates)
	dates_pb = character(0)
	values_pb = c()
	statistiques = c()
	rendement = c()
	moyenne = c()
	ecart_moyenne = c()
	cinq_ecart_type = c()
	for(i in seq(id_d,id_f,1)){
		if(!is.na(rdt[i])){
			Bandwidth = rdt[seq(max(1,(i-0.5*CI$Bandwidth)),min(length(rdt),(i+0.5*CI$Bandwidth)),1)]
			m_Bandwidth = mean(Bandwidth,na.rm=TRUE)
			s_Bandwidth = sd(Bandwidth,na.rm=TRUE)
			if(!is.na(s_Bandwidth)){
				Test = abs(rdt[i]-m_Bandwidth) >= (5*s_Bandwidth)
				if(Test){
					statistiques = c(statistiques, abs(rdt[i]-m_Bandwidth)-(5*s_Bandwidth))
					dates_pb = c(dates_pb,as.character(dates[i]))
					values_pb_t = as.double(histo[seq(max(1,(i-2)),min(length(rdt),(i+2)),1)])
					if(length(values_pb_t)<5){
						if(i==1){ 
							values_pb_t = c(NA,NA,values_pb_t)
						}else if(i==2){
							values_pb_t = c(NA,values_pb_t)
						}else if(i==(length(rdt)-1)){
							values_pb_t = c(values_pb_t,NA)
						}else{
							values_pb_t = c(values_pb_t,NA,NA)
						}
					}
					values_pb = rbind(values_pb,values_pb_t)
					rendement = c(rendement,rdt[i])
					moyenne = c(moyenne,m_Bandwidth)
					ecart_moyenne = c(ecart_moyenne,abs(rdt[i]-m_Bandwidth))
					cinq_ecart_type = c(cinq_ecart_type,(5*s_Bandwidth))
				}
			}
		}
	}
	return(list(name=rep(name,length(dates_pb)),dates_pb=dates_pb,values_pb=values_pb,statistiques=statistiques,
				rendement=rendement,moyenne=moyenne,ecart_moyenne=ecart_moyenne,cinq_ecart_type=cinq_ecart_type))
}
CleanYieldType = function(CI,type,export=FALSE){
	path = paste(CI$DataPath,type,"/",sep="")
	liste = list.files(path)
	suppressWarnings(liste <- as.double(unique(unlist(strsplit(liste,"_")))))
	liste = liste[!is.na(liste)]
	n = length(liste)
	
	# INFORMATIONS
	FirstDate = character(n)
	EndDate = character(n)
	ConstantRange = double(n)
	WrongData = double(n)
	NbData = double(n)
	NbLogReturn1Y = double(n)
	NbLogReturn2Y = double(n)
	NbLogReturn3Y = double(n)
	NbLogReturn4Y = double(n)
	Stocks = character(0)
	Date = character(0)
	Values = c()
	Statistique = c()
	Rendement = c()
	Moyenne = c()
	Ecart_moyenne = c()
	Cinq_ecart_type = c()
	
	# NETTOYAGE NIVEAU DIVYIELD IMPLI
	Synt_Abb = data.frame()
	cat(paste("NETTOYAGE ",type,"\n",sep=""))
	for(i in seq(1,n,1)){
		stock = liste[i]
		#------  NETTOYAGE ET INFORMATIONS SUR DIVIDENDES ----
		cat(paste("Traitement de ",stock,"_DIV.csv ",round(i / n * 100,digits=2),"%\n",sep=""))
		y = read.table(paste(path,stock,"_DIV.csv",sep=""),header=TRUE,sep=",")
		# RESTRICTION AUX DATES DE CALIBRATION
		H = TruncateHistoDatas(dates=as.Date(y$Date,format="%m/%d/%Y"),Histo=as.matrix(y[,-1]),Begin=CI$Begin,End=CI$End)
		y = data.frame(H$dates,H$Histo,row.names=NULL)
		names(y) = c("Date","Spot","Y1","Y2","Y3","Y4")
		
		# NOMBRE DE TOMBEES CONSTANTES
		R = RemoveConstantRange(y=y,nettoyage_mode=CI$nettoyage_mode)
		ConstantRange[i] = R$n_bad
		y = R$z
		FirstDate[i] = as.character(y$Date[length(y$Date)])
		EndDate[i] = as.character(y$Date[1])
		NbData[i] = length(y$Date)
		
		# NOMBRE DE TOMBEES IMPLICITES ABERRANTES
		Abb = CountDivAberrant(y=y,stock=stock)
		WrongData[i] = Abb$nb_MisData
		Synt_Abb = rbind(Synt_Abb,Abb$Synthese)
		
		#------ NETTOYAGE ET INFORMATIONS SUR YIELDS  ------
		y = read.table(paste(path,stock,"_YIELD.csv",sep=""),header=TRUE,sep=",")
		
		returns = ComputeYieldReturns(dates=y$Date,Histo=as.matrix(y[,-c(1,2)]))
		returns_1Y = returns[,1]		
		returns_2Y = returns[,2]		
		returns_3Y = returns[,3]
		returns_4Y = returns[,4]
		
		NbLogReturn1Y[i] = length(returns_1Y[!is.na(returns_1Y)])
		NbLogReturn2Y[i] = length(returns_2Y[!is.na(returns_2Y)])
		NbLogReturn3Y[i] = length(returns_3Y[!is.na(returns_3Y)])
		NbLogReturn4Y[i] = length(returns_4Y[!is.na(returns_4Y)])
		
		#------ NETTOYAGE SPOT ------
		result = CleanOne(name=stock,histo=y$Spot,rdt=diff(log(y$Spot)),dates=y$Date,CI=CI)
		Date = c(Date,result$dates_pb)
		Stocks = c(Stocks,result$name)
		Values = rbind(Values,result$values_pb)
		Statistique = c(Statistique,result$statistiques)
		Rendement = c(Rendement,result$rendement)
		Moyenne = c(Moyenne,result$moyenne)
		Ecart_moyenne = c(Ecart_moyenne,result$ecart_moyenne)
		Cinq_ecart_type = c(Cinq_ecart_type,result$cinq_ecart_type)
	}	
	
	StatsDiv = data.frame(liste,FirstDate,EndDate,NbData,WrongData,ConstantRange,NbLogReturn1Y,NbLogReturn2Y,NbLogReturn3Y,NbLogReturn4Y)
	StatsStocks = data.frame(Date,Stocks,Values,Rendement,Moyenne,Ecart_moyenne,Cinq_ecart_type,Statistique,Statistique>=0.1,row.names=NULL)
	names(StatsStocks) = c("Date","Stock","J-2","J-1","J","J+1","J+2","Rendement","Moyenne","Ecart abs moyenne","Cing Ecart type","DIFF","Statut")
	if(export){
		path_stats = paste(CI$ResPath,"STATISTIQUES/",sep="")
		dir.create(path_stats,showWarnings=FALSE,recursive=FALSE)
		write.table(StatsStocks,paste(path_stats,"StatsStocks ",CI$Name," ",type,".csv",sep=""),row.names=FALSE,quote=FALSE,sep=";")
		write.table(StatsDiv,paste(path_stats,"StatsDiv ",CI$Name," ",type,".csv",sep=""),row.names=FALSE,quote=FALSE,sep=";")	
	}
	return(list(StatsDivs=StatsDiv,StatsStocks=StatsStocks))
}
CleanYield = function(CI,export=FALSE){
	StatsDivs = data.frame()
	StatsStocks = data.frame()
	res_index = CleanYieldType(CI=CI,type="INDICES",export=export)
	res_stock = CleanYieldType(CI=CI,type="ACTIONS",export=export)
	StatsDivs = rbind(StatsDivs,res_index$StatsDivs,res_stock$StatsDivs)
	StatsStocks = rbind(StatsStocks,res_index$StatsStocks,res_stock$StatsStocks)
	return(list(StatsDivs=StatsDivs,StatsStocks=StatsStocks))
}

CleanEqty = function(CI,export=FALSE){ #
	# CHARGEMENT DES HISTORIQUES
	Histo = ExtractHistoricalPricesEqtyIndex(CI=CI,Begin=CI$Begin,End=CI$End) #
	Dates = Histo$dates
	Data = Histo$h
	name = Histo$name
	
	# DETECTION VALEURS ABBERANTES
	Data_dln = apply(log(Data),2,diff,na.rm=TRUE)
	Stocks = character(0)
	Date = character(0)
	Values = c()
	Statistique = c()
	Rendement = c()
	Moyenne = c()
	Ecart_moyenne = c()
	Cinq_ecart_type = c()
	for(k in seq(1,ncol(Data_dln),1)){
		result = CleanOne(name=name[k],histo=Data[,k],rdt=Data_dln[,k],dates=Dates,CI=CI) #
		Date = c(Date,result$dates_pb)
		Stocks = c(Stocks,result$name)
		Values = rbind(Values,result$values_pb)
		Statistique = c(Statistique,result$statistiques)
		Rendement = c(Rendement,result$rendement)
		Moyenne = c(Moyenne,result$moyenne)
		Ecart_moyenne = c(Ecart_moyenne,result$ecart_moyenne)
		Cinq_ecart_type = c(Cinq_ecart_type,result$cinq_ecart_type)
	}
	y = data.frame(Date,Stocks,Values,Rendement,Moyenne,Ecart_moyenne,Cinq_ecart_type,Statistique,Statistique>=0.1,row.names=NULL)
	names(y) = c("Date","Stock","J-2","J-1","J","J+1","J+2","Rendement","Moyenne","Ecart abs moyenne","Cing Ecart type","DIFF","Statut")
	
	# PLAGES CONSTANTES
	n_bad = c()
	y_vf = data.frame(Dates)
	for(k in seq(1,ncol(Data),1)){
		plage_const = RemoveConstantRange(y=data.frame(Date=Dates,as.double(Data[,k]),row.names=NULL), #
										  nettoyage_mode=CI$nettoyage_mod,type="EQTY")
		n_bad = c(n_bad,plage_const$n_bad)
		y_vf = cbind(y_vf,plage_const$z[,2])
	}
	names(y_vf)= c("Date",name)
	
	if(export){
		path_nett = paste(CI$PathOutput,"NETTOYAGE/",sep="")
		dir.create(path_nett,showWarnings=FALSE,recursive=FALSE)
		write.table(y,paste(path_nett,CI$Name,".csv",sep=""),row.names=FALSE,quote=FALSE,sep=";")
		
		# HISTORIQUE NETTOYE DES PLAGES CONSTANTES
		write.table(y_vf,paste(CI$DataPath,"Histo_CLEAN.csv",sep=""),row.names=FALSE,quote=FALSE,sep=";")
		write.table(data.frame(name=name,Suppression_Plage_Constantes = n_bad,row.names=NULL),
					paste(path_nett,CI$Name,"_Plages_Constantes.csv",sep=""),row.names=FALSE,quote=FALSE,sep=";")
	}
	return(y)
}

CleanData = function(CI,export){ #
	n = length(CI)
	res = list()
	for(i in seq(1,n,1)){
		if(CI[[i]]$type == "EQTY"){
			res[[i]] = CleanEqty(CI=CI[[i]],export=export)
		}else{
			res[[i]] = CleanYield(CI=CI[[i]],export=export)
		}
	}
	return(res)
}











