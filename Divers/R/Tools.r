
######################################### IR #########################################
nb_numeric_data = function(histo_data){ # TOOLS

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
CleanConstData = function(histo_data,seuil_cleaning_data){ # TOOLS

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
QuantMoy_Theo = function(CI,a_infla,b_infla,sigma_infla,N=1e5){
	for(i in 1:CI$nbInfla) #1:4
	{
		ind_deb = 5 * (i-1) + 1
		a = a_infla[ind_deb]
		b = b_infla[ind_deb]
		sigma = sigma_infla[ind_deb]
		res = quantiles_moyennes_diffusion_exp_vasicek(a,b,sigma,N)
		
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













####################################### Global #######################################
ComputeReturns = function(dates,Histo,nbFacteurs){

	dates = as.Date(as.character(dates))
	nb_dates = length(dates)
	log_Histo = cbind(as.data.frame(dates),log(Histo))
	d_log_Histo = cbind(as.data.frame(dates[-nb_dates]),sapply(log_Histo[,2:(nbFacteurs+1)],diff))
	colnames(d_log_Histo) = c("dates",colnames(d_log_Histo)[2:ncol(d_log_Histo)])
	
	return(list(log_datas=log_Histo,d_log_datas=d_log_Histo))
}

vol_fractile = function(x,type=1){

	x = x[!is.na(x)]
	n = length(x)
	m = mean(x,na.rm=TRUE)
	
	vol_sd = sd(x) * sqrt(1 - 1/n)
	
	q1 = quantile(x,0.01,type=type,na.rm=TRUE)
	q99 = quantile(x,0.99,type=type,na.rm=TRUE)
	
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
vol_fractile3 = function(x){

	x = x[!is.na(x)]
	b = mean(x,na.rm=TRUE)
	n = length(x)
	
	vol_sd = sd(x,na.rm=TRUE) * sqrt(1-1/n)
	vol_1p = (quantile(x,0.01) - b) / qnorm(0.01)
	vol_99p = (quantile(x,0.99) - b) / qnorm(0.99)
	
	res = max(vol_sd,vol_1p,vol_99p)
	
	return(res)
}
vol_standard = function (x){
	x = x[!is.na(x)]
	b = mean(x)
	n = length(x)
	vol_std = sd(x,na.rm = T)*sqrt(1-1/n)
	return(vol_std)
}
