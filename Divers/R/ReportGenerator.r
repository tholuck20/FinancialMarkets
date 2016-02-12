exportInSampleBackTestResults<-function(In_sample_backtest_results,exportPath,CQ)
{
    clusters_perf_test_1Day_ <-In_sample_backtest_results$Emerging_currencies$clusters_perf_test_1Day[,-c(6,7,12,13)]
    clusters_perf_test_6M_   <-In_sample_backtest_results$Emerging_currencies$clusters_perf_test_6M[,-c(6,7,12,13)]
	clusters_perf_test_1Y_   <-In_sample_backtest_results$Emerging_currencies$clusters_perf_test_1Y[,-c(6,7,12,13)]
	cluster_rcm_test_1Y_     <-In_sample_backtest_results$Emerging_currencies$cluster_rcm_test_1Y
	stationnary_regime_perf_test_1Day_   <-In_sample_backtest_results$Emerging_currencies$stationnary_regime_perf_test_1Day[,-c(6,7,12,13)]
	stationnary_regime_perf_test_6M_     <-In_sample_backtest_results$Emerging_currencies$stationnary_regime_perf_test_6M[,-c(6,7,12,13)]
	stationnary_regime_perf_test_1Y_     <-In_sample_backtest_results$Emerging_currencies$stationnary_regime_perf_test_1Y[,-c(6,7,12,13)]
	stationnary_regime_perf_test_RCM_1Y_ <-In_sample_backtest_results$Emerging_currencies$stationnary_regime_perf_test_RCM_1Y
	normal_regime_perf_test_1Day         <-In_sample_backtest_results$Emerging_currencies$normal_regime_perf_test_1Day
	normal_regime_perf_test_6M           <-In_sample_backtest_results$Emerging_currencies$normal_regime_perf_test_6M
	normal_regime_perf_test_1Y           <-In_sample_backtest_results$Emerging_currencies$normal_regime_perf_test_1Y
	normal_regime_perf_test_RCM          <-In_sample_backtest_results$Emerging_currencies$normal_regime_perf_test_RCM
	dev_currencies_perf_test_1Day        <-In_sample_backtest_results$Developed_currencies$dev_currencies_perf_test_1Day
	dev_currencies_perf_test_6M          <-In_sample_backtest_results$Developed_currencies$dev_currencies_perf_test_6M
	dev_currencies_perf_test_1Y          <-In_sample_backtest_results$Developed_currencies$dev_currencies_perf_test_1Y
	dev_currencies_perf_test_RCM         <-In_sample_backtest_results$Developed_currencies$dev_currencies_perf_test_RCM
	# colnames(stationnary_regime_perf_test_1Day_comparaison) <-  c("FX", "$Q^{01}_{histo}$","$Q^{01}_{theo|r=1}$","$Q^{01}_{theo|r=10}$", "$Q^{01}_{theo|r=20}$","$Q^{99}_{histo}$","$Q^{99}_{theo|r=1}$","$Q^{99}_{theo|r=10}$", "$Q^{99}_{theo|r=20}$")
	# colnames(stationnary_regime_perf_test_6M_comparaison) <-  c("FX", "$Q^{01}_{histo}$","$Q^{01}_{theo|r=1}$","$Q^{01}_{theo|r=10}$", "$Q^{01}_{theo|r=20}$","$Q^{99}_{histo}$","$Q^{99}_{theo|r=1}$","$Q^{99}_{theo|r=10}$", "$Q^{99}_{theo|r=20}$")
	# colnames(stationnary_regime_perf_test_1Y_comparaison) <-  c("FX", "$Q^{01}_{histo}$","$Q^{01}_{theo|r=1}$","$Q^{01}_{theo|r=10}$", "$Q^{01}_{theo|r=20}$","$Q^{99}_{histo}$","$Q^{99}_{theo|r=1}$","$Q^{99}_{theo|r=10}$", "$Q^{99}_{theo|r=20}$")
	# colnames(stationnary_regime_perf_test_6M_comparaison) <-  c("FX", "$Max Depreciation$","$Q^{01}_{theo|r=1}$","$Q^{01}_{theo|r=10}$", "$Q^{01}_{theo|r=20}$","$Max Appreciation}$","$Q^{99}_{theo|r=1}$","$Q^{99}_{theo|r=10}$", "$Q^{99}_{theo|r=20}$")
	# colnames(stationnary_regime_perf_test_1Y_comparaison) <-  c("FX", "$Max Depreciation$","$Q^{01}_{theo|r=1}$","$Q^{01}_{theo|r=10}$", "$Q^{01}_{theo|r=20}$","$Max Appreciation$","$Q^{99}_{theo|r=1}$","$Q^{99}_{theo|r=10}$", "$Q^{99}_{theo|r=20}$")


	colnames(dev_currencies_perf_test_1Day)  <-  c("FX", "$Quantile^{01}_{histo}$","$Quantile^{01}_{theo}$","Test", "$\\Delta \\sigma^{01}(bp)$","$Quantile^{99}_{histo}$","$Quantile^{99}_{theo}$","Test", "$\\Delta \\sigma^{99}(bp)$")
	colnames(dev_currencies_perf_test_6M )    <-  c("FX", "$Quantile^{01}_{histo}$","$Quantile^{01}_{theo}$","Test", "$\\Delta \\sigma^{01}(bp)$","$Quantile^{99}_{histo}$","$Quantile^{99}_{theo}$","Test", "$\\Delta \\sigma^{99}(bp)$")
	colnames(dev_currencies_perf_test_1Y)    <-  c("FX", "$Quantile^{01}_{histo}$","$Quantile^{01}_{theo}$","Test", "$\\Delta \\sigma^{01}(bp)$","$Quantile^{99}_{histo}$","$Quantile^{99}_{theo}$","Test", "$\\Delta \\sigma^{99}(bp)$")
	colnames(dev_currencies_perf_test_RCM)   <-  c("FX", "$RCM_{histo}$","$RCM_{theo}$","Test", "$\\sigma_{histo}$","$\\sigma_{theo}$","$\\Delta \\sigma^{RCM}(bp)$")
	
	colnames(normal_regime_perf_test_1Day)  <-   c("FX", "$Quantile^{01}_{histo}$","$Quantile^{01}_{theo}$","Test", "$\\Delta \\sigma^{01}(bp)$","$Quantile^{99}_{histo}$","$Quantile^{99}_{theo}$","Test", "$\\Delta \\sigma^{99}(bp)$")
	colnames(normal_regime_perf_test_6M)  	<-   c("FX", "$Quantile^{01}_{histo}$","$Quantile^{01}_{theo}$","Test", "$\\Delta \\sigma^{01}(bp)$","$Quantile^{99}_{histo}$","$Quantile^{99}_{theo}$","Test", "$\\Delta \\sigma^{99}(bp)$")
	colnames(normal_regime_perf_test_1Y)  	<-   c("FX", "$Quantile^{01}_{histo}$","$Quantile^{01}_{theo}$","Test", "$\\Delta \\sigma^{01}(bp)$","$Quantile^{99}_{histo}$","$Quantile^{99}_{theo}$","Test", "$\\Delta \\sigma^{99}(bp)$")
	colnames(normal_regime_perf_test_RCM)  	<-   c("FX", "$RCM_{histo}$","$RCM_{theo}$","Test", "$\\sigma_{histo}$","$\\sigma_{theo}$","$\\Delta \\sigma^{RCM}(bp)$")
	
	colnames(clusters_perf_test_1Day_)      <- c("Rating range", "$Quantile^{01}_{histo}$","$Quantile^{01}_{theo}$","Test", "$\\Delta \\sigma^{01}(bp)$","$Quantile^{99}_{histo}$","$Quantile^{99}_{theo}$","Test", "$\\Delta \\sigma^{99}(bp)$")
	colnames(clusters_perf_test_6M_)  	    <- c("Rating range", "$Quantile^{01}_{histo}$","$Quantile^{01}_{theo}$","Test", "$\\Delta \\sigma^{01}(bp)$","$Quantile^{99}_{histo}$","$Quantile^{99}_{theo}$","Test", "$\\Delta \\sigma^{99}(bp)$")
	colnames(clusters_perf_test_1Y_)  	    <- c("Rating range", "$Quantile^{01}_{histo}$","$Quantile^{01}_{theo}$","Test", "$\\Delta \\sigma^{01}(bp)$","$Quantile^{99}_{histo}$","$Quantile^{99}_{theo}$","Test", "$\\Delta \\sigma^{99}(bp)$")
	colnames(cluster_rcm_test_1Y_)  	    <- c("Rating range", "$RCM_{histo}$","$RCM_{theo}$","Test", "$\\sigma_{histo}$","$\\sigma_{theo}$","$\\Delta \\sigma^{RCM}(bp)$")
	
	colnames(stationnary_regime_perf_test_1Day_)     <- c("FX", "$Quantile^{01}_{histo}$","$Quantile^{01}_{theo}$","Test", "$\\Delta \\sigma^{01}(bp)$","$Quantile^{99}_{histo}$","$Quantile^{99}_{theo}$","Test", "$\\Delta \\sigma^{99}(bp)$")
	# colnames(stationnary_regime_perf_test_1Day_DR)  <- c("FX", "$Quantile^{01}_{histo}$","$Quantile^{01}_{theo}$","Test", "$\\Delta \\sigma^{01}(bp)$","$Quantile^{99}_{histo}$","$Quantile^{99}_{theo}$","Test", "$\\Delta \\sigma^{99}(bp)$")
	colnames(stationnary_regime_perf_test_6M_)  	 <- c("FX", "$Quantile^{01}_{histo}$","$Quantile^{01}_{theo}$","Test", "$\\Delta \\sigma^{01}(bp)$","$Quantile^{99}_{histo}$","$Quantile^{99}_{theo}$","Test", "$\\Delta \\sigma^{99}(bp)$")
	colnames(stationnary_regime_perf_test_1Y_)       <- c("FX", "$Quantile^{01}_{histo}$","$Quantile^{01}_{theo}$","Test", "$\\Delta \\sigma^{01}(bp)$","$Quantile^{99}_{histo}$","$Quantile^{99}_{theo}$","Test", "$\\Delta \\sigma^{99}(bp)$")
	colnames(stationnary_regime_perf_test_RCM_1Y_)   <- c("FX", "$RCM_{histo}$","$RCM_{theo}$","Test", "$\\sigma_{histo}$","$\\sigma_{theo}$","$\\Delta \\sigma^{RCM}(bp)$")

	file_1=paste("Test de performance ",CQ,".tex",sep="")
	write(paste("\\documentclass{article} \n \\usepackage{color} \n \\usepackage[margin=0.5in]{geometry} \n \\title{Modele FX: Tests de Performance ",CQ," } \n \\author{RISQ/MAR/RIM}\n \\begin{document} \n \\maketitle \\newpage",sep=""),file=file_1,append=FALSE)
	xx<-latex(xtable(dev_currencies_perf_test_1Day),title="",caption="Developed currencies: 1 day returns performance test",caption.loc='bottom',file=file_1,append=TRUE,spacing=0.7)
	xx<-latex(xtable(dev_currencies_perf_test_6M),title="",caption="Developed currencies: 6 months returns performance test",caption.loc='bottom',file=file_1,append=TRUE,spacing=0.7)
	xx<-latex(xtable(dev_currencies_perf_test_1Y),title="",caption="Developed currencies: 1 year returns performance test",caption.loc='bottom',file=file_1,append=TRUE,spacing=0.7)
	xx<-latex(xtable(dev_currencies_perf_test_RCM),title="",caption="Developed currencies: RCM performance test",caption.loc='bottom',file=file_1,append=TRUE,spacing=0.7)

	xx<-latex(xtable(normal_regime_perf_test_1Day[-25,]),title="",caption="Emerging currencies normal regime: 1 day returns performance test",caption.loc='bottom',file=file_1,append=TRUE,spacing=0.7)
	xx<-latex(xtable(normal_regime_perf_test_6M  [-25,]),  title="",caption="Emerging currencies normal regime: 6 months returns performance test",caption.loc='bottom',file=file_1,append=TRUE,spacing=0.7)
	xx<-latex(xtable(normal_regime_perf_test_1Y  [-25,]),  title="",caption="Emerging currencies normal regime: 1 year returns performance test",caption.loc='bottom',file=file_1,append=TRUE,spacing=0.7)
	xx<-latex(xtable(normal_regime_perf_test_RCM [-25,]), title="",caption="Emerging currencies normal regime: RCM performance test",caption.loc='bottom',file=file_1,append=TRUE,spacing=0.7)
	
	xx<-latex(xtable(clusters_perf_test_1Day_),title="",caption="Emerging currencies crisis regime: 1 day returns performance test",caption.loc='bottom',file=file_1,append=TRUE,spacing=0.7)
	xx<-latex(xtable(clusters_perf_test_6M_),  title="",caption="Emerging currencies crisis regime: 6 months returns performance test",caption.loc='bottom',file=file_1,append=TRUE,spacing=0.7)
	xx<-latex(xtable(clusters_perf_test_1Y_),  title="",caption="Emerging currencies crisis regime: 1 year returns performance test",caption.loc='bottom',file=file_1,append=TRUE,spacing=0.7)
	xx<-latex(xtable(cluster_rcm_test_1Y_),    title="",caption="Emerging currencies crisis regime: RCM performance test",caption.loc='bottom',file=file_1,append=TRUE,spacing=0.7)
	
	xx<-latex(xtable(stationnary_regime_perf_test_1Day_ [-25,]),  title="",caption="Emerging currencies stationary regime: 1 day returns performance test",caption.loc='bottom',file=file_1,append=TRUE,spacing=0.7)
	# xx<-latex(xtable(stationnary_regime_perf_test_1Day_DR [-25,]),  title="",caption="Emerging currencies stationary regime (diffused ratings): 1 day returns performance test",caption.loc='bottom',file="Test de Performance CQ1 2014.tex",append=TRUE,spacing=0.7)

	xx<-latex(xtable(stationnary_regime_perf_test_6M_   [-25,]),    title="",caption="Emerging currencies stationary regime: 6 months returns performance test",caption.loc='bottom',file=file_1,append=TRUE,spacing=0.7)
	xx<-latex(xtable(stationnary_regime_perf_test_1Y_   [-25,]),    title="",caption="Emerging currencies stationary regime: 1 year returns performance test",caption.loc='bottom',file=file_1,append=TRUE,spacing=0.7)
	xx<-latex(xtable(stationnary_regime_perf_test_RCM_1Y_[-25,]),title="",caption="Emerging currencies stationary regime: RCM performance test",caption.loc='bottom',file=file_1,append=TRUE,spacing=0.7)
	 	# xx<-latex(xtable(stationnary_regime_perf_test_1Y_comparaison),title="",caption="Emerging currencies stationary regime and conditional rating: 1 year returns quantile comparison",caption.loc='bottom',file="Test de Performance CQ1 2014.tex",append=TRUE,spacing=0.7)
 	
    
	write("\\end{document}",file=file_1,append=TRUE)
	zz	 <- as.character(read.table(file=file_1,sep = "\n")$V1)
	zz  <-	 gsub("(\\TRUE)", "\\\\textcolor{green}{\\1}", zz)
	zz  <-	 gsub("(\\FALSE)", "\\\\textcolor{red}{\\1}", zz)
	prev_wd <- getwd()
	setwd(exportPath)
	write(zz,file=file_1)
	setwd(prev_wd)
    # file_2=paste("Stationary regime and Conditional rating ",CQ,".tex",sep="")
	# write(paste("\\documentclass{article} \n \\usepackage[dvips]{color} \n \\usepackage[margin=0.5in]{geometry} \n \\title{Modele FX: Tests de Performance ",CQ," } \n \\author{RISQ/MAR/MRC}\n \\begin{document} \n \\maketitle \\newpage",sep=""),file=file_2,append=FALSE)
	# xx<-latex(xtable(stationnary_regime_perf_test_1Day_comparaison),title="",caption="Emerging currencies stationary regime and conditional rating: 1 day returns quantile comparison",caption.loc='bottom',file=file_2,append=TRUE,spacing=0.7)
    # xx<-latex(xtable(stationnary_regime_perf_test_6M_comparaison),title="",caption="Emerging currencies stationary regime and conditional rating: 6 months returns quantile comparison",caption.loc='bottom',file=file_2,append=TRUE,spacing=0.7)
   	# xx<-latex(xtable(stationnary_regime_perf_test_1Y_comparaison),title="",caption="Emerging currencies stationary regime and conditional rating: 1 year returns quantile comparison",caption.loc='bottom',file=file_2,append=TRUE,spacing=0.7)
    # write("\\end{document}",file=file_2,append=TRUE)
	# tools::texi2pdf(file="C:/Users/Etudes/Test de Performance CQ1 2014.tex")
	

}	

exportOutOfSampleBackTestResults <-function(Out_Of_sample_backtest_results,exportPath,CQ)
{

	 prev_wd <- getwd()
	 setwd(exportPath)

	
	
	BTest1J_dev_currencies     <- Out_Of_sample_backtest_results$BTest1J$BTestResults_Dev_FX[,c(1,2,3,4,5,6,31,32,33,34)]
	BTest1J_emrging_currencies <- Out_Of_sample_backtest_results$BTest1J$BTestResults_Emerg_FX[,c(1,2,3,4,5,6,31,32,33,34)]
	BTest9J_dev_currencies     <- Out_Of_sample_backtest_results$BTest9J$BTestResults_Dev_FX[,c(1,2,3,4,5,6,31,32,33,34)]
	BTest9J_emrging_currencies <- Out_Of_sample_backtest_results$BTest9J$BTestResults_Emerg_FX[,c(1,2,3,4,5,6,31,32,33,34)]
	colnames(BTest1J_dev_currencies) <- c("$FX$", "$N$","$N^{]0\\%,1\\%]}$","$L_{N,95\\%}^{]0\\%,1\\%]}$","$L_{N,99.99\\%}^{]0\\%,1\\%]}$", "$result$","$N^{[99\\%,100\\%[}$","$L_{N,95\\%}^{[99\\%,100\\%[}$","$L_{N,99.99\\%}^{[99\\%,100\\%[}$", "$result$")
	colnames(BTest1J_emrging_currencies) <- c("$FX$", "$N$","$N^{]0\\%,1\\%]}$","$L_{N,95\\%}^{]0\\%,1\\%]}$","$L_{N,99.99\\%}^{]0\\%,1\\%]}$", "$result$","$N^{[99\\%,100\\%[}$","$L_{N,95\\%}^{[99\\%,100\\%[}$","$L_{N,99.99\\%}^{[99\\%,100\\%[}$", "$result$")
	colnames(BTest9J_dev_currencies) <-c("$FX$", "$N$","$N^{]0\\%,1\\%]}$","$L_{N,95\\%}^{]0\\%,1\\%]}$","$L_{N,99.99\\%}^{]0\\%,1\\%]}$", "$result$","$N^{[99\\%,100\\%[}$","$L_{N,95\\%}^{[99\\%,100\\%[}$","$L_{N,99.99\\%}^{[99\\%,100\\%[}$", "$result$")
	colnames(BTest9J_emrging_currencies) <- c("$FX$", "$N$","$N^{]0\\%,1\\%]}$","$L_{N,95\\%}^{]0\\%,1\\%]}$","$L_{N,99.99\\%}^{]0\\%,1\\%]}$", "$result$","$N^{[99\\%,100\\%[}$","$L_{N,95\\%}^{[99\\%,100\\%[}$","$L_{N,99.99\\%}^{[99\\%,100\\%[}$", "$result$")
	file_name=paste("Back Test Out Of Sample ",CQ,".tex")
	write(paste("\\documentclass{article} \n \\usepackage[dvips]{color} \n \\usepackage[margin=0.5in]{geometry} \n \\title{Modele FX: Back Test Out Of Sample ",CQ," } \n \\author{RISQ/MAR/RIM}\n \\begin{document} \n \\maketitle \\newpage",sep=""),file=file_name,append=FALSE)
	xx<-latex(xtable(BTest1J_dev_currencies),title="",caption="Developed currencies: 1 day returns Back Test Out Of Sample",caption.loc='bottom',file=file_name,append=TRUE,spacing=0.7,n.rgroup=nrow(BTest1J_dev_currencies), rgroup=c(""),n.cgroup=c(1,1,4,4), cgroup=c(" "," ","]0\\%,1\\%]", "[99\\%,100\\%["))
	xx<-latex(xtable(BTest9J_dev_currencies),title="",caption="Developed currencies: 9 days returns Back Test Out Of Sample",caption.loc='bottom',file=file_name,append=TRUE,spacing=0.7,n.rgroup=nrow(BTest9J_dev_currencies), rgroup=c(""),n.cgroup=c(1,1,4,4), cgroup=c(" "," ","]0\\%,1\\%]", "[99\\%,100\\%["))
	## Note
	write(paste("\\footnote{  \\$N$:Total number of return backtested \n  \\$N^{]p\\%,q\\%]}$: Total number of returns in the $]p\\%,q\\%]$ slice \n \\$L_{N,\\alpha\\%}^{]p\\%,q\\%]}$: $\\alpha\\%$ quantile of binomial distribution with parameters size = $N$ and prob = $q\\%-p\\%$ .\n }",sep=""),file=file_name,append=TRUE)
	
	xx<-latex(xtable(BTest1J_emrging_currencies),title="",caption="Emerging currencies: 1 day returns Back Test Out Of Sample",caption.loc='bottom',file=file_name,append=TRUE,spacing=0.7,n.rgroup=nrow(BTest1J_emrging_currencies), rgroup=c(""),n.cgroup=c(1,1,4,4), cgroup=c(" "," ","]0\\%,1\\%]", "[99\\%,100\\%["))
	xx<-latex(xtable(BTest9J_emrging_currencies),title="",caption="Emerging currencies: 9 days returns Back Test Out Of Sample",caption.loc='bottom',file=file_name,append=TRUE,spacing=0.7,n.rgroup=nrow(BTest9J_emrging_currencies), rgroup=c(""),n.cgroup=c(1,1,4,4), cgroup=c(" "," ","]0\\%,1\\%]", "[99\\%,100\\%["))
	write("\\end{document}",file=file_name,append=TRUE)
	 setwd(prev_wd)

}

exportCalibrationResults <- function(Calibration_resutls,basePath,backTestPath)
{
  # File Formatting for Scenario Engine
  {
   cluster_1_5		<- Calibration_resutls$Emerging_currencies$cluster_1_5
   cluster_6_14		<- Calibration_resutls$Emerging_currencies$cluster_6_14
   cluster_15_20    <- Calibration_resutls$Emerging_currencies$cluster_15_20
   tansitionProbabilities <- Calibration_resutls$Emerging_currencies$tp

   ratings     <-seq(1,20)
   VolMSWRR    <-NULL
   lambdaMSWRR <-NULL
   r_down_MSWRR     <-NULL
   for(i in c(1:length(ratings)))
   {
			if( ratings[i] <= 5 )
			{
						VolMSWRR<-c(VolMSWRR,ratings[i],cluster_1_5$sigma )
						lambdaMSWRR<-c(lambdaMSWRR,ratings[i],cluster_1_5$lambda_down )
						r_down_MSWRR<-c(r_down_MSWRR,ratings[i],cluster_1_5$r_down )
			}else if ( (ratings[i] >= 6) & (ratings[i] <= 14))
			{
						VolMSWRR<-c(VolMSWRR,ratings[i],cluster_6_14$sigma )
						lambdaMSWRR<-c(lambdaMSWRR,ratings[i],cluster_6_14$lambda_down )
						r_down_MSWRR<-c(r_down_MSWRR,ratings[i],cluster_6_14$r_down )

			}else if (ratings[i] >= 15 )
			{
						VolMSWRR<-c(VolMSWRR,ratings[i],cluster_15_20$sigma )
						lambdaMSWRR<-c(lambdaMSWRR,ratings[i],cluster_15_20$lambda_down )
						r_down_MSWRR<-c(r_down_MSWRR,ratings[i],cluster_15_20$r_down )
	        }
	
   }
   
   emerging_currencies  <-Calibration_resutls$Emerging_currencies_list
   peggCurrencies       <-Calibration_resutls$pegg_currencies_list
   emerging_pegg_currencies<-c(emerging_currencies,peggCurrencies)
   VolMSWRR_all_currencies          <-NULL
   lambdaMSWRR_all_currencies       <-NULL
   r_down_MSWRR_all_currencies      <-NULL
   vol_regime_normal_all_currencies <-NULL
   p00_all_currencies<-NULL
   p10_all_currencies<-NULL
   p01_all_currencies<-NULL
   p11_all_currencies<-NULL
   VolMSWRR_str    <-paste_function(VolMSWRR,",")
   lambdaMSWRR_str <-paste_function(lambdaMSWRR,",")
   r_down_MSWRR_str<-paste_function(r_down_MSWRR,",")
   emerging_vol_normal      <-cbind(emerging_currencies, Calibration_resutls$Emerging_currencies$vol_regime_normal[,2])
   peggCurrencies_vol_Normal<-cbind(peggCurrencies,Calibration_resutls$Pegg_Currencies$peggCurrencies_vol)
   vol_regime_normal<-rbind(emerging_vol_normal,peggCurrencies_vol_Normal)
   for(i in c(1:length(emerging_pegg_currencies)))
   {  
		VolMSWRR_all_currencies   	        <-rbind(VolMSWRR_all_currencies,paste("FX",emerging_pegg_currencies[i],VolMSWRR_str,sep=""))
		lambdaMSWRR_all_currencies	        <-rbind(lambdaMSWRR_all_currencies,paste("FX",emerging_pegg_currencies[i],lambdaMSWRR_str,sep=""))
		r_down_MSWRR_all_currencies	        <-rbind(r_down_MSWRR_all_currencies,paste("FX",emerging_pegg_currencies[i],r_down_MSWRR_str,sep=""))
        vol_regime_normal_all_currencies	<-rbind(vol_regime_normal_all_currencies,paste("FX",emerging_pegg_currencies[i],",",vol_regime_normal[i,2],sep=""))
    	p00_all_currencies<-rbind(p00_all_currencies,paste("FX",emerging_pegg_currencies[i],",",tansitionProbabilities$p_0_0_0,sep=""))
        p10_all_currencies<-rbind(p10_all_currencies,paste("FX",emerging_pegg_currencies[i],",",tansitionProbabilities$p_1_1_0,sep=""))
        p01_all_currencies<-rbind(p01_all_currencies,paste("FX",emerging_pegg_currencies[i],",",tansitionProbabilities$p_0_0_1,sep=""))
        p11_all_currencies<-rbind(p11_all_currencies,paste("FX",emerging_pegg_currencies[i],",",tansitionProbabilities$p_1_1_1,sep=""))
   }
  	p0Sys<-paste("FXSYS",tansitionProbabilities$p0Sys,sep=",")
	p1Sys<-paste("FXSYS",tansitionProbabilities$p1Sys,sep=",")

  	}
 
  #Export Scenario Engine Results
  {
		write(p00_all_currencies,paste(basePath,"SG.SG.P00.csv",sep=""))
		write(p10_all_currencies,paste(basePath,"SG.SG.P10.csv",sep=""))
		write(p01_all_currencies,paste(basePath,"SG.SG.P01.csv",sep=""))
		write(p11_all_currencies,paste(basePath,"SG.SG.P11.csv",sep=""))
		write(p0Sys,paste(basePath,"SG.SG.P0Sys.csv",sep=""))
		write(p1Sys,paste(basePath,"SG.SG.P1Sys.csv",sep=""))
		write(vol_regime_normal_all_currencies,paste(basePath,"SG.SG.Volatility.MSWRR.csv",sep=""))
		write(VolMSWRR_all_currencies,paste(basePath,"SG.SG.Volatility.RatingModel.csv",sep=""))
		write(lambdaMSWRR_all_currencies,paste(basePath,"SG.SG.Lambda.RatingModel.csv",sep=""))
		write(r_down_MSWRR_all_currencies,paste(basePath,"SG.SG.Rate.RatingModel.csv",sep=""))
		write(Calibration_resutls$Developed_currencies$dev_FX_Vol,paste(basePath,"SG.SG.Volatility.FX.DEV.csv",sep=""))
		write(Calibration_resutls$corr_FX,paste(basePath,"SG.SG.Correlation.FX.NoXFloor.csv",sep=""))
		write(Calibration_resutls$corr_FX_XFloor,paste(basePath,"SG.SG.Correlation.FX.csv",sep=""))
		##write.table(data.frame(vol_fract_std),paste(resultPath,"VolRegimeNormal.csv",sep=""),sep=";",row.names =F)
		###result export
		# write.table(data.frame(crisis_histo_list),paste(resultPath,"Histo_Crisis_List.csv",sep=""),sep=";",row.names =F)
		# write.table(data.frame(crisis_theo_list),paste(resultPath,"Theo_Crisis_List.csv",sep=""),sep=";",row.names =F)
		# write.table(data.frame(Contagion_transition_prob_list),"Projets en cours/New FX Model Integration RAting/Calibration du Modèle [Methodo]/Resultats/V4/FX_RS_recap_transition_proba_global_crisis_last.csv",sep=";",row.names =F)
		# write.table(data.frame(paramStudent),paste(resultPath,"RS_param.csv",sep=""),sep=";",row.names =F)
		# write.table(data.frame(crisis_cluster_2),paste(resultPath,"Clusters_param.csv",sep=""),sep=";",row.names =F)
		# write.table(data.frame(EM_crisis$EmergingCrisisList),paste(resultPath,"EmergingCrisisList.csv",sep=""),sep=";",row.names =F)
		# write.table(data.frame(FX=vol$name,vol_std=vol$std,vol_fract=vol$fract),paste(resultPath,"Volatilies.csv",sep=""),sep=";",row.names =F)
}
 #file formatting for backTest exercice
  {
    ####export transition probabilities
	BackTestParamPath<- paste(backTestPath,"param/",sep="")
	write(vol_regime_normal_all_currencies,paste(BackTestParamPath,"SG.SG.NormalRegime.Volatilities.BT.csv",sep=""))
	clustersParam=rbind(c("crisis_volatility","rate","lambda"),c(Calibration_resutls$Emerging_currencies$cluster_1_5$sigma,Calibration_resutls$Emerging_currencies$cluster_1_5$r_down,Calibration_resutls$Emerging_currencies$cluster_1_5$lambda_down),c(Calibration_resutls$Emerging_currencies$cluster_6_14$sigma,Calibration_resutls$Emerging_currencies$cluster_6_14$r_down,Calibration_resutls$Emerging_currencies$cluster_6_14$lambda_down),c(Calibration_resutls$Emerging_currencies$cluster_15_20$sigma,Calibration_resutls$Emerging_currencies$cluster_15_20$r_down,Calibration_resutls$Emerging_currencies$cluster_15_20$lambda_down))
	write.table(data.frame(clustersParam),paste(BackTestParamPath,"SG.SG.Clusters.param.BT.csv",sep=""),sep=",",row.names =F,col.names =F)
	write.table(data.frame(rbind(c("p0Sys","p1Sys","p_0_0_1","p_1_1_1","p_0_0_0","p_1_1_0"),c(Calibration_resutls$Emerging_currencies$tp$p0Sys,Calibration_resutls$Emerging_currencies$tp$p1Sys,Calibration_resutls$Emerging_currencies$tp$p_0_0_1,Calibration_resutls$Emerging_currencies$tp$p_1_1_1,Calibration_resutls$Emerging_currencies$tp$p_0_0_0,Calibration_resutls$Emerging_currencies$tp$p_1_1_0))),paste(BackTestParamPath,"SG.SG.TransitionProbabilities.BT.csv",sep=""),sep=",",row.names =F,col.names =F)
    write(Calibration_resutls$Developed_currencies$dev_FX_Vol,paste(BackTestParamPath,"SG.SG.Dev.Volatility.BT.csv",sep=""))
  }

 
} 
 
 drawTable<-function(file_name,table_contents,col_Names,caption)
 {
    table_contents<-data.frame(table_contents)
	colnames(table_contents)<-col_Names
	write(paste("\\documentclass{article} \n \\usepackage[dvips]{color} \n \\usepackage[margin=0.5in]{geometry} \n \\title{Inflation Model} \n \\author{RISQ/MAR/RIM}\n \\begin{document} \n \\maketitle \\newpage",sep=""),file=file_name,append=FALSE)
	xx<-latex(xtable(table_contents),title="",caption=caption,caption.loc='bottom',file=file_name,append=TRUE,spacing=0.7,n.rgroup=nrow(table_contents), rgroup=c(""))
	write("\\end{document}",file=file_name,append=TRUE)
	
 }
 
 
 GenerateReport <- function(fileName,CalibrationOutput,exporPath)
 {
	CQ 						         <- CalibrationOutput$CQ
	In_sample_backtest_results       <- CalibrationOutput$In_sample_backtest_results
	Out_Of_sample_backtest_results   <- CalibrationOutput$Out_Of_sample_backtest_results
	ModelPresentation.Print(fileName)	
    ParametersEvolution.Print(fileName) 
    BacktestMethod.Print(fileName)
    BackTestResults.Print(fileName)
    EAD.EEPE.Impact.Print(fileName)
 }
 ModelPresentation.Print <- function()
 {
 
 }
 ParametersEvolution.Print <- function()
 {
 
 }
 BacktestMethod.Print<- function()
 {
 
 }
 BackTestResults.Print<- function()
 {
 }
 EAD.EEPE.Impact.Print<- function()
 {
 }
 
 
