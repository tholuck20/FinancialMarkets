rm(list=ls())
library(Rsolnp)
library(sn)
library(tseries)
library(timeDate)
library(timeSeries)
library(mnormt)
library(KernSmooth) 
library(xtable)
library(Hmisc)
library(lubridate)
library(sfsmisc)
#
Calibration 	 <-"CQ3 2015"
CalibrationName  <-paste("Calibration ",Calibration,sep="")
workingDirectory <-"F:/RISQ-MAR-RIM/FX/EEPE/Calibration/" #D:/achagou021411/USERS/Etudes/Projets en cours/Calibrations du Modele FX/"
#
path_script =paste(workingDirectory,CalibrationName,"/Script Calibration/",sep="")
source(paste(path_script,"FX Calibration Functions.r",sep=""))
source(paste(path_script,"FX BackTest Functions.r",sep=""))
source(paste(path_script,"FX Export Functions.r",sep=""))
############################# Variable Initialisation #########################################
resultPath     		<-	paste(workingDirectory,CalibrationName,"/results/",sep="")
dataPath       		<-	paste(workingDirectory,CalibrationName,"/data/",sep="")
basePath       		<-	paste(workingDirectory,CalibrationName,"/Base/",sep="")
backTestPath        <-  paste(workingDirectory,CalibrationName,"/Back Test Out Of Sample/",sep="")
exportPathBTResults <-  paste(workingDirectory,CalibrationName,"/Presentation/",sep="")
BackTestWindow		<-  list(date_debut="2014-07-01",date_fin="2015-06-30")	
CalibrationWindow 	<-  list(date_debut="1997-01-01",date_fin="2015-06-30")						
# Calibration
Calibration_resutls  		         =	calibrate_FX_Model(resultPath,dataPath,basePath,CalibrationName,CalibrationWindow,44)
In_sample_backtest_results           =	In_sample_backtest_FX_Model(Calibration_resutls)
Out_Of_sample_backtest_results       =	Out_of_Sample_backtest_FX_Model(backTestPath,BackTestWindow)
# Export Results
exportInSampleBackTestResults(In_sample_backtest_results,exportPathBTResults,Calibration)
exportOutOfSampleBackTestResults(Out_Of_sample_backtest_results,exportPathBTResults,Calibration)
exportCalibrationResults(Calibration_resutls,basePath,backTestPath)
