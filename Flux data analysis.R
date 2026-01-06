##################################
#### Pre-treatment of EC data ####
##################################


#Objectives:
# 1/ QC filtering of data: 
#     Reading the EddyPro data  
#     ==> produce "EddyPro_Data_qcCheck_year.Rdata" (Mauder scale 0 and 1 kept only)
# 2/ Preparing to gap-filling and flux partitioning and launch REddyProc procedure
#     Reading the "EddyPro_Data_qcCheck_year.Rdata" and meteo data 
#     ==> produce .txt input file for ReddyProc treatment

# Libraries required ------------------------------------------------------
install.packages("readr")
install.packages("REddyProc")
install.packages("zoo")
install.packages("zoo")
install.packages("oce")
install.packages("zoo")
install.packages("lubridate")
#CALL FOR 
library(REddyProc)
library(readr)
library("tidyr")
library("lattice")
require(chron)
library(zoo)
require(tidyr)
require(Hmisc)
library(openair)
require(plotrix)
library(stringr)
library(data.table)
library(ggplot2)
library(reshape2)
library(cowplot)
library(dplyr)
library(oce)
library(gsw)
library(lubridate)
rm(list=ls())

#GMT+1 time system
Sys.setlocale("LC_TIME", "English")
Sys.setenv(TZ='Etc/GMT-1')

#Year, Licor System
year=2023
Licor_system=7500



#Directories pour traitement juillet 2023

WD=paste0("C:/Users/mabdulsalam/Downloads/essaie/") #"C:/Users/pbuysse/Documents/Projets/ICOS/Data/FluxNet/"
Directory_REddyProc="C:/Users/mabdulsalam/Downloads/essaie/"
Directory_Meteo="C:/Users/mabdulsalam/Downloads/essaie/"
Directory_EddyPro_Data=WD
#WD_Fluxnet="D:/ICOS/Data/FluxNet/To_Upload"

###################################################################################################################################

#date vect to get a complete year of data
Dates_vect <- seq(
  from=as.POSIXct("2023-01-01 00:30",tz="Etc/GMT-1"),
  to=as.POSIXct("2023-12-31 00:00",tz="Etc/GMT-1"),  
  by="30 min"
)




# 1/ Prepare the data and get QCchecked data from EddyPro -----------------------------------------------------

#A/ Read eddypro data and get Rdata dataset

Datafile=dir(path = Directory_EddyPro_Data,pattern = glob2rx("*full_output*"))
Ramphilon_Flux_Data = read.csv(paste0(Directory_EddyPro_Data,Datafile),sep=",",dec=".",header=F, skip=3)  

#Arrange the data in R
Header_Ramphilon_Flux_Data=read.csv(paste0(Directory_EddyPro_Data,Datafile),sep=",",dec=".",header=F,skip=1,nrow=1)
Units_Ramphilon_Flux_Data=read.csv(paste0(Directory_EddyPro_Data,Datafile),sep=",",dec=".",header=F,skip=2,nrow=1)
EddyPro_Data=Ramphilon_Flux_Data[1:dim(Ramphilon_Flux_Data)[1],]
colnames(EddyPro_Data)=make.names(lapply(Header_Ramphilon_Flux_Data,as.character))
EddyPro_Data=as.data.frame(EddyPro_Data)



#Concatenate date and time


EddyPro_Data$DateTime_str <- paste(EddyPro_Data$date, EddyPro_Data$time)
print(head(EddyPro_Data$DateTime_str))

#change date and time to POSIXct
EddyPro_Data$DateTime <- as.POSIXct(EddyPro_Data$DateTime_str, format="%d/%m/%Y %H:%M", tz="Etc/GMT-1")
head(EddyPro_Data)

#EddyPro_Data$DateTime=as.POSIXct(paste(strptime(EddyPro_Data$date,format="%Y-%m-%d"), EddyPro_Data$time), format="%Y-%m-%d %H:%M",tz="Etc/GMT-1")
head(EddyPro_Data)

#Suppress lines for which EddyPro does not have enough data
EddyPro_Data<-EddyPro_Data[!(EddyPro_Data$filename=="not_enough_data"),]
#Check co2 flux
EddyPro_Data[EddyPro_Data ==-9999]= NA
plot(EddyPro_Data$DateTime,EddyPro_Data$co2_flux)
dates_ok <- as.data.frame(Dates_vect)  #Dates_vect defined above
names(dates_ok) <- "DateTime"
#dates_ok$DateTime=as.POSIXct(strptime(dates_ok$DateTime, "%Y-%m-%d %H:%M:%S"),tz="Etc/GMT-1")

write.csv(EddyPro_Data, "C:/Users/mabdulsalam/Downloads/essaie/eddy_pro_data_NA_Blank.csv", row.names = FALSE)

#Continous_Data_Halfhour=merge(x=dates_ok,y=EddyPro_Data,by.x="datetime",by.y="DateTime",all.x=T)
Continous_Data_Halfhour <- left_join(dates_ok,EddyPro_Data,by=c("DateTime"))

###################################################################################################################3

names(EddyPro_Data)

duplicated_columns <- names(EddyPro_Data)[duplicated(names(EddyPro_Data))]
print(duplicated_columns)
# Renaming duplicate columns
names(EddyPro_Data) <- make.unique(names(EddyPro_Data))

# Check the new column names
print(names(EddyPro_Data))
#########################################################################################################################""

#Continous_Data_Halfhour=merge(x=dates_ok,y=EddyPro_Data,by.x="datetime",by.y="DateTime",all.x=T)
Continous_Data_Halfhour <- left_join(dates_ok,EddyPro_Data,by=c("DateTime"))

EddyPro_Data=Continous_Data_Halfhour
plot(EddyPro_Data$DateTime,EddyPro_Data$co2_flux)

#save raw data in Rdata format
save(EddyPro_Data,file=paste0(Directory_EddyPro_Data,"EddyPro_Data_raw_",Licor_system,"_",year,".Rdata"))
#save(EddyPro_Data,file=paste0(WD_Fluxnet,"EddyPro_Data_raw_",Licor_system,"_",year,".Rdata"))


#2/Filter out the data based on different criteria : 
#2.1/remove inconsistent data based on Mauder scale (0-1-2) 
#setwd(Directory_EddyPro_Data)
#load(EddyPro_Data,file=paste0(Directory_EddyPro_Data,"EddyPro_Data_raw_",Licor_system,"_",year,".Rdata"))
plot(EddyPro_Data$co2_flux)
EddyPro_Data[,9:dim(EddyPro_Data)[2]][which(EddyPro_Data$qc_co2_flux==2),]= NA  #remove the data flagged = 2 

plot(EddyPro_Data$co2_flux)


#use other criteria 
#(eg those used by Greg, cf R script BW_filter)
#apply despiking? (DespikeBen function?) 
#based on meteo data also (PAR, rain,...) ==> avoir un dataset continu (datetime) avant

#2.2/ Remove larger and too low fluxes 
#According to the general scale,it should be between -50_ +50
EddyPro_Data[,9:dim(EddyPro_Data)[2]][which(EddyPro_Data$co2_flux<=(-20) |EddyPro_Data$co2_flux>=20),]=NA
plot(EddyPro_Data$co2_flux)
plot(EddyPro_Data$co2_flux)
plot(EddyPro_Data$DateTime,EddyPro_Data$co2_flux)

EddyPro_Data_long <- EddyPro_Data_clean %>%
  pivot_longer(cols = c(co2_flux, ET, H, LE), names_to = "Variable", values_to = "Value")





head(EddyPro_Data)

write.csv(EddyPro_Data, "C:/Users/mabdulsalam/Downloads/essaie/eddy_pro_data_qc_1_2.csv", row.names = FALSE)

head(EddyPro_Data)

######################################################################################################################
# 1bis/ For Li-7500: Eliminate flux data corresponding to rain --------
#Read meteo data
if (Licor_system==7500){
  Ramp_Met_Data_All=read.table(file = paste0(Directory_Meteo,"Ramphillon_Meteo_data.csv"),sep=";",header=T)
  Ramp_Met_Data_All[is.na(Ramp_Met_Data_All)] = -9999
}
Ramp_Met_Data_All <- as.data.frame(Ramp_Met_Data_All)
Ramp_Met_Data_All$DateTime <-as.POSIXct(Ramp_Met_Data_All$DateTime, format = "%d/%m/%Y %H:%M",tz="Etc/GMT-1")
Ramp_Met_Data_All<- left_join(dates_ok,Ramp_Met_Data_All, by =c("DateTime"))

nrow(Ramp_Met_Data_All)
nrow(EddyPro_Data)
#Ramp_Met_Data_All <- Ramp_Met_Data_All %>%
  #mutate(across(where(is.character), as.numeric))
#Ramp_Met_Data_All <- Ramp_Met_Data_All %>%
  #mutate(across(where(is.factor), as.character)) %>%    # Convert factors to characters
  #mutate(across(where(is.character), ~ suppressWarnings(as.numeric(.)))) # Convert characters to numerics if possible

#Adding my precipitation data into my climatic data with the same timestamp

Pre_result <- read.csv("Pre_result.csv", sep = ";")
Pre_result$DateTime <- as.POSIXct(Pre_result$DateTime,format="%d/%m/%Y %H:%M", tz ="ETC/GMT-1")

Pre_result<- left_join(dates_ok,Pre_result, by =c("DateTime"))
#write.csv(Pre_result,"C:/Users/mabdulsalam/Downloads/essaie/pre_result_saved.csv",row.names = FALSE)
Ramp_Met_Data_All <- merge(Ramp_Met_Data_All, Pre_result[, c("DateTime", "P_Value")], by = "DateTime", all.x = TRUE)
# Replace NA values with -9999
#Ramp_Met_Data_All[is.na(Ramp_Met_Data_All)] <- -9999
Ramp_Met_Data_All$P_Value[is.na(Ramp_Met_Data_All$P_Value )] <- -9999
plot(Ramp_Met_Data_All$DateTime,Ramp_Met_Data_All$P_Value)
#Ramp_Met_Data_All$diag_irga_Avg[is.na(Ramp_Met_Data_All$diag_irga_Avg )] <- "" #-9999 
#EddyPro_Data[, 2:ncol(EddyPro_Data)][with(Ramp_Met_Data_All,diag_irga_Avg  != 0), ] <- NA
#EddyPro_Data[!is.na(Ramp_Met_Data_All$diag_irga_Avg ) & Ramp_Met_Data_All$diag_irga_Avg  > 0, 2:ncol(EddyPro_Data)] <- NA

# Proceed with your other operations
#EddyPro_Data[, 2:ncol(EddyPro_Data)][with(Ramp_Met_Data_All, diag_irga_Avg != 0), ] <- NA
View(EddyPro_Data)
nrow(EddyPro_Data)
nrow(Ramp_Met_Data_All)
print(head(EddyPro_Data))
head(Ramp_Met_Data_All)
summary(Ramp_Met_Data_All$diag_irga_Avg)
EddyPro_Data[2:ncol(EddyPro_Data)][Ramp_Met_Data_All$P_Value > 0, ] <-NA #eliminating with precipitation
plot(EddyPro_Data$DateTime,EddyPro_Data$co2_flux)
#EddyPro_Data[2:ncol(EddyPro_Data)][Ramp_Met_Data_All$diag_irga_Avg< 0.8, ] <- "" #NA #eliminating with diagnostic strength. Threshold is 0.8 But 0.9 is more recognise

#####e#################################################################################################################
#To compare number of rows in the two dataSet
#This should be run when the rows of your datasets are not same
#Ramp_Met_Data_All[Ramp_Met_Data_All$DateTime != EddyPro_Data$DateTime, "DateTime"]
#EddyPro_Data[EddyPro_Data$DateTime == Ramp_Met_Data_All$DateTime, "DateTime"]

#Date_all <- data.frame(DateTime = Ramp_Met_Data_All$DateTime)
#EddyPro_Data <- left_join(Date_all,EddyPro_Data,by=c("DateTime"))
#nrow(EddyPro_Data)
#nrow(Ramp_Met_Data_All)
#plot(EddyPro_Data$DateTime,EddyPro_Data$co2_flux)

#Read and align my precipitation data to have the same time period as the other two dataset
#Pre_result <- read.csv("Pre_result.csv", sep = ";")
#Pre_result <- as.data.frame(Pre_result)
#Pre_result$DateTime<- as.POSIXct(Pre_result$DateTime,format="%d/%m/%Y %H:%M", tz ="ETC/GMT-1")
#Pre_result<- left_join(Date_all,Pre_result, by =c("DateTime"))
#write.csv(Pre_result, "C:/Users/mabdulsalam/Downloads/essaie/pre_result_saved.csv",row.names = F)

######################################################################################################################################
plot(EddyPro_Data$DateTime,EddyPro_Data$co2_flux)
#######################################################################################################################3
write.csv(EddyPro_Data, "C:/Users/mabdulsalam/Downloads/essaie/eddy_pro_data_Filter_precipitation_3rd_qc.csv", row.names = FALSE)

write.csv(Ramp_Met_Data_All, "C:/Users/mabdulsalam/Downloads/essaie/Ramp_Met_Data_All.csv", row.names = FALSE)
colnames(EddyPro_Data)
########################################################################################################################

#Ramp_Met_Data_All$DateTime <- as.POSIXct(Ramp_Met_Data_All$DateTime, format = '%d/%m/%Y %H:%M', tz = "Etc/GMT-1")

##################################################################################################
#To compare number of rows in the two data 
#Ramp_Met_Data_All[Ramp_Met_Data_All$DateTime != EddyPro_Data$DateTime, "DateTime"]
#EddyPro_Data[EddyPro_Data$DateTime == Ramp_Met_Data_All$DateTime, "DateTime"]

#Date_all <- data.frame(DateTime = Ramp_Met_Data_All$DateTime)
#EddyPro_Data <- left_join(Date_all,EddyPro_Data,by=c("DateTime"))
nrow(EddyPro_Data)
nrow(Ramp_Met_Data_All)
plot(EddyPro_Data$DateTime,EddyPro_Data$co2_flux)


############################################################################################################################################
#Ramp_Met_Data_All <- read.csv(file = paste0(Directory_Meteo, "Ramphillon_Meteo_data.csv"), sep = ";")

#Ramp_Met_Data_All <- read.csv(file = paste0(Directory_Meteo,"Ramphillon_Meteo_data.csv"), header = TRUE, sep = ";")
#Ramp_Met_Data_All <- Ramp_Met_Data_All[-c(1:2), ]


##################################################################################################################################################################


#Save QC checked data in R format
save(EddyPro_Data,file=paste0(WD,"EddyPro_Data_qcCheck_",Licor_system,"_",year,".Rdata"))
save(EddyPro_Data,file=paste0(Directory_REddyProc,"EddyPro_Data_qcCheck_",Licor_system,"_",year,".Rdata"))


#############################################################################################################################


# 2/ Preparation for REddyProc procedure -------------------------------------


#set the WD
setwd(Directory_REddyProc)
#set dates
Date_start = "2023-01-01 00:30"
Date_end = "2023-12-31 00:00"    

#load the flux dataset to analyze
load(file = paste0(Directory_REddyProc,"EddyPro_Data_qcCheck_",Licor_system,"_",year,".Rdata"))


#despiking the flux data (bien v?rifier les graphiques, que l'on ne retire pas trop d epoints! Sinon changre les param?tres n et k et recommencer)
source("DespikeBen2.R")

list.files()
plot(EddyPro_Data$co2_flux)
b=despike(EddyPro_Data$co2_flux, reference = c("median"), n = 4, k = 7,
          min = NA, max = NA, replace = c("NA"), skip)
points(b, col="red")
EddyPro_Data$co2_flux=b

# Plot the original co2_flux data
plot(EddyPro_Data$DateTime, EddyPro_Data$co2_flux, type = "l", main = "CO2 Flux Before and After Despiking", 
     xlab = "DateTime", ylab = "CO2 Flux", col = "blue")

# Overlay the despiked data
lines(EddyPro_Data$DateTime, EddyPro_Data$co2_flux, col = "red")

plot(EddyPro_Data$LE, ylim=c(-10, 600))
b=despike(EddyPro_Data$LE, reference = c("median"), n = 4, k = 7,
          min = NA, max = NA, replace = c("NA"), skip)
points(b, col="red")
EddyPro_Data$LE=b
# Plot the original LE_flux data
plot(EddyPro_Data$DateTime, EddyPro_Data$LE, type = "l", main = "LE Flux Before and After Despiking", 
     xlab = "DateTime", ylab = "LE Flux", col = "blue")

# Overlay the despiked data
lines(EddyPro_Data$DateTime, EddyPro_Data$LE, col = "red")


plot(EddyPro_Data$H)
b=despike(EddyPro_Data$H, reference = c("median"), n = 4, k = 7,
          min = NA, max = NA, replace = c("NA"), skip)
points(b, col="red")
EddyPro_Data$H=b

plot(EddyPro_Data$ET)
b=despike(EddyPro_Data$ET, reference = c("median"), n = 4, k = 7,
          min = NA, max = NA, replace = c("NA"), skip)
points(b, col="red")
EddyPro_Data$ET=b


plot(EddyPro_Data$h2o_flux)
b=despike(EddyPro_Data$ET, reference = c("median"), n = 4, k = 7,
          min = NA, max = NA, replace = c("NA"), skip)
points(b, col="red")
EddyPro_Data$h2o_flux=b


plot(EddyPro_Data$u.)
b=despike(EddyPro_Data$u., reference = c("median"), n = 4, k = 7,
          min = NA, max = NA, replace = c("NA"), skip)

points(b, col="red")
EddyPro_Data$u.=b
# Plot the original LE_flux data
plot(EddyPro_Data$DateTime, EddyPro_Data$LE, type = "l", main = "UStar Before and After Despiking", 
     xlab = "DateTime", ylab = "u.", col = "blue")

# Overlay the despiked data
lines(EddyPro_Data$DateTime, EddyPro_Data$u., col = "red")

######################################################################################################################
#####################################################################################
#Initializing the REddyProc input file
Data_PostTreated <- data.frame(matrix(ncol = 12, nrow = dim(EddyPro_Data)[1]))
colnames(Data_PostTreated)=c("Year","DOY","Hour","NEE","LE","H","Rg","Tair","rH","VPD","Ustar","ET")

#To get a complete year timeseries (change the date in the vector)
Dates_vect = seq(from=as.POSIXct(Date_start, tz="Etc/GMT-1"),to=as.POSIXct(Date_end,tz="Etc/GMT-1"),by="30 min")
Data_PostTreated$Year=year(Dates_vect)
Data_PostTreated$DOY=yday(Dates_vect)
Data_PostTreated$Hour=(hour(Dates_vect)*60+minute(Dates_vect))/60

#Fill the input file with EC data
Data_PostTreated$NEE=EddyPro_Data$co2_flux
Data_PostTreated$LE=EddyPro_Data$LE
Data_PostTreated$H=EddyPro_Data$H
Data_PostTreated$Ustar=EddyPro_Data$u.
Data_PostTreated$ET=EddyPro_Data$ET
#Data_PostTreated$Windspeed=EddyPro_Data$wind_speed
#Data_PostTreated$L=EddyPro_Data$L
#Data_PostTreated$sigma_v=EddyPro_Data$v_var
colnames(EddyPro_Data)
row_number(EddyPro_Data)
nrow(EddyPro_Data)
nrow(Ramp_Met_Data_All)



############################################################################################################


Ramp_Met_Data_All$SlrW_Avg[Ramp_Met_Data_All$SlrW_Avg<0]=0
Data_PostTreated$Rg[1:dim(Ramp_Met_Data_All)[1]]=Ramp_Met_Data_All$SlrW_Avg
Data_PostTreated$Tair[1:dim(Ramp_Met_Data_All)[1]]=Ramp_Met_Data_All$AirTC_Avg 
#Data_PostTreated$Tsoil[1:dim(Ramp_Met_Data_All)[1]]=Ramp_Met_Data_All$TS 



Ramp_Met_Data_All$RH_Avg[Ramp_Met_Data_All$RH_Avg>100]=100
Data_PostTreated$rH[1:dim(Ramp_Met_Data_All)[1]]=Ramp_Met_Data_All$RH_Avg  
Ramp_Met_Data_All$SVP=610.7*10^(7.5*(Ramp_Met_Data_All$AirTC_Avg )/(237.3+(Ramp_Met_Data_All$AirTC_Avg )))
Data_PostTreated$VPD[1:dim(Ramp_Met_Data_All)[1]]=((100-Ramp_Met_Data_All$RH_Avg)/100)*Ramp_Met_Data_All$SVP/100  
colnames(Data_PostTreated)
#replace NA by -9999 
Data_PostTreated[,4:12][is.na(Data_PostTreated[,4:12])]=-9999

#Create final input file for REddyProc
write.table(x=Data_PostTreated,file=paste0(Directory_REddyProc,"FR-RMP_CLI_",year,"_",Licor_system,".txt"),sep="\t",row.names=FALSE)

write.table(x=Data_PostTreated,file=paste0("C:/Users/mabdulsalam/Downloads/essaie/","Data_PostTreated_Complete_yrs",year,"_",Licor_system,".csv"),sep=",",row.names=FALSE)









rm(list=ls())

#GMT+1 time system
Sys.setlocale("LC_TIME", "English")
Sys.setenv(TZ='Etc/GMT-1')

#Year, Licor System
year=2023
Licor_system=7500
# 1/ Import the data into a R dataframe --------------------


Ramp_Trted_CData<- read.table(paste0("FR-RMP_CLI_", year, "_", Licor_system,".txt"), header = T)

nrow(Ramp_Trted_CData)
#change time stamp from POSIXct to posixct
RampPOSIX.F <-fConvertTimeToPosix(Ramp_Trted_CData, 'YDH',Year = 'Year'
                                      ,Day = 'DOY',Hour = 'Hour')
length(RampPOSIX.F$DateTime)
length(RampPOSIX.F$NEE)

plot(RampPOSIX.F$Datetime, RampPOSIX.F$NEE )
RampPOSIX.F[RampPOSIX.F == -9999] <- NA
#RampPOSIX.F$NEE[ RampPOSIX.F$NEE < -50 ] <- NA
plot(RampPOSIX.F$DateTime, RampPOSIX.F$NEE )

plot(RampPOSIX.F$Ustar, RampPOSIX.F$NEE, type="b", col="blue",
     xlab="Ustar", ylab="NEE", main="Quality of Turbulence Flux ")
plot(RampPOSIX.F$Ustar, RampPOSIX.F$NEE, type="b", col="blue",
     xlab="Ustar", ylab="NEE", main="Quality of Turbulence Flux",
     xlim=c(0, 1))
head(RampPOSIX.F)

#  checking lengths
#length_date <- length(Ramp_Trted_CData$DateTime)
#length_nee <- length(Ramp_Trted_CData$NEE)
#sum(is.na(RampPOSIX.F$DateTime))
#sum(is.na(RampPOSIX.F$NEE))



#GAP FILLING AND PARTITONING MY FLUX
# initialize eddy proc class
Ramphillon.C <- sEddyProc$new('Ramphillon', RampPOSIX.F, c('NEE','Rg','Tair','rH','VPD','Ustar',"ET"))
Ramphillon.C$sSetLocationInfo(LatDeg = 48.3, LongDeg = 3.06, TimeZoneHour = 1)  # or TimeZoneHour = 2 during daylight saving time
View(Ramphillon.C)

############################################################################################
# 2/ Estimating the uStar thresholds ---------------------------------------

Solution_1=T

if (Solution_1){
  #Solution 1: getting annual distributions and use different scenarios
  Ramphillon.C$sEstimateUstarScenarios(nSample = 100L, probs = c(0.05,0.50,0.95))
  # inspect the thresholds to be used by default
  Ramphillon.C$sGetUstarScenarios()
  #(uStarThAgg <- Ramphillon.C$sGetEstimatedUstarThresholdDistribution())
  #EProc$sSetUstarScenarios(
  #  usGetSeasonalSeasonUStarMap(uStarThAgg)[,-2])
  Ramphillon.C$useSeaonsalUStarThresholds()
  # inspect the changed thresholds to be used
  
  # #Gap-filling according to the scenarios
  # Ramphillon.C.C$sMDSGapFillUStarScens("NEE")
  
}else{
  
  #Solution 2: defining subperiods and applying different ustar thresholds to those subperiods
  seasonStarts <- as.data.frame( do.call( rbind, list(
    c(63,2023)
    ,c(212,2023)   # define the subperiods (DOY)
    ,c(244,2023)
    ,c(284,2023)
  )))
  seasonFactor <- usCreateSeasonFactorYdayYear(
    RampPOSIX.F$DateTime - 15*60, starts = seasonStarts)
  
  View(seasonFactor)
#plot to visualize the user-defined seasons 
  plot( NEE ~ DateTime, RampPOSIX.F )
  seasonStartsDate <- fConvertTimeToPosix( data.frame(Year = seasonStarts[,2]
                                                      , DOY = seasonStarts[,1], Hour = 0.25), 'YDH'
                                           , Year = "Year", Day = "DOY", Hour = "Hour")
  abline( v = seasonStartsDate$DateTime)
  
#The user-specific seasoning is provided to the gap-filling by the argument seasonFactor.
  (uStarTh <- Ramphillon.C$sEstUstarThold(seasonFactor = seasonFactor))
#Ramphillon.C$sPlotNEEVersusUStarForSeason( levels(uStarTh$season)[4] )  # to get plots. Change [number] for season
  
  #By default the gap-filling uses annually aggregated estimates of uStar-Threshold. 
  #This usually works for sites with continuous vegetation cover. 
  #For the crop-site of this example, we will use a different threshold for each of the defined seasons, 
  #by calling sEddyProc_useSeaonsalUStarThresholds before gapfilling.
  
#first get uncertainty scenarios
  Ramphillon.C$sEstimateUstarScenarios( 
    seasonFactor = seasonFactor, nSample = 30L,probs = c(0.1,0.9))  #one can change nSample and probs
  #(uStarScens <- usGetSeasonalSeasonUStarMap(
   #EProc$sGetEstimatedUstarThresholdDistribution()
  #))
#EProc$sSetUstarScenarios(uStarScens)
  Ramphillon.C$useSeaonsalUStarThresholds()
  Ramphillon.C$sGetUstarScenarios()
  
}

UStarThresholds<-Ramphillon.C$useSeaonsalUStarThresholds()
View(UStarThresholds)
uStarScens<-  Ramphillon.C$sGetUstarScenarios()

write.table(x=uStarScens,file=paste0("C:/Users/mabdulsalam/Downloads/essaie/","uStarScens",year,"_",Licor_system,".csv"),sep=",",row.names=FALSE)






plot(Ramphillon.C)

# 3/ Gap-filling ----------------------------------------------------------

Ramphillon.C$sMDSGapFillUStarScens('NEE',FillAll = FALSE) 
#grep("NEE_.*_f$",names(Ramphillon.C$sExportResults()), value = TRUE)
#grep("NEE_.*_fsd$",names(Ramphillon.C$sExportResults()), value = TRUE)

Ramphillon.C$sMDSGapFillUStarScens('ET',FillAll = FALSE) 


View(Ramphillon.C)
#plot fingerprint
Ramphillon.C$sPlotFingerprint('NEE_uStar_f', Format='png', 
                           valueLimits = quantile(Ramphillon.C$sExportResults()$NEE_uStar_f, prob = c( 0.05, 0.95), na.rm = TRUE))  #replace "f" by "orig" to visualize the ustar filtering effect

Ramphillon.C$sPlotFingerprint('ET_uStar_orig', Format='png', 

                        valueLimits = quantile(Ramphillon.C$sExportResults()$ET_uStar_orig, prob = c( 0.05, 0.95), na.rm = TRUE))  #replace "f" by "orig" to visualize the ustar filtering effect

# 4/ Cumulated NEE --------------------------------------------------------
# export filled results in a R dataframe and csv
FilledEddyData.F <- Ramphillon.C$sExportResults()

write.csv(FilledEddyData.F, "C:/Users/mabdulsalam/Downloads/essaie/FilledEddyData.F.csv", row.names = FALSE)

FilledEddyDataET.F <- Ramphillon.C$sExportResults()
plot(FilledEddyDataET.F$DateTime, FilledEddyDataET.F$ET_uStar_f, type="l", col="blue",
     xlab="DateTime", ylab="ET", main="ET over Time Filled")

#Calculate cumulated NEE
FilledEddyData.F$cumNEE <- cumsum(FilledEddyData.F$NEE_uStar_f)*12*1800*10^-6

FilledEddyData.F$cumET <- cumsum(FilledEddyDataET.F$ET_uStar_f)

colnames(FilledEddyData.F)
plot(FilledEddyData.F$DateTime, FilledEddyData.F$cumNEE, type="l",
     col="green", xlab="Date", ylab="Cumulative NEE (gC/m²)",
     main="Cumulative Net Ecosystem Exchange over Time")

plot(as.Date( RampPOSIX.F$DateTime),FilledEddyData.F$cumNEE,type="l",col="red")
plot(as.Date( RampPOSIX.F$DateTime),FilledEddyDataET.F$cumNET,type="l",col="red")

plot(as.Date(RampPOSIX.F$DateTime, format="%Y-%m-%d"), FilledEddyDataET.F$cumNET, 
     type = "l", col = "red", xlab = "Date", ylab = "Cumulative ET (mm)")



# 5/ Flux Partitioning ----------------------------------------------------
head(Ramphillon.C)
Ramphillon.C$sMDSGapFill('Tair', FillAll = FALSE) 
#Ramphillon.C$sPlotFingerprint('Tair_f', Format='png', 
                              #valueLimits = quantile(Ramphillon.C$sExportResults()$Tair_uStar_f, prob = c( 0.05, 0.95), na.rm = TRUE))  #replace "f" by "orig" to visualize the ustar filtering effect



View(FilledTair)
Ramphillon.C$sMDSGapFill('VPD', FillAll = FALSE) 


##################################################################



# Step 2: Export and view results (assuming export results contain the data)
#filled_data <- Ramphillon.C$sExportResults()

# Step 3: View the first few rows to ensure gap-filling worked
#head(filled_data)

# Step 4: Plot the filled data (replace `VPD_filled` with actual column name)
#plot(filled_data$DateTime, filled_data$VPD_filled, type = "l", col = "blue",
    # xlab = "Date", ylab = "VPD (gap-filled)", main = "Gap-Filled VPD")
################################################################
#Night-time approach
Ramphillon.C$sMRFluxPartitionUStarScens()  # MR = methode de Reichstein et al 2005 (night fluxes)
grep("GPP.*_f$|Reco",names(Ramphillon.C$sExportResults()), value = TRUE)  #==> to get the names of the possible variables to plot
#plot fingerprints
Ramphillon.C$sPlotFingerprint('GPP_uStar_f', Format='png', 
                           valueLimits = quantile(Ramphillon.C$sExportResults()$GPP_uStar_f, prob = c( 0.05, 0.95), na.rm = TRUE))
Ramphillon.C$sPlotFingerprint('Reco_uStar', Format='png', 
                           valueLimits = quantile(Ramphillon.C$sExportResults()$Reco_uStar, prob = c( 0.05, 0.95), na.rm = TRUE))


# 6/ Estimating the uncertainties -----------------------------------------

FilledEddyData <- Ramphillon.C$sExportResults()
uStarSuffixes <- colnames(Ramphillon.C$sGetUstarScenarios())[-1]
GPPAggCO2 <- sapply( uStarSuffixes, function(suffix) {
  GPPHalfHour <- FilledEddyData[[paste0("GPP_",suffix,"_f")]]
  mean(GPPHalfHour, na.rm = TRUE)
})
molarMass <- 12.011
GPPAgg <- GPPAggCO2 * 1e-6 * molarMass * 3600*24*365.25
print(GPPAgg)
#GPPAgg_kgC <- GPPAggCO2 * 1e-6 * 0.01201 * 3600 * 24 * 365.25
#The difference between those aggregated values is a first estimate of uncertainty range in GPP due to uncertainty of the u??? threshold.
(max(GPPAgg) - min(GPPAgg)) / median(GPPAgg)



# 7/ Store the results in a .txt file and in Rdata format ------------------

FilledRamp.F <- Ramphillon.C$sExportResults()
CombinedData.F <- cbind( Ramp_Trted_CData, FilledRamp.F)
fWriteDataframeToFile(CombinedData.F, paste0("FR-Ramp-Results_",year,"_",Licor_system,".txt"), Dir.s = 'Results')

#save in Rdata format
save(CombinedData.F,file=paste0("FR-Ramp-Results_2_",year,"_",Licor_system,".Rdata"))

#save in CSV format

write.csv(CombinedData.F, "C:/Users/mabdulsalam/Downloads/essaie/FilledRamp.F.csv", row.names = FALSE)


