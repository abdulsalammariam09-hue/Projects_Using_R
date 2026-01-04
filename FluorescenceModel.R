#Set the working directory
setwd("C:/Users/Dell/Documents/classes/numerical modeling")
#setwd("~/Numerical Modelling of fluorescence in a boreal forest overtime")
#=================================================================

library(graphics)
library(chron)
library(ggplot2) 
library(dplyr)  
library(lubridate) 
library(cowplot)

#===============================================================================

#Load Data
PAM_Data <- read_csv("MONITORING PAM Data T6 2008-2009 (2).csv", 
                     skip = 6)

#transform data
PAM_Data=as.data.frame(PAM_Data)
PAM_Data$DateTime<-as.POSIXct(as.character(PAM_Data$DateTime),format="%m/%d/%y %H:%M", tz="UTC")

#===============================================================================

#Fluorescence rate coefficient (kF)= 0.05

kF<-0.05

#===============================================================================
#Dynamic heat dissipation rate coefficient (kD)= 0.95


kD<-0.95

#===============================================================================

#Rate constant of sustained thermal energy dissipation kS = ( FmR/Fm − 1)(kf + kD )

calculate_NPQs_rate <- function(FmR, Fm, kf, kD) {
  kS <- (FmR/Fm - 1) * (kf + kD)
  return(kS)
}


FmR <-   1155  # found from average of seasonal FmR values in data set
FmM <- PAM_Data$Fm


kS <- calculate_NPQs_rate(FmR, FmM, kF, kD)
cat("NPQs Rate Coefficient (kS):", kS, "\n")


#===============================================================================
#Reversible NPQ rate coefficient (kR)=(FmR/F′m − FmR/Fm)*(kF + kD)

calculate_reversible_NPQ_rate <- function(FmR, Fm, FmP, kF, kD) {
  kR <- (FmR/FmP - FmR/Fm) * (kF + kD)
  return(kR)
}


FmP <- PAM_Data$Fm_Corrected


kR <- calculate_reversible_NPQ_rate(FmR, FmM, FmP, kF, kD)
cat("Reversible NPQ Rate Coefficient (kR):", kR, "\n")

#===============================================================================
#Non-photochemical quenching (NPQ) rate coefficient (kN)= kR + kS

calculate_NPQ_rate <- function(kR, kS) {
  kN <- kR + kS
  return(kN)
}


kN <- calculate_NPQ_rate(kR, kS)
cat("NPQ Rate Coefficient (kN):", kN, "\n")


#===============================================================================
#Photochemistry rate coefficient (kP) = 4.0

kP<-4

#===============================================================================

fnps <- 0.15

#===============================================================================
#Potential transport rate (Je)= 0.5(1−fnps)APAR

calculate_Potential_transport_rate <- function(fnps, APAR) {
  je <- 0.5 * (1-fnps) * APAR
  return(je)
}

APAR <-  PAM_Data$PAR

je <- calculate_Potential_transport_rate(fnps, APAR)
cat("Potential transport rate (Je):", je, "\n")

#===============================================================================
#Degree of light saturation (x) = 1-ja/je

calculate_degree_of_light_saturation <- function (ja, je) {
  x = 1 - ja/je
  return(x)
}

ja <- PAM_Data$ETR

x <- calculate_degree_of_light_saturation (ja, je)  
cat ("Degree of light saturation (x):", x, "\n")


#===============================================================================
#Maximum photochemical yeild (ΦPmax) = KP/ KF + KD + KP + Ks

calculate_Maximum_photochemical_yeild <- function(kF, kD, kP,kS) {
  phi_pmax <- kP/ (kF + kD + kP + kS)
  return(phi_pmax)
}


phi_pmax <-calculate_Maximum_photochemical_yeild(kF, kD, kP,kS)
cat("maximum photochemical yeild (Φ_Pmax):",phi_pmax, "\n")


#===============================================================================
#Photochemical yeild (ΦP)= ΦPmax (1-x)

calculate_Photochemical_yeild <- function(phi_pmax, x){
  phi_P <- phi_pmax*(1-x)  
  return(phi_P)
}


phi_P <- calculate_Photochemical_yeild(phi_pmax, x)
cat("Photochemical Yield (Φ_P):", phi_P, "\n")

#===============================================================================
#Fluorescence yeild (ΦF)= kF/ kF + kD + kN * (1-ΦP)

calculate_fluorescence_yield <- function(kF, kD, kN, phi_P) {
  phi_F <- (kF / (kF + kD + kN)) * (1 - phi_P)
  return(phi_F)
}


phi_F <- calculate_fluorescence_yield(kF, kD, kN, phi_P)
cat("Fluorescence Yield (Φ_F):", phi_F, "\n")

#================================================================================

#Leaf fluorescence: Fleaf= ΦF(APAR)

calculate_leaf_fluorescence <- function(phi_F, APAR) {
  F_leaf <- phi_F * APAR
  return(F_leaf)
}



F_leaf <- calculate_leaf_fluorescence(phi_F, APAR)
cat("Leaf fluorescence (F_leaf):", F_leaf, "\n")


# Model results -----------------------------------------------------------

## add into full data set

PAM_Data$kS<- kS
PAM_Data$kR<- kR
PAM_Data$kN<- kN
PAM_Data$je<- je
PAM_Data$phi_pmax<-phi_pmax
PAM_Data$phi_P<-phi_P
PAM_Data$phi_F<-phi_F
PAM_Data$F_Leaf<-F_leaf


# Real v simulated comparison ---------------------------------------------

PAM_Data$year <- as.character(year(PAM_Data$DateTime))
PAM_Data$month <- as.character(month(PAM_Data$DateTime))
PAM_Data$season <- ifelse((PAM_Data$month=="12"|PAM_Data$month=="1"),"winter","summer")

ks_test<-PAM_Data%>% ggplot(aes(kS,kNPQs))+
  geom_point()+
  theme_classic()+
  labs(x = "Simulated kS",y="Measured kS")+
  geom_smooth(method = "lm", formula= y~x, se = FALSE, color = "#F8766D") +           
  stat_regline_equation(aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
                        formula = y ~ x, 
                        parse = TRUE,
                        geom = "text",
                        color = "#F8766D") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "#00C19A")
  


kr_test<- PAM_Data%>% ggplot(aes(kR,kNPQr))+
  geom_point()+
  theme_classic()+
  labs(x = "Simulated kR",y="Measured kR")+
  geom_smooth(method = "lm", formula= y~x, se = FALSE, color = "#F8766D") +           
  stat_regline_equation(aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
                        formula = y ~ x, 
                        parse = TRUE,
                        geom = "text",
                        color = "#F8766D") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "#00C19A")


kn_test<- PAM_Data%>% ggplot(aes(kN,kNPQt))+
  geom_point()+
  theme_classic()+
  labs(x = "Simulated kN",y="Measured kN")+
  geom_smooth(method = "lm", formula= y~x, se = FALSE, color = "#F8766D") +           
  stat_regline_equation(aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
                        formula = y ~ x, 
                        parse = TRUE,
                        geom = "text",
                        color = "#F8766D") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "#00C19A")


YP_test<- PAM_Data%>% ggplot(aes(phi_P,YP))+
  geom_point()+
  theme_classic()+
  labs(x = "Simulated ΦP",y="Measured ΦP")+
  geom_smooth(method = "lm", formula= y~x, se = FALSE, color = "#F8766D") +           
  stat_regline_equation(aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
                        formula = y ~ x, 
                        parse = TRUE,
                        geom = "text",
                        color = "#F8766D") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "#00C19A")


plot_grid(ks_test,kr_test,kn_test,YP_test, ncol = 2, nrow = 2)


# simulated vs modelled time plots ----------------------------------------

ks_time<- PAM_Data%>% ggplot(aes(DateTime,kNPQs))+
  geom_line(aes(colour="Measured"))+
  geom_line(aes(x=DateTime, y=kS, colour="Simulated"))+
  theme_classic()+
  theme(legend.position=c(0.2, 0.8), legend.title = element_blank())+
  labs(x = "",y="kS")

ks_smooth<- PAM_Data%>% ggplot(aes(DateTime,kNPQs))+
  geom_smooth(aes(colour="Measured"))+
  geom_smooth(aes(x=DateTime, y=kS, colour="Simulated"))+
  theme_classic()+
  theme(legend.position=c(0.2, 0.8), legend.title = element_blank())+
  labs(x = "",y="kS")

kr_time<- PAM_Data%>% ggplot(aes(DateTime,kNPQr))+
  geom_point(aes(colour="Measured kR"))+
  geom_point(aes(x=DateTime, y=kR, colour="Simulated kR"))+
  theme_classic()+
  theme(legend.position= "none")+
  labs(x = "",y="kR")

kr_smooth<-PAM_Data%>% ggplot(aes(DateTime,kNPQr))+
       geom_smooth(aes(colour="Measured kR"))+
       geom_smooth(aes(x=DateTime, y=kR, colour="Simulated kR"))+
       theme_classic()+
       theme(legend.position= "none")+
       labs(x = "",y="kR")

kn_time<- PAM_Data%>% ggplot(aes(DateTime,kNPQt))+
  geom_point(aes(colour="Measured kN"))+
  geom_point(aes(x=DateTime, y=kN, colour="Simulated kN"))+
  theme_classic()+
  theme(legend.position= "none")+
  labs(x = "",y="kN")

kn_smooth<- PAM_Data%>% ggplot(aes(DateTime,kNPQt))+
     geom_smooth(aes(colour="Measured kN"))+
     geom_smooth(aes(x=DateTime, y=kN, colour="Simulated kN"))+
     theme_classic()+
     theme(legend.position= "none")+
     labs(x = "",y="kN")


YP_time<- PAM_Data%>% ggplot(aes(DateTime,YP))+
  geom_point(aes(colour="Measured ΦP"))+
  geom_point(aes(x=DateTime, y=phi_P, colour="Simulated ΦP"))+
  theme_classic()+
  theme(legend.position= "none")+
  labs(x = "",y="ΦP")

YP_smooth<- PAM_Data%>% ggplot(aes(DateTime,YP))+
  geom_smooth(aes(colour="Measured ΦP"))+
  geom_smooth(aes(x=DateTime, y=phi_P, colour="Simulated ΦP"))+
  theme_classic()+
  theme(legend.position= "none")+
  labs(x = "",y="ΦP")

plot_grid(ks_smooth,kr_smooth,kn_smooth, YP_smooth, ncol = 2, nrow = 2)



# seasonal diff, year average graph ---------------------------------------

Fleaf_time<- PAM_Data_test%>% ggplot(aes(DOY,F_Leaf))+
  stat_summary(geom = "point", fun = "mean")+
  geom_smooth(colour="#00C19A")+
  theme_classic()+
  labs(x = "Day of Year",y="Leaf Fluorescence (μmol·m−2·s−1)")+
  theme(legend.position= "none")
Fleaf_time


# Diurnal graphs ----------------------------------------------------------


winter_sample<-subset(PAM_Data_test, date_only>=as.Date("2009-02-18")&date_only<=as.Date("2009-02-24"))

Fleaf_diurnalW<- winter_sample%>% ggplot(aes(DateTime,F_Leaf))+
  geom_point(size = 0.5)+
  theme_classic()+
  scale_y_continuous(limits = c(0, 35))+
  labs(x = "",y="Leaf Fluorescence (μmol·m−2·s−1)")
Fleaf_diurnalW

summer_sample<-subset(PAM_Data_test, date_only>=as.Date("2009-07-18")&date_only<=as.Date("2009-07-24"))

Fleaf_diurnalS<- summer_sample%>% ggplot(aes(DateTime,F_Leaf))+
  geom_point(size = 0.5)+
  theme_classic()+
  scale_y_continuous(limits = c(0, 35))+
  labs(x = "",y="Leaf Fluorescence (μmol·m−2·s−1)")
Fleaf_diurnalS

plot_grid(Fleaf_diurnalS, Fleaf_diurnalW, ncol = 2)



