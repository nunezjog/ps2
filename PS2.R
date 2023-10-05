#Johanna Nunez
#SOC 756

rm(list = ls()

#install.packages("HMDHFDplus")
library(HMDHFDplus)

#Female 2004
lt=readHMDweb("USA", "fltper_5x1", username = "jgnunez@wisc.edu", password = "pswd")
flt2004<-subset(lt,Year==2004)

#Male 2004
lt2=readHMDweb("USA", "mltper_5x1", username = "jgnunez@wisc.edu", password = "pswd")
mlt2004<-subset(lt2,Year==2004)

#Poverty Data
data <- read.csv("/Users/jo/Desktop/Fall 2023/DemTechII/Problem Set 2/ps2data_2023.csv", header = TRUE)

mdata <- data [c(1:24),]
mlt2004$pov <- mdata$proportion_poverty_1.n_x
mlt2004$Npov <- mdata$number_sampled_N

fdata <- data [c(25:48),]
flt2004$pov <- fdata$proportion_poverty_1.n_x
flt2004$Npov <- fdata$number_sampled_N

#Proportion Not in Poverty
flt2004$notpov = 1-flt2004$pov
mlt2004$notpov = 1-mlt2004$pov

#Lx for Poverty
flt2004$Lxpov = flt2004$Lx*flt2004$pov
mlt2004$Lxpov = mlt2004$Lx*mlt2004$pov

#Tx for Poverty
flt2004$Txpov[1] <- sum(flt2004$Lxpov)
for(i in 2:nrow(flt2004)) {
  flt2004$Txpov[i] <- flt2004$Txpov[i-1]-flt2004$Lxpov[i-1]
}

mlt2004$Txpov[1] <- sum(mlt2004$Lxpov)
for(i in 2:nrow(mlt2004)) {
  mlt2004$Txpov[i] <- mlt2004$Txpov[i-1]-mlt2004$Lxpov[i-1]
}

#e - Life expectancy (years living in poverty)
flt2004$expov <- flt2004$Txpov/flt2004$lx
mlt2004$expov <- mlt2004$Txpov/mlt2004$lx

#Variance in Prevalence
flt2004$var.prev = ((flt2004$pov*flt2004$notpov)/(fdata$number_sampled_N))
mlt2004$var.prev = ((mlt2004$pov*mlt2004$notpov)/(mdata$number_sampled_N))

#Variance/Standard error
flt2004$var = ((flt2004$Lx^2)*flt2004$var.prev)
mlt2004$var = ((mlt2004$Lx^2)*mlt2004$var.prev)

flt2004$vari[1] <- sum(flt2004$var)
for(i in 2:nrow(flt2004)) {
  flt2004$vari[i] <- flt2004$vari[i-1]-flt2004$var[i-1]
}

mlt2004$vari[1] <- sum(mlt2004$var)
for(i in 2:nrow(mlt2004)) {
  mlt2004$vari[i] <- mlt2004$vari[i-1]-mlt2004$var[i-1]
}

flt2004$variance = flt2004$vari/(flt2004$lx^2)
mlt2004$variance = mlt2004$vari/(mlt2004$lx^2)
flt2004$se = sqrt(flt2004$variance)
mlt2004$se = sqrt(mlt2004$variance)


#Z scores!
flt2004$diffex = abs(flt2004$expov-mlt2004$expov)
flt2004$diffse = flt2004$se+mlt2004$se
flt2004$zscore = flt2004$diffex/flt2004$diffse


#Significance
flt2004$sig95 = flt2004$zscore >= 1.96 
flt2004$sig99 = flt2004$zscore >= 2.58
flt2004$sig999 = flt2004$zscore >= 3.29

library(tidyverse)
results = flt2004 %>%
  select(Age, diffex, diffse, zscore, sig95, sig99, sig999, by="Age")

#install.packages("writexl")
library(writexl)
write_xlsx(results, "PS2table.xlsx")
