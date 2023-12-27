#install.packages("usethis")
#install.packages("brms")
#install.packages("MCMCpack")
#install.packages("fastmatrix")
library(ggplot2)
library(gridExtra)
library(zoo)
library(brms)
library(MASS)
library(MCMCpack)
library(vars)
library(fastmatrix)
#usethis::use_git_config(user.name="pyoph", user.email="paul.guillot2@gmail.com")

#Simulation of normal prior
Priornormal <- function(n_samples, mu, Sigma) {

  return(mvrnorm(n = n_samples, mu = mu, Sigma = Sigma))
}

#Simulation of inverse Wishart prior
invWishart <- function(A,df){
  return (riwish(df,A)) 
}


####Opening of data files####


fichierTL <- read.csv("TLAACBW027SBOG.csv")

fichierFFR <- read.csv("EFFR.csv")

fileReserves <- read.csv("WRESBAL.csv")

#Removing of NA's
fichierFFR <- na.omit(fichierFFR)
fichierFFR <- fichierFFR[fichierFFR$Spread != "NA ",]

fichierTL$DATE <- as.Date(fichierTL$DATE, format = "%Y-%m-%d")
fichierTL$DATE
names(fichierFFR)[names(fichierFFR) == "DATE"] <- "Date"

fichierFFR$Date <- as.Date(fichierFFR$Date)

dates_journalieres <- seq(as.Date("2009-01-07"), as.Date("2021-12-29"), by = "day")
dates_journalieres
fichierTL$TLAACBW027SBOG
#Interpolation in order to have daily data
TLAACts <- approx(fichierTL$DATE, fichierTL$TLAACBW027SBOG, xout = dates_journalieres)$y
fichierTL$TLAACBW027SBOG
TLAACts
TLAAj <- data.frame(Date = dates_journalieres, reserves = TLAACts)
TLAAj
fichierFFR$Spread
mergedData <- merge(TLAAj,fichierFFR,by = "Date")

###Representation of datas###

#Representation of reserves

plot1 <- ggplot(fichierTL, aes(x = as.Date(DATE, format = "%Y-%m-%d"))) + 
  geom_line(aes(y = as.numeric(TLAACBW027SBOG), color = "reserves",group = 1))  +
  ylab("Reserves billions $")+
  xlab("Year") + ggtitle("Reserves billions")+
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")


#Representation of the federal fund rates
plot2 <- ggplot(fichierFFR, aes(x = as.Date(Date, format = "%Y-%m-%d"))) + 
  geom_line(aes(y = Spread, color = "spread federal fund rates - IORB",group = 1))  +
  ylab("spread federal fund rates - IORB")+
  xlab("Year") + ggtitle("spread federal fund rates - IORB")+
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")

plot2
fichierFFR$Spread
# affichage des graphiques côte à côte
grid.arrange(plot1, plot2, ncol = 2)



###Variables###

#Data vector
y <- cbind(mergedData$reserves,mergedData$Spread)
colnames(y) <- c("reserves","spread")

##presample
mergedDataps <- mergedData[mergedData$Date <= "2010-01-19",]
yps <- cbind(as.numeric(mergedDataps$reserves),as.numeric(mergedDataps$Spread))
colnames(yps) <- c("reserves","spread")
typeof(mergedDataps$Spread)
as.double(mergedDataps$Spread)
mergedDataps$Spread
#Parameters
m = 10 #number of lags


l1 = 0.04
l2 = 0.2
l3 = 0.01




##Setting of B^, alpha^, log(sigma^), PsiB^ and Psialpha^ by OLS###

#Regression on datas from 01/07/2009 to 19/01/2010
mergedDataps <- na.omit(mergedDataps)


modelOLSps <- lm(as.double(Spread) ~ as.double(reserves), data = mergedDataps)
summary(modelOLSps)

#Recuperation of B^

Bhat <- coef(modelOLSps)[2]
Bhat

Omega0 <- vcov(modelOLSps)
Omega0


#Extraction of PsiA and PsiB
PsiA <- Omega0[1,1]
PsiB <- Omega0[2,2]

#LDL décompostion of Sigma0*t(Sigma0)

A0 <- ldl(Omega0*t(Sigma0))[2]

#Extraction of sigma10 and sigma20
str <- as.character(A0[1])
sigma10 <- as.double(substr(str[1],3,8))
sigma10
sigma20 <- as.double(substr(str[1],23,32))
sigma20 <- sigma20 * 10^(-23)
sigma20

#Extration of a10
Sigma0 <- ldl(Omega0*t(Sigma0))[1] 
str <- as.character(Sigma0[1])
a10 <- as.double(substr(str,6,12))*10^(-9)
a10

#Settings of priors
set.seed(13579)                        # Set seed

B0 <- rnorm(1,Bhat,PsiB)
B0
