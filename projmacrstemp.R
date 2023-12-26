#install.packages("usethis")
#install.packages("brms")
#install.packages("MCMCpack")
library(ggplot2)
library(gridExtra)
library(zoo)
library(brms)
library(MASS)
library(MCMCpack)
library(vars)
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

sigma1 = 0.5
sigma2 = 0.8
alpha21 = 0.9

l1 = 0.04
l2 = 0.2
l3 = 0.01

sigma_t <- diag(c(sigma1, sigma2))

A_t <- matrix(c(1, 0, alpha21, 1), ncol = 2)

##Setting of B^, alpha^, log(sigma^), PsiB^ and Psialpha^ by OLS###

#Regression on datas from 01/07/2009 to 19/01/2010
mergedDataps <- na.omit(mergedDataps)


modelOLSps <- lm(as.double(Spread) ~ as.double(reserves), data = mergedDataps)
summary(modelOLSps)

#Recuperation of B^

B0 <- coef(modelOLSps)[2]
B0

sigma0 <- vcov(modelOLSps)

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




