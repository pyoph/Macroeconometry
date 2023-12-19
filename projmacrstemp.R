library(ggplot2)
library(gridExtra)
install.packages("usethis")

usethis::use_git_config(user.name="pyoph", user.email="paul.guillot2@gmail.com")

####Ouverture des fichiers de données####

#Ouverture du fichier des réserves

fichierTL <- read.csv("TLAACBW027SBOG.csv")
fichierTL$TLAACBW027SBOG

#Ouverture du fichier federal fund rates. 

fichierFFR <- read.csv("EFFR.csv")


###Représentation des données###

#Représentation des réserves

plot1 <- ggplot(fichierTL, aes(x = as.Date(DATE, format = "%Y-%m-%d"))) + 
  geom_line(aes(y = as.numeric(TLAACBW027SBOG), color = "reserves",group = 1))  +
  ylab("Reserves billions $")+
  xlab("Year") + ggtitle("Reserves billions")+
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")

fichierFFR <- na.omit(fichierFFR)


plot2 <- ggplot(fichierFFR, aes(x = as.Date(DATE, format = "%Y-%m-%d"))) + 
  geom_line(aes(y = as.numeric(EFFR), color = "federal fund rates",group = 1))  +
  ylab("Federal fund rates")+
  xlab("Year") + ggtitle("Federal fund rates")+
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")

plot2

# affichage des graphiques côte à côte
grid.arrange(plot1, plot2, ncol = 2)

