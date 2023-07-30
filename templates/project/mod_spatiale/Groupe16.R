
#link to download package by default
options(repos = list(CRAN="http://cran.rstudio.com/"))


#Packages
install.packages("raster")
install.packages("broom")
install.packages("RColorBrewer")
install.packages("rgeos")
install.packages("sf")
install.packages("ggplot2")
install.packages("data.table")
install.packages("pander")
#install.packages("knitr")
install.packages("htmltools")
install.packages("gstat")
install.packages("car")
install.packages("rgdal")
install.packages("rgl")
install.packages("latticeExtra")
install.packages("gridExtra")
install.packages("corrplot")
install.packages("performanceAnalytics")

# Importation et lecture du .csv

library(data.table)
library(ggplot2)
library(gridExtra)
library(PerformanceAnalytics)
library(car)
library(rgl)
library(gstat)
library(latticeExtra)
library(AID)
library(rgdal)
library(raster)
library(broom)
library(RColorBrewer)
library(rgeos)
library(dplyr)
library(sf)
library(pander)

mydata <- fread('groupe16.csv')
summary(mydata)
head(mydata)

#-========================== SHAPEFILE IMPORTATION  ===========================

Prov<-readOGR(dsn="ExtraData/BEL_adm",layer="BEL_adm2")
#On change le système de coordonnées en un système de projection L72
Prov72<-spTransform(Prov,CRS=CRS("+init=epsg:31370")) #CRS code
#transformation du polygone en dataframe
Provdf<-tidy(Prov72)
#fusion du dataframe avec la table d'attribut
Prov72$id<-rownames(Prov72@data)
#On appelle le Provdf
Provdf<-left_join(Provdf,Prov72@data,by="id")
#transformation de la dataframe en datatable
Provdt<-as.data.table(Provdf)
#séléction des lignes correspondant à la province de Liège
Liegedt<-Provdt[7165:7892,]

shp_liege <- st_read(
  "liege.shp")

# Analyse exploratoire des données
## Carte de la position des points de mesure des ETM + limite de la province de Liege

#-==============================================================================

#-========================== MAPING ETMS ===========================
 
# Zinc
Zn <- ggplot(data = shp_liege) + 
  geom_sf() + coord_sf(datum = st_crs(31370)) +
  geom_point(data = mydata, aes(x=X, y=Y, color=Zn), size=0.6) + 
  xlab("Longitude") +
  ylab("Latitude") + 
  ggtitle("concentrations du zinc de la province de Liège") +
  theme(plot.title=element_text(hjust=0.5)) + # center title
  scale_color_gradientn(name="Zinc [mg/kg]", colors=c('royalblue','green3', 'yellow', 'red')) + theme_minimal()
Zn

# Plomb
Pb <- ggplot(data = shp_liege) + 
  geom_sf() + coord_sf(datum = st_crs(31370)) +
  geom_point(data = mydata, aes(x=X, y=Y, color=Pb), size=0.6) + 
  xlab("Longitude") +
  ylab("Latitude") +  
  ggtitle("concentrations de Plomb de la province de Liège") +
  theme(plot.title=element_text(hjust=0.5)) + # center title
  scale_color_gradientn(name="Plomb [mg/kg]", colors=c('royalblue','green3', 'yellow', 'red')) + theme_minimal()
Pb

### Cuivre
Cu <- ggplot(data = shp_liege) + 
  geom_sf() + coord_sf(datum = st_crs(31370)) +
  geom_point(data = mydata, aes(x=X, y=Y, color=Cu), size=0.6) + 
  xlab("Easting") +
  ylab("Northing") +  
  ggtitle("concentrations de Cuivre de la province de Liège") +
  theme(plot.title=element_text(hjust=0.5)) + # center title
  scale_color_gradientn(name="concentrations Cuivre [mg/kg]", colors=c('royalblue','green3', 'yellow', 'red')) + theme_minimal()
Cu

#-============================================================================== 

#-========================== BASIC STATISTICS ===========================
# Pb sample
attach(mydata)
Pb <- na.omit(subset(mydata,select=c("X","Y","Pb")))
ggplot() + 
  geom_point(data=grid, aes(x=X,y=Y), color="grey50", shape=3) +
  geom_point(data=Pb, aes(x=X, y=Y, color=Pb), size=2) +
  xlab("Longitude") +
  ylab("Latitude") +
  ggtitle("Sample Pb")


#-----------------------------------------------------

# Zn sample
Zn <- na.omit(subset(mydata,select=c("X","Y","Zn")))
ggplot() + 
  geom_point(data=grid, aes(x=X,y=Y), color="grey50", size=1, shape=3) +
  geom_point(data=Zn, aes(x=X, y=Y, color=Zn), size=2) +
  xlab("Longitude") +
  ylab("Latitude") +
  ggtitle("Sample Zn")
#-----------------------------------------------------

# cu sample
Cu <- na.omit(subset(mydata,select=c("X","Y","Cu")))
ggplot() + 
  geom_point(data=grid, aes(x=X,y=Y,fill="Grid points"), color="grey50", size=1, shape=3) +
  geom_point(data=Cu, aes(x=X, y=Y, color=Cu), size=2) +
  xlab("Longitude") +
  ylab("Latitude") +
  ggtitle("Sample Cu")
#-----------------------------------------------------

#statistique de base
nbre <- cbind("Pb"= nrow(mydata),
              "Zn" = nrow(mydata),
              "Cu"= nrow(mydata))
mean <- cbind("Pb"= mean(Pb$Pb),
              "Zn" = mean(Zn$Zn),
              "Cu"=mean(Cu$Cu))
sd <- cbind("Pb"= sd(Pb$Pb),
            "Zn" = sd(Zn$Zn),
            "Cu"=sd(Cu$Cu))
med <- cbind("Pb"= median(Pb$Pb),
             "Zn" = median(Zn$Zn),
             "Cu"=median(Cu$Cu))
min <- cbind("Pb"= min(Pb$Pb),
             "Zn" = min(Zn$Zn),
             "Cu"=min(Cu$Cu))
max <- cbind("Pb"= max(Pb$Pb),
             "Zn" = max(Zn$Zn),
             "Cu"= max(Cu$Cu))
amp <- cbind("Pb"= max[1,'Pb']-min[1,'Pb'],
             "Zn"= max[1,'Zn']-min[1,'Zn'],
             "Cu"= max[1,'Cu']-min[1,'Cu'])
na <- cbind("Pb"=nrow(mydata)-nrow(Pb),
            "Zn"=nrow(mydata)-nrow(Zn),
            "Cu"==nrow(mydata)-nrow(Cu))
cor <- cbind("Pb"= nrow(Pb),
             "Zn" = nrow(Zn),
             "Cu"= nrow(Cu))
resume <- rbind(nbre,min,max,mean,sd,med,amp,na,cor)
row.names(resume) <- c("Nombre de données","Min","Max","Moyenne",
                       "Ecart-type","Mediane","Amplitude","#Na","Nombre de données corrigé")
pander(round(resume,3))

#-----------------------------------------------------

# AFFICHAGE
par(mfrow=c(2,4))
barplot(nbre, main = "Nombre de données")
barplot(min, main="Minimas")
barplot(max,main="Maximas")
barplot(mean, main = "Moyennes")
barplot(sd, main="Ecarts-Types")
barplot(med, main="Mediane")
barplot(na,main="Nombre de Na")
barplot(cor,main = "nombre de données corrigé")
par(mfrow=c(1,1))

#-----------------------------------------------------

# correlation entre les ETM

corr.etm <- pander(cor(na.omit(mydata)[,.(Pb,Zn,Cu)]))

#-----------------------------------------------------

## Corrélation
chart.Correlation(mydata[,.(Zn,Pb,Cu)]) 


#-============================================================================== 

#-========================== DATA NORMALIZATION ===========================

### Zinc
normalZn<-ggplot(mydata, aes(x = Zn)) + # if you put 'aes' here, all geom_ functions will use rain as x
  geom_histogram(aes(y = ..density..,  # display density function (and not count)
                     fill = "Density histogram" ),
                 bins = 15, # number of bins
                 color = "white") + 
  geom_density(col = "lightsteelblue3", 
               aes(fill = "Fitted pdf"), 
               alpha = 0.2) +  
  xlab("Zinc[mg/kg]") + 
  ylab("f(x)") + 
  ggtitle("Density histogram of the Zinc concentration") +
  stat_function(fun=dnorm, # display a normal distribution
                args=list(mean = mean(mydata$Zn , na.rm=TRUE), sd = sd(mydata$Zn , na.rm=TRUE)), # with mean and sd of rain
                aes(color="Normal pdf"),
                size= 1) + # width of the line
  scale_fill_manual("", values = c("Density histogram"="lightsteelblue2",
                                   "Fitted pdf"=alpha("lightsteelblue3",.2))) +
  scale_color_manual("", values="red") + theme_minimal()
normalZn

qqplotZn<-qqPlot(mydata$Zn)
qqplotZn

### Plomb
normalPb<-ggplot(mydata, aes(x = Pb)) + # if you put 'aes' here, all geom_ functions will use rain as x
  geom_histogram(aes(y = ..density..,  # display density function (and not count)
                     fill = "Density histogram" ),
                 bins = 15, # number of bins
                 color = "white") + 
  geom_density(col = "lightsteelblue3", 
               aes(fill = "Fitted pdf"), 
               alpha = 0.2) +  
  xlab("Plomb [mg/kg]") + 
  ylab("f(x)") + 
  ggtitle("Density histogram of the Plomb concentration") +
  stat_function(fun=dnorm, # display a normal distribution
                args=list(mean = mean(mydata$Pb , na.rm=TRUE), sd = sd(mydata$Pb , na.rm=TRUE)), # with mean and sd of rain
                aes(color="Normal pdf"),
                size= 1) + # width of the line
  scale_fill_manual("", values = c("Density histogram"="lightsteelblue2",
                                   "Fitted pdf"=alpha("lightsteelblue3",.2))) +
  scale_color_manual("", values="red") + theme_minimal()

normalPb

qqplotPb<-qqPlot(mydata$Pb)
qqplotPb

### Cuivre
normalCu<-ggplot(mydata, aes(x = Cu)) + # if you put 'aes' here, all geom_ functions will use rain as x
  geom_histogram(aes(y = ..density..,  # display density function (and not count)
                     fill = "Density histogram" ),
                 bins = 15, # number of bins
                 color = "white") + 
  geom_density(col = "lightsteelblue3", 
               aes(fill = "Fitted pdf"), 
               alpha = 0.2) +  
  xlab("Cuivre[mg/kg]") + 
  ylab("f(x)") + 
  ggtitle("Density histogram of the Cuivre concentration") +
  stat_function(fun=dnorm, # display a normal distribution
                args=list(mean = mean(mydata$Cu , na.rm=TRUE), sd = sd(mydata$Cu , na.rm=TRUE)), # with mean and sd of rain
                aes(color="Normal pdf"),
                size= 1) + # width of the line
  scale_fill_manual("", values = c("Density histogram"="lightsteelblue2",
                                   "Fitted pdf"=alpha("lightsteelblue3",.2))) +
  scale_color_manual("", values="red") + theme_minimal()
normalCu

qqplotCu<-qqPlot(mydata$Cu)
qqplotCu

# ----------------------------------------------------------------------------

# Correction de la distribution des concentrations, diagnostic des éventuels outliers et Analyse et modélisation de la dépendance spatiale


mydata <- mydata[ , list(Zn = mean(Zn , na.rm=TRUE) , Pb = mean(Pb , na.rm=TRUE) , Cu = mean(Cu , na.rm=TRUE) ) , by=c( "X,Y" )]



## Transformation normale des données via Boxcox

Znbx <- boxcoxnc(na.omit(mydata$Zn), verbose = FALSE)
mydata[!is.na(Zn) , Zn_tf := Znbx$tf.data ] # Create column with transformed data
thetaZn <- Znbx$lambda.hat

normalZn_tf<-ggplot(mydata, aes(x = Zn_tf)) + # if you put 'aes' here, all geom_ functions will use rain as x
  geom_histogram(aes(y = ..density..,  # display density function (and not count)
                     fill = "Density histogram" ),
                 bins = 15, # number of bins
                 color = "white") + 
  geom_density(col = "lightsteelblue3", 
               aes(fill = "Fitted pdf"), 
               alpha = 0.2) +  
  xlab("Cuivre[mg/kg]") + 
  ylab("f(x)") + 
  ggtitle("Density histogram of the transformed Cuivre concentration") +
  stat_function(fun=dnorm, # display a normal distribution
                args=list(mean = mean(mydata$Zn_tf , na.rm=TRUE), sd = sd(mydata$Zn_tf , na.rm=TRUE)), # with mean and sd of rain
                aes(color="Normal pdf"),
                size= 1) + # width of the line
  scale_fill_manual("", values = c("Density histogram"="lightsteelblue2",
                                   "Fitted pdf"=alpha("lightsteelblue3",.2))) +
  scale_color_manual("", values="red") + theme_minimal()
normalZn_tf

# On observe la tendance du Zn : on suppose que la distribution de base est non-stationnaire d'ordre 1, 



Zn.lm <- lm(Zn_tf ~X+Y, data = mydata)
test <- scatter3d(x = mydata$X, z = mydata$Y, y = mydata$Zn_tf, 
                  xlab = 'Northing', zlab = 'Easting', ylab = 'Zn')
rglwidget(width = 1000, height = 1000)

### On calcule le vecteur de résidus à partir des données de Zn transformées à laquelle on enlève la tendance
Zn.derive <- predict(Zn.lm, mydata)
mydata[, Zn.res := Zn_tf - Zn.derive] 
scatter3d(x = mydata$X, z = mydata$Y, y = mydata$Zn.res, 
          xlab = 'Northing', zlab = 'Easting', ylab = 'Zn')
rglwidget(width = 1000, height = 1000) #On obtient le graphe 3D sans la tendance en l'appelant avec rglwidget

### On enlève les outliers
# On utilise la méthode basée sur les percentiles
pinf = 0.025 # fixe le percentile de limite inférieure 
psup = 0.975 # fixe le percentile de limite inférieure 
dat <- mydata[,list(X,Y,Zn.res)]
binf <- quantile(na.omit(dat)$Zn.res, pinf) # calcule la borne inf de l'intervalle
bsup <- quantile(na.omit(dat)$Zn.res,psup) # calcule la borne sup de l'intervalle bsup
# On peut obtenir les indices des lignes des outliers comme ceci:
outlier_idx <- which(na.omit(dat)$Zn.res < binf | na.omit(dat)$Zn.res > bsup) 
outlier_idx 
#Puis leur valeur :
outlier_val <- na.omit(dat)[outlier_idx,"Zn.res"] 
outlier_val
# On enlève tous les outliers détectés (90)
Znclean.mydata <- dat[-outlier_idx,]
Znclean.mydata <- na.omit(Znclean.mydata)

### On estime le variogramme et on fait sa modélisation sur base des résidus
Zn.res.gstat <- gstat(formula = Zn.res~1, data = Znclean.mydata, locations = ~X+Y)
Zn.res.vario <- variogram(Zn.res.gstat, cutoff = 40000, width = 3000)
plot(Zn.res.vario, main = "Variogram of Zn residuals", pch=16,col='black')
head(Zn.res.vario)

trellis.focus("panel",1,1)
llines(x=c(0,max(Zn.res.vario$dist)), y=c(var(Znclean.mydata$Zn.res),var(Znclean.mydata$Zn.res)), col="red", lwd=1, lty=2)
trellis.unfocus()

var(Znclean.mydata$Zn.res)

Znclean.mydata.vg.sph <- vgm(model='Sph', nugget = 0.002)
Znclean.mydata.fit.sph <- fit.variogram(Zn.res.vario, model = Znclean.mydata.vg.sph, fit.method = 6)

plot(Zn.res.vario, model = Znclean.mydata.fit.sph, main = "Variogram of Zn residuals", pch=16,col='black')

trellis.focus("panel",1,1)
llines(x=c(0,max(Zn.res.vario$dist)), y=c(var(Znclean.mydata$Zn.res),var(Znclean.mydata$Zn.res)), col="red", lwd=1, lty=2)


## Plomb

## Transformation notmale des données via Boxcox

Pbbx <- boxcoxnc(na.omit(mydata$Pb), verbose = FALSE)
mydata[!is.na(Pb) , Pb_tf := Pbbx$tf.data ] # Create column with transformed data
thetaPb <- Pbbx$lambda.hat

normalPb_tf<-ggplot(mydata, aes(x = Pb_tf)) + # if you put 'aes' here, all geom_ functions will use rain as x
  geom_histogram(aes(y = ..density..,  # display density function (and not count)
                     fill = "Density histogram" ),
                 bins = 15, # number of bins
                 color = "white") + 
  geom_density(col = "lightsteelblue3", 
               aes(fill = "Fitted pdf"), 
               alpha = 0.2) +  
  xlab("Plomb[mg/kg]") + 
  ylab("f(x)") + 
  ggtitle("Density histogram of the transformed Plomb concentration") +
  stat_function(fun=dnorm, # display a normal distribution
                args=list(mean = mean(mydata$Pb_tf , na.rm=TRUE), sd = sd(mydata$Pb_tf , na.rm=TRUE)), # with mean and sd of rain
                aes(color="Normal pdf"),
                size= 1) + # width of the line
  scale_fill_manual("", values = c("Density histogram"="lightsteelblue2",
                                   "Fitted pdf"=alpha("lightsteelblue3",.2))) +
  scale_color_manual("", values="red") + theme_minimal()
normalPb_tf

### On observe la tendance du Pb : on suppose que la distribution de base est non-stationnaire d'ordre 1 
Pb.lm <- lm(Pb_tf ~X+Y, data = mydata)
test <- scatter3d(x = mydata$X, z = mydata$Y, y = mydata$Pb_tf, 
                  xlab = 'Northing', zlab = 'Easting', ylab = 'Pb')
rglwidget(width = 1000, height = 1000)

# On calcule le vecteur de résidus et on enlève la tendance du Pb
Pb.derive <- predict(Pb.lm, mydata)
mydata[, Pb.res := Pb_tf - Pb.derive]
scatter3d(x = mydata$X, z = mydata$Y, y = mydata$Pb.res, 
          xlab = 'Northing', zlab = 'Easting', ylab = 'Pb')
rglwidget(width = 1000, height = 1000) 

# On enlève les outliers
# On utilise la méthode basée sur les percentiles
pinf = 0.025 # fixe le percentile de limite inférieure 
psup = 0.975 # fixe le percentile de limite inférieure 
dat <- mydata[,list(X,Y,Pb.res)]
binf <- quantile(na.omit(dat)$Pb.res, pinf) # calcule la borne inf de l'intervalle
bsup <- quantile(na.omit(dat)$Pb.res,psup) # calcule la borne sup de l'intervalle bsup


outlier_idx <- which(na.omit(dat)$Pb.res < binf | na.omit(dat)$Pb.res > bsup) 
outlier_idx 
#Puis leur valeur :
outlier_val <- na.omit(dat)[outlier_idx,"Pb.res"] 
outlier_val
# On enlève tous les outliers détectés 
Pbclean.mydata <- dat[-outlier_idx,]
Pbclean.mydata <- na.omit(Pbclean.mydata)
#On a créé nouvelle table de données Pbclean.mydata qui a supprimé les LIGNES qui correspondent à des outliers de Pb

### On refait un variogramme (corrigé)
Pb.res.gstat <- gstat(formula = Pb.res~1, data = Pbclean.mydata, locations = ~X+Y)
Pb.res.vario <- variogram(Pb.res.gstat, cutoff = 35000, width = 2000)
plot(Pb.res.vario, main = "Variogram of Pb residuals", pch=16,col='black')
head(Pb.res.vario)

trellis.focus("panel",1,1)
llines(x=c(0,max(Pb.res.vario$dist)), y=c(var(Pbclean.mydata$Pb.res),var(Pbclean.mydata$Pb.res)), col="red", lwd=1, lty=2)
trellis.unfocus()

var(Pbclean.mydata$Pb.res)

### On refait un variogramme (corrigé) + FIT
Pbclean.mydata.vg.sph <- vgm(psill=0.0012, model='Sph', range = 20000, nugget = 0.0003)
Pbclean.mydata.fit.sph <- fit.variogram(Pb.res.vario, model = Pbclean.mydata.vg.sph, fit.method = 6)

plot(Pb.res.vario, model = Pbclean.mydata.fit.sph, main = "Variogram of Pb residuals", pch=16,col='black')
trellis.focus("panel",1,1)
llines(x=c(0,max(Pb.res.vario$dist)), y=c(var(Pbclean.mydata$Pb.res),var(Pbclean.mydata$Pb.res)), col="red", lwd=1, lty=2)


#Cuivre

## Transformation normale des données via Boxcox

Cubx <- boxcoxnc(na.omit(mydata$Cu), verbose = FALSE)
mydata[!is.na(Cu) , Cu_tf := Cubx$tf.data ] # Create column with transformed data
thetaCu <- Cubx$lambda.hat

normalCu_tf<-ggplot(mydata, aes(x = Cu_tf)) + # if you put 'aes' here, all geom_ functions will use rain as x
  geom_histogram(aes(y = ..density..,  # display density function (and not count)
                     fill = "Density histogram" ),
                 bins = 15, # number of bins
                 color = "white") + 
  geom_density(col = "lightsteelblue3", 
               aes(fill = "Fitted pdf"), 
               alpha = 0.2) +  
  xlab("Cuivre[mg/kg]") + 
  ylab("f(x)") + 
  ggtitle("Density histogram of the transformed Cuivre concentration") +
  stat_function(fun=dnorm, # display a normal distribution
                args=list(mean = mean(mydata$Cu_tf , na.rm=TRUE), sd = sd(mydata$Cu_tf , na.rm=TRUE)), # with mean and sd of rain
                aes(color="Normal pdf"),
                size= 1) + # width of the line
  scale_fill_manual("", values = c("Density histogram"="lightsteelblue2",
                                   "Fitted pdf"=alpha("lightsteelblue3",.2))) +
  scale_color_manual("", values="red") + theme_minimal()
normalCu_tf

### On enlève la tendance du Cu : on suppose que la distribution de base est non-stationnaire d'ordre 1 
Cu.lm <- lm(Cu_tf ~X+Y, data = mydata)
test <- scatter3d(x = mydata$X, z = mydata$Y, y = mydata$Cu_tf, 
                  xlab = 'Northing', zlab = 'Easting', ylab = 'Cu')
rglwidget(width = 1000, height = 1000)

### On calcule le vecteur de résidus
Cu.derive <- predict(Cu.lm, mydata)
mydata[, Cu.res := Cu_tf - Cu.derive]
scatter3d(x = mydata$X, z = mydata$Y, y = mydata$Cu.res, 
          xlab = 'Northing', zlab = 'Easting', ylab = 'Cu')
rglwidget(width = 1000, height = 1000) #On obtient le graphe 3D sans la tendance en appelant avec rglwidget

### On enlève les outliers
# On utilise la méthode basée sur les percentiles
pinf = 0.025 # fixe le percentile de limite inférieure 
psup = 0.975 # fixe le percentile de limite inférieure 
dat <- mydata[,list(X,Y,Cu.res)]
binf <- quantile(na.omit(dat)$Cu.res, pinf) # calcule la borne inf de l'intervalle
bsup <- quantile(na.omit(dat)$Cu.res,psup) # calcule la borne sup de l'intervalle bsup
# On peut obtenir les indices des lignes des outliers comme ceci:
outlier_idx <- which(na.omit(dat)$Cu.res < binf | na.omit(dat)$Cu.res > bsup) 
outlier_idx 
#Puis leur valeur :
outlier_val <- na.omit(dat)[outlier_idx,"Cu.res"] 
outlier_val


# On enlève tous les outliers détectés (62)
Cuclean.mydata <- dat[-outlier_idx,]
Cuclean.mydata <- na.omit(Cuclean.mydata)
#On a créé nouvelle table de données Cuclean.mydata qui a supprimé les LIGNES qui correspondent à des outliers de Pb

### On fait un variogramme sur base des résidus (conditions obligatoire pour faire un variogramme)
Cu.res.gstat <- gstat(formula = Cu.res~1, data = Cuclean.mydata, locations = ~X+Y)
Cu.res.vario <- variogram(Cu.res.gstat, cutoff = 35000, width = 2000)
plot(Cu.res.vario, main = "Variogram of Cu residuals", pch=16,col='black')
head(Cu.res.vario)

trellis.focus("panel",1,1)
llines(x=c(0,max(Cu.res.vario$dist)), y=c(var(Cuclean.mydata$Cu.res),var(Cuclean.mydata$Cu.res)), col="red", lwd=1, lty=2)
trellis.unfocus()

var(Cuclean.mydata$Cu.res)

#-============================================================================== 

### On refait un variogramme (corrigé) + FIT

Cuclean.mydata.vg.sph <- vgm(psill=0.7, model='Sph', range = 30000, nugget = 0.1)
Cuclean.mydata.fit.sph <- fit.variogram(Cu.res.vario, model = Cuclean.mydata.vg.sph, fit.method = 6)

plot(Cu.res.vario, model = Cuclean.mydata.fit.sph, main = "Variogramme du cuivre residuel", pch=16,col='black')
trellis.focus("panel",1,1)
llines(x=c(0,max(Cu.res.vario$dist)), y=c(var(Cuclean.mydata$Cu.res),var(Cuclean.mydata$Cu.res)), col="red", lwd=1, lty=2)



# Prédiction des différents ETM
## Méthode Distance Inverse



mygridsize = 750
x <- seq(floor(min(mydata$X-5000)), # from minimum Northing
         ceiling(max(mydata$X+5000)), # to maximum Northing
         by=mygridsize)
y <- seq(floor(min(mydata$Y-5000)), # from minimum Easting
         ceiling(max(mydata$Y+5000)), # to maximum Easting
         by=mygridsize)
mydata.grid <- as.data.table(expand.grid(X=x, Y=y))

# Create a spatial version of the grid
mydataSpatial <- copy (mydata.grid)
coordinates(mydataSpatial) <- ~X+Y
proj4string(mydataSpatial) <- CRS( "+init=epsg:31370" )


myprovince <- readOGR("liege.shp" , 
                     use_iconv = TRUE, encoding = "UTF-8" )

myprovince <- spTransform(myprovince, CRS("+init=epsg:31370"))

# Get index o f po int s in the provinc e o f Li è ge
prov.id <- over(myprovince,
                 mydataSpatial ,
                 returnList = TRUE)



liege.grid = mydata.grid[prov.id[[1]],]

ggplot() +
  geom_sf(data = shp_liege) + coord_sf(datum = st_crs(31370)) +
  geom_point(data=mydata.grid, 
             aes(x=X,y=Y,fill="Grid points"),
             color="grey50",
             size=0.05,
             shape=2) +
  geom_point(data=Znclean.mydata,
             aes(x=X, y=Y, color=Znclean.mydata$Zn),
             size=2) +
  labs(fill="", color="Zn [mg/kg]") +
  scale_color_gradientn(name="Zn [mg/kg]", colors=c('royalblue','green3', 'yellow', 'red')) +
  xlab("Easting") +
  ylab("Northing") + 
  ggtitle("Zinc concentration in Liège and prediction grid") + theme_minimal()


#on fait une prédiction via la distance inverse sur base des résidus et oÃ¹ l'on rajoutera la tendance 
# puis on retrouvera les données de base sur base de la formule de boxcox en faisant la transformée inverse 
# (retour sur la distribution de base des données) : 
Zn.idw <- idw(formula = Zn.res~1,
              data = Znclean.mydata,  
              locations = ~X+Y,
              newdata = liege.grid,
              idp = 0.8,
              nmax = 20,
              maxdist=6500)

setnames(Zn.idw, "var1.pred", "Zn.pred")
# On rajoute la tendance et on fait une transformée inverse pour retrouver les valeurs de concentrations prédites
# On rajoute une colonne avec les résidus + la tendance dans le Zn.idw

Zn.idw$Zn.pred.tend <- Zn.idw$Zn.pred + predict(Zn.lm, Zn.idw) # ajout de la tendance via predict(Zn.lm se trouvant dans Zn.idw) pour calculer la tendance
Zn.idw$Zn.pred.corr <- exp(log(thetaZn*Zn.idw$Zn.pred.tend +1)/thetaZn) #transformation inverse

ggplot() +
  geom_tile(data=Zn.idw, 
            aes(x = X, y = Y, fill = Zn.pred.corr)) +
  geom_point(data=Znclean.mydata, 
             aes(x=X, y=Y, color="Measurments points"),
             shape=18,
             size=0.6) +
  scale_color_manual("", values="black") +
  scale_fill_gradientn(name="Zn predictions [mg/mÂ³]", colors=c('royalblue','green3', 'yellow', 'red')) +
  theme(legend.key = element_rect(fill = "green3", 
                                   color = NA)) +
  xlab("Easting") +
  ylab("Northing") + 
  ggtitle("Cuivre concentration in the province of Liege - Inverse Distance prediction")  + theme_minimal()


#Optimizing the power value (cross validation)
powers <- seq(0.4,1.4,0.2)
pmse <- data.table(power = powers, mse = rep(0,length(powers)) )# Power mean squared error
for (p in powers){
  pse <- rep(0,nrow(Znclean.mydata)) # Power squared errors
  for (i in 1:nrow(Znclean.mydata)){
    point.idw <- idw(formula = Zn.res~1,
                     data = Znclean.mydata[-i,],  
                     locations = ~X+Y,
                     newdata = Znclean.mydata[i,],
                     idp = p,
                     nmax = 20,
                     maxdist=6500,
                     debug.level = 0) # to avoid getting many output messages
    pse[i] <- (point.idw$var1.pred - Znclean.mydata$Zn.res[i])^2
  }
  pmse[power==p,"mse"] = mean(pse)
}

plot(pmse)


### Plomb

ggplot() + 
  geom_point(data=liege.grid, 
             aes(x=X,y=Y,fill="Grid points"),
             color="grey50",
             size=0.05,
             shape=3) +
  geom_point(data=Pbclean.mydata,
             aes(x=X, y=Y, color=Pbclean.mydata$Pb),
             size=2) +
  labs(fill="", color="Pb [mg/mÂ³]") +
  scale_color_gradientn(name="Pb [mg/mÂ³]", colors=c('royalblue','green3', 'yellow', 'red')) +
  xlab("Easting") +
  ylab("Northing") + 
  ggtitle("Plomb concentration in Liège and prediction grid")  + theme_minimal()

#on fait une prédiction via la distance inverse: 
Pb.idw <- idw(formula = Pb.res~1,
              data = Pbclean.mydata,  
              locations = ~X+Y,
              newdata = liege.grid,
              idp = 1,
              nmax = 20,
              maxdist=20000)

setnames(Pb.idw, "var1.pred", "Pb.pred")

# On rajoute la tendance et on fait une transformée inverse pour retrouver les valeurs de concentrations prédites
# On rajoute une colonne avec les résidus + la tendance dans le Cr.idw

Pb.idw$Pb.pred.tend <- Pb.idw$Pb.pred + predict(Pb.lm, Pb.idw) # ajout de la tendance via predict(Pb.lm se trouvant dans Pb.idw) pour calculer la tendance
Pb.idw$Pb.pred.corr <- exp(log(thetaPb*Pb.idw$Pb.pred.tend +1)/thetaPb) #transformation inverse

ggplot() + 
  geom_tile(data=Pb.idw, 
            aes(x = X, y = Y, fill = Pb.pred.corr)) +
  geom_point(data=Pbclean.mydata, 
             aes(x=X, y=Y, color="Measurments points"),
             shape=18,
             size=0.6) +
  scale_color_manual("", values="black") +
  scale_fill_gradientn(name="Pb predictions [mg/mÂ³]", colors=c('royalblue','green3', 'yellow', 'red')) +
  theme(legend.key = element_rect(fill = "green3", 
                                  color = NA)) +
  xlab("Easting") +
  ylab("Northing") + 
  ggtitle("Plomb concentration in the province of Liege - Inverse Distance prediction")  + theme_minimal()


#Optimizing the power value (cross validation)
powers <- seq(0.8,1.2,0.2)
pmse <- data.table(power = powers, mse = rep(0,length(powers)) )# Power mean squared error
for (p in powers){
  pse <- rep(0,nrow(Pbclean.mydata)) # Power squared errors
  for (i in 1:nrow(Pbclean.mydata)){
    point.idw <- idw(formula = Pb.res~1,
                     data = Pbclean.mydata[-i,],  
                     locations = ~X+Y,
                     newdata = Pbclean.mydata[i,],
                     idp = p,
                     nmax = 20,
                     maxdist=20000,
                     debug.level = 0) # to avoid getting many output messages
    pse[i] <- (point.idw$var1.pred - Pbclean.mydata$Pb.res[i])^2
  }
  pmse[power==p,"mse"] = mean(pse)
}

plot(pmse)



## Cuivre

ggplot() + 
  geom_point(data=mydata.grid, 
             aes(x=X,y=Y,fill="Grid points"),
             color="grey50",
             size=0.05,
             shape=3) +
  geom_point(data=Cuclean.mydata,
             aes(x=X, y=Y, color=Cuclean.mydata$Cu),
             size=2) +
  labs(fill="", color="Cu [mg/mÂ³]") +
  scale_color_gradientn(name="Cu [mg/mÂ³]", colors=c('royalblue','green3', 'yellow', 'red')) +
  xlab("Easting") +
  ylab("Northing") + 
  ggtitle("Cuivre concentration in Liège and prediction grid") + theme_minimal()

#on fait une prédiction via la distance inverse: 
Cu.idw <- idw(formula = Cu.res~1,
              data = Cuclean.mydata,  
              locations = ~X+Y,
              newdata = liege.grid,
              idp = 1.5,
              nmax = 20,
              maxdist=20000)

setnames(Cu.idw, "var1.pred", "Cu.pred")

# On rajoute la tendance et on fait une transformée inverse pour retrouver les valeurs de concentrations prédites
# On rajoute une colonne avec les résidus + la tendance dans le Cr.idw

Cu.idw$Cu.pred.tend <- Cu.idw$Cu.pred + predict(Cu.lm, Cu.idw) # ajout de la tendance via predict(Cu.lm (là oÃ¹ on stocke) à partir des données de Cu.idw) pour calculer la tendance
Cu.idw$Cu.pred.corr <- sph(log(thetaCu*Cu.idw$Cu.pred.tend +1)/thetaCu) #transformation inverse

ggplot() + 
  geom_tile(data=Cu.idw, 
            aes(x = X, y = Y, fill = Cu.pred.corr)) +
  geom_point(data=Cuclean.mydata, 
             aes(x=X, y=Y, color="Measurments points"),
             shape=18,
             size=0.6) +
  scale_color_manual("", values="black") +
  scale_fill_gradientn(name="Cu predictions [mg/mÂ³]", colors=c('royalblue','green3', 'yellow', 'red')) +
  theme(legend.key = element_rect(fill = "green3", 
                                  color = NA)) +
  xlab("Easting") +
  ylab("Northing") + 
  ggtitle("Cuivre concentration in the province of Liege - Inverse Distance prediction") + theme_minimal()


#Optimizing the power value (cross validation)
powers <- seq(1,2,0.25)
pmse <- data.table(power = powers, mse = rep(0,length(powers)) )# Power mean squared error
for (p in powers){
  pse <- rep(0,nrow(Cuclean.mydata)) # Power squared errors
  for (i in 1:nrow(Cuclean.mydata)){
    point.idw <- idw(formula = Cu~1,
                     data = Cuclean.mydata[-i,],  
                     locations = ~X+Y,
                     newdata = Cuclean.mydata[i,],
                     idp = p,
                     nmax = 20,
                     maxdist=20000,
                     debug.level = 0) # to avoid getting many output messages
    pse[i] <- (point.idw$var1.pred - Cuclean.mydata$Cu[i])^2
  }
  pmse[power==p,"mse"] = mean(pse)
}

plot(pmse)




# ========================= KRIGEAGE ========================================

### Zinc

#3. Predict etms values by kriging using the semi-variogram model(s) you just optimized and plot the result.
# Kriging


Zn.krig <- krige(formula = Zn.res~1,
                 data = Znclean.mydata,  
                 locations = ~X+Y,
                 newdata = liege.grid, 
                 model = Znclean.mydata.fit.sph)
## [using ordinary kriging]
setnames(Zn.krig, c("var1.pred", "var1.var"), c("Zn.predkrig", "Zn.varkrig"))

# On rajoute la tendance et on fait une transformée inverse pour retrouver les valeurs de concentrations prédites
# On rajoute une colonne avec les résidus + la tendance dans le Cr.idw

Zn.krig$Zn.pred.tend <- Zn.krig$Zn.predkrig + predict(Zn.lm, Zn.krig) # ajout de la tendance via predict(Zn.lm se trouvant dans Zn.krig) pour calculer la tendance
Zn.krig$Zn.pred.corr <- exp(log(thetaZn*Zn.krig$Zn.pred.tend +1)/thetaZn) #transformation inverse


# plot kriging
ggplot() + 
  geom_tile(data=Zn.krig, 
            aes(x = X, y = Y, fill = Zn.pred.corr)) +
  geom_point(data=Znclean.mydata, 
             aes(x=X, y=Y, color="Measurement points"),
             shape=5,
             size=0.5) +
  scale_color_manual("", values="black") +
  scale_fill_gradientn(name="Prédiction du zn [mg/kg]", colors=c('royalblue','green3', 'yellow', 'red'))+
  theme(legend.key = element_rect(fill = "green3", 
                                  color = NA)) +
  xlab("Longitude") +
  ylab("Latitude") +
  ggtitle("concentration du zinc à Liège - Prédiction par Krigeage")+ theme_minimal()


# plot kriging variance
ggplot() + 
  geom_tile(data=Zn.krig, 
            aes(x = X, y = Y, fill = Zn.varkrig)) +
  geom_point(data=Znclean.mydata, 
             aes(x=X, y=Y, color="Points de mesure"),
             shape=18,
             size=0.9) +
  scale_color_manual("", values="black") +
  scale_fill_gradientn(name="Var. pred. Zn
  [mg/kg]", colors=c('royalblue','green3', 'yellow', 'red'))+
  theme(legend.key = element_rect(fill = "green3", 
                                  color = NA)) +
  xlab("Longitude") +
  ylab("Latitude") +
  ggtitle("concentration du zinc à Liège - Variance de prédiction")+ theme_minimal()

### Plomb

#3. Predict Plomb values by kriging using the semi-variogram model(s) you just optimized and plot the result.
# Kriging
Pb.krig <- krige(formula = Pb.res~1,
                 data = Pbclean.mydata,  
                 locations = ~X+Y,
                 newdata = liege.grid, 
                 model = Pbclean.mydata.fit.sph)
## [using ordinary kriging]
setnames(Pb.krig, c("var1.pred", "var1.var"), c("Pb.predkrig", "Pb.varkrig"))

Pb.krig$Pb.pred.tend <- Pb.krig$Pb.predkrig + predict(Pb.lm, Pb.krig) # ajout de la tendance via predict(Pb.lm se trouvant dans Pb.krig) pour calculer la tendance
Pb.krig$Pb.pred.corr <- exp(log(thetaPb*Pb.krig$Pb.pred.tend +1)/thetaPb) #transformation inverse


# plot kriging
ggplot() + 
  geom_tile(data=Pb.krig, 
            aes(x = X, y = Y, fill = Pb.pred.corr)) +
  geom_point(data=Pbclean.mydata, 
             aes(x=X, y=Y, color="Points de mesure"),
             shape=3,
             size=0.7) +
  scale_color_manual("", values="black") +
  scale_fill_gradientn(name="Prédiction du Pb [mg/kg]", colors=c('royalblue','green3', 'yellow', 'red'))+
  theme(legend.key = element_rect(fill = "green3", 
                                  color = NA)) +
  xlab("Longitude") +
  ylab("Latitude") +
  ggtitle("Prédiction de la concentration du Pb à Liège - Krigeage")+ theme_minimal()


# plot kriging variance
ggplot() + 
  geom_tile(data=Pb.krig, 
            aes(x = X, y = Y, fill = Pb.varkrig)) +
  geom_point(data=Pbclean.mydata, 
             aes(x=X, y=Y, color="Points de mesure"),
             shape=18,
             size=0.6) +
  scale_color_manual("", values="black") +
  scale_fill_gradientn(name="Var. Pred. du Pb
  [mg/kg]", colors=c('royalblue','green3', 'yellow', 'red'))+
  theme(legend.key = element_rect(fill = "green3", 
                                  color = NA)) +
  xlab("Longitude") +
  ylab("Latitude") +
  ggtitle("Variance de prédiction de la concentration du plomb à Liège")+ theme_minimal()

### Cuivre
#3. Predict calcium values by kriging using the semi-variogram model(s) you just optimized and plot the result.
# Kriging
Cu.krig <- krige(formula = Cu.res~1,
                 data = Cuclean.mydata,  
                 locations = ~X+Y,
                 newdata = liege.grid, 
                 model = Cuclean.mydata.fit.sph)
## [using ordinary kriging]
setnames(Cu.krig, c("var1.pred", "var1.var"), c("Cu.predkrig", "Cu.varkrig"))

Cu.krig$Cu.pred.tend <- Cu.krig$Cu.predkrig + predict(Cu.lm, Cu.krig) # ajout de la tendance via predict(Cu.lm se trouvant dans Cu.krig) pour calculer la tendance
Cu.krig$Cu.pred.corr <- exp(log(thetaCu*Cu.krig$Cu.pred.tend +1)/thetaCu) #transformation inverse

# plot kriging
ggplot() + 
  geom_tile(data=Cu.krig, 
            aes(x = X, y = Y, fill = Cu.pred.corr)) +
  geom_point(data=Cuclean.mydata, 
             aes(x=X, y=Y, color="Points de mesure"),
             shape=18,
             size=0.8) +
  scale_color_manual("", values="black") +
  scale_fill_gradientn(name="Préd. du cuivre [mg/kg]", colors=c('royalblue','green3', 'yellow', 'red'))+
  theme(legend.key = element_rect(fill = "green3", 
                                  color = NA)) +
  xlab("Longitude") +
  ylab("Latitude") +
  ggtitle("Prédiction de la concentration du Cuivre à Liège - Krigeage")+ theme_minimal()


# plot kriging variance
ggplot() + 
  geom_tile(data=Cu.krig, 
            aes(x = X, y = Y, fill = Cu.varkrig)) +
  geom_point(data=Cuclean.mydata, 
             aes(x=X, y=Y, color="Points de mesure"),
             shape=18,
             size=0.7) +
  scale_color_manual("", values="black") +
  scale_fill_gradientn(name="Var. pred. Cu.
  [mg/kg]", colors=c('royalblue','green3', 'yellow', 'red'))+
  theme(legend.key = element_rect(fill = "green3", 
                                  color = NA)) +
  xlab("Longitude") +
  ylab("Latitude") +
  ggtitle("Variance de prédiction de la concentration du cuivre à Liège")+ theme_minimal()


#==============================================================================

# ========================= COKRIGEAGE ========================================

# We add the Plomb data to the gstat object created earlier (we can only we points with both variables)
# On ajoute Pb.res au data Znclean.mydata

g <- gstat(id ="Zn.res", formula=Zn.res~1, 
           data=na.omit(Znclean.mydata), locations=~X+Y, nmax=20)

g <- gstat(g, id ="Pb.res", formula=Pb.res~1, 
           data=na.omit(Pbclean.mydata), locations=~X+Y, nmax=20)

v.cross <- variogram(g, cutoff=40000, width=3000)

#The covariance between Zinc and plomb
plot(v.cross)

LMC <- fit.lmc(v.cross, g, 
               model=Znclean.mydata.fit.sph, 
               correct.diagonal=1.01,
               fit.method=6) # Voir documentation pour comprendre correct.diagonal. Ici ce n'est pas nécessaire à part pour vous montrer que l'option existe au cas oÃ¹ vous rencontreriez des problèmes
LMC

#Refill your gstat object with the complete Zn data
g <- copy(LMC)
g <- gstat(g, id="Zn.res", form=Zn.res~1, data=Znclean.mydata, 
           locations=~X+Y, model=LMC$model$Zn.res) # Attention ici il ne faut pas mettre na.omit car on veut bien garder toutes les données du Pb !!!

#Display the variograms, cross variograms and models on a single figure.
plot(v.cross, model=g$model, col='black', pch=16)
trellis.focus("panel",1,1)
llines(x=c(0,max(v.cross$dist)), 
       y=c(cov(na.omit(Znclean.mydata)$Zn.res, na.omit(Pbclean.mydata)$Pb.res),
           cov(na.omit(Znclean.mydata)$Zn.res, na.omit(Pbclean.mydata)$Pb.res)),
       col="red", lwd=1, lty=2)


#Predict plomb values at your grid points using cokriging
Pb.cok <- predict(g, liege.grid)

setnames(Pb.cok, c("Pb.res.pred", "Pb.res.var"), c("Pb.predcok", "Pb.varcok"))

#Transformee inverse avec ajout de la tendance
Cokvalue = Pb.cok$Pb.predcok + predict(Pb.lm, Pb.cok) # ajout de la tendance via predict(Pb.lm se trouvant dans Pb.krig) pour calculer la tendance
Pb.cok$Pbpredcok.corr <- exp(log(thetaPb*Cokvalue +1)/thetaPb) #transformation inverse

# plot cokriging
ggplot() + 
  geom_tile(data=Pb.cok, 
            aes(x=X, y=Y, fill=Pbpredcok.corr)) +
  geom_point(data=Pbclean.mydata, 
             aes(x=X, y=Y, shape="Zinc data", size="Zinc data"),
             color="purple") +
  geom_point(data=na.omit(Pbclean.mydata), 
             aes(x=X, y=Y, shape="Plomb data", size="Plomb data"),
             color="black") +
  scale_shape_manual("", values=c(20,15)) +
  scale_size_manual("", values=c(0.6,0.6)) +
  scale_fill_gradientn(name="Préd. du plomb [mg/kg]", colors=c('royalblue','green3', 'yellow', 'red'))+
  theme(legend.key=element_rect(fill="green3", 
                                color=NA)) +
  xlab("Longitude") +
  ylab("Latitude") +
  ggtitle("Prédiction du Plomb par cokrigeage avec le zinc")+ theme_minimal()



ggplot() + 
  geom_tile(data=Pb.cok, 
            aes(x=X, y=Y, fill=cov.Zn.res.Pb.res)) +
  geom_point(data=Znclean.mydata, 
             aes(x=X, y=Y, shape="Données du Zn", size="Données du Zn"),
             color="purple") +
  geom_point(data=na.omit(Pbclean.mydata), 
             aes(x=X, y=Y, shape="Données du Pb", size="Données du Pb"),
             color="black") +
  scale_shape_manual("", values=c(1,6)) +
  scale_size_manual("", values=c(1,2)) +
  scale_fill_gradientn(name="Var. Pred. Pb 
  [mg/kg]", colors=c('royalblue','green3', 'yellow', 'red'))+
  theme(legend.key=element_rect(fill="green3", 
                                color="red")) +
  xlab("Longitude") +
  ylab("Latitude") +
  ggtitle("Variance de prediction du Plomb (via Zinc)")+ theme_minimal()

# Display the prediction variance of the calcium for the kriging and cokriging side by side.
# First we define the max and min values of both prediction variances to build the same scale on both figures to be able to compare them
Pb_minvar = min(c(Pb.cok$Pb.varcok, Pb.krig$Pb.varkrig))
Pb_maxvar = max(c(Pb.cok$Pb.varcok, Pb.krig$Pb.varkrig))

# Kriging prediction variance
ggplot() + 
  geom_tile(data=Pb.krig, 
            aes(x = X, y = Y, fill = Pb.varkrig)) +
  geom_point(data=na.omit(Pbclean.mydata), 
             aes(x=X, y=Y, color="Points de mesure"),
             shape=19,
             size=1.2) + 
  scale_color_manual("", values="black") +
  scale_fill_gradientn(name="Var. préd. krig. Pb [mg/kg]",
                       colors=c('royalblue','green3', 'yellow', 'red'),
                       limits=c(Pb_minvar,Pb_maxvar))+
  theme(legend.key = element_rect(fill = 'green3', 
                                  color = NA)) +
  xlab("Longitude") +
  ylab("Latitude") +
  ggtitle("Variance de prédiction du krigeage de la mydata. du Pb à Liège")+ theme_minimal()

#To have a clearer view of the improvement brought by chloride data, display the difference between the prediction variance of the Cdlcium for the kriging and cokriging
# First we define the max and min values of both prediction variances to build the same sCdle on both figures to be able to compare them
vardif <- liege.grid
vardif$vardif <- Pb.cok$Pb.varcok - Pb.krig$Pb.varkrig


# Cokriging prediction variance
ggplot() + 
  geom_tile(data=vardif, 
            aes(x=X, y=Y, fill=vardif)) +
  geom_point(data=Znclean.mydata, 
             aes(x=X, y=Y, shape="Zinc", size="Zinc"),
             color="magenta") +
  geom_point(data=na.omit(Pbclean.mydata), 
             aes(x=X, y=Y, shape="Plomb", size="Plomb"),
             color="black") +
  
  labs(fill="Diff. de var. de prédiction du Pb") +
  scale_fill_gradientn( colors=c('royalblue','green3', 'yellow', 'red'))+
  scale_shape_manual("", values=c(20,16)) +
  scale_size_manual("", values=c(1.2,1.2)) +
  theme(legend.key=element_rect(fill='green3', 
                                color=NA)) +
  xlab("Longtitude") +
  ylab("Latitude") +
  ggtitle("Différence entre variance krigeage et var. cokrigeage")+ theme_minimal()




#============= CONDITIONAL SIMULATION ===================================

# Carte de risque du Plomb


Pb.cond.sim <- krige(formula = Pb~1, data =na.omit(mydata), loc=~X+Y, 
                     newdata = liege.grid,
                     model = Pbclean.mydata.fit.sph, nsim = 1000, nmax = 20, maxdist = 40000)

Pb.cond.sim <- as.data.table(Pb.cond.sim)

## Cartographie des 1ere et n/2 ieme simulations

ggplot() + 
  geom_tile(data=Pb.cond.sim, 
            aes(x = X, y = Y, fill = sim1)) + 
  geom_point(data=na.omit(mydata), 
             aes(x=X, y=Y, color="Points de mesure"),
             shape=20,
             size=2) +
  scale_color_manual("", values="black") +
  scale_fill_gradientn(name="Pb simulation [mg/kg]", colors=c('royalblue','green3', 'yellow', 'red')) +
  theme(legend.key = element_rect(fill = "green3", 
                                  color = NA))+
  xlab("Latitude") +
  ylab("Longitude") +
  ggtitle("Simulation conditionnelle du plomb")+ theme_minimal()


ggplot() + 
  geom_tile(data=Pb.cond.sim, 
            aes(x = X, y = Y, fill = sim500)) + # center title
  geom_point(data=Pbclean.mydata, 
             aes(x=X, y=Y, color="Points de mesure"),
             shape=20,
             size=2) +
  scale_color_manual("", values="black") +
  scale_fill_gradientn(name="Pb simulation [mg/kg]", colors=c('royalblue','green3', 'yellow', 'red')) +
  theme(legend.key = element_rect(fill = "green3", 
                                  color = NA))+
  xlab("Latitude") +
  ylab("Longitude") +
  ggtitle("Simulation conditionnelle du plomb")+ theme_minimal()


#-----------------------------------------------------------------------------

# Find the points of the grid where the probability to have a plomb concentration higher than 70mg/kg is higher than 0,8. 
#Map the grid points in black and circle the points at risk in red.


ishigh <- Pb.cond.sim[,-c(1,2)] > 70
risk <- data.table(x=Pb.cond.sim$X, y = Pb.cond.sim$Y, 
                   Cam = rowSums(ishigh)/1000)

ggplot() + geom_point(data = risk, aes(x=x,y=y), color='gray') + 
  geom_point(data = risk[Cam>.8,], aes(x=x,y=y), 
             shape = 1, color = 'red',size = 2) +xlab("Longitude") +
  ylab("Latitude") +
  ggtitle("Carte de risque du plomb [concentration > 70 mg/kg]")+ theme_minimal()+
theme(plot.title=element_text(hjust=0.5))

#Create a color map showing the probability to have a calcium concentration higher than 70mg/l.

ggplot() + geom_tile(data = risk, aes(x=x,y=y, fill=Cam))+
  scale_fill_gradientn(name="P(Pb > 70 mg/kg)", 
                       colors=c('green1','cyan', 'yellow', 
                                'red')) +
  xlab("Longitude") +
  ylab("Latitude") +
  ggtitle("Zone de forte concentration en plomb")+ theme_minimal()+
  theme(plot.title=element_text(hjust=0.5))


# ===================== VARIOGRAMS MODELES =====================================

vgm() # Donne les indices (index) associes au variogrammes

show.vgms()  # Graphiques de tous les modeles de variogrammes

#========================= FIN =================================================

