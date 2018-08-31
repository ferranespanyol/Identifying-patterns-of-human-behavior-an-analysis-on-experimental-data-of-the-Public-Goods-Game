
library(data.table)
library(plyr) 
library(dplyr)
library(fitdistrplus)
library(logspline)


# CLIMATE DAU 

#ronda_dau <- read.csv("~/dades/dades_csv/climatedau/game_user.csv")
partida_dau <- read.csv("C:/Users/USER/Desktop/masterdatascience/TFM/dades/dades_csv/climatedau/game_partida.csv")
ronda_dau <- read.csv("C:/Users/USER/Desktop/masterdatascience/TFM/dades/dades_csv/climatedau/game_ronda.csv")
user_dau<- read.csv("C:/Users/USER/Desktop/masterdatascience/TFM/dades/dades_csv/climatedau/game_user.csv")
user_dau = user_dau[-c(4), ] 
userronda_dau<- read.csv("C:/Users/USER/Desktop/masterdatascience/TFM/dades/dades_csv/climatedau/game_userronda.csv")

encuestas <- function(user_ds){
  user_ds$prfinal1<-as.factor(user_ds$prfinal1)
  levels(user_ds$prfinal1)<-c('','No','Yes','Yes, in previous ds experiments')
  user_ds$prfinal2<-as.factor(user_ds$prfinal2)
  levels(user_ds$prfinal2)<-c('','Very Much','Somewhat','Not Really','Not at all')
  user_ds$prfinal3<-as.factor(user_ds$prfinal3)
  levels(user_ds$prfinal3)<-c('','Yes, from the beginning','Yes, after a few rounds',
                               'No, from the beginning','No, after a few rounds')
  user_ds$prfinal4<-as.factor(user_ds$prfinal4)
  levels(user_ds$prfinal4)<-c('','Agree','Disagree','My contribution should not depend on this','n/a')
  user_ds$prfinal5<-as.factor(user_ds$prfinal5)
  levels(user_ds$prfinal5)<-c('','Agree','Disagree','My contribution should not depend on this','n/a')
  user_ds$prfinal6<-as.factor(user_ds$prfinal6)
  levels(user_ds$prfinal6)<-c('','Agree','Disagree','n/a')
  user_ds$prfinal7<-as.factor(user_ds$prfinal7)
  levels(user_ds$prfinal7)<-c('','Agree','Disagree','n/a')
  user_ds$prfinal8<-as.factor(user_ds$prfinal8)
  levels(user_ds$prfinal8)<-c('','Agree','Disagree','n/a')
  user_ds$prfinal9<-as.factor(user_ds$prfinal9)
  levels(user_ds$prfinal9)<-c('','Agree','Disagree','n/a')
  user_ds$prfinal10<-as.factor(user_ds$prfinal10)
  levels(user_ds$prfinal10)<-c('','Doppler Effect','Greenhouse Effect','Faraday Effect','Refrigerator Effect')
  user_ds$prfinal11<-as.factor(user_ds$prfinal11)
  levels(user_ds$prfinal11)<-c('','U.S','Italy','China','Japan')
  user_ds$prfinal12<-as.factor(user_ds$prfinal12)
  levels(user_ds$prfinal12)<-c('','Oil','Carbon','Solar energy','Nuclear energy')
  user_ds$prfinal13<-as.factor(user_ds$prfinal13)
  levels(user_ds$prfinal13)<-c('','Declaration of Helsinki','Kyoto Protocol','Schengen Agreement','Treaty of Versailles')
  user_ds$prfinal14<-as.factor(user_ds$prfinal14)
  levels(user_ds$prfinal14)<-c('','Carbon footprint','Eco-Impact','Individual gas fee','Reduced environmental cost')
  user_ds$prfinal15<-as.factor(user_ds$prfinal15)
  levels(user_ds$prfinal15)<-c('','2%','5%','15%','20%')
  return(user_ds)
}

user_dau = encuestas(user_dau)

#Dades usuaris: 

#prop.table(as.data.frame.matrix(user_dau), 2)
#as.data.frame.matrix(user_dau)

#Eliminar rondes que han arribat al 0:
#id_elimin = filter(ronda_dau, bucket_inici_ronda <=0)
#names(id_elimin)[names(id_elimin) == 'num_ronda'] <- 'ronda_id'

#contr_dau<-contributions.round(userronda_dau,6,as.numeric(tail(userronda_dau["ronda_id"], n=1)/10))
#contr_dau <- subset(contr_dau, partida%in%unique(eq[,"partida_id"]))
#
#
#rounds=10
#players=6
#ngames = as.numeric(tail(userronda_dau["ronda_id"], n=1)/10)
#userronda_dau$partida_id <- rep(seq(1,ngames,length=ngames), each=players*rounds,times=1)
#
#
#
#library(dplyr)
#x =anti_join(userronda_dau, id_elimin, by = "ronda_id")



# CLIMATE STREET

partida_street <- read.csv("C:/Users/USER/Desktop/masterdatascience/TFM/dades/dades_csv/climatestreet/game_partida_street.csv")
ronda_street <- read.csv("C:/Users/USER/Desktop/masterdatascience/TFM/dades/dades_csv/climatestreet/game_ronda_street.csv")
user_street <- read.csv("C:/Users/USER/Desktop/masterdatascience/TFM/dades/dades_csv/climatestreet/game_user_street.csv")
userronda_street <- read.csv("C:/Users/USER/Desktop/masterdatascience/TFM/dades/dades_csv/climatestreet/game_userronda_street.csv")

user_street <-subset(user_street, partida_id!=0)
#install.packages("Hmisc")
library("Hmisc")
userronda_street = userronda_street[which(userronda_street[['user_id']] %in% user_street[['id']]), ]

user_street=encuestas(user_street)

# CLIMATE VILANOVA

## vilanova: import 
partida_vil <- read.csv("C:/Users/USER/Desktop/masterdatascience/TFM/dades/dadesvil/partida.csv")
user_vil <- read.csv("C:/Users/USER/Desktop/masterdatascience/TFM/dades/dadesvil/user.csv")
userronda_vil <- read.csv("C:/Users/USER/Desktop/masterdatascience/TFM/dades/dadesvil/userronda.csv")
ronda_vil<- read.csv("C:/Users/USER/Desktop/masterdatascience/TFM/dades/dadesvil/ronda.csv")

user_vil$rang_edat<-as.factor(user_vil$rang_edat)
levels(user_vil$rang_edat)<-c('','14-19','18-24','25-34','35-44','45-54','55-64','65+')
user_vil$enquesta_final_pr1<-as.factor(user_vil$enquesta_final_pr1)
levels(user_vil$enquesta_final_pr1)<-c('','Cada dia','Cada setmana','Cada mes','Molt poc','Mai')
user_vil$enquesta_final_pr2<-as.factor(user_vil$enquesta_final_pr2)
levels(user_vil$enquesta_final_pr2)<-c('','Matí','Tarda','Nit','Mai')
user_vil$enquesta_final_pr3<-as.factor(user_vil$enquesta_final_pr3)
levels(user_vil$enquesta_final_pr3)<-c('','places','pistes esportives','parcs','espais verds','platja','altres')
user_vil$enquesta_final_pr4<-as.factor(user_vil$enquesta_final_pr4)
levels(user_vil$enquesta_final_pr4)<-c('','Passejo','Surto amb amics','Conec gent','Faig esport',
                                       'Activitats lúdiques','Altres')
user_vil$enquesta_final_pr5<-as.factor(user_vil$enquesta_final_pr5)
levels(user_vil$enquesta_final_pr5)<-c('','Sí','No','n/a')
#user_vil$enquesta_final_pr6<-as.factor(user_vil$enquesta_final_pr6)
#levels(user_vil$enquesta_final_pr6)<-c('','No em sento segur','Està brut','Hi ha massa trànsit de vehicles',
#                                        'Hi ha massa soroll','No hi ha prou verd','Falta espai','Està en mal estat',
#                                        'No hi ha prou pistes esportives','No és accessible per a tothom (nens, gent gran, persones amb discapacitats...)',
#                                        'No és segur per tothom (nens, gent gran, persones amb discapacitats...)','Altres')
user_vil$enquesta_final_pr7<-as.factor(user_vil$enquesta_final_pr7)
levels(user_vil$enquesta_final_pr7)<-c('','Torrent Ballester','Parc de la Marina','La Rambla','Camí del Mar i platja',
                                       'Torre-roja','Plaça Europa','Jardins de Magdalena Modolell','Parc de Can Xic')
user_vil$enquesta_final_pr8<-as.factor(user_vil$enquesta_final_pr8)
levels(user_vil$enquesta_final_pr8)<-c('','Gens','Poc','Regular','Bastant','Molt')
user_vil$enquesta_final_pr9<-as.factor(user_vil$enquesta_final_pr9)
levels(user_vil$enquesta_final_pr9)<-c('','Paviment','Enllumenat','Soroll','Neteja','Espai verd')
user_vil$enquesta_final_pr10<-as.factor(user_vil$enquesta_final_pr10)
levels(user_vil$enquesta_final_pr10)<-c('','Cada dia','Cada setmana','Cada mes','Molt poc','Mai')
user_vil$enquesta_final_pr11<-as.factor(user_vil$enquesta_final_pr11)
levels(user_vil$enquesta_final_pr11)<-c('','Matí','Tarda','Nit','Mai')
user_vil$enquesta_final_pr12<-as.factor(user_vil$enquesta_final_pr12)
levels(user_vil$enquesta_final_pr12)<-c('','Passejo','Surto amb amics','Conec a gent','Activitats lúdiques',
                                        'Estic de pas','Altres')




##1) Subdataset: users / round. 

contributions.round <- function(userronda, players, ngames,user_ds) {
  col <- rep(seq(1,10,length=10), each=players,times=ngames)
  contrib <- userronda[c("seleccio","ronda_id", "user_id")]
  contrib["ronda_id"]<- col
  res <- reshape(contrib, timevar = "ronda_id", idvar = "user_id", direction = "wide")
  rownames(res) <- res$user_id
  res$user_id <- NULL
  return(res)
}

contributions.norm <- function(csv){
  contr_norm<- read.csv(paste("C:/Users/USER/Desktop/final_TFM/Identifying-patterns-of-human-behavior-an-analysis-on-experimental-data-of-the-Public-Goods-Game/dades/contr_norm/",csv , sep = ""),
                                 header = TRUE)
  rownames(contr_norm) <- contr_norm$user_id
  contr_norm$user_id <- NULL
  return(contr_norm)
}
contr_dau <- contributions.round(userronda_dau,6,54, user_dau)
contr_street <- contributions.round(userronda_street,6,18,user_street)
contr_vil <- contributions.round(userronda_vil,6, 30, user_vil)


ineq_contr_dau_norm <- contributions.norm("ineq_contr_dau_norm.csv")
eq_contr_dau_norm <-contributions.norm("eq_contr_dau_norm.csv")
contr_street_norm<-contributions.norm("contr_street_norm.csv")
contr_vil_norm<-contributions.norm("contr_vil_norm.csv")
  
heterogeneous <- rbind(ineq_contr_dau_norm, contr_street_norm) 
homogeneous <- rbind(eq_contr_dau_norm, contr_vil_norm) 

## Mean contribution per round with INEQUALITY

col_mean_ineqdau = colMeans(ineq_contr_dau_norm,na.rm=FALSE, dim=1)
col_mean_eqdau = colMeans(eq_contr_dau_norm,na.rm=FALSE, dim=1)
col_mean_street = colMeans(contr_street_norm,na.rm=FALSE, dim=1)
col_mean_vil = colMeans(contr_vil_norm,na.rm=FALSE, dim=1)

x <- seq(1,10,length=10)
#sdev <-apply(contr_dau[,2:11],2,sd,na.rm=TRUE)
plot(x,col_mean_ineqdau, col="blue", lwd=2, type="l",xlab = "Round", 
     ylab = "Mean contribution", main="Mean Contribution with all games")
lines(x,col_mean_eqdau, col ="orange", lwd=2)
#arrows(x, col_mean_dau-sdev, x, col_mean_dau+sdev, length=0.05, angle=90, code=3)
#sdev <-apply(contr_street[,2:11],2,sd,na.rm=TRUE)
lines(x,col_mean_street, col ="green", lwd=2)
#arrows(x, col_mean_street-sdev, x, col_mean_street+sdev, length=0.05, angle=90, code=3)
#sdev <-apply(contr_vil[,2:11],2,sd,na.rm=TRUE)
lines(x,col_mean_vil,col = "red", lwd=2)
#arrows(x, col_mean_vil-sdev, x, avg+sdev, length=0.05, angle=90, code=3)
legend("topright",legend=c("Heterogeneous DAU", "Homogeneous DAU","STREET","VIL"),lwd=3.0,col=c("blue", "orange","green","red"))


#user_dau = user_dau[-c(4), ] 
table(user_dau[,"genere"])
table(user_street[,"genere"])
table(user_vil[,"genere"])




socio.demog.data <- function(user_ds,ds){
  names(user_ds)[names(user_ds) == 'id'] <- 'user_id'
  ds_user <- merge(ds, user_ds, by='user_id')

  # Genere:
  ds_user$genere<-as.factor(ds_user$genere)
  levels(ds_user$genere)<-c('female','male')
  
  # Ages:
  agebreaks <- c(0,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,100)
  #agelabels <- c("0-1","1-4","5-9","10-14","15-19","20-24","25-29","30-34",
  #               "35-39","40-44","45-49","50-54","55-59","60-64","65-69",
  #               "70-74","75-79","80-84","85+")

  agelabels <- c("0-9","10-14","15-19","20-24","25-29","30-34",
                 "35-39","40-44","45-49","50-54","55-59","60-64","65-69",
                 "70-74","75-79","80+")
  setDT(ds_user)[ , agegroups := cut(ds_user$rang_edat, 
                                      breaks = agebreaks, 
                                      right = FALSE, 
                                      labels = agelabels)]
  
  #Studies level:
  ds_user$nivell_estudis<-as.factor(ds_user$nivell_estudis)
  levels(ds_user$nivell_estudis)<-c('','none','primary','secondary','bachillerat','prof','univ','other')
  return(ds_user)
}




#DAU: 
dau_socio = socio.demog.data(user_dau,dau)

#Dades socio: 
table(dau_socio$agegroups)
#plot(dau_socio$agegroups)
plot(dau_socio$agegroups,xlab="age",ylab="counts", main= "Age distribution DAU",col="cyan")
table(dau_socio$nivell_estudis)
plot(dau_socio$nivell_estudis,xlab="studies",ylab="counts", main= "Studies distribution DAU",col="cyan")

#STREET: 
street_socio = socio.demog.data(user_street,street)
table(street_socio$agegroups)
plot(street_socio$agegroups,xlab="age",ylab="counts", main= "Age distribution STREET",col="cyan")
table(street_socio$nivell_estudis)
plot(street_socio$nivell_estudis,xlab="studies",ylab="counts", main= "Studies distribution STREET",col="cyan")

#VIL: 
names(user_vil)[names(user_vil) == 'id'] <- 'user_id'
vil_user <- merge(vil, user_vil, by='user_id')
#Genere
vil_user$genere<-as.factor(vil_user$genere)
levels(vil_user$genere)<-c('female','male')
#Age

vil_user$rang_edat<-as.factor(vil_user$rang_edat)
#levels(vil_user$rang_edat)<-c('0-10','11-15','16-18','19-25','26-35','36-45','46-55','56-65','66 +')
levels(vil_user$rang_edat)<-c('14-19','20-24','25-34','35-44','45-54','55-64','65+')
#levels(vil_user$rang_edat) <- c("0-9","10-14","15-19","20-24","25-29","30-34",
#               "35-39","40-44","45-49","50-54","55-59","60-64","65-69",
#               "70-74","75-79","80+")
#Studies level:
vil_user$nivell_estudis<-as.factor(vil_user$nivell_estudis)
levels(vil_user$nivell_estudis)<-c('','none','primary','secondary','bachillerat','prof','univ','other')
vil_socio = vil_user
table(vil_socio$rang_edat)
table(vil_socio$nivell_estudis)
plot(vil_socio$rang_edat,xlab="age",ylab="counts", main= "Age distribution VIL",col="cyan")
plot(vil_socio$nivell_estudis,xlab="studies",ylab="counts", main= "Studies distribution VIL",col="cyan")
##Write to csv:
#write.csv(dau_socio, file = "C:/Users/USER/Desktop/masterdatascience/TFM/dau_r.csv")
#write.csv(street_socio, file = "C:/Users/USER/Desktop/masterdatascience/TFM/street_r.csv")
#write.csv(vil_socio, file = "C:/Users/USER/Desktop/masterdatascience/TFM/vil_r.csv")



###Eliminar rondes que han arribat al 0:
## DAU
ronda_dau_0= filter(ronda_dau, !bucket_inici_ronda <=0)
table(ronda_dau_0[,"num_ronda"])
end_game_dau = c(0,0,0,0,0,1,5,15,14,19)
plot(end_game_dau,type="o",xlab="round",ylab="ending games", main= "Ending Game DAU")

## STREET
ronda_street_0= filter(ronda_street, !bucket_inici_ronda <=0)
table(ronda_street_0[,"num_ronda"])
end_game_street = c(0,0,0,0,0,0,2,3,6,7)
plot(end_game_street,type="o",xlab="round",ylab="ending games", main= "Ending Game STREET")

## VIL
#Eliminar rondes que han arribat al 0:
ronda_vil_0= filter(ronda_vil, !bucket_inici_ronda <=0)
table(ronda_vil_0[,"num_ronda"])
x = table(ronda_vil_0$num_ronda)
end_game_vil = c(0,0,0,0,0,0,3,9,11,7)
plot(end_game_vil,type="o",xlab="round",ylab="games", main= "Ending Game VIL")

## PLOT DELS ENDING GAMES##
plot(end_game_dau,xlab="round",type="l",ylab="games", main= "Ending Games",col = "red",lwd=3.0)
lines(end_game_street,col="green",lwd=3.0)
lines(end_game_vil, col = "blue",lwd=3.0)
legend("topleft",legend=c("DAU","STREET","VIL"),lwd=3.0,col=c("red","green","blue"))


#write.csv(ronda_dau_0, file = "C:/Users/USER/Desktop/masterdatascience/TFM/ronda_dau_0.csv")
#write.csv(ronda_street_0, file = "C:/Users/USER/Desktop/masterdatascience/TFM/ronda_street_0.csv")
#write.csv(ronda_vil_0, file = "C:/Users/USER/Desktop/masterdatascience/TFM/ronda_vil_0.csv")
##First I need to reorder datasets:
#write.csv(contr_dau, file = "C:/Users/USER/Desktop/masterdatascience/TFM/daur.csv")
#write.csv(contr_street, file = "C:/Users/USER/Desktop/masterdatascience/TFM/streetr.csv")
#write.csv(contr_vil, file = "C:/Users/USER/Desktop/masterdatascience/TFM/vilr.csv")


#### MUTUAL INFORMATION AND ENTROPY:
#
#library("infotheo")
#osel<- dau[dau["partida"]==1,3:12]
#only_selection <- dau[dau["partida"]==1,3:12]
#
#for (i in c(1:10)){
#  only_selection[,i] <- as.factor(only_selection[,i])
#  levels(only_selection[,i])<-list(L = c("0","1"), M = c("2","3"), H = c("4","5"))
#}
#round(mutinformation(only_selection),5)
#
#I2<- mutinformation(dau[,3],dau[,4])
#



#### PARTIDES

#ronda_dau$bucket <- transform(ronda_dau, new.col = bucket_final_ronda - bucket_inici_ronda)
## DAU
ronda_dau$bucket <- ronda_dau$bucket_inici_ronda - ronda_dau$bucket_final_ronda
ronda_dau_partida <- ronda_dau[,c("num_ronda","partida_id","bucket")]
res <- reshape(ronda_dau_partida, timevar = "num_ronda", idvar = "partida_id", direction = "wide")
res

## STREET
ronda_street$bucket <- ronda_street$bucket_inici_ronda - ronda_street$bucket_final_ronda
ronda_street_partida <- ronda_street[,c("num_ronda","partida_id","bucket")]
res <- reshape(ronda_street_partida, timevar = "num_ronda", idvar = "partida_id", direction = "wide")
res

## VIL
ronda_vil$bucket <- ronda_vil$bucket_inici_ronda - ronda_vil$bucket_final_ronda
ronda_vil_partida <- ronda_vil[,c("num_ronda","partida_id","bucket")]
res <- reshape(ronda_vil_partida, timevar = "num_ronda", idvar = "partida_id", direction = "wide")
res

#install.packages('reshape')
library(reshape)

info_partida = cast(ronda_dau_partida, partida_id ~ num_ronda)


#write.csv(ronda_dau_partida, file = "C:/Users/USER/Desktop/masterdatascience/TFM/ronda_dau_partida.csv")
#write.csv(ronda_street_partida, file = "C:/Users/USER/Desktop/masterdatascience/TFM/ronda_street_partida.csv")
#write.csv(ronda_vil_partida, file = "C:/Users/USER/Desktop/masterdatascience/TFM/ronda_vil_partida.csv")
#
##Write to csv:
#write.csv(dau, file = "C:/Users/USER/Desktop/masterdatascience/TFM/dau.csv")
#write.csv(street, file = "C:/Users/USER/Desktop/masterdatascience/TFM/street.csv")
#write.csv(vil, file = "C:/Users/USER/Desktop/masterdatascience/TFM/vil.csv")

### PRINCIPAL COMPONENT ANALYSIS: 
## http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/118-principal-component-analysis-in-r-prcomp-vs-princomp/
#
#library('factoextra')
#res.pca <- prcomp(heterogeneous, scale = TRUE)
#fviz_eig(res.pca)
#
#fviz_pca_ind(res.pca,
#             col.ind = "cos2", # Color by the quality of representation
#             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
#             repel = TRUE     # Avoid text overlapping
#)
#
#fviz_pca_var(res.pca,
#             col.var = "contrib", # Color by contributions to the PC
#             repel = TRUE     # Avoid text overlapping
#)


#############################
##### CLUSTERING ############
#############################
library("MASS")
library("cluster")
library("NbClust")
library('factoextra')

## Number of clusters for treatment groups
#Heterogeneous - Contribucions 270 users
nb <- NbClust(heterogeneous, distance = "euclidean", min.nc = 2,
              max.nc = 8, method = "kmeans")
fviz_nbclust(nb)

#Homogeneous DAU - Contribucions 342 users
nb <- NbClust(homogeneous, distance = "euclidean", min.nc = 2,
              max.nc = 8, method = "kmeans")
fviz_nbclust(nb)

--------------------------------------------------------------------------
## Number of clusters according particular indices: 

nb <- NbClust(heterogeneous, distance = "euclidean", min.nc = 2,
              max.nc = 8, method = "kmeans", index =  "silhouette")
fviz_nbclust(nb)

nb <- NbClust(homogeneous, distance = "euclidean", min.nc = 2,
              max.nc = 8, method = "kmeans", index =  "silhouette")
fviz_nbclust(nb)

--------------------------------------------------------------------------
#Heterogeneous DAU - Contribucions
nb <- NbClust(ineq_contr_dau_norm, distance = "euclidean", min.nc = 2,
              max.nc = 10, method = "kmeans", index =  "gap")
fviz_nbclust(nb)

#STREET
nb <- NbClust(contr_street_norm, distance = "euclidean", min.nc = 2,
              max.nc = 10, method = "kmeans", index =  "gap")
fviz_nbclust(nb)

#Homogeneous DAU - Contribucions
nb <- NbClust(eq_contr_dau_norm, distance = "euclidean", min.nc = 2,
              max.nc = 10, method = "kmeans", index =  "gap")
fviz_nbclust(nb)

#VIL
nb <- NbClust(contr_vil_norm, distance = "euclidean", min.nc = 2,
              max.nc = 10, method = "kmeans", index =  "gap")
fviz_nbclust(nb)

--------------------------------------------------------------------------
###KMEANS
#clust_street <- kmeans(contr_street_norm, 2, nstart = 20)
#clust_street_vect <- clust_street$cluster
#
#clust_street <- kmeans(contr_street[,2:11], 3, nstart = 20)
#clust_street_vect <- clust_street$cluster
#
#clust_vil <- kmeans(contr_vil[,2:11], 3, nstart = 20)
#clust_vil_vect <- clust_vil$cluster
#
#require(cluster)
#layout(1:3)
#clusplot(contr_dau[,2:11], clust_dau_vect, color=TRUE, shade=TRUE, labels=2, lines=0)
#clusplot(contr_street[,2:11], clust_street_vect, color=TRUE, shade=TRUE, labels=2, lines=0)
#clusplot(contr_vil[,2:11], clust_vil_vect, color=TRUE, shade=TRUE, labels=2, lines=0)
#
##Dau -- conditional cooperation
#clust_dau_cond <- kmeans(dau_comp_last[,1:10], 2, nstart = 20)
#clust_dau_cond_vect <- clust_dau_cond$cluster
##street -- conditional cooperation
#clust_street_cond <- kmeans(street_comp_last[,1:10], 3, nstart = 20)
#clust_street_cond_vect <- clust_street_cond$cluster

--------------------------------------------------------------------------
setwd('C:/Users/USER/Desktop/final_TFM/Identifying-patterns-of-human-behavior-an-analysis-on-experimental-data-of-the-Public-Goods-Game/dades/')
categ_het <- read.csv("categ_het.csv")
categ_hom <- read.csv("categ_hom.csv")
colnames(categ_het)[colnames(categ_het) == 'contr.budget'] <- 'contr_ratio'
colnames(categ_hom)[colnames(categ_hom) == 'contr.budget'] <- 'contr_ratio'

# 1 - Heatmap. y-axis: cluster, x-axis: 
#library(RColorBrewer)
#coul = colorRampPalette(brewer.pal(8, "PiYG"))(25)
#dau_fin = cbind(contr_dau[,2:11],clust_dau_vect)
#heatmap(data.matrix(dau_fin), Colv = NA, Rowv = NA, scale="column", col = coul, xlab="variable", ylab="car", main="heatmap")


## 2 - Distributions of the total contribution histogram:
fitdist(categ_het$contr_ratio, "norm")
layout(1:1)

#HETEROGENEOUS
descdist(categ_het$contr_ratio, discrete = FALSE)

#Heterogeneous contribution ratio seems to indicate that the distribution is a beta distribution and close to a normal one. 
# The method used: mme = Moment matching estimation consists in equalizing theoretical and empirical
# moments

fit.beta <- fitdist(categ_het$contr_ratio, "beta", method="mme")
plot(fit.beta)
fit.norm <- fitdist(categ_het$contr_ratio, "norm", method="mme")
plot(fit.norm)

## Normal distribution seems to gives better results. 
fit.beta$aic
#[1] Inf
fit.norm$aic
#[1] -48.76997

#According to aic parameter I would choose the norm distribution

#HOMOGENEOUS
fitdist(categ_hom$contr_ratio, "norm")

layout(1:1)
descdist(categ_hom$contr_ratio, discrete = FALSE)

#Homogeneous contribution ratio seems to indicate that the distribution is a normal distribution or at least a gamma one.
fit.gamma <- fitdist(categ_hom$contr_ratio, "gamma", method="mme")
plot(fit.gamma)
fit.norm <- fitdist(categ_hom$contr_ratio, "norm", method="mme")
plot(fit.norm)

#The QQPlot for the normal distribution seems to show better results than for the gamma distribution case. 

## Normal distribution seems to gives better results. 
fit.lnorm$aic
#[1] Inf
fit.norm$aic
#[1] -311.1873

## Again, normal distribution is the best distribution. 


### GINI coefficient: 

#install.packages('ineq')
library(ineq)

gini.ineq <- function(contr_ds){
  gini <- NULL
  for (i in 1:10) {
    x1 <- c(contr_ds[i])
    #as.numeric(unlist(x1))
    #ineq(as.numeric(unlist(x1)), type = 'Gini')
    gini <- c(gini,ineq(as.numeric(unlist(x1)), type = 'Gini'))
    #gini <- c(gini,ineq(as.numeric(unlist(x1)),type="square.var")) 
  }
  cat(gini)
  return(gini)
}


gini_het <- gini.ineq(heterogeneous)
gini_hom <- gini.ineq(homogeneous)

## PLOT DELS ENDING GAMES##
plot(gini_het,xlab="Rounds",type="l",ylab="Gini Coefficient", main= "Gini Coefficient per treatment",
     col = "red",lwd=3.0, ylim=c(0,1))
lines(gini_hom,col="blue",lwd=3.0)

legend("topleft",legend=c("Heterogeneous","Homogeneous"),lwd=3.0,col=c("red","blue"))

## Now gini coefficient in a game:



### GLM: 
install.packages('glmnet')
library('glmnet')

x=subset(categ_het, select=c("contr.budget",'mean_contr'))
x <- matrix(unlist(x), ncol = 2, byrow = TRUE)
y=categ_het[['bin_ed']]
fit1=glmnet(x,y)
print(fit1)
coef(fit1,s=0.01) # extract coefficients at a single value of lambda
#predict(fit1,newx=x[1:10,],s=c(0.01,0.005)) # make predictions

plot(fit1)


## Fucntions used along the analysis.

contributions.round <- function(userronda, players, ngames,user_ds) {
  col <- rep(seq(1,10,length=10), each=players,times=ngames)
  contrib <- userronda[c("seleccio","ronda_id", "user_id")]
  contrib["ronda_id"]<- col
  res <- reshape(contrib, timevar = "ronda_id", idvar = "user_id", direction = "wide")
  res$mean_contr = rowMeans(res[,2:11], na.rm = FALSE, dims = 1)
  res$partida <- user_ds[c('partida_id')]
  contributions <- transform(res, SD=apply(res[,2:11],1, sd, na.rm = TRUE))
}


estBetaParams <- function(mu, var) {
  alpha <- ((1 - mu) / var - 1 / mu) * mu ^ 2
  beta <- alpha * (1 / mu - 1)
  return(params = list(alpha = alpha, beta = beta))
}

create.variables <- function(contr_ds){
  mean_contr_round <- ddply(contr_ds[,c(2:11,13)], .(partida), numcolwise(mean))
  nameVec <- gsub("seleccio","mean",names(mean_contr_round))
  setnames(mean_contr_round, names(mean_contr_round),nameVec)
  View(mean_contr_round)
  View(contr_ds)
  #merge mean_contr_round amb contr_ds
  ds <- merge(contr_ds,mean_contr_round, by="partida")
  # Respect the same round mean
  ds = within(ds, {
    comp_mean10 = ifelse(ds$seleccio.10>=ds$mean.10, 1, 0)
    comp_mean9 = ifelse(ds$seleccio.9>=ds$mean.9, 1, 0)
    comp_mean8 = ifelse(ds$seleccio.8>=ds$mean.8, 1, 0)
    comp_mean7 = ifelse(ds$seleccio.7>=ds$mean.7, 1, 0)
    comp_mean6 = ifelse(ds$seleccio.6>=ds$mean.6, 1, 0)
    comp_mean5 = ifelse(ds$seleccio.5>=ds$mean.5, 1, 0)
    comp_mean4 = ifelse(ds$seleccio.4>=ds$mean.4, 1, 0)
    comp_mean3 = ifelse(ds$seleccio.3>=ds$mean.3, 1, 0)
    comp_mean2 = ifelse(ds$seleccio.2>=ds$mean.2, 1, 0)
    comp_mean1 = ifelse(ds$seleccio.1>=ds$mean.1, 1, 0)
  })
  # According last round mean
  ds = within(ds, {
    comp_last_mean10 = ifelse(ds$seleccio.10>=ds$mean.9 , 1, 0)
    comp_last_mean9 = ifelse(ds$seleccio.9>=ds$mean.8, 1, 0)
    comp_last_mean8 = ifelse(ds$seleccio.8>=ds$mean.7, 1, 0)
    comp_last_mean7 = ifelse(ds$seleccio.7>=ds$mean.6, 1, 0)
    comp_last_mean6 = ifelse(ds$seleccio.6>=ds$mean.5, 1, 0)
    comp_last_mean5 = ifelse(ds$seleccio.5>=ds$mean.4, 1, 0)
    comp_last_mean4 = ifelse(ds$seleccio.4>=ds$mean.3, 1, 0)
    comp_last_mean3 = ifelse(ds$seleccio.3>=ds$mean.2, 1, 0)
    comp_last_mean2 = ifelse(ds$seleccio.2>=ds$mean.1, 1, 0)
  })
  
  #Full conditional cooperation
  ds = within(ds, {
    cond_coop10 = ifelse(ds$seleccio.10>=ds$mean.9 & ds$seleccio.9<=ds$mean.9 | ds$seleccio.10<=ds$mean.9 & ds$seleccio.9>=ds$mean.9, 1, 0)
    cond_coop9 = ifelse(ds$seleccio.9>=ds$mean.8 & ds$seleccio.8<=ds$mean.8 | ds$seleccio.9<=ds$mean.8 & ds$seleccio.8>=ds$mean.8, 1, 0)
    cond_coop8 = ifelse(ds$seleccio.8>=ds$mean.7 & ds$seleccio.7<=ds$mean.7 | ds$seleccio.8<=ds$mean.7 & ds$seleccio.7>=ds$mean.7, 1, 0)
    cond_coop7 = ifelse(ds$seleccio.7>=ds$mean.6 & ds$seleccio.6<=ds$mean.6 | ds$seleccio.7<=ds$mean.6 & ds$seleccio.6>=ds$mean.6, 1, 0)
    cond_coop6 = ifelse(ds$seleccio.6>=ds$mean.5 & ds$seleccio.5<=ds$mean.5 | ds$seleccio.6<=ds$mean.5 & ds$seleccio.5>=ds$mean.5, 1, 0)
    cond_coop5 = ifelse(ds$seleccio.5>=ds$mean.4 & ds$seleccio.4<=ds$mean.4 | ds$seleccio.5<=ds$mean.4 & ds$seleccio.4>=ds$mean.4, 1, 0)
    cond_coop4 = ifelse(ds$seleccio.4>=ds$mean.3 & ds$seleccio.3<=ds$mean.3 | ds$seleccio.4<=ds$mean.3 & ds$seleccio.3>=ds$mean.3, 1, 0)
    cond_coop3 = ifelse(ds$seleccio.3>=ds$mean.2 & ds$seleccio.2<=ds$mean.2 | ds$seleccio.3<=ds$mean.2 & ds$seleccio.2>=ds$mean.2, 1, 0)
    cond_coop2 = ifelse(ds$seleccio.2>=ds$mean.1 & ds$seleccio.1<=ds$mean.1 | ds$seleccio.2<=ds$mean.1 & ds$seleccio.1>=ds$mean.1, 1, 0)
  })
  #FREE RIDING  
  ds = within(ds, {
    free_10 = ifelse(ds$seleccio.10==0, 1, 0)
    free_9 = ifelse(ds$seleccio.9==0, 1, 0)
    free_8 = ifelse(ds$seleccio.8==0, 1, 0)
    free_7 = ifelse(ds$seleccio.7==0, 1, 0)
    free_6 = ifelse(ds$seleccio.6==0, 1, 0)
    free_5 = ifelse(ds$seleccio.5==0, 1, 0)
    free_4 = ifelse(ds$seleccio.4==0, 1, 0)
    free_3 = ifelse(ds$seleccio.3==0, 1, 0)
    free_2 = ifelse(ds$seleccio.2==0, 1, 0)
    free_1 = ifelse(ds$seleccio.1==0, 1, 0)
  })
  return(ds)
}



### STRATEGY 1 : CONDITIONAL COOPERATION ####
#Nombre de vegades que tenim cooperació condicional

conditional.cooperation <- function(ds,name){
  print(name)
  #par(mfrow=c(1,2))
  layout(1:1)
  ds_cond_coop <- ds
  ds_cond_coop$cond_coop <- apply(ds_cond_coop,1,mean)
  
  #Basic statistics for conditional cooperation
  print(summary(ds_cond_coop$cond_coop))
  mu_ds <- mean(ds_cond_coop$cond_coop)
  var_ds <- var(ds_cond_coop$cond_coop)
  #computation of the standard error of the mean
  sem<-sd(ds_cond_coop$cond_coop)/sqrt(length(ds_cond_coop$cond_coop))
  #95% confidence intervals of the mean
  
  print("Confidence Inteval")
  print(c(mean(ds_cond_coop$cond_coop)-2*sem,mean(ds_cond_coop$cond_coop)+2*sem))
  
  #Normality test
  layout(1:1)
  descdist(ds_cond_coop$cond_coop, discrete = FALSE)
  fit.norm <- fitdist(ds_cond_coop$cond_coop, "norm")
  plot(fit.norm)
  
  #The best model that can fit this distribution is a beta-distribution
  #La millor distribució és una distribució beta, la forma concreta:
  params = estBetaParams(mu_ds,var_ds)
  
  #Plot the best fit distribution
  layout(1:1)
  x<- ds_cond_coop$cond_coop
  x.sub <- subset(ds_cond_coop, cond_coop > 0.9)
  print("Users with higher conditional cooperation moves:")
  print(x.sub)
  hist(x, breaks=10, probability=TRUE, c="cyan", main="",xlab="% of conditional cooperators")
  lines(seq(0,1,length=100), dbeta(x=seq(0,1,length=100), params$alpha, params$beta), col="blue")
  lines(density(x), lty=2)
  legend("topright",legend=c("Beta Distribution","Direct adjust"),lwd=3.0,col=c("blue","black"))
}


### STRATEGY 2 : Last Mean ####
#Nombre de vegades que tenim last mean strategy
#quan mirem si la contribució ha estat superior o inferior a la mitja anterior estem evaluant 
#la reciprocitat. 

last.mean <- function(ds,name){
  print(name)
  ds_comp_last <- ds
  ds_comp_last$coop_cond <- apply(ds_comp_last,1,mean)
  
  #Basic statistics for last mean
  print(summary(ds_comp_last$coop_cond))
  mu_ds <- mean(ds_comp_last$coop_cond)
  var_ds <- var(ds_comp_last$coop_cond)
  #computation of the standard error of the mean
  sem<-sd(ds_comp_last$coop_cond)/sqrt(length(ds_comp_last$coop_cond))
  #95% confidence intervals of the mean
  print("Confidence Interval:")
  print(c(mean(ds_comp_last$coop_cond)-2*sem,mean(ds_comp_last$coop_cond)+2*sem))
  
  #Normality tests
  layout(1:1)
  descdist(ds_comp_last$coop_cond, discrete = FALSE)
  fit.norm <- fitdist(ds_comp_last$coop_cond, "norm")
  plot(fit.norm)
  #Again, the best fit is a beta distribution with parameters:
  params = estBetaParams(mu_ds,var_ds)
  
  layout(1:1)
  x2<- ds_comp_last$coop_cond
  hist(x2, breaks=10, probability=TRUE, c="cyan", main=("% Last mean"),xlab="% of use of the strategy")
  lines(seq(0,1,length=100), dbeta(x=seq(0,1,length=100), params$alpha, params$beta), col="blue")
  lines(density(x2), lty=2)
  legend("topright",legend=c("Beta Distribution","Direct adjust"),lwd=3.0,col=c("blue","black"))
}

# STRATEGY 3: FREE RIDING

free.riding <- function(ds,name){
  print(name)
  layout(1:1)
  ds_free <- ds
  ds_free$cond_free <- apply(ds_free,1,mean)
  
  #Basic statistics for conditional cooperation
  print(summary(ds_free$cond_free))
  mu_ds <- mean(ds_free$cond_free)
  var_ds <- var(ds_free$cond_free)
  #computation of the standard error of the mean
  sem<-sd(ds_free$cond_free)/sqrt(length(ds_free$cond_free))
  #95% confidence intervals of the mean
  print("Confidence interval")
  print(c(mean(ds_free$cond_free)-2*sem,mean(ds_free$cond_free)+2*sem))
  
  #Normality test
  layout(1:1)
  descdist(ds_free$cond_free, discrete = FALSE)
  fit.norm <- fitdist(ds_free$cond_free, "norm")
  plot(fit.norm)
  
  #The best model that can fit this distribution is a beta-distribution
  #La millor distribució és una distribució beta, la forma concreta:
  params = estBetaParams(mu_ds,var_ds)
  
  #Plot the best fit distribution
  layout(1:1)
  x3<- ds_free$cond_free
  hist(x3, breaks=10, probability=TRUE, c="cyan", main="Free riding",xlab="% of uses of this strategy")
  lines(seq(0,1,length=100), dbeta(x=seq(0,1,length=100), params$alpha, params$beta), col="blue")
  lines(density(x3), lty=2)
  legend("topright",legend=c("Beta Distribution","Direct adjust"),lwd=3.0,col=c("blue","black"))
}


# DAU: 
#Create the dataset with the new dummy variables: 

## For ALL games
dau = create.variables(contr_dau)
names(user_dau)[names(user_dau) == 'id'] <- 'user_id'
dau_dataset <- merge(dau, user_dau, by='user_id')

dau_same_mean <- dau_dataset[,25:34]
dau_last_mean <- dau_dataset[,35:43]
dau_cond_coop <- dau_dataset[,44:52]
dau_free_riding <- dau_dataset[,53:62]

conditional.cooperation(dau_cond_coop,"DAU")
table(apply(dau_cond_coop,1,sum))

last.mean(dau_last_mean,"DAU")
table(apply(dau_last_mean,1,sum))

free.riding(dau_free_riding,"DAU")
table(apply(dau_free_riding,1,sum))
hist(table(apply(dau_free_riding,1,sum)), breaks=10, probability=TRUE, c="cyan", main="Free riding",xlab="% of uses of this strategy")
barplot(table(apply(dau_free_riding,1,sum)), main="DAU", col = "cyan",
        xlab="Times free riding")

# STREET: 
#Create the dataset with the new dummy variables: 

## For ALL games = Unquality Games
street = create.variables(contr_street_norm)

street_same_mean <- street[,25:34]
street_last_mean <- street[,35:43]
street_cond_coop <- street[,44:52]
street_free_riding <- street[,53:62]

#Strategy 1: Conditional Cooperation
conditional.cooperation(street_cond_coop,"STREET")
table(apply(street_cond_coop,1,sum))

#Strategy 2: Last mean
last.mean(street_last_mean,"STREET")
table(apply(street_last_mean,1,sum))

#Strategy 3: Free Riding
free.riding(street_free_riding,"STREET")
table(apply(street_free_riding,1,sum))
barplot(table(apply(street_free_riding,1,sum)), main="STREET", col = "cyan",
        xlab="Times free riding")

# VILADECANS: 
#Create the dataset with the new dummy variables: 

## For ALL games = Equality Games
vil = create.variables(contr_vil)

vil_same_mean <- vil[,25:34]
vil_last_mean <- vil[,35:43]
vil_cond_coop <- vil[,44:52]
vil_free_riding <- vil[,53:62]

#Strategy 1: Conditional Cooperation
conditional.cooperation(vil_cond_coop,"VIL")
table(apply(vil_cond_coop,1,sum))

#Strategy 2: Last mean
last.mean(vil_last_mean,"VIL")
table(apply(vil_last_mean,1,sum))

#Strategy 3: Free Riding
free.riding(vil_free_riding,"VIL")
table(apply(vil_free_riding,1,sum))
barplot(table(apply(vil_free_riding,1,sum)), main="VIL", col = "cyan",
        xlab="Times free riding")


## Prepare demographic variables: 

### STRATEGY 4: DIFFERENCES IN GENRE ACTIONS #### 

print(dau)
ds_cond_coop <- ds[,44:52]
print(ds_cond_coop)
print()
ds_cond_coop$cond_coop <- apply(ds_cond_coop,1,mean)

dau = create.variables(contr_dau)

dau_same_mean <- dau[,25:34]
dau_last_mean <- dau[,35:43]
dau_cond_coop <- dau[,44:52]
dau_free_riding <- dau[,53:62]
