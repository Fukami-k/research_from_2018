require(countrycode)
require(mgcv)

croplist<-c("Barley","Barley.Winter","Cassava","Cotton","Groundnuts","Maize.2",
                "Maize","Millet","Oats","Oats.Winter","Potatoes","Pulses","Rapeseed.Winter",
                    "Rice.2","Rice","Rye.Winter","Sorghum.2","Sorghum","Soybeans","Sugarbeets",
                        "Sunflower","Sweet.Potatoes","Wheat","Wheat.Winter","Yams")
croplist_for_areacall<-c("barley","barley","cassava","cotton","groundnut","maize",
                            "maize","millet","oats","oats","potato","pulsenes","rapeseed","rice",
                                "rice","rye","sorghum","sorghum","bean","sugarbeet",
                                    "sunflower","sweetpotato","wheat","wheat","yam")



##########
#set up files
#########
#require(gam) #why not mgcv?

read.csv("../processed_data/csv/countrylis.csv")->countrylis
countrylis<-countrylis$x
read.csv("../processed_data/csv/climateData324_180417_for_tmp2.csv")->tmpall
tmpall<-subset(tmpall,crop==gochumon)[,-3]
read.csv("../processed_data/csv/climateData324_180417_for_pre2.csv")->preall
preall<-subset(preall,crop==gochumon)[,-3]
read.csv("../processed_data/csv/climateData324_for_cld22.csv")->cldall
cldall<-subset(cldall,crop==gochumon)[,-3]
read.csv("../rawdata/FAO2014/FAOcrop.csv")->faodata
faodata<-subset(faodata,ItemName==gochumon)

###########
#merge&reshape
###########
faon<-countrycode(countrylis[tmpall$cown],"cown","fao")
tmpall<-cbind(tmpall,faon)

CD<-merge(tmpall,preall,by.x=c("year","cown"),by.y=c("year","cown"))
CD<-merge(CD,cldall,by.x=c("year","cown"),by.y=c("year","cown"))

CD<-CD[,c("year","cown","faon","tmpall","preall","cldall")]

faodata<-reshape(faodata,timevar = "ElementName",idvar=names(faodata)[c(-3,-6)],direction = "wide")
faodata<-subset(faodata,faodata$AreaCode<5000) #we do not need world sum
ClimateFAO<-merge(CD,faodata,by.x=c("year","faon"),by.y=c("Year","AreaCode"),all=T)
ClimateFAO<-cbind(ClimateFAO,countrycode(countrylis[as.integer(ClimateFAO$cown)],"cown","country.name"))

CFAO<-ClimateFAO[,-12]
#head(CFAO)
names(CFAO)<-c(names(CFAO)[-12],"AreaNameFull")
head(CFAO)
#write.table(CFAO,paste0(getwd(),"/CRUFAOdata_for_",gochumon,".csv"),append=TRUE,quote=FALSE,col.names = FALSE) #wish i can find best way to produce..
#CFAO<-na.omit(CFAO)

#######
#set up analyse
#######
CRUFAO<-na.omit(CFAO)
