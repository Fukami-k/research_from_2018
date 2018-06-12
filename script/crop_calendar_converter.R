require(RNetCDF)
require(cshapes)
require(rgl)
require(sf)
require(raster)
require(countrycode)

source("../rawdata/shp2raster/shp2raster.R")

############
#settings
############

#write climate terms to convert
climateterm<-c("tmp","pre")

croplist <- c("Barley","Barley.Winter","Cassava","Cotton","Groundnuts","Maize.2",
            "Maize","Millet","Oats","Oats.Winter","Potatoes","Pulses",
            "Rapeseed.Winter","Rice.2","Rice","Rye.Winter","Sorghum.2",
            "Sorghum","Soybeans","Sugarbeets","Sunflower","Sweet.Potatoes",
            "Wheat","Wheat.Winter","Yams")

crop.ordered<-croplist[7] #sugarbeets　#7 is maize
print(paste(crop.ordered, "is set"))

#crop calendar data position
cropkc<-open.nc(paste0("../rawdata/Sacks_et_al_2010/",crop.ordered,".crop.calendar.fill.nc"))

yearlist<-1980:2008 #year used on calcurate.

order2 <- c("barley", "barley", "cassava", "cotton", "grqaoundnut", "maize",
            "maize", "millet", "oats", "oats", "potato", "pulsenes",
            "rapeseed", "rice", "rice", "rye", "sorghum",
            "sorghum", "soybean", "sugarbeet", "sunflower", "sweetpotato",
            "wheat", "wheat", "yam")[7] #sugarbeets

#crop area data position
cropareaNC <- open.nc(paste0("../rawdata/Monfreda_et_al_2008/HarvestedAreaYield175Crops_NetCDF/", order2, "_HarvAreaYield2000_NetCDF/",
                        order2, "_AreaYieldProduction.nc"))

#CRU file pass
CRUpass <- "../rawdata/CRU_2017/cru_ts"

#CRU file ver
CRUver <- "3.24"


#######
#initialize conponets
#####
yearlength<-length(yearlist)

#give the end of month
EndOfMonth<-c(0,31,59,90,120,151,181,212,243,273,304,334,365) #Number of days in a standard year on the end of each month

print("initialize component has set")
print(Sys.time())
#make list of countries
countrylis<-matrix(numeric(yearlength*250),nrow=yearlength,ncol=250) #first make empty matrix
for(i in yearlist){
    #list all countries that have .shp file
    cshp(date=as.Date(paste0(as.character(i),"-1-1")),useGW=FALSE)$COWCODE->
    countrylis[i-min(yearlist)+1, 1:length(cshp(date=as.Date(paste0(as.character(i),"-1-1")),
    useGW=FALSE)$COWCODE)]
}
countrylis <- as.integer(names(table(countrylis)))[-1]
countrylength <- length(countrylis)

countrylis.table <- data.frame(connum = 1:length(countrylis),  cown = countrylis,
                                faon = countrycode(countrylis,"cown", "fao"),
                                country.name = countrycode(countrylis, "cown", "country.name"))
write.csv(countrylis.table, "../produced_data/csv/countrylis.csv", row.names = FALSE)

print("all countries in .shp are listed")
print(Sys.time())

#set len.yearlist
climateData <- numeric(120*720*360)
dim(climateData) <- c(720,360,120) #make vessel of claimte data

coumaps<- raster(nrow = 360, ncol = 720, xmn = -179.75, xmx = 179.75, ymn = -89.75, ymx = 89.75)
values(coumaps) <- 0

Ccalendar<-array(numeric(720*360*4), dim=(c(720,360,4)))

YdM<-numeric(12)

##########
#make Crop Calender from  month to year
########
Ccalendar[,,1] <- var.get.nc(cropkc,   "plant.start")
Ccalendar[,,2] <- var.get.nc(cropkc,   "plant.range")
Ccalendar[,,3] <- var.get.nc(cropkc, "harvest.start")
Ccalendar[,,4] <- var.get.nc(cropkc, "harvest.range")
Ccalendar[,,3] <- Ccalendar[,,3]+(sign(Ccalendar[,,1]-Ccalendar[,,3])==1)*365


CCalW <- array(numeric(720*360*24),dim=(c(720,360,24)))
for (longit in 1:720){
    for (latit in 1:360){
        if(is.na(sum(Ccalendar[longit,latit,1:4])) == FALSE){
            YdCal<-numeric(730) #365*2

            for(i in  Ccalendar[longit,latit,1]:(Ccalendar[longit,latit,1]+Ccalendar[longit,latit,2]-1)){
                YdCal[i] <- (i-Ccalendar[longit,latit,1])/Ccalendar[longit,latit,2]
            }

            for(i in (Ccalendar[longit,latit,1]+Ccalendar[longit,latit,2]):730){
                YdCal[i] <- 1
            }

            for(i in (Ccalendar[longit,latit,3]+Ccalendar[longit,latit,4]):730){
                YdCal[i] <- 0
            }

            for(i in  Ccalendar[longit,latit,3]:(Ccalendar[longit,latit,3]+Ccalendar[longit,latit,4]-1)){
                YdCal[i]<-YdCal[i]-(i-Ccalendar[longit,latit,3])/Ccalendar[longit,latit,4]
            }

            YdM <- numeric(24)

            for(i in 1:12){
                YdM[i] <- sum(YdCal[(EndOfMonth[i]+1)     : EndOfMonth[i+1]])
            }

            for(i in 13:24){
                YdM[i] <- sum(YdCal[(EndOfMonth[i-12]+366):(EndOfMonth[i-12+1]+365)])
            }

            CCalW[longit,latit,] <- YdM/sum(YdM)

        }else{
            CCalW[longit,latit,] <- rep(NA,24)
        }
    }
    if(longit%%120==0)print(paste0("now ",longit%/%120,"/6 has ended"))
}

save.image(file = paste0("../produced_data/RData/crop_calendar.",crop.ordered,".RData"))

################
#get harvest end day (representitive)
################
cult.start <- var.get.nc(cropkc,   "plant")
cult.end   <- var.get.nc(cropkc,   "harvest")

PH_order <- cult.start < cult.end
