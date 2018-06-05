require(RNetCDF)
require(raster)

############
#settings
############

#write climate terms to convert
croplist <- c("Barley","Barley.Winter","Cassava","Cotton","Groundnuts","Maize.2",
            "Maize","Millet","Oats","Oats.Winter","Potatoes","Pulses",
            "Rapeseed.Winter","Rice.2","Rice","Rye.Winter","Sorghum.2",
            "Sorghum","Soybeans","Sugarbeets","Sunflower","Sweet.Potatoes",
            "Wheat","Wheat.Winter","Yams")

crop.ordered <- croplist[20] #Sugarbeets
print(paste(crop.ordered, "is set"))

cropkc <- open.nc(paste0("../rawdata/Sacks_et_al_2010/",crop.ordered,".crop.calendar.fill.nc"))

OutputFolder <- "../produced_data/RData"

#######
#initialize conponets
#####

#give the end of month
EndOfMonth <- c(0,31,59,90,120,151,181,212,243,273,304,334,365) #Number of days in a standard year on the end of each month

print("initialize component has set")
print(Sys.time())

climateData <- numeric(120*720*360)
dim(climateData) <- c(720,360,120) #make vessel of claimte data

coumaps<- raster(nrow = 360, ncol = 720, xmn = -179.75, xmx = 179.75, ymn = -89.75, ymx = 89.75)
values(coumaps) <- 0

Ccalendar <- array(numeric(720*360*4), dim=(c(720,360,4)))

YdM <- numeric(12)

##########
#make Crop Calender from  month to year
########
Ccalendar[,,1] <- var.get.nc(cropkc,   "plant.start")
Ccalendar[,,2] <- var.get.nc(cropkc,   "plant.range")
Ccalendar[,,3] <- var.get.nc(cropkc, "harvest.start")
Ccalendar[,,4] <- var.get.nc(cropkc, "harvest.range")
Ccalendar[,,3] <- Ccalendar[,,3]+(sign(Ccalendar[,,1]-Ccalendar[,,3])==1)*365


CCalW <- array(numeric(720*360*12),dim=(c(720,360,12)))
for (longit in 1:720){
    for (latit in 1:360){
        if(is.na(sum(Ccalendar[longit,latit,1:4])) == FALSE){
            YdCal<-numeric(1095) #365*3

            for(i in  Ccalendar[longit,latit,1]:(Ccalendar[longit,latit,1]+Ccalendar[longit,latit,2]-1)){
                YdCal[i]<-(i-Ccalendar[longit,latit,1])/Ccalendar[longit,latit,2]
            }

            for(i in (Ccalendar[longit,latit,1]+Ccalendar[longit,latit,2]):1095){
                YdCal[i]<-1
            }

            for(i in (Ccalendar[longit,latit,3]+Ccalendar[longit,latit,4]):1095){
                YdCal[i]<-0
            }

            for(i in  Ccalendar[longit,latit,3]:(Ccalendar[longit,latit,3]+Ccalendar[longit,latit,4]-1)){
                YdCal[i]<-YdCal[i]-(i-Ccalendar[longit,latit,3])/Ccalendar[longit,latit,4]
            }

            YdM<-numeric(12)

            for(i in 1:12){
                YdM[i]<-sum(YdCal[     EndOfMonth[i]+1 :     EndOfMonth[i+1] ])+
                        sum(YdCal[(365+EndOfMonth[i]+1):(365+EndOfMonth[i+1])])+
                        sum(YdCal[(730+EndOfMonth[i]+1):(730+EndOfMonth[i+1])])
            }
            CCalW[longit,latit,]<-YdM/sum(YdM)
        }else{
            CCalW[longit,latit,]<-rep(NA,12)
        }
    }
    if(longit%%120==0)print(paste0("now ",longit%/%120,"/6 has ended"))
}

save.image(file = paste0(OutputFolder, "/crop_calendar.",crop.ordered,".RData"))
