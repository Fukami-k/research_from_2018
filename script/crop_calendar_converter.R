require(RNetCDF)
require(cshapes)
require(rgl)
require(sf)
require(raster)

shp2raster <- function(shp, mask.raster, label, value, transform = FALSE, proj.from = NA,
                       proj.to = NA, map = TRUE) {
  require(raster, rgdal)

  # use transform==TRUE if the polygon is not in the same coordinate system as
  # the output raster, setting proj.from & proj.to to the appropriate
  # projections
  if (transform == TRUE) {
    proj4string(shp) <- proj.from
    shp <- spTransform(shp, proj.to)
  }

  # convert the shapefile to a raster based on a standardised background
  # raster
  r <- rasterize(shp, mask.raster)
  # set the cells associated with the shapfile to the specified value
  r[!is.na(r)] <- value
  # merge the new raster with the mask raster and export to the working
  # directory as a tif file
  r <- mask(merge(r, mask.raster), mask.raster, filename = label, format = "GTiff",
            overwrite = T)

  # plot map of new raster
  if (map == TRUE) {
    plot(r, main = label, axes = F, box = F)
  }

  names(r) <- label
  return(r)
  }

##################
#coded following
#################

############
#settings
############

#write climate terms to convert
climateterm<-c("tmp","pre","cld")


croplist<-c("Barley","Barley.Winter","Cassava","Cotton","Groundnuts","Maize.2",
            "Maize","Millet","Oats","Oats.Winter","Potatoes","Pulses",
            "Rapeseed.Winter","Rice.2","Rice","Rye.Winter","Sorghum.2",
            "Sorghum","Soybeans","Sugarbeets","Sunflower","Sweet.Potatoes",
            "Wheat","Wheat.Winter","Yams")

crop.ordered<-croplist[7] #maize
print(paste(crop.ordered, "is set"))

yearlist<-1961:2015
yearlength<-length(yearlist)

#######
#initialize conponets
#####

#give the end of month
EndOfMonth<-c(0,31,59,90,120,151,181,212,243,273,304,334,365) #Number of days in a standard year on the end of each month

print("initialize component has set")
print(Sys.time())
#make list of countries
countrylis<-matrix(numeric(yearlength*250),nrow=yearlength,ncol=250) #first make empty matrix
for(i in yearlist){
    #list all countries that have .shp file
    cshp(date=as.Date(paste0(as.character(i),"-1-1")),useGW=FALSE)$COWCODE->
    countrylis[i-1960,1:length(cshp(date=as.Date(paste0(as.character(i),"-1-1")),
    useGW=FALSE)$COWCODE)]
}
countrylis<-as.integer(names(table(countrylis)))[-1]
coutrylength<-length(countrylis)

print("all countries in .shp are listed")
print(Sys.time())

#set len.yearlist
climateData<-numeric(120*720*360)
dim(climateData)<-c(720,360,120) #make vessel of claimte data

coumaps<- raster(nrow = 360, ncol = 720, xmn = -179.75, xmx = 179.75, ymn = -89.75, ymx = 89.75)
values(coumaps)<-0

Ccalendar<-array(numeric(720*360*4),dim=(c(720,360,4)))

YdM<-numeric(12)

##########
#make Crop Calender from  month to year
########

cropkc<-open.nc(paste0("../rawdata/Sacks_et_al_2010/",crop.ordered,".crop.calendar.fill.nc"))
Ccalendar[,,1]<-var.get.nc(cropkc,  "plant.start")
Ccalendar[,,2]<-var.get.nc(cropkc,  "plant.range")
Ccalendar[,,3]<-var.get.nc(cropkc,"harvest.start")
Ccalendar[,,4]<-var.get.nc(cropkc,"harvest.range")
Ccalendar[,,3]<-Ccalendar[,,3]+(sign(Ccalendar[,,1]-Ccalendar[,,3])==1)*365


CCalW<-array(numeric(720*360*12),dim=(c(720,360,12)))
for (longit in 1:720){
    for (latit in 1:360){
        if(is.na(sum(Ccalendar[longit,latit,1:4])) == FALSE){
            YdCal<-numeric(1095)

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

save.image(file = paste0("../processed_data/RData/crop_calendar.",crop.ordered,".RData"))
