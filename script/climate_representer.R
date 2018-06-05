require(RNetCDF)
require(cshapes)
require(rgl)
require(raster)
require(sf)

#################
#make representive term each countries
#write .csv file
#################

#################
#settings
#################
load(paste0("../produced_data/Rdata/crop_calendar.", crop.ordered, ".RData"))

#################
#convert Monfreda_et_al_2008 data into 720*360 data
#################
cropareaNC.Lv1 <- var.get.nc(cropareaNC, paste0(order2, "Data"))[,,1]
croparea <- matrix(rep(NA,360*720),nrow=720)

for(i in 1:720){
    for(j in 1:360){
        cropareaNC.Lv1.r <- cropareaNC.Lv1[(6*i-5):(6*i-5+5),(6*j-5):(6*j-5+5)]
        croparea[i,j] <- ifelse(sum(!is.na(cropareaNC.Lv1.r)) > 17, sum(cropareaNC.Lv1.r, na.rm = TRUE)/sum(!is.na(cropareaNC.Lv1.r)), NA)
    }
    if(i%%72==0) print(paste(i%/%7.2,"% has finished"))
}


##################
#calc representive of cliamte factor on each countries
##################

yearlength <- length(yearlist)
print(Sys.time())
for(l in 1:length(climateterm)){
    climateGD <- data.frame(year = rep(yearlist, each = countrylength), connum = rep(1:countrylength, times = yearlength),
                            value = rep(NA, times = countrylength * yearlength))
    climateData<-rep(NA,720*360*120)

    climateData.nc <- open.nc(paste0(CRUpass, CRUver,".", as.character(1 + min(yearlist-1) %/%10*10),
            ".", as.character(10 + min(yearlist-1) %/%10*10), ".", climateterm[l], ".dat.nc"))
    climateData <- var.get.nc(climateData.nc, climateterm[l])

    for(yearc in yearlist){
        if(yearc%%10 == 1){
            if(yearc%/%10 <= 200){
                climateData.nc <- open.nc(paste0(CRUpass, CRUver,".", as.character(1+yearc%/%10*10),
                    ".", as.character(10+yearc%/%10*10), ".", climateterm[l], ".dat.nc"))
                climateData <- var.get.nc(climateData.nc, climateterm[l])
            }else{
                climateData.nc <- open.nc(paste0(CRUpass, CRUver,".2011.2015.", climateterm[l], ".dat.nc"))
                climateData <- var.get.nc(climateData.nc,climateterm[l])
            }
        }

        worldmaps <- cshp(date = as.Date(paste0(as.character(yearc),"-1-1")), useGW = FALSE)
        for(connum in 1:countrylength){
            values(coumaps) <- 0

            if(length(worldmaps[worldmaps$COWCODE == countrylis[connum],]$"FEATUREID") != 0){
                coumaps <- shp2raster(worldmaps[worldmaps$COWCODE == countrylis[connum],], coumaps, label="", value = 1, map = FALSE)
            }

            CountryExistPoint <- which(coumaps[,]==1)

            if(length(CountryExistPoint) > 0){
                CountryCoordinate <- matrix(0, ncol=4, nrow=length(CountryExistPoint))
                CountryCoordinate[,1] <- (CountryExistPoint-1)%%720+1
                CountryCoordinate[,2] <- (CountryExistPoint-1)%/%720+1
                for(i in 1:length(CountryExistPoint)){
                    CountryCoordinate[i,3] <- CCalW[CountryCoordinate[i,1],CountryCoordinate[i,2],]%*%
                        climateData[CountryCoordinate[i,1]+(360-CountryCoordinate[i,2])*720+
                            ((yearc-min(yearlist))%%10*12):((yearc-min(yearlist))%%10*12+11)*720*360]
                }

                for(i in 1:length(CountryExistPoint)){
                    CountryCoordinate[i,4] <- croparea[CountryCoordinate[i,1],CountryCoordinate[i,2]]*
                                                cos(abs(180.5-CountryCoordinate[i,2])*pi/360)
                }

                Value <- na.omit(CountryCoordinate)

                if(class(Value)=="matrix"){
                    climateGD[(yearc-min(yearlist))*countrylength+connum,3] <- (Value[,3]%*%Value[,4])/sum(Value[,4])
                }else{
                    climateGD[(yearc-min(yearlist))*countrylength+connum,3] <- (Value[3]%*%Value[4])/sum(Value[4])
                }

            }else{
                for(i in 1:25){
                    climateGD[(yearc-min(yearlist))*countrylength+connum+i,3] <- NA
                }
            }
        }
        print(paste(climateterm[l], as.character(yearc), "done! at", Sys.time()))
    }

    iso3c <- countrycode(countrylis[climateGD$connum], "cown", "iso3c")
    country.name <- countrycode(countrylis[climateGD$connum], "cown", "country.name")

    climateGD <- cbind(climateGD, iso3c, country.name)
    rm(iso3c)
    rm(country.name)

    write.csv(climateGD, paste0("../produced_data/csv/climateData_", order2 , "_for_", climateterm[l], ".csv"),
            quote = FALSE, row.names = FALSE)
}
