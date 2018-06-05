require(countrycode)

order2<-"Maize"

croplist <- c("Barley","Barley.Winter","Cassava","Cotton","Groundnuts","Maize.2",
                "Maize","Millet","Oats","Oats.Winter","Potatoes","Pulses","Rapeseed.Winter",
                    "Rice.2","Rice","Rye.Winter","Sorghum.2","Sorghum","Soybeans","Sugarbeets",
                        "Sunflower","Sweet.Potatoes","Wheat","Wheat.Winter","Yams")
croplist_for_areacall <- c("barley","barley","cassava","cotton","groundnut","maize",
                            "maize","millet","oats","oats","potato","pulsenes","rapeseed","rice",
                                "rice","rye","sorghum","sorghum","bean","sugarbeet",
                                    "sunflower","sweetpotato","wheat","wheat","yam")

##########
#set up files
#########
countrylis.csv <- read.csv("../produced_data/csv/countrylis.csv")

tmp <- read.csv(paste0("../produced_data/csv/climateData_", order2 ,"_for_tmp.csv"))
pre <- read.csv(paste0("../produced_data/csv/climateData_", order2 ,"_for_pre.csv"))

faodata <- read.csv("../rawdata/FAO_2016/FAOSTAT_data.csv")
faodata <- faodata[faodata[,"Item"] == order2,]
faodata <- faodata[,c("Area.Code", "Area", "Element", "Year", "Value")]

###########
#merge&reshape
###########
CD <- merge(tmp, pre, by.x = c("year", "COWN"), by.y = c("year", "COWN"))
names(CD) <- c("year", "connum", "tmp", "pre")
CD <- cbind(CD, countrylis.csv[CD[,"connum"],c("cown","faon","country.name")])

iso3c <- countrycode(CD$country.name, "country.name", "iso3c")
CD <- cbind(CD, iso3c)
rm(iso3c)

faodata <- reshape(faodata, timevar = "Element", idvar = names(faodata)[!names(faodata)%in%c("Element","Value")],
                    direction = "wide")
faodata <- subset(faodata, as.numeric(faodata$Area.Code) <5000) #we do not need world sum

#descriminate if area code ofFAO data is "iso3c" or "fao's original area code"
#if 90% of area code is 3 letters, code is recognized as "iso3c"
nchar.table <- table(nchar(as.character(faodata$Area.Code)))
fao_code_type <- nchar.table["3"] > 0.9
faocode <- c("faon", "iso3c")[1 + fao_code_type]

#if iso3c is adopted, Russian federation is not sinchronized with USSR
#this will be solved by individual input?

ClimateFAO <- merge(CD, faodata, by.x = c("year", faocode), by.y = c("Year","Area.Code"), all=T)
CRUFAO <- na.omit(ClimateFAO)

#export merged data
write.csv(ClimateFAO, "../produced_data/csv/merged_CRU_FAO_full.csv")
write.csv(CRUFAO, "../produced_data/csv/merged_CRU_FAO.csv")
