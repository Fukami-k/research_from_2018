##############
#This code is to produce .pdf s
#############

producepdfs <- function (main = "plot", height = 8.9/2.54, width = 8.9/2.54, unit = "inch", ...){
    if(width == "double" | width == "wide"  ) width <- 18.3/2.54
    if(width == "single" | width == "narrow") width <-  8.9/2.54

    if(unit == "cm"){
        height <- height /2.54
        width  <- width  /2.54
    }

    pdf(paste0(title, ".pdf"), height = height, width = width, ... )
}

producetiffs <- function (main = "plot", height = 8.9, width = 18.3, ...){
    if(width == "double" | width == "wide"  ) width <- 18.3
    if(width == "single" | width == "narrow") width <-  8.9
    tiff(paste0(title, ".pdf"), height = height, width = width, pointsize = 9, res = 500, unit = "cm", ...)
}
