require(countrycode)
require(ggmap)
require(RColorBrewer)
require(scales) #扱いに気をつけるべし？

CRUFAO <- read.csv("../produced_data/csv/merged_CRU_FAO.csv")

###################
#追試
#0. データセット整形
#year, tmp, pre, iso, group, Value.Yield, Predict
###################
#年代による選別
CRUFAO.8008 <- CRUFAO[CRUFAO$year<=2008 & CRUFAO$year>=1980,]

#自己回帰係数計算のため、11年未満の国データを削除
country.8008 <- tapply(!is.na(CRUFAO.8008$Value.Yield), CRUFAO.8008$country.name, sum)
countryname.8008 <- names(country.8008)[country.8008>=11 & !is.na(country.8008)]

#10^4 ha threshold line #country which is not exceed 10^4ha is removed
country.area.8008 <- tapply(CRUFAO.8008$Value.Area.harvested, CRUFAO.8008$country.name, max)
countryname.area.8008 <- names(country.area.8008[country.area.8008>=10^4 & !is.na(country.area.8008)])
countryname.8008 <- intersect(countryname.8008, countryname.area.8008)

#連続して同じ値を出したのがいくつあったか
check.seqdata <- function(country.name, data = CRUFAO.8008){
    sample.data <- data[data$country.name == country.name,]
    if(nrow(sample.data) == 0) return(NA)
    seq.counter <- 0
    for(i in 1:nrow(sample.data)){
        year.remark <- sample.data[i, "year"]
        if((year.remark-1) %in% sample.data$year){
            if(sample.data[sample.data$year == year.remark, "Value.Yield"] ==
                sample.data[sample.data$year == (year.remark-1), "Value.Yield"]){
                    seq.counter <- seq.counter+1
            }
        }
    }
    return(seq.counter)
}
#連続して同じYieldが無いように選別
country.seq.8008 <- sapply(names(table(CRUFAO.8008$country.name)), check.seqdata)
countryname.seq.8008 <- names(country.seq.8008[country.seq.8008 <= 1])
countryname.8008 <- intersect(countryname.8008, countryname.seq.8008)

#元データへの反映
countryiso.8008 <- countrycode(countryname.8008, "country.name", "iso3c")
CRUFAO2.8008 <- CRUFAO.8008[CRUFAO.8008$country.name %in% countryname.8008,] #this was CRUFAO.8008[CRUFAO$AreaNameFull%in%countryname.8008,] till 180123
CRUFAO2.8008 <- data.frame(CRUFAO2.8008, iso3c = countrycode(CRUFAO2.8008$country.name, "country.name", "iso3c"))
CRUFAO2.8008 <- CRUFAO2.8008[!is.na(CRUFAO2.8008$Value.Yield),]

#############
#data selection by self regression
#自己回帰係数によるデータ選別
#############
acf_test_10 <- function(iso3c, data = CRUFAO2.8008, lag = 10){
    data <- data[data$iso3c==iso3c,]
    lm.model0 <- lm(log(Value.Yield) ~ year + I(year^2), data = data)
    x <- lm.model0$residual
    len.x <- length(x)
    cor.test(x[1:(len.x -10)],x[11:len.x], method = "pearson")$p.value
}

cor_p <- sapply(countryiso.8008, acf_test_10)

acf_test_10l <- function(iso3c, data = CRUFAO2.8008, lag = 10){
    data <- data[data$iso3c==iso3c,]
    lm.model0 <- lm(log(Value.Yield) ~ year , data = data)
    x <- lm.model0$residual
    len.x <- length(x)
    cor.test(x[1:(len.x -10)],x[11:len.x], method = "pearson")$p.value
}

cor_pl <- sapply(countryiso.8008, acf_test_10l)

plot(log10(data.frame(cor_p,cor_pl)),type="n");text(log10(data.frame(cor_p,cor_pl)),countryiso.8008)
abline(h=log10(0.05),col=2)
abline(v=log10(0.05),col=2)

country.p2 <- names(cor_pl[cor_pl>0.05])
CRUFAO3.8008 <- CRUFAO2.8008[CRUFAO2.8008$iso3c %in% country.p2,]



#grouping by yield
mean.yield<-tapply(CRUFAO3.8008$Value.Yield,CRUFAO3.8008$iso3c,mean)
len.mean.yield <- table(is.na(mean.yield))[1]

groupnum <- 4
class.yield <- rep(LETTERS[1:groupnum],
                   each = ceiling(len.mean.yield/4))[- (ceiling(len.mean.yield/4)*4 + 4 : (len.mean.yield+1))]

names(class.yield) <- names(sort(mean.yield))
CRUFAO3.8008 <- data.frame(CRUFAO3.8008, group = class.yield[as.character(CRUFAO3.8008$iso3c)])

##################
#1. 線形モデル作成
###################
lm.model1<-lm(log(Value.Yield)~iso3c+iso3c:year+iso3c:I(year^2)+
                  group:tmp+group:I(tmp^2)+
                  group:pre+group:I(pre^2),
              data=CRUFAO3.8008,na.action = na.omit)

lm.model2<-lm(log(Value.Yield)~iso3c+iso3c:year+iso3c:I(year^2)+
                  tmp+I(tmp^2)+
                  pre+I(pre^2),
              data=CRUFAO3.8008,na.action = na.omit)

#################
#2.Predict 併合
#################
CRUFAO3.8008.p<-data.frame(CRUFAO3.8008,predict=exp(lm.model1$fitted.values))


#what i should do is...... at 180619
#1. make fun : vec -> lm(vec)$a
TrendGrad <- function(x){
    t <- 1:length(x)
    return(lm(x~t)$coefficients[2])
}

#2. detrend tmp & pre then bind with CRUFAO
tmpgradiso <- tapply(CRUFAO3.8008.p$tmp, CRUFAO3.8008.p$iso3c, TrendGrad)
pregradiso <- tapply(CRUFAO3.8008.p$pre, CRUFAO3.8008.p$iso3c, TrendGrad)
CRUFAO3.8008.p$tmpd <- CRUFAO3.8008.p$tmp - (CRUFAO3.8008.p$year - 1980) * tmpgradiso[CRUFAO3.8008.p$iso3c]
CRUFAO3.8008.p$pred <- CRUFAO3.8008.p$pre - (CRUFAO3.8008.p$year - 1980) * pregradiso[CRUFAO3.8008.p$iso3c]

#3. calc ii iii iv
predict_Td_P  <-  exp(with(CRUFAO3.8008.p, predict(lm.model1, newdata = data.frame(iso3c = iso3c, year =  year, group =group, tmp = tmpd, pre = pre))))
predict_T_Pd  <-  exp(with(CRUFAO3.8008.p, predict(lm.model1, newdata = data.frame(iso3c = iso3c, year =  year, group =group, tmp = tmp, pre = pred))))
predict_Td_Pd <-  exp(with(CRUFAO3.8008.p, predict(lm.model1, newdata = data.frame(iso3c = iso3c, year =  year, group =group, tmp = tmpd, pre = pred))))

#4. calc ii-1 iii-1 iv-1
ii_i  <- predict_Td_P - CRUFAO3.8008.p$predict
iii_i <- predict_T_Pd - CRUFAO3.8008.p$predict
iv_i  <- predict_Td_Pd - CRUFAO3.8008.p$predict

#5. calc trend of above with fun
trendT  <- tapply(ii_i,  CRUFAO3.8008.p$iso3c, TrendGrad)
trendP  <- tapply(iii_i, CRUFAO3.8008.p$iso3c, TrendGrad)
trendTP <- tapply(iv_i,  CRUFAO3.8008.p$iso3c, TrendGrad)
trendY  <- tapply(predict_Td_Pd, CRUFAO3.8008.p$iso3c, TrendGrad)
trendTPdY <- trendTP / trendY

#6. let them into world map
colscaleN <- colorRampPalette(c("#EE2020", "#EC5040", "#EA9078", "#E9C0B0", "#E5D0C5"))
colscaleP <- colorRampPalette(c("#C5D0E8", "#B0C0E0", "#7A87C0", "#4B60A6", "#3355A0"))
world <- map_data("world")
world$trendTPdY<-sapply(world$region,function(x)trendTPdY[ifelse(x%in%c("China","UK"),c("CHN","GBR")[(x=="UK")+1],iso3166[x==iso3166$mapname,"a3"])])

Manual_Scale <- function(x){
    if(is.na(x)){
        return("NA")
    }else if(x < -0.5){
        return("0")
    }else if(x < -0.4){
        return("1")
    }else if(x < -0.3){
        return("2")
    }else if(x < -0.2){
        return("3")
    }else if(x < -0.1){
        return("4")
    }else if(x < 0){
        return("5")
    }else if(x < 0.1){
        return("6")
    }else if(x < 0.2){
        return("7")
    }else if(x < 0.3){
        return("8")
    }else if(x < 0.4){
        return("9")
    }else if(x < 0.5){
        return("A")
    }else{
        return("B")
    }
}

cols <- c("#FF00FF",colscaleN(5),colscaleP(5),"#00FF00","gray")
names(cols) <- c(0:9,"A","B","NA")

world$trendTPdY_M <- sapply(world$trendTPdY, Manual_Scale)

world_base <- ggplot(data = world, mapping = aes(x = long, y = lat, group = group)) + geom_polygon(color = "gray", fill = "black")
world_base +
    geom_polygon(data = world, aes(fill = trendTPdY_M), color = "black",lwd=0.1) +
    geom_polygon(color = "black", fill = NA, lwd=0.1) +
#    scale_fill_gradientn(colours = c("#FF00FF",colscaleN(5),colscaleP(5),"#00FF00"), breaks = c(-1,seq(-0.5,0.5,by=0.1),1),
#                         values =rescale(c(-1,seq(-0.5,0.5,by=0.1),1), to = c(0, 1), from = range(world$trendTPdY, na.rm = TRUE)),
#                         guide = guide_colourbar(title = "[?]",title.position = "bottom")) +
    scale_fill_manual(values = cols, guide = guide_legend(title = "[?]",title.position = "bottom"))+

    coord_fixed(xlim = c(-180, 180),  ylim = c(-75, 75))+
    scale_x_continuous(name="",breaks=seq(-180,180,by=30),
                       labels = c("180°W","150°W","120°W","90°W","60°W","30°W","0°","30°E","60°E","90°E","120°E","150°E","180°E"))+
    scale_y_continuous(name="",breaks=seq(-90,90,by=30),
                       labels = c("90°S","60°S","30°S","0°","30°N","60°N","90°N"))+
    theme(line = element_line(size=0.25),
          text = element_text(size=5),
          plot.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"),
          legend.position = "right",
          legend.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"))

#################
#3.matplot, layout 作成
##################

pdf("Tsuishi01_180417.pdf")
#parameter
layout.rat<-matrix(c(3,1,1),nrow = 3)
par(mar=c(1.5,2,1,0.5))

for(i in 1:length(country.p2)){
    country<-country.p2[i]
    data<-CRUFAO3.8008.p[CRUFAO3.8008.p$iso3c==country,]

    #predict2
    eff.country0<-lm.model1$coefficients[paste0("iso3c",country)]+lm.model1$coefficients["(Intercept)"]
    eff.country1<-lm.model1$coefficients[paste0("iso3c",country,":year")]
    eff.country2<-lm.model1$coefficients[paste0("iso3c",country,":I(year^2)")]
    eff.cliT1<-lm.model1$coefficients[paste0("group",data$group[1],":tmp")]
    eff.cliT2<-lm.model1$coefficients[paste0("group",data$group[1],":I(tmp^2)")]
    eff.cliP1<-lm.model1$coefficients[paste0("group",data$group[1],":pre")]
    eff.cliP2<-lm.model1$coefficients[paste0("group",data$group[1],":I(pre^2)")]

    if(i==1)eff.country0<-lm.model1$coefficients["(Intercept)"]
    print(c(eff.country0,eff.country1,eff.country2,eff.cliT1,eff.cliT2,eff.cliP1,eff.cliP2))

    predict2<-eff.country0+eff.country1*(min(data$year):max(data$year))+eff.country2*(min(data$year):max(data$year))^2+
        eff.cliT1*median(data$tmp)+eff.cliT2*median(data$tmp)^2+eff.cliP1*median(data$pre)+eff.cliP2*median(data$pre)^2

    layout(1:3,height=layout.rat)
    #first layer
    matplot(data$year,cbind(data$predict,data$Value.Yield),col=c(2,1),
            type=c("b","p"),pch=c(1,2),main=paste(countrycode(country,"iso3c","country.name"),"group",data$group[1]))
    points((min(data$year):max(data$year)),exp(predict2),col=5,type="b",pch=3)
    #legend()
    text(par("usr")[2]*0.9+par("usr")[1]*0.1,par("usr")[3]*0.9+par("usr")[4]*0.1,
         paste("Box.test's p.value is",round(Box.test(data$Value.Yield-data$predict,lag=10)$p.value,3)))

    text(par("usr")[2]*0.8+par("usr")[1]*0.2,par("usr")[3]*0.8+par("usr")[4]*0.2,
         paste("Value.Area.harvested is mean",signif(mean(data$Value.Area.harvest),3)))

    #second layer
    plot(data$tmp~data$year, type = "b", col=3)

    #3rd layer
    plot(data$pre~data$year, type="b", col=4)

    rm(data)
}

dev.off()

###############
#lobell fig4
###############
CRUFAO3.8008.p$iso3c
name.country<-names(mean.yield)[!is.na(mean.yield)]

trend.ax<-function(x)lm(x~I(1:length(x)))$coefficients[2]
trend.b<-function(x)lm(x~I(1:length(x)))$coefficients[2]+lm(x~I(1:length(x)))$coefficients[1]

ToTrace<-data.frame(iso3c=name.country,
group=class.yield[name.country],
year.1=lm.model1$coefficients[sapply(name.country,function(x)paste0("iso3c",x,":year"))],
year.2=lm.model1$coefficients[sapply(name.country,function(x)paste0("iso3c",x,":I(year^2)"))],
tmp.1=lm.model1$coefficients[sapply(class.yield[name.country],function(x)paste0("group",x,":tmp"))],
tmp.2=lm.model1$coefficients[sapply(class.yield[name.country],function(x)paste0("group",x,":I(tmp^2)"))],
pre.1=lm.model1$coefficients[sapply(class.yield[name.country],function(x)paste0("group",x,":pre"))],
pre.2=lm.model1$coefficients[sapply(class.yield[name.country],function(x)paste0("group",x,":I(pre^2)"))],
trend.tmp.ax=tapply(CRUFAO3.8008$tmp,CRUFAO3.8008$iso3c,trend.ax)[name.country],
trend.tmp.b=tapply(CRUFAO3.8008$tmp,CRUFAO3.8008$iso3c,trend.b)[name.country],
trend.pre.ax=tapply(CRUFAO3.8008$pre,CRUFAO3.8008$iso3c,trend.ax)[name.country],
trend.pre.b=tapply(CRUFAO3.8008$pre,CRUFAO3.8008$iso3c,trend.b)[name.country])
D.year<-ToTrace$year.1*(2008-1981)+ToTrace$year.2*(2008^2-1981^2)
D.tmp<-ToTrace$tmp.1*(2008-1981)*ToTrace$trend.tmp.ax+
    ToTrace$tmp.2*((ToTrace$trend.tmp.b+(2008-1981)*ToTrace$trend.tmp.ax)^2-(ToTrace$trend.tmp.b^2))
D.pre<-ToTrace$pre.1*(2008-1981)*ToTrace$trend.pre.ax+
    ToTrace$pre.2*((ToTrace$trend.pre.b+(2008-1981)*ToTrace$trend.pre.ax)^2-(ToTrace$trend.pre.b^2))


plot(CRUFAO3.8008.p[CRUFAO3.8008.p$iso3c=="TKM","Value.Yield"],type="b")
countrycode("TKM","iso3c","country.name")
points(CRUFAO3.8008.p[CRUFAO3.8008.p$iso3c=="TKM","predict"],col=2)

ToTrace$DtDy<-(exp(D.tmp)-1)/(exp(D.year)-1)
ToTrace$DpDy<-(exp(D.pre)-1)/(exp(D.year)-1)
ToTrace$Dyt<-(exp(D.tmp+D.pre)-1)/(exp(D.year)-1)

ToTrace$Dyt[abs(ToTrace$Dyt)>5]<-NA #下に凸の国って..
Dyt2<-ToTrace$Dyt
names(Dyt2)<-ToTrace$iso3c

world <- map_data("world")
world$Dyt2<-sapply(world$region,function(x)Dyt2[ifelse(x%in%c("China","UK"),c("CHN","GBR")[(x=="UK")+1],iso3166[x==iso3166$mapname,"a3"])])
world_base <- ggplot(data = world, mapping = aes(x = long, y = lat, group = group)) + geom_polygon(color = "gray", fill = "black")

pdf("180417fig6L2.pdf",width=18.0/2.54,height = 9.0/2.54, pointsize=10) #make theme(text=element(size=10))
world_base +
    geom_polygon(data = world, aes(fill = Dyt2), color = "black",lwd=0.1) +
    geom_polygon(color = "black", fill = NA, lwd=0.1) +
    scale_fill_gradientn(colours = c("#FFFFFF",brewer.pal(10, "RdYlBu"),"#00FF00"), breaks = c(-2,seq(-0.5,0.5,by=0.1),2),
                         values =rescale(c(-2,seq(-0.5,0.5,by=0.1),2), to = c(0, 1), from = range(world$Dyt2, na.rm = TRUE)),
                         guide = guide_colourbar(title = "[?]",title.position = "bottom")) +
    coord_fixed(xlim = c(-180, 180),  ylim = c(-75, 75))+
    scale_x_continuous(name="",breaks=seq(-180,180,by=30),
                       labels = c("180°W","150°W","120°W","90°W","60°W","30°W","0°","30°E","60°E","90°E","120°E","150°E","180°E"))+
    scale_y_continuous(name="",breaks=seq(-90,90,by=30),
                       labels = c("90°S","60°S","30°S","0°","30°N","60°N","90°N"))+
    theme(line = element_line(size=0.25),
          text = element_text(size=5),
          plot.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"),
          legend.position = "right",
          legend.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"))
dev.off()

#############
#data selection by self regression
#自己回帰係数によるデータ選別(旧)
#############
#check self regression #parabora 10
Box.p <- function(iso3c, data = CRUFAO2.8008){
    data <- data[data$iso3c==iso3c,]
    lm.model0 <- lm(log(Value.Yield) ~ year + I(year^2), data = data)
    return(Box.test(lm.model0$residuals, lag = 10)$p.value)
}
country.p <- sapply(countryiso.8008, Box.p)
country.p2 <- names(country.p[country.p >= 0.05])

#check self regression #linear
Box.pl <- function(iso3c, data = CRUFAO2.8008){
    data<-data[data$iso3c==iso3c,]
    lm.model0<-lm(log(Value.Yield)~year,data = data)
    return(Box.test(lm.model0$residuals,lag=10)$p.value)
}
country.pl<-sapply(countryiso.8008,Box.pl)
country.p2l<-names(country.p[country.p>=0.05])
#plot self regression
plot(log10(data.frame(country.p,country.pl)),type="n");text(log10(data.frame(country.p,country.pl)),countryiso.8008)
abline(h=log10(0.05),col=2)
abline(v=log10(0.05),col=2)

#check self regression #parabora 1
Box.p.1<-function(iso3c,data=CRUFAO2.8008){
    data<-data[data$iso3c==iso3c,]
    lm.model0<-lm(log(Value.Yield)~year+I(year^2),data = data)
    return(Box.test(lm.model0$residuals,lag=1)$p.value)
}
country.p.1<-sapply(countryiso.8008, Box.p.1)
country.p2.1<-names(country.p[country.p>=0.05])
plot(log10(data.frame(country.p,country.p.1)),type="n");text(log10(data.frame(country.p,country.p.1)),countryiso.8008)
abline(h=log10(0.05),col=2)
abline(v=log10(0.05),col=2)

#check self regression #parabora 5
Box.p.5<-function(iso3c,data=CRUFAO2.8008){
    data<-data[data$iso3c==iso3c,]
    lm.model0<-lm(log(Value.Yield)~year+I(year^2),data = data)
    return(Box.test(lm.model0$residuals,lag=5)$p.value)
}
country.p.5<-sapply(countryiso.8008,Box.p.5)
country.p2.5<-names(country.p[country.p>=0.05])
plot(log10(data.frame(country.p,country.p.5)),type="n");text(log10(data.frame(country.p,country.p.5)),countryiso.8008)
abline(h=log10(0.05),col=2)
abline(v=log10(0.05),col=2)

##############
#自己相関係数の分析
#なぜMEXはなくPRKはあるのか
##############
pdf("acfp.pdf")
acf.p <- function(iso3c, data = CRUFAO2.8008){
    data <- data[data$iso3c==iso3c,]
    lm.model0 <- lm(log(Value.Yield) ~ year + I(year^2), data = data)
    return(acf(lm.model0$residuals,plot=TRUE,main=iso3c)$acf[11])
}
country.p.acf <- sapply(countryiso.8008, acf.p)
country.p2.acf <- names(country.p.acf[country.p.acf >= 0.05])
dev.off()

pdf("acfpl.pdf")
acf.pl <- function(iso3c, data = CRUFAO2.8008){
    data <- data[data$iso3c==iso3c,]
    lm.model0 <- lm(Value.Yield ~ year, data = data)
    return(acf(lm.model0$residuals,plot=TRUE,main=iso3c)$acf[11])
}
country.pl.acf <- sapply(countryiso.8008, acf.pl)
country.p2l.acf <- names(country.pl.acf[country.pl.acf >= 0.05])
dev.off()

pdf("acfpl_exp.pdf")
acf.pl_exp <- function(iso3c, data = CRUFAO2.8008){
    data <- data[data$iso3c==iso3c,]
    lm.model0 <- lm(Value.Yield ~ year, data = data)
    return(acf(lm.model0$residuals,plot=TRUE,main=iso3c)$acf[11])
}
sapply(countryiso.8008, acf.pl_exp)
dev.off()


ite <- 10000
acf.mat <- matrix(numeric(ite*15),nrow=ite)
for(i in 1:ite) acf.mat[i,] <- acf(5*rnorm(29), plot=F)$acf
hist(acf.mat[,11],freq=F)
sort(acf.mat[,11])[249:251]
sort(acf.mat[,11])[9749:9751]

ite <- 10000
acf.mat <- matrix(numeric(ite*15),nrow=ite)
for(i in 1:ite) acf.mat[i,] <- acf(5*rnorm(29)+c(rep(-0.5,15),rep(0.5,14)), plot=F)$acf
hist(acf.mat[,11],freq=F)
sort(acf.mat[,11])[249:251]
sort(acf.mat[,11])[9749:9751]
