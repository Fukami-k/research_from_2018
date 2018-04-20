require("mgcv")
require("RColorBrewer")
require("maps")
require("countrycode")
parset<-par()

#fig1 加法モデル気象効果
tiff("fig1.tiff",height=7.5,width=15,pointsize=9,res=500,unit="cm")
	par(mfrow=c(1,2))
	par(mar=c(5,4,4,2))
	par(cex=0.5)
	
	plot(rnorm(2),xlim=c(8,32),ylim=c(0,550),type="n")
	#実際にプロットをして、点を入れる
	vis.gam(gam.model7,view=c("tmpall","preall"),plot.type="contour",color="cm",
	        main="",xlab="栽培期間　平均気温 [℃]",ylab="栽培期間　月平均降水量 [mm]",
            add=T)
	points(CRUFAOY2$tmpall,CRUFAOY2$preall,cex=0.1)

	##########################
	#領域外プロット、凡例
	#########################
    #######
	#凡例位置[位置は%単位] パラメータ
    #######
	NumColors<-5
	
	leftpercent<-35
	rightprecent<-80
	top<-10
	bottom<-5
	
	fontcex<-1
    #ここまでパラメータ
	#######
	
	par(xpd=TRUE)
	usr<-par("usr")
	leftp<-leftpercent/100
	widthp<-(rightprecent-leftpercent)/100/NumColors
	
	#色付き長方形
	for(i in 1:NumColors){
        rect(usr[1]+(usr[2]-usr[1])*(leftp+widthp*(i-1)),usr[4]+(usr[4]-usr[3])*top/100,
             usr[1]+(usr[2]-usr[1])*(leftp+widthp*i),usr[4]+(usr[4]-usr[3])*bottom/100,
             col=cm.colors(NumColors)[i],lty=0)
	}
	rect(usr[1]+(usr[2]-usr[1])*leftp,usr[4]+(usr[4]-usr[3])*bottom/100,
	     usr[1]+(usr[2]-usr[1])*rightprecent/100,usr[4]+(usr[4]-usr[3])*top/100,border="gray")

	#日本語を入れる
	text(usr[1]+(usr[2]-usr[1])*(leftp-widthp*0.75),usr[4]+(usr[4]-usr[3])*(top+bottom)/2/100,"収量減",cex=fontcex)
	text(usr[1]+(usr[2]-usr[1])*(rightprecent/100+widthp*0.75),usr[4]+(usr[4]-usr[3])*(top+bottom)/2/100,"収量増",cex=fontcex)

	#後処理
	rm(usr)
	#par()<-parset

    ##############
    #ここからだいず
    ###############

    par(mar=c(5,4,4,2))
    par(cex=0.5)

    #実際にプロットをして、点を入れる
    vis.gam(gam.model8,view=c("tmpall","preall"),plot.type="contour",color="cm",
            #	        main="",xlab="Average Temperture during Cultivating [cerucius degree]",ylab="Avrage Precipitaion during Cultivating [mm]")
            main="",xlab="栽培期間　平均気温 [℃]",ylab="栽培期間　月平均降水量 [mm]",
            xlim=c(8,32),ylim=c(0,550))
    points(CRUFAOY2$tmpall,CRUFAOY2$preall,cex=0.1)

    ##########################
    #領域外プロット、凡例
    #########################
    #######
    #凡例位置[位置は%単位] パラメータ
    #######
    NumColors<-5

    leftpercent<-35
    rightprecent<-80
    top<-10
    bottom<-5

    fontcex<-1
    #ここまでパラメータ
    #######

    par(xpd=TRUE)
    usr<-par("usr")
    leftp<-leftpercent/100
    widthp<-(rightprecent-leftpercent)/100/NumColors

    #色付き長方形
    for(i in 1:NumColors){
        rect(usr[1]+(usr[2]-usr[1])*(leftp+widthp*(i-1)),usr[4]+(usr[4]-usr[3])*top/100,
             usr[1]+(usr[2]-usr[1])*(leftp+widthp*i),usr[4]+(usr[4]-usr[3])*bottom/100,
             col=cm.colors(NumColors)[i],lty=0)
    }
    rect(usr[1]+(usr[2]-usr[1])*leftp,usr[4]+(usr[4]-usr[3])*bottom/100,
         usr[1]+(usr[2]-usr[1])*rightprecent/100,usr[4]+(usr[4]-usr[3])*top/100,border="gray")
    
    #日本語を入れる
    text(usr[1]+(usr[2]-usr[1])*(leftp-widthp*0.75),usr[4]+(usr[4]-usr[3])*(top+bottom)/2/100,"収量減",cex=fontcex)
    text(usr[1]+(usr[2]-usr[1])*(rightprecent/100+widthp*0.75),usr[4]+(usr[4]-usr[3])*(top+bottom)/2/100,"収量増",cex=fontcex)

    #後処理
    rm(usr)
    par<-parset
dev.off()

as.matrix(as.data.frame(reshape(MaizePrid,timevar = "Area",idvar= "Year", direction = "wide")[,-1]))->SampleMat
SampleMat<-matrix(as.numeric(SampleMat),nrow = dim(SampleMat)[1])
colnames(SampleMat)<-substring(names(reshape(MaizePrid,timevar = "Area",idvar= "Year", direction = "wide")[,-1]),9,11)
rownames(SampleMat)<-reshape(MaizePrid,timevar = "Area",idvar= "Year", direction = "wide")[,1]
SampleMat<-exp(SampleMat)
#SampleMat<-t(na.omit(t(SampleMat)))



#fig2 弾性率世界地図
tiff("fig3.tiff",height=13,width=23,pointsize=9,res=500,unit="cm")
    par(mfrow=c(2,2))
    par(mar=c(1,0.5,1,0.5))
    par(cex=0.6)
    maincex=0.8
    
    colbase<-brewer.pal(8,"BuPu")[sapply(ceiling(D87/0.25),min,8)]
    plotWorldISO3c(MG1980_1970$iso,colbase,colset = "BuPu",col.num = 8,main="(Y1980/Y1970)/(GDP1980/GDP1970)",
                   col.legend=c("NA",as.character(seq(0.25,by=0.25,length=8)),">2"),maincex=maincex)

    colbase<-brewer.pal(8,"BuPu")[sapply(ceiling(D98/0.25),min,8)]
    plotWorldISO3c(MG1990_1980$iso,colbase,colset = "BuPu",col.num = 8,main="(Y1990/Y1980)/(GDP1990/GDP1980)",
                   col.legend=c("NA",as.character(seq(0.25,by=0.25,length=8)),">2"),maincex=maincex)

    colbase<-brewer.pal(8,"BuPu")[sapply(ceiling(D09/0.25),min,8)]
    plotWorldISO3c(MG2000_1990$iso,colbase,colset = "BuPu",col.num = 8,main="(Y2000/Y1990)/(GDP2000/GDP1990)",
                   col.legend=c("NA",as.character(seq(0.25,by=0.25,length=8)),">2"),maincex=maincex)

    colbase<-brewer.pal(8,"BuPu")[sapply(ceiling(D10/0.25),min,8)]
    plotWorldISO3c(MG2010_2000$iso,colbase,colset = "BuPu",col.num = 8,main="(Y2010/Y2000)/(GDP2010/GDP2000)",
                   col.legend=c("NA",as.character(seq(0.25,by=0.25,length=8)),">2"),maincex=maincex)
dev.off()

#fig3 弾性率プロット
tiff("fig4.tiff",height=20,width=15,pointsize=9,res=500,unit="cm")
	par(mfcol=c(4,3))
#	par(oma=c(3,4,2,1))
	par(mar=c(2,3,0,0))
	par(mgp=c(1.5,0.5,0))
	par(cex=1)
	par(bty="l")
	par(xpd=TRUE)
	
	for(i in 1:3){
	    if(i==1)par(mar=c(2.5,3,0.5,0))
	    if(i==2)par(mar=c(2.5,1.5,0.5,1.5))
	    if(i==3)par(mar=c(2.5,0,0.5,3))
		for(j in 1:4){
		    plotTecno(year=1965+10*j,threshrY = Inf,ylim=c(0,4.5), limGDP = c((i-1)/3,i/3),
		              xlab=c("GDP1980/GDP1970","GDP1990/GDP1980","GDP2000/GDP1990","GDP2010/GDP2000")[j],
		              ylab=c("収量1980/収量1970","収量1990/収量1980","収量2000/収量1990","収量2010/収量2000")[j],
		              xaxt="n",yaxt="n",coltext = 1)
		    text(2.5,4,paste("R=",round(cor2,3)),cex=0.8)
		    
		    usr<-par("usr")
		    
		    if(i==1){
		        axis(side = 2, at=0:4, labels = T)
		    }else{
		        axis(side = 2, at=0:4, labels = T)
		    }

		    
		    if(i==3){
		        text(usr[2]+(usr[2]-usr[1])*0/100,(usr[3]+usr[4])/2,c("1970~1980","1980~1990","1990~2000","2000~2010")[j],srt=-90)
		    }
		    
		    #上側GDP区分
		    if(j==1){
		        text((usr[1]+usr[2])/2,usr[4]+(usr[4]-usr[3])*0/100,c("GDP下位三分の一","GDP中位三分の一","GDP上位三分の一")[i])
		    }
		    
		    #下側軸目盛		    
		    if(j==4){
		        axis(side = 1, at=0:3, labels = T)
		    }else{
		        axis(side = 1, at=0:3, labels = T)
		    }
		}
	}
dev.off()