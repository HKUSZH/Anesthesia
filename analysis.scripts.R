library(ggplot2)
library(ggbeeswarm)
library(tidyr)
library(dplyr)
library(gridExtra)

DATA_OIANES<-readr::read_tsv("Anes-2015-2019-pkchen-june18.txt")
DATA_OIANES[["PACU"]] <-as.numeric(DATA_OIANES$PACU_Time_min)
Analgesics<-DATA_OIANES%>%select(Paracetamol:Diclofenac )
DATA_OIANES[["NumAnalgesics"]] <-rowSums(Analgesics=="YES")
DATA_OIANES[["TempGap"]] <- as.numeric(DATA_OIANES$maxTemp) - as.numeric(DATA_OIANES$minTemp)
DATA_OIANES[["MaxRatio"]] <- as.numeric(DATA_OIANES$maxTemp) / as.numeric(DATA_OIANES$initTemp)
DATA_OIANES[["AnesDrug"]] <-DATA_OIANES[["Sevoflurane-vs-propofol"]]
#DATA_OIANES[["maxTemp"]] <- as.numeric(DATA_OIANES$maxTemp)
#DATA_OIANES[["minTemp"]] <- as.numeric(DATA_OIANES$minTemp)
#DATA_OIANES[["initTemp"]] <- as.numeric(DATA_OIANES$initTemp)

DATA_OIANES[["midTemp"]] <- (DATA_OIANES$minTemp+DATA_OIANES$maxTemp)/2

DATA_OIANES[["isEmergAnal"]] <-!is.na(DATA_OIANES$EmergencyAnalgesics) & DATA_OIANES$EmergencyAnalgesics!="??o|????им???????им?????им?"
DATA_OIANES[["NumSurg"]]<-as.integer(table(DATA_OIANES$ChiName)[DATA_OIANES$ChiName])
DATA_OIANES[["NumSurgIsMulit"]]<-factor(as.integer(table(DATA_OIANES$ChiName)[DATA_OIANES$ChiName])>1)

#####################################

 table(DATA_OIANES$AnesMethod)
 table(DATA_OIANES$AnesMethod,DATA_OIANES$NumBones)
 table(DATA_OIANES$AnesMethod,DATA_OIANES$NumBones)
 table(DATA_OIANES$AnesMethod,DATA_OIANES$NumAnalgesics)
 table(DATA_OIANES$NumBones,DATA_OIANES$NumAnalgesics)
 table(DATA_OIANES$IntubationDevice)

 table(DATA_OIANES$Tibia,DATA_OIANES$Femur)

 table(DATA_OIANES$AnesMethod,DATA_OIANES$Femur)
 table(DATA_OIANES$AnesMethod,DATA_OIANES$Tibia)
tabLower<- table(DATA_OIANES$AnesMethod,DATA_OIANES$Tibia=="YES" | DATA_OIANES$Femur=="YES")
tabUpper<- table(DATA_OIANES$AnesMethod[DATA_OIANES$Osteotomy=="YES"],DATA_OIANES$Tibia[DATA_OIANES$Osteotomy=="YES"]=="YES"|DATA_OIANES$Femur[DATA_OIANES$Osteotomy=="YES"]=="YES")

 table(DATA_OIANES$AnesMethod[DATA_OIANES$Osteotomy=="YES"],DATA_OIANES$Femur[DATA_OIANES$Osteotomy=="YES"])
 table(DATA_OIANES$AnesMethod[DATA_OIANES$Osteotomy=="YES"],DATA_OIANES$Tibia[DATA_OIANES$Osteotomy=="YES"])

 table(DATA_OIANES$BloodTransfusionUnit>0,!is.na(DATA_OIANES$AutologousBloodTsfs))

LowerLimb<-paste0(DATA_OIANES$Femur,"_", DATA_OIANES$Tibia,"_",DATA_OIANES$Fibula,"_",DATA_OIANES$Hip)
UpperLimb<-paste0(DATA_OIANES$Humerus,"_", DATA_OIANES$Ulna,"_", DATA_OIANES$Radius)
table(UpperLimb, LowerLimb)
table(UpperLimb, DATA_OIANES$Osteotomy)
table(LowerLimb, DATA_OIANES$Osteotomy)

DATA_OIANESnonOsteo<-DATA_OIANES[DATA_OIANES$Osteotomy=="NO",]

 table(paste0(DATA_OIANES$Tibia,"_",DATA_OIANES$Fibula,"_",DATA_OIANES$Femur,"_",DATA_OIANES$Hip,"_",DATA_OIANES$Humerus,"_", DATA_OIANES$Ulna,"_", DATA_OIANES$Radius),
	DATA_OIANES$Osteotomy)
table(DATA_OIANESnonOsteo$Osteotomy, DATA_OIANESnonOsteo$Fracture)
table(DATA_OIANESnonOsteo$Osteotomy, DATA_OIANESnonOsteo$Adjustment)
table(DATA_OIANESnonOsteo$Osteotomy, DATA_OIANESnonOsteo$Revision)
table(DATA_OIANESnonOsteo$Osteotomy, DATA_OIANESnonOsteo$Adjustment)

Revision
Tibia					

 table(DATA_OIANES$IntubationDevice,DATA_OIANES$NumIntubation )
 table(DATA_OIANES$IntubationDevice,DATA_OIANES$GlotticGrade)

 table(DATA_OIANES$NumAnalgesics,DATA_OIANES$EmergencyAnalgesics)
chisq.test(as.matrix(table(DATA_OIANES$NumAnalgesics,DATA_OIANES$EmergencyAnalgesics)))

 table(DATA_OIANES$NumAnalgesics,DATA_OIANES$AnesMethod)
chisq.test(table(DATA_OIANES$NumAnalgesics,DATA_OIANES$AnesMethod))
chisq.test(table(DATA_OIANES$isEmergAnal,DATA_OIANES$AnesMethod))
chisq.test(table(DATA_OIANES$isEmergAnal,DATA_OIANES$NumAnalgesics))
chisq.test(table(DATA_OIANES$isEmergAnal,DATA_OIANES$AnesDrug))

boxplot(AgeSurgery~Ibuprofen, data=DATA_OIANES)
boxplot(AgeSurgery~Paracetamol, data=DATA_OIANES)

summary(DATA_OIANES$AgeSurgery[which(DATA_OIANES$Paracetamol=="YES")])
summary(DATA_OIANES$AgeSurgery[which(DATA_OIANES$Ibuprofen=="YES")])
summary(DATA_OIANES$AgeSurgery[which(DATA_OIANES$Tramadol=="YES")])
summary(DATA_OIANES$AgeSurgery[which(DATA_OIANES$Celecoxib=="YES")])

DATA_OIANES$AgeSurgery[which(DATA_OIANES$Diclofenac=="YES")]

table(DATA_OIANES$NumAnalgesics)/sum(table(DATA_OIANES$NumAnalgesics))

table(DATA_OIANES$Paracetamol)
table(DATA_OIANES$Ibuprofen)
table(DATA_OIANES$Tramadol)
table(DATA_OIANES$Celecoxib)
table(DATA_OIANES$Diclofenac)

pdf("SurgDurationHrs.vs.NumBones.pdf", width=8.5)
	ggplot(DATA_OIANES,aes(x=factor(NumBones),y=SurgDurationHrs)) +
		geom_violin(width=1.5) + geom_boxplot()  + 
		geom_quasirandom(aes(color=Gender,size=AgeSurgery))

	ggplot(DATA_OIANES,aes(x=factor(NumBones),y=SurgDurationHrs)) +
		geom_violin(width=1.5) + geom_boxplot()  + 
		geom_quasirandom(size=4, aes(color=Osteotomy))
dev.off()
###########################################
weight<-readr::read_tsv("weight-data-2013-china.txt")
height<-readr::read_tsv("height-data-2013-china.txt")

	SDsM<-data.frame(weight%>% select(ageMo, "-2SD":  "2SD"), "5PC"=NA, "1QR"=NA, "3QR"=NA, "90PC"=NA)
	SDsF<-data.frame(weight%>% select(ageMo, "-2SD_1":  "2SD_1"), "5PC"=NA, "1QR"=NA, "3QR"=NA, "90PC"=NA)
	hSDsM<-data.frame(height%>% select(ageMo, "-2SD":  "2SD"), "5PC"=NA, "1QR"=NA, "3QR"=NA, "90PC"=NA)
	hSDsF<-data.frame(height%>% select(ageMo, "-2SD_1":  "2SD_1"), "5PC"=NA, "1QR"=NA, "3QR"=NA, "90PC"=NA)

for(i in 1:nrow(weight)){
	SDsM[i,c(7:10)]<-weight$M[i] * (1+weight$L[i]*weight$S[i]*c(qnorm(0.05), qnorm(0.25), qnorm(0.75), qnorm(0.90)))^(1/weight$L[i])
	SDsF[i,c(7:10)]<-weight$M_1[i] * (1+weight$L_1[i]*weight$S_1[i]*c(qnorm(0.05), qnorm(0.25), qnorm(0.75), qnorm(0.90)))^(1/weight$L_1[i])
}
for(i in 1:nrow(height)){
	hSDsM[i,c(7:10)]<-height$M[i] * (1+height$L[i]*height$S[i]*c(qnorm(0.05), qnorm(0.25), qnorm(0.75), qnorm(0.90)))^(1/height$L[i])
	hSDsF[i,c(7:10)]<-height$M_1[i] * (1+height$L_1[i]*height$S_1[i]*c(qnorm(0.05), qnorm(0.25), qnorm(0.75), qnorm(0.90)))^(1/height$L_1[i])
}

pdf("Weight.against.reference.pdf")
 
	ggplot(DATA_OIANES %>% filter(Gender=="Male"),
		aes(x=jitter(AgeSurgery),y=Weight)) +
		geom_point(aes(color=NumSurgIsMulit), size=4) + #geom_text() +
		geom_line(data=SDsM, aes(x=ageMo/12, y=SDsM[["X5PC"]]), size=1, color="blue") + 
		geom_line(data=SDsM, aes(x=ageMo/12, y=SDsM[["X1QR"]]), size=1, color="blue") + 
		geom_line(data=SDsM, aes(x=ageMo/12, y=SDsM[["Median"]]),size=1,color = "black") + 
		geom_line(data=SDsM, aes(x=ageMo/12, y=SDsM[["X3QR"]]),size=1,color="red") +
		geom_line(data=SDsM, aes(x=ageMo/12, y=SDsM[["X90PC"]]),size=1,color="red") +
		theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 

	ggplot(DATA_OIANES %>% filter(Gender=="Female"),
		aes(x=jitter(AgeSurgery),y=Weight)) +
		geom_point(aes(color=NumSurgIsMulit), size=4) + #geom_text() +
		geom_line(data=SDsF, aes(x=ageMo/12, y=SDsF[["X5PC"]]), size=1, color="blue") + 
		geom_line(data=SDsF, aes(x=ageMo/12, y=SDsF[["X1QR"]]), size=1, color="blue") + 
		geom_line(data=SDsF, aes(x=ageMo/12, y=SDsF[["Median_1"]]),size=1,color = "black") + 
		geom_line(data=SDsF, aes(x=ageMo/12, y=SDsF[["X3QR"]]),size=1,color="red") +
		geom_line(data=SDsF, aes(x=ageMo/12, y=SDsF[["X90PC"]]),size=1,color="red") +
		theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 


dev.off()



pdf("Age.vs.Weight.pdf",width=8)

	ggplot(DATA_OIANES ,
		aes(x=AgeSurgery,y=log10(Weight),label=ChiName)) +
		geom_point(aes(color=AnesMethod),size=4) + #geom_text() +
		geom_smooth(method="lm") +
		theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 

	ggplot(DATA_OIANES ,
		aes(x=jitter(AgeSurgery),y=Weight,label=ChiName)) +
		geom_point(aes(color=AnesMethod,size=NumBones)) + #geom_text() +
		geom_smooth(method="lm") +
		theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 

	ggplot(DATA_OIANES ,
		aes(x=jitter(AgeSurgery),y=Weight,label=ChiName)) +
		geom_point(aes(color=Gender,size=NumBones)) + #geom_text() +
		geom_smooth(data=DATA_OIANES%>%filter(Gender=="Male"),aes(AgeSurgery,Weight),method="lm") +
		geom_smooth(data=DATA_OIANES%>%filter(Gender=="Female"),aes(AgeSurgery,Weight),method="lm",color="red") +
		theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 

	ggplot(DATA_OIANES ,
		aes(x=jitter(AgeSurgery),y=Weight)) +
		geom_point(aes(color=NumSurgIsMulit ,shape=Gender), size=4) + #geom_text() +
		geom_smooth(data=DATA_OIANES%>%filter(Gender=="Male"),aes(AgeSurgery,Weight),method="lm") +
		geom_smooth(data=DATA_OIANES%>%filter(Gender=="Female"),aes(AgeSurgery,Weight),method="lm",color="red") +
		geom_line(data=SDsM, aes(x=ageMo/12, y=SDsM[["X1QR"]]), size=1, color="blue") + 
		geom_line(data=SDsM, aes(x=ageMo/12, y=SDsM[["Median"]]),size=1,color = "black") + 
		geom_line(data=SDsM, aes(x=ageMo/12, y=SDsM[["X3QR"]]),size=1,color="red") +
		geom_line(data=SDsF, aes(x=ageMo/12, y=SDsF[["X1QR"]]), size=1, color="blue") + 
		geom_line(data=SDsF, aes(x=ageMo/12, y=SDsF[["Median_1"]]),size=1,color = "black") + 
		geom_line(data=SDsF, aes(x=ageMo/12, y=SDsF[["X3QR"]]),size=1,color="red") +
		theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 



	summary(lm(Weight~AgeSurgery,DATA_OIANES%>%filter(Gender=="Male")))
	summary(lm(Weight~AgeSurgery,DATA_OIANES%>%filter(Gender=="Female")))


dev.off()


pdf("Temperature.ordered.by.MinTemp.pdf",width=12)

	ord<-order(DATA_OIANES$minTemp)
	drugs<-as.integer(as.factor(DATA_OIANES[["Sevoflurane-vs-propofol"]]))
	PCH<-c(15, 16, 17)[drugs[ord]]
	DATA_OIANES[["Sevoflurane-vs-propofol"]][ord]
	plot(DATA_OIANES$minTemp[ord],ylim=range(c(DATA_OIANES$minTemp,DATA_OIANES$maxTemp),na.rm=T),
		pch=PCH,cex=2,col="#619CFF",xlim=c(0,215),xlab="patients (n=215)",ylab="Temperature")
	fit1<-loess(DATA_OIANES$maxTemp[ord][1:215]~seq(215))
	pred1<-predict(fit1, data.frame(seq(215)), se = TRUE)
	polygon(c(seq(215),rev(seq(215))),c(pred1$fit-pred1$se.fit,rev(pred1$fit+pred1$se.fit)),col="grey",border=0)
	lines(seq(215),pred1$fit,lwd=2,col="#F8766D")

	fit1<-loess(DATA_OIANES$initTemp[ord][1:215]~seq(215))
	pred1<-predict(fit1, data.frame(seq(215)), se = TRUE)
	polygon(c(seq(215),rev(seq(215))),c(pred1$fit-pred1$se.fit,rev(pred1$fit+pred1$se.fit)),col="#00FA38",border=0)
	lines(seq(215),pred1$fit,lwd=2,col="#adb2ad")

	points(DATA_OIANES$maxTemp[ord],pch=PCH,cex=2,col="#F8766D")
	text(1:252, DATA_OIANES$maxTemp[ord],DATA_OIANES[["Sevoflurane-vs-propofol"]][ord], srt=90, offset=0, pos=4)

	points(DATA_OIANES$initTemp[ord],pch=PCH,cex=2,col="#00BA38")

	for(i in 1:215)
		lines(c(i,i),c(DATA_OIANES$minTemp[ord][i],DATA_OIANES$maxTemp[ord][i]))
	abline(h=c(35,38.5))
dev.off()

pdf("Temperature.ordered.by.initTemp.pdf",width=20)

	ord<-order(DATA_OIANES$initTemp)
	drugs<-as.integer(as.factor(DATA_OIANES[["Sevoflurane-vs-propofol"]]))
	PCH<-c(15, 16, 17)[drugs[ord]]
	DATA_OIANES[["Sevoflurane-vs-propofol"]][ord]
	plot(DATA_OIANES$minTemp[ord],ylim=range(c(DATA_OIANES$minTemp,DATA_OIANES$maxTemp),na.rm=T),
		pch=PCH,cex=2,col="#619CFF",xlim=c(0,215),xlab="patients (n=215)",ylab="Temperature")
	fit1<-loess(DATA_OIANES$maxTemp[ord][1:215]~seq(215))
	pred1<-predict(fit1, data.frame(seq(215)), se = TRUE)
	polygon(c(seq(215),rev(seq(215))),c(pred1$fit-pred1$se.fit,rev(pred1$fit+pred1$se.fit)),col="grey",border=0)
	lines(seq(215),pred1$fit,lwd=2,col="#F8766D")

	fit1<-loess(DATA_OIANES$initTemp[ord][1:215]~seq(215))
	pred1<-predict(fit1, data.frame(seq(215)), se = TRUE)
	polygon(c(seq(215),rev(seq(215))),c(pred1$fit-pred1$se.fit,rev(pred1$fit+pred1$se.fit)),col="#00FA38",border=0)
	lines(seq(215),pred1$fit,lwd=2,col="#adb2ad")

	points(DATA_OIANES$maxTemp[ord],pch=PCH,cex=2,col="#F8766D")
	text(1:252, DATA_OIANES$maxTemp[ord],DATA_OIANES[["Sevoflurane-vs-propofol"]][ord], srt=90, offset=0, pos=4)

	points(DATA_OIANES$initTemp[ord],pch=PCH,cex=2,col="#00BA38")

	for(i in 1:215)
		lines(c(i,i),c(DATA_OIANES$minTemp[ord][i],DATA_OIANES$maxTemp[ord][i]))
	abline(h=c(35,37.5, 38.5))
dev.off()

###########################################

###########################################

diff(aggregate(DATA_OIANES$SurgDurationHrs,by=list(DATA_OIANES$NumBones),summary)$x[,4])
mean(diff(aggregate(DATA_OIANES$SurgDurationHrs,by=list(DATA_OIANES$NumBones),summary)$x[,4]))
s1<-summary(lm(SurgDurationHrs~NumBones,DATA_OIANES))
str(summary(aov(SurgDurationHrs~NumBones,DATA_OIANES)))

pdf("AnesMethod.vs.Duration.pdf",width=9)
	summary(lm(SurgDurationHrs~Weight,DATA_OIANES%>%filter(Gender=="Male")))
	summary(lm(SurgDurationHrs~Weight,DATA_OIANES%>%filter(Gender=="Female")))
	summary(lm(SurgDurationHrs~Weight,DATA_OIANES))

	ggplot(DATA_OIANES ,
		aes(x=Weight,y=SurgDurationHrs,label=ChiName)) +
		geom_point(aes(color=Gender,size=NumBones)) + #geom_text() +
		geom_smooth(data=DATA_OIANES%>%filter(Gender=="Male"),aes(Weight,SurgDurationHrs),method="lm") +
		geom_smooth(data=DATA_OIANES%>%filter(Gender=="Female"),aes(Weight,SurgDurationHrs),method="lm",color="red") +
		theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 


	ggplot(DATA_OIANES ,
		aes(x=Weight,y=SurgDurationHrs,label=ChiName)) +
		geom_point(aes(color=AnesMethod),size=4) + 
		theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 

	ggplot(DATA_OIANES ,
		aes(x=(Weight),y=jitter(NumBones),label=ChiName)) +
		geom_point(aes(color=AnesMethod),size=4) + 
		theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 

	ggplot(DATA_OIANES ,
		aes(x=(Weight),y=SurgDurationHrs,label=ChiName)) +
		geom_point(aes(color=AnesMethod),size=4) + 
		theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 

	ggplot(DATA_OIANES ,
		aes(x=jitter(AgeSurgery),y=SurgDurationHrs,label=ChiName)) +
		geom_point(aes(color=AnesMethod),size=4) + 
		theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 


	ggplot(DATA_OIANES ,
		aes(x=Weight,y=SurgDurationHrs,label=ChiName)) +
		geom_point(aes(color=AnesMethod),size=4) + 
		geom_density_2d(data=DATA_OIANES%>%filter(AnesMethod=="GA"),aes(x=Weight,y=SurgDurationHrs)) +
		geom_density_2d(data=DATA_OIANES%>%filter(AnesMethod=="GA+caudal"),aes(x=Weight,y=SurgDurationHrs),color="green") +
		geom_density_2d(data=DATA_OIANES%>%filter(AnesMethod=="GA+EA"),aes(x=Weight,y=SurgDurationHrs),color="purple") +
		geom_density_2d(data=DATA_OIANES%>%filter(AnesMethod=="GA+NB"),aes(x=Weight,y=SurgDurationHrs),color="blue") +
		xlim(c(0,60))+ylim(c(0,15))+
		theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 
##############
	p1<-	ggplot(data=DATA_OIANES%>%filter(AnesMethod=="GA"),
		aes(x=Weight,y=SurgDurationHrs,label=ChiName)) +
		geom_point(aes(color=AnesMethod),size=4) + 
		xlim(c(0,60))+ylim(c(0,15))+geom_density_2d()+
		theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
		theme(legend.position = "none") + ggtitle("GA")

	p2<-	ggplot(data=DATA_OIANES%>%filter(AnesMethod=="GA+caudal"),
		aes(x=Weight,y=SurgDurationHrs,label=ChiName)) +
		geom_point(aes(color=AnesMethod),size=4) + 
		xlim(c(0,60))+ylim(c(0,15))+geom_density_2d()+
		theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
		theme(legend.position = "none") + ggtitle("GA+caudal")

	p3<-	ggplot(data=DATA_OIANES%>%filter(AnesMethod=="GA+NB"),
		aes(x=Weight,y=SurgDurationHrs,label=ChiName)) +
		geom_point(aes(color=AnesMethod),size=4) + 
		xlim(c(0,60))+ylim(c(0,15))+geom_density_2d()+
		theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
		theme(legend.position = "none") + ggtitle("GA+NB")

	p4<-	ggplot(data=DATA_OIANES%>%filter(AnesMethod=="GA+EA"),
		aes(x=Weight,y=SurgDurationHrs,label=ChiName)) +
		geom_point(aes(color=AnesMethod),size=4) + 
		xlim(c(0,60))+ylim(c(0,15))+geom_density_2d()+
		theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
		theme(legend.position = "none") + ggtitle("GA+EA")

	ggplot(DATA_OIANES ,
		aes(x=Weight,y=SurgDurationHrs,label=ChiName)) +
		geom_point(aes(color=AnesMethod),size=4) + 
		xlim(c(0,60))+ylim(c(0,15))+
		theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 
	grid.arrange(p1,p2,p3,p4)
##############
	p1<-	ggplot(data=DATA_OIANES%>%filter(AnesMethod=="GA"),
		aes(x=AgeSurgery,y=SurgDurationHrs,label=ChiName)) +
		geom_point(aes(color=AnesMethod),size=4) + 
		xlim(c(0,20))+ylim(c(0,15))+geom_density_2d()+
		theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
		theme(legend.position = "none") + ggtitle("GA")

	p2<-	ggplot(data=DATA_OIANES%>%filter(AnesMethod=="GA+caudal"),
		aes(x=AgeSurgery,y=SurgDurationHrs,label=ChiName)) +
		geom_point(aes(color=AnesMethod),size=4) + 
		xlim(c(0,20))+ylim(c(0,15))+geom_density_2d()+
		theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
		theme(legend.position = "none") + ggtitle("GA+caudal")

	p3<-	ggplot(data=DATA_OIANES%>%filter(AnesMethod=="GA+NB"),
		aes(x=AgeSurgery,y=SurgDurationHrs,label=ChiName)) +
		geom_point(aes(color=AnesMethod),size=4) + 
		xlim(c(0,20))+ylim(c(0,15))+geom_density_2d()+
		theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
		theme(legend.position = "none") + ggtitle("GA+NB")

	p4<-	ggplot(data=DATA_OIANES%>%filter(AnesMethod=="GA+EA"),
		aes(x=AgeSurgery,y=SurgDurationHrs,label=ChiName)) +
		geom_point(aes(color=AnesMethod),size=4) + 
		xlim(c(0,20))+ylim(c(0,15))+geom_density_2d()+
		theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
		theme(legend.position = "none") + ggtitle("GA+EA")
	ggplot(DATA_OIANES ,
		aes(x=AgeSurgery,y=SurgDurationHrs,label=ChiName)) +
		geom_point(aes(color=AnesMethod),size=4) + 
		xlim(c(0,20))+ylim(c(0,15))+
		theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 
	grid.arrange(p1,p2,p3,p4)

##############
	ggplot(DATA_OIANES %>%
			mutate(class = reorder(AnesMethod, SurgDurationHrs, median)) ,
		aes(x=class ,y=SurgDurationHrs)) +
		geom_violin() + geom_boxplot() + 
		geom_quasirandom(aes(color=Gender,size=Weight)) + 
		theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

	aggregate(DATA_OIANES$Weight,by=list(DATA_OIANES$AnesMethod),summary)

	ggplot(DATA_OIANES %>%
			mutate(clasz = reorder(AnesMethod, Weight,median)),
		aes(x=clasz ,y=Weight)) +
		geom_boxplot() + geom_violin() + 
		geom_quasirandom(aes(color=Gender,size=SurgDurationHrs)) + 
		theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


	ggplot(DATA_OIANES %>%
			mutate(class = reorder(AnesMethod, AgeSurgery,median)) ,
		aes(x=class ,y=AgeSurgery)) +
		geom_violin() + geom_boxplot() + ylab('Age (yrs)') +
		geom_quasirandom() + 
		theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


dev.off()
##############################################
library(lme4)

fitWeight<-lm(Weight ~ AgeSurgery, DATA_OIANES)
boxplot(residuals(fitWeight)~DATA_OIANES$EngName)

summary(fitWeight)
summary(lmer(Weight ~ AgeSurgery + (AgeSurgery | EngName), DATA_OIANES)
summary(lmer(Weight ~ AgeSurgery + (AgeSurgery | EngName), DATA_OIANES%>%filter(EngName %in% names(which(table(DATA_OIANES$EngName)>1)))))

DATA_OIANES2<-DATA_OIANES%>%filter(EngName %in% names(which(table(DATA_OIANES$EngName)>1)))
##############################################
median(DATA_OIANES$AgeSurgery)
mean(DATA_OIANES$AgeSurgery)

SurIntervals<-sapply(split(as.Date(DATA_OIANES$SurgeryDate),DATA_OIANES$ChiName),function(x){
	DIFF<-c()
	y<-sort(x)
	if(length(y)>1){
		for(i in 1:(length(y)-1))
			DIFF[i]<-difftime(y[i+1],y[i],units="days")
	}
	DIFF
})

str(unique(unlist(SurIntervals)))
str(names(SurIntervals[sapply(SurIntervals,length)>0]))
summary(as.vector(unlist(SurIntervals)))

summary(DATA_OIANES$SurgDurationHrs)
##############################################
table(DATA_OIANES$AnesMethod,DATA_OIANES$IBP)
table(DATA_OIANES$AnesMethod,DATA_OIANES$CVP)
table(DATA_OIANES$AnesMethod,DATA_OIANES$preOpParacetamol )
table(DATA_OIANES$AnesMethod,DATA_OIANES[["Sevoflurane-vs-propofol"]])

table(DATA_OIANES$AnesMethod,DATA_OIANES$Bearhugger2)

##############################################
pdf("Blood.Loss.Aug2.pdf",width=10)
	ggplot(DATA_OIANES %>%
			mutate(class = reorder(AnesMethod, BloodLoss,median)) ,
		aes(x=class ,y=BloodLoss)) +
		geom_violin() + geom_boxplot() + ylab('Blood loss (%)') +
		geom_quasirandom() + 
		theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

	ggplot(DATA_OIANES %>%
			mutate(class = reorder(AnesMethod, PercentLoss,median)) ,
		aes(x=class ,y=PercentLoss)) +
		geom_violin() + geom_boxplot() + ylab('Blood loss (%)') +
		geom_quasirandom() + 
		theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

	ggplot(DATA_OIANES %>%
			mutate(NumBones= reorder(NumBones, PercentLoss,median)) ,
		aes(x=NumBones,y=PercentLoss)) +
		geom_violin() + geom_boxplot() + ylab('Blood loss (%)') +
		geom_quasirandom() + 
		theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

	ggplot(DATA_OIANES %>%
			mutate(NumBones= reorder(NumBones, as.numeric(PACU_Time_min),median)) ,
		aes(x=NumBones,y= as.numeric(PACU_Time_min))) +
		geom_violin() + geom_boxplot() + ylab('Blood loss (%)') +
		geom_quasirandom() + 
		theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

	ggplot(DATA_OIANES %>%
			mutate(NumBones= reorder(AnesMethod, as.numeric(PACU_Time_min),median)) ,
		aes(x=AnesMethod,y= as.numeric(PACU_Time_min))) +
		geom_violin() + geom_boxplot() + ylab('Blood loss (%)') +
		geom_quasirandom() + 
		theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

	ggplot(DATA_OIANES,
		aes(x=SurgDurationHrs,y=(PercentLoss))) +
		geom_point(aes(size=AgeSurgery)) + geom_smooth(method="lm") +
		theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

	ggplot(DATA_OIANES,
		aes(x=AnesMethod,y=(BloodLoss))) +
		geom_point(size=4) + geom_smooth(method="lm") +
		theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


	ggplot(DATA_OIANES,
		aes(x=SurgDurationHrs,y=sqrt(PercentLoss))) +
		geom_point() + geom_smooth(method="lm") +
		theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

	ggplot(DATA_OIANES,
		aes(x=SurgDurationHrs,y=sqrt(BloodLoss))) +
		geom_point() + geom_smooth(method="lm") +
		theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

	ggplot(DATA_OIANES%>%mutate(NumBones=factor(NumBones)),
		aes(x=NumBones,y=(BloodLoss))) +
		geom_boxplot() + geom_violin() + 
		geom_quasirandom(aes(color=(NumBones),size=AgeSurgery)) +
		theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


	ggplot(DATA_OIANES,
		aes(x=AgeSurgery,y=(BloodLoss))) +
		geom_point(aes(color=factor(NumBones)),size=4) + 
		geom_smooth(data=DATA_OIANES%>%filter(NumBones==0),aes(AgeSurgery,(BloodLoss)),method="lm",color=hue_pal()(5)[1]) +
		geom_smooth(data=DATA_OIANES%>%filter(NumBones==1),aes(AgeSurgery,(BloodLoss)),method="lm",color=hue_pal()(5)[2]) +
		geom_smooth(data=DATA_OIANES%>%filter(NumBones==2),aes(AgeSurgery,(BloodLoss)),method="lm",color=hue_pal()(5)[3]) +
		geom_smooth(data=DATA_OIANES%>%filter(NumBones==3),aes(AgeSurgery,(BloodLoss)),method="lm",color=hue_pal()(5)[4]) +
		geom_smooth(data=DATA_OIANES%>%filter(NumBones==4),aes(AgeSurgery,(BloodLoss)),method="lm",color=hue_pal()(5)[5]) +
		theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))



	ggplot(DATA_OIANES,
		aes(x=PercentLoss,y=BloodTransfusionUnit)) +
		geom_point() + geom_smooth(method="lm") +
		theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


	ggplot(DATA_OIANES,
		aes(x=BloodLoss,y=BloodTransfusionUnit)) +
		geom_point() + geom_smooth(method="lm") +
		theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


	summary(lm((BloodLoss/Weight)~AgeSurgery,DATA_OIANES))
	summary(lm((BloodLoss/Weight)~Weight,DATA_OIANES))
	summary(lm((BloodLoss/Weight)~SurgDurationHrs,DATA_OIANES))
	summary(lm((BloodLoss/Weight)~NumBones,DATA_OIANES))
	summary(lm((BloodLoss/Weight)~AnesMethod+AgeSurgery,DATA_OIANES))
	summary(lm((BloodLoss/Weight)~AnesMethod,DATA_OIANES))
	summary(lm((BloodLoss/Weight)~AnesDrug,DATA_OIANES))
	summary(lm((BloodLoss/Weight)~BloodTransfusionUnit,DATA_OIANES))
	summary(lm((BloodLoss/Weight)~sillence252$SillenceType,DATA_OIANES))
	summary(lm((BloodLoss/Weight)~sillence252$SillenceType,DATA_OIANES))

	#summary(lm(BloodLoss~Weight+Tibia+Femur+Fibula+Hip+Humerus+Ulna+Radius,DATA_OIANES))
	#confint(lm(BloodLoss~Weight+Tibia+Femur+Fibula+Hip+Humerus+Ulna+Radius,DATA_OIANES))
	fitBloodLoss<-lm(BloodLoss~Weight+Tibia+Femur+Fibula+Hip+Humerus+Ulna+Radius,DATA_OIANES)
	confint(fitBloodLoss)

	chisq.test(table(DATA_OIANES$PercentLoss>20, sillence252$SillenceType))

	summary(lm((BloodLoss/Weight)~Tibia+Femur+Fibula+Hip+Humerus+Ulna+Radius,DATA_OIANES))
	confint(lm((BloodLoss/Weight)~Tibia+Femur+Fibula+Hip+Humerus+Ulna+Radius,DATA_OIANES))

	fitBloodLossW<-lm((BloodLoss/Weight)~Tibia+Femur+Fibula+Hip+Humerus+Ulna+Radius,DATA_OIANES)
	summary(fitBloodLossW)
	confint(fitBloodLossW)

dev.off()
###########################################
lm((BloodLoss/)~AgeSurgery+Weight+Tibia+Femur+Fibula+Hip+Humerus+Ulna+Radius,DATA_OIANES)


	summary(fitBloodLoss)

	hist(residuals(fitBloodLossW), breaks=50)
	qqnorm(scale(residuals(fitBloodLossW)))
	abline(0,1,col="red")

	par(mar=c(12,4,4,2))
	boxplot(residuals(fitBloodLossW)~DATA_OIANES$EngName, las=2, xlab="")
	abline(h=0,col="red", lwd=2)


	table(DATA_OIANES$AutologousBloodTsfs)

	plot(Weight, BloodLoss, data=DATA_OIANES)

###########################################
pdf("PACU.pdf")
	ggplot(DATA_OIANES,
		aes(x=AgeSurgery,y=PACU)) +
		geom_point() + geom_smooth(method="lm") +
		theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

	ggplot(DATA_OIANES,
		aes(x=PercentLoss,y=PACU)) +
		geom_point() + geom_smooth(method="lm") +
		theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

	ggplot(DATA_OIANES,
		aes(x=NumAnalgesics,y=PACU)) +
		geom_point() + geom_smooth(method="lm") +
		theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

	ggplot(DATA_OIANES %>%
			mutate(NumAnalgesics= reorder(NumAnalgesics, PACU,median,na.rm=T)) ,
		aes(x=NumAnalgesics,y=PACU)) +
		geom_violin() + geom_boxplot() + ylab('PACU_Time(min )') +
		geom_quasirandom() + 
		theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

	ggplot(DATA_OIANES,
		aes(x=as.numeric(maxTemp),y=PACU)) +
		geom_point() + geom_smooth(method="lm") +
		theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


	ggplot(DATA_OIANES,
		aes(x=as.numeric(maxTemp),y=PACU)) +
		geom_point() + geom_smooth(method="lm") +
		theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))



dev.off()

	summary(lm(PACU~AgeSurgery,DATA_OIANES))
	summary(lm(PACU~Weight,DATA_OIANES))
	summary(lm(PACU~SurgDurationHrs,DATA_OIANES))
	summary(lm(PACU~BloodLoss,DATA_OIANES))
	summary(lm(PACU~NumBones,DATA_OIANES))
	summary(lm(PACU~AnesMethod+AgeSurgery,DATA_OIANES))
	summary(lm(PACU~AnesMethod,DATA_OIANES))
	summary(lm(PACU~AnesDrug,DATA_OIANES))
	summary(lm(PACU~BloodTransfusionUnit,DATA_OIANES))
	summary(lm(PACU~initTemp,DATA_OIANES))
	summary(lm(PACU~minTemp,DATA_OIANES))
	summary(lm(PACU~maxTemp,DATA_OIANES))

	summary(lm(PACU~Tibia+Femur+Fibula+Hip+Humerus+Ulna+Radius,DATA_OIANES))
	summary(lm(PACU~Weight+Tibia+Femur+Fibula+Hip+Humerus+Ulna+Radius,DATA_OIANES))
	summary(lm(PACU~AgeSurgery+Weight+Tibia+Femur+Fibula+Hip+Humerus+Ulna+Radius,DATA_OIANES))
	summary(DATA_OIANES$PACU)
###########################################
# use point: temp drop and increase under control

summary(as.numeric(DATA_OIANES$minTemp)-as.numeric(DATA_OIANES$initTemp),na.rm=T)
summary(as.numeric(DATA_OIANES$maxTemp)-as.numeric(DATA_OIANES$initTemp),na.rm=T)

1-summary(as.numeric(DATA_OIANES$minTemp)/as.numeric(DATA_OIANES$initTemp),na.rm=T)
summary(as.numeric(DATA_OIANES$maxTemp)/as.numeric(DATA_OIANES$initTemp),na.rm=T)-1

pdf("Temperature.scatter.pdf")
	TempData<-data.frame(DATA_OIANES%>%select(initTemp,minTemp:maxTemp))
	plot(TempData,pch=16)
dev.off()

pdf("Temperature.pdf",width=10)
	hist(DATA_OIANES$initTemp,breaks=20)
	hist(DATA_OIANES$minTemp,breaks=20)
	hist(DATA_OIANES$maxTemp,breaks=20)
	ggplot(DATA_OIANES %>%
			mutate(AnesMethod= reorder(AnesMethod, maxTemp,median)) ,
		aes(x=AnesMethod,y=maxTemp)) +
		geom_violin() + geom_boxplot() + ylab('Max temp') +
		geom_quasirandom() + 
		theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

	ggplot(DATA_OIANES %>%
			mutate(AnesMethod= reorder(AnesMethod, TempGap,median,na.rm=T)) ,
		aes(x=AnesMethod,y=TempGap)) +
		geom_violin() + geom_boxplot() + ylab('Max temp - Min temp') +
		geom_quasirandom() + 
		theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

	ggplot(DATA_OIANES %>%mutate(AnesMethod2= reorder(AnesMethod, MaxRatio,median,na.rm=T)) ,
		aes(x=AnesMethod2,y=MaxRatio)) +
		geom_violin() + geom_boxplot() + ylab('Max temp / Init temp') +
		geom_quasirandom() + 
		theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


	ggplot(DATA_OIANES,
		aes(x=minTemp,y=maxTemp)) +
		geom_point() + geom_smooth(method="lm") +
		theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

	ggplot(DATA_OIANES,
		aes(x=initTemp,y=maxTemp)) +
		geom_point() + geom_smooth(method="lm") +
		theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

	ggplot(DATA_OIANES,
		aes(x=initTemp,y=minTemp,label=ChiName)) +
		geom_point() + geom_text() + geom_smooth(method="lm") +
		theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

	ggplot(DATA_OIANES,
		aes(x=AgeSurgery,y=as.numeric(initTemp))) +
		geom_point() + geom_smooth(method="lm") +
		theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

	ggplot(DATA_OIANES,
		aes(x=AgeSurgery,y=as.numeric(maxTemp))) +
		geom_point() + geom_smooth(method="lm") +
		theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))



	
dev.off()

	summary(lm(as.numeric(TempGap)~AgeSurgery+AnesMethod+Gender+SurgDurationHrs,DATA_OIANES))
	summary(lm(as.numeric(MaxRatio)~AgeSurgery+AnesMethod,DATA_OIANES))
	summary(lm(as.numeric(initTemp)~AgeSurgery+AnesMethod,DATA_OIANES))
	summary(lm(as.numeric(maxTemp)~AgeSurgery+AnesMethod,DATA_OIANES))
	summary(lm(as.numeric(minTemp)~AgeSurgery+AnesMethod,DATA_OIANES))


	summary(lm(as.numeric(maxTemp)~AgeSurgery+AnesDrug+AnesMethod+SurgDurationHrs,DATA_OIANES))

	DATA_OIANESMaxTemp<-DATA_OIANES %>% filter(!is.na(maxTemp))
	fitMaxTemp<- lm(as.numeric(maxTemp)~AgeSurgery+AnesDrug+AnesMethod+SurgDurationHrs,DATA_OIANESMaxTemp)

	summary(fitMaxTemp)

	hist(residuals(fitMaxTemp), breaks=50)
	qqnorm(scale(residuals(fitMaxTemp)))
	abline(0,1,col="red")

	par(mar=c(12,4,4,2))
	boxplot(residuals(fitMaxTemp)~DATA_OIANESMaxTemp$EngName, las=2, xlab="")
	abline(h=0,col="red", lwd=2)




	DATA_OIANES2<-DATA_OIANES%>%filter(EngName %in% names(which(table(DATA_OIANES$EngName)>1)))
	summary(lmer(as.numeric(maxTemp)~AgeSurgery+AnesDrug+AnesMethod+SurgDurationHrs + (AgeSurgery | EngName) ,DATA_OIANES2))
	summary(lmer(as.numeric(maxTemp)~AgeSurgery+AnesDrug+AnesMethod+SurgDurationHrs  + (SurgDurationHrs | EngName) ,DATA_OIANES2))


	summary(lm(as.numeric(maxTemp)~AgeSurgery+SurgDurationHrs,DATA_OIANES))
	summary(lm(as.numeric(minTemp)~AgeSurgery+SurgDurationHrs,DATA_OIANES))
	summary(lm(maxTemp~AgeSurgery+Bearhugger2+SurgDurationHrs,DATA_OIANES))
	summary(lm(minTemp~AgeSurgery+Bearhugger2+SurgDurationHrs,DATA_OIANES))
	table(DATA_OIANES$maxTemp>37.5)
	tabTempType<-table(DATA_OIANES$isHypo, as.factor(sillence252$SillenceType))
	chisq.test(tabTempType)
	tabTempType<-table(DATA_OIANES$isHyper, as.factor(sillence252$SillenceType))
	chisq.test(tabTempType)
	DATA_OIANES[["isHypo"]]<-DATA_OIANES$minTemp<35
	DATA_OIANES[["isHyper"]]<-DATA_OIANES$maxTemp>37.5
	DATA_OIANES[["isFever"]]<-DATA_OIANES$maxTemp>38.5

	summary(lm(AgeSurgery~isHypo, data=DATA_OIANES))
	summary(lm(AgeSurgery~isHyper, data=DATA_OIANES))
	summary(lm(AgeSurgery~isFever, data=DATA_OIANES))
	boxplot(AgeSurgery~isHyper, data=DATA_OIANES)
	boxplot(AgeSurgery~isHypo, data=DATA_OIANES)

	ggplot(DATA_OIANES,
		aes(x=SurgDurationHrs,y=maxTemp)) +
		geom_point() + geom_smooth(method="lm") +
		theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

	summary(lm(as.numeric(maxTemp)~AgeSurgery+AnesMethod+AnesDrug+SurgDurationHrs,DATA_OIANES))

	DATA_OIANESTemp<-DATA_OIANES%>%filter(!is.na(maxTemp))

	summary(lm(lm(maxTemp~AgeSurgery,DATA_OIANESTemp)$residuals ~AgeSurgery+AnesMethod+AnesDrug+SurgDurationHrs,DATA_OIANESTemp))

	summary(lm(as.numeric(maxTemp)~AgeSurgery,DATA_OIANES))

	summary(lm(as.numeric(maxTemp)~AgeSurgery,DATA_OIANES))
	summary(lm(PercentLoss~BloodTransfusionUnit,DATA_OIANES))
##############################################

	summary(lm(PercentLoss~NumBones+AnesMethod+AgeSurgery,DATA_OIANES))

# useful point: % blood loss irrelevant to anethetic method
	summary(lm(lm(PercentLoss~NumBones,DATA_OIANES)$residuals~NumBones+AnesMethod+AgeSurgery,DATA_OIANES))
# useful point: younger children tend to have slightly more % of blood loss
	summary(lm(lm(PercentLoss~NumBones,DATA_OIANES)$residuals~NumBones+AnesMethod+SurgDurationHrs+AgeSurgery,DATA_OIANES))

##############################################
bodytemp1<-30+c(7.9, 7.4, 7.4, 7.4, 7.7, 7.2, 7.3, 7.2, 7.3, 7.4, 7.5,  7.4, 6.7, 7.1, 7.2, 7.7, 6.7, 6.6, 7.2, 6.7, 7.0)
ages1<-c(4.25, 6, 6.5, 6+10/12, 7.67, 7.75, 7.75, 8+10/12, 8+10/12, 9.5, 10+1/12, 11.33, 11.33, 11.67, 12+11/12, 13+1/12, 
	15+7/12, 16.25, 16.33, 32.25, 32.25)
bodytemp2<-30+c(7.3, 7.7, 7.4, 7.3, 6.5, 7.5, 6.5, 7.1, 7.2)
ages2<-c(7+7/12, 7+10/12, 8+10/12, 9.5, 9.67, 10+7/12, 13+1/12, 15+1/12, 18.67)

bodytemp<-c(bodytemp1, bodytemp2)
ages<-c(ages1, ages2)
plot(ages, bodytemp)
cor(ages, bodytemp)
lm(bodytemp~ages)
summary(lm(bodytemp~ages))
##############################################
table(DATA_OIANES$Bearhugger2)
table(DATA_OIANES$AnesMethod,DATA_OIANES$Bearhugger2)

table(DATA_OIANES$Osteotomy)
table(DATA_OIANES$Revision)
table(DATA_OIANES$Osteotomy,DATA_OIANES$Revision)
table(DATA_OIANES$Osteotomy,DATA_OIANES$Fracture)
as.data.frame(DATA_OIANES%>%filter(Osteotomy=="NO")%>%select(Osteotomy:TendonLengthening))
	Fracture	Adjustment	Revision	RemoveRodding	InternalFixation	Rodding	Casting	Neurolysis	Kirschner	TendonLengthening



SiteTab<-apply(DATA_OIANES%>%select(Tibia:Radius),2,table)
round(SiteTab["YES",]/colSums(SiteTab)*100,2)
table(DATA_OIANES$Tibia,DATA_OIANES$Fibula)

table(DATA_OIANES$NumBones)/sum(table(DATA_OIANES$NumBones))

mean(DATA_OIANES$SurgDurationHrs)
sd(DATA_OIANES$SurgDurationHrs)

