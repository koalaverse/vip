
## Flowchart examples
par(ask=TRUE)

## MODELLING DIAGRAM
mar <- par(mar=c(1,1,1,1))
openplotmat(main="from Soetaert and herman, book in prep",cex.main=1)
elpos<-coordinates (c(1,1,1,1,1,1,1,1),mx=-0.1)
segmentarrow(elpos[7,],elpos[2,],arr.pos=0.15,dd=0.3,arr.side=3,endhead=TRUE)
segmentarrow(elpos[7,],elpos[3,],arr.pos=0.15,dd=0.3,arr.side=3,endhead=TRUE)
segmentarrow(elpos[7,],elpos[4,],arr.pos=0.15,dd=0.3,arr.side=3,endhead=TRUE)

pin   <- par ("pin")        # size of plotting region, inches
xx  <- 0.2
yy  <- xx*pin[1]/pin[2]*0.15  # used to make circles round

sx    <- rep(xx,8)
sx[7] <- 0.05

sy    <- rep(yy,8)
sy[6] <-yy*1.5
sy[7] <- sx[7]*pin[1]/pin[2]

for (i in c(1:7)) straightarrow (to=elpos[i+1,],from=elpos[i,],lwd=2,arr.pos=0.6,endhead=TRUE)
lab <- c("Problem","Conceptual model","Mathematical model","Parameterisation",
         "Mathematical solution","","OK?","Prediction, Analysis")

for (i in c(1:5,8)) textround(elpos[i,],sx[i],sy[i],lab=lab[i])

textround(elpos[6,],xx,yy*1.5,lab=c("Calibration,sensitivity","Verification,validation"))
textdiamond(elpos[7,],sx[7],sy[7],lab=lab[7])

textplain(c(0.7,elpos[2,2]),yy*2,lab=c("main components","relationships"),font=3,adj=c(0,0.5))
textplain(c(0.7,elpos[3,2]),yy ,"general theory",adj=c(0,0.5),font=3)
textplain(c(0.7,elpos[4,2]),yy*2,lab=c("literature","measurements"),font=3,adj=c(0,0.5))
textplain(c(0.7,elpos[6,2]),yy*2,lab=c("field data","lab measurements"),font=3,adj=c(0,0.5))

#####
##  DIAGRAM

par(mar=c(1,1,1,1))
openplotmat()
elpos<-coordinates (c(1,1,2,4))
fromto <- matrix(ncol=2,byrow=TRUE,data=c(1,2,2,3,2,4,4,7,4,8))
nr     <-nrow(fromto)
arrpos <- matrix(ncol=2,nrow=nr)
for (i in 1:nr) 
    arrpos[i,]<- straightarrow (to=elpos[fromto[i,2],],from=elpos[fromto[i,1],]
        ,lwd=2,arr.pos=0.6,arr.length=0.5)
textellipse(elpos[1,],0.1,lab="start",box.col="green",shadow.col="darkgreen",shadow.size=0.005,cex=1.5)
textrect   (elpos[2,],0.15,0.05,lab="found term?",box.col="blue",shadow.col="darkblue",shadow.size=0.005,cex=1.5)
textrect   (elpos[4,],0.15,0.05,lab="related?",box.col="blue",shadow.col="darkblue",shadow.size=0.005,cex=1.5)
textellipse(elpos[3,],0.1,0.1,lab=c("other","term"),box.col="orange",shadow.col="red",shadow.size=0.005,cex=1.5)
textellipse(elpos[3,],0.1,0.1,lab=c("other","term"),box.col="orange",shadow.col="red",shadow.size=0.005,cex=1.5)
textellipse(elpos[7,],0.1,0.1,lab=c("make","a link"),box.col="orange",shadow.col="red",shadow.size=0.005,cex=1.5)
textellipse(elpos[8,],0.1,0.1,lab=c("new","article"),box.col="orange",shadow.col="red",shadow.size=0.005,cex=1.5)

dd <- c(0.0,0.025)
text(arrpos[2,1]+0.05,arrpos[2,2],"yes")
text(arrpos[3,1]-0.05,arrpos[3,2],"no")
text(arrpos[4,1]+0.05,arrpos[4,2]+0.05,"yes")
text(arrpos[5,1]-0.05,arrpos[5,2]+0.05,"no")


#####
par(mfrow=c(2,2))
par(mar=c(0,0,0,0))
openplotmat()
elpos<-coordinates (c(2,3))
treearrow(from=elpos[1:2,],to=elpos[3:5,],arr.side=2,path="H")
for ( i in 1:5) textrect (elpos[i,],0.15,0.05,lab=i,cex=1.5)

openplotmat()
elpos<-coordinates (c(3,2),hor=FALSE)
treearrow(from=elpos[1:3,],to=elpos[4:5,],arr.side=2,arr.pos=0.2,path="V")
for ( i in 1:5) textrect (elpos[i,],0.15,0.05,lab=i,cex=1.5)

openplotmat()
elpos<-coordinates (c(1,4))
treearrow(from=elpos[1,],to=elpos[2:5,],arr.side=2,arr.pos=0.7,path="H")
for ( i in 1:5) textrect (elpos[i,],0.05,0.05,lab=i,cex=1.5)

openplotmat()
elpos<-coordinates (c(2,1,2,3))
elpos[1,1]<-0.3;elpos[2,1]<-0.7
treearrow(from=elpos[1:3,],to=elpos[4:8,],arr.side=2,path="H")
for ( i in 1:8) bentarrow(from=elpos[i,],to=elpos[i,]+c(0.1,-0.05),
                arr.pos=1,arr.type="circle",arr.col="white",arr.length=0.2)
for ( i in 1:8) textrect (elpos[i,],0.05,0.05,lab=i,cex=1.5)
mtext(side=3,outer=TRUE,line=-2,"treearrow",cex=1.5)



par(mfrow=c(1,1))

par(mar=c(0,0,0,0))
openplotmat()
elpos<-coordinates (c(1,1,2,1))
straightarrow (to=elpos[2,],from=elpos[1,])
treearrow(from=elpos[2,],to=elpos[3:4,],arr.side=2,path="H")
treearrow(from=elpos[3:4,],to=elpos[5,],arr.side=2,path="H")
segmentarrow(from=elpos[5,],to=elpos[2,],dd=0.4)
curvedarrow(from= elpos[5,],to=elpos[2,],curve=0.8)
col <- femmecol(5)
texthexa (mid=elpos[1,],radx=0.1,angle=20,shadow.size=0.01,rady=0.05,lab=1,box.col=col[1])
textrect (mid=elpos[2,],radx=0.1,shadow.size=0.01,rady=0.05,lab=2,box.col=col[2])
textround (mid=elpos[3,],radx=0.05,shadow.size=0.01,rady=0.05,lab=3,box.col=col[3])
textellipse (mid=elpos[4,],radx=0.05,shadow.size=0.01,rady=0.05,lab=4,box.col=col[4])
textellipse (mid=elpos[5,],radx=0.05,shadow.size=0.01,rady=0.08,angle=45,lab=5,box.col=col[5])



par(mar=c(1,1,1,1))
openplotmat(main="Arrowtypes")
elpos<-coordinates (c(1,2,1),mx=0.1,my=-0.1)
curvedarrow(from=elpos[1,],to=elpos[2,],curve=-0.5,lty=2,lcol=2)
straightarrow(from=elpos[1,],to=elpos[2,],lty=3,lcol=3)
segmentarrow(from=elpos[1,],to=elpos[2,],lty=1,lcol=1)
treearrow(from=elpos[2:3,],to=elpos[4,],lty=4,lcol=4)
bentarrow(from=elpos[3,],to=elpos[3,]-c(0.1,0.1),arr.pos=1,lty=5,lcol=5)
bentarrow(from=elpos[1,],to=elpos[3,],lty=5,lcol=5)
selfarrow(pos=elpos[3,],path="R",lty=6,curve=0.075,lcol=6)
splitarrow(from=elpos[1,],to=elpos[2:3,],lty=1,lwd=1,dd=0.7,arr.side=1:2,lcol=7)

for ( i in 1:4) textrect (elpos[i,],0.05,0.05,lab=i,cex=1.5)

legend("topright",lty=1:7,legend=c("segmentarrow","curvedarrow","straightarrow",
"treearrow","bentarrow","selfarrow","splitarrow"),lwd=c(rep(2,6),1),col=1:7)

openplotmat(main="textbox shapes")
rx <- 0.1
ry <- 0.05
pos <- coordinates(c(1,1,1,1,1,1,1),mx=-0.2)
textdiamond(mid=pos[1,],radx=rx,rady=ry,lab=LETTERS[1],cex=2,shadow.col="lightblue")
textellipse(mid=pos[2,],radx=rx,rady=ry,lab=LETTERS[2],cex=2,shadow.col="blue")
texthexa(mid=pos[3,],radx=rx,rady=ry,lab=LETTERS[3],cex=2,shadow.col="darkblue")
textmulti(mid=pos[4,],nr=7,radx=rx,rady=ry,lab=LETTERS[4],cex=2,shadow.col="red")
textrect(mid=pos[5,],radx=rx,rady=ry,lab=LETTERS[5],cex=2,shadow.col="darkred")
textround(mid=pos[6,],radx=rx,rady=ry,lab=LETTERS[6],cex=2,shadow.col="black")
textempty(mid=pos[7,],lab=LETTERS[7],cex=2,box.col="yellow")
pos[,1] <- pos[,1] + 0.5
text(pos[,1],pos[,2],c("textdiamond","textellipse","texthexa","textmulti","textrect","textround","textempty"))


mf<-par(mfrow=c(2,2))
example(bentarrow)
example(coordinates)
par(mfrow=c(2,2))
example(curvedarrow)
example(segmentarrow)
example(selfarrow)
example(straightarrow)
par(mfrow=c(2,2))
example(treearrow)
par(mfrow=c(2,2))
example(splitarrow)
par(mfrow=c(2,2))
example(textdiamond)
example(textellipse)
example(textempty)
example(texthexa)
example(textmulti)
example(textplain)
example(textrect)
example(textround)


par(mfrow=mf)