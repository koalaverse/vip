### DEMONSTRATION FOR PLOTMAT
## plots diagram based on a matrix


## SIMPLE PLOTMAT example
par(ask=TRUE)
par(mar=c(1,1,1,1),mfrow=c(2,2))

names <- c("A","B","C","D")
M <- matrix(nrow=4,ncol=4,byrow=TRUE,data=0)
pp<-plotmat(M,pos=c(1,2,1),name=names,lwd=1,box.lwd=2,cex.txt=0.8,
            box.size=0.1,box.type="square",box.prop=0.5)


M[2,1]<-M[3,1]<-M[4,2]<-M[4,3] <- "flow"
pp<-plotmat(M,pos=c(1,2,1),curve=0,name=names,lwd=1,box.lwd=2,cex.txt=0.8,
            box.type="circle",box.prop=1.0)

diag(M) <- "self"
pp<-plotmat(M,pos=c(2,2),curve=0,name=names,lwd=1,box.lwd=2,cex.txt=0.8,
            self.cex=0.5,self.shiftx=c(-0.1,0.1,-0.1,0.1),
            box.type="diamond",box.prop=0.5)

M <- matrix(nrow=4,ncol=4,data=0)
M[2,1]<-1  ;M[4,2]<-2;M[3,4]<-3;M[1,3]<-4
pp<-plotmat(M,pos=c(1,2,1),curve=0.2,name=names,lwd=1,box.lwd=2,cex.txt=0.8,
            arr.type="triangle",box.size=0.1,box.type="hexa",box.prop=0.5)
mtext(outer=TRUE,side=3,line=-1.5,cex=1.5,"plotmat")

##PLOTMAT example 2
names <- c("A","B","C","D")
M <- matrix(nrow=4,ncol=4,byrow=TRUE,data=0)
M[2,1]<-M[3,2]<-M[4,3]<-1

par(mfrow=c(1,2))
pp<-plotmat(M,pos=c(1,1,1,1),curve=0,name=names,lwd=1,box.lwd=2,cex.txt=0.,
            box.size=0.2,box.type="square",box.prop=0.5,arr.type="triangle",
            arr.pos=0.6)
p2 <-plotmat(M[1:2,1:2],pos=pp$comp[c(1,4),],curve=0,name=names[c(1,4)],lwd=1,box.lwd=2,
            cex.txt=0.,box.size=0.2,box.type="square",box.prop=0.5,
            arr.type="triangle",arr.pos=0.6)
text(p2$arr$ArrowX+0.1,p2$arr$ArrowY,font=3,adj=0,"one flow")
par(mfrow=c(1,1))
mtext(outer=TRUE,side=3,line=-1.5,cex=1.5,"plotmat")

# Plotmat example NPZZDD model
names <- c("PHYTO","NH3","ZOO","DETRITUS","BotDET","FISH")
M <- matrix(nrow=6,ncol=6,byrow=TRUE,data=c(
#   p n z  d  b  f
    0,1,0, 0, 0, 0, #p
    0,0,4, 10,11,0, #n
    2,0,0, 0, 0, 0, #z
    8,0,13,0, 0, 12,#d
    9,0,0, 7, 0, 0, #b
    0,0,5, 0, 0, 0  #f
    ))

pp<-plotmat(M,pos=c(1,2,1,2),curve=0,name=names,lwd=1,box.lwd=2,cex.txt=0.8,
            box.type="square",box.prop=0.5,arr.type="triangle",
            arr.pos=0.4,shadow.size=0.01,prefix="f",
            main="NPZZDD model, from Soetaert and herman, 2009, Springer")

# extra arrows: flow 5 to Detritus and flow 2 to detritus
phyto   <-pp$comp[names=="PHYTO"]
zoo     <-pp$comp[names=="ZOO"]
nh3     <-pp$comp[names=="NH3"]
detritus<-pp$comp[names=="DETRITUS"]
fish    <-pp$comp[names=="FISH"]

# flow5->detritus
m2 <- 0.5*(zoo+fish)
m1 <- detritus
m1[1]<-m1[1]+ pp$radii[4,1]
mid<-straightarrow (to=m1,from=m2,arr.type="triangle",arr.pos=0.4,lwd=1)
text(mid[1],mid[2]+0.03,"f6",cex=0.8)

# flow2->detritus
m2 <- 0.5*(zoo+phyto)
m1 <- detritus
m1[1] <-m1[1]+ pp$radii[3,1]*0.2
m1[2]<-m1[2] + pp$radii[3,2]
mid<-straightarrow (to=m1,from=m2,arr.type="triangle",arr.pos=0.3,lwd=1)
text(mid[1]-0.01,mid[2]+0.03,"f3",cex=0.8)



# TRANSITION MATRIX

par(mfrow=c(2,1))

#labels as formulae
Numgenerations   <- 6

# Original Population matrix 
DiffMat  <- matrix(data=0,nrow=Numgenerations,ncol=Numgenerations)   # declare it
AA <- as.data.frame(DiffMat)
AA[[1,4]]<- "f[3]"
AA[[1,5]]<- "f[4]"
AA[[1,6]]<- "f[5]"

AA[[2,1]]<- "s[list(0,1)]"
AA[[3,2]]<- "s[list(1,2)]"
AA[[4,3]]<- "s[list(2,3)]"
AA[[5,4]]<- "s[list(3,4)]"
AA[[6,5]]<- "s[list(4,5)]"

name  <- c("Age0","Age1","Age2","Age3","Age4","Age5")

PP <- plotmat(A=AA,pos=6,curve=0.7,name=name,lwd=2,arr.len=0.6,arr.width=0.25,my=-0.2,
              box.size=0.05,arr.type="triangle",dtext= 0.95,cex.txt=0,
              main="Age-structured population model 1")

for (i in 1:nrow(PP$arr))
  text(as.double(PP$arr[i,"TextX"]),as.double(PP$arr[i,"TextY"]),
  parse(text=as.character(PP$arr[i,"Value"])))

# reduced population matrix
Numgenerations   <- Numgenerations-1
DiffMat          <- DiffMat[-1,-1]
AA <- as.data.frame(DiffMat)
AA[[1,3]]<- "f[3]*s[list(0,1)]"
AA[[1,4]]<- "f[4]*s[list(0,1)]"
AA[[1,5]]<- "f[5]*s[list(0,1)]"

AA[[2,1]]<- "s[list(0,2)]"
AA[[3,2]]<- "s[list(2,3)]"
AA[[4,3]]<- "s[list(3,4)]"
AA[[5,4]]<- "s[list(4,5)]"

name  <- c("Age0","Age2","Age3","Age4","Age5")

pos <- PP$comp[-1,]
PP <- plotmat(AA,pos=pos,curve=0.7,name=name,lwd=2,arr.len=0.6,arr.width=0.25,my=-0.1,
              box.size=0.05,arr.type="triangle",dtext= 0.95,cex.txt=0,main="Age-structured population model 2")
for (i in 1:nrow(PP$arr))
  text(as.double(PP$arr[i,"TextX"]),as.double(PP$arr[i,"TextY"]),
  parse(text=as.character(PP$arr[i,"Value"])))

par(mfrow=c(1,1),mar=c(2,2,2,2))


#################3
par(mfrow=c(1,1))
par(mar=c(4,4,4,4))
par(xaxs="r",yaxs="r")
# Fecundity and Survival for each generation
NumClass    <- 10
Fecundity   <- c(0,      0.00102,0.08515,0.30574,0.40002,
                 0.28061,0.1526 ,0.0642 ,0.01483,0.00089)
Survival    <- c(0.9967 ,0.99837,0.9978 ,0.99672,0.99607,
                 0.99472,0.99240,0.98867,0.98274,NA)            # survival from i to i+1

cbind(Fecundity,Survival)

# Population matrix M
DiffMatrix       <- matrix(data=0,nrow=NumClass,ncol=NumClass)     # declare it
DiffMatrix[1,]   <- Fecundity                                      # first row: fecundity
for (i in 1:(NumClass-1))  DiffMatrix[i+1,i] <- Survival[i]

DiffMatrix                                                         # print the matrix to screen
names <- c("0-5yr","5-10yr","10-15yr","15-20yr","20-25yr","25-30yr","30-35yr","35-40yr","40-45yr","45-50yr")
# first generation will be positioned in middle; other generations on a circle
pos <- coordinates(NULL,N=NumClass-1)
pos <- rbind(c(0.5,0.5),pos)
curves <- DiffMatrix
curves[]   <- -0.4
curves[1, ] <- 0
curves[2,1] <- -0.125
curves[1,2] <- -0.125
plotmat(main="US population, life cycle, 1966",DiffMatrix,pos=pos,name=names,curve=curves,lcol="darkblue",arr.col="lightblue",
        box.size=0.07,arr.type="triangle",cex.txt=0.8,box.col="lightyellow",box.prop =1)

#####

A        <- matrix(nrow=7,ncol=7,NA)
A[,1]    <- 1 ; A[1,1]<-0
pos <- coordinates(NULL,N=6,relsize=0.8)       # 6 boxes in circle
pos <- rbind(c(0.5,0.5),pos)       # one in middle

plotmat(A,pos=pos,lwd=1,curve=0,box.lwd=2,cex.txt=0.8,box.col=2:8, 
        box.cex=0.8,box.size=0.125,arr.length=0.5,box.type=c("multi","rect","ellipse"),
        shadow.size = 0.01,nr=5,main="plotmat")


# TRANSITION MATRIX EXAMPLE
# dataset Teasel

curves <- matrix(nrow=ncol(Teasel),ncol=ncol(Teasel),0)
curves[3,1]<- curves[1,6]<- -0.35             
curves[4,6]<-curves[6,4]<-curves[5,6]<-curves[6,5]<-0.08
curves[3,6]<-  0.35

plotmat(Teasel,pos=c(3,2,1),curve=curves,name=colnames(Teasel),lwd=1,box.lwd=2,cex.txt=0.8, 
        box.cex=0.8,box.size=0.08,arr.length=0.5,box.type="circle",box.prop=1,
        shadow.size = 0.01,self.cex=0.6,my=-0.075, mx=-0.01,relsize=0.9,
        self.shiftx=c(0,0,0.125,-0.12,0.125,0),self.shifty=0,main="Teasel population model")

