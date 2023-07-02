# the interactive plotrix demo
par(ask=FALSE)
answer<-"Z"
whichplot<-"Z"
while(answer != "Q" && whichplot != "X") {
cat("1. Plots B-K\n2. Plots L-Z\n3. Enhancements A-L\n")
cat("4. Enhancements L-V\nQ. Quit\n")
answer<-toupper(readline("Choose a group - "))
if(answer == "Q") break
if(answer=="1") {
 cat("1. barNest - Plot nested breakdowns as superimposed bars\n")
 cat("2. barp - A bar plotting routine similar to barplot\n")
 cat("3. battleship.plot - Plot the values of a matrix as stacked rectangles\n")
 cat("4. box.heresy - An unconventional box plot\n")
 cat("5. brkdn.plot - Plot aggregate values from groups defines by factors\n")
 cat("6. bumpchart - A league table by time plot\n")
 cat("7. centipede.plot - A league table (ranking) plot\n")
 cat("8. clock24.plot - Plot values on a 24 hour 'clockface'\n")
 cat("9. clustered.dotplots - a sort of graphical crosstabulation\n")
 cat("A. color2D.matplot - Display a numeric matrix as colors\n")
 cat("B. color.scale.lines - Plot lines with colors dependent upon values\n")
 cat("C. dendroPlot - Display distributions as dendrites\n")
 cat("D. densityGrid - Overlay observation density and intensity on a map\n")
 cat("E. diamondplot - Plot variables as polygons on a radial grid\n")
 cat("F. dotplot.mtb - Minitab style dotplot\n")
 cat("G. ehplot - Englemann-Hecker plot\n")
 cat("H. election - Display party affiliations by color\n")
 cat("I. fan.plot - Like a pie chart with overlaid sectors\n")
 cat("J. feather.plot - Display vectors along a horizontal line\n")
 cat("K. floating.pie - Display one or more pie charts\n")
 cat("L. gantt.chart - Display a Gantt chart\n")
 cat("M. gap.barplot - A bar plot with a specified gap\n")
 cat("N. gap.boxplot - A box plot with a specified gap\n")
 cat("O. gap.plot - A scatterplot with a specified gap\n")
 cat("P. histStack - Display a stacked histogram\n")
 cat("Q. intersectDiagram - Display set intersections as rectangles\n")
 cat("R. joyPlot - Display a series of density or other curves\n")
 cat("S. kiteChart - Display a matrix of values as polygon segments\n")
 cat("X. Exit\n")
 #par(ask=TRUE)
 whichplot<-toupper(readline("Choose a plot - "))
 if(whichplot == "1") {
  test.df<-data.frame(Age=rnorm(100,25,10),
   Sex=sample(c("M","F"),100,TRUE),
   Marital=sample(c("M","X","S","W"),100,TRUE),
   Employ=sample(c("FT","PT","NO"),100,TRUE))
  test.col<-list(Overall="green",Employ=c("purple","orange","brown"),
   Marital=c("#1affd8","#caeecc","#f7b3cc","#94ebff"),Sex=c(2,4))
  barNest(formula=Age~Employ+Marital+Sex,data=test.df,main="barNest",
   col=test.col,showall=TRUE,ylab="Mean age")
 }
 if(whichplot == "2") {
  happyday<-data.frame(Monday=c(2.3,3.4),Tuesday=c(2.8,3.3),Wednesday=c(3.2,3.1),
  Thursday=c(3.6,2.8),Friday=c(4.2,2.6),Saturday=c(4.5,2.9),Sunday=c(4.1,2.8))
  happylabels<-c("Utterly dashed","Rather mopey","Indifferent","Somewhat elated",
   "Euphoric")
  barp(happyday,names.arg=names(happyday),legend.lab=c("Slaves","Unemployed"),
   legend.pos=list(x=2,y=4.5),col=c("#ee7700","#3333ff"),
   main="Test of barp, staxlab and color.legend",
   xlab="Day of week",ylab="Happiness rating",ylim=c(1,5),staxx=TRUE,staxy=TRUE,
   height.at=1:5,height.lab=happylabels,cex.axis=1,cylindrical=TRUE,
   shadow=TRUE)
  par(mar=c(5,4,4,2))
  h1<-table(cut(rnorm(100,4),breaks=seq(0,8,by=2)))
  h2<-table(cut(rnorm(100,4),breaks=seq(0,8,by=2)))
  h3<-table(cut(rnorm(100,4),breaks=seq(0,8,by=2)))
  hmat<-matrix(c(h1,h2,h3),nrow=3,byrow=TRUE)
  barp(hmat,names.arg=names(h1),width=0.45,col=2:4,
   main="Multiple histogram using barp",xlab="Bins",ylab="Frequency")
  legend(3.8,50,c("h1","h2","h3"),fill=2:4)
 }
 if(whichplot == "3") {
  x<-matrix(sample(10:50,100,TRUE),10)
   xaxlab=c("One","Two","Three","Four","Five","Six","Seven","Eight","Nine","Ten")
   yaxlab=c("First","Second","Third","Fourth","Fifth","Sixth","Seventh",
    "Eighth","Ninth","Tenth")
  battleship.plot(x,xlab="The battle has just begun",main="Battleship1",
   xaxlab=xaxlab,yaxlab=yaxlab)
 }
 if(whichplot == "4") {
  y1<-runif(20,2,10)
  y2<-rnorm(30,6,2)
  y3<-sample(0:20,40,TRUE)
  Ns<-c(20,30,40)
  ymean<-c(mean(y1),mean(y2),mean(y3))
  y1inner<-quantile(y1,probs=c(.16,.84))
  y2inner<-c(ymean[2]+sd(y2),ymean[2]-sd(y2))
  y3inner<-quantile(y3,probs=c(.16,.84))
  uinner<-c(y1inner[1],y2inner[1],y3inner[1])
  linner<-c(y1inner[2],y2inner[2],y3inner[2])
  ulim<-c(max(y1),max(y2),max(y3))
  llim<-c(min(y1),min(y2),min(y3))
  box.heresy(ymean,uinner=uinner,linner=linner,ulim=ulim,llim=llim,boxwidth=Ns,
   main="Boxplot of means, central spread and range",xlab="Distribution",
   xaxlab=c("Uniform","Normal","Sample"))
 }
 if(whichplot == "5") {
  test.df<-data.frame(a=rnorm(80)+4,b=rnorm(80)+4,c=rep(LETTERS[1:4],each=20),
   d=rep(rep(letters[1:4],each=4),5))
  # first use the default values
  brkdn.plot("a","c","d",test.df,pch=1:4,col=1:4)
 }
 if(whichplot == "6") {
  educattn<-matrix(c(90.4,90.3,75.7,78.9,66,71.8,70.5,70.4,68.4,67.9,
   67.2,76.1,68.1,74.7,68.5,72.4,64.3,71.2,73.1,77.8),ncol=2,byrow=TRUE)
  rownames(educattn)<-c("Anchorage AK","Boston MA","Chicago IL",
   "Houston TX","Los Angeles CA","Louisville KY","New Orleans LA",
   "New York NY","Philadelphia PA","Washington DC")
  colnames(educattn)<-c(1990,2000)
  bumpchart(educattn,rank=FALSE,
   main="Percentage high school completion by over 25s",col=rainbow(10))
  par(mar=c(5,5,4,2))
 }
 if(whichplot == "7") {
  testcp<-list("",40)
  for(i in 1:40) testcp[[i]]<-rnorm(sample(1:8,1)*50)
  segs<-get.segs(testcp)
  centipede.plot(segs,main="Centipede plot",vgrid=0)
  xy.mat<-cbind(sample(1:10,200,TRUE),sample(1:10,200,TRUE))
 }
 if(whichplot == "8") {
  testlen<-rnorm(24)*2+5
  testpos<-0:23+rnorm(24)/4
  clock24.plot(testlen,testpos,main="Test Clock24 (lines)",show.grid=FALSE,
   line.col="green",lwd=3)
 }
 if(whichplot == "9") {
  data(mtcars)
  cumcars<-by(mtcars$carb,list(mtcars$cyl,mtcars$gear),valid.n)
  mtcars2<-data.frame(cyl=NA,gear=NA,carb=NA)
  rownum<-1
  for(cyl in dimnames(cumcars)[[1]]) {
   for(gear in dimnames(cumcars)[[2]]) {
    if(!is.na(cumcars[cyl,gear])) {
     mtcars2[rownum,]<-c(as.numeric(cyl),as.numeric(gear),cumcars[cyl,gear])
     rownum<-rownum+1
    }
   }
  }
  clustered.dotplots(xgroup = mtcars2$cyl, ygroup = mtcars2$gear,
   freq = mtcars2$carb,main="Cars by number of cylinders and gears",
   xlab="Number of cylinders",ylab="Number of gears",type="points",pch=5)
 }
 if(whichplot == "A") {
  x<-matrix(rnorm(1024),nrow=32)
  # simulate a correlation matrix with values -0.5 to 0.5
  x<-rescale(x,c(-0.5,0.5))
  # add a column with the extreme values (-1,1) to calculate
  # the colors, then drop the extra column in the result
  cellcol<-color.scale(cbind(x,c(-1,rep(1,31))),c(0,1),0,c(1,0))[,1:32]
  color2D.matplot(x,cellcolors=cellcol,main="Blue to red correlations")
  # do the legend call separately to get the full range
  color.legend(0,-4,10,-3,legend=c(-1,-0.5,0,0.5,1),
   rect.col=color.scale(c(-1,-0.5,0,0.5,1),c(0,1),0,c(1,0)),align="rb")
 }
 if(whichplot == "B") {
  x<-c(0,cumsum(rnorm(99)))
  y<-c(0,cumsum(rnorm(99)))
  xydist<-sqrt(x*x+y*y)
  plot(x,y,main="Random walk plot (color.scale.lines)",xlab="X",ylab="Y",type="n")
  color.scale.lines(x,y,c(1,1,0),0,c(0,1,1),colvar=xydist,lwd=2)
  boxed.labels(x,y,labels=1:100,border=FALSE,cex=0.5)
 }
 if(whichplot == "C") {
  x<-list(runif(90,1,2),factor(sample(LETTERS,100,TRUE)),rnorm(80,mean=5))
  dendroPlot(x,breaks=list(seq(1,2,by=0.1),0,0:10),nudge=c(0.03,0.3),
   xlab="Groups",ylab="Counts",main="Test dendroPlot")
 }
 if(whichplot == "D") {
  x<-sample(1:20,400,TRUE)
  y<-sample(1:20,400,TRUE)
  z<-runif(400,5,20)
  xyz<-makeDensityMatrix(x,y,z,nx=20,ny=20,xlim=c(1,10),ylim=c(1,10),
   geocoord=FALSE)
  par(mar=c(7,3,2,3))
  plot(0,xlim=c(1,10),ylim=c(1,10),type="n",xlab="",axes=FALSE)
  box()
  densityGrid(xyz,range.cex=c(1,4),xlim=c(1,10),ylim=c(1,10),
   red=c(0,0.5,0.8,1),green=c(1,0.8,0.5,0),blue=0,pch=15)
  color.legend(3,-0.7,7,-0.2,c(5,10,15,20),
   rect.col=color.scale(1:4,cs1=c(0,0.5,0.8,1),cs2=c(1,0.8,0.5,0),cs3=0,alpha=1))
  par(xpd=TRUE)
  text(5,0.3,"Intensity")
  points(c(3.5,4.5,5.5,6.5),rep(-1.7,4),pch=15,cex=1:4)
  text(c(3.5,4.5,5.5,6.5),rep(-1.3,4),1:4)
  text(5,-1,"Density")
  par(xpd=FALSE)
 } 
 if(whichplot == "E") {
  data(mtcars)
  mysubset<-mtcars[substr(dimnames(mtcars)[[1]],1,1)=="M",c("mpg","hp","wt","disp")]
  diamondplot(mysubset,name="Diamondplot")
 } 
 if(whichplot == "F") {
  x <- rpois(100,10)
  dotplot.mtb(x,yaxis=TRUE,main="Minitab dotplot with y-axis.")
 } 
 if(whichplot == "G") {
  data(iris)
  ehplot(iris$Sepal.Length, iris$Species, intervals=20, cex=1.8, pch=20)
 } 
 if(whichplot == "H") {
  eu = structure(list(colour = c("#3399FF", "#F0001C", "#0054A5",
  "#FFD700", "#990000", "#909090", "#32CD32", "#40E0D0"),
   party = c("EPP", "S and D", "ECR", "ALDE", "GUE-NGL",
   "Non-Inscrits", "Greens-EFA", "EFDD"),
   members = c(220L, 191L, 70L, 68L, 52L, 52L, 50L, 48L)),
  .Names = c("colour", "party", "members"), row.names = c(NA,
  -8L), class = "data.frame")
  strasbourg = seats(751, 16)
  eugov = election(strasbourg, eu, party~members, colours=eu$colour)
  oldmar<-par(mar=c(2,4,4,2))
  plot(eugov$x, eugov$y, col=eugov$colour, asp=1, pch=19, ylim=c(-2,2.5),
   xlab="", ylab="", main="EU Parliament 2014", axes=FALSE)
  legend(-0.7,-0.3,eu$party,fill=eu$colour)
  par(oldmar)
 } 
 if(whichplot == "I") {
  iucn.df<-data.frame(area=c("Africa","Asia","Europe","N&C America",
   "S America","Oceania"),threatened=c(5994,7737,1987,4716,5097,2093))
  fan.plot(iucn.df$threatened,max.span=pi,
   labels=paste(iucn.df$area,iucn.df$threatened,sep="-"),
   main="Threatened species by geographical area (fan.plot)",ticks=276)
 } 
 if(whichplot == "J") {
  feather.plot(0.6+rnorm(8)/5,seq(0,7*pi/4,by=pi/4),1:8,
   main="Test of feather.plot",xlab="Time",ylab="Value")
 } 
 if(whichplot == "K") {
  plot(1:5,type="n",main="Floating Pie test",xlab="",ylab="",axes=FALSE)
  box()
  polygon(c(0,0,5.5,5.5),c(0,3,3,0),border="#44aaff",col="#44aaff")
  floating.pie(1.7,3,c(2,4,4,2,8),radius=0.5,
   col=c("#ff0000","#80ff00","#00ffff","#44bbff","#8000ff"))
  floating.pie(3.1,3,c(1,4,5,2,8),radius=0.5,
   col=c("#ff0000","#80ff00","#00ffff","#44bbff","#8000ff"))
  floating.pie(4,1.5,c(3,4,6,7),radius=0.5,
   col=c("#ff0066","#00cc88","#44bbff","#8000ff"))
  draw.circle(3.9,2.1,radius=0.04,col="white")
  draw.circle(3.9,2.1,radius=0.04,col="white")
  draw.circle(3.9,2.1,radius=0.04,col="white")
  draw.circle(4,2.3,radius=0.04,col="white")
  draw.circle(4.07,2.55,radius=0.04,col="white")
  draw.circle(4.03,2.85,radius=0.04,col="white")
  text(c(1.7,3.1,4),c(3.7,3.7,3.7),c("Pass","Pass","Fail"))
 } 
 if(whichplot == "L") {
  Ymd.format<-"%Y/%m/%d"
  gantt.info<-list(labels=
   c("First task","Second task","Third task","Fourth task","Fifth task"),
  starts=as.POSIXct(strptime(
  c("2004/01/01","2004/02/02","2004/03/03","2004/05/05","2004/09/09"),
  format=Ymd.format)),
  ends=as.POSIXct(strptime(
  c("2004/03/03","2004/05/05","2004/05/05","2004/08/08","2004/12/12"),
  format=Ymd.format)),
  priorities=c(1,2,3,4,5))
  vgridpos<-as.POSIXct(strptime(c("2004/01/01","2004/02/01","2004/03/01",
  "2004/04/01","2004/05/01","2004/06/01","2004/07/01","2004/08/01",
  "2004/09/01","2004/10/01","2004/11/01","2004/12/01"),format=Ymd.format))
  vgridlab<-
  c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
  gantt.chart(gantt.info,main="Calendar date Gantt chart (2004)",
   priority.legend=TRUE,vgridpos=vgridpos,vgridlab=vgridlab,hgrid=TRUE)
 }
 if(whichplot == "M") {
  twogrp<-c(rnorm(10)+4,rnorm(10)+20)
  gap.barplot(twogrp,gap=c(8,16),xlab="Index",ytics=c(3,6,17,20),
  ylab="Group values",main="gap.barplot")
 } 
 if(whichplot == "N") {
  twovec<-list(vec1=c(rnorm(30),-6),vec2=c(sample(1:10,40,TRUE),20))
  gap.boxplot(twovec,gap=list(top=c(12,18),bottom=c(-5,-3)),
   main="Test gap.boxplot")
 }
 if(whichplot == "O") {
  twogrp<-c(rnorm(5)+4,rnorm(5)+20,rnorm(5)+5,rnorm(5)+22)
  gpcol<-c(2,2,2,2,2,3,3,3,3,3,4,4,4,4,4,5,5,5,5,5)
  gap.plot(twogrp,gap=c(8,16),xlab="Index",ylab="Group values",
   main="Test gap.plot",col=gpcol)
 }
 if(whichplot == "P") {
  df<-data.frame(len=rnorm(100)+5,
   grp=sample(c("A","B","C","D"),100,replace=TRUE))
  histStack(len~grp,data=df,main="Default (rainbow) colors",
   xlab="Length category")
 }
 if(whichplot == "Q") {
  druguse<-matrix(c(sample(c(0,1),200,TRUE,prob=c(0.15,0.85)),
   sample(c(0,1),200,TRUE,prob=c(0.35,0.65)),
   sample(c(0,1),200,TRUE,prob=c(0.5,0.5)),
   sample(c(0,1),200,TRUE,prob=c(0.9,0.1))),ncol=4)
  colnames(druguse)<-c("Alc","Tob","THC","Amp")
  druglist<-makeIntersectList(druguse,sep="\n")
  # first display it as counts
  intersectDiagram(druglist,main="Patterns of drug use",sep="\n")
 }
 if(whichplot == "R") {
  numbmat<-matrix(runif(500,0,1),nrow=10)
  denslist<-apply(numbmat,1,density)
  names(denslist)<-month.abb[1:10]
  joyPlot(denslist,main="Test of joyPlot",fill="lightgray")
 }
  if(whichplot == "S") {
  testmat<-matrix(c(runif(50),sample(1:50,50),rnorm(50)+5,
   sin(1:50)),ncol=50,byrow=TRUE)
  kiteChart(testmat,varlabels=c("Uniform","Sample","Normal","Sine"),
   timepos=seq(1,50,by=5),timex=FALSE)
  # not enough space for the last label, add it
  mtext("Sine",at=65,side=1,line=2)
 }
 if(whichplot=="X") break
}
if(answer == "2") {
 cat("1. labbePlot - Display a L'Abbe plot - successes as sizes of circles\n")
 cat("2. ladderplot - Plot 1D scatterplots with connecting lines\n")
 cat("3. multhist - Histogram for multiple series\n")
 cat("4. oz.windrose - Australian Bureau of Meteorology wind rose\n")
 cat("5. panes - Prepare a 'panel' type plot\n")
 cat("6. perspx - Perspective plot\n")
 cat("7. pie3D - 3D pie chart\n")
 cat("8. plotH - Scatterplot with histogram-like bars\n")
 cat("9. polar.plot - Plot values on a 360 degree chart\n")
 cat("A. pyramid.plot - Pyramid plot\n")
 cat("B. radial.pie - Plot sectors/annuli on a circular grid\n")
 cat("C. radial.plot - Plot values on a 0 to 2*pi grid\n")
 cat("D. raw.means.plot - Plot for experimental designs\n")
 cat("E. sizeplot - Plot with repeated symbols by size\n")
 cat("F. sizetree - Categorical breakdown as stacked rectangles\n")
 cat("G. size_n_color - Display circles with specified size and color\n")
 cat("H. stackpoly - Like a line plot with fill under the lines\n")
 cat("I. staircase.plot - Display a staircase plot\n")
 cat("J. taylor.diagram - Display a Taylor diagram\n")
 cat("K. triax.plot - Triangle (three axis) plot\n")
 cat("L. twoord.plot - Plot with two ordinates\n")
 cat("M. vectorField - Diaplay magnitude/direction vectors\n")
 cat("N. violin_plot - Display a violin plot\n")
 cat("O. weighted.hist - Display a weighted histogram\n")
 cat("P. zoomInPlot - Display a plot with a magnified section\n")
 cat("X. Exit\n")
 whichplot<-toupper(readline("Choose a plot - "))
 if(whichplot == "1") {
  didf<-data.frame(subject=1:50,interv=rep(c("therapist","ex-drinker"),each=25),
   outcome=sample(c("more","less"),50,TRUE))
  # make it into a table
  didf.tab<-table(didf$interv,didf$outcome)
  # now mix in some raw percentages just for the example
  didf2<-c(74,46,200)
  didf3<-c(33,87,500)
  x<-list(didf.tab,didf2,didf3)
  labbecol<-list("red","green","blue")
  labbePlot(x,main="Ex-drinkers vs therapists",
   xlab="Percent reduced drinking (ex-drinkers)",
   ylab="Percent reduced drinking (therapists)",
  labels=list("A","B52","X117"),col=labbecol)
  labbePlot(list(c(20,40,20)),col=list("purple"),labels=list("Z"),add=TRUE)
 }
 if(whichplot == "2") {
  x<-data.frame(A=c(1:10), B=c(2:11)+rnorm(10))
  y<-data.frame(x, C=c(1:10)+rnorm(10))
  ladderplot(x)
 }
 if(whichplot == "3") {
  l<-list(runif(10)*10,1:10,c(1,1,1,1,4,8))
  multhist(l)
 }
 if(whichplot == "4") {
  windagg<-matrix(c(8,0,0,0,0,0,0,0,4,6,2,1,6,3,0,4,2,8,5,3,5,2,1,1,
   5,5,2,4,1,4,1,2,1,2,4,0,3,1,3,1),nrow=5,byrow=TRUE)
  oz.windrose(windagg)
 }
 if(whichplot == "5") {
  y<-runif(8)
  oldpar<-panes(matrix(1:4,nrow=2,byrow=TRUE))
  par(mar=c(0,2,1.6,0))
  boxplot(y,axes=FALSE)
  axis(2)
  box()
  par(mar=c(0,0,1.6,2))
  tab.title("Boxplot of y",tab.col="#88dd88")
  barplot(y,axes=FALSE,col=2:9)
  axis(4)
  box()
  tab.title("Barplot of y",tab.col="#88dd88")
  par(mar=c(2,2,1.6,0))
  pie(y,col=2:9)
  tab.title("Pie chart of y",tab.col="#88dd88")
  box()
  par(mar=c(2,0,1.6,2))
  plot(y,xaxs="i",xlim=c(0,9),axes=FALSE,col=2:9)
  axis(4)
  box()
  tab.title("Scatterplot of y",tab.col="#88dd88")
  # center the title at the left edge of the last plot
  mtext("Test of panes function",at=0,side=1,line=0.8,cex=1.5)
  par(mfrow=c(1,1))
 }
 if(whichplot == "6") {
  x <- 1:10
  y <- 1:10
  z <- outer(x,y,function(x,y) { 3*sin(2*pi*x)/(2*pi*x)+exp(y/10)+(x*y)/1000 })
  par(mar=c(5,10,2,2))
  pp <- perspx(x,y,z,ticktype="detailed",phi=30,theta=80,nticks=3,r=10,
   axes=FALSE)
  par(mar=c(5,4,4,2))
 }
 if(whichplot == "7") {
  pieval<-c(2,4,6,8)
  pielabels<-c("We hate\n pies","We oppose\n  pies",
   "We don't\n  care","We just love pies")
  # grab the radial positions of the labels
  lp<-pie3D(pieval,radius=0.9,labels=pielabels,explode=0.1,
   main="3D PIE OPINIONS")
  par(mar=c(5,4,4,2))
 }
 if(whichplot == "8") {
  d<-data.frame(x=c(1,5,10:20),y=runif(13)+1,
   g=factor(sample(c("A","B","C"),13,replace=TRUE)))
  # new plotH function with formula notation
  plotH(y~x,data=d)
 }
 if(whichplot == "9") {
  testlen<-c(rnorm(36)*2+5)
  testpos<-seq(0,350,by=10)
  polar.plot(testlen,testpos,main="Test Polar Plot",lwd=3,line.col=4)
 }
 if(whichplot == "A") {
  xy.pop<-c(3.2,3.5,3.6,3.6,3.5,3.5,3.9,3.7,3.9,3.5,3.2,2.8,2.2,1.8,
   1.5,1.3,0.7,0.4)
  xx.pop<-c(3.2,3.4,3.5,3.5,3.5,3.7,4,3.8,3.9,3.6,3.2,2.5,2,1.7,1.5,
   1.3,1,0.8)
  agelabels<-c("0-4","5-9","10-14","15-19","20-24","25-29","30-34",
   "35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74",
   "75-79","80-44","85+")
  mcol<-color.gradient(c(0,0,0.5,1),c(0,0,0.5,1),c(1,1,0.5,1),18)
  fcol<-color.gradient(c(1,1,0.5,1),c(0.5,0.5,0.5,1),c(0.5,0.5,0.5,1),18)
  par(mar=pyramid.plot(xy.pop,xx.pop,labels=agelabels,
   main="Australian population pyramid 2002",lxcol=mcol,rxcol=fcol,
   gap=0.5,show.values=TRUE))
 }
 if(whichplot == "B") {
  pie1<-c(3,6,5,4,7,8,9,1,4)
  pie2<-list(0:3,1:6,2:5,1:4,0:7,4:8,2:9,0:1,0:4)
  pie3<-sample(10:60,36)
  pie4<-list(sort(sample(1:60,8)))
  for(sector in 2:36) pie4[[sector]]<-sort(sample(1:60,8))
  par(radial.pie(pie1,labels=LETTERS[1:9]))
 }
 if(whichplot == "C") {
  testlen<-runif(10,0,10)
  testpos<-seq(0,18*pi/10,length=10)
  testlab<-letters[1:10]
  par(radial.plot(testlen,testpos,main="Test Radial Lines",line.col="red",
   lwd=3,rad.col="lightblue"))
 }
 if(whichplot == "D") {
  x <- data.frame(id = 1:150,
   offset = rep(c("Group A", "Group B", "Group C"),
   each = 50), xaxis = sample(c("A", "B", "C", "D"),150, replace = TRUE),
   data = c(rnorm(50, 10, 5), rnorm(50, 15,6), rnorm(50, 20, 5)))
  raw.means.plot(x)
 }
 if(whichplot == "E") {
  x <- c(0.1,0.1,0.1,0.1,0.1,0.2,0.2,0.2,0.2,0.3,0.3)
  y <- c( 1,  1,  1,  1,  2,  2,  2,  3,  3,  4,  5 )
  plot(x,y)
  sizeplot(x,y)
 }
 if(whichplot == "F") {
  cat1<-factor(sample(c("None","Low","Medium","High","Extreme"),40,TRUE),
   levels=c("None","Low","Medium","High","Extreme"))
  cat2<-factor(sample(c("None","Low","Medium","High"),40,TRUE),
   levels=c("None","Low","Medium","High"))
  cat3<-factor(sample(c("None","Low","High"),40,TRUE),
   levels=c("None","Low","High"))
  hcats<-data.frame(cat1,cat2,cat3)
  # throw in a few NAs
  hcats$cat1[10]<-NA
  hcats$cat2[c(15,20)]<-NA
  hcats$cat3[c(11,14,25)]<-NA
  # first let sizetree work out the colors
  sizetree(hcats,main="Sizetree with automatic colors")
  par(mar=c(5,4,4,2))
 }
 if(whichplot == "G") {
  meantemp<-c(19,22,25,29,21,20,16,27,23,26)
  totalrain<-c(174,152,196,120,177,183,92,153,161,85)
  numpumpkin<-c(53,47,61,63,38,42,48,71,66,29)
  meanwt<-c(1.5,2.3,2.8,1.9,2.4,1.8,2.6,2.2,1.7)
  size_n_color(meantemp,totalrain,meanwt/5,NA,xlim=c(15,30),
   color.scale(numpumpkin,c(0.8,0),c(0.8,1),0),
   xlab="Temperature (degrees C)",ylab="Rainfall (mm)",
   main="Number and weight of pumpkins by temperature and rainfall",
   xat=seq(15,30,by=5),yat=seq(80,200,by=20))  
  color.legend(15,55,18.5,60,seq(40,70,by=10),
   rect.col=color.scale(seq(40,70,by=10),c(0.8,0),c(0.8,1),0))
  points(15:18,rep(126,4),cex=seq(1.5,3.0,by=0.5))
  text(15:19,rep(134,5),c("1.5","2.0","2.5","3.0","kg"))
  par(xpd=TRUE)
  text(13.5,60,"Number of\npumpkins")
  par(xpd=FALSE)
 }
 if(whichplot == "H") {
  testx<-matrix(abs(rnorm(100)),nrow=10)
  stackpoly(matrix(cumsum(testx),nrow=10),main="Test Stackpoly I",
   xaxlab=c("One","Two","Three","Four","Five",
   "Six","Seven","Eight","Nine","Ten"),border="black",staxx=TRUE)
 }
 if(whichplot == "I") {
  sample_size<-c(500,-72,428,-94,334,-45,289)
  totals<-c(TRUE,FALSE,TRUE,FALSE,TRUE,FALSE,TRUE)
  labels<-c("Contact list","Uncontactable","","Declined","","Ineligible",
   "Final sample")
  staircase.plot(sample_size,totals,labels,
   main="Acquisition of the sample (staircase.plot)",
   total.col="gray",inc.col=2:4,bg.col="#eeeebb",direction="s")
 }
 if(whichplot == "J") {
  ref<-rnorm(30,sd=2)
  model1<-ref+rnorm(30)/2
  model2<-ref+rnorm(30)
  oldpar<-taylor.diagram(ref,model1)
  taylor.diagram(ref,model2,add=TRUE,col="blue")
  lpos<-1.5*sd(ref)
  legend(lpos,lpos,legend=c("Better","Worse"),pch=19,col=c("red","blue"))
  par(oldpar)
 }
 if(whichplot == "K") {
  data(soils)
  triax.plot(soils[1:10,],main="DEFAULT")
 }
 if(whichplot == "L") {
  going_up<-seq(3,7,by=0.5)+rnorm(9)
  going_down<-rev(60:74)+rnorm(15)
  twoord.plot(2:10,going_up,1:15,going_down,xlab="Sequence",
   ylab="Ascending values",rylab="Descending values",lcol=4,
   main="Plot with two ordinates - points and lines",
   do.first="plot_bg();grid(col=\"white\",lty=1)")
 }
 if(whichplot == "M") {
  plot(1:10,type="n",main="Random vectors")
  mag<-runif(100)+1
  dir<-runif(100)*2*pi
  xpos<-rep(1:10,10)
  ypos<-rep(1:10,each=10)
  vectorcol<-sample(colors(),100)
  vectorField(dir,mag,xpos,ypos,scale=0.8,vecspec="rad",col=vectorcol)
 }
 if(whichplot == "N") {
  normvar<-c(rnorm(49),-4)
  unifvar<-runif(50,-2,2)
  violin_plot(matrix(c(normvar,unifvar),ncol=2),
   main="Default plot",x_axis_labels=c("Normal","Uniform"))
 }
 if(whichplot == "O") {
  testx<-sample(1:10,300,TRUE)
  testw<-seq(1,4,by=0.01)
  weighted.hist(testx,testw,breaks=1:10,main="Test weighted histogram")
 }
 if(whichplot == "P") {
  zoomInPlot(rnorm(100),rnorm(100),rxlim=c(-1,1),rylim=c(-1,1),
   zoomtitle="Zoom In Plot",titlepos=-1.5)
 }
 if(whichplot=="X") break
}
if(answer=="3") {
 cat("1. ablineclip - add a line to a plot clipped to a specified rectangle\n")
 cat("2. addtable2plot - Add a table of values to a plot\n")
 cat("3. arctext - Display text on a circular arc\n")
 cat("4. axis.break - Add a 'break' mark to an axis\n")
 cat("5. axis.mult - Display an axis with a multiplier value\n")
 cat("6. barlabels - Add bar labels to a bar plot\n")
 cat("7. boxed.labels - Add labels with optional boxes around them\n")
 cat("8. clean.args - Remove inappropriate arguments from a list\n")
 cat("9. color.id - identify closest match to a named color\n")
 cat("A. color.legend - Legend matching categories or values to colors\n")
 cat("B. color.scale - Turn values into colors\n")
 cat("C. corner.label - Find the corners of the plot and display a label\n")
 cat("D. cylindrect - Display an apparent cylinder\n")
 cat("E. dispersion - Display error bars or confidence bands\n")
 cat("F. draw.(arc|circle|line) - display a graphic element on a plot\n")
 cat("G. emptyspace/MaxEmptyRect - Find the largest empty space on a plot\n")
 cat("H. fullaxis - Display an axis that extends the full width/height\n")
 cat("I. getFigCtr - Get the coordinates of the center of the current figure\n")
 cat("J. getMarginWidth - Calculate the margin needed for text or a legend\n")
 cat("K. getYmult - Calculate the ratio of y values to x values\n")
 cat("L. gradient.rect - Display a rectangle with shaded colors\n")
 cat("M. hexagon - Draw a hexagon on the current plot\n")
 cat("N. jiggle - Move points apart, a bit like jitter\n")
 cat("O. legendg - Display a grouped legend\n")
 cat("P. length.key - Key for interpreting lengths in a plot\n")
 cat("X. Exit\n")
 whichplot<-toupper(readline("Choose an enhancement - "))
 if(whichplot == "1") {
  x <- rnorm(100)
  y <- x + rnorm(100)
  lmfit <- lm(y~x)
  plot(x, y, xlim=c(-3.5, 3.5))
  ablineclip(lmfit, x1 = -2, x2 = 2, lty = 2)
  ablineclip(h = 0, x1 = -2,x2 = 2,lty = 3, col = "red")
  ablineclip(v = 0, y1 = -2.5, y2 = 1.5, lty=4, col = "green")
 }
 if(whichplot == "2") {
  testdf <- data.frame(Before = c(10, 7, 5, 9), During = c(8, 6, 2, 5),
   After = c(5, 3, 4, 3))
  rownames(testdf) <- c("Red", "Green", "Blue", "Lightblue")
  barp(testdf, main = "Test addtable2plot", ylab = "Value",
   names.arg = colnames(testdf), col = 2:5)
 }
 if(whichplot == "3") {
  plot(0, xlim = c(1, 5),ylim = c(1, 5),main = "Test of arctext", xlab = "",
   ylab = "", type = "n")
  arctext("bendy like spaghetti", center = c(3,3), col = "blue")
  arctext("bendy like spaghetti", center = c(3,3), radius = 1.5, start = pi,
   cex = 2)
  arctext("bendy like spaghetti", center = c(3, 3),radius = 0.5,
   start = pi/2, stretch = 1.2)
  arctext("bendy like spaghetti", center = c(3, 3), radius = 1.7,
   start = 4 * pi / 3, cex = 1.3, clockwise = FALSE)
 }
 if(whichplot == "4") {
  plot(3:10, main = "Axis break test")
  # put a break at the default axis and position
  axis.break()
  axis.break(2, 2.9, style = "zigzag")
 }
 if(whichplot == "5") {
  plot(1:10 * 0.001, 1:10 * 100,axes = FALSE, xlab = "", ylab = "",
   main = "Axis multipliers")
  box()
  axis.mult(1, mult = 0.001)
  axis.mult(2, mult = 100)
 }
 if(whichplot == "6") {
  heights<-c(14,20,9,31,17)
  barpos<-barplot(heights,main="A redundant bar plot")
  # show the usual value labels on the bars
  barlabels(barpos,heights)
 }
 if(whichplot == "7") {
  x<-rnorm(10)
  y<-rnorm(10)
  plot(x,y,type="p")
  nums<-c("one","two","three","four","five","six",
   "seven","eight","nine","ten")
  boxed.labels(x,y-0.1,nums)
 }
 if(whichplot == "8") {
  tststr <- list(n=2,mean=0,sd=1,foo=4,bar=6) 
  clean.args(tststr,rnorm)
  try(do.call("rnorm",tststr))
  do.call("rnorm",clean.args(tststr,rnorm))
  remove.args(tststr,rnorm)
 }
 if(whichplot == "9") {
  cat("Color ID -",color.id("#cc00cc"),"\n")
 }
 if(whichplot == "A") {
  # get some extra room
  par(mar=c(7,4,4,6))
  testcol<-color.gradient(c(0,1),0,c(1,0),nslices=5)
  col.labels<-c("Cold","Warm","Hot")
  # this will put the labels at the intersections
  # col.labels<-c("","Cold","","Warm","","Warmer","","Hot","")
  color2D.matplot(matrix(rnorm(100),nrow=10),c(1,0),0,c(0,1),
   main="Test color legends")
  color.legend(11,6,11.8,9,col.labels,testcol,gradient="y")
  color.legend(10.2,2,11,5,col.labels,testcol,align="rb",gradient="y")
  color.legend(0.5,-2,3.5,-1.2,col.labels,testcol)
  color.legend(7,-1.8,10,-1,col.labels,testcol,align="rb",col=testcol[c(1,3,5)])
  par(mar=c(5,4,4,2))
 }
 if(whichplot == "B") {
  x<-rnorm(20)
  y<-rnorm(20)
  plot(x,y,col=color.scale(y,c(0,1,1),c(1,1,0),0),main="Color scale plot",
   pch=16,cex=2)
  plot(1:10,rep(1:3,length.out=10),axes=FALSE,type="n",xlim=c(0,11),ylim=c(0,4),
   main="Test of RGB, HSV and HCL",xlab="",ylab="Color specification")
  axis(2,at=1:3,labels=c("HCL","HSV","RGB"))
  points(1:10,rep(1,10),pch=19,cex=8,col=color.scale(1:10,c(0,300),35,85,
   color.spec="hcl"))
  points(1:10,rep(2,10),pch=19,cex=8,col=color.scale(1:10,c(0,1),
   0.8,1,color.spec="hsv"))
  points(1:10,rep(3,10),pch=19,cex=8,col=color.scale(1:10,c(1,0.5,0),
   c(0,0.5,0),c(0,0,1),color.spec="rgb"))
 }
 if(whichplot == "C") {
  plot(1:10,1:10)
  corner.label("A")
  corner.label(x=1,y=1)
  corner.label("B",y=-1,x=1,figcorner=TRUE,col="red")
 }
 if(whichplot == "D") {
  plot(0,xlim=c(0,5),ylim=c(0,5),main="Examples of pseudocylindrical rectangles",
   xlab="",ylab="",axes=FALSE,type="n")
  cylindrect(0,0,1,5,"red")
  cylindrect(rep(1,3),c(0,2,4),rep(4,3),c(1,3,5),"green",gradient="y")
  cylindrect(4,0,5,5,"#8844aa")
 }
 if(whichplot == "E") {
  disptest<-matrix(rnorm(200),nrow=20)
  disptest.means<-rowMeans(disptest)
  row.order<-order(disptest.means)
  se.disptest<-unlist(apply(disptest,1,std.error))
  plot(disptest.means[row.order],main="Dispersion as error bars",
   ylim=c(min(disptest.means-se.disptest),max(disptest.means+se.disptest)),
   xlab="Occasion",ylab="Value")
  dispersion(1:20,disptest.means[row.order],se.disptest[row.order])
 }
 if(whichplot == "F") {
  plot(0,xlim=c(0,10),ylim=c(0,10),type="n")
  draw.arc(5,5,4,deg1=0,deg2=180,col="blue",lwd=3)
  draw.circle(5,5,3,lwd=3,col="red")
  draw.ellipse(5,5,2.5,1.5,col="green",lwd=3)
  draw.radial.line(3,4,center=c(5,5),deg=270,lwd=3,col="brown")
 }
 if(whichplot == "G") {
  x<-runif(100)
  y<-runif(100)
  plot(x,y,main="Find the maximum empty rectangle",xlab="X",ylab="Y")
  mer<-maxEmptyRect(c(0,1),c(0,1),x,y)
  rect(mer$rect[1],mer$rect[2],mer$rect[3],mer$rect[4],border="red")
  es<-emptyspace(x,y)
  boxed.labels(es,labels="Here is the\nempty space",bg="transparent")
 }
 if(whichplot == "H") {
  plot(runif(20,-1,1),runif(20,-1,1),xlim=c(-1,1.5),main="Demo of fullaxis",
   xlab="X",ylab="Y",axes=FALSE)
  fullaxis(1,col="red",col.axis="red")
  fullaxis(2,col="blue",col.axis="blue")
  fullaxis(4,at=c(-0.5,0,0.5),labels=c("Negative","Zero","Positive"),pos=1.2,
   col="green",las=1)
  xylim<-par("usr")
  segments(xylim[1],xylim[4],xylim[2],xylim[4])
 }
 if(whichplot == "I") {
  plot(1:10)
  getFigCtr()
 }
 if(whichplot == "J") {
  plot(rnorm(10))
  newmarinfo<-getMarginWidth(labels=c("Long label","Even longer label"))
  oldmar<-par("mar")
  par(mar=c(oldmar[1:3],newmarinfo$newmar))
  plot(rnorm(10))
  par(xpd=TRUE)
  text(rep(newmarinfo$marcenter,2),c(0.5,-0.5),
   c("Long label","Even longer label"))
  par(mar=oldmar,xpd=FALSE)
 }
 if(whichplot == "K") {
  plot(1:3,c(10,20,30))
  getYmult()
 }
 if(whichplot == "L") {
  plot(0:10,type="n",axes=FALSE)
  gradient.rect(1,0,3,6,reds=c(1,0),
   greens=c(seq(0,1,length=10),seq(1,0,length=10)),
   blues=c(0,1),gradient="y")
  gradient.rect(4,0,6,6,c(seq(0,1,length=10),rep(1,10)),
   c(rep(1,10),seq(1,0,length=10)),c(0,0),gradient="y")
  gradient.rect(7,0,9,6,col=smoothColors("red",38,"blue"),border=NA)
 }
 if(whichplot == "M") {
  plot(1:3,type="n")
  hexagon(1.5,1.5,col="green")
 }
 if(whichplot == "N") {
  ahw.df<-data.frame(Age=rnorm(100,35,10),
   Height=rnorm(100,160,15),Weight=rnorm(100,75,20))
  par(mfrow=c(1,3))
  boxplot(ahw.df$Age,main="Age")
  points(jiggle(100,c(0.5,1.5)),ahw.df$Age,col="red")
  boxplot(ahw.df$Height,main="Height")
  points(jiggle(100,c(0.5,1.5)),ahw.df$Height,col="green")
  boxplot(ahw.df$Weight,main="Weight")
  points(jiggle(100,c(0.5,1.5)),ahw.df$Weight,col="blue")
  par(mfrow=c(1,1))
 }
 if(whichplot == "O") {
  plot(0.5,0.5,xlim=c(0,1),ylim=c(0,1),type="n",
   main="Test of grouped legend function")
  legendg(0.5,0.8,c("one","two","three"),pch=list(1,2:3,4:6),
   col=list(1,2:3,4:6),pt.space=1.5)
  legendg(0.5,0.5,c("one","two","three"),fill=list(1,2:3,4:6))
 }
 if(whichplot == "P") {
  o<-matrix(rep(pi*seq(0.1,0.8,by=0.1),7),ncol=8,byrow=TRUE)
  m<-matrix(rnorm(56)+4,ncol=8,byrow=TRUE)
  plot(0,xlim=c(0.7,8.3),ylim=c(0.7,7.3),type="n")
  vectorField(o,m,vecspec="rad")
  lengthKey(0.3,-0.5,c(0,5,10),0.24)
 }
 if(whichplot=="X") break
}
if(answer == "4") {
 cat("1. multsymbolbox - Draw boxes filled with symbols\n")
 cat("2. oz.windrose.legend - Draw a legend for oz.windrose\n")
 cat("3. p2p_arrows - Draw arrows between specified points\n")
 cat("4. pie.labels - Display labels on a pie chart\n")
 cat("5. placeLabels - manually place labels on a plot\n")
 cat("6. plot_bg - Add a background color to a plot\n")
 cat("7. polygon.shadow - Display a shadow effect\n")
 cat("8. print.brklist - Print the list generated by brkdnNest\n")
 cat("9. propbrk - Calculate the proportion of a specified value\n")
 cat("A. rectFill - Display rectangle(s) filled with symbols\n")
 cat("B. rescale - Rescale a vector of numbers into a new range\n")
 cat("C. Plot with one or both x and y axes reversed\n")
 cat("D. ruginv - Add an inverse rug axis to a plot\n")
 cat("E. smoothColors - Build a vector of interpolated colors\n")
 cat("F. spread.labels - Spread out labels for clustered values\n")
 cat("G. spreadout - Spread out a vector of numbers to a minimum spacing\n")
 cat("H. starPie - A polygonal graphic object almost unlike a pie chart\n")
 cat("I. staxlab - Stagger or rotate axis labels\n")
 cat("J. tab.title - Display a plot title in a colored tab\n")
 cat("K. thigmophobe.labels - Place labels away from the nearest point\n")
 cat("L. triax.abline - Display a line on a triangle plot\n")
 cat("M. triax.fill - Color the triangles on a triangle plot\n")
 cat("N. tsxpos - Calculated equispaced x positions of plotted values\n")
 cat("O. valid.n - Find the number of valid (not NA) values\n")
 cat("X. Exit\n")
 whichplot<-toupper(readline("Choose an enhancement - "))
 if(whichplot == "1") {
  plot(1:10,1:10,type="n")
  multsymbolbox(c(2,4),5,c(4,5),8,tot=c(10,8))
 }
 if(whichplot == "2") {
  plot(0,xlim=c(-20,20),ylim=c(-20,20),type="n",axes=FALSE,xlab="",ylab="")
  par(xpd=TRUE)
  oz.windrose.legend()
  par(xpd=FALSE)
 }
 if(whichplot == "3") {
  plot(1:2)
  points(2:1)
  p2p_arrows(c(1,2),c(1,1),c(2,1),c(2,2),code=3)
 }
 if(whichplot == "4") {
  pieval<-c(2,1,3,94)
  plot(1:5,type="n",axes=FALSE,xlab="",ylab="")
  box()
  bisect.angles<-floating.pie(3,3,pieval)
  pie.labels(3,3,bisect.angles,c("two","one","three","ninety\nfour"))
 }
 if(whichplot == "5") {
  x<-rnorm(3)
  y<-rnorm(3)
  cat("Click where the labels are to be placed\n")
  plot(x,y)
  placeLabels(x,y,LETTERS[1:3],flagcol="purple")
 }
 if(whichplot == "6") {
  barp(1:5,do.first="plot_bg()",col=1:5)
 }
 if(whichplot == "7") {
  par(pty="s")
  plot(1:5,type="n",main="Polygon Shadow test",xlab="",ylab="",axes=FALSE)
  box()
  polygon(c(1,2.2,2.2,1),c(5,5,3.8,3.8),col="#ffff00")
  polygon.shadow(c(1.2,2,2,1.2),c(4.8,4.8,4,4),col=c("#ffff00","#cccc00"))
  polygon(c(1.2,2,2,1.2),c(4.8,4.8,4,4),col=c("#ff0000"))
  polygon(c(4,5,5,4),c(2,2,1,1),col="#aaaaff")
  polygon.shadow(c(4.5,4.8,4.2),c(1.7,1.2,1.2),col=c("#aaaaff","#8888cc"),
   offset=c(0.1,-0.1),inflate=c(0.2,0.2))
  polygon(c(4.5,4.8,4.2),c(1.7,1.2,1.2),col=c("#00ff00"))
  polygon.shadow(cos(seq(0,2*pi,by=pi/20))+3,sin(seq(0,2*pi,by=pi/20))+3,
   offset=c(0,0),inflate=c(0.1,0.1))
  text(3,3,"Polygon shadow\nas a circular\ntext background",cex=1.5)
 }
 if(whichplot == "8") {
  printbrktest<-data.frame(A=c(sample(1:10,99,TRUE),NA),
   B=sample(c("Yes","No"),100,TRUE),
   C=sample(LETTERS[1:3],100,TRUE))
  pbt<-brkdnNest(A~B+C,printbrktest)
  print(pbt)
 }
 if(whichplot == "9") {
  cat("Proportion of M -",propbrk(sample(LETTERS,100,TRUE),trueval="M"))
 }
 if(whichplot == "A") {
  plot(1:7,type="n",xlab="",ylab="",main="Test of rectFill")
  rectFill(1:6,1:6,2:7,2:7,bg=2:7,pch=c("+","*","o",".","#","^"),
   xinc=c(0.2,0.1,0.2,0.1,0.2,0.2),yinc=c(0.2,0.1,0.2,0.1,0.2,0.2),
   pch.col=1:6)
 }
 if(whichplot == "B") {
  normal.counts<-rnorm(100)
  normal.tab<-tabulate(cut(normal.counts,breaks=seq(-3,3,by=1)))
  normal.density<-rescale(dnorm(seq(-3,3,length=100)),range(normal.tab))
  plot(c(-2.5,-1.5,-0.5,0.5,1.5,2.5),normal.tab,xlab="X values",
   type="h",col="green")
  lines(seq(-3,3,length=100),normal.density,col="blue")
 }
 if(whichplot == "C") {
 x <- runif(20)
 y <- runif(20)
 revaxis(x,y,yside=4)
}
 if(whichplot == "D") {
  require(stats)
  plot(density(faithful$eruptions,bw=0.15))
  ruginv(faithful$eruptions,ticksize=-0.05)
  ruginv(jitter(faithful$eruptions,amount=0.01),side=3,col="lightblue")
 }
 if(whichplot == "E") {
  plot(1:10,main="Test opaque colors",type="n",axes=FALSE)
  box()
  rect(1:7,1:7,3:9,3:9,col=smoothColors("red",2,"green",2,"blue"))
 }
 if(whichplot == "F") {
  x<-sort(rnorm(10))
  y<-rnorm(10)/10
  plot(x,y,ylim=c(-1,1),type="p")
  nums<-c("one","two","three","four","five","six","seven","eight","nine","ten")
  spread.labels(x,y,nums)
 }
 if(whichplot == "G") {
  cat("Spread out values -",spreadout(c(5,2.5,2.5,NA,3.5,1,3.5,NA),0.2),"\n")
 }
 if(whichplot == "H") {
  date_mat<-data.frame(sex=rep(c("M","F"),each=10),
   names=c("Abe","Bob","Col","Dave","Eddie","Frank","Geoff","Harry","Igor","Jack",
   "Alice","Betty","Clare","Dora","Eva","Fran","Grace","Hilda","Iris","Joan"),
   eating=sample(0:100,20),dancing=sample(0:100,20),movies=sample(0:100,20),
   reading=sample(0:100,20),travel=sample(0:100,20))
  plot(0,xlim=c(0.5,10.5),ylim=c(0,3),type="n",axes=FALSE,xlab="",ylab="Sex",
   main="Date matching matrix")
  par(xpd=TRUE)
  legend(0.7,-0.3,c("Eat out","Dance","Movies","Read","Travel"),fill=rainbow(5),
   ncol=5)
  par(xpd=FALSE)
  box()
  axis(2,at=c(0.9,2.4),labels=c("Male","Female"))
  starPie(x=rep(1:10,2),y=rep(c(0.9,2.4),each=10),radext=0.5,
   values=as.matrix(date_mat[,3:7]),label=as.character(date_mat[["names"]]))
 }
 if(whichplot == "I") {
  x<-rnorm(12)
  plot(x,axes=FALSE)
  box()
  months<-c("January","February","March","April","May","June",
   "July","August","September","October","November","December")
  staxlab(1,1:12,months)
 }
 if(whichplot == "J") {
  testx<-matrix(cumsum(rnorm(30)^2)+1,nrow=10)
  stackpoly(testx,main="",
   xaxlab=c("One","Two","Three","Four","Five",
   "Six","Seven","Eight","Nine","Ten"),staxx=TRUE)
  tab.title("Three Squiggly Lines",tab.col="yellow",radius=0.5)
 }
 if(whichplot == "K") {
  x<-rnorm(20)
  y<-rnorm(20)
  xlim<-range(x)
  xspace<-(xlim[2]-xlim[1])/20
  xlim<-c(xlim[1]-xspace,xlim[2]+xspace)
  ylim<-range(y)
  yspace<-(ylim[2]-ylim[1])/20
  ylim<-c(ylim[1]-yspace,ylim[2]+yspace)
  plotlabels<-
   c("one","two","three","four","five","six","seven","eight","nine","ten",
   "eleven","twelve","thirteen","fourteen","fifteen","sixteen","seventeen",
   "eighteen","nineteen","twenty")
  plot(x=x,y=y,xlim=xlim,ylim=ylim,main="Test thigmophobe.labels")
  thigmophobe.labels(x,y,plotlabels,col=c(2:6,8:12),font=2)
 }
 if(whichplot == "L") {
  oldpar<-par()
  triax.plot(data.frame(bottom=0.4,right=0.3,left=0.3),
   main="Triax ablines",no.add=FALSE)
  triax.abline(l=0.3,col="red")
  triax.abline(r=0.3,col="green")
  triax.abline(b=0.4,col="blue")
  par(oldpar)
 }
 if(whichplot == "M") {
  oldpar<-par()
  fillval<-list(0,c(0,0.1,0),c(0,0.1,0.2,0.1,0),
   c(0,0.1,0.2,0.3,0.2,0.1,0),c(0,0.1,0.2,0.3,0.4,0.3,0.2,0.1,0),
   c(0,0.1,0.2,0.3,0.4,0.5,0.4,0.3,0.2,0.1,0),
   c(0,0,0.1,0.2,0.3,0.4,0.5,0.4,0.3,0.2,0.1,0,0),
   c(0,0,0,0.1,0.1,0.2,0.3,0.4,0.3,0.2,0.1,0.1,0,0,0))
  fillcol<-sapply(fillval,function(x) {x*10+1} )
  triax.plot(main="Test of triax.fill function")
  triax.fill(fillcol)
  par(oldpar)
 }
 if(whichplot == "N") {
  y<-rnorm(28)
  par(mfrow=c(2,1))
  plot(y,main="Plot of the values")
  yt<-ts(y,start=2011,frequency=12)
  plot(yt,main="Plot of the time series",xaxt="n",xlab="Month")
  labelpos<-tsxpos(yt)
  staxlab(1,labelpos,rep(month.abb,length.out=28))
  par(mfrow=c(1,1))
 }
 if(whichplot == "O") {
  cat("Valid n =",valid.n(c(1,2,3,NA,5,6,7,NA,9,10)),"\n")
 }
 if(whichplot=="X") break
}
}
