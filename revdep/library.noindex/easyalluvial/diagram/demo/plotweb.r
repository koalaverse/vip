
# PLOTWEB examples
par(ask=TRUE)
# plotweb examples
feed <- matrix(nrow=20,ncol=20,1)
plotweb(feed,legend=FALSE,length=0,main="plotweb")

feed <- matrix(nrow=20,ncol=20,1)
diag(feed)<-0
plotweb(feed,legend=FALSE,main="plotweb")

feed <- diag(nrow=20,ncol=20,1)
plotweb(feed,legend=FALSE,main="plotweb")

plotweb(Rigaweb,main="Gulf of Riga food web",sub="mgC/m3/d",val=TRUE)
plotweb(Takapotoweb,main="Takapoto atoll planktonic food web",leg.title="mgC/m2/day",lab.size=1)
plotweb(Takapotoweb,main="Takapoto atoll planktonic food web",sub="mgC/m2/day",lab.size=1,log=TRUE)

