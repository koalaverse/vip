# test.ordinal.R: ordinal models by way of package "ordinal" and earth's bx matrix

source("test.prolog.R")
source("check.models.equal.R")
options(warn=1) # print warnings as they occur
library(earth)

# toy data, where response is non-monotonic with input

input <- 1:20

resp <- ifelse(input < 8, "low", ifelse(input > 16, "med", "high"))
resp[8] <- resp[15] <- "med" # add some noise to make it more interesting
resp <- ordered(resp, levels=c("low", "med", "high"))
cat("\nsummary(resp)\n")
print(summary(resp))

dat <- data.frame(input=input, resp=resp)

library(ordinal)
clm.mod <- clm(resp ~ input, data=dat)
cat("\nsummary(clm.mod)\n")
print(summary(clm.mod))

earth.mod <- earth(resp ~ input, data=dat)
cat("\nsummary(earth.mod)\n")
print(summary(earth.mod))

bx <- earth.mod$bx
bx <- bx[,-1,drop=FALSE] # drop intercept column
bx <- as.data.frame(bx)
bx$resp <- dat$resp # add resp (needed for formula interface below)

clm.earth <- clm(resp ~ ., data=bx)
cat("\nsummary(clm.earth)\n")
print(summary(clm.earth))

earth.bx.mod <- earth(resp ~ input, data=bx)
cat("\nsummary(earth.bx.mod)\n")
print(summary(earth.bx.mod))

cat("\n=== models after converting ordered response to numeric ===\n")
# i.e. artificially impose equal distance between each level in the response

dat.numeric.resp <- data.frame(input=input, resp=as.numeric(resp))

earth.numeric.resp <- earth(resp ~ input, data=dat.numeric.resp)
cat("\nsummary(earth.numeric.resp)\n")
print(summary(earth.numeric.resp))

bx.numeric.resp <- earth.numeric.resp$bx
bx.numeric.resp <- bx.numeric.resp[,-1,drop=FALSE] # drop intercept column
bx.numeric.resp <- as.data.frame(bx.numeric.resp)
bx.numeric.resp$resp <- resp # add resp (needed for formula interface below)
                             # note that for clm() we use the ORIGINAL resp (ordered factor, not numeric)

clm.earth.numeric.resp <- clm(resp ~ ., data=bx.numeric.resp)
cat("\nsummary(clm.earth.numeric.resp)\n")
print(summary(clm.earth.numeric.resp))

bx.numeric.resp$resp <- as.numeric(resp)
                            # add resp (needed for formula interface below)
                            # note that for earth() we use as.mumeric(resp)
                            # (else we generate a multiple resp model, which we don't want here)

earth.bx.numeric.resp.mod <- earth(resp ~ input, data=bx.numeric.resp)
cat("\nsummary(earth.bx.numeric.resp.mod)\n")
print(summary(earth.bx.numeric.resp.mod))

cat("\n== use plots to compare predicted to measured response ==\n")

# color points using measured response values (the "true" response)
col <- ifelse(resp == "low", "red", ifelse(resp == "med", "pink", "green"))

par(mfrow = c(3,3), mar = c(4, 3, 3, 1), mgp = c(1.5, 0.5, 0))

cat("\nplot measured response\n")

plot(input, resp, main="measured response",
     yaxp=c(1,3,2), pch=20, col=col, ylab="measured response")

legend("topleft", legend=c("low", "med", "high"),
       col=c("red", "pink", "green"), pch=20, cex=.8)

cat("\nplot response predicted by clm model\n")

predict.clm <- predict(clm.mod, type="class")$fit

plot(input, predict.clm, main="clm.mod",
     yaxp=c(1,3,2), pch=20, col=col, ylab="predicted response")

points(input, predict.clm, # black rings around wrong predictions
       col=ifelse(predict.clm == as.character(resp), 0, "black"))

plot.legend <- function()
{
    legend("topleft", legend=c("low", "med", "high", "wrong"),
           col=c("red", "pink", "green", "black"), pch=c(20,20,20,1), cex=.8)
}
plot.legend()

empty.plot()

cat("\nplot response predicted by earth.bx model\n")

predict.earth.bx.mod <- predict(earth.bx.mod, type="class")
predict.earth.bx.mod <- ifelse(predict.earth.bx.mod == "low", 1,
                        ifelse(predict.earth.bx.mod == "med", 2,
                                                              3))

plot(input, predict.earth.bx.mod, main="earth.bx.mod",
     yaxp=c(1,3,2), pch=20, col=col, ylab="predicted response")

points(input, predict.earth.bx.mod, # black rings around wrong predictions
       col=ifelse(predict.earth.bx.mod == as.numeric(resp), 0, "black"), cex=1)

plot.legend()

cat("\nplot response predicted by clm/earth model\n")

predict.clm.earth <- predict(clm.earth, type="class")$fit

plot(input, predict.clm.earth, main="clm.earth",
     yaxp=c(1,3,2), pch=20, col=col, ylab="predicted response")

points(input, predict.clm.earth, # black rings around wrong predictions
       col=ifelse(predict.clm.earth == as.character(resp), 0, "black"), cex=1)

plot.legend()

empty.plot()

cat("\nplot response predicted by earth.bx model with as.numeric(resp)\n")

predict.earth.bx.numeric.resp.mod <- predict(earth.bx.numeric.resp.mod)
predict.earth.bx.numeric.resp.mod <- ifelse(predict(earth.bx.numeric.resp.mod) < 1.5, 1,
                         ifelse(predict(earth.bx.numeric.resp.mod) < 2.5, 2,
                                                              3))

plot(input, predict.earth.bx.numeric.resp.mod, main="earth.bx.numeric.resp.mod",
     yaxp=c(1,3,2), pch=20, col=col, ylab="predicted response")

points(input, predict.earth.bx.numeric.resp.mod, # black rings around wrong predictions
       col=ifelse(predict.earth.bx.numeric.resp.mod == as.numeric(resp), 0, "black"), cex=1)

plot.legend()

cat("\nplot response predicted by clm/earth model with as.numeric(resp)\n")

predict.clm.earth.numeric.resp <- predict(clm.earth.numeric.resp, type="class")$fit

plot(input, predict.clm.earth.numeric.resp, main="clm.earth.numeric.resp",
     yaxp=c(1,3,2), pch=20, col=col, ylab="predicted response")

points(input, predict.clm.earth.numeric.resp, # black rings around wrong predictions
       col=ifelse(predict.clm.earth.numeric.resp == as.character(resp), 0, "black"), cex=1)

plot.legend()

empty.plot()

par(org.par)

cat("\n=== plotmo plots ===\n")

par(mfrow = c(3,3), mar = c(4, 3, 3, 1), mgp = c(1.5, 0.5, 0))

# in the plotmo plots below we use nresp=1 to select the first response level ("low"),
# and  predict probabilites by setting type="prob" for predict.clm
plotmo(clm.mod, type="prob", do.par=0, nresp=1, main="clm.mod: is.low")
plotmo(clm.mod, type="prob", do.par=0, nresp=2, main="clm.mod: is.med")
plotmo(clm.mod, type="prob", do.par=0, nresp=3, main="clm.mod: is.high")

plotmo(earth.mod, do.par=0, nresp=1, main="earth.mod: is.low")
plotmo(earth.mod, do.par=0, nresp=2, main="earth.mod: is.med")
plotmo(earth.mod, do.par=0, nresp=3, main="earth.mod: is.high")

# plotmo(clm.earth, do.par=0, nresp=1, all2=TRUE) # main="clm.earth: is.low")
# plotmo(clm.earth, do.par=0, nresp=2, all2=TRUE) # main="clm.earth: is.med")
plotmo(clm.earth, do.par=0, nresp=3, all2=TRUE) # main="clm.earth: is.high")

par(org.par)

cat("\n=== plotmo plots with as.numeric(response) ===\n")

par(mfrow = c(3,3), mar = c(4, 3, 3, 1), mgp = c(1.5, 0.5, 0))

plotmo(earth.numeric.resp, do.par=0, all2=TRUE, main="earth.numeric.resp")
empty.plot()
empty.plot()

plotmo(clm.earth.numeric.resp, do.par=0, nresp=3, all2=2)

par(org.par)

source("test.epilog.R")
