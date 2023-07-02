### R code from vignette source 'diagram.Rnw'

###################################################
### code chunk number 1: preliminaries
###################################################
library("diagram")
options(prompt = "> ")
options(width=90)


###################################################
### code chunk number 2: plotmat1
###################################################
par(mar = c(1, 1, 1, 1), mfrow = c(2, 2))
#
#
names <- c("A", "B", "C", "D")
M <- matrix(nrow = 4, ncol = 4, byrow = TRUE, data = 0)
plotmat(M, pos = c(1, 2, 1), name = names, lwd = 1,
        box.lwd = 2, cex.txt = 0.8, box.size = 0.1,
        box.type = "square", box.prop = 0.5)
#
M[2, 1] <- M[3, 1] <- M[4, 2] <- M[4, 3] <- "flow"
plotmat(M, pos = c(1, 2, 1), curve = 0, name = names, lwd = 1,
        box.lwd = 2, cex.txt = 0.8, box.type = "circle", box.prop = 1.0)
#
#
diag(M) <- "self"
plotmat(M, pos = c(2, 2), curve = 0, name = names, lwd = 1, box.lwd = 2,
        cex.txt = 0.8, self.cex = 0.5, self.shiftx = c(-0.1, 0.1, -0.1, 0.1),
        box.type = "diamond", box.prop = 0.5)

M <- matrix(nrow = 4, ncol = 4, data = 0)
M[2, 1] <- 1 ; M[4, 2] <- 2 ; M[3, 4] <- 3 ; M[1, 3] <- 4

Col <- M
Col[] <- "black"
Col[4, 2] <- "darkred"
pp <- plotmat(M, pos = c(1, 2, 1), curve = 0.2, name = names, lwd = 1,
              box.lwd = 2, cex.txt = 0.8, arr.type = "triangle",
              box.size = 0.1, box.type = "hexa", box.prop = 0.25,
              arr.col = Col, arr.len = 1)
mtext(outer = TRUE, side = 3, line = -1.5, cex = 1.5, "plotmat")
#
par(mfrow = c(1, 1))


###################################################
### code chunk number 3: diagram.Rnw:160-161
###################################################
pp


###################################################
### code chunk number 4: fig1
###################################################
par(mar = c(1, 1, 1, 1), mfrow = c(2, 2))
#
#
names <- c("A", "B", "C", "D")
M <- matrix(nrow = 4, ncol = 4, byrow = TRUE, data = 0)
plotmat(M, pos = c(1, 2, 1), name = names, lwd = 1,
        box.lwd = 2, cex.txt = 0.8, box.size = 0.1,
        box.type = "square", box.prop = 0.5)
#
M[2, 1] <- M[3, 1] <- M[4, 2] <- M[4, 3] <- "flow"
plotmat(M, pos = c(1, 2, 1), curve = 0, name = names, lwd = 1,
        box.lwd = 2, cex.txt = 0.8, box.type = "circle", box.prop = 1.0)
#
#
diag(M) <- "self"
plotmat(M, pos = c(2, 2), curve = 0, name = names, lwd = 1, box.lwd = 2,
        cex.txt = 0.8, self.cex = 0.5, self.shiftx = c(-0.1, 0.1, -0.1, 0.1),
        box.type = "diamond", box.prop = 0.5)

M <- matrix(nrow = 4, ncol = 4, data = 0)
M[2, 1] <- 1 ; M[4, 2] <- 2 ; M[3, 4] <- 3 ; M[1, 3] <- 4

Col <- M
Col[] <- "black"
Col[4, 2] <- "darkred"
pp <- plotmat(M, pos = c(1, 2, 1), curve = 0.2, name = names, lwd = 1,
              box.lwd = 2, cex.txt = 0.8, arr.type = "triangle",
              box.size = 0.1, box.type = "hexa", box.prop = 0.25,
              arr.col = Col, arr.len = 1)
mtext(outer = TRUE, side = 3, line = -1.5, cex = 1.5, "plotmat")
#
par(mfrow = c(1, 1))


###################################################
### code chunk number 5: plotmat2
###################################################
names <- c("PHYTO", "NH3", "ZOO", "DETRITUS", "BotDET", "FISH")
M <- matrix(nrow = 6, ncol = 6, byrow = TRUE, data = c(
#   p  n  z   d   b   f
    0, 1, 0,  0,  0,  0, #p
    0, 0, 4, 10, 11,  0, #n
    2, 0, 0,  0,  0,  0, #z
    8, 0, 13, 0,  0, 12, #d
    9, 0, 0,  7,  0,  0, #b
    0, 0, 5,  0,  0,  0  #f
    ))
#
pp <- plotmat(M, pos = c(1, 2, 1, 2), curve = 0, name = names,
              lwd = 1, box.lwd = 2, cex.txt = 0.8,
              box.type = "square", box.prop = 0.5, arr.type = "triangle",
              arr.pos = 0.4, shadow.size = 0.01, prefix = "f",
              main = "NPZZDD model")
#
phyto    <- pp$comp[names=="PHYTO"]
zoo      <- pp$comp[names=="ZOO"]
nh3      <- pp$comp[names=="NH3"]
detritus <- pp$comp[names=="DETRITUS"]
fish     <- pp$comp[names=="FISH"]
#
# flow5->detritus
#
m2 <- 0.5*(zoo+fish)
m1 <- detritus
m1[1] <- m1[1] + pp$radii[4,1]
mid <- straightarrow (to = m1, from = m2, arr.type = "triangle",
                      arr.pos = 0.4, lwd = 1)
text(mid[1], mid[2]+0.03, "f6", cex = 0.8)
#
# flow2->detritus
#
m2 <- 0.5*(zoo+phyto)
m1 <- detritus
m1[1] <-m1[1] + pp$radii[3,1]*0.2
m1[2] <-m1[2] + pp$radii[3,2]
mid <- straightarrow (to = m1, from = m2, arr.type = "triangle",
                      arr.pos = 0.3, lwd = 1)
text(mid[1]-0.01, mid[2]+0.03, "f3", cex = 0.8)


###################################################
### code chunk number 6: fig2
###################################################
names <- c("PHYTO", "NH3", "ZOO", "DETRITUS", "BotDET", "FISH")
M <- matrix(nrow = 6, ncol = 6, byrow = TRUE, data = c(
#   p  n  z   d   b   f
    0, 1, 0,  0,  0,  0, #p
    0, 0, 4, 10, 11,  0, #n
    2, 0, 0,  0,  0,  0, #z
    8, 0, 13, 0,  0, 12, #d
    9, 0, 0,  7,  0,  0, #b
    0, 0, 5,  0,  0,  0  #f
    ))
#
pp <- plotmat(M, pos = c(1, 2, 1, 2), curve = 0, name = names,
              lwd = 1, box.lwd = 2, cex.txt = 0.8,
              box.type = "square", box.prop = 0.5, arr.type = "triangle",
              arr.pos = 0.4, shadow.size = 0.01, prefix = "f",
              main = "NPZZDD model")
#
phyto    <- pp$comp[names=="PHYTO"]
zoo      <- pp$comp[names=="ZOO"]
nh3      <- pp$comp[names=="NH3"]
detritus <- pp$comp[names=="DETRITUS"]
fish     <- pp$comp[names=="FISH"]
#
# flow5->detritus
#
m2 <- 0.5*(zoo+fish)
m1 <- detritus
m1[1] <- m1[1] + pp$radii[4,1]
mid <- straightarrow (to = m1, from = m2, arr.type = "triangle",
                      arr.pos = 0.4, lwd = 1)
text(mid[1], mid[2]+0.03, "f6", cex = 0.8)
#
# flow2->detritus
#
m2 <- 0.5*(zoo+phyto)
m1 <- detritus
m1[1] <-m1[1] + pp$radii[3,1]*0.2
m1[2] <-m1[2] + pp$radii[3,2]
mid <- straightarrow (to = m1, from = m2, arr.type = "triangle",
                      arr.pos = 0.3, lwd = 1)
text(mid[1]-0.01, mid[2]+0.03, "f3", cex = 0.8)


###################################################
### code chunk number 7: plotmat3
###################################################
# Create population matrix
#
Numgenerations   <- 6
DiffMat  <- matrix(data = 0, nrow = Numgenerations, ncol = Numgenerations)
AA <- as.data.frame(DiffMat)
AA[[1,4]] <- "f[3]"
AA[[1,5]] <- "f[4]"
AA[[1,6]] <- "f[5]"
#
AA[[2,1]] <- "s[list(0,1)]"
AA[[3,2]] <- "s[list(1,2)]"
AA[[4,3]] <- "s[list(2,3)]"
AA[[5,4]] <- "s[list(3,4)]"
AA[[6,5]] <- "s[list(4,5)]"
#
name  <- c(expression(Age[0]), expression(Age[1]), expression(Age[2]),
           expression(Age[3]), expression(Age[4]), expression(Age[5]))
#
plotmat(A = AA, pos = 6, curve = 0.7, name = name, lwd = 2, 
        arr.len = 0.6, arr.width = 0.25, my = -0.2,
        box.size = 0.05, arr.type = "triangle", dtext = 0.95,
        main = "Age-structured population model 1")


###################################################
### code chunk number 8: fig3
###################################################
# Create population matrix
#
Numgenerations   <- 6
DiffMat  <- matrix(data = 0, nrow = Numgenerations, ncol = Numgenerations)
AA <- as.data.frame(DiffMat)
AA[[1,4]] <- "f[3]"
AA[[1,5]] <- "f[4]"
AA[[1,6]] <- "f[5]"
#
AA[[2,1]] <- "s[list(0,1)]"
AA[[3,2]] <- "s[list(1,2)]"
AA[[4,3]] <- "s[list(2,3)]"
AA[[5,4]] <- "s[list(3,4)]"
AA[[6,5]] <- "s[list(4,5)]"
#
name  <- c(expression(Age[0]), expression(Age[1]), expression(Age[2]),
           expression(Age[3]), expression(Age[4]), expression(Age[5]))
#
plotmat(A = AA, pos = 6, curve = 0.7, name = name, lwd = 2, 
        arr.len = 0.6, arr.width = 0.25, my = -0.2,
        box.size = 0.05, arr.type = "triangle", dtext = 0.95,
        main = "Age-structured population model 1")


###################################################
### code chunk number 9: teasel
###################################################
Teasel


###################################################
### code chunk number 10: plotmat4
###################################################
curves <- matrix(nrow = ncol(Teasel), ncol = ncol(Teasel), 0)
curves[3, 1] <- curves[1, 6] <- -0.35
curves[4, 6] <- curves[6, 4] <- curves[5, 6] <- curves[6, 5] <- 0.08
curves[3, 6] <-  0.35

plotmat(Teasel, pos = c(3, 2, 1), curve = curves, 
        name = colnames(Teasel), lwd = 1, box.lwd = 2,
        cex.txt = 0.8, box.cex = 0.8, box.size = 0.08,
        arr.length = 0.5, box.type = "circle", box.prop = 1,
        shadow.size = 0.01, self.cex = 0.6, my = -0.075, mx = -0.01,
        relsize = 0.9, self.shiftx = c(0, 0, 0.125, -0.12, 0.125, 0),
        self.shifty = 0, main = "Teasel population model")


###################################################
### code chunk number 11: fig4
###################################################
curves <- matrix(nrow = ncol(Teasel), ncol = ncol(Teasel), 0)
curves[3, 1] <- curves[1, 6] <- -0.35
curves[4, 6] <- curves[6, 4] <- curves[5, 6] <- curves[6, 5] <- 0.08
curves[3, 6] <-  0.35

plotmat(Teasel, pos = c(3, 2, 1), curve = curves, 
        name = colnames(Teasel), lwd = 1, box.lwd = 2,
        cex.txt = 0.8, box.cex = 0.8, box.size = 0.08,
        arr.length = 0.5, box.type = "circle", box.prop = 1,
        shadow.size = 0.01, self.cex = 0.6, my = -0.075, mx = -0.01,
        relsize = 0.9, self.shiftx = c(0, 0, 0.125, -0.12, 0.125, 0),
        self.shifty = 0, main = "Teasel population model")


###################################################
### code chunk number 12: plotweb1
###################################################
BB <- matrix(nrow = 20, ncol = 20, 1:20)
diag(BB) <- 0
Col <- BB
Col[] <- "black"
Col[BB<10]<- "red"
plotweb(BB, legend = TRUE, maxarrow = 3, arr.col = Col)
par(mfrow = c(1, 1))


###################################################
### code chunk number 13: fig5
###################################################
BB <- matrix(nrow = 20, ncol = 20, 1:20)
diag(BB) <- 0
Col <- BB
Col[] <- "black"
Col[BB<10]<- "red"
plotweb(BB, legend = TRUE, maxarrow = 3, arr.col = Col)
par(mfrow = c(1, 1))


###################################################
### code chunk number 14: diagram.Rnw:337-338
###################################################
Rigaweb


###################################################
### code chunk number 15: foodweb1
###################################################
plotweb(Rigaweb, main = "Gulf of Riga food web",
        sub = "mgC/m3/d", val = TRUE)


###################################################
### code chunk number 16: fig6
###################################################
plotweb(Rigaweb, main = "Gulf of Riga food web",
        sub = "mgC/m3/d", val = TRUE)


###################################################
### code chunk number 17: chart1
###################################################

par(mar = c(1, 1, 1, 1))
openplotmat()
elpos  <- coordinates (c(1, 1, 2, 4))
fromto <- matrix(ncol = 2, byrow = TRUE,
                 data = c(1, 2, 2, 3, 2, 4, 4, 7, 4, 8))
nr     <- nrow(fromto)
arrpos <- matrix(ncol = 2, nrow = nr)
for (i in 1:nr)
 arrpos[i, ] <- straightarrow (to = elpos[fromto[i, 2], ],
                              from = elpos[fromto[i, 1], ],
                              lwd = 2, arr.pos = 0.6, arr.length = 0.5)
textellipse(elpos[1,], 0.1,       lab = "start",           box.col = "green",
            shadow.col = "darkgreen", shadow.size = 0.005, cex = 1.5)
textrect   (elpos[2,], 0.15, 0.05,lab = "found term?",     box.col = "blue",
            shadow.col = "darkblue", shadow.size = 0.005, cex = 1.5)
textrect   (elpos[4,], 0.15, 0.05,lab = "related?",        box.col = "blue",
            shadow.col = "darkblue", shadow.size = 0.005, cex = 1.5)
textellipse(elpos[3,], 0.1, 0.1,  lab = c("other","term"), box.col = "orange",
            shadow.col = "red", shadow.size = 0.005, cex = 1.5)
textellipse(elpos[3,], 0.1, 0.1,  lab = c("other","term"), box.col = "orange",
            shadow.col = "red", shadow.size = 0.005, cex = 1.5)
textellipse(elpos[7,], 0.1, 0.1,  lab = c("make","a link"),box.col = "orange",
            shadow.col = "red", shadow.size = 0.005, cex = 1.5)
textellipse(elpos[8,], 0.1, 0.1,  lab = c("new","article"),box.col = "orange",
            shadow.col = "red", shadow.size = 0.005, cex = 1.5)
#
dd <- c(0.0, 0.025)
text(arrpos[2, 1] + 0.05, arrpos[2, 2], "yes")
text(arrpos[3, 1] - 0.05, arrpos[3, 2], "no")
text(arrpos[4, 1] + 0.05, arrpos[4, 2] + 0.05, "yes")
text(arrpos[5, 1] - 0.05, arrpos[5, 2] + 0.05, "no")
#


###################################################
### code chunk number 18: fig7
###################################################

par(mar = c(1, 1, 1, 1))
openplotmat()
elpos  <- coordinates (c(1, 1, 2, 4))
fromto <- matrix(ncol = 2, byrow = TRUE,
                 data = c(1, 2, 2, 3, 2, 4, 4, 7, 4, 8))
nr     <- nrow(fromto)
arrpos <- matrix(ncol = 2, nrow = nr)
for (i in 1:nr)
 arrpos[i, ] <- straightarrow (to = elpos[fromto[i, 2], ],
                              from = elpos[fromto[i, 1], ],
                              lwd = 2, arr.pos = 0.6, arr.length = 0.5)
textellipse(elpos[1,], 0.1,       lab = "start",           box.col = "green",
            shadow.col = "darkgreen", shadow.size = 0.005, cex = 1.5)
textrect   (elpos[2,], 0.15, 0.05,lab = "found term?",     box.col = "blue",
            shadow.col = "darkblue", shadow.size = 0.005, cex = 1.5)
textrect   (elpos[4,], 0.15, 0.05,lab = "related?",        box.col = "blue",
            shadow.col = "darkblue", shadow.size = 0.005, cex = 1.5)
textellipse(elpos[3,], 0.1, 0.1,  lab = c("other","term"), box.col = "orange",
            shadow.col = "red", shadow.size = 0.005, cex = 1.5)
textellipse(elpos[3,], 0.1, 0.1,  lab = c("other","term"), box.col = "orange",
            shadow.col = "red", shadow.size = 0.005, cex = 1.5)
textellipse(elpos[7,], 0.1, 0.1,  lab = c("make","a link"),box.col = "orange",
            shadow.col = "red", shadow.size = 0.005, cex = 1.5)
textellipse(elpos[8,], 0.1, 0.1,  lab = c("new","article"),box.col = "orange",
            shadow.col = "red", shadow.size = 0.005, cex = 1.5)
#
dd <- c(0.0, 0.025)
text(arrpos[2, 1] + 0.05, arrpos[2, 2], "yes")
text(arrpos[3, 1] - 0.05, arrpos[3, 2], "no")
text(arrpos[4, 1] + 0.05, arrpos[4, 2] + 0.05, "yes")
text(arrpos[5, 1] - 0.05, arrpos[5, 2] + 0.05, "no")
#


###################################################
### code chunk number 19: chart2
###################################################
openplotmat(main = "textbox shapes")
rx <- 0.1
ry <- 0.05
pos <- coordinates(c(1, 1, 1, 1, 1, 1, 1,1 ), mx = -0.2)
textdiamond(mid = pos[1,], radx = rx, rady = ry, lab = LETTERS[1],
            cex = 2, shadow.col = "lightblue")
textellipse(mid = pos[2,], radx = rx, rady = ry, lab = LETTERS[2],
            cex = 2, shadow.col = "blue")
texthexa(mid = pos[3,], radx = rx, rady = ry, lab = LETTERS[3],
         cex = 2, shadow.col = "darkblue")
textmulti(mid = pos[4,], nr = 7, radx = rx, rady = ry, lab = LETTERS[4],
         cex = 2, shadow.col = "red")
textrect(mid = pos[5,], radx = rx, rady = ry, lab = LETTERS[5],
         cex = 2, shadow.col = "darkred")
textround(mid = pos[6,], radx = rx, rady = ry, lab = LETTERS[6],
         cex = 2, shadow.col = "black")
textparallel(mid = pos[7,], radx = rx, rady = ry, lab = LETTERS[7],
         cex = 2, theta = 40, shadow.col = "black")
textempty(mid = pos[8,], lab = LETTERS[8], cex = 2, box.col = "yellow")
pos[ ,1] <- pos[ ,1] + 0.5
text(pos[ ,1],pos[ ,2], c("textdiamond", "textellipse", "texthexa",
                  "textmulti", "textrect", "textround",
                  "textparallel", "textempty"))


###################################################
### code chunk number 20: fig8
###################################################
openplotmat(main = "textbox shapes")
rx <- 0.1
ry <- 0.05
pos <- coordinates(c(1, 1, 1, 1, 1, 1, 1,1 ), mx = -0.2)
textdiamond(mid = pos[1,], radx = rx, rady = ry, lab = LETTERS[1],
            cex = 2, shadow.col = "lightblue")
textellipse(mid = pos[2,], radx = rx, rady = ry, lab = LETTERS[2],
            cex = 2, shadow.col = "blue")
texthexa(mid = pos[3,], radx = rx, rady = ry, lab = LETTERS[3],
         cex = 2, shadow.col = "darkblue")
textmulti(mid = pos[4,], nr = 7, radx = rx, rady = ry, lab = LETTERS[4],
         cex = 2, shadow.col = "red")
textrect(mid = pos[5,], radx = rx, rady = ry, lab = LETTERS[5],
         cex = 2, shadow.col = "darkred")
textround(mid = pos[6,], radx = rx, rady = ry, lab = LETTERS[6],
         cex = 2, shadow.col = "black")
textparallel(mid = pos[7,], radx = rx, rady = ry, lab = LETTERS[7],
         cex = 2, theta = 40, shadow.col = "black")
textempty(mid = pos[8,], lab = LETTERS[8], cex = 2, box.col = "yellow")
pos[ ,1] <- pos[ ,1] + 0.5
text(pos[ ,1],pos[ ,2], c("textdiamond", "textellipse", "texthexa",
                  "textmulti", "textrect", "textround",
                  "textparallel", "textempty"))


###################################################
### code chunk number 21: chart3
###################################################
par(mar = c(1, 1, 1, 1))
openplotmat(main = "Arrowtypes")
elpos <- coordinates (c(1, 2, 1), mx = 0.1, my = -0.1)
curvedarrow(from = elpos[1, ], to = elpos[2, ], curve = -0.5, 
            lty = 2, lcol = 2)
straightarrow(from = elpos[1, ], to = elpos[2, ], lty = 3, lcol = 3)
segmentarrow (from = elpos[1, ], to = elpos[2, ], lty = 1, lcol = 1)
treearrow    (from = elpos[2:3, ], to = elpos[4, ], lty = 4, lcol = 4)
bentarrow (from = elpos[3, ], to = elpos[3, ]-c(0.1, 0.1), 
           arr.pos = 1, lty = 5, lcol = 5)
bentarrow(from = elpos[1, ], to = elpos[3, ], lty = 5, lcol = 5)
selfarrow(pos = elpos[3, ], path = "R",lty = 6, curve = 0.075, lcol = 6)
splitarrow(from = elpos[1, ], to = elpos[2:3, ], lty = 1, 
           lwd = 1, dd = 0.7, arr.side = 1:2, lcol = 7)

for ( i in 1:4) 
  textrect (elpos[i, ], 0.05, 0.05, lab = i, cex = 1.5)

legend("topright", lty = 1:7, legend = c("segmentarrow",
       "curvedarrow", "straightarrow", "treearrow", "bentarrow",
       "selfarrow", "splitarrow"), lwd = c(rep(2, 6), 1), col = 1:7)


###################################################
### code chunk number 22: fig9
###################################################
par(mar = c(1, 1, 1, 1))
openplotmat(main = "Arrowtypes")
elpos <- coordinates (c(1, 2, 1), mx = 0.1, my = -0.1)
curvedarrow(from = elpos[1, ], to = elpos[2, ], curve = -0.5, 
            lty = 2, lcol = 2)
straightarrow(from = elpos[1, ], to = elpos[2, ], lty = 3, lcol = 3)
segmentarrow (from = elpos[1, ], to = elpos[2, ], lty = 1, lcol = 1)
treearrow    (from = elpos[2:3, ], to = elpos[4, ], lty = 4, lcol = 4)
bentarrow (from = elpos[3, ], to = elpos[3, ]-c(0.1, 0.1), 
           arr.pos = 1, lty = 5, lcol = 5)
bentarrow(from = elpos[1, ], to = elpos[3, ], lty = 5, lcol = 5)
selfarrow(pos = elpos[3, ], path = "R",lty = 6, curve = 0.075, lcol = 6)
splitarrow(from = elpos[1, ], to = elpos[2:3, ], lty = 1, 
           lwd = 1, dd = 0.7, arr.side = 1:2, lcol = 7)

for ( i in 1:4) 
  textrect (elpos[i, ], 0.05, 0.05, lab = i, cex = 1.5)

legend("topright", lty = 1:7, legend = c("segmentarrow",
       "curvedarrow", "straightarrow", "treearrow", "bentarrow",
       "selfarrow", "splitarrow"), lwd = c(rep(2, 6), 1), col = 1:7)


###################################################
### code chunk number 23: enet
###################################################
layoutmat <- matrix(data = c(rep(1, 12), 2, 3, 4, 5),
                    nrow = 4, ncol = 4, byrow = TRUE)
nf <- layout(layoutmat, respect = FALSE)
par(lwd = 1.5)
par(mar = c(0, 0, 2, 0))
emptyplot(main = "transistor Amplifier", asp = FALSE,
          ylim = c(-0.1, 1), xlim = c(-0.1, 1.1))
x1 <- 0; x2 <- 0.2; x3 <- 0.4; x4 <- 0.6; x5 <- 0.8; x6 <- 1
y1 <- 0.05; y2 <- 0.4; y3 <- 0.5; y4 <- 0.6; y5 <- 0.95 
x23 <- (x2 + x3)/2
x56 <- (x5 + x6)/2
lines(c(x2, x6, x6, x2, x2, x1, x1, x23, x3, x3),
      c(y1, y1, y5, y5, y1, y1, y3, y3,  y4, y5))
lines(c(x23, x3, x3),            c(y3,  y2, y1))
lines(c(x3,  x4,  x4),           c(y2,  y2,  y1))
lines(c(x3,  x5, x5),            c(y4,  y4, y1))
en.Amplifier(c(x23, y3), r = 0.035)

en.Signal(c(x1, 0.2), lab = expression("U"["in"]))
en.Signal(c(x6, y2), lab = expression("U"["b"]))
straightarrow(c(x1 - 0.05, 0.23), c(x1 - 0.05, 0.17),
              arr.pos =1, arr.type = "triangle", lwd = 1)
straightarrow(c(x6 + 0.05, y2 + 0.03), c(x6 + 0.05, y2 - 0.03),
              arr.pos = 1, arr.type = "triangle", lwd = 1)

en.Node(c(x1, y3), lab = "u1")
en.Node(c(x2, y3), lab = "u2")
en.Node(c(x3, y2), lab = "u3", pos = 1.5)
en.Node(c(x3, y4), lab = "u4", pos = 2.5)
en.Node(c(x5, y4), lab = "u5")

en.Capacitator(c(0.5*(x1 + x2),y3), lab = "C1", vert = FALSE)
en.Capacitator(c(x4, y4), lab = "C3", vert = FALSE)
en.Capacitator(c(x4, 0.5*(y1+y2)), lab = "C2", vert = TRUE)

en.Resistor(c(x1, y2), lab = "R0")
en.Resistor(c(x2, 0.5*(y1+y2)), lab = "R1")
en.Resistor(c(x2, 0.5*(y4+y5)), lab = "R2")
en.Resistor(c(x3, 0.5*(y4+y5)), lab = "R4")
en.Resistor(c(x3, 0.5*(y1+y2)), lab = "R3")
en.Resistor(c(x5, 0.5*(y1+y2)), lab = "R5")

en.Ground(c(1.0, 0.05))

par(mar=c(2, 2, 2, 2))

emptyplot(main = "transistor")
lines(c(0.1, 0.5,0.9), c(0.5, 0.5, 0.9))
lines(c(0.5, 0.9), c(0.5, 0.1))
lines(c(0.5, 0.5), c(0.4, 0.6))
text(0.2, 0.4, "Gate", font = 3)
text(0.8, 0.9, "Drain", font = 3,adj = 1)
text(0.8, 0.1, "Source", font = 3,adj = 1)
en.Amplifier(c(0.5, 0.5), r = 0.15)
box(col = "grey")


emptyplot(main = "capacitator")
straightarrow(c(0.5, 0.9), c(0.5, 0.1),
              arr.pos = 0.3, arr.length = 0.25, arr.type = "triangle")
en.Capacitator(c(0.5, 0.5), width = 0.075, length = 0.5, vert = TRUE)
text(0.4, 0.65, "i", font = 3, cex = 2)
straightarrow(c(0.8, 0.3), c(0.8, 0.77), arr.pos = 1,
              arr.length = 0.25, arr.type = "triangle", lwd = 1)
text(0.925, 0.65, "v", font = 3, cex = 2)
text(0.15, 0.5, "C", font = 3, cex = 2)
box(col = "grey")

emptyplot(main = "resistor")
straightarrow(c(0.5, 0.9), c(0.5, 0.1), arr.pos = 0.2,
              arr.length = 0.25, arr.type = "triangle", lwd = 1)
text(0.4, 0.85, "i", font = 3, cex = 2)
en.Resistor(c(0.5, 0.5), width = 0.25, length = 0.35 )
straightarrow(c(0.8, 0.3), c(0.8, 0.77), arr.pos = 1,
              arr.length = 0.25, arr.type = "triangle", lwd = 1)
text(0.925, 0.65, "v", font = 3, cex = 2)
text(0.5, 0.5, "R", font = 3, cex = 2)
box(col = "grey")

emptyplot(main = "voltage source")
lines(c(0.5, 0.5), c(0.1, 0.9))
en.Signal(c(0.5, 0.5), r = 0.15)
straightarrow(c(0.8, 0.3), c(0.8, 0.77), arr.pos = 1,
              arr.length = 0.25, arr.type = "triangle", lwd = 1)
text(0.925, 0.65, "v", font = 3, cex = 2)
box(col = "grey")


###################################################
### code chunk number 24: enet
###################################################
layoutmat <- matrix(data = c(rep(1, 12), 2, 3, 4, 5),
                    nrow = 4, ncol = 4, byrow = TRUE)
nf <- layout(layoutmat, respect = FALSE)
par(lwd = 1.5)
par(mar = c(0, 0, 2, 0))
emptyplot(main = "transistor Amplifier", asp = FALSE,
          ylim = c(-0.1, 1), xlim = c(-0.1, 1.1))
x1 <- 0; x2 <- 0.2; x3 <- 0.4; x4 <- 0.6; x5 <- 0.8; x6 <- 1
y1 <- 0.05; y2 <- 0.4; y3 <- 0.5; y4 <- 0.6; y5 <- 0.95 
x23 <- (x2 + x3)/2
x56 <- (x5 + x6)/2
lines(c(x2, x6, x6, x2, x2, x1, x1, x23, x3, x3),
      c(y1, y1, y5, y5, y1, y1, y3, y3,  y4, y5))
lines(c(x23, x3, x3),            c(y3,  y2, y1))
lines(c(x3,  x4,  x4),           c(y2,  y2,  y1))
lines(c(x3,  x5, x5),            c(y4,  y4, y1))
en.Amplifier(c(x23, y3), r = 0.035)

en.Signal(c(x1, 0.2), lab = expression("U"["in"]))
en.Signal(c(x6, y2), lab = expression("U"["b"]))
straightarrow(c(x1 - 0.05, 0.23), c(x1 - 0.05, 0.17),
              arr.pos =1, arr.type = "triangle", lwd = 1)
straightarrow(c(x6 + 0.05, y2 + 0.03), c(x6 + 0.05, y2 - 0.03),
              arr.pos = 1, arr.type = "triangle", lwd = 1)

en.Node(c(x1, y3), lab = "u1")
en.Node(c(x2, y3), lab = "u2")
en.Node(c(x3, y2), lab = "u3", pos = 1.5)
en.Node(c(x3, y4), lab = "u4", pos = 2.5)
en.Node(c(x5, y4), lab = "u5")

en.Capacitator(c(0.5*(x1 + x2),y3), lab = "C1", vert = FALSE)
en.Capacitator(c(x4, y4), lab = "C3", vert = FALSE)
en.Capacitator(c(x4, 0.5*(y1+y2)), lab = "C2", vert = TRUE)

en.Resistor(c(x1, y2), lab = "R0")
en.Resistor(c(x2, 0.5*(y1+y2)), lab = "R1")
en.Resistor(c(x2, 0.5*(y4+y5)), lab = "R2")
en.Resistor(c(x3, 0.5*(y4+y5)), lab = "R4")
en.Resistor(c(x3, 0.5*(y1+y2)), lab = "R3")
en.Resistor(c(x5, 0.5*(y1+y2)), lab = "R5")

en.Ground(c(1.0, 0.05))

par(mar=c(2, 2, 2, 2))

emptyplot(main = "transistor")
lines(c(0.1, 0.5,0.9), c(0.5, 0.5, 0.9))
lines(c(0.5, 0.9), c(0.5, 0.1))
lines(c(0.5, 0.5), c(0.4, 0.6))
text(0.2, 0.4, "Gate", font = 3)
text(0.8, 0.9, "Drain", font = 3,adj = 1)
text(0.8, 0.1, "Source", font = 3,adj = 1)
en.Amplifier(c(0.5, 0.5), r = 0.15)
box(col = "grey")


emptyplot(main = "capacitator")
straightarrow(c(0.5, 0.9), c(0.5, 0.1),
              arr.pos = 0.3, arr.length = 0.25, arr.type = "triangle")
en.Capacitator(c(0.5, 0.5), width = 0.075, length = 0.5, vert = TRUE)
text(0.4, 0.65, "i", font = 3, cex = 2)
straightarrow(c(0.8, 0.3), c(0.8, 0.77), arr.pos = 1,
              arr.length = 0.25, arr.type = "triangle", lwd = 1)
text(0.925, 0.65, "v", font = 3, cex = 2)
text(0.15, 0.5, "C", font = 3, cex = 2)
box(col = "grey")

emptyplot(main = "resistor")
straightarrow(c(0.5, 0.9), c(0.5, 0.1), arr.pos = 0.2,
              arr.length = 0.25, arr.type = "triangle", lwd = 1)
text(0.4, 0.85, "i", font = 3, cex = 2)
en.Resistor(c(0.5, 0.5), width = 0.25, length = 0.35 )
straightarrow(c(0.8, 0.3), c(0.8, 0.77), arr.pos = 1,
              arr.length = 0.25, arr.type = "triangle", lwd = 1)
text(0.925, 0.65, "v", font = 3, cex = 2)
text(0.5, 0.5, "R", font = 3, cex = 2)
box(col = "grey")

emptyplot(main = "voltage source")
lines(c(0.5, 0.5), c(0.1, 0.9))
en.Signal(c(0.5, 0.5), r = 0.15)
straightarrow(c(0.8, 0.3), c(0.8, 0.77), arr.pos = 1,
              arr.length = 0.25, arr.type = "triangle", lwd = 1)
text(0.925, 0.65, "v", font = 3, cex = 2)
box(col = "grey")


