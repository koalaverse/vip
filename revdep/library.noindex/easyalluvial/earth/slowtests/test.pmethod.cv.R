# test.pmethod.cv.R: example pmethod.cv model built by earth
# Stephen Milborrow May 2015 Berea

source("test.prolog.R")
library(earth)
data(etitanic)
options(digits=4)

printf <- function(format, ...) cat(sprint(format, ...)) # like c printf

cat("\npmethod=cv with formula interface\n\n")

# following is so we can directly compare pmethod=back to pmethod=cv
set.seed(2)
a100.form <- earth(survived ~ ., data=etitanic, degree=2, trace=0, pmethod="back", nfold=2, ncross=2, keepxy=TRUE)
cat("print(a100.form)\n")
print(a100.form)
plot(a100.form, which=1,
     main="a100.form: pmethod=\"back\"")

par(mfrow = c(2, 2), mar = c(3, 3, 3, 1), mgp = c(1.5, 0.5, 0), oma=c(0,0,2,0))

set.seed(2)
cat("\n")
a101.form <- earth(survived ~ ., data=etitanic, degree=2, trace=1, pmethod="cv", nfold=2, ncross=2)
cat("\nprint(a101.form)\n")
print(a101.form)
cat("\nprint(summary(a101.form))\n")
print(summary(a101.form))
plot(a101.form, which=1, legend.cex=.5, main="a101.form: pmethod=\"cv\"", cex.main=.8, caption="formula interface")

# test trace=.5
set.seed(2)
a101a.form <- earth(survived ~ ., data=etitanic, degree=2, trace=.5, pmethod="cv", nfold=2, ncross=3)

# multiple response model
set.seed(2015)
a102.form <- earth(pclass ~ ., data=etitanic, degree=2, pmethod="cv", nfold=3)
cat("\nprint(a102.form)\n")
print(a102.form)
cat("\nprint(summary(a102.form))\n")
print(summary(a102.form))
plot(a102.form, which=1, nresponse=1, main="a102.form: pmethod=\"cv\" multiple response", cex.main=.8)

# test trace=.5 with multiple response model
set.seed(2015)
a102.form <- earth(pclass ~ ., data=etitanic, degree=2, trace=.5, pmethod="cv", nfold=3)

# multiple response model
# following is useful because the model selected by cv is same as that selected by gcv
set.seed(1900) # don't change
a103.form <- earth(pclass ~ ., data=etitanic, degree=2,
           pmethod="cv", nfold=3, nprune=9)
cat("\nprint(a103.form)\n")
print(a103.form)
plot(a103.form, which=1, nresponse=1,
     main="a103.form: pmethod=\"cv\" multiple response\nmax(GRSq) == which.max(mean.oof.rsq)", cex.main=.8)

# test cv with nprune less than what would be normally selected
set.seed(1) # don't change
a104.form <- earth(pclass ~ ., data=etitanic, degree=2, pmethod="cv", nfold=3, nprune=7)
cat("\nprint(a104.form)\n")
print(a104.form)
plot(a104.form, which=1, nresponse=1, grid=T, main="a104.form: pmethod=\"cv\" nprune=7", cex.main=.8)

cat("\n\npmethod=cv with x,y interface\n\n")
par(mfrow = c(2, 2), mar = c(3, 3, 3, 1), mgp = c(1.5, 0.5, 0), oma=c(0,0,2,0))

etitanic.except.survived <- etitanic[,c(1,3,4,5,6)]
survived <- etitanic$survived

# # following is so we can directly compare pmethod=back to pmethod=cv
# # commented out because already done above with model a100.formula
# set.seed(2)
# a100.xy <- earth(etitanic.except.survived, survived, degree=2, trace=0, pmethod="back", nfold=2, ncross=2, keepxy=TRUE)
# cat("\nprint(a100.xy)\n")
# print(a100.xy)
# plot(a100.xy, which=1, legend.cex=.5, main="a100.xy: pmethod=\"back\"", cex.main=.8)


set.seed(2)
a101.xy <- earth(etitanic.except.survived, survived, degree=2, trace=1, pmethod="cv", nfold=2, ncross=2)
cat("\nprint(a101.xy)\n")
print(a101.xy)
cat("\nprint(summary(a101.xy)\n")
print(summary(a101.xy))
plot(a101.xy, which=1, legend.cex=.5, main="a101.xy: pmethod=\"cv\"", cex.main=.8, caption="xy interface")

# a101.form
# a102.xy

# multiple response model
x.except.pclass <- etitanic[,c(2,3,4,5,6)]
pclass <- etitanic$pclass
set.seed(2015)
a102.xy <- earth(x.except.pclass, pclass, degree=2,
           pmethod="cv", nfold=3)
cat("\nprint(a102.xy)\n")
print(a102.xy)
plot(a102.xy, which=1, nresponse=1, main="a102.xy: pmethod=\"cv\" multiple response", cex.main=.8)

# multiple response model
# following is useful because the model selected by cv is same as that selected by gcv
set.seed(1900) # don't change
a103.xy <- earth(x.except.pclass, pclass, degree=2,
           pmethod="cv", nfold=3, nprune=9)
cat("\nprint(a103.xy)\n")
print(a103.xy)
cat("\nprint(summary(a103.xy)\n")
print(summary(a103.xy))
plot(a103.xy, which=1, nresponse=1,
     main="a103.xy: pmethod=\"cv\" multiple response\nmax(GRSq) == which.max(mean.oof.rsq)", cex.main=.8)

# test cv with nprune less than what would be normally selected
set.seed(1) # don't change
a104.xy <- earth(x.except.pclass, pclass, degree=2, pmethod="cv", nfold=3, nprune=7)
cat("\nprint(a104.xy)\n")
print(a104.xy)
plot(a104.xy, which=1, nresponse=1, grid=T, main="a104.xy: pmethod=\"cv\" nprune=7", cex.main=.8)

# This model used to cause following error: (fixed July 2020, see nprune1 code in earth.R)
#    evimp: Error in object$prune.terms[isubset, -1] : subscript out of bounds
set.seed(1900)
a3 <- earth(survived~., data=etitanic, degree=2, nprune=4, nfold=2, pmethod="cv", trace=.5)
print(evimp(a3, trim=FALSE))
plot(a3, which=1, main="a3: pmethod=\"cv\" nprune=4", cex.main=.8, ylim=c(0,.5))

source("test.epilog.R")
