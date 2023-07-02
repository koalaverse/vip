## This file keeps record of reading the data into R and
## transformations (if any) that have been applied. All
## transformations are indicated in the respective help pages.

###**********************************************************

LetterRecognition <- scan(file="LetterRecognition.data")

LetterRecognition <- matrix(LetterRecognition,ncol=17,byrow=TRUE)
LetterRecognition <- as.data.frame(LetterRecognition)
colnames(LetterRecognition) <-
    c("lettr", "x.box", "y.box", "width", "high", "onpix", "x.bar",
      "y.bar", "x2bar", "y2bar", "xybar", "x2ybr", "xy2br", "x.ege",
      "xegvy", "y.ege", "yegvx")
LetterRecognition$lettr <- factor(LetterRecognition$lettr,
                                  labels=LETTERS)

save(LetterRecognition, file="data/LetterRecognition.rda")


###**********************************************************

### PimaIndiansDiabetes2

load("data/PimaIndiansDiabetes.rda")
PimaIndiansDiabetes2 = PimaIndiansDiabetes

for(n in c("glucose", "pressure","triceps", "insulin",  "mass")){
    PimaIndiansDiabetes2[[n]][PimaIndiansDiabetes[[n]]==0] <- NA
}

save(PimaIndiansDiabetes2, file="data/PimaIndiansDiabetes2.rda")

###**********************************************************

Satellite <- scan("Satellite.data")

Satellite <- matrix(Satellite,ncol=37,byrow=TRUE)
Satellite <- data.frame(x=Satellite[,1:36], classes=factor(Satellite[,37]))
levels(Satellite$classes) <- c("red soil",
                               "cotton crop",
                               "grey soil",
                               "damp grey soil",
                               "vegetation stubble",
                               "very damp grey soil")

save(Satellite, file="data/Satellite.rda")

###**********************************************************

### Zoo

## download zoo.data from UCI repository (2007-02-02)
## edit zoo.data from UCI repository: two rows have name "frog"
## -> frog.1 and frog.2

Zoo <- read.csv("zoo.data", header=FALSE, row.names=1)

colnames(Zoo) <- c("hair",
                   "feathers",	
                   "eggs",		
                   "milk",		
                   "airborne",	
                   "aquatic",	
                   "predator",	
                   "toothed",	
                   "backbone",	
                   "breathes",	
                   "venomous",	
                   "fins",		
                   "legs",		
                   "tail",		
                   "domestic",	
                   "catsize",	
                   "type")

Zoo[,1:12] <- lapply(Zoo[,1:12], as.logical)
Zoo[,14:16] <- lapply(Zoo[,14:16], as.logical)
Zoo[,17] <- factor(Zoo[,17],
                   labels=c("mammal","bird","reptile","fish",
                   "amphibian","insect","mollusc.et.al"))

save(Zoo, file="Zoo.rda")

###**********************************************************

## change compression type

for(f in list.files("data")){
    n <- sub(".rda", "", f)
    load(file.path("data", f))
    save(list=n, file=f, compress="xz")
}
