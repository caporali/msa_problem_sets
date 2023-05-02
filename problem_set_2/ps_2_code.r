
#                  ## Problem Set 2 ##

##############################################
##############################################

## Exercise 1

rm(list=ls())

## psych data

psych<-read.table("data/psych.txt",header=T)
dim(psych)
head(psych)
with(psych,table(group))

# V1 visual perception, 
# V2 cubes, 
# V3 paper form board, 
# V4 flags, 
# V5 general information, 
# V6 paragraph comprehension, 
# V7 sentence completion, 
# V8 word classification, 
# V9 word meaning, 
# V10 addition, 
# V11 code, 
# V12 counting dots, 
# V13 straight-curved capitals, 
# V14 word recognition, 
# V15 number recognition, 
# V16 figure recognition, 
# V17 object-number, 
# V18 number-figure, 
# V19 figure-word, 
# V20 deduction, 
# V21 numerical puzzles, 
# V22 problem reasoning, 
# V23 series completion, 
# V24 arithmetic problems.


##############################################
##############################################

## Exercise 2

rm(list=ls())

## Pen digits data

pendigits<-read.table("data/pendigits.txt", sep=",",head=F)
names(pendigits)<-c(paste0(rep(c("x","y"),8),rep(1:8,each=2)),"digit")
dim(pendigits)
head(pendigits)

lookup<-c("darkgreen",  "brown", "lightblue",  "magenta", "purple", 
                     "blue", "red", "lightgreen", "orange", "cyan")
names(lookup)<-as.character(0:9)
digit.col<-lookup[as.character(pendigits$digit)]
#                                            # color coding
groupCV<-rep(1:44, each=250)
groupCV<-groupCV[1:length(pendigits$digit)]

