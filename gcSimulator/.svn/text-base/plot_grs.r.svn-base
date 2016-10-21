args <- commandArgs(trailingOnly=TRUE)
args
table <- read.csv(file=args[1],head=FALSE,sep=",")
v <- c(table,recursive=TRUE)

png(file=paste("allhist_",args[1],".png", sep=""))
hist(v, plot=TRUE)
