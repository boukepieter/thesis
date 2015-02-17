setwd("E:/thesis/data/reference dem/data")
require(rgdal)

las2txt -i "E:\thesis\data\reference dem\data\ot_581000_5179000.laz" -otxt -parse xyzrn -sep comma
test <- read.csv("ot_581000_5179000_1.txt")
test2 <- read.csv("ot_581000_5179000.txt", header=FALSE)
head(test2)
test2[1:30,]
test2tje <- test2[test2[4]==test2[5],]
nrow(test2)
nrow(test2tje)
test2tje[100:200,]

setwd("..")
system('D:/LAStools/bin/las2txt -i "E:/thesis/data/reference dem/data/ot_581000_5179000.laz" -otxt -parse xyzrn -sep comma')
test2 <- read.csv("ot_581000_5179000.txt", header=FALSE)
test2tje <- test2[test2[4]==test2[5],]