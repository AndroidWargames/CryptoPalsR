setwd("C:/Users/u342907/Desktop/Pals")
require(textcat)

x <- readLines("1.4.Source.txt")

j = 0
singles <- function(x){
        for(i in x){
                j = j+1
                print(j)
                i <- substring(i,first=1,last=nchar(i))
                if(grammer(singleByte(i)[1])>.8){
                        print(singleByte(i))
                        break
                }
        }
}