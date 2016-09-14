setwd("C:/Users/u342907/Desktop/Pals")
source('EnglishTest2.R')
source("1.2.R")

# bacon <- "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"

singleByte <- function(inp){
        len <- length(h2raw(inp))
        a <- c()
        for(i in 0:255){
                xor <- raw2h(rep(as.raw(i),len))
                bytes <- h2raw(hexor(xor,inp))
                bytes <- bytes[!bytes==as.raw(0)]
                a[i+1] <- grammer2(rawToChar(bytes))
        }
        i <- match(max(a,na.rm=TRUE),a) -1
        xor <- raw2h(rep(as.raw(i),len))
        bytes <- h2raw(hexor(xor,inp))
        bytes <- bytes[!bytes==as.raw(0)]
        c(rawToChar(bytes),as.character(i))
}
