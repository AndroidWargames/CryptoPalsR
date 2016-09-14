setwd("C:/Users/u342907/Desktop/Pals")
source("1.6.R")
source("3.19.R")
require(openssl)

startCTR <- function(sauce){
        pt20 <- lapply(readLines(sauce),base64_decode)
        ct20 <- lapply(pt20,FUN=function(x) EnCTR(x,k=mainKey))
        pt20 <- raw()
        ct20
}

breakCTR <- function(inp){
        if(!exists("mainKey")) mainKey <<- randkey()

        l <- min(unlist(lapply(inp,length)))
        print(l)
        ctCut <- unlist(lapply(inp, FUN=function(x) head(x,l)))

        key <- breaker(ctCut,l)

        rawToChar(xor(key,ctCut))
}