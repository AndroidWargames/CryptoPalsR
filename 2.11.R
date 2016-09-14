require(digest)
require(openssl)
setwd("C:/Users/u342907/Desktop/Pals")
source("2.10.R")
source("1.8.R")

randkey <- function() as.raw(sample(0:127,16,TRUE))


encryption_oracle <- function(inp){
        xb <- sample(5:10,2)
        inp <- c(as.raw(sample(0:127,xb[1],TRUE)),
                 inp,
                 as.raw(sample(0:127,xb[2],TRUE)))
        k <- randkey()
        iv <- randkey()
        r <- sample(c(T,F),1)
        if(r){
                out <- EnECB(inp,k)
        }else{
                out <- EnCBC(inp,iv,k)
        }
        out
}


