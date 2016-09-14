require(openssl)
require(digest)

setwd("C:/Users/u342907/Desktop/Pals")
source("1.6.R")
hexes <- readLines('1.8.Source.txt')

detectECB <- function(hexLines){
        for(i in seq_along(hexLines)){
                r <- h2raw(hexLines[i])
                if(isECB(r)) break

        }
        cat("Row: ")
        cat(i)
        cat("\n")
        hexLines[i]
}

isECB <- function(inp){
        blocks <- chunk(inp,16)
        sum(duplicated(blocks))>0
}