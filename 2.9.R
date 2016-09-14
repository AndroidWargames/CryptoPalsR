setwd("C:/Users/u342907/Desktop/Pals")
source("1.1.R")

padRaw <- function(inp,p){
        len <- p - (length(inp) %% p)
        pd <- rep(as.raw(len),len)
        if(length(pd) == 0){
                pd <- rep(as.raw(p),p)
        }
        c(inp,pd)
}