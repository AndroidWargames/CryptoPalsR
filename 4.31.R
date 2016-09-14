setwd('C:/Users/u342907/Desktop/Pals/')
source("4.28.R")

HMAC <- function(key, msg){

        # verify inputs
        if(is.character(msg)) msg <- charToRaw(msg)
        if(!is.raw(msg)) msg <- as.raw(msg)
        if(is.character(key)) key <- charToRaw(key)
        if(!is.raw(key)) key <- as.raw(key)

        if(length(key) > 64){
                key <- c(SHA1(key),raw(44))
        }
        if(length(key) < 64){
                key <- c(key,raw(64-length(key)))
        }

        okp <- xor(h2raw(paste(rep("5c",64),collapse = "")),key)
        ikp <- xor(h2raw(paste(rep("36",64),collapse = "")),key)

        raw2h(SHA1(c(okp,SHA1(c(ikp,msg)))))
}