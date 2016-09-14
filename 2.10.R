require(digest)
setwd("C:/Users/u342907/Desktop/Pals")
source("1.1.R")
source("1.6.R")
source("2.9.R")

k <- charToRaw("YELLOW SUBMARINE")

IV <- as.raw(rep(0,16))

mess <- h2raw(b64h(unlist(readLines('2.10.source.txt'))))

unpadRaw <- function(inp){
        head(inp,-1*as.integer(tail(inp,1)))
}

DeCBC <- function(inp,iv,key){

        a <- AES(key,mode="ECB")
        blocks <- chunk(c(iv,inp),16)
        out <- raw(0)
        for(i in 2:length(blocks)){
                temp <- xor(blocks[[i-1]],
                        a$decrypt(blocks[[i]],raw=T))
                out <- c(out,temp)
        }
        out
}

EnCBC <- function(inp,iv,key){
        if(class(inp)=="character") inp <- charToRaw(inp)
        out <- iv
        inp <- padRaw(inp,16)
        a <- AES(key,mode="ECB")
        blocks <- chunk(inp,16)
        for(i in 1:length(blocks)){
                temp <- xor(tail(out,16),blocks[[i]])
                temp <- a$encrypt(temp)
                out <- c(out,temp)
        }
        out[-16:0]
}

EnECB <- function(inp,key){
        if(class(inp)=="character"){inp <- charToRaw(inp)}
        inp <- padRaw(inp,p=16)
        a <- AES(k,mode="ECB")
        a$encrypt(inp)
}

DeECB <- function(inp,key){
        a <- AES(k,mode="ECB")
        unpadRaw(a$decrypt(inp,raw=T))
}

#DeCBC(mess,IV,k)