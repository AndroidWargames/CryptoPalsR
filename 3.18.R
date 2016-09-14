setwd("C:/Users/u342907/Desktop/Pals")
source("2.16.R")
require(openssl)

exCTR <- 'L77na/nrFsKvynd6HzOoG7GHTLXsTVu9qvY/2syLXzhPweyyMTJULu/6/kXX0KSvoOLSFQ=='

EnCTR <- function(inp,nonce=raw(8),k){

        # validate inputs as raw
        if(!is.raw(inp)) inp <- charToRaw(inp)
        if(!is.raw(nonce)) nonce <- charToRaw(nonce)
        if(!is.raw(k)) k <- charToRaw(k)

        # validate nonce as 8 bytes
        if(length(nonce)!=8){
                stop(paste("Invalid nonce: length = ",
                           as.character(length(nonce)),sep=""))
                geterrmessage()
        }
        # get length of input string
        t <- chunk(inp,16)

        # initialize output as raw
        out <- raw()

        # NOTE: seq_along begins at 1, not 0. values subtracted by one
        for(i in seq_along(t)){
                # set 'counter'
                counter <- as.raw((i-1) %% 256)
                for(j in 1:7){
                        counter <- c(counter,as.raw((i-1) %/% (256^(j))))
                }

                # append 'counter' to 'nonce'
                stream <- c(nonce,counter)

                # create encryption object & encrypt 'stream'
                a <- AES(k,mode="ECB")
                x <- a$encrypt(stream)

                # get relevant code chunk
                p <- unchunk(t[i])

                # shorten 'x' (keystream) if code chunk shorter than blocksize
                if(length(p)<16){
                        x <- head(x,length(p))
                }

                # xor chunk with keystream
                temp <- xor(x,p)

                # append result to output 'out'
                out <- c(out,temp)
        }
        out
}