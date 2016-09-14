setwd("C:/Users/u342907/Desktop/Pals")
source("2.14.R")
source("2.15.R")


cbcpadEn <- function(){
        # Read Text
        a <- sample(readLines('3.17.Source.txt'),1)

        # Create key and IV
        if(!exists("mainKey")) mainKey <<- randkey()
        IV1 <- randkey()

        # encrypt, attaching IV to head of string
        c(IV1,EnCBC(a,IV1,mainKey))
}

cbcpadDe <- function(inp){
        # chop off IV
        iv <- head(inp,16)
        inp <- tail(inp,-16)

        # decrypt text
        out <- DeCBC(inp,iv,mainKey)
        out

        # validate output
        ok <- FALSE
        try(ok <- is.raw(unpadVal(out)),silent=TRUE)
        ok
}


cbcpadBreak <- function(){
        # draw a random encrypted string
        inp <- cbcpadEn()
        out <- raw()
        # detemine # of blocks (subract one block to account for IV)
        l <- length(inp)/16-1

        # for each block
        for(i in 1:l){
                # take the last 32 characters of inp
                ct <- tail(inp,32)
                # initialize block output 'temp'
                temp <- raw(16)
                # for each byte in block
                for(j in 0:15){
                        # for each possible character
                        for(k in 128:0){
                                # create 'a', which is:
                                #       - a head of 00's
                                #       - the character to be tested
                                #       - a tail of padding bytes
                                a <- c(raw(15-j),
                                       as.raw(k),
                                       rep(as.raw(j+1),j))

                                # xor the preceding block against what's been
                                # solved already
                                x <- xor(head(ct,16),temp)

                                # xor THAT with 'a'
                                y <- xor(x,a)

                                # add the second half of bytes
                                y <- c(y,tail(ct,16))
                                if(cbcpadDe(y)){
                                        temp[16-j] <- xor(as.raw(k),as.raw(j+1))
                                        break
                                }
                        }
                        cat(rawToChar(temp[16-j]))
                }
                # chop off the last block of inp
                inp <- head(inp,-16)

                # append out to temp (we're working back to front)
                out <- c(temp,out)
        }
        unpadRaw(out)
}