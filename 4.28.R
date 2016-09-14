setwd('C:/Users/u342907/Desktop/Pals/')
source("3.24.R")

keyMacSHA1 <- function(m,k){
        # verify inputs
        if(is.character(m)) m <- charToRaw(m)
        if(is.character(k)) k <- charToRaw(k)

        SHA1(c(k,m))
}




SHA1 <- function(inp){

        # verify inputs
        if(is.character(inp)) inp <- charToRaw(inp)
        if(!is.raw(inp)) inp <- as.raw(inp)

        # initialize variables
        h0 <- h2raw("67452301")
        h1 <- h2raw("EFCDAB89")
        h2 <- h2raw("98BADCFE")
        h3 <- h2raw("10325476")
        h4 <- h2raw("C3D2E1F0")

        ml <- 8*length(inp)

        # add pad
        inp <- shaPre(inp)

        # cut into pieces
        x1 <- chunk(inp,64)

        for(i in x1){
                # cut into 32-bit words
                w <- chunk(i,4)
                for(j in 17:80){
                        w[[j]] <- lrotate(
                                chainXOR(w[[j-3]],
                                         w[[j-8]],
                                         w[[j-14]],
                                         w[[j-16]]),
                                1)
                }
                a <- h0
                b <- h1
                c <- h2
                d <- h3
                e <- h4

                # do the whole jumbly thing
                for(j in 1:80){
                        if(1 <= j & j <= 20){
                                f <- xor(d,(b&xor(c,d)))
                                k <- h2raw("5a827999")

                        }else if(21 <= j & j <= 40){
                                f <- chainXOR(b,c,d)
                                k <- h2raw("6ed9eba1")

                        }else if(41 <= j & j <= 60){
                                f <- (b&c)|(d&(b|c))
                                k <- h2raw("8f1bbcdc")

                        }else if(61 <= j & j <= 80){
                                f <- chainXOR(b,c,d)
                                k <- h2raw("ca62c1d6")
                        }

                        temp <- rawToInt(lrotate(a,5)) +
                                rawToInt(f) + rawToInt(e) +
                                rawToInt(k) + rawToInt(w[[j]])
                        temp <- intToRaw(temp %% 2^32)
                        e <- d
                        d <- c
                        c <- lrotate(b,30)
                        b <- a
                        a <- temp
                }

                # reassign words
                h0 <- intToRaw((rawToInt(h0) + rawToInt(a)) %% 2^32)
                h1 <- intToRaw((rawToInt(h1) + rawToInt(b)) %% 2^32)
                h2 <- intToRaw((rawToInt(h2) + rawToInt(c)) %% 2^32)
                h3 <- intToRaw((rawToInt(h3) + rawToInt(d)) %% 2^32)
                h4 <- intToRaw((rawToInt(h4) + rawToInt(e)) %% 2^32)
                hh <- c(h0,h1,h2,h3,h4)

        }

        hh
}

sha1Validator <- function(inp){
        paste(as.character(SHA1(inp)),collapse="")==sha1(inp)
}


# this will rotate the number a bit to the left
lrotate <- function(inp,i){
        out <- 0
        if(is.raw(inp)) inp <- rawToInt(inp)
        out <- bitShiftL(inp,i)
        out <- out + bitShiftR(inp,32-i)
        intToRaw(out)
}

# this will perform a sequence of xors on raw inputs
chainXOR <- function(...){
        x <- raw(1)
        for(i in list(...)){
                if(!is.raw(i)){
                        stop("Raw input required")
                        geterrmessage()
                }
        }
        for(i in list(...)){
                x <- xor(x,i)
        }
        x
}

# this formats by adding the SHA1 Pad
shaPre <- function(inp){
        ml <- length(inp)
        # add a 1 and some 0's
        inp <- c(inp,h2raw("80"))

        # add more 0's
        while(length(inp) %% 64!=56) inp <- c(inp,raw(1))

        # add the message length
        inp <- c(inp,longToRaw(ml*8))
        inp
}

# this converts from big (64 bit) numberic values to raw
longToRaw <- function(inp){
        out <- raw()
        if(inp>2^64 | inp<0){
                stop("Number out of range")
                geterrmessage()
        }
        for(i in 1:8){
                a <- inp %% 256
                out <- c(as.raw(a),out)
                inp <- inp %/% 256
        }

        out
}

# this converts from raw to big (64 bit) numeric values
rawToLong <- function(inp){
        l <- length(inp)
        out <- 0
        if(l>8){
                stop("Too many bits!")
                geterrmessage()
        }else if(l<1){
                stop("Not enough bits!")
                geterrmessage()
        }else{
                for(i in 0:l-1){
                        out <- out + as.numeric(inp[l-i])*256^i
                }
        }
        out
}
