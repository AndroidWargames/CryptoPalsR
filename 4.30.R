setwd('C:/Users/u342907/Desktop/Pals/')
source("4.29.R")


# Algorithm taken from "https://tools.ietf.org/html/rfc1320"
#

keyMacSHA1 <- function(m,k){
        # verify inputs
        if(is.character(m)) m <- charToRaw(m)
        if(is.character(k)) k <- charToRaw(k)

        MD4(c(k,m))
}

add32 <- function(a,b){
        intToRaw((rawToInt(a) + rawToInt(b)) %% 2^32)
}

mdF <- function(a,b,c,d,k,s){
        lrotate(add32(add32(a,
                            (b&c)|(!b&d)
                            ),
                      k
                      ),s)
}

mdG <- function(a,b,c,d,k,g,s){
        lrotate(add32(add32((b&c)|(b&d)|(c&d),
                            a),
                      add32(k,
                            g)),
                s)

}

mdH <- function(a,b,c,d,k,h,s){
        lrotate(add32(add32(chainXOR(b,c,d),
                            a),
                      add32(k,
                            h)),
                s)
}


MD4 <- function(inp){

        # verify inputs
        if(is.character(inp)) inp <- charToRaw(inp)
        if(!is.raw(inp)) inp <- as.raw(inp)

        # initialize variables
        A <- h2raw("01234567")
        B <- h2raw("89abcdef")
        C <- h2raw("fedcba98")
        D <- h2raw("76543210")

        z1 <- h2raw("5a827999")
        z2 <- h2raw("6ed9eba1")

        inp <- shaPre(inp)

        x1 <- chunk(inp,64)
        for(i in x1){
                w <- chunk(i,4)

                AA <- A
                BB <- B
                CC <- C
                DD <- D

                # Round 1
                for(j in 0:3){
                        AA <- mdF(AA,BB,CC,DD,w[[j*4+1]],3)
                        DD <- mdF(DD,AA,BB,CC,w[[j*4+2]],7)
                        CC <- mdF(CC,DD,AA,BB,w[[j*4+3]],11)
                        BB <- mdF(BB,CC,DD,AA,w[[j*4+4]],19)
                }

                # Round 2
                for(j in 0:3){
                        AA <- mdG(AA,BB,CC,DD,z1,w[[j*4+1]],3)
                        DD <- mdG(DD,AA,BB,CC,z1,w[[j*4+2]],5)
                        CC <- mdG(CC,DD,AA,BB,z1,w[[j*4+3]],9)
                        BB <- mdG(BB,CC,DD,AA,z1,w[[j*4+4]],13)
                }

                # Round 3
                for(j in 0:3){
                        AA <- mdH(AA,BB,CC,DD,z2,w[[j*4+1]],3)
                        DD <- mdH(DD,AA,BB,CC,z2,w[[j*4+2]],9)
                        CC <- mdH(CC,DD,AA,BB,z2,w[[j*4+3]],11)
                        BB <- mdH(BB,CC,DD,AA,z2,w[[j*4+4]],15)
                }

                A <- add32(AA,A)
                B <- add32(BB,B)
                C <- add32(CC,C)
                D <- add32(DD,D)
                hh <- c(A,B,C,D)
        }

        hh
}

MD4hack <- function(inp,a,b,c,d,fl){

        # verify inputs
        if(is.character(inp)) inp <- charToRaw(inp)
        if(!is.raw(inp)) inp <- as.raw(inp)

        A <- a
        B <- b
        C <- c
        D <- d

        z1 <- h2raw("5a827999")
        z2 <- h2raw("6ed9eba1")

        inp <- c(inp,gluepad(length(inp) + fl))

        x1 <- chunk(inp,64)
        for(i in x1){
                w <- chunk(i,4)

                AA <- A
                BB <- B
                CC <- C
                DD <- D

                # Round 1
                for(j in 0:3){
                        AA <- mdF(AA,BB,CC,DD,w[[j*4+1]],3)
                        DD <- mdF(DD,AA,BB,CC,w[[j*4+2]],7)
                        CC <- mdF(CC,DD,AA,BB,w[[j*4+3]],11)
                        BB <- mdF(BB,CC,DD,AA,w[[j*4+4]],19)
                }

                # Round 2
                for(j in 0:3){
                        AA <- mdG(AA,BB,CC,DD,z1,w[[j*4+1]],3)
                        DD <- mdG(DD,AA,BB,CC,z1,w[[j*4+2]],5)
                        CC <- mdG(CC,DD,AA,BB,z1,w[[j*4+3]],9)
                        BB <- mdG(BB,CC,DD,AA,z1,w[[j*4+4]],13)
                }

                # Round 3
                for(j in 0:3){
                        AA <- mdH(AA,BB,CC,DD,z2,w[[j*4+1]],3)
                        DD <- mdH(DD,AA,BB,CC,z2,w[[j*4+2]],9)
                        CC <- mdH(CC,DD,AA,BB,z2,w[[j*4+3]],11)
                        BB <- mdH(BB,CC,DD,AA,z2,w[[j*4+4]],15)
                }

                A <- add32(AA,A)
                B <- add32(BB,B)
                C <- add32(CC,C)
                D <- add32(DD,D)
                hh <- c(A,B,C,D)
        }

        hh
}

#xmsg <- "comment1=cooking%20MCs;userdata=foo;comment2=%20like%20a%20pound%20of%20bacon;admin=true"
breakMD4Mac <- function(xmsg){

        # get a random word for the key
        bk <- ""
        while(bk == "") bk <- sample(readLines('English.txt'),1)
        bk <- strsplit(bk," ")
        bk <- sample(bk[[1]],1)

        # this is the input message, we'll steal the hash
        msg <- "comment1=cooking%20MCs;userdata=foo;comment2=%20like%20a%20pound%20of%20bacon"
        hash <- MD4(paste(bk,msg,sep=""))
        # these will feed the SHA1hack
        hackinserts <- chunk(hash,4)

        # cheats gets us a list of raw vectors that might be the
        # valid MACs, depending on the length of the initial message,
        # we'll try the first 20 multiples of 64 bytes (512 bits)
        # x is the message we want to cheat in, b is the 'old' hash


        # get some cheats
        cheat <- cheatsMD(xmsg,hackinserts)

        # this will be what would actually happen, to prove we'll
        # get a valid mac
        liar <- MD4(c(charToRaw(bk),
                       charToRaw(msg),
                       gluepad(nchar(bk)+nchar(msg)),
                       charToRaw(xmsg)))

        # array of boolean, true index indicates the length of
        # the key + original message
        secretlength <- lapply(cheat,FUN = function(x) all(x==liar))

        # return valid hash
        cheat[[match(T,secretlength)]]

}

cheatsMD <- function(x,b){
        out <- list()
        for(i in 1:20){

                out[[i]] <- MD4hack(x,
                                    b[[1]],
                                    b[[2]],
                                    b[[3]],
                                    b[[4]],
                                    64*i
                )
        }
        out
}
