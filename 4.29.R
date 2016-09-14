setwd('C:/Users/u342907/Desktop/Pals/')
source("4.28.R")



gluepad <- function(ml){
        # make a 1 and some 0's
        out <- h2raw("80")

        # add more 0's
        while((ml+length(out)) %% 64!=56) out <- c(out,raw(1))

        # add the message length
        out <- c(out,longToRaw(ml*8))
        out
}

SHA1hack <- function(inp,a1,b1,c1,d1,e1,fl){

        # verify inputs
        if(is.character(inp)) inp <- charToRaw(inp)
        if(!is.raw(inp)) inp <- as.raw(inp)

        # initialize variables
        h0 <- a1
        h1 <- b1
        h2 <- c1
        h3 <- d1
        h4 <- e1

        ml <- 8*length(inp)

        inp <- c(inp,gluepad(length(inp) + fl))
        x1 <- chunk(inp,64)
        for(i in x1){
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

                for(j in 1:80){
                        if(1 <= j & j <= 20){
                                f <- (b&c)|((!b)&d)
                                k <- h2raw("5a827999")
                        }else if(21 <= j & j <= 40){
                                f <- chainXOR(b,c,d)
                                k <- h2raw("6ed9eba1")
                        }else if(41 <= j & j <= 60){
                                f <- (b&c)|(b&d)|(c&d)
                                k <- h2raw("6ed9eba1")
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

                h0 <- intToRaw((rawToInt(h0) + rawToInt(a)) %% 2^32)
                h1 <- intToRaw((rawToInt(h1) + rawToInt(b)) %% 2^32)
                h2 <- intToRaw((rawToInt(h2) + rawToInt(c)) %% 2^32)
                h3 <- intToRaw((rawToInt(h3) + rawToInt(d)) %% 2^32)
                h4 <- intToRaw((rawToInt(h4) + rawToInt(e)) %% 2^32)
                hh <- c(h0,h1,h2,h3,h4)

        }

        hh
}

#xmsg <- "comment1=cooking%20MCs;userdata=foo;comment2=%20like%20a%20pound%20of%20bacon;admin=true"
breakSHA1Mac <- function(xmsg){

        # get a random word for the key
        bk <- ""
        while(bk == "") bk <- sample(readLines('English.txt'),1)
        bk <- strsplit(bk," ")
        bk <- sample(bk[[1]],1)

        # this is the input message, we'll steal the hash
        msg <- "comment1=cooking%20MCs;userdata=foo;comment2=%20like%20a%20pound%20of%20bacon"
        hash <- SHA1(paste(bk,msg,sep=""))
        # these will feed the SHA1hack
        hackinserts <- chunk(hash,4)

        # cheats gets us a list of raw vectors that might be the
        # valid MACs, depending on the length of the initial message,
        # we'll try the first 20 multiples of 64 bytes (512 bits)
        # x is the message we want to cheat in, b is the 'old' hash
        cheatsSHA <- function(x,b){
                out <- list()
                for(i in 1:20){

                        out[[i]] <- SHA1hack(xmsg,
                                  b[[1]],
                                  b[[2]],
                                  b[[3]],
                                  b[[4]],
                                  b[[5]],
                                  64*i
                                 )
                }
                out
        }

        # get some cheats
        cheat <- cheatsSHA(xmsg,hackinserts)

        # this will be what would actually happen, to prove we'll
        # get a valid mac
        liar <- SHA1(c(charToRaw(bk),
                     charToRaw(msg),
                     gluepad(nchar(bk)+nchar(msg)),
                     charToRaw(xmsg)))

        # array of boolean, true index indicates the length of
        # the key + original message
        secretlength <- lapply(cheat,FUN = function(x) all(x==liar))
        secretlength
}




