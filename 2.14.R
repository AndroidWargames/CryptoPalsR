require(digest)
require(openssl)
setwd("C:/Users/u342907/Desktop/Pals")
source("2.10.R")
source("1.8.R")

randkey <- function() as.raw(sample(0:127,16,TRUE))

# ALL THESE FUNCTIONS ONLY WORK WITH THESE
# VALUES INITIALIZED IN THE GLOBAL ENVIRONMENT
#sKey <<- randkey()
#sec <<- "Um9sbGluJyBpbiBteSA1LjAKV2l0aCBteSByYWctdG9wIGRvd24gc28gbXkgaGFpciBjYW4gYmxvdwpUaGUgZ2lybGllcyBvbiBzdGFuZGJ5IHdhdmluZyBqdXN0IHRvIHNheSBoaQpEaWQgeW91IHN0b3A/IE5vLCBJIGp1c3QgZHJvdmUgYnkK"

baatEN2 <- function(inp){
        myst <- sec
        myst <- base64_decode(myst)
        out <- EnECB(c(sample(as.raw(0:127),
                              sample(1:32,1),
                              replace=TRUE),
                       inp,
                       myst),
                     sKey)
        out
}

cSize <- function(){
        l <- length(baatEN(""))
        for(i in 1:48){
                a <- paste(rep("A",i),collapse = "")
                if(length(baatEN(a))>l){
                        break
                }
        }
        length(baatEN(a)) - l
}

#isECB(baatEN("                                                     "))


baatDE2 <- function(){
        # create a dictionary, raw output 'out'
        dic <- new.env()
        out <- raw()

        # chunk a bunch of raw stuff and send it to the oracle
        r <- chunk(baatEN2(raw(64)),16)

        # discover the duplicated string, store as 's'
        s <- r[duplicated(r)][1]

        # initialize 'k'
        k <- 1

        # do while less than number of chunks
        while(k<length(r)+1){
                # for each byte of block
                for(i in 0:15){

                        t <- c(raw(16),rep(as.raw(65),15-i))

                        while(T){
                                u <- chunk(baatEN2(t),16)
                                if(s %in% u) break
                        }
                        if(!is.raw(u[(match(s, u) + k)][[1]])){
                                x1 <<- u[(match(s, u) + k)][[1]]
                                x2 <<- u
                                x3 <<- s
                                x4 <<- k
                        }
                        w <- base64_encode(u[(match(s,u)+k)][[1]])
                        for(j in 3:127){
                                while(T){
                                        v <- chunk(
                                                baatEN2(c(t,
                                                          out,
                                                          as.raw(j))),16)
                                        if(s %in% v) break
                                }
                                x <- base64_encode(
                                        v[(match(s,v)+k)][[1]])

                                dic[[x]] <- as.raw(j)
                        }
                        y <- dic[[w]]
                        if(!is.raw(y)) stop
                        cat(rawToChar(as.raw(y)))
                        out <- c(out,y)
                }
                k <- k + 1
        }
        out
}



