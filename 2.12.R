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

baatEN <- function(inp){
        myst <- sec
        myst <- base64_decode(myst)
        out <- EnECB(c(inp,myst),sKey)
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


baatDE <- function(){
        dic <- new.env()
        l <- length(baatEN(raw()))
        out <- raw()

        for(i in 1:l){
                # make a prepended repeating string
                pre <- c(rep(as.raw(65),l-i))
                code <- head(baatEN(pre),l)
                sauce <- base64_encode(code)
                for(i in 0:127){
                        temp <- base64_encode(head(baatEN(c(pre,out,as.raw(i))),l))
                        dic[[temp]] <- as.raw(i)
                }
                #tryCatch(dic[[sauce]],break)
                out <- c(out, dic[[sauce]])
                cat(rawToChar(dic[[sauce]]))
        }
        out
}



