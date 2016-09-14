setwd("C:/Users/u342907/Desktop/Pals")
source("2.14.R")

# ALL THESE FUNCTIONS ONLY WORK WITH THESE
# VALUES INITIALIZED IN THE GLOBAL ENVIRONMENT
# sKey <<- randkey()
# IV <<- randkey()

bitflipEn <- function(inp){
        if(!is.character(inp)){
                stop("Character input required")
                geterrmessage()
        }
        a1 <- "comment1=cooking%20MCs;userdata="
        a2 <- ";comment2=20like%20a%20pound%20of%20bacon"

        # strip invalid values ('=' and '&') out 'inp'
        inp <- sub("=","",inp)
        inp <- sub("&","",inp)

        # paste the comments and 'inp' together, convert to raw, and pad
        a <- paste(a1,inp,a2,sep="")
        b <- charToRaw(a)
        c <- padRaw(b,16)

        # encrypt the output
        EnCBC(c,IV,sKey)
}

bitflipDe <- function(inp){
        if(!is.raw(inp)){
                stop("Raw input required")
                geterrmessage()
        }

        # decrypt, unpad and convert to character
        a <- charToRaw(DeCBC(inp,IV,sKey))
        b <- unpadVal(a)
        c <- rawToChar(b)

        # determin if admin=true is in string
        grepl(";admin=true;",c)
}

bitflipBreak <- function(inp){
        # create desired text with "scrambled block" before
        a <- paste(rep(" ",16),collapse = "")
        b <- paste(a,"     ;admin=true",sep="")

        # Convert to raw and xor each byte with as.raw(1)
        c <- charToRaw(b)
        d <- xor(c,rep(as.raw(1),length(c)))
        e <- rawToChar(d)

        # Encrypt previous piece AND nothing (to find out
        # where the difference occurs)
        f <- chunk(bitflipEn(""),16)
        g <- chunk(bitflipEn(e),16)

        # Locate "scrambled block" and xor with as.raw(1)
        h <- match(TRUE,!(g %in% f))
        i <- xor(as.vector(unlist(g[h])),rep(as.raw(1),16))
        g[h] <- chunk(i,16)

        # Decrypt
        bitflipDe(as.vector(unlist(g)))

}