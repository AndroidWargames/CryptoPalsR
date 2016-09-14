setwd("C:/Users/u342907/Desktop/Pals")
source("3.19.R")
source("2.16.R")

mainKey <- randkey()

bitflipEn2 <- function(inp){
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
        EnCTR(c,k=mainKey)
}

bitflipDe2 <- function(inp){
        if(!is.raw(inp)){
                stop("Raw input required")
                geterrmessage()
        }

        # decrypt, unpad and convert to character
        a <- EnCTR(inp,k=mainKey)
        b <- unpadRaw(a)
        c <- rawToChar(b)
        # determin if admin=true is in string
        grepl(";admin=true;",c)
}

bitflipBreak2 <- function(b){
        # create desired token
        # '=' (char(61)) xor char(1) = '<' (char(60))
        a <- ";admin<true"

        # encrypt our stupid token
        b <- bitflipEn2(a)
        l <- length(b)

        # for each byte of dumb token, xor (1)
        for(i in 1:l){
                # 0's, a 1, more zeros
                x <- c(raw(i-1),as.raw(1),raw(l-i))

                # test if that xor un-dumbs our stuff
                # print 'i' when we break (character the
                # bitflip worked for)
                if(bitflipDe2(xor(x,b))){
                        print(i)
                        break
                }
        }

}