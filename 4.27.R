setwd("C:/Users/u342907/Desktop/Pals")
source("2.14.R")

mainKey = randkey()

bitflipEn3 <- function(inp){
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
        EnCBC(c,mainKey,mainKey)
}

bitflipDe3 <- function(inp){
        if(!is.raw(inp)){
                stop("Raw input required")
                geterrmessage()
        }

        # decrypt, unpad and convert to character
        a <- DeCBC(inp,mainKey,mainKey)
        if(is.character(a)) a <- charToRaw(a)
        b <- unpadRaw(a)
        # determine if admin=true is in string
        for(i in b){
                if(as.integer(i)>127){
                        warning("Tampering may have occured")
                        break
                }
        }

        b
}


breakIVKey <- function(){
        # split encryption into chunks
        a <- chunk(bitflipEn3(""),16)

        # set third chunk = 1
        a[3] <- a[1]

        # set second to 0's
        a[2] <- list(raw(16))

        # unchunk and decrypt
        b <- unchunk(a)
        c <- bitflipDe3(b)

        # chunk and xor
        d <- chunk(c,16)
        xor(d[[1]],d[[3]])
}