setwd("C:/Users/u342907/Desktop/Pals")
source("3.19.R")

# mainKey <- randkey()
# set up cipher text
salsa <- paste(readLines('4.25.Source.txt'),collapse="")
ct4.25 <- EnCTR(DeECB(base64_decode(salsa),"YELLOW SUBMARINE"),
                k=mainKey)


CTReditor <- function(ct,k,offset,nt){
        # decrypt
        ct <- EnCTR(ct,k=mainKey)
        # change a byte
        ct[offset] <- as.raw(nt)
        # re-encrypt
        EnCTR(ct,k=mainKey)
}

breakCTR_rw <- function(inp){
        # determine the length
        l <- length(inp)
        # initialize output vector
        out <- raw()
        # for each byte
        for(j in 1:l){
                # take the first 'j' bytes
                x <- inp[1:j]
                # sub in letters until the subbed ciphertext
                # matches the unsubbed ciphertext
                for(i in 1:127){
                        if(all(CTReditor(x,mainKey,j,i)==x)) break
                }
                # append output
                out <- c(out,as.raw(i))
                # (hashtag)print4progress
                cat(rawToChar(as.raw(i)))
        }
        out
}