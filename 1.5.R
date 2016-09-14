setwd("C:/Users/u342907/Desktop/Pals")
source("1.1.R")

burn <- "Burning 'em, if you ain't quick and nimble
I go crazy when I hear a cymbal"

repXOR <- function(text,ex){
        r <- charToRaw(text)
        x <- charToRaw(ex)
        xx <- rep(x,(length(r)/length(x))-1)
        raw2h(xor(r,xx))
}

#repXOR(burn,"ICE") == "0b3637272a2b2e63622c2e69692a23693a2a3c6324202d623d63343c2a26226324272765272a282b2f20430a652e2c652a3124333a653e2b2027630c692b20283165286326302e27282f"