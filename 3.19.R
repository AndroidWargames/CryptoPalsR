setwd("C:/Users/u342907/Desktop/Pals")
source("3.18.R")
source("1.3.R")

if(!exists("mainKey")) mainKey <<- randkey()

# So here's the theory behind this file:
#
# Basically, If we XOR all the ciphertexts against
# one specific ciphertext (the reference string), we
# should get the XOR of their respective plaintexts
# (because they've all been XOR'd against the same
# keystream). Interestingly, character 32 (" ")
# below 64 on the ASCII Table, which means that the XOR
# of " " and any character is going to be
# greater than that of other characters, the cutoff, being at
# 64. So what we do is go byte by byte through the XORs
# (e.g. first iteration we take the first byte from each
# XOR) and we determine whether there are greater
# or fewer values above 64. if there are more, then the
# reference string is likely a " ". If there are fewer,
# we assume the most frequent among those over-64 values
# are actually the space (which we're hoping is more frequent
# than ',' or '.', etc). Ultimately we're left with a set
# of ciphertexts, and the best guess at a decryption
# keystream
#
# Additionally, we have the Editor, which (since the other
# method isn't error proof) allows us to edit byte by byte,
# substituting obvious corrections and updating the
# keystream.


# here we go...

# load plaintexts and create ciphertexts
pts <- lapply(readLines('3.19.Source.txt'),base64_decode)
cts <- lapply(pts,FUN=function(x) EnCTR(x,k=mainKey))
# kill it so you can't see it
pts <- raw()

# get second-longest string
n <- unlist(lapply(cts,length))
m <- length(n)
ml <- sort(n,partial=m-1)[m-1]
Q <- match(ml,n)

a <- list()
for(i in 1:length(cts)){
                # figure which one is shorter
                l <- min(length(cts[[Q]]),
                         length(cts[[i]]))
                # cut to short length
                x <- head(cts[[Q]],l)
                y <- head(cts[[i]],l)
                #store value in a
                a[i] <- list(xor(x,y))
}

# set master values
masters <- cts
# set reference string
master <- masters[[Q]]

noncense <- function(a,cts,master){
        #for each byte
        ml <- max(unlist(lapply(cts,length)))
        for(i in 1:ml){
                # exclude finished strings
                cts <- cts[n>=i]
                n <- n[n>=i]
                # create blank raws
                cl <- length(cts)
                x1 <- raw(cl)
                x2 <- raw(cl)
                # fill raws with nth bytes
                for(j in 1:cl){
                        x1[j] <- a[[j]][i]
                        x2[j] <- cts[[j]][i]
                }
                # convert to int
                y <- unlist(lapply(x1,as.integer))
                # figure out proportion over/under 64
                z <- sum(y>=64)/length(y)
                if(z>.5){
                        # assign xor(byte," ")
                        out[i] <- xor(as.raw(32),master[i])
                }else{
                        # take only numbers over 64
                        v <- y[y>=64]
                        if(length(v)==0) v <- 1
                        # get the most frequent byte
                        v1 <- match(max(tabulate(v)),tabulate(v))
                        # convert to raw
                        mo <- as.raw(v1)
                        # xor with " "
                        no <- xor(as.raw(32),mo)
                        # xor with reference ciphertext
                        out[i] <- xor(no,master[i])
                }
        }
        out
}

# done <- breaker(cts,master)

EditorDisplay <- function(out,masters){
        lapply(masters,FUN=function(x){
                a <- min(length(x),length(out))
                x <- head(x,a)
                b <- head(out,a)
                rawToChar(xor(x,b))
        })
}

Editor <- function(out,Q,s,f,masters){
        # XOR replacement character (index 'Q')
        # with s and f, the old and new character values
        out[Q] <- xor(out[Q],xor(charToRaw(s),charToRaw(f)))
        print(EditorDisplay(out,masters))
        out
}

# Final decryption (hex coded):
# "ee699ab278da4ffd662888d1dc32b30a276981983637ba1dfb518dda98f8372a31cf154f"
