setwd("C:/Users/u342907/Desktop/Pals")
source("1.1.R")
source("1.3.R")
source("1.4.R")

text <- h2raw(b64h(readLines("1.6.Source.txt")))

ham <- function(a,b){
        a <- xor(a,b)
        b <- rawToBits(a)
        sum(b=="01")
}

chunk <- function(x,n){
        a <- suppressWarnings(split(x,0:(length(x)-1) %/% n))
        names(a) <- NULL
        a
}

unchunk <- function(x){
        as.vector(unlist(x))
}

# Determines key length of Vigenere encoded inp
drXOR <- function(inp){
        # initialize list
        a <- c()
        for(i in 2:50){
                temp <- c()
                for(j in 1:100){
                        temp[j] <- ham(inp[j],inp[j+i])
                }
                a[i-1] <- sum(temp)
        }
        match(min(a),a)+1
}

# determines repeating XOR decryption for inp given key Length n
breaker <- function(inp,n){
        # split into blocks
        blocks <- split(inp,1:n)

        # initialize raw output
        a <- c()

        # for each block:
        # - convert to hex
        # - perform singleByte
        # - store as output
        cat("Key: ")
        for(i in seq_along(blocks)){
                y <- raw2h(blocks[[i]])
                z <- singleByte(y)
                a[i] <- z[2]
                cat(rawToChar(as.raw(as.numeric(a[i]))))
        }

        # return list as raw vector
        cat("\n\n")
        as.raw(as.numeric(a))
}

breakVigenere <- function(inp){

        len <- suppressWarnings(drXOR(inp))
        cat(paste("Key Length:",as.character(len)))
        cat("\n\n")

        code <- breaker(inp,len)
        out <- suppressWarnings(rawToChar(xor(code,inp)))

        cat("\n\nMessage:\n")
        cat(out)
}


