setwd("C:/Users/u342907/Desktop/Pals")
source("3.23.R")
source("3.19.R")

# VERY SLOW brute force break of the MT stream

breakMTstream <- function(){

        # Create "AAAAAAAAAAAAAAAA"
        test <- rep(as.raw(65),16)

        # create random bytes
        rpre <- as.raw(sample(1:127,sample(1:12,1)))

        # pick a random seed
        s <- sample(1:2^16,1)

        # pull "AAAA" from characters 13-16
        a <- EnMT(c(rpre,test),seed=s)[13:16]

        # convert it to an integer
        b <- rawToInt(xor(a,rep(as.raw(65),4)))

        # try the fourth output of every 16-bit integer
        for(i in 1:2^16){
                print(i) # print for progress (2^16 = 65,536)
                Rand(i) # seed/run it
                Rand()
                Rand()
                if(Rand()==b){
                        cat("Seed: \n")
                        return(i)
                }
        }
        "Failure"
}

# makes random token
makeToken <- function(){
        x <- as.integer(Sys.time())
        cat("Generating Token...\n")
        Sys.sleep(sample(1:5,1))
        cat("Token Generated\n")
        EnMT("        ",x)
}


# returns T if token was generated in last five minutes
findToken <- function(inp){
        a = inp
        x <- as.integer(Sys.time())
        for(i in 0:300){
                b <-EnMT("        ",x-i)
                b <- b
                if(sum(a==b)==8){
                        return(T)
                        break
                }
        }
        return(F)
}


EnMT <- function(inp,seed,craw=T){

        # verify seed is int
        if(!is.integer(seed)){
                stop("Integer required for argument 'seed'")
                geterrmessage()
        }
        # Require 16 bit seed
        seed <- seed %% 2^16

        # convert inp to raw
        if(is.character(inp)){
                inp <- charToRaw(inp)
        }

        # init 'RandObj' if necessary
        if(!exists("RandObj")) Rand(0)

        # seed 'RandObj'
        seed_mt(seed=seed,rando=RandObj)

        # initialize output
        out <- raw()

        # generate random output until enough exists to XOR
        # with input text
        while(length(out)<length(inp)){
                out <- c(out,intToRaw(Rand()))
        }

        # cut output to exactly input length
        out <- out[1:length(inp)]

        # xor out with input
        out <- xor(out,inp)

        # convert to character if requested
        if(!craw) out <- rawToChar(out)
        out
}


intToRaw <- function(inp){
        out <- raw()
        if(inp>2^32 | inp<0){
                stop("Number out of range")
                geterrmessage()
        }

        for(i in 1:4){
                a <- inp %% 256
                out <- c(as.raw(a),out)
                inp <- inp %/% 256
        }

        out

}


rawToInt <- function(inp){
        l <- length(inp)
        out <- 0
        if(l>4){
                stop("Too many bits!")
                geterrmessage()
        }else if(l<1){
                stop("Not enough bits!")
                geterrmessage()
        }else{
                for(i in 0:l-1){
                        out <- out + as.integer(inp[l-i])*256^i
                }
        }
        out
}