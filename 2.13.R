setwd("C:/Users/u342907/Desktop/Pals")
source("2.12.R")

# ALL THESE FUNCTIONS ONLY WORK WITH THIS
# VALUE INITIALIZED IN THE GLOBAL ENVIRONMENT
# gKey <<- randkey()

parser <- function(inp){
        a <- strsplit(inp,"&")[[1]]
        b <- new.env()
        c <- lapply(a,FUN=function(x){
                y <- strsplit(x,"=")[[1]]
                b[[y[1]]] <- y[2]
        })
        b
}

emailer <- function(inp){
        inp <- sub("=","",inp)
        inp <- sub("&","",inp)
        a <- paste("email=",
                   inp,
                   "&uid=10&role=user",sep="")
        a
}

profile <- emailer("example@ok.gov")


emEn <- function(inp){
        a <- emailer(inp)
        a <- charToRaw(a)
        EnECB(a,gKey)
}

emDe <- function(inp){
        a <- rawToChar(DeECB(inp))
        parser(a)
}

emailBreak <- function(){
        a <- charToRaw("          admin")
        a <- c(a,rep(as.raw(11),11))

        b <- rep(as.raw(15),26)

        t <- emEn(rawToChar(a))[17:32]
        u <- emEn(rawToChar(b))[17:32]

        fish <- paste(paste(rep("a",3),collapse = ""),
                      "@gmail.com",
                      sep="")

        print(fish)
        out <- emEn(fish)
        out <- c(head(out,-16),t)
        out
}