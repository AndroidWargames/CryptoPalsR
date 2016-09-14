setwd('C:/Users/u342907/Desktop/Pals/')

getFiles <- function(){
        a <- list.files(getwd(),pattern="*.R")
        b <- lapply(a,FUN=function(x){
                paste(readLines(x),collapse="\n")
                }
                )
        colie <-paste(rep(rawToChar(as.raw(10)),3),collapse="")
        d <- paste(c(rbind(a,b)),collapse=colie)
        write(d,file="outie.txt")

}