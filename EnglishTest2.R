unigram <- function(){
        freq <- c(0.08167,0.01492,0.02782,0.04253,0.12702,0.02228,0.02015,0.06094,0.06966,0.00153,0.00772,0.04025,0.02406,0.06749,0.07507,0.01929,0.00095,0.05987,0.06327,0.09056,0.02758,0.00978,0.0236,0.0015,0.01974,0.00074)
        data.frame(letters,freq,stringsAsFactors = FALSE)
}

grammer <- function(text){
        cases <- strsplit(text,"")[[1]]
        text <- tolower(text)
        a <- strsplit(text,"")[[1]]
        x <- length(a)
        z <- unlist(lapply(a,FUN = function(x){
                tolower(x) %in% c(letters,".","'",'"',","," ")
        }
        ))
        spaces <- length(a)/(length(a[a==" "])+1)
        a <- a[z]
        z <- sum(z)
        b <- c(" ",a)
        a <- c(a," ")
        x <- x-length(a)
        out <- paste(b,a,sep="")
        sum(z)/(x+length(a))-abs(spaces-5.5)/5

}


grammer2 <- function(text){
        cases <- strsplit(text,"")[[1]]
        a <- strsplit(text,"")[[1]]
        x <- length(a)
        # get a count of letters and spaces
        z <- unlist(lapply(a,FUN = function(x){
                x %in% c(LETTERS,letters," ")
        }))
        b <- sum(z)/x

        c <- a[unlist(lapply(a,FUN = function(x){
                x %in% c(LETTERS,letters)
        }))]

        # compare frequency with standard
        d <- converter(c)[,2]
        e <- unigram()[,2]
        # math a little bit to normalize
        f <- abs(d-e)
        g <- 1-f
        g <- g[g==min(g)]
        g <- g[g==min(g)]

        # add bias against capital letters
        h <- sum(unlist(lapply(a,FUN = function(x){
                x %in% c(LETTERS)
        })))
        prod(g)*b-(.0001*h)

}



alternate <- function(List1,List2){
        len <- max(length(List1),length(List2))
        List1 <- List1[1:len]
        List2 <- List2[1:len]
        outList <- unlist(as.list(matrix(c(List1,List2),
                                  ncol=len,
                                  byrow=TRUE)))
        outList[!is.na(outList)]
}

converter <- function(inp){
        p <- unigram()
        p[,2] <- 0
        for(x in inp){
                q <- match(tolower(x),letters)
                p[q,2] <- p[q,2]+1
        }
        y <- sum(p[,2])
        p[,2] <- p[,2]/y
        p
}