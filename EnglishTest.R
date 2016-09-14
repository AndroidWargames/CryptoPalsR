setwd("F:/R/Pals")

English <- function(text){
        testEnglish(text) < .05
}

EngMatrix <- function(text){
        chr <- strsplit(text,split="")[[1]]
        dat1 <- data.frame(chr,stringsAsFactors=FALSE)
        dat1$num1 <- lapply(dat1$chr,FUN=function(x){
                as.integer(charToRaw(x))
        })

        dat1$num2 <- c(rep(NA, 1), head(dat1$num1, -1))
        mat1 <- matrix(data=0,nrow=128,ncol=128)
        for(i in 2:(nrow(dat1)-1)){
                a <- as.integer(dat1[i,2])
                b <- as.integer(dat1[i,3])
                mat1[a,b] <- mat1[a,b] + 1
        }

        mat1

}

EngList <- function(text){
        chr <- strsplit(text,split="")[[1]]
        dat1 <- data.frame(chr,stringsAsFactors=FALSE)
        dat1$num1 <- lapply(dat1$chr,FUN=function(x){
                as.integer(charToRaw(x))
        })
        mat1 <- matrix(data=0,nrow=128,ncol=1)
        for(i in 1:nrow(dat1)){
                a <- as.integer(dat1[i,2])
                mat1[a,1] <- mat1[a,1] + 1
        }

        mat2 <- matrix(data=0,nrow=128,ncol=1)
        for(i in 1:128){
                mat2[i,1] = mat1[i,1]/sum(mat1,na.rm=TRUE)
        }
        mat2
}

getEnglish <- readChar("English.txt",file.info("English.txt")$size)

StandardMatrix <- EngMatrix(getEnglish)
StandardList <- EngList(getEnglish)

testEnglish <- function(inp,
                        sm=StandardMatrix,
                        sl=StandardList){
        a <- abs(EngMatrix(inp)-sm)
        b <- abs(EngList(inp)-sl)


        for(i in 1:128){
                for(j in 1:128){
                        a[i,j] <- a[i,j]*b[j,1]
                }
        }

        sum(a,na.rm=TRUE)
}