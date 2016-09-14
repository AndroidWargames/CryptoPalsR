require(openssl)
require(digest)

setwd("C:/Users/u342907/Desktop/Pals")
source("1.1.R")
text <- readLines('1.7.Source.txt')

text <- h2raw(b64h(text))
yskey <- charToRaw("YELLOW SUBMARINE")
a <- AES(key=yskey,mode = "ECB")
cat(a$decrypt(text))
