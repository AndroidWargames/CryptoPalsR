setwd("C:/Users/u342907/Desktop/Pals")
require(base64enc)
paste("49276d206b696c6c696e6720",
      "796f757220627261696e206c",
      "696b65206120706f69736f6e",
      "6f7573206d757368726f6f6d",sep="")

h2raw <- function(inp){
        h <- sapply(seq(1, nchar(inp), by=2),
                    function(x) substr(inp, x, x+1))
        as.raw(strtoi(h, 16L))
}

raw2h <- function(inp){
        paste(as.character(inp),collapse="")
}

hb64 <- function(inp){
        base64encode(h2raw(inp))
}

b64h <- function(inp){
        raw2h(base64decode(inp))
}
