setwd("C:/Users/u342907/Desktop/Pals")
source("1.1.R")

"1c0111001f010100061a024b53535009181c" #string 1
"686974207468652062756c6c277320657965" #string 2

hexor <- function(x,y){
        raw2h(xor(h2raw(x),h2raw(y)))
}

"746865206b696420646f6e277420706c6179" #output string