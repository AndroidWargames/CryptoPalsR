setwd("C:/Users/u342907/Desktop/Pals")
source("1.1.R")
require(bitops)

# this architecture stores an environment called 'RandObj'
# to the global environment. This means that all variables
# are retrievable from RandObj at any time, but for the
# purposes of the challenge, its values are never referenced
# by the 'attacker'

Rand <- function(seed = integer()){
        # test for existing Random Object, create one
        if(!exists("RandObj")){
                RandObj <<- initRando()
        }

        # test for seed, re-seed if argument exists
        if(length(seed)>0){
                RandObj <<- seed_mt(RandObj,seed)
        }

        # pull the correct value from the object
        RandObj <<- extract_number(RandObj)

        # return that value
        RandObj$y
}

RandSeed <- function(seed){
        # test for existing Random Object, create one
        # (this is so we can set the seed independent
        # of drawing a number, required for challenge 22)
        if(!exists("RandObj")){
                RandObj <<- initRando()
        }
        # seed Random Object
        if(length(seed)>0){
                RandObj <<- seed_mt(RandObj,seed)
        }
}

initRando <- function(){

        # initialization for MT19937 per the values
        # given by wikipedia

        X <- new.env()
        X$w1 <- 32
        X$n1 <- 624
        X$m1 <- 397
        X$r1 <- 31
        X$a1 <- h2int("9908b0df")
        X$u1 <- 29
        X$d1 <- h2int("ffffffff")
        X$s1 <- 17
        X$b1 <- h2int("9d2c5680")
        X$t1 <- 37
        X$c1 <- h2int("efc60000")
        X$l1 <- 43
        X$f1 <- 1812433253

        X$lower_mask <- (bitShiftL(1,X$r1-1)*2-1)
        X$upper_mask <- bitFlip(X$lower_mask)

        X$MT <- integer(X$n1)
        X$index <- X$n1+1
        X
}

h2int <- function(inp){
        # converts the hex masks to integer values
        # for processing

        # convert to raw
        r <- h2raw(inp)
        l <- length(r)
        out = 0
        # multiply the integer value of each byte by
        # 256^(length-index value)
        for(i in 1:l) out <- out +
                as.integer(r[i])*(256^(l-i))
        out
}

seed_mt <- function(rando,seed){
        # seeding coded per pseudocode on wikipedia for
        # MT19937

        X <- rando
        X$seed <- seed
        X$MT[1] <- seed
        for(i in 2:X$n1){
                X$a1 <- X$MT[i-1]
                X$a2 <- bitShiftR(X$MT[i-1],X$w1-2)
                X$MT[i] <- (X$f1*(bitXor(X$a1,X$a2))+i) %% 2 ^ 32

        }
        X$index <- X$n1+1
        X
}

extract_number <- function(rando){
        X <- rando
        # twist when end state is reached
        if(X$index>length(X$MT)){
                X <- twist(X)
        }
        # pull from correct index
        X$y <- X$MT[X$index]

        # shift right
        X$y <- bitXor(
                X$y,bitShiftR(X$y,X$u1)
        )

        # shift left and mask
        X$y <- bitXor(
                X$y,bitAnd(
                        bitShiftL(X$y,X$s1)
                        ,X$b1)
        )

        # shift left and mask
        X$y <- bitXor(
                X$y,bitAnd(
                        bitShiftL(X$y,X$t1)
                        ,X$c1)
        )
        # shift right
        X$y <- bitXor(X$y, bitShiftR(X$y,X$l1))

        # increment index
        X$index <- X$index + 1
        # return random object
        X
}

twist <- function(rando){
        X <- rando
        for(i in 1:(X$n1-1)){
                # x <- (MT[i] & upper_mask) +
                #       (MT[(i+1) mod n] & lowermask)
                x <- bitAnd(X$MT[i],X$upper_mask) +
                        bitAnd(X$MT[((i) %% X$n1)+1], X$lower_mask)
                # xA <- x >> 1
                xA <- bitShiftR(x,1)

                # if x is odd, xor xA,a
                if (x %% 2 != 0){
                        xA <- bitXor(xA,X$a1)
                }
                # MT[i] <- MT[(i + m) mod n] xor xA
                X$MT[i] <- bitXor(X$MT[((i+X$m1) %% X$n1)+1], xA)
        }
        X$index <- 1
        X
}