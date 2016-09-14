setwd("C:/Users/u342907/Desktop/Pals")
source("3.21.R")

distemper <- function(inp){
        w <- 32
        n <- 624
        m <- 397
        r <- 31
        a <- h2int("9908b0df")
        u <- 29
        d <- h2int("ffffffff")
        s <- 17
        b <- h2int("9d2c5680")
        t <- 37
        c <- h2int("efc60000")
        l <- 11
        f <- 1812433253
        inp <- unshiftR(inp,l)
        inp <- unshiftL(inp,t,c)
        inp <- unshiftL(inp,s,b)
        inp <- unshiftR(inp,u)
        inp
}

# First things First: UnRand Requires a 624-long integer vector
# created by 'RandFeeder', as well 'RandObj', the pRNG that
# we're trying to clone.

# once you've initialized 'unRand' with arguments, you
# should be able to use it without arguments to predict
# future values of the 'Rand' function, which outputs
# integers from the 'RandObj'

unRand <- function(hackMT = c(), rando = c()){

        # test for seed, re-seed if argument exists
        if(length(hackMT)>0){

        }

        # pull the correct value from the object
        hackRandObj <<- extract_number(hackRandObj)

        # return that value
        hackRandObj$y
}

RandFeeder <- function(){
        seed_mt(RandObj,sample(1:2^30,1))
        a <- c()
        for(i in 1:624){
                a[i] <- Rand()
                a[i] <- distemper(a[i])
        }
        a
}

hackRand <- function(mt,rando){
        # initialize a new random environment
        hackRandObj <<- initRando()

        # feed it the correct index and
        hackRandObj$MT <<- mt
        hackRandObj$index <<- rando$index

        # pull the next value out
        hackRandObj <<- extract_number(hackRandObj)

        # return that value
        hackRandObj$y
}

hackRando <- function(){

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

unshiftR <- function(value,shift){
        i <- 0
        result <- 0
        shift <- shift %% 32
        while(i*shift < 32){
                # partmask is a set of ones for the number of
                # digits we can expect to be solved for
                partmask <- bitShiftR(bitShiftL(-1,
                                                32-shift
                                                ),
                                      shift*i
                                      )
                part <- bitAnd(value,
                               partmask)
                value <- bitXor(value,
                                bitShiftR(part,
                                          shift)
                                )
                result <- bitOr(part,
                                result)
                i <- i+1
        }
        result
}

unshiftL <- function(value,shift,mask){
        i <- 0
        result <- 0
        shift <- shift  %% 32
        while(i*shift < 32){
                partmask <- bitShiftL(bitShiftR(-1,
                                                32-shift
                                                ),
                                      shift*i)
                part <- bitAnd(value,
                               partmask)
                value <- bitXor(value,
                            bitAnd(mask,
                                   bitShiftL(part,
                                             shift)
                                   )
                            )
                result <- bitOr(part,
                                result)
                i <- i+1
        }
        result
}
