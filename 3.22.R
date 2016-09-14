setwd("C:/Users/u342907/Desktop/Pals")
source("3.21.R")

waitToRun <- function(){
        aaa <<- as.integer(Sys.time())
        Sys.sleep(sample(40:100,1))
        RandSeed(as.integer(Sys.time()))
        Sys.sleep(sample(40:100,1))
        Rand()
}

# just set a = waitToRun output and run the below code
# a <- waitToRun()
# while(a != c){
#         aaa <- aaa + 1
#         c <- extract_number(i)
# }

