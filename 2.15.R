
unpadVal <- function(inp){

        if(!is.raw(inp)){
                stop("Input not of class(raw)")
                geterrmessage()
        }

        len <- as.integer(tail(inp,1))

        if (len>32){
                stop("Last byte exceeds max pad")
                geterrmessage()
        }
        if (len==0){
                stop("Last byte = 0")
                geterrmessage()
        }
        if (len > length(inp)){
                stop("Invalid Pad")
                geterrmessage()
        }
        if (sum(tail(inp,len)==as.raw(len))!=len){
                stop("Invalid Pad")
                geterrmessage()
        }
        unpadRaw(inp)
}