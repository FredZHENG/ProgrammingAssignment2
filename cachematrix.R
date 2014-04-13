## This function returns the inverse matrix of a nonsingular matrix specified
## by users.

## makeCacheMatrix returns a function list that would be used in cacheSolve

makeCacheMatrix <- function(x = matrix()) {
        
        ## initialize an object "m", which will be used to store inverse matrix
        m <-NULL
       
        ## assign the input into "x", and also initialize the "m".
        set <-function(y){
                x <- y
                m <- NULL
        }
        
        get <- function() x             ## retrives the matrix stored in "x"
        
        setInv <- function(inv) m<<-inv     ## store the input into "m"
        
        getInv <- function() m              ## return the value of "m"
        
        ## combine the four functions into a list variable as an output
        list(set = set, get = get,
                 setInv = setInv,
                 getInv = getInv)
        
}


## cacheSolve function is for retriving the cache if it exist, or create one
## and store the inverse matrix in it if cache does not exist.

cacheSolve <- function(x, ...) {
    
        m<- x$getInv()
        
        ## if "m" has a value in it, return this value (the inverse matrix)
        ## and exit the function, otherwise jump to the next command.
        if(!is.null(m)) {
                message("getting cached inverse matrix")
                return(m)
        }
        
        ## if "m" is null, calculate the inverse of original matrix using
        ## solve function.
        data <- x$get()
        m <- solve(data,...)
        
        ## store the result in "m", and return the value of "m", 
        ## which is the inverse of "x"
        x$setInv(m)
        m
}
