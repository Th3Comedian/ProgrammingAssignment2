## My function will do pretty much the same thing the example function for the 
## mean would do and has a similar structure.
## set initializes the matrix, get returns the value of the matrix, set will  
##assign the matrix new values and get will return the values of the inverse 
##matrix.

makeCacheMatrix <- function(x = matrix()) {
        invm <- NULL
        set <- function(y){
                x <<- y
                invm <<- NULL
        }
        get <- function(){
                x
        }
        SetImat <- function(invmat){
                invm <<- invmat
        }
        GetImat <- function(){
                invm
        }
        list(set = set, get = get, SetImat=SetImat, GetImat=GetImat)
}


## This function just as the example will first check whether the 
##inverse matrix has already been filled and will return those values,
#if its empty, it will compute the inverse matrix and store it using the 
##SetImat function.

cacheSolve <- function(x, ...) {
        invm <- x$GetImat()
        if(!is.null(invm)){
                message('getting cached data')
                return(invm)
        }
        else{
        data <- x$get()
        invm <- solve(data, ...)
        x$SetImat(invm)
        invm
        }
}
