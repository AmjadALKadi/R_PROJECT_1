##This instruction temporarily saves the inverse of the matrix.
##This instruction contains several instructions that reverse and save the inverse temporarily.


makeCacheMatrix <- function(x = matrix()) {
                    nel <- NULL 
                    pro <- function(a){
                      x <<- a 
                      nel <<- NULL
                    }
                    do <- function() {x}
                    setinvrse <- function(inverse) {nel <<- inverse}
                    getinvrse <- function() {nel}
                    list(pro = pro , do = do , setinvrse =setinvrse , getinvrse=getinvrse)
                    
}


## This instruction calculates the reciprocal of a cached array, 
#inverting the cached array and showing the result of the inverse of the array.

cacheSolve <- function(x, ...) {
                    nel <- x$getinvrse()
                    if (!is.null(nel)) {
                      message("get data")
                      return(nel)
                    }
                    matx <- x$do()
                    nel <- solve(matx , ...)
                    x$setinvrse(nel)
                    nel
                    
}
