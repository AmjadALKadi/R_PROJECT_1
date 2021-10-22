##This instruction temporarily saves the inverse of the matrix.
##This instruction contains several instructions that reverse and save the inverse temporarily.


makeCacheMatrix <- function(x = matrix()) {
                           nel <- NULL                                    ## nel = null make var  = null
                           pro <- function(a){                            ## that function to store tempo var in x 
                                          x <<- a 
                                          nel <<- NULL                                 ## make the var nel = null again
                                            }
                           do <- function() {x}                           ## function to make x as input
                           setinvrse <- function(inverse) {nel <<- inverse}
                           getinvrse <- function() {nel}
                           list(pro = pro , do = do , setinvrse =setinvrse , getinvrse=getinvrse)             ## make tempo list to store var x 
                    
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
