## Put comments here that give an overall description of what your
## functions do

## Creating 2 functions 1.) Make a list with methods that set and get a matrix and its inverse in an intrinsic environment variable
## 2.) Pass the list from 1.) function and attempts to calculate and set its inverse.  If the inverse is already set, the cached value is used

## Write a short comment describing this function
## The following function (makeCacheMatrix) creates a matrix x, and exposes three methods to set x, get x and inverse x

makeCacheMatrix <- function(x = matrix()) {
        cachedInv <- NULL ## initialize Cached inverse
        
        ## set x in parent environment. If inverse is already set, then clear the inverse.
        set <- function(userValue = matrix()) {
                x <<- userValue 
                cachedInv <<- NULL
        }
        
        get <- function() x
        
        ## set inverse variable in parent environment to desired value and return the value
        setInverse <- function(invVal) {
                cachedInv <<- invVal 
                return(cachedInv)
        }
        
        getInverse  <- function() cachedInv
        list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## Write a short comment describing this function
## Check if there's already a cached inverse and return otherwise will attempt 
##  to solve its inverse and set/return it
cacheSolve <- function(x=makeCacheMatrix(1:4, nrow=2, ncol=2), ...) { ##special matrix provided or create a test 2x2 matrix
        
        ## Check if the Inverse already exists
        calculatedInverse <- x$getInverse() 
        
        ## Check if there is a cached value AND it is a matrix
        if(!is.null(calculatedInverse) && is.matrix(calculatedInverse)) { 
                message("We found cached data and saved valuable cpus!!!")
                return(calculatedInverse)
        }
        
        ## otherwise get the matrix
        matrixToSolve <- x$get()  
        
        ## try to solve the matrix and catch errors and warnings
        calculatedInverse <- tryCatch({ 
                solve(matrixToSolve)
        }, warning=function(w) {
                message("This may not be the result you're looking for")
                message(w)
        }, error=function(e) {
                message("Something went wrong solving your matrix")
                message(e)
                message("\n")
        })
        
        ## Finally, set the value of the inverse, if there is an error then set it to NULL
        message("Setting the value of inverse to:") 
        x$setInverse(calculatedInverse)
}

## End of the Program.