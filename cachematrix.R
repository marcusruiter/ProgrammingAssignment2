## functions do

## ******************* makeCacheMatrix() ******************************
## The function makeCacheMatrix caches the original Matrix passed as argument in the global environment using <<-
## The methods of the function set, get allow for the setting and getting of the original matrix
## The methods of the function setinv, getinv allow for the setting and getting of the inverse of the original matrix
## The return object is a list of the methods set, get, setinv, getinv

## Write a short comment describing this function
makeCacheMatrix <- function(cacheMatrix = matrix()) {
        
	cacheMatrixinv <- NULL

    	if (!is.matrix(cacheMatrix)) {
        	message("makeChachematrix(): error parameter must be a matrix")
        	return
    	}

    	set <- function(orgMatrix) {
        	cacheMatrix <<- orgMatrix
        	cacheMatrixinv <<- NULL
    	}
    
	get <- function() {
        	cacheMatrix
    	}

    	setinv <- function(invMatrix) {
        	cacheMatrixinv <<- invMatrix
	}

	getinv <- function() {
        	cacheMatrixinv
    	}

    	list (      set = set
        	  , get = get
          	  , setinv = setinv
          	  , getinv = getinv )

}


## ********************************* cacheSolve() *************************************
## The function cacheSolve retrieves the inverse of the original matrix in a list object passed as an argument to cacheSolve()
## The function makeCacheMatrix() must first be used to create the list object x that is to be passed to cacheSolve()
## If cacheSolve() is called for the first time the inverse will be calculated
## If cacheSolve() is called subsequently the inverse will be retrieved from the cache in the Global environment

cacheSolve <- function(x, ...) {
            
    invmat <- x$getinv()
    
    if (!is.null(invmat)) {
        message("Getting cached inverse")
        return (invmat)
    }
    
    invmat <- solve(x$get())
    x$setinv(invmat)
    invmat
    
}
