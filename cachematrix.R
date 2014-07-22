## Author: Angel Miranda
## Date: 2014-07-22

## This program returns the inverse of a matrix, taking into account if a 
## previous calculation of the same matrix has been made and returning the 
## cached inverse if this is the case. If there is no previously cached data, 
## computes the inverse and returns that matrix.


## makeCacheMatrix creates a vector of functions to be called by cacheSolve, to 
## get and put the values of the original matrix, and get and put the inverted
## matrix.


makeCacheMatrix <- function(x = matrix()) {

        inverse <- NULL                 ## inverse is the cached value
        set <- function(y) {            ## initializes x and inverse
                x <<- y
                inverse <<- NULL
        }
        get <- function() x             ## returns x
        setinverse <- function(inv) inverse <<- inv     ## sets cached inverse
        getinverse <- function() inverse                ## gets cached inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## cacheSolve returns the inverse of a matrix defined in makeCacheMatrixm
## using a cached value if it exists previously. If not, calculates via solve()
## and writes back the cached value for future references.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getinverse()       ## gets cached value
        if(!is.null(inverse)) {         ## if it has been already cached
                message("getting cached data")  ## say is using cached data
                return(inverse)                 ## and return it
        }
        data <- x$get()                 ## if it is not cached, get matrix
        inverse <- solve(data, ...)     ## compute inverse
        x$setinverse(inverse)           ## write back inverse to cache
        inverse                         ## and return it
        
}
