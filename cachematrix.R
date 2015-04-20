## R Programming Coursera course 
## Programming Assignment 2
## Submitted by Antti Manninen in April 2015

## There are two functions in this R file to enable caching the result
## of matrix inverse calculations.


## Function 'makeCacheMatrix' creates a special matrix like object that 
## supports storing the result of matrix inverse in a cache and retrieving 
## the result from the cache. The function returns a list with items
## set and get for setting and retrieving the matrix for which inverse 
## is being calculated, as well as setinverse and getinverse for setting
## and retrieving the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
        ## This object is used to cache the matrix inverse of x
        inv <- NULL
        
        ## Set and get functions for initialising and retrieving the matrix
        set <- function(y) {
                x <<- y
                inv <<- NULL ## Initialising also the inv variable
        }
        get <- function() x
        
        ## Set and get functions for setting and retrieving the matrix inverse
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        
        ## The function returns a list of 4 functions - sets and gets for
        ## the matrix x and its inverse
        list(set = set, get = get, 
             setinverse = setinverse, getinverse = getinverse)
}


## Function 'cacheSolve' calculates the matrix inverse, returning the 
## cached result if it is available or calling the solve function to 
## calculate it otherwise.

cacheSolve <- function(x, ...) {
        
        ## Test first if cache exists by retrieving it from the 
        ## makeCacheMatrix object retrieved as parameter
        inv <- x$getinverse()
        
        ## Cached matrix inverse exists if getinverse returned
        ## something else than null - return it in that case
        if (!is.null(inv)) { 
                message("getting cached matrix inverse")
                return(inv)
        }
        
        ## If we reach this part of code, it means that no cache
        ## was found, so we need to calculate the matrix inverse
        
        ## Need to retrieve the matrix itself from the special
        ## makeCacheMatrix object retrieved as parameter
        data <- x$get()
        
        ## Solve function solves the inverse of a given matrix if no
        ## parameter 'b' is given.
        inv <- solve(data, ...) 
        x$setinverse(inv) ## Store the calculated inverse in the cache
        
        ## Finally, return the calculated matrix inverse
        inv
}
