## How to Use the functions: makeCacheMatrix() and cacheSolve()
## > source("cachematrix.R")    load R program and initialize the two functions
## > a <- makeCacheMatrix()     create functions in the variable a
## > a$set(matrix(1:4, 2, 2))   define a matrix 2x2 by columns
## > cacheSolve(a)              calculate the inverse matrix and save it to cache

## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
##
## > cacheSolve(a)              run again to retrieve inverse matrix from cache
##  
## getting cached data          
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
##
##
## makeCacheMatrix returns a list of four functions
## which are used by cacheSolve
##
makeCacheMatrix <- function(x = matrix()) {
        
        cache <- NULL
        
        ## initialize a new matrix and the cache (inverse matrix)
        set <- function(y) {
                x <<- y         # initialize the matrix
                cache <<- NULL  # clean the cache for a new inverse matrix
        }
        
        ## retrieve the matrix
        get <- function() x

        # store inverse matrix in cache
        setInverse <- function(inverse) cache <<- inverse
        
        # get the inverted matrix from cache
        getInverse <- function() cache
        
        # return a list of the created functions
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## cacheSolve calcluates the inverse of the matrix created in makeCacheMatrix
## If the inverted matrix does not exist in cache,
## it it created in the working environment and then
## it is stored in cache
cacheSolve <- function(x, ...) {
        ## attempt to get the inverse of the matrix stored in cache
        cache <- x$getInverse()
        
        # if the cache is not null then return the inverse matrix
        if (!is.null(cache)) {
                message("getting cached data")
                return(cache)           # return the inverse matrix and exit
        }
        
        # Otherwise the cache is null so we need to calculate the inverse matrix
        matrix <- x$get()               # get the matrix
        mcache <- solve(matrix, ...)    # calculate inverse 
        x$setInverse(mcache)            # save the inverse matix in the cache

        return (mcache)                 # return the inverse matrix and exit
}