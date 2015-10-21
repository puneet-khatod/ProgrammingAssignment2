## Functions in this file are written to demonstate basic implementation of object caching in R
## Here we are creating a special matrix which can cache its inverse and use the result from cache.

## This function accepts a matrix as argument and creates cacheable matrix through it.

makeCacheMatrix <- function(x = matrix()) {
        inverse.x <- NULL
        
        #Return the matrix
        get <- function() x
        
        #Set new matrix and mark inverse as null
        set <- function(m){
                x <<- m
                inverse.x <<- NULL
        }
        
        # return inverted matrix value from cache
        getInverse <- function() inverse.x
        
        # set inverted matrix value into the cache
        setInverse <- function(inverted){
                inverse.x <<- inverted
        }
        
        # return list of functions available
        list(get = get, set = set, 
             getInverse = getInverse,
             setInverse = setInverse
             )
}


## This function accepts a cacheable matrix as argument and calculates its inverse.
## If inverse is already present in cache, same will be returned
## Else the inverse is calculated and stored in cache for further use.

cacheSolve <- function(x, ...) {
        solved <- x$getInverse()
        
        if(!is.null(solved)){
                message("Getting inverted matrix from cache!")
        } else {
                solved <- solve(x$get())
                message("Putting inverted matrix in cache!")
                #print(solved)
                x$setInverse(solved)
        }
        return(solved)
}

## Sample usage
#m <- matrix(1:4,2,2)
#v <- makeCacheMatrix(m)
#print(v$get())
#print(cacheSolve(v))
#print(cacheSolve(v))
#print(cacheSolve(v))

