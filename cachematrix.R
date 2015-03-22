### Below are two functions that are used to create a special object that stores a numeric matrix and 
### caches its inverse.


### The 'makeCacheMatrix' function creates a special "matrix" that contains the original matrix value 
### and its inverse, accessible by these methods: 
### 1. set the value of the matrix
### 2. get the value of the matrix
### 3. set the value of the inverse
### 4. get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        
        setInverse <- function(i) inv <<- i
        getInverse <- function() inv
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


### The 'cacheSolve' function calculates the inverse of the special "matrix" created with the above function. 
### It first checks to see if the inverse has already been calculated. If so, it gets the inverse from 
### the cache and skips the computation. Otherwise, it calculates the inverse of the data and sets the 
### value of the inverse in the cache via the setInverse function.
cacheSolve <- function(x, ...) {
        invMatr <- x$getInverse()
        if(!is.null(invMatr)) {
                return(invMatr)             ### getting cached data
        }
        
        data <- x$get()                     ### calculating the inverse
        invMatr <- solve(data)
        x$setInverse(invMatr)               ### caching the inverse
        invMatr                             ### geting the inverse
}
