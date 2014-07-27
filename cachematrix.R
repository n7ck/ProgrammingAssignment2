## Below are two functions that are used to create a special 
## object that stores a MATRIX and cache's its INVERSE.
## assumes that the matrix supplied is always invertible.

# Inverse Matrix Example:
# ma <- matrix(c(2,2,3,2),2,2)
# solve (ma)
#       [,1] [,2]
# [1,]   -1  1.5
# [2,]    1 -1.0

## makeCacheMatrix: This function creates a special 
## "matrix" object that can cache its inverse.
makeCacheMatrix <- function(trix = matrix()) {
    invr <- NULL
    set <- function(y) {
        trix <<- y
        invr <<- NULL
        message("New Matrix stored, now call cacheSolve(...) to calc inverse")
    }
    get <- function(){ trix }
    #setinverse() should not be called directly!
    #only cacheSolve should call setinverse()
    setinverse <- function(inverse){ invr <<- inverse } 
    getinverse <- function() {
        if(!is.null(invr)) {  
            #message("Here is your Inverse:")
            return(invr)
        }
        #message("call cacheSolve(...) 1st")
        #message("since I am too damn lazy to call it myself...")
        NULL
    }
    message("New Matrix stored, now call cacheSolve(...) to calc inverse")
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## cacheSolve: This function computes the inverse of the special
## "matrix" returned by makeCacheMatrix above. If the inverse
## has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("inverse was already in cache!")
        return(m)
    }
    message("inverse is being calculated and cached!")
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    #message("now you can call getinverse()!")
    m
}




## Teacher example function: makeVector:
makeVector <- function(x = numeric()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setmean <- function(mean){ m <<- mean } 
    getmean <- function(){ m }
    list(set = set, get = get,
         setmean = setmean,
         getmean = getmean)
}
## Teacher example function: cachemean:
cachemean <- function(x, ...) {
    m <- x$getmean()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- mean(data, ...)
    x$setmean(m)
    m
}