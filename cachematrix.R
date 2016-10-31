## As requested by the instructions on the assignment, the following cache the inverse of a matrix by creating two functions:
## 1-The first function creates a special matrix
## 2- The second function calculates the inverse. If an inverse was not calculated in before, then it will grab the data and compute the same,
### if not, the functin will return the inverse of the matrix that was previously calculated

## This function creates a special matrix to cache the inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## This function calculates the inverse of the matrix

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
        ## Return a matrix that is the inverse of 'x'
}
