## These functions work together to assign a matrix, compute its inverse,
## and cache the value of the inverse, so it will not be computed again,
## unless the contents of the matrix change (through the set() command).
## 

## Example 1 of intended operation:
##  m <- matrix(c(12, 7, 3, 99, 44, 2, 2, 1, -2), 3, 3) # create a matrix
##  x <- makeCacheMatrix(m) # cache the matrix.
##  cacheSolve(x) #compute and cache the inversethe inverse
##  x$get() # return the matrix
##  x$getinv() # return the inverse of the matrix
##  cacheSolve(x) #returns the cached inverse
##  x$set(matrix(c(2:5), 2,2))  #reset the cached matrix to a different value
##  cacheSolve(x)  # recompute inverse and return new value

## Create a suite of functions to create (set) and get a matrix, as well as
## get and set the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }      
        get <- function() x
        setinv <- function(inv) m <<- inv
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)        
}


## compute the matrix inverse only if the inverse has not yet been computed
##  (i.e. the cached value is NULL). If the cached value of the inverse IS NULL,
##  then the matrix has been set() and the inverse needs to be recomputed.

cacheSolve <- function(x, ...) {
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
        ## Return a matrix that is the inverse of 'x'
}
