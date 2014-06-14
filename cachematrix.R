## 1.   set the velue of matrix
## 2.   get the value of matrix
## 3.   set the value of inverse matrix
## 4.   get the value of inverse matrix

##  this function creates a special "matrix" object that can cache its inverse


makeCacheMatrix <- function(x = matrix()) {
        
        if(!is.matrix(x)) stop("argument must be a matrix")
        if(nrow(x) != ncol(x)) stop("must be a square matrix")
        m <- NULL
        set <- function (y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        
        setinversematrix <- function(inversematrix) m <<- inversematrix
        getinversematrix <- function() m
        list(set = set, get = get,
             setinversematrix = setinversematrix,
             getinversematrix = getinversematrix)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
        m <-x$getinversematrix()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)             
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinversematrix(m)
        m
}
