
## makeCacheMatrix returns a list of functions to do the following:
##  1. Set the value of the matrix
##  2. Get the value of the matrix
##  3. Set the value of the inverse
##  4. Get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


#cacheSolve checks to see if the inverse of a given matrix has been cached
#if it has not, it calculates it and caches it. 

cacheSolve <- function(x, ...) {
    
    #Returns inverted matrix if it has already been cached 
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    
    #If inverse hasn't been cached, invert it now
    data <- x$get()
    
    #qr.solve() is slightly more efficient than solve()
    i <- qr.solve(data, ...)
    
    #Cache the inverse
    x$setinverse(i)
    
    #Return the inverse
    i
}
