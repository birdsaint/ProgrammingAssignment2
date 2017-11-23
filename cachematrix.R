## Function "makeChcheMatrix" creates a Matrix to chche the data

## 1 set the value of the matrix
## 2 get the value of the matrix
## 3 set the value of inverse
## 4 get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    set_inverse <- function(input) inverse <<- input
    get_inverse <- function() inverse
    list(set = set, get = get,
         set_inverse = set_inverse,
         get_inverse = get_inverse)
}


## Function "cacheSolve" calculate and output the inverse of a matrix.
## 1 check if the inverse has been calculated
## 2 if so, output the result; otherwise calculate the inverse.
## 3 cache the inverse and output the result.

cacheSolve <- function(x, ...) {
    inverse <- x$get_inverse()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data, ...)
    x$set_inverse(inverse)
    inverse    
    ## Return a matrix that is the inverse of 'x'
}
