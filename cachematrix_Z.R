## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix <- make a list of 4 functions whit their environments that will  
## cache the inverse of a matrix

## example: 
## m <- matrix(c(2, 10, 5, 0), nrow = 2, ncol = 2, byrow = TRUE)
## cacheSolve(makeCacheMatrix(m))

makeCacheMatrix <- function(x = matrix()) {
    in.m <- NULL
    
    get <- function() x
    set <- function(y) {
        x <<- y
        in.m <<- NULL
    }
    
    get.in.m <- function() in.m
    set.in.m <- function(inverse) in.m <<- inverse
   
    list(set = set, get = get, 
         set.in.m = set.in.m,
         get.in.m = get.in.m)
}

## Write a short comment describing this function
## cand the environment of makeCacheMatrix

cacheSolve <- function(x, ...) {  ## Return a matrix that is the inverse of 'x'
    in.m <- x$get.in.m()
    if(!is.null(in.m)) {
        message("getting cached data")
        return(in.m)
    }
    
    data <- x$get()
    in.m <- solve(data, ...)
    x$set.in.m(in.m)
    return(in.m)
        
}


m <- matrix(c(2, 10, 5, 0), nrow = 2, ncol = 2, byrow = TRUE)
cacheSolve(makeCacheMatrix(m))


