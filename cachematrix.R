#Set of functions used to cache and calculate inverse of a matrix

#Creates a special matrix that can cache its inverse

makeCacheMatrix <- function(ip = matrix()) {
    inverse <- NULL
    set <- function(temp) {
        ip <<- temp
        inverse <<- NULL
    }
    get <- function() return(ip);
    setinv <- function(inv) inverse<<-inv;
    getinv <- function() return(inverse);
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
    
}

#Finds inverse of the special matrix. If already calculated, loads from memory
cacheSolve <- function(ip, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse <- ip$getinv()
    if(!is.null(inverse)) {
        message("Getting cached data")
        return(inverse)
    }
    data <- ip$get()
    inverse <- solve(t(data,...))
    ip$setinv(inverse)
    inverse
}

a <- makeCacheMatrix(matrix(1:4, 2, 2))
a$get()
cacheSolve(a)

b <- makeCacheMatrix(matrix(4:8, 2, 2))
b$get()
cacheSolve(b)

