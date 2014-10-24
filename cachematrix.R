#We define two different funtions: makeCacheMatrix and cacheSolve.  
#We want to create a special "matrix" object that can store its inverse
#in cache. This way, when we call cacheSolve of a particular matrix,
#the funtion will first check if there is a cache stored valued for its
#inverse. If there is, it will return this inverse matrix without having
#to compute it again. If there is not, it will calculate it and store it 
#in cache.

#makeCacheMatrix creates a special "matrix" object that can cache its inverse

makeCacheMatrix<-function(x = matrix()){
        
    inv<-NULL # we initialize inv to NULL (if)
    
    set<-function(y) {
        x <<- y         # we can use "set" to assign a new matrix
        inv <<- NULL    #every time we call makeCacheMatrix with a new matrix, inv is assigned NULL again (so the cacheSolve will be able to tell whether the matrix has changed or not)
    }
    get <- function() {
        x               # "get" will return the matrix
    }
    setinverse <- function(inverse) {
        inv <<- inverse # we give inv the value of the inverse, so we will have it in cache
    }
    getinverse <- function() {
        inv             # getinverse will return the inverse of the matrix 
    }
    list(set = set, get = get,   # this is the list of our special "matrix"
         setinverse = setinverse,
         getinverse = getinverse)
}
    
#cacheSolve computes the inverse of the special "matrix" returned by 
#makeCacheMatrix above. If the inverse has already been calculated 
#(and the matrix has not changed), then the cachesolve should retrieve 
#the inverse from the cache.
    
cacheSolve<- function(x,...){   
    inv <- x$getinverse()          # we search the value of inv
    if(!is.null(inv)) {            # if inv is not Null, it means we already have a calculated value of inv and the matrix has not changed
        message("getting cached data")
        return(inv)                # so we can return the cache value of the inv     
    }                              # otherwise, we need to calculate the matrix's inverse:
    data <- x$get()                # we take the value of the matrix
    inv <- solve(data, ...)        # we calculate its inverse
    x$setinverse(inv)              # we define inv in the cache
    inv                            # we return the inverse
}