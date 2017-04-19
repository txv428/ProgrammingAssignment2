## Here in the makeCacheMatrix, a special "matrix" object is created which cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  p<-NULL
        
  set<-function(y){
    x<<-y
    p<<-NULL
  }

  get <- function() {
    x
  }

  setInverse <- function(inverse) {
    i <<- inverse
  }
  
  getInverse <- function() {
    p
  }

  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## In cacheSolve function, inverse to the "matrix" object gets computed and returned by the makeCacheMatrix function
cacheSolve <- function(x, ...) {
        p <- x$getInverse()
        if( !is.null(p) ) {
          message("getting cached data")
          return(p)
        }

        data <- x$get()
        p <- solve(data) %*% data
        x$setInverse(p)
p 
}
