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

To verify the function and results
## x = rbind(c(1, -1/4), c(-1/4, 1))
## m = makeCacheMatrix(x)
## m$get()
##            [,1]        [,2]
##[1,]  1.0000000 -0.02325581
##[2,] -0.3333333  1.00000000

## No cache in the first run
## > cacheSolve(m)
##           [,1]      [,2]
## [1,] 1.0666667 0.2666667
## [2,] 0.2666667 1.0666667

