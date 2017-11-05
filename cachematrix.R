## makeCacheMatrix - pass in the matrix details to be inversed
## eg mymatrix2 <- makeCacheMatrix(matrix(c(1,2,3,4),2,2))
## cacheSolve - uses the output from makeCacheMatrix to inverse matrix
## eg solvedmat2 <- cacheSolve(mymatrix2)
##    solvedmat2 (will then display inversed matrix) 

## function makeCacheMatrix - pass in matrix as input

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)}


## cacheSolve function - pass in output from makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
  		if(!is.null(m)) {
  			message("getting cached data")
  			return(m)
  		}
  		data <- x$get()
  		m <- solve(data, ...)
  		x$setinverse(m)
  }
