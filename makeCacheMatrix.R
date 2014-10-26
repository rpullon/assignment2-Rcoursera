makeCacheMatrix <- function(matrix = numeric(),...) {
  
  matrixinverse <- NULL ##set inverse to NULL
  
  ## $set sets a new value for the matrix
  set <- function(y){ 
    matrix <<- y
    matrixinverse <<- NULL
  }
  ## $get prints the matrix to the dashboard
  get <- function() {
    matrix
  }
  ## set the inverse of the matrix
  setinverse <- function(inverse) {
    matrixinverse <<- inverse
  }
  ## print the inverse to the dashboard
  getinverse <- function() {
    matrixinverse
  }
  list(set = set, get = get, 
       setinverse = setinverse, getinverse = getinverse)
}

cacheSolve <- function(x,...){
  ## get current inverse
  matrix <- x$get()
  matrixinverse <- x$getinverse()
  
  ## if it is not null, get from memory
  if(!is.null(matrixinverse)){
    ## Make sure same size
    dimMatrix <- dim(matrix)
    dimInverse <- dim(matrixInverse)
    same <- identical(dimMatrix,dimInverse)
    
    ## If same size, assume same matrix, get cached data
    if(same) {
      message("getting cached data")
      return(matrixinverse)
    }
  }
  
  ## otherwise work out matrix inverse
  matrixinverse <- solve(matrix)
  
  x$setinverse(matrixinverse)
  matrixinverse ## print out to consol
}
  