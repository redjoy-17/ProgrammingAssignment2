## Put comments here that give an overall description of what your		 

## This function constructs the matrix and defines its inverse
makeCacheMatrix<- function(x = matrix())
{
  Inv<- NULL
  set<- function(y)
  {
    x <<- y
    Inv <<- NULL
  }
  get<- function() x
  setInverse<- function(inverse) Inv <<- inverse
  getInverse <- function() Inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}

## This function checks if the inverse is already computed and if not gets it
cacheSolve<- function(x, ...) {
  Inv<- x$getInverse()
  if(!is.null(Inv)) {
    message("getting cached data")
    return(Inv)
  }
  mat<- x$get()
  Inv<- solve(mat, ...)
  x$setInverse(Inv)
  Inv
}
