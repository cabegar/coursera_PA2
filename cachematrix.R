## Put comments here that give an overall description of what your
## functions do
# Funciones para almacenar la inversa de una matriz en cache
# solo matrices cuadradas

## Write a short comment describing this function
# Esta función crea un objeto especial "matriz" 

makeCacheMatrix <- function(x = matrix()) {

  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function
# Esta función calcula la inversa de la "matriz" devuelt por makeCacheMatrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}

# prueba de ejecucion
# my_matrix <- makeCacheMatrix(matrix(1:4, 2,2))
# my_matrix$get()
# cacheSolve(my_matrix)

# my_matrix2 <- makeCacheMatrix(matrix(c(1,2,3,6,4,5,8,8,7), 3,3))
# my_matrix2$get()
# cacheSolve(my_matrix2)
