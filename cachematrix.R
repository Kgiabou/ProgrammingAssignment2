## The first function, `makeCacheMatrix` creates a matrix, which is
### a list containing 4 different functions :

#1.  set the values of the matrix
#2.  get the value of the matrix
#3.  set the value of the inverse matrix 
#4.  get the value of the inverse matrix

### The function also caches the inverse matrix (inverted square matrix) in the 
##  variable m_inv.

makeCacheMatrix <- function(x = matrix()) {
        m_inv <- NULL
        set <- function(y) {
          x <<- y
        m_inv <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m_inv <<- inv
  getinv <- function() m_inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)

}

## cacheSolve is a function which computes the inverse of the matrix 
## returned by the function makeCacheMatrix above. 
## If the inverse matrix has already been calculated then the cachesolve 
## retrieves the inverse matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m_inv <- x$getinv()
  if(!is.null(m_inv)) {
    message("getting cached matrix")
    return(m_inv)
  }
  data <- x$get()
  m_inv <- solve(data, ...)
  x$setinv(m_inv)
  m_inv
}

#example: 
mat <- matrix(rnorm(9, 2.2, 1:3), nrow=3)
mat1 <- makeCacheMatrix(mat)
cacheSolve(mat1)
