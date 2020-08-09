## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##This function asks for the user to input a Matrix, which then it "cache" it's inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() {
    x
  }
  SetInv <- function(inverse) {
    inv <<- inverse
  }
  GetInv <- function() {
    inv
  }
  list(set = set, get = get, SetInv = SetInv, GetInv = GetInv)
}


## Write a short comment describing this function
##This function "computes" the inverse of the given matrix from makeCacheMatrix
## If this function has previously been run and the original matrix hasn't been modified
##then, it'll retrieve the inverse from the cache, otherwise it'll compute it again

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$GetInv()
  if (!is.null(inv)) {
    message("getting cached matrix")
    return(inv)
  }
  matrix<- x$get()
  inv <- solve(matrix, ...)
  x$SetInv(inv)
  inv
}

a<- makeCacheMatrix(matrix(c(1,0,1,1),2,2))
cacheSolve(a)
