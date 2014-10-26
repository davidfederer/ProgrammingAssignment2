## The functions below allow the inverse of a non-singular matrix to be cached and looked
## up, rather than recomputed which may be time consuming for big matrices


## The makeCacheMatrix function creates/caches the inverse of a non-singular matrix provides the 
## functionality to get and set either the original and the inverse matrices

makeCacheMatrix <- function(x = matrix()) {
  inv_mat <- NULL
  set <- function(y) {
    x <<- y #sets matrix
    inv_mat <<- matrix()
  }
  get <- function() x #gets matrix
  setinvmat <- function(inverse) inv_mat <<- inverse
  getinvmat <- function() inv_mat
  list(set = set, get = get,
       setinvmat = setinvmat,
       getinvmat = getinvmat)
  
}


## The cachesolve function calculates the inverse of a non-singular matrix created with the above function. 
## However, it first checks to see if the inverse matrix has already been calculated. 
## If so, it gets the inverse matrix from the cache and skips the computation. 
## Otherwise, it calculates the inverse matrix of the data and sets the inverse matrix in the cache via the setinvmat function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv_mat <- x$getinvmat()
  if(!is.nan(det(inv_mat))) {
    message("getting cached data")
    return(inv_mat)
  }
  mat <- x$get()
  inv_mat <- solve(mat, ...)
  x$setinvmat(inv_mat)
  inv_mat 
}