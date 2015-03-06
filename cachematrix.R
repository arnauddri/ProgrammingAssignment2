# Matrix inversion is usually a costly operation. To increase time performance,
# it is usually a good thing to stores the computed inverse in cache so we don't
# need to run the computation a second time in the future. Here we write a function
# which stores the matrix in cache and another which computes its inverse

# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL

  set <- function() {
    x <<- y
    m <<- NULL
  }

  get<- function() x
  setInverse <- function(Inverse) m <<- Inverse
  getInverse <- function() m

  list(
    set = set,
    get = get,
    setInverse = setInverse,
    getInverse = getInverse
  )
}


# cacheSolve returns the inverse of a matrix.
# It does so by first checking if the inverse of this matrix
# has been cached before. If yes, it returns the cache value.
# If not, it computes it using 'solve', stores the result in
# the cache and returns it

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if (!is.null(m)) {
    message("getting cached data")
    return(m)
  }

  data <- x$get()
  m <- solve(data)
  x$setInverse(m)
  m
}

## Sample test

# A = matrix(
#  c(2,4,3,1,5,7,1,2,3),
#  nrow = 3,
#  ncol = 3,
#  byrow = TRUE
# )

# B <- cacheSolve(makeCacheMatrix(A))

# print(A)
# print(B)
# print(A %*% B)
