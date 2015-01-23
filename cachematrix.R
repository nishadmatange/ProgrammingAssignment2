## These functions return the inverse of a matrix either from a cache,
## if previously calculated or freshly calculated

## Function for creating inverse matrix and cache

makeCacheMatrix <- function(x = matrix()) {
  inv <- matrix()
  get_data <- function() {x}
  set_inverse <- function(inverse) {inv <<- inverse}
  get_inverse <- function() {inv}
  list(get_data = get_data, set_inverse = set_inverse, get_inverse = get_inverse)
  
}
## Function for retrieving value of inverse from cache/
## calculating value of inverse and caching it

cacheSolve <- function(x, ...) {
  inv <- x$get_inverse()
  if (!is.na(inv[1,1])) {
    message("inverse from cache ")
    return(inv)
  } else {
    data <- x$get_data()
    inv <- solve(data)
    message("calculated inverse ")    
    x$set_inverse(inv)
    return(inv)
  }
}
