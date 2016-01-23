## makeCacheMatrix - creates an object containing functions for get/set matrix,
##                    get/set inversed matrix and matrix-object for inversed matrix cache
##                    
## cacheSolve - computes and returns inversed matrix for the assigned one (if the cache value
##              exists - computation is skipped)

## makeCacheMatrix
## input param: square invertible matrix
## returns: list-object for matrix (and it's inverted edition) management

makeCacheMatrix <- function(x = matrix()) {
  inversed_matrix <- NULL
  
  set <- function(y) {
    x <<- y
    inversed_matrix <<- NULL
  }
  get <- function() x
  
  set_inversed_matrix <- function(solved_m) inversed_matrix <<- solved_m
  
  get_inversed_matrix <- function() inversed_matrix
  
  list(set = set, get = get,
       set_inversed_matrix = set_inversed_matrix,
       get_inversed_matrix = get_inversed_matrix)
  
}


## cacheSolve
## input param: list-object created with makeCacheMatrix function
## returns: returns inversed matrix for the assigned one

cacheSolve <- function(x, ...) {
  
  im <- x$get_inversed_matrix()
  
  ## check if inverted matrix is already computed
  if(!is.null(im)) {
    message("getting inverted matrix from cache")
    return(im)
  }
  temp_matrix <- x$get()
  im <- solve(temp_matrix)
  x$set_inversed_matrix(im)
  im
}
