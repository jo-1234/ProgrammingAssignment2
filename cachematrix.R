## makeCacheMatrix receives data in a matrix then stores the data
## and data handling functions in a list.  It then returns the list.
## 
## Data handling functions are:
## 'set', sets the matrix data
## 'get', gets/returns the matrix data
## 'setInv', sets the inverse cache
## 'getInv', returns the inverse cache
## 

makeCacheMatrix <- function(data = matrix()) { 
        matrix_inverse <- NULL

	## set the matrix data
        set <- function(data_in) { 
                data <<- data_in
                matrix_inverse <<- NULL
        }

	## get/return the matrix data
        get <- function() {
		data
	}

	## set the inverse cache
        setInv <- function(matrix_data) {  
		matrix_inverse <<- matrix_data
	}

	## return the inverse cache
        getInv <- function() {  
		matrix_inverse
	}

	## store functions in a list and return the list
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
}

## cacheSolve receives a list created by makeCacheMatrix then
## returns the inverse of the data in 'matrix_list'
##
## It checks for a cached inverse result.  If found, it returns
## the cache.  If not found, it sets the cached inverse value.
##
## Data handling functions in matrix_list are:
## 'set', sets the matrix data
## 'get', gets/returns the matrix data
## 'setInv', sets the inverse cache
## 'getInv', returns the inverse cache
##

cacheSolve <- function(matrix_list, ...) {

	matrix_inverse <- matrix_list$getInv()

	## check if there is a cached result. If so, returns it
	if(!is.null(matrix_inverse)) {
		message("getting cached data")
		return(matrix_inverse)
	}

	## retrieve data
	inv_data <- matrix_list$get()

	## calculate the inverse of the matrix data
	matrix_inverse <- solve(inv_data, ...)

	## store inverse result in matrix
	matrix_list$setInv(matrix_inverse)

	## return inverse result
	matrix_inverse
}
