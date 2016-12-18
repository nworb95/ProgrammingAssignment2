## makeCacheMatrix creates a special matrix object, and initializes 'i' to be updated later

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL ## Initializes object i
        set <- function(y) { ## Lets you change matrix later if you use $ subset
                x <<- y ## Assigns your new value to the matrix
                i <<- NULL ## Re-initializes object i 
        }
        get <- function() x ## Lets you retrieve matrix
        setinverse <- function(inverse) i <<- inverse # Sets the inverse of the matrix
        getinverse <- function() i ## Retrieves inverse of the matrix (?)
        list(set=set, get=get, 
             setinverse=setinverse, 
             getinverse=getinverse) ## Names objects to allow use of $ operator
}


## cacheSolve then solves the matrix (i.e. gives the inverse of the matrix), but then stores
## it so that if cacheSolve is called again without changing the matrix, it is returned from the 
## cache to save time as opposed to calculating it again
cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)){
                message("perserverence is the answer")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
