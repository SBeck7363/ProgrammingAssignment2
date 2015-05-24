## There will be two functions and together the functions will cache the inverse of a matrix
## These functions will create, store and recall a matrix along with its inverse in/from cache

## This function called makeCacheMatrix creates a special matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        im<-NULL #sets setting the matrix inverse to null
        set<-function(y){
                x<<-y
                im<--NULL #store matrix in the cache
        }
        get<-function() x #get the matrix
        setInverse<-function(solve) im<<-solve #set the inverse matrix
        getInverse<-function() im #get the inverse matrix
        list(set=set,get=get,
             setInverse=setInverse,
             getInverse=getInverse) #creates list of funcitons 

}
## This function computes the inverse of the special matrix created by the makeCacheMatrix function
## and calculates the inverse matrix of it
## it first checks to detrermin if there is a need to compute
## if there is no need to compute it recalles the data from the cache.  If not completed before
## it calculates the inverse matrix then it will store it in the cache

cacheSolve <- function(x, ...) {
        
        ## Return a matrix that is the inverse of 'x'
        im<-x$getInverse() #query the x matrix cache
        if(!is.null(im)){
                message("getting the cached data")
                retun(im) #return cache
        }
        data<-x$get() #get matrix used by makeCacheMaxtrix function
        im<-solve(data,...) #calculate inverse of matrix
        x$setInverse(im) #store teh inverse matrix in cache
}

