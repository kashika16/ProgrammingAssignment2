## The first function makes a list with methods that set and get a matrix as its inverse in an intrinsic environment variable
## The second function is passed the list from the first and attempts to calculate and set its inverse .If the inverse is already set ,the cached value is used.


## makeCacheMatrix will create a matrix x,and expose three methods to set or get the matrix x and its inverse.

makeCacheMatrix <- function(x = matrix()) {
cachedInv<- NULL ## inverse is being initialized
## set x in parent environment with the desired value,if inverseis already set we can get rid of it.
set<- function(userValue=matrix()){
x<<- userValue
cachedInv<<-NULL

}
get<-function() x
##set inverse variable in parent environment to desired value and return the value
setInverse <- function(invVal) {
cachedInv<<-invVal
return(cachedInv)
}
getInverse <- function() cachedInv
list(set=set, get=get, setInverse=setInverse,getInverse=getInverse)
##given the list variable from the first function ,will first check to see if there is already a cached inverse and return 
## otherwise it will attempot to solve its inverse and set or return it.

cacheSolve <- function(x=makeCacheMatrix(1:4, nrow=2,ncol=2),...) {## special matrix provided or a test matrix of 2*2
calculatedInverse<- x $getInverse()
## check if there is a cached value and its matrix
if(!is.null(calculatedInverse) && is.matrix(calculatedInverse)){
message("we found the cached data and saved value")
return(calculatedInverse)
}
        ## otherwise get the matrix
        matrixToSolve<-x $get()
        ## try to solve the matrix and catch errors and warning
        calculatedInvere<-tryCatch({
        solve(matrixToSolve)
        },warning=function(w){
        message("This may not be the result you are looking for!!")
        message(w)
        }, error=function(e) {
        message("something went wrong while solving the matrix")
        message(e)
        message("\n")
        })
        ##whatever case set the value of the inverse(null if something went wrong )
        message("setting the value of inverse to :")
        x $setInverse(calculatedInverse)
}
