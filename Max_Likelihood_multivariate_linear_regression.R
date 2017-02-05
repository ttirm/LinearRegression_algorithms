
# Load Data
data_x <- read.table("https://raw.githubusercontent.com/BradNeuberg/andrew-ng-course/master/linear_regression_2/ex3x.dat", 
                     header=FALSE)
names(data_x) <- c("x1", "x2")
data_x <- scale(data_x)
data_y <- read.table("https://raw.githubusercontent.com/BradNeuberg/andrew-ng-course/master/linear_regression_2/ex3y.dat", 
                     header=FALSE)
names(data_y) <- "y"

data <- data.frame(data_y, x0 = rep(1, nrow(data_x)), data_x)


mlestimation <- function(theta){
    
    err <- theta[length(theta)]
    
    y <- as.matrix(data[,1])
    
    x <- as.matrix(data[,-1])
    
    theta <- theta[1:(length(theta)-1)]
    
    #loglikelihood computation
    likeli <- dnorm(y, mean = x %*% theta, sd = err)
    
    loglikeli <- -2 * sum(log(likeli))
    
    # When the value corresponds to an extreme density point in the normal line, loglikelihood will return -INF. 
    # In these cases it's necessary increase the bell curve's width 
    if(loglikeli == -Inf){
        err <- 100000
        
        likeli <- dnorm(data[1, 'y'], mean = data[1, 'x'] * slope + intercept, sd = err)
        
        loglikeli <- -2 * sum(log(likeli))
    }
    
    loglikeli
    
}


fit <- optim(par = c(1, 1, 1,100000),
             fn = mlestimation, hessian = T
)

fit


