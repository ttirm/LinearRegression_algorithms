# Load Data
data <- read.csv("https://raw.githubusercontent.com/llSourcell/linear_regression_live/master/data.csv", header = FALSE)
names(data) <- c("x", "y")
head(data)

mlestimation <- function(theta){
    slope <- theta[1]
    intercept <- theta[2]
    err <- theta[3]
    
    #loglikelihood computation
    likeli <- dnorm(data[1, 'y'], mean = data[1, 'x'] * slope + intercept, sd = err)
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


fit <- optim(par = c(0, 0, 1),
                        fn = mlestimation, hessian = T
)

fit



