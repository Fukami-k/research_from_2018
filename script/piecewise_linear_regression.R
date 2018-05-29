#this fike defines piecewise linear regression
###########
#make data
###########
f <- function(x){
    if(x<=0){
        return(1)
    }else if(x<=1){
        return(x+1)
    }else if(x<=2){
        return(-x+3)
    }else if(x<=3){
        return(x-1)
    }else if(x<=4){
        return(-x+5)
    }else{
        return(1)
    }
}

f2 <- function(x) sapply(x,f)

len_x <- 40

data_produce <- function(len_x = 40){
    x <- rep(0:3,10) + runif(len_x)
    y <- f2(x) + rnorm(len_x, sd = 1/9) -1.5
    return(data.frame(x,y))
}
##########
#set regression
##########
len_section <- 3
w<-numeric((len_section+1)*2)
g<- function(x,w){
    break_point <- 0
    value <- w[1] * x + w[len_section + 2]
    for(i in 1:len_section){
        break_point <- w[len_section+2+i]
        value <- value + w[i+1] * max(x-break_point, 0)
    }
    return(value)
}

g2 <- function(x,w) sapply(x,g,w=w)

utility <- function(x,y,w) sum((y-g2(x,w))^2)

utility2 <- function(w) utility(x, y, w)

#########
#optim and plot
#########
test_plr <- function(i, method1 = "SANN"){
    for(k in 1:i){

        data <- data_produce(len_x)
        x<-data$x
        y<-data$y

        param_set <- list(par = c(numeric(len_section + 2), (0:(len_section - 1))))

        param_optim  <- optim(par = param_set$par  , fn = utility2, method = method1)
        curve(g2(x, param_optim$par ), xlim = c(-1,5), ylim = c(-2,2), col = 2)
        points(x, y, col = 1)

        param_optim2 <- optim(par = param_optim$par, fn = utility2, method = "BFGS")
        curve(g2(x, param_optim2$par), col = 3, add = T)
        print(param_optim2)

        param_optim  <- optim(par = param_set$par  , fn = utility2, method = "SANN")
        curve(g2(x, param_optim$par ), col = 4, add = T)
        points(x, y, col = 1)

        param_optim2 <- optim(par = param_optim$par, fn = utility2, method = "BFGS")
        curve(g2(x, param_optim2$par), col = 5, add = T)
        print(param_optim2)

        print(paste("session", k, "/", i, "has finished."))
    }
}
