
xbarthetadist = function(n,iter,mu,sigma){
  library(mvtnorm)
  library(ggplot2)
  library(gridExtra)
  mat = matrix(NA, nr= iter, nc=4)
  colnames(mat)= c("xbar1","xbar2","thetae","thetar")
  for(i in 1:iter){
    x = rmvnorm(n,mu,sigma)
    mat[i,c(1,2)] <- colMeans(x)
    s=(n-1)/n*cov(x)
    eig=eigen(s)
    thetaeigen=acos(eig$vectors[,1][1])
    thetarot= .5*atan(2*s[1,2]/(s[1,1]-s[2,2])) + pi/2
    
    mat[i,3]<-thetaeigen
    mat[i,4]<-thetarot
    


  }

  df=as.data.frame(mat)
  g=ggplot(df, aes(x=xbar1,y=xbar2))  + coord_equal()

  gp = g + geom_point()
 
  gd = g + stat_density_2d()
  

  gt=ggplot(data=df, aes(df$thetar)) + geom_histogram() + xlab("Theta Value")
  

  gt2 = ggplot(data=df, aes(df$thetar)) + geom_density() + xlab("Theta")
  
  grid.arrange(gp,gd,gt,gt2,nrow=2)
 
 
 
 # thetaValues = df$theta;
 # hist(thetaValues)
  #plot(density(thetaValues))

  head(mat)
}


mu1=c(0,0)
sigma1 = matrix(c(100,40,40,200), nr=2,nc=2,byrow=FALSE)
xbarthetadist(n=1000,iter=1000,mu=mu1,sigma=sigma1)

mu1=c(0,0)
sigma1 = matrix(c(100,40,40,200), nr=2,nc=2,byrow=FALSE)
xbarthetadist(n=80,iter=1000,mu=mu1,sigma=sigma1)

sigma2 = matrix(c(100,-40,-40,200), nr=2,nc=2,byrow=FALSE)
xbarthetadist(n=80,iter=1000,mu=mu1,sigma=sigma2)


