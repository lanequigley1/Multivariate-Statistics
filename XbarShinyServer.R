library(shiny)
library(shinydashboard)
library(gridExtra)

ui <- dashboardPage( 
  dashboardHeader(title = "Xbar Simulation"),
  dashboardSidebar(disable = TRUE), #disable = TRUE
  dashboardBody( 
    
    fluidRow(
      box(plotOutput("plot1", height=500)),
      
      
      box(
        status = "warning",
        solidHeader = TRUE,
        collapsible = TRUE,
        width = 3,
        background= "black",
        title = "Controls 1",
        numericInput("nnumber", "sample size", 50),
        numericInput("iternumber", "Iterations", 500)
        
      ),
      
      box(
        status = "warning",
        solidHeader = TRUE,
        collapsible = TRUE,
        width = 3,
        background= "green",
        title = "Controls 2",
        numericInput("mu1", "mu 1", 0),
        numericInput("mu2", "mu 2", 0)
        
      ),
      
      box(
        status = "warning",
        solidHeader = TRUE,
        collapsible = TRUE,
        width = 4,
        background= "red",
        title = "Controls 3",
        numericInput("s11", "x1 variance", 100),
        numericInput("s12", "covariance", 40),
        numericInput("s22", "x2 variance",200)
      )
      
    )
  )
)

server <- function(input, output) { 
  library(mvtnorm)
  library(ggplot2)
  xbarthetadist = function(n,iter,mu,sigma){
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
    list(gp=gp,gd=gd,gt=gt,gt2=gt2)
  }
  # thetaValues = df$theta;
  # hist(thetaValues)
  #plot(density(thetaValues))
  mu1=c(0,0)
  sigma1 = matrix(c(100,40,40,200), nr=2,nc=2,byrow=FALSE)
  
  
  output$plot1 <- renderPlot({
    mu1=c(input$mu1,input$mu2)
    sigma1 = matrix(c(input$s11,input$s12,input$s12,input$s22), nr=2,nc=2,byrow=FALSE)
    a=xbarthetadist(n=input$nnumber,iter=input$iternumber,mu=mu1,sigma=sigma1)
    grid.arrange(a$gp,a$gd,a$gt,a$gt2,nrow=2)
    
  })
  
  
  
  
}  

shinyApp(ui, server)
