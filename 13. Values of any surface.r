

## please install "ggplot2" packages at first
## The function "plot.generalize" is used to generate surface of generalized Budyko equation
## Here are introduction to four parameters
## 1) "equation" can be equal to Fu-Fu, Fu-Yang, Fu-Wang, Yang-Fu, Yang-Yang, Yang-Wang, Wang-Fu, Wang-Yang, Wang-Wang
## 2) "n1" is the parameters in the generalized Budyko equations
## 3) "n2" is the parameters in the generalized Budyko equations
## 4) "space" can be equal to "supply", "demand", and "storage"


## plot function
plot.generalize=function(equation="Yang-Yang",n1=1,n2=1,space="supply"){
  
  #  Three Budyko-type equations
  Budy=function(x1,x2,n,Budyko=1){
    re=switch(Budyko,
              x1+x2-(x1^n+x2^n)^(1/n),
              (x1^(-n)+x2^(-n))^(-1/n),
              (x1+x2-((x1+x2)^2-4*n*(2-n)*x1*x2)^0.5)/(2*n*(2-n))
    )
    return(re)
  }
  
  # Nine generalized Budyko functions
  generalize=function(P,Wp,PE,n1,n2,First=1,Second=2){
    W=Budy(x1 = P,x2 = Wp,n = n1,Budyko = First)
    E=Budy(x1 = W,x2 = PE,n = n2, Budyko=Second)
    return(E)
  }
  
  ## equations
  if(equation=="Fu-Fu") {first=1;second=1}
  if(equation=="Fu-Yang") {first=1;second=2}
  if(equation=="Fu-Wang") {first=1;second=3}
  
  if(equation=="Yang-Fu") {first=2;second=1}
  if(equation=="Yang-Yang") {first=2;second=2}
  if(equation=="Yang-Wang") {first=2;second=3}
  
  if(equation=="Wang-Fu") {first=3;second=1}
  if(equation=="Wang-Yang") {first=3;second=2}
  if(equation=="Wang-Wang") {first=3;second=3}
  
  
  ## plot
  library(ggplot2)

  if(space=="supply"){
    
    WpP.seq=seq(0.01,10,length.out = 100)
    PEP.seq=seq(0.01,10,length.out = 100)
    Values=data.frame("PEP"=NA,"WpP"=NA,"Value"=NA)
    number=0
    for(x1 in 1:100){
      for(y1 in 1:100){
        PEP=PEP.seq[x1]
        WpP=WpP.seq[y1]
        temp=generalize(P=1,Wp=WpP.seq[y1],
                        PE=PEP.seq[x1],
                        n1=n1,n2=n2,First=2,Second=2)
        number=number+1
        Values[number,c("PEP","WpP")]=c(PEP,WpP)
        Values[number,c("Value")]=temp
      }
    }
    
    ## plot
    p1=ggplot()+
      geom_tile(data=Values,
                aes(x=PEP,y=WpP,fill=Value))+
      scale_fill_gradientn(colors = rainbow(5),limits=c(-.01,1.01))+
      labs(x="PE/P",y="Wp/P",fill="E/P",title="(a)")+
      scale_x_continuous(limits = c(-0.1,10.1), expand = c(0,0))+
      scale_y_continuous(limits = c(-0.1,10.1), expand = c(0,0))+
      theme_minimal()+
      theme(legend.key.height = unit(43,"pt"),
            legend.position = "right")
    
  }
  
  if(space=="demand"){
    
    WpPE.seq=seq(0.01,10,length.out = 100)
    PPE.seq=seq(0.01,10,length.out = 100)
    Values=data.frame("PPE"=NA,"WpPE"=NA,"Value"=NA)
    number=0
    for(x1 in 1:100){
      for(y1 in 1:100){
        PPE=PPE.seq[x1]
        WpPE=WpPE.seq[y1]
        temp=generalize(P=PPE,Wp=WpPE,
                        PE=1,
                        n1=n1,n2=n2,First=2,Second=2)
        number=number+1
        Values[number,c("PPE","WpPE")]=c(PPE,WpPE)
        Values[number,c("Value")]=temp
      }
    }
    
    ## plot
    p1=ggplot()+
      geom_tile(data=Values,
                aes(x=PPE,y=WpPE,fill=Value))+
      scale_fill_gradientn(colors = rainbow(5),limits=c(-.01,1.01))+
      labs(x="P/PE",y="Wp/PE",fill="E/PE",title="(a)")+
      scale_x_continuous(limits = c(-0.1,10.1), expand = c(0,0))+
      scale_y_continuous(limits = c(-0.1,10.1), expand = c(0,0))+
      theme_minimal()+
      theme(legend.key.height = unit(43,"pt"),
            legend.position = "right")
    
  }
  
  if(space=="storage"){
    
    PWp.seq=seq(0.01,10,length.out = 100)
    PEWp.seq=seq(0.01,10,length.out = 100)
    Values=data.frame("PWp"=NA,"PEWp"=NA,"Value"=NA)
    number=0
    for(x1 in 1:100){
      for(y1 in 1:100){
        PWp=PWp.seq[x1]
        PEWp=PEWp.seq[y1]
        temp=generalize(P=PWp,Wp=1,
                        PE=PEWp,
                        n1=n1,n2=n2,First=2,Second=2)
        number=number+1
        Values[number,c("PWp","PEWp")]=c(PWp,PEWp)
        Values[number,c("Value")]=temp
      }
    }
    
    ## plot
    p1=ggplot()+
      geom_tile(data=Values,
                aes(x=PWp,y=PEWp,fill=Value))+
      scale_fill_gradientn(colors = rainbow(5),limits=c(-.01,1.01))+
      labs(x="P/Wp",y="PE/Wp",fill="E/Wp",title="(a)")+
      scale_x_continuous(limits = c(-0.1,10.1), expand = c(0,0))+
      scale_y_continuous(limits = c(-0.1,10.1), expand = c(0,0))+
      theme_minimal()+
      theme(legend.key.height = unit(43,"pt"),
            legend.position = "right")
  }
  
  p1
}

## example
## Fu -Fu equation with n1=2 and n2=1.2 in supply-limited space
plot.generalize(equation="Fu-Fu",n1=2,n2=1.2,space="supply")

## Fu -Fu equation with n1=2 and n2=1.2 in demand-limited space
plot.generalize(equation="Fu-Fu",n1=2,n2=1.2,space="demand")

## Fu -Fu equation with n1=2 and n2=1.2 in storage-limited space
plot.generalize(equation="Fu-Fu",n1=2,n2=1.2,space="storage")
