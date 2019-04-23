##################################################################################################
## This creates a data frame with a column of trend estimates from the year ranges in year.ranges
## From a MARSS fit object
##################################################################################################
trend_15_table = function(pops, mpg, total.fit, fracwild.fit, year.ranges=list(1990:2005,1999:2014)) {

  n = length(pops)
  short.pops = clean.pops(pops)
  
  #set up the years
  datyears = as.numeric(colnames(total.fit$model$data))
  min.year = min(unlist(year.ranges))
  max.year = max(unlist(year.ranges))
  
  nyr = length(min.year:max.year)
  min.year.data=max(datyears[1], min.year)
  max.year.data=min(max(datyears),max.year)
  data.years = as.character(min.year.data:max.year.data)
  
  tabtrend=data.frame( population=short.pops, mpg=mpg, matrix(NA,length(pops),length(year.ranges)) )
  for(pop in 1:n){
    #set up the data
    popname  = pops[pop]
    
    data = rep(NA,nyr);names(data)=min.year:max.year
    states = rep(NA,nyr);names(states)=min.year:max.year
      wild.states = total.fit$states[paste("X.",popname,sep=""),]+log(fracwild.fit$fracwild.states[popname, ])
      names(wild.states)=colnames(total.fit$model$data)
      wild.raw = total.fit$model$data[popname, ]+log(fracwild.fit$fracwild.raw[popname, ])
      names(wild.raw)=colnames(total.fit$model$data)

    data[data.years]= wild.raw[data.years]
    states[data.years] = wild.states[data.years]
    
    for(i in 1:length(year.ranges)){
      years=year.ranges[[i]]
      tmp.raw = data[as.character(years)]
      tmp = states[as.character(years)]
      if(all(is.na(tmp))){ 
        trend=NA
      }else{
        tmp.lm=lm(tmp~years)
        trend=coef(tmp.lm)[2]
      }
      tabtrend[pop,(i+2)]=paste(round(trend, digits=2), " (", 
                                round(confint(tmp.lm)[2,1], digits=2),", ", 
                                round(confint(tmp.lm)[2,2], digits=2),")",sep="")
      if(sum(!is.na(tmp.raw[1:5]))<2 | sum(!is.na(tmp.raw[11:15]))<2) tabtrend[pop,(i+2)] = ""
      
    }
  }
  yrranges=paste(unlist(lapply(year.ranges,min)),
    unlist(lapply(year.ranges,max)),sep="-")
  tmp.col=as.vector(sapply(yrranges,c,c("lowCI","upCI")))
  colnames(tabtrend)=c("Population", "MPG", yrranges)
  tabtrend
}