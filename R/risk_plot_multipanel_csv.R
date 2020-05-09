Status_trendfigure_multipanel_csv = function(esu, pops, total.fit, fracwild.fit, min.year=NULL, max.year=NULL, silent=FALSE, CI.method="hessian", CI.sim=1000, log.scale=FALSE, same.scale=FALSE, type="abundance") {
  if(!(CI.method %in% c("hessian","parametric","innovations","none"))) 
    stop("Stopped in Status_trendfigure_multipanel_csv because allowed CI methods are none, hessian, parametric, and innovations.\n", call.=FALSE)
  
  #Set up the min and max years
  years = as.numeric(colnames(total.fit$model$data))
  if(is.null(min.year)) min.year = years[1]
  if(min.year<years[1]) min.year = years[1]
  if(is.null(max.year)) max.year = max(years)
  if(max.year>max(years)) max.year = max(years)
  
  n = length(pops)
  short.pops = clean.pops(pops)
  
  df <- data.frame(pop="NA", years=NA, total.raw=NA, total.smoothed=NA,
                   total.smoothed.high=NA, total.smoothed.low=NA,
                   wild.smoothed=NA)

  for(pop in 1:n){
    #set up the data
    popname  = pops[pop]
    total.states=total.raw=y.high.total=y.low.total=rep(NA, length(years))
    if(popname %in% rownames(total.fit$model$data)){
      total.states = total.fit$states[paste("X.",popname,sep=""), ]
      total.raw = total.fit$model$data[popname, ] 
      y.high.total = total.states + 1.96*total.fit$states.se[paste("X.",popname,sep=""), ]
      y.low.total =  total.states - 1.96*total.fit$states.se[paste("X.",popname,sep=""), ]
    }
    wild.states = total.states+log(fracwild.fit$fracwild.states[popname, ])
    wild.raw = total.raw+log(fracwild.fit$fracwild.raw[popname, ])
    if(all(is.na(wild.raw))){
        wild.states[]=NA
    }else{
        n.start=max(min(which(!is.na(wild.raw))), which(years==min.year))
        if(n.start>1) wild.states[1:(n.start-1)]=NA
        n.end=min(max(which(!is.na(wild.raw))), which(years==max.year))
        if(n.end<length(wild.states)) wild.states[(n.end+1):length(wild.states)]=NA
    }
          
    if(all(is.na(total.raw))){
      min.year = min.year
      n.start = which(years==min.year)
    }else{
      n.start=max(min(which(!is.na(total.raw))), which(years==min.year))
    }
    
    n.end = which(years==max.year)
    
    #trim down the data
    wild.raw = wild.raw[n.start:n.end]
    total.raw = total.raw[n.start:n.end]
    wild.states=wild.states[n.start:n.end]
    total.states=total.states[n.start:n.end]
    y.high.total = y.high.total[n.start:n.end]
    y.low.total =  y.low.total[n.start:n.end]
    years.trim = years[n.start:n.end]
    if(!log.scale){
      wild.raw = exp(wild.raw)
      total.raw = exp(total.raw)
      total.states = exp(total.states)
      wild.states = exp(wild.states)
      y.high.total = exp(y.high.total)
      y.low.total = exp(y.low.total)
    }

    tmp <- data.frame(pop=short.pops[pop], years=years.trim, 
                      total.raw=total.raw, total.smoothed=total.states, total.smoothed.high=y.high.total, total.smoothed.low=y.low.total, wild.smoothed=wild.states)
    
    df <- rbind(df, tmp)
    
  }
  df <- df[-1,]
  df$total.smoothed <- round(df$total.smoothed, digits=2)
  df$total.smoothed.high <- round(df$total.smoothed.high, digits=2)
  df$total.smoothed.low <- round(df$total.smoothed.low, digits=2)
  df$wild.smoothed <- round(df$wild.smoothed, digits=2)
  
  return(df)
  
}