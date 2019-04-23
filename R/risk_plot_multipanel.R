Status_trendfigure_multipanel = function(esu, pops, total.fit, fracwild.fit, min.year=NULL, max.year=NULL, silent=FALSE, CI.method="hessian", CI.sim=1000, log.scale=FALSE, same.scale=FALSE, type="abundance") {
  if(!(CI.method %in% c("hessian","parametric","innovations","none"))) 
    stop("Stopped in CSEGriskfigure because allowed CI methods are none, hessian, parametric, and innovations.\n", call.=FALSE)
  
  #Set up the min and max years
  years = as.numeric(colnames(total.fit$model$data))
  if(is.null(min.year)) min.year = years[1]
  if(min.year<years[1]) min.year = years[1]
  if(is.null(max.year)) max.year = max(years)
  if(max.year>max(years)) max.year = max(years)
  
  n = length(pops)
  short.pops = clean.pops(pops)
  
  if(n==1){ nplotcol=1 }
  if(n==2){ nplotcol=2 }
  if(n>2 & n<5){ nplotcol=2 }
  if(n>4) nplotcol=4
  nplotrow = ceiling(n/nplotcol)
  #the.omi=c(max(0,(6-nplotrow)*1.5)+.25,.5,.5,0)
  the.omi=c(.25,.5,.5,0)
  par(mfrow=c(nplotrow, nplotcol), omi=the.omi)
  
  ylims=c(.9*min(0, total.fit$model$data, na.rm=TRUE),
          1.1*max(0.1, total.fit$model$data, na.rm=TRUE) )
  if(!log.scale) ylims = exp(ylims)
  
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
    if(!same.scale) ylims=c( 0.9*min(0,wild.raw, total.raw, y.low.total, na.rm=TRUE),
                             1.1*max(.1,wild.raw, total.raw, y.high.total, na.rm=TRUE) )
    
    #plot the data    
    par(mar=c(2, 2, 2, 2) + 0.1)
    plot(years.trim, total.raw, type="n", bty="L", xlab="", ylab="", 
         ylim=ylims, 
         xlim=c(min.year-1,max.year+1))
    polygon(c(years.trim, rev(years.trim)), c(y.high.total, rev(y.low.total)), col = "grey75", border = NA)
    lines(years.trim, total.states, col="black", lwd=4)
    
    points(years.trim, total.raw)
    
    lines(years.trim, wild.states, col="red", lwd=1, lty=1)
    
    title(short.pops[pop], cex.main=1)
  }
  mtext( esu, side = 3, outer = TRUE, line=0, cex=1.5)
  if(log.scale){ 
    mtext(paste("Predicted log ", type, " and 95% CIs", sep=""), side=2, outer=TRUE, line=0, cex=.8)
  }else{
    mtext(paste("Predicted ", type, " and 95% CIs", sep=""), side=2, outer=TRUE, line=0, cex=.8)
  }
  
}