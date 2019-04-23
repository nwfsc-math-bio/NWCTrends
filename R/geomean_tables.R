geomean_table = function(pops, mpg, total.fit, fracwild.fit, min.year=NULL, max.year=2014) {
  lenbands=5
  nbands=5
  
  n = length(pops)
  short.pops = clean.pops(pops)
  
  #set up the years
  datyears = as.numeric(colnames(total.fit$model$data))
  min.year = max.year-nbands*lenbands+1
  nyr = length(min.year:max.year)
  min.year.data=max(datyears[1], min.year)
  max.year.data=min(max(datyears),max.year)
  data.years = as.character(min.year.data:max.year.data)
  
  tabgeomean1=tabgeomean2=data.frame(population=short.pops, mpg=mpg, matrix("",length(pops),nbands+1),stringsAsFactors = FALSE)
  geo.start=3 # col where geo means start
  for(pop in 1:n){
    #set up the data
    data = rep(NA,nyr);names(data)=min.year:max.year
    popname  = pops[pop]
    state.vals=raw.vals=""
    if(popname %in% rownames(total.fit$model$data)){
      
      states = rep(NA,nyr);names(states)=min.year:max.year
        wild.states = total.fit$states[paste("X.",popname,sep=""),]+log(fracwild.fit$fracwild.states[popname, ])
        names(wild.states)=colnames(total.fit$model$data)
        wild.raw = total.fit$model$data[popname, ]+log(fracwild.fit$fracwild.raw[popname, ])
        names(wild.raw)=colnames(total.fit$model$data)
      data[data.years]= wild.raw[data.years]
      states[data.years] = wild.states[data.years]
      statesgeomean=filter(states,rep(1/lenbands,lenbands),sides=1)
      statesgeomean=round(exp(statesgeomean),digits=0)
      
      dat=dat0=data
      dat[is.na(dat)]=0
      rawgeomean = filter(dat,rep(1,lenbands),sides=1)
      dat0[!is.na(dat0)]=1; dat0[is.na(dat0)]=0
      not0 = filter(dat0, rep(1,5), sides=1)
      #require 2 datapoints
      not0[not0==1]=NA
      rawgeomean = round(exp(rawgeomean/not0),digits=0)
      state.vals.numeric = rev(statesgeomean[seq(length(data),lenbands,-1*lenbands)])
      state.vals.numeric = c(state.vals.numeric, 100*(state.vals.numeric[nbands]-state.vals.numeric[nbands-1])/state.vals.numeric[nbands-1])
      raw.vals.numeric = rev(rawgeomean[ seq(length(data),lenbands,-1*lenbands) ])
      raw.vals.numeric = c(raw.vals.numeric, 100*(raw.vals.numeric[nbands]-raw.vals.numeric[nbands-1])/raw.vals.numeric[nbands-1])
      state.vals = paste(round(state.vals.numeric, digits=0),sep="")
      raw.vals = paste(round(raw.vals.numeric, digits=0),sep="")
      raw.vals[raw.vals=="NA"]=""; raw.vals[raw.vals=="NaN"]=""
      state.vals[state.vals=="NA"]=""; state.vals[state.vals=="NaN"]=""
    }
    
    data=rep(NA,nyr);names(data)=min.year:max.year
    states=rep(NA,nyr);names(states)=min.year:max.year
    if(!is.null(total.fit)){
      if(popname %in% rownames(total.fit$model$data)){ 
        total.states=total.fit$states[paste("X.",popname,sep=""),];names(total.states)=colnames(total.fit$model$data)
        states[data.years] = total.states[data.years]
        total.statesgeomean=filter(states,rep(1/lenbands,lenbands),sides=1)
        total.statesgeomean=round(exp(total.statesgeomean),digits=0)
        
        data[data.years] = total.fit$model$data[popname, data.years]
        dat=dat0=data
        dat[is.na(dat)]=0
        total.rawgeomean = filter(dat,rep(1,lenbands),sides=1)
        dat0[!is.na(dat0)]=1; dat0[is.na(dat0)]=0
        not0 = filter(dat0, rep(1,5), sides=1)
        #require 2 datapoints
        not0[not0==1]=NA
        total.rawgeomean = round(exp(total.rawgeomean/not0),digits=0)
        
        state.vals.numeric = rev(total.statesgeomean[seq(length(data),lenbands,-1*lenbands)])
        state.vals.numeric = c(state.vals.numeric, 100*(state.vals.numeric[nbands]-state.vals.numeric[nbands-1])/state.vals.numeric[nbands-1])
        state.vals=paste(state.vals, " (", round(state.vals.numeric, digits=0), ")",sep="")
        raw.vals.numeric = rev(total.rawgeomean[ seq(length(data),lenbands,-1*lenbands) ])
        raw.vals.numeric = c(raw.vals.numeric, 100*(raw.vals.numeric[nbands]-raw.vals.numeric[nbands-1])/raw.vals.numeric[nbands-1])
        raw.vals=paste(raw.vals, " (", round(raw.vals.numeric, digits=0), ")",sep="")
      }
    }
    raw.vals[raw.vals=="NA (NaN)"]=""; raw.vals[raw.vals=="NaN (NaN)"]=""; raw.vals[raw.vals==" (NaN)"]=""; raw.vals[raw.vals==" (NA)"]=""
    tabgeomean1[pop,geo.start:(nbands+geo.start)]=state.vals
    tabgeomean2[pop,geo.start:(nbands+geo.start)]=raw.vals
    
  }
  yrranges=paste(rev(seq(max.year-lenbands+1,min.year,-1*lenbands)),
                 rev(seq(max.year,min.year+lenbands-1,-1*lenbands)),sep="-")
  colnames(tabgeomean1)=c("Population", "MPG", yrranges,"% Change")
  colnames(tabgeomean2)=c("Population", "MPG", yrranges,"% Change")
  list(statesgeomean=tabgeomean1,rawgeomean=tabgeomean2)
}