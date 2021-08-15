### Time Series - ARIMA and GARCH Script Development

data_pull        <- function(type, tick, from, to)           # Pull stock/etf/indicator data
{
  if(type == "indicator")
  {
    cnames <- c("Data")
    getSymbols.FRED(tick, env = .GlobalEnv, return.class = "xts")
    temp <- data.frame(get(tick))
    colnames(temp) <- cnames 
    #rm(get(tick))
    temp
  }
  else if(type == "stock/etf")
  {
    cnames <- c("Open","High","Low","Close","Volume","Adjusted")
    getSymbols(tick, from = from, to = to)
    if(tick == "^GSPC") { temp <- data.frame(GSPC) }
    else { temp <- data.frame(get(tick)) }
    colnames(temp) <- cnames
    temp
  }
  else { bad <- "Invalid data type to pull"; bad }
}

comp.data        <- function(type, dlist, cnames, from, to)  # Put data sets together, match by date sequence
{
  # First check for time frame (days, months, years)
  if(type == "d")
  {
    dates <- seq(as.Date(from), as.Date(to), by = "day")      # Create a data list for subsetting
    dates <- as.character(dates)                              # Make data character type for compatibility
  }
  else if (type == "m")
  {
    dates <- seq(as.Date(from), as.Date(to), by = "month")    # Create a data list for subsetting
    dates <- as.character(dates)                              # Make data character type for compatibility
  }
  else 
  {
    dates <- seq(as.Date(from), as.Date(to), by = "quarter")  # Create a data list for subsetting
    dates <- as.character(dates)                              # Make data character type for compatibility
  }
  
  for(i in 1:length(dlist))
  {
    print(noquote(paste("Data Frame:",i," - ",cnames[i])))
    if(i == 1)
    {
      temp.df             <- dlist[[i]]
      values              <- data.frame(temp.df[dates, ])
      rownames(values)    <- dates
      new.lst             <- data.frame(values)
    }
    else
    {
      temp.df             <- dlist[[i]]
      values              <- data.frame(temp.df[dates, ])
      rownames(values)    <- dates
      new.lst <- cbind(new.lst, values)
    }
  }
  
  colnames(new.lst) <- cnames
  new.lst
}

strip.param      <- function(data, param) # Strip adjusted price column from ohlc objects (standard equity)
{
  rnames <- rownames(data)
  new.df <- data.frame(data[,param])
  colnames(new.df) <- "Data"
  rownames(new.df) <- rnames
  new.df
}

calc.ret.df      <- function(data) # Calculate return for 2+ column data frame
{
  rnames <- rownames(data); rnames <- rnames[-1]
  cnames <- colnames(data)
  new.df <- data.frame(matrix(nrow = length(data[,1])-1))
  
  for(i in 1:length(data))
  {
    temp <- c()
    for(j in 1:length(data[,i]))
    {
      temp <- c(temp, log(data[j,i]/data[j-1,i]))
    }
    new.df <- cbind(new.df, temp)
  }
  new.df <- new.df[-1]
  colnames(new.df) <- cnames
  rownames(new.df) <- rnames
  new.df
}

calc.ret.uni     <- function(data, type, keepformat = FALSE)
{
  if(class(data) == "list")
  {
    out <- NULL
    for(i in 1:length(data))
    {
      t <- data[[i]]
      sr <- c() ; gr = 0
      for(j in 2:length(t))
      {
        if(type == "growth") { gr <- log(t[j]/t[1]) }
        else                 { gr <- log(t[j]/t[i]) }
        sr <- c(sr, gr)
      }
      out <- cbind(out, sr)
    }
    
    if(keepformat == TRUE)
    { new <- list(); for(i in 1:ncol(out)) { new <- list[[i]]; names(new) <- names(data); return(new) } }
    else { colnames(out) <- names(data); return(out) }
  }
  else if(class(data) == "data.frame")
  {
    out <- NULL
    for(i in 1:ncol(data))
    {
      t  <- data[,i]
      sr <- c() ; gr = 0
      for(j in 2:length(t))
      {
        if(type == "growth") { gr <- log(t[j,i]/t[1,i]) }
        else                 { gr <- log(t[j,i]/t[(j-1),i]) }
        sr <- c(sr, gr)
      }
      out <- cbind(out, sr)
    }
    colnames(out) <- colnames(data) ; return(out)
  }
  else if(class(data) == "numeric")
  {
    out <- c() ; gr = 0
    for(i in 2:length(data))
    {
      if(type == "growth") { gr <- log(t[i]/t[1]) }
      else                 { gr <- log(t[i]/t[(i-1)]) }
      out <- c(out, gr)
    }
    return(out)
  }
  else { print(noquote("Unsupported Data Object Class")); return(NULL) }
}

cor.agg          <- function(data) # Generate a correlation matrix for a full data set
{
  params <- colnames(data)
  cor.matrix <- data.frame(matrix(ncol = 1))
  for(i in 1: length(data))
  {
    temp <- data.frame()
    for(j in 1:length(data))
    {
      temp <- rbind(temp, cor(as.numeric(data[,i]), as.numeric(data[,j])))
    }
    cor.matrix <- cbind(cor.matrix, temp)
  }
  cor.matrix <- cor.matrix[,-1]
  colnames(cor.matrix) <- params; rownames(cor.matrix) <- params
  cor.matrix
  
}

reg.agg          <- function(data) # Generates a regression model matrix for an entire data set
{
  reg.matrix <- data.frame(matrix(nrow=length(data)))
  names <- colnames(data)
  
  for(i in 1: length(data))
  {
    temp <- data.frame()
    for(j in 1:length(data))
    {
      if(j == i) { temp <- rbind(temp, 1); next }
      form  <- as.formula(paste(names[i],"~",names[j]))
      model <- lm(form, data = data)
      sum   <- summary(model)
      stats <- glance(sum)
      r     <- stats$r.squared
      temp <- rbind(temp, r)
    }
    reg.matrix <- cbind(reg.matrix, temp)
  }
  reg.matrix <- reg.matrix[,-1]
  colnames(reg.matrix) <- names
  rownames(reg.matrix) <- names
  return(reg.matrix)
}

rank.correg      <- function(data) # Correlation Table or Regression R^2 table
{
  obs.table <- data
  cnames    <- colnames(obs.table)
  
  orders.name  <- NULL
  orders.value <- NULL
  
  for(i in 1:length(obs.table[,1]))       # Go by row
  {
    temp  <- obs.table[i,]
    
    tcnames <- cnames
    order.n <- c() 
    order.v <- c()
    
    for(j in 1:length(temp))              # Inside current row
    {
      cm = 1                              # Current Max
      for(k in 1:length(temp))            # Check all rows for max, repeat with length -1 until order established
      {
        if(abs(temp[k]) > abs(temp[cm])) { cm = k } 
      }
      
      order.n <- c(order.n, tcnames[cm])
      order.v <- c(order.v, temp[,cm])
      temp    <- temp[-cm]
      tcnames <- tcnames[-cm]
    }
    
    orders.name   <- rbind(orders.name, order.n[-1])
    orders.value  <- rbind(orders.value, order.v[-1])
  }
  
  rownames(orders.name)  <- rownames(obs.table)
  rownames(orders.value) <- rownames(obs.table)
  
  out <- list(orders.name, orders.value)
  return(out)
}

plot.ranks       <- function(obs, type, dname, dval)
{
  for(i in 1:length(obs))
  {
    val      <- signif(as.numeric(dval[obs,]),2)
    val.abs  <- abs(val)      # signif(as.numeric(abs(dval[obs,])),2)
    name     <- as.character(dname[obs,]) 
    
    if(val[1] == 1.00) # Drop correlation with self
    {
      val     <- val[-1]
      val.abs <- val.abs[-1]
      name    <- name[-1]
    }
    
    t <- NULL
    if(type == "correlation") { t = "correlation" }
    if(type == "regression")  { t = "r^2" }
    
    
    plot(val.abs, ylab = t, type = "p", pch = 20, col = "black", main = paste(obs,type,"indicator rankings"))
    text(val.abs, name             , pos=1, offset=0.3, cex=0.7,  col="blue")
    text(val.abs, as.character(val), pos=3, offset=0.3, cex=0.7,  col="red")
  }
  
}

analyze.ranks    <- function(data, title, print = FALSE, plot = FALSE)
{
  line <- noquote("-------------------------------------------------------------------------")
  
  print(line)
  print(noquote(paste("Ranking Correlations for",title)))
  print(noquote(paste("(1) Building Correlation + Regression (R^2) Tables...")))
  
  
  cors <- cor.agg(data) ; cors <- round(cors, 4)
  regs <- reg.agg(data) ; regs <- round(regs, 4)
  
  print(noquote(paste("(2) Ranking Correlations and R^2 values + Creating Summary Data Frame...")))
  
  cd <- rank.correg(cors)
  rd <- rank.correg(regs)
  
  nc <- cd[[1]] ; nr <- rd[[1]]
  vc <- cd[[2]] ; vr <- rd[[2]]
  
  # Create DF
  cor.namerank.df <- data.frame(matrix(ncol = ncol(nc)))
  reg.namerank.df <- data.frame(matrix(ncol = ncol(nr)))
  
  rn <- c() ; cn <- c()
  
  for(i in 1:nrow(nc))
  {
    target <- rownames(nc)[i]
    r1 <- paste(target,"~")
    r2 <- paste("value.",i,sep="")
    
    cor.namerank.df <- rbind(cor.namerank.df, nc[i,]) 
    cor.namerank.df <- rbind(cor.namerank.df, vc[i,])
    
    reg.namerank.df <- rbind(reg.namerank.df, nr[i,])
    reg.namerank.df <- rbind(reg.namerank.df, vr[i,])
    
    rn <- c(rn, r1, r2)
    cn <- c(cn, i)
    
  }
  
  cor.namerank.df <- cor.namerank.df[-1,]
  reg.namerank.df <- reg.namerank.df[-1,]
  
  cn <- cn[1:ncol(nc)]
  
  rownames(cor.namerank.df) <- rn ; colnames(cor.namerank.df) <- cn 
  rownames(reg.namerank.df) <- rn ; colnames(reg.namerank.df) <- cn 
  
  tc <- rownames(nc) ; type.c <- "correlation"
  tr <- rownames(vc) ; type.r <- "regression"
  
  if(plot == TRUE) 
  { 
    par(mfrow=c(1,2))
    for(i in 1:length(nc)) 
    { 
      plot.ranks(nc[i], type.c, nc, vc) 
      plot.ranks(nr[i], type.r, nr, vr)
    } 
  }
  
  print(noquote(paste("(3) Subsetting Correlations >= 75th quantile of correlations...")))
  
  cor.sub <- list()
  
  for(i in 1:nrow(nc))
  {
    q      <- as.numeric(quantile(vc[i,])[4])
    subn   <- nc[i,which(vc[i,] >= q)]
    subv   <- vc[i,which(vc[i,] >= q)]
    subr   <- vr[i,which(nr[i,] %in% subn)]
    sub.df <- data.frame(subn, subv, subr)
    colnames(sub.df) <- c("etf","corr","r2")
    
    cor.sub[[i]] <- sub.df
  }
  
  names(cor.sub) <- tc
  
  print(noquote(paste("(4) Selecting Best Pairs by Correlation then R^2 fit + Creating Data Frame...")))
  if(print == TRUE) { print(line) }
  
  pairs <- NULL ; rn <- c()
  
  for(i in 1:length(cor.sub))
  {
    t <- names(cor.sub)[i]
    
    s <- cor.sub[[i]]
    b <- s[which.max(s$r2),]
    
    if(print == TRUE)
    {
      print(noquote(paste("Best Pair:",t,"+",b$etf,"based on corr =",b$corr,"and r^2 =",b$r2)))
    }
    
    rn <- c(rn, paste("Pair.",i,sep=""))
    
    pairs <- rbind(pairs,c(t,as.character(b$etf),b$corr,b$r2))
  }
  
  rownames(pairs) <- rn ; colnames(pairs) <- c("A","B","Corr","R^2")
  
  out <- list(pairs, cors, cor.namerank.df, regs, reg.namerank.df)
  names(out) <- c("subsets","cormatrix","corrankdf","regmatrix","regrankdf")
  
  print(noquote(paste("(5) FUNCTION END")))
  
  return(out)
}




test.box         <- function(lst, maxlag, title, plot)
{
  ret.lst <- c()
  for(i in 1:maxlag)
  {
    temp.tst <- Box.test(lst, lag = i, type = "Ljung-Box")
    temp.res <- temp.tst$p.value
    ret.lst <- c(ret.lst, temp.res)
  }
  
  if(plot == TRUE)
  {
    plot(ret.lst, type="l", main = paste(title,"LB test:",maxlag,"lags"))
    abline(0.05,0,col="red")
  }
  
  return(ret.lst)
}

combomaker       <- function(d1, d2)
{
  new <- data.frame(matrix(ncol=2))
  
  for(i in 1:length(d1))
  {
    for(j in 1:length(d2))
    {
      temp <- c()
      temp <- c(d1[i], d2[j])
      new <- rbind(new, temp)
    }
  }
  new <- new[-1,]
  colnames(new) <- c("AR order","MA order")
  return(new)
}

check.unit.root  <- function(data, title, plot, print) # Returns differences needed to remove unit root and data set differenced at that level
{
  # Testing for unit roots
  test.p = 1.0
  diffs  = 0
  test   = 0
  
  while(test.p > 0.05)
  {
    if(plot == TRUE)
    {
      par(mfrow=c(1,3))
      plot(data, type = "l", main = paste(title,"with",diffs,"differences")) ; acf(data) ; pacf(data) 
    }
    
    test   <- adf.test(data)
    test.p = as.numeric(test$p.value)
    if(test.p > 0.05) 
    { 
      if(print == TRUE) { print(noquote(paste("Unit root found at",diffs,"differences - test:",round(test.p,5)))) }
      diffs = diffs + 1 
      data  = diff(data, 1)
    }
  }
  
  if(print == TRUE) { print(noquote(paste("Unit root removed with",diffs,"differences"))) }
  out <- list(diffs, data)
  return(out)
}

get.orders       <- function(data, maxlag, sig.det = 0, type, title, printr, plotr, plot.innerTests = FALSE)
{
  line <- noquote("-------------------------------------------------------------------------------------------")
  if(printr == TRUE) { print(line) }
  
  if(printr == TRUE) { print(noquote(paste("Evaluating",title,"for",maxlag,"lags..."))) }
  if(printr == TRUE) { print(line) }
  
  par(mfrow=c(1,3))
  
  x.acf    <- acf(data , (maxlag-1), plot = plot.innerTests) ; s.acf  <- data.frame(x.acf$acf)
  x.pacf   <- pacf(data, maxlag  , plot = plot.innerTests) ; s.pacf <- data.frame(x.pacf$acf)
  
  bt.lag   <- test.box(data, maxlag, title, plot.innerTests)
  
  sl       <- qnorm((1 + 0.95)/2)/sqrt(sum(!is.na(data)))
  sl       <- sl*(1+sig.det)
  
  eval.mat <- matrix(data=NA,ncol=maxlag,nrow=6) ; r1 = 0 ; r2 = 0 ; r3 = 0; what = NULL
  pts <- list()
  
  for(q in 1:2)
  {
    if(q == 1) { r1 = 1; r2 = 2; r3 = 3; what = s.acf} 
    else       { r1 = 4; r2 = 5; r3 = 6; what = s.pacf}    
    for(j in 1:ncol(eval.mat))
    {
      if(what[j,1] >= sl)   { eval.mat[r1,j] = 1 }
      if(what[j,1] <= -sl)  { eval.mat[r2,j] = 1 }
      if(bt.lag[j] <= 0.05) { eval.mat[r3,j] = 1 }
    }
    
    us <- c() ; ds <- c() ; ld <- c() ; uv <- c() ; dv <- c()
    
    for(k in 1:ncol(eval.mat))
    {
      if(!(is.na(eval.mat[r1,k]))) { us <- c(us, k) }
      if(!(is.na(eval.mat[r2,k]))) { ds <- c(ds, k) }
      if(!(is.na(eval.mat[r3,k]))) { ld <- c(ld, k) }
    }
    
    if(!(is.null(us))) { for(k in 1:length(us)) { if(us[k] %in% ld) { uv <- c(uv, us[k]) } } }
    else { us <- c(0) ; uv <- c(0) }
    if(!(is.null(ds))) { for(k in 1:length(ds)) { if(ds[k] %in% ld) { dv <- c(dv, ds[k]) } } }
    else { ds <- c(0) ; dv <- c(0) }
    
    pts[[q]] <- list(usig = us, dsig = ds, uvalid = uv, dvalid = dv)
  }
  
  names(pts) <- c("ma","ar") # validated orders
  ma <- pts[[1]] 
  ar <- pts[[2]]
  
  if(type == "res") { ma <- list(NULL,NULL,NULL,NULL) } # Residuals Tests only use ACF
  
  if(plotr == TRUE)
  {
    par(mfrow=c(1,1))
    us.acf  <- ma[[1]] ; ds.acf  <- ma[[2]] ; uv.acf  <- ma[[3]] ; dv.acf  <- ma[[4]]
    us.pacf <- ar[[1]] ; ds.pacf <- ar[[2]] ; uv.pacf <- ar[[3]] ; dv.pacf <- ar[[4]]
    
    check.lst <- list(us.acf, uv.acf, ds.acf, dv.acf, us.pacf, uv.pacf, ds.pacf, dv.pacf)
    n         <- c("usa","dsa","uva","dva","usm","dsm","uvm","dvm")
    no.plot   <- c()
    
    for(p in 1:length(check.lst))
    {
      tp <- check.lst[[p]]
      if(length(tp) == 1 && tp[1] == 0)       { no.plot <- c(no.plot,0) }
      else if(length(tp) >= 1  && tp[1] != 0) { no.plot <- c(no.plot,1) }
      else if(is.null(tp))                    { no.plot <- c(no.plot,0) }
      else { if(printr == TRUE) { print(noquote("There has to be an error...")) } }
    }
    
    names(no.plot) <- n
    
    x <- seq((maxlag-(maxlag+5)),(maxlag+7),1)
    y <- seq(-5,5)
    
    offs = 0.2
    z  = 0    ; zu = z  + offs ; zd = z  - offs  
    al = 2.5  ; au = al + offs ; ad = al - offs
    pl = -2.5 ; pu = pl + offs ; pd = pl - offs
    
    ts   = x[3]
    lt   = 0.5
    loff = 0.1
    
    usa = uva = al+1 ; dsa = dva = al-1 
    usm = uvm = pl+1 ; dsm = dvm = pl-1
    
    sc = 1.25
    vc = sc*0.75
    tc = vc
    
    toff = 0.2
    
    plot(1, type = "n",
         xlim = c(x[1],x[length(x)]), ylim = c(y[1],y[length(y)]), 
         ylab = "Threshold Crossovers", 
         xlab = "Lag (order)",
         main = paste(title,"ACF/PACF plot with BT dependancy verification"))
    
    abline(h = z            ,cex = 4  ,col = "black" ,lty = 1)
    text(ts ,zu ,pos = 3    ,cex = lt ,offset = loff ,col = "black"    ,labels = "(ACF Crossovers)" )
    text(ts ,zd ,pos = 1    ,cex = lt ,offset = loff ,col = "black"    ,labels = "(PACF Crossovers)")
    
    abline(h = al           ,cex = 2  ,col = "darkgray" ,lty = 1)
    text(ts ,au ,pos = 3    ,cex = lt ,offset = loff    ,col = "black" ,labels = "(ACF >= SL)" )
    text(ts ,ad ,pos = 1    ,cex = lt ,offset = loff    ,col = "black" ,labels = "(ACF <= -SL)")
    
    abline(h = pl           ,cex = 2  ,col = "darkgray" ,lty = 1)
    text(ts ,pu ,pos = 3    ,cex = lt ,offset = loff    ,col = "black" ,labels = "(PACF >= SL)" )
    text(ts ,pd ,pos = 1    ,cex = lt ,offset = loff    ,col = "black" ,labels = "(PACF <= -SL)")
    
    abline(v = 0          ,type = "l", cex = 2.5, col = "black", lty = 1)
    abline(v = (maxlag+1) ,type = "l", cex = 2.5, col = "black", lty = 1) 
    
    if(no.plot[n[1]] == 1)
    {
      lines(x = us.acf  ,y = rep(usa,length(us.acf))   ,type = "p" ,pch = 24 ,cex = sc      ,col = "red")
      lines(x = uv.acf  ,y = rep(uva,length(uv.acf))   ,type = "p" ,pch = 20 ,cex = vc      ,col = "black")
      if(no.plot[n[2]] == 1) 
      { text(x  =  uv.acf ,y = rep(uva,length(uv.acf))   ,pos = 1    ,cex = tc ,offset = toff ,labels = uv.acf) }
    }
    if(no.plot[n[3]] == 1)
    {
      lines(x = ds.acf  ,y = rep(dsa,length(ds.acf))   ,type = "p" ,pch = 25 ,cex = sc      ,col = "red")
      lines(x = dv.acf  ,y = rep(dva,length(dv.acf))   ,type = "p" ,pch = 20 ,cex = vc      ,col = "black")
      if(no.plot[n[4]] == 1) 
      { text(x  =  dv.acf ,y = rep(dva,length(dv.acf))   ,pos = 3    ,cex = tc ,offset = toff ,labels = dv.acf) }
    }
    if(no.plot[n[5]] == 1)
    {
      lines(x = us.pacf ,y = rep(usm,length(us.pacf))  ,type = "p" ,pch = 24 ,cex = sc      ,col = "blue")
      lines(x = uv.pacf ,y = rep(uvm,length(uv.pacf))  ,type = "p" ,pch = 20 ,cex = vc      ,col = "black")
      if(no.plot[n[6]] == 1) 
      { text(x  = uv.pacf ,y = rep(uvm,length(uv.pacf))  ,pos = 1    ,cex = tc ,offset = toff ,labels = uv.pacf) }
    }
    if(no.plot[n[7]] == 1)
    {
      lines(x = ds.pacf ,y = rep(dsm,length(ds.pacf)) ,type = "p" ,pch = 25 ,cex = sc       ,col = "blue")
      lines(x = dv.pacf ,y = rep(dvm,length(dv.pacf)) ,type = "p" ,pch = 20 ,cex = vc       ,col = "black")
      if(no.plot[n[8]] == 1) 
      { text(x  = dv.pacf ,y = rep(dvm,length(dv.pacf)) ,pos = 3    ,cex = tc ,offset = toff  ,labels = dv.pacf) }
    }
    
    legend(x=c((maxlag+1),(x[length(x)]+1.75)),
           y=c((y[length(y)]-3),y[length(y)]),
           legend = c("SigACF-Up","SigACF-Down","SigPACF-Up","SigPACF-Down","ValACF","ValPACF"),
           col = c("red","red","blue","blue","black","black"),
           pch = c(24,25,24,25,20,20), cex = 0.60,
           bty = "o", bg = "gray59")
  }
  
  acf.tag  <- rep("-",nrow(s.acf))    ; acf.tag[which(abs(s.acf[,1])   >=  sl)] = "***"
  pacf.tag <- rep("-",nrow(s.pacf))   ; pacf.tag[which(abs(s.pacf[,1]) >=  sl)] = "***"
  bt.tag   <- rep("-",length(bt.lag)) ; bt.tag[which(bt.lag <= 0.05)] = "SIGNFICANT"
  
  lag.df <- data.frame(pacf.tag, bt.tag, acf.tag)
  rownames(lag.df) <- seq(1,nrow(lag.df),1)
  colnames(lag.df) <- c("AR","Dependancy","MA")
  
  ar.all <- sort(c(ar[[3]], ar[[4]])) ; ar.all <- ar.all[which(ar.all != 0)]
  ma.all <- sort(c(ma[[3]], ma[[4]])) ; ma.all <- ma.all[which(ma.all != 0)]
  
  max.ar = 0 ; max.ma = 0;
  
  if(length(ar.all) == 0) { ar.all <- NULL                     }
  else                    { max.ar = ar.all[which.max(ar.all)] }
    
  if(length(ma.all) == 0) { ma.all <- NULL                     }
  else                    { max.ma = ma.all[which.max(ma.all)] }
  
  if(printr == TRUE) { print(noquote(paste(title,": AR orders = (",paste(ar.all,collapse=","),") with max =",max.ar))) }
  if(printr == TRUE) { print(noquote(paste(title,": MA orders = (",paste(ma.all,collapse=","),") with max =",max.ma))) }
  if(printr == TRUE) { print(line) }
  
  temp <- list(ar.all, max.ar, ma.all, max.ma, lag.df)
  names(temp) <- c("aro","maxar","mao","maxma","df")
  return(temp)
}

check.arma.coef  <- function(obj, a, m)
{
  if(a == 0 && m == 0) { return(NULL) }
  
  arterms <- NULL; materms <- NULL
  coef <- data.frame(obj$model$coef)
  
  if(a > 0)           { arterms <- coef[1:a,]        }
  if(m > 0 && a > 0)  { materms <- coef[(a+1):(a+m),]}
  if(m > 0 && a == 0) { materms <- coef[1:m,]        }
  
  if(!(is.null(arterms)))
  {
    a.val <- NULL; add0 = FALSE
    if(nrow(coef) > sum(a,m)) { a.val <- c(a.val, as.numeric(arterms))} #, as.numeric(coef[(nrow(coef)),])) }
    else                      { a.val <- c(a.val, as.numeric(arterms)); } #add0 = TRUE }
    pt <- polyroot(a.val) #; if(add0 == TRUE) { a.val <- a.val[-1] }
    test <- Mod(pt)
    if(any(test < 1)) { return(noquote("AR portion is non-stationary (reject)")) }
  }
  if(!(is.null(materms)))
  {
    m.val <- as.numeric(materms)
    if(any(abs(m.val) > 1)) { return(noquote("MA portion is not invertable (reject)")) }
  }
  return(NULL)
}

build.arma       <- function(data, maxlag, sigboost = 0, maxcoeff = NULL, use.auto = FALSE, title, plot, print, force.model = FALSE, threshold = 0.025, plot.innerTests = FALSE, print.innerTests = FALSE)
{
  # data   = time series
  # maxlag = maximum lag order to analyze for ACF and PACF (also auto-calibration function)
  # title  = add title to plots
  # plot   = plot ts/acf/pacf (unit root check) + acf/pacf (translate lags)
  # force.model      = FALSE implies that if an ARIMA(0,d,0) is selected as best model, compare by AIC threshold to find "next best" model (Caution here)
  # threshold = force.model AIC allowed decrease percentage (%)
  # plot.innerTests  = FALSE implies DO NOT plot residuals lag validation testing
  # print.innerTests = FALSE implies DO NOT print residuals lag testing
  
  # Time Function
  start_time <- Sys.time()
  line <- noquote("-------------------------------------------------------------------------------------------")
  
  print(line)
  
  # Check for unit root
  print(noquote(paste("Building Models for Time Series:",title))) ; print(line)
  print(noquote("(1) Checking for Unit Roots (ADF Test)..."))
  
  ur.check = check.unit.root(data, title, plot, TRUE)
  diffs     = ur.check[[1]]
  diff.data = ur.check[[2]] 
  
  print(noquote(paste("(2) Selecting + Validating AR/MA orders up to",maxlag,"lags to build models...")))
  print(noquote(paste("* User set 'significance booster' (Additional Crossover Length needed to be truly significant) at",round(sigboost*100,2),"%")))
  print(noquote(paste("* User set 'significance booster' (Additional Crossover Length needed to be truly significant) at",round(sigboost*100,2),"%")))
  
  
  orders <- get.orders(diff.data, maxlag, sig.det = sigboost, type = "data", title, printr = print.innerTests, plot, plot.innerTests)
  
  ar.order <- orders[[1]] ; amax <- orders[[2]]
  ma.order <- orders[[3]] ; mmax <- orders[[4]]
  
  if(!(is.null(maxcoeff)))
  {
    print(noquote(paste("**Maximum Allowed Coefficients per model has been set to:",maxcoeff)))
    if(any(ar.order) > maxcoeff) { ar.order <- ar.order[which(ar.order < maxcoeff)] }
    if(any(ma.order) > maxcoeff) { ma.order <- ma.order[which(ma.order < maxcoeff)] }
    if(amax > maxcoeff) { amax = maxcoeff }
    if(mmax > maxcoeff) { mmax = maxcoeff }
  }
  
  print(noquote(paste("AR order = (",paste(ar.order,collapse=","),") with max =",amax)))
  print(noquote(paste("MA order = (",paste(ma.order,collapse=","),") with max =",mmax))) ; print(line)
  
  # Create model list
  model.list <- list()
  aic.list   <- c()
  var.list   <- c()
  orders     <- c()
  
  inc = 1
  
  # Always test a (0,0) model
  print(noquote("(3) Creating ARMA(0,0,0) model..."))
  
  m <- arima(data, order = c(0,diffs,0))
  o        <- arimaorder(m)
  ord      <- paste("(",o[1],",",o[2],",",o[3],")",sep="")
  aic.list <- c(aic.list, round(m$aic,4))
  var.list <- c(var.list, round(m$sigma2,8)) 
  orders   <- c(orders, ord)
  if(print == TRUE) { print(noquote(paste("ARMA model",ord,"has AIC =",aic.list[inc],"and variance =",var.list[inc]))) }
  model.list[[inc]] <- m
  
  # AR models
  if(!(is.null(ar.order)))
  {
    print(noquote("(4) Creating AR models..."))
    for(i in 1:length(ar.order))
    {
      m        <- try(arima(data, order = c(ar.order[i],diffs,0)), silent = TRUE)
      
      if(isTRUE(class(m) == "try-error"))
      { if(print == TRUE) { print(noquote(paste("Attempted Model with AR order",ar.order[i],"failed: calculation/convergence error (Reject)"))); next } }
      
      coef.check <- check.arma.coef(m, ar.order[i], 0)
      if(!(is.null(coef.check)))
      { if(print == TRUE) { print(noquote(paste("Attempted Model with AR order",ar.order[i],"failed:",coef.check))); next } }
      
      o        <- arimaorder(m)
      ord      <- paste("(",o[1],",",o[2],",",o[3],")",sep="")
      
      res.lag  <- get.orders(m$residuals, maxlag, sig.det = 0, type = "res", title, print.innerTests, plot.innerTests, print.innerTests)
      res.lag  <- res.lag[[3]]
      
      if(is.null(res.lag))
      {
        inc = inc + 1
        aic.list <- c(aic.list, round(m$aic,4))
        var.list <- c(var.list, round(m$sigma2,8)) 
        orders   <- c(orders, ord)
        if(print == TRUE) { print(noquote(paste("ARMA model",ord,"has AIC =",aic.list[inc],"and variance =",var.list[inc]))) }
        model.list[[inc]] <- m
      }
      else { if(print == TRUE) { print(noquote(paste("ARMA model",ord,"rejected based on residuals test"))) } }
    }
  }
  else  { ar.order <- c(0) ; print(noquote("No AR orders were validated to be significant...")) }
  
  # MA models
  if(!(is.null(ma.order)))
  {
    print(noquote("(5) Creating MA models..."))
    for(i in 1:length(ma.order))
    {
      m        <- try(arima(data, order = c(0,diffs,ma.order[i])), silent = TRUE)
      
      if(isTRUE(class(m) == "try-error"))
      { if(print == TRUE) { print(noquote(paste("Attempted Model with MA order",ma.order[i],"failed: calculation/convergence error (Reject)"))); next; } }
      
      coef.check <- check.arma.coef(m, 0, ma.order[i])
      if(!(is.null(coef.check)))
      { if(print == TRUE) { print(noquote(paste("Attempted Model with MA order",ma.order[i],"failed:",coef.check))); next } }
      
      o        <- arimaorder(m)
      ord      <- paste("(",o[1],",",o[2],",",o[3],")",sep="")
      
      res.lag  <- get.orders(m$residuals, maxlag, sig.det = 0, type = "res", title, print.innerTests, plot.innerTests, print.innerTests)
      res.lag  <- res.lag[[3]]
      
      if(is.null(res.lag))
      {
        inc = inc + 1
        aic.list <- c(aic.list, round(m$aic,4))
        var.list <- c(var.list, round(m$sigma2,8))
        orders   <- c(orders, ord)
        if(print == TRUE) { print(noquote(paste("ARMA model",ord,"has AIC =",aic.list[inc],"and variance =",var.list[inc]))) }
        model.list[[inc]] <- m
        
      }
      else { if(print == TRUE) { print(noquote(paste("ARMA model",ord,"rejected based on residuals test"))) } }
    }
  }
  else { ma.order <- c(0) ; print(noquote("No MA orders were validated to be significant...")) }
  
  # Create list of possible ARMA orders
  arma.order  <- combomaker(ar.order, ma.order)
  
  if(!(is.null(maxcoeff)))
  {
    for(i in nrow(arma.order):1)
    {
      if(sum(arma.order[i,]) > maxcoeff) 
      { arma.order <- arma.order[-i,] }
    }
  }
  
  # ARMA models
  if(nrow(arma.order) > 0)
  {
    print(noquote("(6) Creating ARMA models..."))
    for(i in 1:nrow(arma.order))
    {
      m        <- try(arima(data, order = c(arma.order[i,1],diffs,arma.order[i,2])), silent = TRUE)
      
      if(isTRUE(class(m) == "try-error"))
      { if(print == TRUE) { print(noquote(paste("Attempted Model with AR,MA order",arma.order[i,1],",",arma.order[i,2],"had a calculation error (Reject)"))); next; } }
      
      coef.check <- check.arma.coef(m, arma.order[i,1], arma.order[i,2])
      if(!(is.null(coef.check)))
      { if(print == TRUE) { print(noquote(paste("Attempted Model with AR,MA order",arma.order[i,1],",",arma.order[i,2],"failed:",coef.check))); next } }
      
      o        <- arimaorder(m)
      ord      <- paste("(",o[1],",",o[2],",",o[3],")",sep="")
      
      res.lag  <- get.orders(m$residuals, maxlag, sig.det = 0, title, type = "res", print.innerTests, plot.innerTests, print.innerTests)
      res.lag  <- res.lag[[3]]
      
      if(is.null(res.lag) && ord != "(0,0,0)")
      {
        inc = inc + 1
        aic.list <- c(aic.list, round(m$aic,4))
        var.list <- c(var.list, round(m$sigma2,8))
        orders   <- c(orders, ord)
        if(print == TRUE) { print(noquote(paste("ARMA model",ord,"has AIC =",aic.list[inc],"and variance =",var.list[inc]))) }
        model.list[[inc]] <- m
      }
      else if(is.null(res.lag) && ord == "(0,0,0)")
      {
        if(print == TRUE) { print(noquote("ARMA model = (0,0,0), already constructed...")) }
      }
      else { if(print == TRUE) { print(noquote(paste("ARMA model",ord,"rejected based on residuals test"))) } }
    }
  }
  else { print(noquote("No ARMA order combinations were created... if this is printing, there should be an error in this function...")) }
  
  if(use.auto == TRUE)
  {
    print(noquote("(7) Creating Auto-Calibrarted ARMA model..."))
    # Auto-calibrated Model
    
    m        <- try(auto.arima(data, max.p = amax, max.q = mmax, test = "adf", ic = "aic", stepwise = TRUE, stationary = TRUE), silent = TRUE) # Speed Up
    
    if(isTRUE(class(m) == "try-error"))
    { if(print == TRUE) { print(noquote(paste("Attempted Auto-Calibrated Model had a calculation error (Reject)"))); next; } }
    else
    {
      o        <- arimaorder(m)
      ord      <- paste("(",o[1],",",o[2],",",o[3],")",sep="")
      
      res.lag  <- get.orders(m$residuals, maxlag, sig.det = 0, title, type = "res", print.innerTests, plot.innerTests, print.innerTests)
      res.lag  <- res.lag[[3]]
      
      if(is.null(res.lag) && ord != "(0,0,0)")
      {
        inc = inc + 1
        aic.list <- c(aic.list, round(m$aic,4))
        var.list <- c(var.list, round(m$sigma2,8))
        orders   <- c(orders, ord)
        if(print == TRUE) { print(noquote(paste("ARMA model",ord,"has AIC =",aic.list[inc],"and variance =",var.list[inc]))) }
        model.list[[inc]] <- m
      }
      else if(is.null(res.lag) && ord == "(0,0,0)")
      {
        if(print == TRUE) { print(noquote("ARMA model = (0,0,0), already constructed...")) }
      }
      else
      {
        if(print == TRUE) { print(noquote(paste("ARMA model",ord,"rejected based on residuals test"))) }
      }
      
      print(line)
    }
  }
  else { print(noquote("(7) Adding an Auto Calibrarted Model Selection NOT SELECTED by user...")); print(line) }
 
  
  if(is.null(model.list))
  {
    print(noquote("No models chosen"))
    return(NULL)
  }
  
  # Best Model Selection
  
  names(model.list) <- orders
  
  loc   = which.min(aic.list)
  
  model = model.list[[loc]]
  
  if(arimaorder(model) == c(0,0,0) && length(model.list) > 1 && force.model == TRUE)
  {
    old.aic       = aic.list[loc]       # Save old min. AIC for testing
    aic.list[loc] = Inf                 # Set current min AIC to infonity
    loc.new       = which.min(aic.list) # Find next best min AIC
    new.aic       = aic.list[loc.new]   # New AIC (2nd best min)
    
    comp = (abs(new.aic) - abs(old.aic))/abs(old.aic)
    if(comp < threshold)
    {
      print(noquote(paste("Model Forcing SUCCESFUL:")))
      if(print == TRUE) { print(noquote(paste("Old AIC    =",old.aic)))                     }
      if(print == TRUE) { print(noquote(paste("New AIC    =",new.aic)))                     }
      if(print == TRUE) { print(noquote(paste("Threshold  = -",threshold*100," %",sep=""))) }
      if(print == TRUE) { print(noquote(paste("Change     =",comp*100,"%")))                }
      
      aic.list[loc] = old.aic
      loc = loc.new
    }
    else
    {
      if(print == TRUE) { print(noquote(paste("Model Forcing FAILED: decrease in AIC was too great..."))) }
      aic.list[loc] = old.aic
    }
  }
  else if (arimaorder(model) == c(0,0,0) && length(model.list) == 1 && force.model == TRUE)
  { if(print == TRUE) { print(noquote("Model Forcing FAILED: ARIMA(0,0,0) was only model created...")) }  }
  else if (arimaorder(model) != c(0,0,0) && length(model.list) >= 1 && force.model == TRUE)
  { if(print == TRUE) { print(noquote("Model Forcing NULL: ARIMA(0,0,0) was not the best model..."))   }  }
  else { if(print == TRUE) { print(noquote("Model Forcing was not allowed by user..."))                }  }
  
  tot.poss.models = sum(length(ar.order), length(ma.order), nrow(arma.order), 2)
  tot.models      = length(model.list)
  perc.used       = round((tot.models/tot.poss.models)*100,2) 
  
  print(line)
  print(noquote(paste("Total Models Constructed = ",tot.models," which was ",perc.used," % of total model consideration",sep="")))
  print(noquote(paste("Model Choosen = ARMA",orders[loc]," with AIC = ",aic.list[loc]," and variance = ",var.list[loc],sep="")))
  print(line) ; print(model.list[[loc]]) ; print(line)
  
  end_time <- Sys.time()
  print(noquote(paste("### RUN TIME:",(end_time - start_time)))) ; print(line) ; print(noquote(""))
  
  out <-list(model.list[[loc]], maxlag) ; names(out) <- c("model","maxlag")
  
  return(out)
}

model.residuals  <- function(lst, type, plotr)
{
  mn   <- names(lst)
  ps   <- NULL
  rest <- NULL 
  m    <- NULL
  o    <- 0
  res  <- NULL
  
  out  <- NULL
  vars <- NULL
  gmo  <- NULL
  
  for(i in 1:length(lst))
  {
    if(type == "arma")
    {
      rest = "Res"
      ps   = paste("Residuals Plots for",mn[i],"ARMA model")
      m    <- lst[[i]][[1]]
      res  <- m$residuals     ; out  <- cbind(out,res)
      o    <- lst[[i]][[2]]   ; if(o < 25) { o = 25 }
    }
    else
    {
      rest = "S-Res"
      ps   = paste("Standardized Residuals Plots for",mn[i],"ARMA+GARCH model")
      
      res <- lst[[i]][[1]][[7]] ; out  <- cbind(out, res)
      v   <- lst[[i]][[1]][[6]] ; vars <- cbind(vars, v)
      o   <- lst[[i]][[2]]      ; if(o < 25) { o = 25 }
    }
    
    fn <- paste(paste(mn[i],rest,sep="_"),".jpg",sep="")
    
    
    if(plotr == TRUE)
    {
      par(mfrow=c(1,4))
      acf(res       ,(o+5), main  = paste(mn[i],rest,"ACF"))
      test.box(res  ,(o+5), title = paste(mn[i],rest)  , TRUE)
      acf(res^2     ,(o+5), main  = paste(mn[i]," ",rest,"^2"," ACF",sep=""))
      test.box(res^2,(o+5), title = paste(mn[i]," ",rest,"^2",sep=""), TRUE)
      #dev.copy(jpeg,filename=fn)
      #dev.off()
    }
    
  }
  
  out  <- data.frame(out) ; if(!(is.null(vars))) { vars <- data.frame(vars) }
  colnames(out) <- mn     ; if(!(is.null(vars))) { colnames(vars) <- mn     }
  if(!(is.null(vars))) { gmo <- list(res=out,var=vars); return(gmo) }
  else { return(out) }
}

standardize.garch.res <- function(res, sigma)
{
  new <- c()
  for(i in 1:length(res))
  {
    str = res[i]/sigma[i]
    new <- c(new, str)
  }
  return(new)
}

build.garch      <- function(models, data, mlim, slim, criteria, print = TRUE, print.innerTests = FALSE, plot.innerTests = FALSE)
{
  line <- noquote("-------------------------------------------------------------------------------------------")
  
  mn    <- names(models)
  dist  <- c("norm","std")
  
  print(line)
  print(noquote(paste("GARCH(m,s) order limits:")))
  print(noquote(paste("m -> [1:",mlim,"]",sep = "")))
  print(noquote(paste("s -> [1:",slim,"]",sep = "")))
  print(noquote(paste("Distributions being tested = (",paste(dist,collapse=","),")",sep = "")))
  print(line)
  
  master <- list()
  
  for(i in 1:length(models))
  {
    start_time <- Sys.time()
    
    arma  <- models[[i]][[1]] ; lmax <- models[[i]][[2]]
    order <- arimaorder(arma)
    datam <- as.ts(data[,mn[i]])
    dref  <- c()
    
    print(noquote(paste("Building arma(p,q)+garch(m,s) models for:",mn[i])))
    
    inc        = 1
    curr       <- list()
    orders     <- c()
    best.model <- NULL
    
    for(d in 1:length(dist))
    {
      print(noquote(paste("(",d,") Distribution = ",dist[d],sep = "")))
      for(m in 1:mlim)
      {
        for(s in 1:slim)
        {
          pline = paste("arma(",order[1],",",order[3],")+garch(",m,",",s,")", sep="")
          form  = as.formula(paste("~",pline))
          
          gm <- garchFit(formula = form, data = datam, cond.dist = dist[d], trace = FALSE)  
          
          att <- attributes(gm)
          fit <- att$fit
          ics <- data.frame(fit$ics)
          
          #print(paste(as.numeric(fit$tval)   , collapse = " "))
          #print(paste(as.numeric(fit$se.coef), collapse = " "))
          
          check.na <- sum(is.na(as.numeric(fit$tval)), is.na(as.numeric(fit$se.coef)))
          if(check.na > 0)
          {
            if(print == TRUE) { print(noquote(paste(pline,"rejected based on NaNs in error testing phase"))); next }
          }
          
          coefmat <- t(fit$matcoef); coefmat <- coefmat[-3,]
          var   <- att$h.t
          std   <- att$sigma.t
          llh   <- round(fit$llh,4)
          aic   <- round(ics[which(rownames(ics) == "AIC"),],4)
          
          # Analyze Residuals
          res   <- att$residuals
          sres  <- standardize.garch.res(res, std)
          ssres <- sres^2
          
          t.res  <- paste(mn[i], "Std. Residuals"    )
          t.sres <- paste(mn[i], "(Std. Residuals)^2")
          
          res.lag   <- get.orders(sres,  lmax, sig.det = 0, type = "res", t.res, print.innerTests, plot.innerTests, plot.innerTests)
          res.lag   <- res.lag[[3]]
          sres.lag  <- get.orders(ssres, lmax, sig.det = 0, type = "res", t.sres, print.innerTests, plot.innerTests, plot.innerTests)
          sres.lag  <- sres.lag[[3]]
          
          #print(paste(res.lag,collapes=" "))
          #print(paste(sres.lag,collapse=" "))
          
          printline   <- paste(pline,"has AIC/LLH =",aic,"/",llh)
          printlined1 <- paste(pline,"rejected based on (std. residuals)^2 test")
          printlined2 <- paste(pline,"rejected based on (std. residuals) test")
          
          if(is.null(res.lag))
          {
            if(is.null(sres.lag))
            {
              if(print == TRUE) { print(noquote(printline)); dref <- c(dref, dist[d]) }
              temp        <- list(gm, coefmat, dist[d], aic, llh, var, sres)
              names(temp) <- c("garchmodel","coefmat","dist","aic","loglikelyhood","var","sres")
              curr[[inc]] <- temp ; orders <- c(orders, pline)      
              inc = inc + 1
            }
            else { if(print == TRUE) { print(noquote(printlined1)) } }
          }
          else { if(print == TRUE) { print(noquote(printlined2)) } }
        }
      }
    }
    names(curr) <- orders
    print(line)
    
    tot.poss = length(dist)*mlim*slim
    tot.made = length(curr)
    tot.perc = round(((tot.made/tot.poss)*100),2)
    
    # select best model
    print(noquote(paste("Model Selection Criteria =",criteria)))
    print(noquote(paste("Models compared =",tot.made,"which is",tot.perc,"% of possible",tot.poss,"models")))
    clist <- c()
    
    what = NULL ; best = NULL
    if(criteria == "aic") { what = 4 }
    if(criteria == "llh") { what = 5 }
    
    for(q in 1:length(curr))
    {
      etfg  <- curr[[q]]
      cr    <- etfg[[what]]
      clist <- c(clist, cr)
    }
    
    if(criteria == "aic") { best = which.min(clist) }
    if(criteria == "llh") { best = which.max(clist) }
    
    best.model <- curr[[best]]
    bml <- list(best.model, lmax) ; names(bml) <- c("modeldata","order")

    master[[i]] <- bml
    print(noquote(paste("Best model:",orders[best],"with dist =",dref[best],"where",criteria,"=",clist[best])))
    end_time <- Sys.time() ; print(noquote(paste("### RUN TIME:",(end_time - start_time)))) ; print(line)
  }
  names(master) <- c(mn)
  return(master)
}


# Compile Project Results --> Comparions of ETF correlation and Backtest Changes, market portfolio (ranked) backtest results and all relative plots
master.comp      <- function(data, pairs, gvars, capital, stop=-0.05, add.tests =NULL, print=FALSE, print.bt=FALSE,print.btALL=FALSE, plot.bt=FALSE)
{
  line = noquote("-----------------------------------------------------------------------")
  
  pairs <- lapply(pairs, function(x) return(x[[1]])) # Pairs = Subset table (x4) with pairs (x10) (List 4 x 10x4 DFs)
  
  out = add.list = t.list = tracks = reps = bags = models.list <- list(); data.n <- c()
  res = vars = NULL
  obs = nrow(pairs[[1]])
  
  bt.cap <- c(); endplots <- c()
  trk.inc = rep.inc = bag.inc = 1
  
  if(print == FALSE) 
  { print(line); print(noquote("Optimizing backtests on selected pairs - Storing results...")) }
  
  ### Get Behvorial Changes and BackTesting Results
  for(j in 1:length(pairs)) # [1:4]
  {
    r1 = r2 = r3 = r4 = r5 = r6 = r7 = r8 = r9 = r10 = r11 = r12 = c()
    
    if(print == TRUE) 
    { print(line); print(noquote(paste("(",j,") Pairs for ",names(pairs)[j],"...",sep=""))) }
    
    temp     <- pairs[[j]] # ETFs (10 rowws)
    capsplit = capital*(1/nrow(temp))
    
    for(t in 1:nrow(temp)) # [1:10]
    {
      #res.r = NULL; v = NULL
      
      target   = temp[t,1]
      
      if(j == 1) { t.list <- c(t.list, target) } # Target ETF List (Names)
      
      r1 <- c(r1, temp[t,2]) # R1 - Partner
      r2 <- c(r2, temp[t,3]) # R2 - Corr
      r3 <- c(r3, temp[t,4]) # R3 - R^2
      
      ## (0) In Advance (residuals and variance)
      if(j == 3) 
      { res = data[[3]]; resa = res[,target] ; resb = res[,r1[t]] ; res.r = resa-resb; }
      
      if(j == 4)
      {
        res = data[[4]]; resa = res[,target]    ; resb = res[,r1[t]] ; res.r = resa-resb;
        va = gvars[,target]; vb = gvars[,r1[t]] ; v    = va-vb                            
      }
          
      ## (1) Ratio Line
      if(j == 1) 
      { ra = data[[1]][,target]; rb = data[[1]][,r1[t]]; r = ra/rb } 
      else
      { ra = data[[2]][,target]; rb = data[[2]][,r1[t]]; r = ra-rb }
      
      ## (2) Market Prices
      if(j >= 2) { mp = data[[1]][-1,] } # Returns data
      else       { mp = data[[1]] }
      
      a = as.numeric(mp[,target])
      b = as.numeric(mp[,r1[t]])
      
      ## (3) Mean 
      if(j <= 2) { m = rep(mean(r),length(r)) }
      else       { m = rollmean(r, 15)        }
      
      ## (4) Std 
      if(j <= 2) { s = rep(sd(r), length(r))   }
      else { s = rep(sd(res.r), length(res.r)) }
      
      ## (5) Testing 'k'
      
      kn.0 <- max(r); kn.0 = round(0.5*kn.0,3)
      kx.0 <- min(r); kx.0 = round(0.5*kn.0,3)
      #if(kx.0 < 0 ) { kx.0 = round(kx.0*-1,3) }
      
      kn = seq(kn.0, (kn.0+1), 0.05)
      kx = seq(kx.0, (kx.0+1), 0.05)
      
      inc = 1 ; models <- list(); profs <- c()
      
      ## (5) Create Signals and Back Test
      for(t1 in 1:length(kn)) # test k number of ks in sequence before selection
      {
        un = ux = ln = lx = c(0)
        
        len   = min(length(r),length(s),length(m)) # Accounts for weird errors in arma/garch tests
        
        if(len < length(r))
        { if(print == TRUE) { print(noquote(paste("Error Adjusted Lengths (NA's) resulted in test length =",len,"periods or",round(len/252,2),"days..."))) } }
        
        for(t2 in 1:len)
        {
          if(j <= 3) # Not Garch
          {
            un  = c(un, (m[t2] + (s[t2]*kn[t1]))) ; ux = c(ux, (m[t2] + (s[t2]*kx[t1])))
            ln  = c(ln, (m[t2] - (s[t2]*kn[t1]))) ; lx = c(lx, (m[t2] - (s[t2]*kx[t1])))
          }
          else # Garch
          {
            un  = c(un, (m[t2] + (s[t2]+v[t2])*kn[t1])) ; ux = c(ux, (m[t2] + (s[t2]+v[t2])*kx[t1]))
            ln  = c(ln, (m[t2] - (s[t2]+v[t2])*kn[t1])) ; lx = c(lx, (m[t2] - (s[t2]+v[t2])*kx[t1]))
          }
        }
        
        #print(summary(r))
        #print(summary(s))
        #print(summary(m))
        #print(summary(lx))
        
        r <- r[1:len]
        m <- m[1:len]
        
        series   <- list(r,m,a,b)
        signals  <- list(un,ux,ln,lx)
        bag      <- list(kn=kn[t1],kx=kx[t1],stp=stop)
        
        if(print == TRUE) { print(noquote(paste("Optimizing type:",names(pairs)[j],"pair:",target,"~",r1[t],"--> stop:",stop,"+ kn/kx:",kn[t1],"/",kx[t1]))) }
        
        pn = paste(names(pairs)[j],"for:",target,"~",r1[t])
        
        p <- pairs.trade(series, signals, stop=-0.05, capital=capsplit,print=print.bt,printcp=print.btALL,plot=plot.bt,extra=bag,plotname=pn)
        profs <- c(profs, p$pr); models[[inc]] <- p ; inc = inc + 1
      }
        
      best = which.max(profs); bestm <- models[[best]]
      
      ret  <- bestm[[1]] ; prf <- bestm[[2]] ; tdf  <- bestm[[3]] ; tracks[[trk.inc]] <- bestm[[4]] ; reps[[rep.inc]] <- bestm[[5]] ; bags[[bag.inc]] <- bestm[[6]]
      
      rp <- bestm[[5]]
      
      trk.inc = trk.inc + 1
      rep.inc = rep.inc + 1
      bag.inc = bag.inc + 1
      
      num.trades <- (length(grep("Close",tdf[,2])) + (length(grep("Stop",tdf[,2]))))
      num.stops  <- (length(grep("Stop",tdf[,2])))
      num.calls  <- (length(grep("Margin Call",tdf[,2])))
      
      r4 <- c(r4, num.trades) # Number of trades
      r5 <- c(r5, num.stops)  # Number of Stops
      r6 <- c(r6, num.calls)  # Number of Margin Calls
      r7 <- c(r7, prf)        # $ return
      r8 <- c(r8, ret)        # % return
      
      print(noquote(paste("COMPLETE >",names(pairs)[j],"for:",target,"~",r1[t])))
    }
    
    add.list[[j]] <- list(r1,r2,r3,r4,r5,r6,r7,r8) # list of 
  }
  names(add.list) <- names(pairs)
  print(line); print(noquote(paste("Creating Summary Data Frames...")))
    
  #return(add.list)
  
  #### Summary Data Frames of relative performance over time with relationships
  
  # (1) Corr + BT + (Cross Comparison Anaysis (CCA))
  for(i in 1:obs) # 10 DF
  {
    summat   <- data.frame(matrix(data=NA,ncol=length(add.list),nrow=8+length(add.list)))
    eval.mat <- data.frame(matrix(data=NA,nrow=length(add.list),ncol=length(add.list)))
    
    track.temp = rep.temp = extra.temp = list()
    
    for(j in 1:length(add.list)) 
    {
      curr.add <- add.list[[j]]
      for(k in 1:8) 
      { 
        if(k > 1) { summat[k,j] <- as.numeric(curr.add[[k]][i]) }
        else { summat[k,j] <- curr.add[[k]][i] }
      }
      
      track.temp[[j]] <- tracks[[i+((j-1)*10)]] 
      rep.temp[[j]]   <- reps[[i+((j-1)*10)]]
      extra.temp[[j]] <- bags[[i+((j-1)*10)]]

      
      if(print == TRUE)
      { print(noquote(paste(t.list[i],"~",summat[1,j],": Corr/R^2 =",summat[2,j],"/",summat[3,j],"with trade =",summat[8,j],"%"))) }
      
      rp = rep.temp[[j]] ; pn = rp$param[[3]]
      
      output.only <- pairs.trade(rp$ts, rp$sigs, stop=rp$param[[1]], capital=rp$param[[2]], print=FALSE,printcp=FALSE,plot=TRUE,plotname=pn)
    }[20]
    
    for(m1 in 1:ncol(eval.mat))
    {
      for(m2 in 1:nrow(eval.mat))
      {
        if(m1 == m2) { eval.mat[m1,m2] = 0; next }
        comp <- round(((as.numeric(summat[7,m1]) - as.numeric(summat[7,m2]))/as.numeric(summat[7,m2]))*100,2)
        eval.mat[m1,m2] <- comp
      }
    }
    
    ### (2) Add CCA to table
    
    for(e in 9:nrow(summat)) { summat[e,] <- eval.mat[(e-8),] }
    
    ## Add names to everything...
    
    matn <- lapply(names(pairs), function(x) return(paste(x,"Comp")))
    
    names(track.temp) = names(rep.temp) = names(extra.temp) <- names(pairs)
    
    colnames(summat) <- names(pairs)
    rownames(summat) <- c(t.list[i],"Corr","R^2","NumTrades","NumStops","NumMargCall","Profit($)","Return(%)",matn)
    
    out[[i]] <- list(sum=summat,track=track.temp,rep=rep.temp,extrainfo=extra.temp)
    
    print(kable(summat, caption = paste("Summary Table:",t.list[i])))
  }
  names(out) <- t.list
  
  print(line); print(noquote(paste("Running Backtests...")))
  
  ### Run back tests
  
  # Back test results
  cap = capital ; cap.div = cap*(1/obs)
  
  bt.tests <- list(add.tests, data[[1]]) # Collect extra test data and price data (for full portfolio backtest)
  
  bt.df <- NULL
  
  inc = 1
  # Market and Holding ETFs 
  for(i in 1:length(bt.tests))
  {
    if(i == 1)
    {
      markets <- bt.tests[[i]]
      market.n <- names(markets)
      
      for(j in 1:length(markets))
      {
        index <- markets[[j]]
        tr <- round(((index[length(index)]-index[1])/index[1])*100,2)
        tp = cap*(1+(tr/100))
        bt.df <- rbind(bt.df, c(market.n[j], tr, tp))
        
        if(print==TRUE) { print(noquote(paste("Backtest Results:",bt.df[inc,1],"return =",bt.df[inc,2],"% with profit = $",bt.df[inc,3]))) }
        inc = inc + 1
      }
    }
    else
    {
      etf.df <- bt.tests[[i]]
      
      end.p   <- sum(as.numeric(etf.df[nrow(etf.df),]))
      start.p <- sum(as.numeric(etf.df[1,]))
      
      tr <- round(((end.p-start.p)/start.p)*100,2)
      tp <- cap*(1+(tr/100))
      bt.df <- rbind(bt.df, c("Long ETFs", tr, tp))
      
      if(print==TRUE) { print(noquote(paste("Backtest Results:",bt.df[inc,1],"return =",bt.df[inc,2],"% with profit = $",bt.df[inc,3]))) }
      inc = inc + 1
    }
  }
  
  # PT strategies (x4)
  
  #return(list(out,bt.df))
  
  pt.n <- names(pairs)
  for(i in 1:length(pairs)) # 4 full backtest
  {
    tp = 0
    for(j in 1:length(out)) { tp = tp + as.numeric(out[[j]][[1]][[i]][7]) }
    
    tr = round((tp/capital)*100,2)
    
    bt.df <- rbind(bt.df, c(pt.n[i], tr, tp))
    if(print==TRUE) { print(noquote(paste("Backtest Results:",bt.df[inc,1],"return =",bt.df[inc,2],"% with profit = $",bt.df[inc,3]))) }
    inc = inc + 1
  }
  
  bt.df <- bt.df[order(bt.df[,2],decreasing=TRUE),]
  colnames(bt.df) <- c("Portfolio","Return (%)","Return ($)")
  
  final.out <- list(summary=out, backtests=bt.df)
  
  print(noquote("Outputting Table of backtest results..."))
  print(kable(bt.df, caption = "Final Portfolio Backtests"))
  
  print(line); print(noquote("FUNCTION RUN COMPLETE"))
  
  return(final.out)
}

# Back Test Strategy --> Returns performance, tracker lists and the values and data needed to replcate this test (mostly for re-plotting) - DONE
pairs.trade      <- function(ts, sigs, stop.perc, capital = 1000000, tc = 5.00, tc.keepInAccount = 6, minPosSize = 0,
                        margin.open = 0.5, margin.maint = 1.25, margin.pen = 0.10, margin.safety = 0,
                        borrowAllow = TRUE, maxBorrowingBeforeClosingPositons = FALSE , emptyCapitalBeforeClosingPositons = FALSE,
                        max.borrow = 0.25, ir = 0.001, compound = "m", tradeDays = 252, debtPayPerDay = FALSE, capMaxPayDebt = 0.25,
                        print = FALSE, printcp = FALSE, print.md = FALSE, result = FALSE, plot = FALSE, extra=NULL, plotname=NULL) {
  # PARAMETERS AS DEFAULT:
  
  # Signal Line is calculatd as A-B ("diff"), A/B ("ratio"), or A-B with std = (GARCH model variance[t])*k ("variance")
  # vars = GARCH model variances (Set to NULL by default, required (to be not NULL) to trade using type = "variance")
  # Starting capital is $1,000,000
  # Trades Incur $5.00 transaction cost (per trade) 
  # Mutltiple of transaction cost to keep in capital account with every open trade set to 6 (i.e. 6 x tc = 6 trades (3 pairs trades) possible)
  # Minimum Position Size is set at 100 (testing)
  # Required Margin Account for intiating a short sale is 150% market value of Short Sale Trade (Regulation T)
  # Required Margin Account Maintenance level for open short sale is 125% (NYSE and NASD requirements)
  # Penalty for Margin Call failure is the difference in value + 10% of that difference
  # Safety Threshold for opening a short position set at 0% (must have 150% of value + trader dictated "cushion" %)
  # Borrowing for the account == TRUE
  # Max out Borrowing principle to decrease call limit before broker closes positions to cover == FALSE
  # Empty Capital to decrease call limit before broker closes positions to cover == FALSE
  # Maximum Borrowing Amount set to 25% of Capital Account
  # Borrowing Interest Rate is 2.5%
  # Borrowing Compounding Period set to "monthly"
  # Trading Days per annum set to 252
  # Paying off debt per day (to keep interest low, etc...) == FALSE (in this case, pay off all debt at end of strategy execution)
  # Maximum Capital Account percentage to use to pay down debt at any time set at 25% (at each day, only use up to 25% of capital account to pay down debt)
  # (print)    - Trace (tracking output (actions) line-by-line) == FALSE (tracking list is returned)
  # (printcp)  - Trace (Capital/MV list values) == FALSE
  # (print.md) - Trace of Margin Calls and Debt balance changes == FALSE (Included in tracking list)
  # Strategy Resulting Profit ($ + %) is NOT printed (but is returned)
  # Strategy Plot produced == FALSE
  # Extra is used for passing in anything the user wants to get back out that cannot be extracted by the test ('k' modiers for signals, comments, etc...)
  
  # Position Update and Margin Call Functions
  update.pos <- function(pt, per, s, p, ap, bp, margin.perc)
  {
    # Declare Variables for outputting use - Also returned (0) if no position is open
    open.l = 0 ; open.s = 0 ; open = 0
    curr.l = 0 ; curr.s = 0
    margin.req = 0
    change.l = 0; change.s = 0;
    chge = 0; chge.perc = 0
    
    if(pt == "long-short")
    {
      # Opening Value
      open.l = s[1]*p[1]
      open.s = s[2]*p[2]
      
      open   = sum(open.l, open.s)
      
      # Current Value
      curr.l = s[1]*ap[per]
      curr.s = s[2]*bp[per]
      
      margin.req = curr.s*(margin.perc) # Margin Call test
      
      # Changes based on long or short trade
      change.l = curr.l - open.l
      change.s = open.s - curr.s
      
      chge      = sum(change.l, change.s)
      chge.perc = chge/open
    }
    if(pt == "short-long")
    {
      # Opening Value
      open.s = s[1]*p[1]
      open.l = s[2]*p[2]
      
      open   = sum(open.l, open.s)
      
      # Current Value
      curr.s = s[1]*ap[per]
      curr.l = s[2]*bp[per]
      
      margin.req = curr.s*(margin.perc) # Margin Call test
      
      # Changes based on long or short trade
      change.l = curr.l - open.l 
      change.s = open.s - curr.s
      
      chge      = sum(change.l, change.s)
      chge.perc = chge/open
    }
    
    out <- lapply(c(curr.l, curr.s, margin.req, chge, chge.perc), function(x) round(x, 2))
    out <- as.numeric(out)
    return(out)
  }

  margin.call <- function(m.req, pt, caplist, s, p, debt.util, debt.lim, reqlist, cost, pen)
  {
    # m.req = margin requirement needed
    # pt    = position type
    # s     = stock position sizes
    # p     = stock price sizes
    # debt.core = current principle
    # debt.lim  = max amount allowed to borrow
    
    borrow    = reqlist[1]  # Is borrowing allowed?
    empty.cap = reqlist[2]  # Is emptying the capital account to partially fill a call allowed?
    max.debt  = reqlist[3]  # Is maxing out debt to partially fill a call allowed?
    
    m.acc = caplist[1]      # Current Margin Account
    c.acc = caplist[2]      # Current Capital Account
    lp    = caplist[3]      # MV long position
    sp    = caplist[4]      # MV short position
    
    debt.core = debt.util   # Current Principal Debt amount
    debt.sum  = caplist[5]  # Current total level of debt (principal + interest gained)
    
    debt.allow = debt.lim - debt.core # Maximum debt still allowed to borrow
    
    need = m.req - m.acc    # required call amount to cover
    
    # Cover call with some or all of capital account - SUCCESS
    if(c.acc >= need)       
    {
      m.acc    = m.acc + need
      c.acc    = c.acc - need
      need = 0
      
      new.caplist <- c(m.acc,c.acc,lp,sp,debt.sum)
      out <- list(s, p, debt.core, new.caplist)
      return(out)
    }
    
    # Empty Capital Account - Balance > 0
    if(c.acc < need && empty.cap == TRUE) 
    {
      m.acc = m.acc - c.acc
      c.acc = 0
      need  = need - c.acc
    }
    
    # Borrow Capital = SUCCESS
    if(borrow == TRUE && debt.allow >= need)
    {
      debt.core = debt.core + need
      debt.sum  = debt.sum  + need
      
      m.acc = m.acc + need
      need = 0
      
      new.caplist <- c(m.acc,c.acc,lp,sp,debt.sum)
      out         <- list(s, p, debt.core, new.caplist)
      return(out)
    }
    
    # Borrow Capital - Balance > 0 (Max out debt)
    if(borrow == TRUE && debt.allow < need && max.debt == TRUE)
    {
      debt.core = debt.core + debt.allow
      debt.sum  = debt.sum  + debt.allow
      
      m.acc = m.acc + debt.allow
      need  = need  - debt.allow
      debt.allow = 0
    }
    
    # Broker sells some or all of open positions to cover the call (uses margin to cover transaction cost = more loss)
    # Since Broker "chooses" we will assume the broker wants to keep the pair whole (same number of long/short shares)
    if(pt == "long-short")
    {
      total.cost = need + (need * pen)                    # Total cost + penalty for margin call 
      
      numshares  = ceiling(total.cost/(lp + sp))          # Number of shares to keep the strategy whole
      
      m.acc = m.acc - (p[2]*numshares) + (p[1]*numshares) # Decrease by short shares, increase by long shares
      
      need  = 0
      
      s = c((s[1]-numshares), (s[2]-numshares))           # Open position size (for long) closes
      
      new.caplist <- c(m.acc,c.acc,lp,sp,debt.sum)
      out         <- list(s, p, debt.core, new.caplist)
      return(out)
    }
    else
    {
      total.cost = need + (need * pen)                    # Total cost + penalty for margin call 
      
      numshares  = ceiling(total.cost/(lp + sp))          # Number of shares to keep the strategy whole
      
      m.acc = m.acc - (p[1]*numshares) + (p[2]*numshares) # Decrease by short shares, increase by long shares
      
      need  = 0
      
      s = c((s[1]-numshares), (s[2]-numshares))           # Open position size (for long) closes
      
      new.caplist <- c(m.acc,c.acc,lp,sp,debt.sum)
      out <- list(s, p, debt.core, new.caplist)
      return(out)
    }
  }
  
  # Set current Account Levels (All in current capital account)
  
  max.debt        = capital * max.borrow
  maxout.debt     = maxBorrowingBeforeClosingPositons
  empty.capital   = emptyCapitalBeforeClosingPositons
  
  reqs <- c(borrowAllow, empty.capital, maxout.debt)
  
  margin.acc = 0
  cap.acc    = capital
  debt.prin  = 0
  debt       = 0
  
  #a.name <- deparse(substitute(a))
  #b.name <- deparse(substitute(b))
  
  ### Strategy Log Data Frames (Returned for analysis)
  # Activity Log - 
  # FullSTAT Log - 
  
  track <- NULL ; track.names <- c("Period","Action","$_Return","%_Return", "Capital_Total","Debt")
  full  <- NULL ; full.names  <- c("Period","up.n","up.x","mean","r","down.x","down.n","cap.acc","mrg.acc","long","short","tot.cash")  
  
  # Extract Ratio, signal lines and stock prices
  
  r  = ts[[1]]
  mu = ts[[2]]
  
  a = ts[[3]]
  b = ts[[4]]
  
  un = sigs[[1]] ; ux = sigs[[2]]
  ln = sigs[[3]] ; lx = sigs[[4]]
  
  if(mu[1] == 0 || 
     un[1] == 0 || 
     ux[1] == 0 ||
     ln[1] == 0 || 
     lx[1] == 0  )
  {
    r  <- r[-1]
    mu <- mu[-1]
    a  <- a[-1]
    b  <- b[-1]
    un <- un[-1] ; ux <- ux[-1]
    ln <- ln[-1] ; lx <- lx[-1]
  }
  
  #print(class(un))
  #print(class(ln))
  #print(class(ux))
  #print(class(lx))
  
  
  # FOR UPPER BOUND - If ratio >= un line, sell num/buy denom - reverse if ratio <= ux line
  # FOR LOWER BOUND - If ratio <= ln line, buy num/sell denom - reverse if ratio <= ux line
  
  # Set Frequency of compunding debt account
  rate.inc = 0 
   
  if(compound == "d") { rate.inc = 1   }
  if(compound == "w") { rate.inc = 5   }
  if(compound == "m") { rate.inc = 20  }
  if(compound == "y") { rate.inc = 252 }
  
  # Set tracking vectors for position value chnages
  size   <- c()
  prce   <- c()
  profit = 0
  return = 0
  
  long = 0 ; short = 0
  chge = 0 ; chge.perc = 0
  
  # Set Margin Requirement Values
  margin.req.open = margin.open  + margin.safety
  margin.req.mv   = margin.maint + margin.safety
  
  # Lists to track actions taken at "i" where ratio = "r[i]" (for plotting)
  opens.i  <- c(); opens.r  <- c() # Position Opened
  closes.i <- c(); closes.r <- c() # Position Closed
  stops.i  <- c(); stops.r  <- c() # Position Stopped Out
  
  # Margin and debt trackers - Currently unused (Ploting will be messy)
  m.call.i <- c(); m.call.r <- c() # Margin Call (Successful)       - Position moved against trader
  m.back.i <- c(); m.back.r <- c() # Margin Reimbursement Withdrawn - Position moved for trader
  
  d.bor.i  <- c(); d.inc.r  <- c() # Trader borrowed 
  d.clr.i  <- c(); d.clr.r  <- c() # Trader cleared debt balance
  
  # Create Data Holding vectors
  margin.req = 0
  tot.cash   = margin.acc + cap.acc - short + long
  
  cap      <- c(margin.acc, cap.acc, long, short, debt)    # c(margin_account, capital_Account, MV_long, MV_short, debt_principle, debt_total)
  mv.stats <- c(long, short, margin.req, chge, chge.perc)  # c(long_MV, short_MV, margin_requirement, positionMV_change, %positionMV_change)
  
  # Set position indicators
  pos.open = FALSE
  pos.type = ""
  
  
  f.names <- c("C.$:","M.$","Lv","Sv","Total:")
  
  full <- rbind(full, c(0,un[1],ln[1],mu[1],r[1],ux[1],lx[1],cap.acc,margin.acc,long,short,tot.cash))
  if(printcp == TRUE) { print(noquote(paste("Pre-test values at",0,":",paste(paste(f.names,as.numeric(full[1,8:12])),collapse=", ")))) }
  
  #print(noquote(paste("Stop set at",stop.perc)))
  
  # Iterate over the length of the stock ratio (r)
  for(i in 1:length(r))
  {
    ### Calc period returns and check for stop losses and margin call first - then check signal cases (trading operations)
    ### ACCOUNTING - Calculate current market values, position value changes and current margin requirement
    
    mv.stats <- update.pos(pos.type, i, size, prce, a, b, margin.req.mv)
    
    #print(paste(mv.stats[1:5],collapse="+"))
    
    long       = mv.stats[1] ; margin.acc = cap[1]
    short      = mv.stats[2] ; cap.acc    = cap[2]
    margin.req = mv.stats[3] ; debt       = cap[5]
    chge       = mv.stats[4]
    chge.perc  = mv.stats[5]
    
    #print(margin.req)
    
    full <- rbind(full, c(i,un[i],ln[i],mu[i],r[i],ux[i],lx[i],cap.acc,margin.acc,long,short,tot.cash))
    if(printcp == TRUE) { print(noquote(paste("Current values at",i,":",paste(paste(f.names,as.numeric(full[(i+1),8:12])),collapse=", ")))) }
    
    ### CHECK STOPLOSS --> automatic sale so penalty for a margin call will automatically be applied
    if(chge.perc <= stop.perc)
    {
      # Close positions + take market (stop) losses + pay broker (if margin call was also activiated)
      if(margin.acc < margin.req)
      {
        results <- margin.call(margin.req, pos.type, cap, size, prce, debt.prin, max.debt, reqs, tc, margin.pen)
        size <- results[[1]] ; prce <- results[[2]] ; debt.prin = results[[3]]; cap <- results[[4]]
        
        mv.stats <- update.pos(pos.type, i, size, prce, a, b, margin.req.mv)
        
        long       = mv.stats[1] ; margin.acc = cap[1]
        short      = mv.stats[2] ; cap.acc    = cap[2]
        margin.req = mv.stats[3] ; debt       = cap[5]
        chge       = mv.stats[4] 
        chge.perc  = mv.stats[5]
        
        tot.cash = margin.acc + cap.acc - short + long
        
        if(print.md == TRUE) { print(noquote(paste("At",i,"Margin Call with..."))) }
        track <- rbind(track, c(i,"Margin Call +",chge,chge.perc,tot.cash,debt))
      }
      
      margin.acc = margin.acc - short - (tc*2)
      cap.acc    = cap.acc    + long  + margin.acc ; margin.acc <- 0
      long  = 0 ; short = 0
      cap   <- c(margin.acc, cap.acc, long, short, debt)
      
      profit = profit + chge   
      stops.i <- c(stops.i, i) ; stops.r <- c(stops.r, r[i]) 
      size <- c()              ; prce <- c()
      
      tot.cash = cap.acc + margin.acc - short + long
      
      if(pos.type == "long-short")
      {
        if(print == TRUE) { print(noquote(paste("At",i,"Long/Short Position Stopped Out"))) }
        track <- rbind(track, c(i,"Long/Short Stop",chge,chge.perc,tot.cash,debt))
        pos.open = FALSE
        pos.type = ""
        next
      }
      else
      {
        if(print == TRUE) { print(noquote(paste("At",i,"Short/Long Position Stopped Out"))) }
        track <- rbind(track, c(i,"Short/Long Stop",chge,chge.perc,tot.cash,debt))
        pos.open = FALSE
        pos.type = ""
        next
      }
    }
    
    ### Margin Withdrawal
    if(margin.acc > margin.req) 
    {
      #print(class(margin.acc)) ; print(class(margin.req))
      #print(margin.acc) ; print(margin.req)
      to.cap     = margin.acc - margin.req #; print(to.cap)
      cap.acc    = cap.acc    + to.cap
      margin.acc = margin.acc - to.cap ; to.cap = 0
      cap <- c(margin.acc, cap.acc, long, short, debt)
      
      tot.cash = margin.acc + cap.acc - short + long
      
      if(print.md == TRUE) { print(noquote(paste("At",i,"Margin Withdrawal"))) }
      track <- rbind(track, c(i,"Margin Withdrawal",chge,chge.perc,tot.cash,debt))
    }
    
    ### Margin Call
    if(margin.acc < margin.req)
    {
      # Deal with margin call
      results <- margin.call(margin.req, pos.type, cap, size, prce, debt.prin, max.debt, reqs, tc, margin.pen)
      size <- results[[1]] ; prce <- results[[2]] ; debt.prin <- results[[3]] ; cap <- results[[4]]
      
      # Update Current Market Position information
      mv.stats <- update.pos(pos.type, i, size, prce, a, b, margin.req.mv)
      
      long       = mv.stats[1] ; margin.acc = cap[1]
      short      = mv.stats[2] ; cap.acc    = cap[2]
      margin.req = mv.stats[3] ; debt       = cap[5]
      chge       = mv.stats[4]
      chge.perc  = mv.stats[5]
      
      tot.cash = margin.acc + cap.acc - short + long
      
      if(print.md == TRUE) { print(noquote(paste("At",i,"Margin Call"))) }
      track <- rbind(track, c(i,"Margin Call",chge,chge.perc,tot.cash,debt))
    }
    
    ### ACCOUNTING - Pay down debt and compunded interest if necessary
    if(debt > 0)
    {
      # Compound Interest if needed
      if(i %% rate.inc == 0)
      {
        debt = debt * (1+ir)
        
        if(print.md == TRUE) { print(noquote(paste("At",i,"Debt Compounded"))) }
        track <- rbind(track, c(i,"Debt Compounded","-","-","-",debt))
      }
      
      # If trader wants to pay debt off throughout the strategy, pay down by max capital % allowed
      if(debtPayPerDay == TRUE)
      {
        max.pay = cap.acc*capMaxPayDebt
        
        if(max.pay >= debt) # Pay off debt in full
        {
          to.pay  = debt
          
          debt      = debt - to.pay
          debt.prin = debt.prin - to.pay
          cap.acc = cap.acc - to.pay
        }
        else                # Pay off as much as possible (debt > 0)
        {
          to.pay = max.pay
          
          debt      = debt - max.pay
          debt.prin = debt.prin - max.pay
          cap.acc = cap.acc - max.pay
        }
        
        if(print.md == TRUE) { print(noquote(paste("At",i,"Debt Decreased"))) }
        track <- rbind(track, c(i,"Debt Decreased","-","-","-",debt))
        
      }
      
      cap <- c(margin.acc, cap.acc ,long ,short, debt)
    }
    
    ### CASE 1: Close final position if one is open at end of data (r)
    if(i == length(r) && pos.open == TRUE)
    {
      # Closing Capital/Margin Account updates
      
      margin.acc = margin.acc - short - (tc*2)
      cap.acc    = cap.acc    + long  + margin.acc ; margin.acc <- 0
      long = 0 ; short = 0
      cap  <- c(margin.acc, cap.acc, long, short, debt)
      
      profit = profit + chge     
      closes.i <- c(closes.i, i) ; closes.r <- c(closes.r, r[i]) 
      size <- c()                ; prce <- c()
      pos.open = FALSE           ; pos.type = ""
      
      tot.cash = cap.acc + margin.acc - short + long
      
      if(pos.type == "long-short")
      {
        if(print == TRUE) { print(noquote(paste("At",i,"Long/Short Position Closed"))) }
        track <- rbind(track, c(i,"Long/Short Close",chge,chge.perc,tot.cash,debt))
        next
      }
      else
      {
        if(print == TRUE) { print(noquote(paste("At",i,"Short/Long Position Closed"))) }
        track <- rbind(track, c(i,"Short/Long Close",chge,chge.perc,tot.cash,debt))
        next
      }
    }
    
    ### CASE 2: ratio <= low-open signal 
    if(r[i] <= ln[i]) 
    {
      # Event: Open a long pair (long a / short b) position
      if(pos.open == FALSE)
      {
        # Open a position (Determine trade size with margin capital limit (no risk of margin failure here))
        long.short.mult = cap.acc/(cap.acc+(cap.acc*margin.req.open))   # Capital allotment % long + short positions (optimized (max) split)
        
        c.used  = (cap.acc * long.short.mult) - (tc*2)                    # Capital allotments - transacion charge (Account for tc before buying)
        
        a.s = floor(c.used/a[i]) ; b.s = floor(c.used/b[i])            # Floor -> whole number of shares to trade
        size <- c(a.s, b.s)      ; prce <- c(a[i],b[i])              # Vector for tracking markt value change over iterations
        long = a.s*a[i]          ; short <- b.s*b[i]                 # Position Market Value at Open
        
        margin.need = short * margin.req.open                          # Total Capital Needed for margin
        
        margin.acc = margin.need + short                               # Margin Account increases by required short sale proceeds (covers long position)
        cap.acc    = cap.acc - margin.need - long - (tc*2)             # Capital Account decreases by long position, margin deposit and transaction costs (~ $0)
        
        cap <- c(margin.acc, cap.acc, long, short, debt)               # Capital tracking vector 
        
        tot.cash = cap.acc + margin.acc - short + long
        
        opens.i <- c(opens.i, i) ; opens.r <- c(opens.r, r[i]) 
        pos.open = TRUE          ; pos.type = "long-short"
        if(print == TRUE) { print(noquote(paste("At",i,"Long/Short Position Opened"))) }
        track <- rbind(track, c(i,"Long/Short Open","-","-",tot.cash,debt))
        next
      }
      
      # Event: Large 1 period price swing (issues casued by non-HTF trading)
      else if(pos.open == TRUE && pos.type == "short-long") 
      {
        # Closing Capital/Margin Account updates
        margin.acc = margin.acc - short - (tc*2)
        cap.acc    = cap.acc    + long  + margin.acc ; margin.acc <- 0
        long = 0 ; short = 0
        cap  <- c(margin.acc, cap.acc, long, short, debt)
        
        profit = profit + chge     
        closes.i <- c(closes.i, i) ; closes.r <- c(closes.r, r[i]) 
        size <- c()                ; prce <- c()
        pos.open = FALSE           ; pos.type = ""
        
        tot.cash = cap.acc + margin.acc - short + long
        
        if(print == TRUE) { print(noquote(paste("At",i,"Short/Long Position Closed"))) }
        track <- rbind(track, c(i,"Short/Long Close",chge,chge.perc,tot.cash,debt))
        next
      }
      
      # Event: Do nothing (Current Open position is still valid)
      else { next } 
    }
    else{}
    
    ### CASE 3: ratio >= high-open signal
    if(r[i] >= un[i])
    {
      # Event: Open a short pair (short a / long b) position
      if(pos.open == FALSE) 
      {
        # Open a position (Determine trade size with margin capital limit (no risk of margin failure here))
        long.short.mult = cap.acc/(cap.acc+(cap.acc*margin.req.open))  # Capital allotment % long + short positions (optimized (max) split)
        
        c.used  = (cap.acc * long.short.mult) - (tc*2)                    # Long  Capital allotment - transacion charge (Account for tc before buying)
                                                                          # Short Capital allotment - transacion charge 
        
        a.s = floor(c.used/a[i])   ; b.s = floor(c.used/b[i])       # Floor -> whole number of shares to trade
        size <- c(a.s, b.s)        ; prce <- c(a[i],b[i])              # Vector for tracking markt value change over iterations
        short = a.s*a[i]           ; long <- b.s*b[i]                  # Position Market Value at Open
        
        margin.need = short * margin.req.open                          # Total Capital Needed for margin
        
        margin.acc = margin.need + short                               # Margin Account increases by required short sale proceeds (covers long position)
        cap.acc    = cap.acc - margin.need - long - (tc*2)             # Capital Account decreases by long position, margin deposit and transaction costs (~ $0)
        
        cap <- c(margin.acc, cap.acc, long, short, debt)               # Capital tracking vector 
        
        tot.cash = cap.acc + margin.acc - short + long
        
        opens.i <- c(opens.i, i) ; opens.r <- c(opens.r, r[i]) 
        pos.open = TRUE          ; pos.type = "short-long"
        if(print == TRUE) { print(noquote(paste("At",i,"Short/Long Position Opened"))) }
        track <- rbind(track, c(i,"Short/Long Open","-","-",tot.cash,debt))
        next
      }
      
      # Event: Large 1 period price swing (issues casued by non-HTF trading)
      else if(pos.open == TRUE && pos.type == "long-short")
      {
        # Closing Capital/Margin Account updates
        margin.acc = margin.acc - short - (tc*2)
        cap.acc    = cap.acc    + long  + margin.acc ; margin.acc <- 0
        long = 0 ; short = 0
        cap  <- c(margin.acc, cap.acc, long, short, debt)
        
        profit = profit + chge     
        closes.i <- c(closes.i, i) ; closes.r <- c(closes.r, r[i]) 
        size <- c()                ; prce <- c()
        pos.open = FALSE           ; pos.type = ""
        
        tot.cash = cap.acc + margin.acc - short + long
        
        if(print == TRUE) { print(noquote(paste("At",i,"Long/Short Position Closed"))) }
        track <- rbind(track, c(i,"Long/Short Close",chge,chge.perc,tot.cash,debt))
        next
      }
      
      # Event: Do nothing (Current Open position is still valid)
      else { next } 
    }
    else{}
    
    ### CASE 4: Ratio is within normal range  (low-close signal >= ratio <= high-close signal) --> CLOSE
    if((r[i] >= lx[i]) && (r[i] <= ux[i])) 
    {
      # Event: Normality "restored" - Reverse trade
      if(pos.open == TRUE)
      {
        margin.acc = margin.acc - short - (tc*2)
        cap.acc    = cap.acc    + long  + margin.acc ; margin.acc <- 0
        long = 0 ; short = 0
        cap  <- c(margin.acc, cap.acc, long, short, debt)
        
        profit = profit + chge     
        closes.i <- c(closes.i, i) ; closes.r <- c(closes.r, r[i]) 
        size <- c()                ; prce <- c()
        pos.open = FALSE           ; pos.type = ""
        
        tot.cash = cap.acc + margin.acc - short + long
        
        if(pos.type == "long-short")
        {
          if(print == TRUE) { print(noquote(paste("At",i,"Long/Short Position Closed"))) }
          track <- rbind(track, c(i,"Long/Short Close",chge,chge.perc,tot.cash,debt))
          next
        }
        else
        {
          if(print == TRUE) { print(noquote(paste("At",i,"Short/Long Position Closed"))) }
          track <- rbind(track, c(i,"Short/Long Close",chge,chge.perc,tot.cash,debt))
          next
        }
      }
      else { }
    }
  }
  
  if(print == TRUE) { print(noquote("------------------- STRATEGY FINISHED ---------------------")) }
  
  replicate <- list(ts=list(r=r,mu=mu,a=a,b=b),sigs=list(un=un,ux=ux,ln=ln,lx=lx),param=list(stop=stop.perc,capital=capital,pname=plotname))
  
  if(class(track) == "list" || class(track) == "numeric" || is.null(track) || is.na(track) || ncol(track) <= 1)
  {
    profit = round(profit,2)
    
    return = ((cap[2]-capital)/capital)*100
    return = round(return, 2)
    
    if(print==TRUE) { print(noquote(paste("Bad strategy --> appears to have had no trades implemented or major error..."))) }
    if(print==TRUE) { print(noquote(paste("Strategy Profit   = $",profit)))   }
    if(print==TRUE) { print(noquote(paste("Strategy Return   =",return,"%"))) }
    
    out <- list(pr=return, pd=profit, trk=NULL, ftrk=NULL, torep=replicate, bag=extra)
    return(out)
  }
    
  # Plot return ratio with mean and standard deviation for position descions
  if(plot == TRUE)
  {
    title.r = title.t = ""
    
    if(is.null(plotname)) { title.r = "Raw Price Plot"             ; title.t = "Pairs Trading Visualization" }
    else                   { title.r = paste("Raw Prices:",plotname); title.t = paste("Pairs Trading:",plotname) }
    
    plot(r,   type="l", col="gray59",ylab="Stock Ratio",main=title.t)
    lines(mu, col ="blue" ,lty=2, lwd=2)
    lines(un, col ="forestgreen" ,lty=1, lwd=2)
    lines(ln, col ="forestgreen" ,lty=1, lwd=2)
    lines(ux, col ="purple"      ,lty=1, lwd=2)
    lines(lx, col ="purple"      ,lty=1, lwd=2)
    points(x = opens.i  , y = opens.r , pch = 20  , col = "green", cex = 1.5)
    points(x = closes.i , y = closes.r, pch = 20  , col = "blue" , cex = 1.5)
    points(x = stops.i  , y = stops.r , pch = "+" , col = "red"  , cex = 1.5)
  }
  
  # Create a replication tag for duplicating the models or plots after running
  
  colnames(track) <- track.names
  colnames(full)  <- full.names
  
  profit = round(profit,2)
  
  return = ((cap[2]-capital)/capital)*100
  return = round(return, 2)
  
  if(result == TRUE)
  {
    print(noquote(paste("Strategy Profit   = $",profit)))
    print(noquote(paste("Strategy Return   =",return,"%")))
  }
  
  out <- list(pr=return, pd=profit, trk=track, ftrk=full, torep=replicate, bag=extra)
  return(out)
}
