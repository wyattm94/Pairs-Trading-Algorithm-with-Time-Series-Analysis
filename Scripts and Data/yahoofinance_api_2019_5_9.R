# Yahoo Finance data API Library 
# Author      : Wyatt Marciniak
# Maintainer  : Wyatt Marciniak [wyatt.marciniak@gmail.com] 
# Description : Web scraping API (Source: Yahoo Finance)
# License     : See LICENSE file - [Unauthorized use Prohibited]

# library(rvest)
# library(httr)
# library(htmltools)
# library(xlsx)

## Handle date conversions (unix timestamps <--> character dates)
# (1) date --> unix
# (2) unix --> date
yahoo_calc_date   <- function(d){
  # Yahoo origin date + Second per day 
  origin = '1970-01-01'
  s.d    = 60*60*24
  # Convert (POSIXlt) string dates to numerical dates (seconds from origin)
  ov  = as.POSIXlt(paste(origin,'12:00:00'))
  dv  = as.POSIXlt(paste(d,'12:00:00'))
  # Calculate diffence in seconds (days -> seconds/total days)
  s.diff = as.numeric(dv-ov)*s.d
  s.diff = s.diff + (5*60*60) # Calculation is off by 5 hours (adjust - unsure why)
  return(s.diff)
}
yahoo.format.date <- function(d){
  # Convert dates from numerical (POSIX) to sring for row index
  dates = unlist(lapply(d,
                 function(x) as.character(
                   as.POSIXct(x,origin='1970-01-01 00:00:00'))
                 )) 
  return(dates)
}

## Data extractors (targetted - based on current structure)
# (1) price data (OHLC + Volume + Adjusted)
# (2) dividend data (payments + dates + ratios)
# (3) stock split data (time + split + ratio)
extract.price     <- function(x){
  # Get and Format Dates
  dt = yahoo.format.date(unlist(x$timestamp))
  # Get and Format Price Data (NOTE: as.character() deals NA values (+supress))
  dv = lapply(x$indicators$quote[[1]], 
              function(x) suppressWarnings(as.numeric(unlist(as.character(x))))) 
  # Get and Format Adjusted Close (if it exists)
  if((!is.null(x$indicators$adjclose)))
  {
    #return(x$indicators$adjclose)
    da = unlist(lapply(x$indicators$adjclose[[1]]$adjclose, function(x){
      if(is.null(x))
        return(NA)
      else
        return(suppressWarnings(as.numeric(unlist(as.character(x)))))
    }))
    # Create + Return DF WITH adjusted close data
    df = data.frame(dt,dv$open,dv$high,dv$low,dv$close,dv$volume,da)
    colnames(df) = c('date','open','high','low','close','volume','adjusted')
    df = df[order(df$date),]
    return(df)
  }
  # Create + Return DF WITHOUT adjusted close data
  else
  {
    df = data.frame(dt,dv$open,dv$high,dv$low,dv$close,dv$volume)
    colnames(df) = c('date','open','high','low','close','volume')
    df = df[order(df$date),]
    return(df)
  }
}
extract.div       <- function(x){
  # Extract Dividend data - If it doesn't exist (NULL) return NA
  dr = x$events$dividends
  if(is.null(dr))
    return(NA)
  # Dividends exist - Create DF for dividends
  dd = dv = c()
  for(i in 1:length(dr)){
    dd = c(dd,dr[[i]]$date)
    dv = c(dv,dr[[i]]$amount)
  }
  dd = yahoo.format.date(dd)
  dv = as.numeric(dv)
  df = data.frame(dd,dv,stringsAsFactors=F)
  colnames(df) = c('date','div')
  df = df[order(df$date),]
  return(df)
}
extract.split     <- function(x){
  # Extract Stock Split data - If it doesn't exist (NULL) - Return NA
  sr = x$events$splits
  if(is.null(sr))
    return(NA)
  # Exists - Create a detailed split DF (Split amount + Value)
  sd = sn = sdn = sf = c()
  for(i in 1:length(sr))
  {
    sd  = c(sd,sr[[i]]$date)
    sn  = c(sn,sr[[i]]$numerator)
    sdn = c(sdn,sr[[i]]$denominator)
    sf  = c(sf,sr[[i]]$splitRatio)
  }
  sd  = yahoo.format.date(sd)
  sn  = as.numeric(sn)
  sdn = as.numeric(sdn) 
  df = data.frame(sd,sn,sdn,sf,stringsAsFactors=F)
  colnames(df) = c('date','num','denom','ratio')
  df = df[order(df$date),]
  return(df)
}

## Main functionalities
# (1) Main Fetching Wrapper (scraping operation)
# (2) (WIP) Creates a multi-asset (type and ticker) data set 
fetch.yahoo       <- function(ticker=NA,type='all',freq='d',from=NA,to=NA){
  # (1) Error Checking - Ticker Input + type and freq
  if(is.na(ticker)){
    cat('(!) ERROR: Ticker empty\n')
    return(NA)
  }
  good.type = c('all','price','div','split')
  good.freq = c('d','w','m')
  if(!(type %in% good.type)){
    cat('(!) ERROR: Bad type input (use: all, price, div, split)\n')
    return(NA)
  }
  if(!(freq %in% good.freq)){
    cat('(!) ERROR: Bad freq input (use: d, w, m)\n')
    return(NA)
  }
  freq = switch(freq,'d'='1d','w'='1wk','m'='1mo')
  
  # (2) Check from and to date range + Adjust
  if(is.na(from)){
    from = '1970-01-01'
  }
  
  if(is.na(to)){
    to = as.character(Sys.Date())
  }
  
  t0 = yahoo_calc_date(from)
  t1 = yahoo_calc_date(to) 
  
  # (3) Check if ticker is a FX input
  fx.flag = F
  if(grepl('=X',ticker)){
    fx.flag = T
  }
  
  # (4) Create Fetching (Call - GET) URL
  base0 = paste0('https://query1.finance.yahoo.com/v8/finance/chart/',ticker)
  base1 = '?&lang=en-US&region=US&period1='
  base2 = '&period2='
  base3 = '&interval='
  end   = '&events=div%7Csplit&corsDomain=finance.yahoo.com'
  url   = paste0(base0,base1,t0,base2,t1,base3,freq,end)
  
  # (5) Fetch Data (raw request) then extract data sets as requested by user
  resp  = try(content(GET(url))[[1]]$result[[1]],silent=T)
  if(class(resp)=='try-error'){
    return(NA)
  }
  if(fx.flag)
    return(extract.price(resp))
  else{
    ## All data (price/div/split)
    if(type=='all'){
      return(list(price=extract.price(resp),
                  div=extract.div(resp),
                  split=extract.split(resp)))
    }
    else if (type=='price'){
      return(extract.price(resp))
    }
    else if (type=='div'){
      return(extract.div(resp))
    }
    else{
      return(extract.split(resp))
    }
  }
}
yahoo.dataset     <- function(fp.in='dataset_map',fp.out='dataset_final',overwrite=F,
                              iso='adjusted',freq='d',from=NA,to=NA,minsize=NA,
                              omit.na=T,trace=F,ss=1,local=F){
  # Complete file path extensions
  fp.in   = paste0(fp.in,'.xlsx')
  fp.save = paste0(fp.out,'.Rdata') 
  fp.out  = paste0(fp.out,'.csv') 
  
  # Check if map file exists
  if(!(file.exists(fp.in))){
    cat('(!) ERROR: File not found\n')
    return(NA)
  }
  
  # Check if output file exists (if so, end unless 'overwrite' = T (default = F))
  if(file.exists(fp.out)){
    if(!overwrite){
      cat('(!) ERROR: Output File already exists (use overwrite or change name)\n')
      return(NA)
    }
    else
      tracer(cat('(!) WARNING: Output File found + overwrite = T (overwriting)\n'))
  }
  
  # Ensure sleep time is AT LEASET 1 second for slowing webscraper 
  ss = max(1,ss)
  
  # Setup Operational Variables
  sheets       = excel_sheets(fp.in)
  desc = NA
  ds_summary   = list()
  df = NA
  df_flag = F
  df_cn = c()
  
  # Execute 
  for(s in sheets){
    tracer(cat('----------\n> Current Sheet:',s,'\n'))
    ## Current Sheet
    cs    = read.xlsx(fp.in,sheetName=s,header=T,stringsAsFactors=F)
    ## Extract Tickers for data fetching
    ticks = cs$ticker
    temp  = list()
    for(i in 1:length(ticks)){
      Sys.sleep(ss); minreq=NA
      tracer(cat('  - Ticker:',format(ticks[i],justify='right',width=5),'\t'))
      
      fetch = fetch.yahoo(ticks[i],type='price',freq=freq,from=from,to=to)
      
      ## Check for valid fetch (NA returned for some 'try-error')
      if(class(fetch)!='data.frame'){
        tracer(cat('- (!) Error: Fetch failed \n'))
        temp = lappend(temp,list(asset=cs$asset[i],class=cs$class[i],name=cs$name[i],
                                 ticker=ticks[i],description=desc,data=NA,reqsize=NA))
        next
      }
      
      ## Omit NA values is specified (default = T)
      if(omit.na){
        fetch = na.omit(fetch)
      }
      
      ## Flag for min.size requirement (T = include)
      if(!is.na(minsize)){
        minreq = nrow(fetch) >= minsize
      }
      
      ## Update DF (Final Data Set)
      if(minreq | is.na(minsize)){
        df_cn = c(df_cn,ticks[i])
        ## First time inserting data into DF
        if(!df_flag){
          df = data.frame(fetch[,c('date',iso)])
          df_flag=T
        }
        else{
          ## Merge afterwards (by date)
          df = merge(df,fetch[,c('date',iso)],by='date')
        }
      }
      
      ## Get asset descriptions (Only ETFs supported currently)
      # if(cs$asset[i]=='etf')
      #   desc = etf.description(ticks[i])
      
      ## Update 'temp' (Used for summary list - Major)
      temp = lappend(temp,list(asset=cs$asset[i],class=cs$class[i],name=cs$name[i],
                               ticker=ticks[i],description=desc,data=fetch,
                               reqsize=minreq))
      
      tracer(cat('- (*) Valid: Required Size? (',minreq,') \n',sep=''))
      # tracer(cat('    [Description]:',desc,'\n'))
    }
    ## Apply names to data list 'temp' + append to summary list
    names(temp) = ticks[1:i]
    ds_summary = lappend(ds_summary,temp)
  }
  # Add names to summary list + Adjust DF (Row/col names)
  names(ds_summary) = sheets
  
  rownames(df) = df[,1]
  df = df[,-1]
  colnames(df) = df_cn
  
  # Output list (data structure)
  yahoo_ds = list(fulldata=ds_summary,dataset=df)
  
  write.csv(df,file=fp.out)
  save(yahoo_ds,file=fp.save)
  
  tracer(cat('\n----------\n(*) Data Set Written to:',fp.out,
             '\n(*) Summary (Full Data List) Saved to:',fp.save,'\n'))
  
  if(local){
    tracer(cat('> Local = T : Returning results locally\n'))
    return(yahoo_ds)
  }
}


# qq = xb2list('amzn_optest1.xlsx')
# dft2 = yahoo.ops.read('amzn_optest1.xlsx')

# a = as.character(curr.dt()['tag'])
# a
## Options Data utiities
# (1) Main wrapper + scraper
# (2) Used to read xlsx books into a named list structure
yahoo.options     <- function(ticker,
                              expr0=as.character(Sys.Date()),
                              expr1=as.character(Sys.Date()+(365*2)),
                              atm_range=0.05,local=F,trace=F,pbar=T){
  
  # [1] Deal with multiple ticker input -> loop w/recursion
  if(length(ticker) > 1){
    chains = list()
    for(i in 1:length(ticker)){
      tracer(cat('Security [',qfmt(i,0,ndigs(length(ticker))),'of',
                 qfmt(length(ticker),0,ndigs(length(ticker))),'] '))
      chains = 
        lappend(chains,yahoo.options(ticker[i],expr0=expr0,expr1=expr1,
                                     atm_range=atm_range,local=local,
                                     trace=trace,pbar=pbar))
      tracer('\n-----\n(!) System waiting (Spider forced Sleep)...')
      Sys.sleep(5)
      tracer(' Done\n-----\n')
    }
    names(chains) = ticker
    tracer(cat('(*) All securities processed'))
    if(local) return(chains)
    else return(NA)
  }
  
  # [2-3] Scraper Setup and Fetch operation --> Create date/time tag at fetch
  tracer(cat('(*) Target:',toupper(ticker),'\n-------------------\n'))
  tracer(cat('> Parsing request URL...'))
  ticker = toupper(ticker)
  base   = sprintf('https://finance.yahoo.com/quote/%s/options?p=%s',
                   ticker,ticker)
  t0     = yahoo_calc_date(expr0)
  t1     = yahoo_calc_date(expr1)
  tracer(cat('  \t(*) Complete\n'))
  
  tracer(cat('> Fetching response...'))
  get = content(GET(base))
  dt_tag = as.character(curr.dt()['tag']) # yymmdd_hhmmss
  tracer(cat('  \t(*) Complete\n'))
  
  ## [3.1] - Clean ticker for '^' character (Indxes) + Parse file name
  fn_ticker = if.else(any(grepl('\\^',tolower(ticker))),
                      gsub('\\^','',tolower(ticker)),
                      tolower(ticker))
  fn = paste0(getwd(),'/',fn_ticker,'_options_',dt_tag,'.xlsx')
  
  # [4] Extract Stock Price Details - From SAME PAGE/TIME as options data 
  out = list()
  tracer(cat('> Extracting price data...'))
  pi = get %>% html_nodes(css='#quote-header-info') %>% 
    html_nodes(css='span') %>% html_text()
  cp = conv.s2n(pi[4])
  cc = strsplit(pi[5],' ')
  cc_price = as.numeric(unlist(cc)[1])
  cc_perct = conv.s2n(gsub('\\)','',gsub('\\(','',unlist(cc)[2])))
  ct = pi[6]
  
  out = list(ticker=ticker,price=cp,chg_price=cc_price,chg_perct=cc_perct,
             time=ct,systime=as.character(Sys.time()),dt_tag=dt_tag)
  
  ## [4.1] Write to excel (.xlsx)
  summ.df = data.frame(id=names(out),val=unlist(out))
  write.xlsx(x=summ.df,file=fn,sheetName='Summary',row.names=F,col.names=T)
  
  pn  = names(out)
  tracer(cat('  \t(*) Complete\n'))
  
  # [5] Extract by Expiration Dates subset by time parameters [expr0:expr1]
  ## [5.1] Extract Dates in range --> parse urls
  tracer(cat('> Extracting maturities:\n'))
  tracer(cat(' > Range: [',expr0,' : ',expr1,']',sep=''))
  res   = get %>% html_nodes(css='#Main') %>% 
    html_nodes(css='#Col1-1-OptionContracts-Proxy')
  dates = res %>% html_nodes(css='select') %>% html_nodes(css='option')
  d_tag = dates %>% html_text()
  d_num = as.numeric(dates %>% html_attr('value'))
  d_loc = d_num >= t0 & d_num <= t1
  
  d_tag = d_tag[d_loc]
  d_num = d_num[d_loc]
  d_url = paste0(base,'&date=',d_num)
  tracer(cat('  \t(*) Complete\n'))
  
  tracer(cat('----------------------------------------------------------------------\n'))
  tracer(cat('> Extracting option chain data for (',length(d_url),') maturities...\n',sep=''))
  
  if(pbar){
    pb = txtProgressBar(min=0,max=length(d_url),style=3)
  } 
  
  ## [5.2] Extract Call/Put option trees --> Rbind into 1 DF
  for(i in 1:length(d_url)){
    page = content(GET(d_url[i]))
    
    call = numdata.cleaner(((page %>% html_nodes(css='section'))[[2]] %>% 
                              html_nodes(css='table') %>% 
                              html_table())[[1]],seq(3,11))
    
    put  = numdata.cleaner(((page %>% html_nodes(css='section'))[[3]] %>% 
                              html_nodes(css='table') 
                            %>% html_table())[[1]],seq(3,11))
    
    call$type      = 'C'
    call$itm       = call$Strike <= cp
    colnames(call) = unlist(lapply(colnames(call),function(x) gsub(' ','_',tolower(x))))
    call           = call[,c('type',colnames(call)[which(colnames(call)!='type')])]
    colnames(call)[which(colnames(call)=='%_change')] = 'perc_change'
    
    put$type       = 'P'
    put$itm        = put$Strike >= cp
    colnames(put)  = unlist(lapply(colnames(put),function(x) gsub(' ','_',tolower(x))))
    put            = put[,c('type',colnames(put)[which(colnames(put)!='type')])]
    colnames(put)[which(colnames(put)=='%_change')] = 'perc_change'
    
    full_chain = data.frame(rbind(call,put),stringsAsFactors=F)
    matdate = unlist(strsplit(yahoo.format.date(d_num[i]),' '))[1]
    
    ## [5.2.1] Write maturity date data (by sheet)
    write.xlsx(x=full_chain,file=fn,
               sheetName=paste0(fn_ticker,'_options_',matdate),
               row.names=F,col.names=T,append=T)
    
    out[[(i+length(out))]] = full_chain
    
    tracer(cat('  + Chain (',qfmt(i,0,ndigs(length(d_url))),') for: ',matdate,
               ' (*) retrieved [ ',qfmt((i/length(d_url))*100),'% ]\n',sep=''))
    
    if(pbar) { 
      tracer(cat('\n'))
      setTxtProgressBar(pb,i)
    }
    
    Sys.sleep(1) # Sleep to slow down spider
  }
  if(pbar) close(pb)
  
  tracer(cat('  \t(*) Complete\n-----\n(*) [Operation Complete]\n----\n\n'))
  
  names(out) = c(pn,unlist(lapply(d_num,function(x) 
    unlist(strsplit(yahoo.format.date(x),' '))[1])))
  
  if(local)
    return(out)
}

# testop = yahoo.options(c('amzn','spy','^vix'),
#                        expr0=as.character(Sys.Date()),
#                        expr1=as.character(Sys.Date()+(31*3)),
#                        export=T,local=T,fnl=paste0(c('amzn','spy','vix'),'_optest1'),overwrite=F,
#                        trace=T,pbar=F)

yahoo.ops.read <- function(fp,trace=F){
  tracer(cat('  + retrieving... '))
  tbl = xb2list(fp,trace=trace)
  names(tbl) = c(names(tbl)[1],
                 unlist(lapply(names(tbl)[-1],
                               function(x) 
                                 unlist(strsplit(x,'options_'))[2])))
  tracer(cat('  + data cleaned\n'))
  return(tbl)
}


## Estimates and Calendars
# (1) Revenue and Earnings Estimates
# (2) Event calendars (Earnings, Splits, IPOs and Economic Data Releases)
yahoo.estimates   <- function(ticker){
  url = paste('https://finance.yahoo.com/quote/',ticker,'/analysis?p=',ticker,sep='')
  eps = data.frame(url %>% read_html() %>% html_nodes(css='table:nth-child(2)') %>% html_table())
  rev = data.frame(url %>% read_html() %>% html_nodes(css='table:nth-child(3)') %>% html_table())
  return(list(eps=eps,rev=rev))
}
yahoo.calendar    <- function(data=c('earnings','splits','ipo','economic'),
                           start=as.character(Sys.Date()),
                           end=as.character(Sys.Date()+90)){
  print(noquote(paste0('> Calendar Dates: ',start,' - ',end)))
  print(noquote(paste0('> Data Types: ',paste(data,collapse=','))))
  print(noquote('----------------------------------------------------------'))
  out  = list()
  base = 'https://finance.yahoo.com/calendar/'
  dseq = as.character(seq(as.Date(start),as.Date(end),by='day'))
  
  for(i in 1:length(dseq)){
    print(noquote(paste0('> Date: ',dseq[i])))
    urls     = unlist(lapply(data,function(x) paste0(base,x,'?day=',dseq[i])))
    #return(urls)
    out[[i]] = lapply(urls,function(x){
      tag = unlist(strsplit(x,'/'))
      tag = unlist(strsplit(tag[length(tag)],'?day=',fixed=TRUE))[1]
      
      get = suppressWarnings(try(content(GET(x)),silent=TRUE))
      if(class(get)[[1]]=='try-error')    { print(noquote(paste0('  - ',tag,': ',0))); return(NULL) }
      
      numres = suppressWarnings(try(get %>% html_nodes(css='#fin-cal-table') %>% html_nodes(css='h3') %>% html_nodes(css='span') %>% html_text(),silent=TRUE))
      if(class(numres)[[1]]=='try-error' | is.null(numres) | length(numres)==0) { print(noquote(paste0('  - ',tag,': ',0))); return(NULL) }
      
      numres = unlist(strsplit(numres[length(numres)],' '))
      nrcurr = as.numeric(unlist(strsplit(numres[1],'-'))[2])
      nrtot  = as.numeric(numres[3])
      addpgs = ceiling((nrtot-nrcurr)/100) # additional pages (&offset=0&size=100)
      tbl    = data.frame((get %>% html_nodes(css='#fin-cal-table') %>% html_nodes(css='table') %>% html_table())[[1]],stringsAsFactors=FALSE)
      
      if(addpgs>=1){
        for(a in 1:addpgs){
          tempu = paste0(x,'&offset=',(a*100),'&size=100')
          tempg = content(GET(tempu))
          tempt = data.frame((tempg %>% html_nodes(css='#fin-cal-table') %>% html_nodes(css='table') %>% html_table())[[1]],stringsAsFactors=FALSE)
          tbl   = rbind(tbl,tempt)
        }
      }
      print(noquote(paste0('  - ',tag,': ',nrow(tbl))))
      return(tbl)
    })
    names(out[[i]]) = data
  }
  names(out) = dseq
  return(out)
}



### VOID (WIP Utilities)
## Dertermine Valid Inraday Sets - **Maintainer use only
yahoo.det.intraday <- function(t,fdir=getwd(),fname='yahoo_data_combos.txt'){
  ## Determine Intraday Data range/interval pairs (i.e. 1month (1mo) = 2m (2minutes))
  path    = paste0(fdir,'/',fname); if(file.exists(path)) { file.remove(path) }
  results = list(); tags = c(); ind = 1
  
  range   = c('1d','5d','1mo','3mo','6mo','ytd','1y','2y','5y','10y') # Max has incorrect results
  intv    = c('1m','2m','5m','15m','30m','60m','90m')
  
  for(i in 1:length(range))
  {
    curr = c(range[i])
    for(j in 1:length(intv))
    {
      url = sprintf(
        'https://query1.finance.yahoo.com/v8/finance/chart/%s?region=US&lang=en-US&includePrePost=false&interval=%s&range=%s&corsDomain=finance.yahoo.com&.tsrc=finance',
        t,intv[j],range[i])
      
      #return(url)
      
      x = GET(url)
      s = as.numeric(status_code(x))
      
      print(noquote(paste('Result:',s,'-',paste0(range[i],'/',intv[j]))))
      
      if(s==200) { curr = c(curr,intv[j]); c = yahoo.extract.data(content(x),intv[j]) }
      else       { c = NULL }
      
      tags = c(tags,paste0(range[i],'/',intv[j]))
      results[[ind]] = list(code=s,range=range[i],interval=intv[j],data=c,url=url); ind = ind + 1
      
      Sys.sleep(2)
    }
    line = paste(curr,collapse=','); write(line,file=path,append=TRUE)
  }
  names(results) = tags
  return(results)
}

## Developing (Alterations for intraday - older designs being reassessed)
fetch.yahoo2       <- function(ticker=NA,type='all',freq='d',from=NA,to=NA){
  # (1) Error Checking - Ticker Input + type and freq
  if(is.na(ticker)){
    cat('(!) ERROR: Ticker empty')
    return(NA)
  }
  good.type = c('all','price','div','split')
  good.freq = c('d','w','m')
  if(!(type %in% good.type)){
    cat('(!) ERROR: Bad type input (use: all, price, div, split)')
    return(NA)
  }
  if(!(freq %in% good.freq)){
    cat('(!) ERROR: Bad freq input (use: d, w, m)')
    return(NA)
  }
  
  # (2) Check from and to date range + Adjust
  if(is.na(from))
    from = '1970-01-01'
  if(is.na(to))
    to = as.character(Sys.Date()+1)
  t0 = yahoo_calc_date(from)
  t1 = yahoo_calc_date(to) 
  
  # (3) Check if ticker is a FX input
  fx.flag = F
  if(grepl('=X',ticker))
    fx.flag = T
  
  # (4) Create Fetching (Call - GET) URL
  ur1 = sprintf('%s?&lang=en-US&region=US&period1=%s&period2=%s&interval=%s%s',
                base,t0,t1,freq,'&events=div%7Csplit&corsDomain=finance.yahoo.com')
  # print(url)
  
  # (5) Fetch Data (raw request) then extract data sets as requested by user
  if(fx.flag){
    ## FX data will only have price data (OHLC + Adjusted, no volume or div/splits)
    resp  = content(GET(url))[[1]]$result[[1]]
    data = extract.price(resp)
    return(data)
  }
  else{
    resp = content(GET(url))[[1]]$result[[1]]
    ## All data (price/div/split)
    if(type=='all')
      return(list(price=extract.price(resp),
                  div=extract.div(resp),
                  split=extract.split(resp)))
    else if (type=='price')
      return(extract.price(resp))
    else if (type=='div')
      return(extract.div(resp))
    else
      return(extract.split(resp))
  }
}

yahoo.data2        <- function(ticker=NULL,type='all',freq='d',intraday=FALSE,range=NULL,
                              from='1970-01-01',to=as.character(Sys.Date())){
  ## Set primary values
  base = 'https://query1.finance.yahoo.com/v8/finance/chart/'
  url  = ''
  fx.flag = FALSE
  
  ## Adjust ticker input to upper case + Check for input Errors
  bad.text = c('.','_','-','>','<','!','@','#','$','%','^','&','*','(',')',',','?')
  if(is.null(ticker)){ print(noquote('Error: Please insert a valid ticker symbol (NULL supplied)')); return(NULL) }
  ticker = toupper(ticker)
  
  if(intraday==TRUE){
    
    if(is.null(range)){ print(noquote('Error: Please insert a valid date range (NULL supplied)')); return(NULL) }
    
    good.ranges = c('1d','5d','1mo','3mo','6mo','ytd','1y','2y')
    good.freq   = c('1m','2m','5m','15m','30m','60m','90m')
    
    if(!(range %in% good.ranges)) { print(noquote('Error: range parameter invalid... (try 1d,5d,1mo,3mo,6mo,ytd,1y,2y)')); return(NULL) }
    if(!(freq %in% good.freq))    { print(noquote('Error: freq parameter invalid... (try 1m,2m,5m,15m,30m,60m,90m)'))    ; return(NULL) }
    
  }else{
    good.freq = c('d','w','m')
    if(!(freq %in% good.freq))    { print(noquote('Error: freq parameter invalid... (try d,w,m)'))     ; return(NULL) }
    
    freq = switch(freq,'d'='1d','w'='1wk','m'='1mo')
  }
  
  ## Calculate proper url parsing from time range inputs
  t0 = yahoo_calc_date(from)
  t1 = yahoo_calc_date(to)
  
  ## Determine FX or Security
  if(grepl('/',ticker) | grepl('=X',ticker)){ fx.flag = TRUE }
  
  ## Create base URL (formatting base + symbol input)
  if(fx.flag==TRUE){
    # Calculate Base
    if(grepl('/',ticker)==TRUE){
      t = unlist(strsplit(ticker,'/'))
      if(t[1]=='USD'){
        base=paste0(base,t[2],'=X')
      }else{
        base=paste0(base,t[1],t[2],'=X')
      }
    }else{
      base=paste0(base,ticker)
    }
  }else{
    base = paste0(base,ticker)
  }
  
  ### Parse Final URL + Pull Data (calls to appropriate cleaning functions then returns result)
  ## <1> (Intraday Data - Date/Price Only)
  if(intraday==TRUE){
    url = sprintf('%s?region=US&lang=en-US&includePrePost=false&interval=%s&range=%s&corsDomain=finance.yahoo.com&.tsrc=finance',
                  base,freq,range)
    # (FX Data)
    if(fx.flag==TRUE){
      get  = content(GET(url))[[1]]$result[[1]]
      data = extract.price(get,TRUE)
      # return(data)
      # (Market Data - Includes Range/Freq error catching)
    }else{
      get = GET(url)
      if(as.numeric(status_code(get)==200)) { data = extract.price(content(get)$chart$result[[1]],TRUE); return(data) }
      else { print(noquote('Freq is invalid for selected range')); return(NULL) }
    }
    ## <2> (LT Market Data - daily/weekly/monthly)
  }else{
    u1  = sprintf('%s?&lang=en-US&region=US&period1=%s&period2=%s&interval=%s',base,t0,t1,freq)
    url = paste0(u1,'&events=div%7Csplit&corsDomain=finance.yahoo.com') 
    print(url)
    # (FX Data - Only date/price)
    if(fx.flag==TRUE){
      get  = content(GET(url))[[1]]$result[[1]]
      #return(get)
      #data = extract.price(get$chart$result[[1]],TRUE)
      # return(get)
      data = extract.price(get)
      return(data)
      # Market Data
    }else{
      res = content(GET(url))[[1]]$result[[1]]
      # All data (price/div/split)
      if(type=='all')
      {
        out = list()
        out$price = extract.price(res)
        out$div   = extract.div(res)
        out$split = extract.split(res)
        return(out)
      } 
      # Specific Data Focus
      else if (type=='price') { return(extract.price(res)) } 
      else if (type=='div')   { return(extract.div(res))   } 
      else                    { return(extract.split(res)) }
    }
  }
}






## WIP Scratch...
# yu = 'https://finance.yahoo.com/screener/unsaved/c86ac240-bfd7-4443-811e-1aec955b305a'
# yg = content(GET(yu))
# 
# yahoo.extract.screen <- function(url,include_criteria=TRUE) # Can also save active criteria names/values for reference 
# {
#   
# }
# 
# # Active Criteria
# sc = yg %>% html_nodes(css='#screener-criteria')
# 
# # Result Parsing
# sr = ((yg %>% html_nodes(css='#screener-results') %>% html_nodes(css='#fin-scr-res-table'))[[1]] %>% html_nodes(css='div'))[[1]] %>% html_nodes(css='span') %>% html_text()
# print(sr)



#html_nodes(css='#scr-res-table') %>% html_nodes(css='table') %>% html_table()






