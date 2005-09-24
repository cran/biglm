bigglm.RODBC<-function(formula, data, family = gaussian(),
                       tablename, ..., chunksize=5000){
  terms<-terms(formula)
  modelvars<-all.vars(formula)	
  dots<-as.list(substitute(list(...)))[-1]
  dotvars<-unlist(lapply(dots,all.vars))
  vars<-unique(c(modelvars,dotvars))
  tablerow<-sqlQuery(data, paste("select * from ",tablename," limit 1"))
  tablevars<-vars[vars %in% names(tablerow)]
  query<-paste("select ",paste(tablevars,collapse=", ")," from ",tablename)
  result<-odbcQuery(data, query)
  got<-0
  chunk<-function(reset=FALSE){
    if(reset){
      if(got>0){
        result<<-odbcQuery(data,query)
        got<<-0
      }
      return(TRUE)
    }
    rval<-sqlGetResults(data,max=chunksize)	
    got<<-got+NROW(rval)
    if (!is.data.frame(rval) || NROW(rval)==0) 
      return(NULL)
    return(rval)
  }
  rval<-bigglm(formula, data=chunk, family=family, ...)
  rval$call<-sys.call()
  rval$call[[1]]<-as.name(.Generic)
  rval
}

		
