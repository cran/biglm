"coef.biglm" <-
function(object,...){
    if (!object$qr$checked)
       object$qr<-biglm:::singcheck.bigqr(object$qr)
    rval<-coef.bigqr(object$qr)
    rval[object$qr$D==0]<-NA
    names(rval)<-object$names
    rval
}

