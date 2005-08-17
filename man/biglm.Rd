\name{biglm}
\alias{biglm}
\alias{update.biglm}
\alias{coef.biglm}
\alias{vcov.biglm}
\alias{print.biglm}
\alias{summary.biglm}
\alias{print.summary.biglm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Bounded memory linear regression }
\description{
 \code{biglm} creates a linear model object that uses only \code{p^2}
memory for \code{p} variables. It can be updated with more data using
\code{update}. This allows linear regression on data sets larger than
memory.
 }
\usage{
biglm(formula, data, weights=NULL, sandwich=FALSE)
\method{update}{biglm}(object, moredata,...)
\method{vcov}{biglm}(object,...)
\method{coef}{biglm}(object,...)
\method{summary}{biglm}(object,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{formula}{A model formula}
  \item{weights}{A one-sided, single term formula specifying weights}
  \item{sandwich}{\code{TRUE} to compute the Huber/White sandwich
    covariance matrix (uses \code{p^4} memory rather than \code{p^2})}
  \item{object}{A \code{biglm} object}
  \item{data}{Data frame that must contain all variables in
    \code{formula} and \code{weights}}
  \item{moredata}{Additional data to add to the model}
  \item{...}{Additional arguments for future expansion}
}
\details{
  The model formula must not contain any data-dependent terms, as these
  will not be consistent when updated.  Factors are permitted, but the
  levels of the factor must be the same across all data chunks (empty
  factor levels are ok).
}
\value{
  An object of class \code{biglm}
}
\references{Algorithm AS274  Applied Statistics (1992) Vol.41, 
No. 2 }

\seealso{lm}
\examples{
data(trees)
ff<-log(Volume)~log(Girth)+log(Height)

chunk1<-trees[1:10,]
chunk2<-trees[11:20,]
chunk3<-trees[21:31,]

a <- biglm(ff,chunk1)
a <- update(a,chunk2)
a <- update(a,chunk3)

summary(a)
}
\keyword{regression}% at least one, from doc/KEYWORDS
