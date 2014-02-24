\name{extract.info}
\alias{extract.info}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
extract.info(raw)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{raw}{
%%     ~~Describe \code{raw} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (raw) 
{
    info <- NULL
    for (i in 1:length(raw)) {
        xlstab <- raw[[i]]
        n <- sum(xlstab[, 1] == "ID MUSEU", na.rm = T)
        info.t <- matrix(NA, n, 12, dimnames = list(c(), xlstab[1:12, 
            1]))
        info.t[, 1] <- as.character(xlstab[1, 2])
        info.t[, 2] <- as.character(xlstab[2, 2])
        for (j in 3:12) info.t[, j] <- as.character(xlstab[which(xlstab == 
            as.character(xlstab[j, 1])), 2])
        info <- rbind(info, info.t)
    }
    return(as.data.frame(info))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
