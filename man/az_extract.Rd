\name{az_extract}
\alias{az_extract}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
az_extract(raw, info, landmarks)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{raw}{
%%     ~~Describe \code{raw} here~~
}
  \item{info}{
%%     ~~Describe \code{info} here~~
}
  \item{landmarks}{
%%     ~~Describe \code{landmarks} here~~
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
function (raw, info, landmarks) 
{
    a_midline_names <- landmarks$a$midline
    a_sides_names <- paste(rep(landmarks$a$side, 2), rep(c("E", 
        "D"), each = length(landmarks$a$side)), sep = "_")
    z_midline_names <- landmarks$z$midline
    z_sides_names <- paste(rep(landmarks$z$side, 2), rep(c("D", 
        "E"), each = length(landmarks$z$side)), sep = "_")
    A <- array(NA, dim = c(sum(length(landmarks$a$midline), length(landmarks$a$side) * 
        2), 3, dim(info)[1], 2), dimnames = list(c(a_midline_names, 
        a_sides_names), c("x", "y", "z"), paste(info[, 1], info[, 
        4]), c("rep1", "rep2")))
    Z <- array(NA, dim = c(sum(length(landmarks$z$midline), length(landmarks$z$side) * 
        2), 3, dim(info)[1], 2), dimnames = list(c(z_midline_names, 
        z_sides_names), c("x", "y", "z"), paste(info[, 1], info[, 
        4]), c("rep1", "rep2")))
    index <- 0
    for (i in 1:length(raw)) {
        n <- sum(raw[[i]][, 1] == "ID MUSEU", na.rm = T)
        index <- (1 + tail(index, 1)):(tail(index, 1) + n)
        inicio <- which(raw[[i]][, 1] == "SUBSPECIES") + 1
        Ai <- which(raw[[i]][, 1] == "A") + 1
        Zi <- which(raw[[i]][, 1] == "Z") + 1
        fim <- c(inicio[-1] - 2, length(raw[[i]][, 1]))
        for (j in 1:n) {
            a <- as.matrix(raw[[i]][Ai[j]:(Zi[j] - 3), c(1, 2:4, 
                6:8)])
            z <- as.matrix(raw[[i]][Zi[j]:(fim[j]), c(1, 2:4, 
                6:8)])
            a.midline <- subset(a[, ], a[, 1] \%in\% landmarks$a$midline)
            a.side <- subset(a[, ], a[, 1] \%in\% landmarks$a$side)
            z.midline <- subset(z[, ], z[, 1] \%in\% landmarks$z$midline)
            z.side <- subset(z[, ], z[, 1] \%in\% landmarks$z$side)
            names <- a.midline[, 1]
            a.midline <- apply(a.midline[, -1], 2, as.numeric)
            row.names(a.midline) <- names
            names <- paste(a.side[, 1], rep(c("E", "D"), each = dim(a.side)[1]/2), 
                sep = "_")
            a.side <- apply(a.side[, -1], 2, as.numeric)
            row.names(a.side) <- names
            names <- z.midline[, 1]
            z.midline <- apply(z.midline[, -1], 2, as.numeric)
            row.names(z.midline) <- names
            names <- paste(z.side[, 1], rep(c("D", "E"), each = dim(z.side)[1]/2), 
                sep = "_")
            z.side <- apply(z.side[, -1], 2, as.numeric)
            row.names(z.side) <- names
            A[c(a_midline_names, a_sides_names), , index[j], 
                1] <- rbind(a.midline, a.side)[c(a_midline_names, 
                a_sides_names), 1:3]
            A[c(a_midline_names, a_sides_names), , index[j], 
                2] <- rbind(a.midline, a.side)[c(a_midline_names, 
                a_sides_names), 4:6]
            Z[c(z_midline_names, z_sides_names), , index[j], 
                1] <- rbind(z.midline, z.side)[c(z_midline_names, 
                z_sides_names), 1:3]
            Z[c(z_midline_names, z_sides_names), , index[j], 
                2] <- rbind(z.midline, z.side)[c(z_midline_names, 
                z_sides_names), 4:6]
        }
    }
    list(A = A, Z = Z)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
