######funções para pegar todos os arquivos########

ImportAll <- function(files){
  xlstabs <- vector("list", length=length(files))
  names(xlstabs) <- files
  for (i in 1:length(files)) xlstabs[[i]] <- read.csv(files[i], header=F, as.is = T)
  return(raw_tabs=xlstabs)
}

ExtractInfo <- function(raw){
  info <- NULL
  for (i in 1:length(raw)){
    xlstab <- raw[[i]]
    n <- sum(xlstab[,1]=="ID MUSEU", na.rm=T)
    info.t <- matrix(NA, n, 12, dimnames=list(c(), xlstab[1:12, 1]))
    info.t[,1] <- as.character(xlstab[1, 2])
    info.t[,2] <- as.character(xlstab[2, 2])
    for (j in 3:12) info.t[,j] <- as.character(xlstab[which(xlstab==as.character(xlstab[j, 1])), 2])
    info <- rbind(info, info.t)
  }
  return(as.data.frame(info))
}

AzExtract <- function(raw, info, landmarks){

  a_midline_names <- landmarks$a$midline
  a_sides_names <- paste(rep(landmarks$a$side, 2), rep(c("E", "D"), each=length(landmarks$a$side)), sep="_")

  z_midline_names <- landmarks$z$midline
  z_sides_names <- paste(rep(landmarks$z$side, 2), rep(c("D", "E"), each=length(landmarks$z$side)), sep="_")

  A <- array(NA, dim=c(sum(length(landmarks$a$midline), length(landmarks$a$side)*2), 3, dim(info)[1], 2), dimnames=list( c(a_midline_names, a_sides_names), c("x", "y", "z"), paste(info[,1], info[,4]), c("rep1", "rep2")))
  Z <- array(NA, dim=c(sum(length(landmarks$z$midline), length(landmarks$z$side)*2), 3, dim(info)[1], 2), dimnames=list( c(z_midline_names, z_sides_names), c("x", "y", "z"), paste(info[,1], info[,4]), c("rep1", "rep2")))

  index <- 0
  for(i in 1:length(raw)){
    n      <- sum(raw[[i]][,1]=="ID MUSEU", na.rm=T)
    index  <- (1+tail(index, 1)):(tail(index, 1)+n)
    inicio <- which(raw[[i]][,1]=="SUBSPECIES")+1
    Ai     <- which(raw[[i]][,1]=="A")+1
    Zi     <- which(raw[[i]][,1]=="Z")+1
    fim    <- c(inicio[-1]-2, length(raw[[i]][,1]))

    for(j in 1:n) {
      a <- as.matrix(raw[[i]][Ai[j]:(Zi[j]-3), c(1, 2:4, 6:8)])
      z <- as.matrix(raw[[i]][Zi[j]:(fim[j]) , c(1, 2:4, 6:8)])

      a.midline <- subset(a[,], a[,1] %in% landmarks$a$midline)
      a.side    <- subset(a[,], a[,1] %in% landmarks$a$side)

      z.midline <- subset(z[,], z[,1] %in% landmarks$z$midline)
      z.side    <- subset(z[,], z[,1] %in% landmarks$z$side)

      names <- a.midline[,1]
      a.midline <- apply(a.midline[,-1], 2, as.numeric)
      row.names(a.midline) <- names
      names <- paste(a.side[,1], rep(c("E", "D"), each=dim(a.side)[1]/2), sep="_")
      a.side <- apply(a.side[,-1], 2, as.numeric)
      row.names(a.side) <- names

      names <- z.midline[,1]
      z.midline <- apply(z.midline[,-1], 2, as.numeric)
      row.names(z.midline) <- names
      names <- paste(z.side[,1], rep(c("D", "E"), each=dim(z.side)[1]/2), sep="_")
      z.side <- apply(z.side[,-1], 2, as.numeric)
      row.names(z.side) <- names

      A[c(a_midline_names, a_sides_names), , index[j], 1] <- rbind(a.midline, a.side) [c(a_midline_names, a_sides_names), 1:3]
      A[c(a_midline_names, a_sides_names), , index[j], 2] <- rbind(a.midline, a.side) [c(a_midline_names, a_sides_names), 4:6]

      Z[c(z_midline_names, z_sides_names), , index[j], 1] <- rbind(z.midline, z.side) [c(z_midline_names, z_sides_names), 1:3]
      Z[c(z_midline_names, z_sides_names), , index[j], 2] <- rbind(z.midline, z.side) [c(z_midline_names, z_sides_names), 4:6]

    }
  }
  list(A=A, Z=Z)
}
