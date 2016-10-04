
df_dplyr <- 
  #get xts as data.frame to take advantage of new features
  data.frame("date"=index(french_factors_xts), french_factors_xts) %>%
  gather(ff_factor,roc,-date) %.%
  group_by( ff_factor )  %.%
  do(
    data.frame(
      date = .$date[seq(1,nrow(.)-199,by=1)],
      omega = rollapply( as.numeric(.$roc) , Omega, width=200, by=1)
    )
  )


wapply <- function(x, width, by = NULL, FUN = NULL, ...)
{
  FUN <- match.fun(FUN)
  if (is.null(by)) {
    by <- width  
  }
  lenX <- length(x)
  SEQ1 <- seq(1, lenX - width + 1, by = by)
  SEQ2 <- lapply(SEQ1, 
                 function(x) {
                   x:(x + width - 1)
                 })
 
  OUT <- lapply(SEQ2, function(a) FUN(x[a], ...))
  OUT <- base:::simplify2array(OUT, higher = TRUE)
  return(OUT)
}


minute’,‘hourly’, ‘daily’,‘weekly’, ‘monthly’,‘quarterly’, and ‘yearly


rollapply.xts <- function(
  data, width, FUN, ..., by=1, by.column=TRUE,
  fill=if(na.pad) NA, na.pad=TRUE, partial=TRUE,
  align=c("right","center","left")
) {

  nr <- NROW(data)
  nc <- NCOL(data)
  width <- as.integer(width)[1]
  stopifnot( width > 0, width <= nr )

  ## process alignment
  align <- match.arg(align)
  n1 <- switch(
    align,    
    "left" = { width - 1},
    "center" = { floor(width/2) },
    "right" = { 0 }
  )
  #
  idx <- index(data)
  tt <- index(data)[seq((width-n1), (nr-n1), by)]
  #
  FUN <- match.fun(FUN)
  ind <- as.matrix(seq.int(width,nr,by))
  if( nc == 1 ) {
    xx <- sapply(ind, function(i) FUN(.subset_xts(data,(i-width+1):i),...))
    if(!is.null(dim(xx))) xx <- t(xx)
    res <- xts(xx, tt, if (by == 1) attr(data, "frequency"))
  } else if( by.column ) {
    res <- xts(sapply( 1:NCOL(data), function(j)
               #apply(e, 1, function(i) FUN(data[i,j],...)) ),
               #apply(ind, 1, function(i) FUN(data[(i-width+1):i,j],...)) ),
               apply(ind, 1, function(i) FUN(.subset_xts(data,(i-width+1):i,j),...)) ),
            tt, if (by == 1) attr(data, "frequency") )
  } else {
    #xx <- apply(e, 1, function(i) FUN(data[i,],...))
    ##xx <- apply(ind, 1, function(i) FUN(data[(i-width+1):i,],...))
    xx <- apply(ind, 1, function(i) FUN(.subset_xts(data,(i-width+1):i),...))
    if(!is.null(dim(xx))) xx <- t(xx)
    res <- xts(xx, tt, if (by == 1) attr(data, "frequency"))
  }
  
  ix <- index(data) %in% index(res)
  tmp <- merge(res, xts(,idx, attr(data, "frequency")))
  if(is.null(colnames(res))) {
    colnames(tmp) <- colnames(res)
  }
  res <- na.fill(tmp, fill, ix)

  if( by.column && !is.null(dim(data)) ) {
    colnames(res) <- colnames(data)
  }
  return(res)
} 