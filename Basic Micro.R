AvgProd <- function(Q,L){
  AP <- Q/L
  return(AP)
} 

MrgProd <- function(deltaQ , deltaL){
  MP <- deltaQ/deltaL
  return(MP)
}

Profit <- function(TR=NULL,TC=NULL,AR=NULL,AC=NULL,Q=NULL)suppressWarnings({
  if (c(is.null(TR), is.null(TC))){
    P <- (AR-AC)*Q
    return(P)
  }
  else if (c(is.null(AR),is.null(AC),is.null(Q))){
     P <- TR-TC
  return(P)}
})

TotRev <- function(P ,Q){
  TR <- P*Q
  return(TR)
}

TotCost <- function(TFC=NULL , TVC=NULL, AC=NULL, Q=NULL)suppressWarnings({
  if (c(is.null(TFC), is.null(TVC))){
    TC <- AC*Q
    return(TC)
  }
  else if (c(is.null(AC),is.null(Q))){
    TC <- TFC+TVC
    return(TC)}
})

AvgCost <- function(TC , Q){
  AC <- TC/Q
  return(AC)
}

AvgFCost <- function(TFC , Q){
  AFC <- TFC/Q
  return(AFC)
}

AvgVCost <- function(TVC , Q){
  AVC <- TVC/Q
  return(AVC)
}

AvgRev <- function(TR , Q){
  AR <- TR/Q
  return(AR)
}

MrgRev <- function(deltaTR , deltaQ){
  MR <- deltaTR/deltaQ
  return(MR)
}

MrgCost <- function(deltaTC , deltaQ){
  MC <- deltaTC/deltaQ
  return(MC)
}

ProfitMaxQ <- function(deltaTR, deltaTC, deltaQ){
  MR <- deltaTR/deltaQ
  MC <- deltaTC/deltaQ
  Q <- MC=MR
  return(Q)
}

