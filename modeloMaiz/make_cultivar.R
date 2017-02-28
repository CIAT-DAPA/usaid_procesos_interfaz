## Codigo para generar archivo .CUL de DSSAT v 4.6

## MZCER046.CUL
## parametros P1    P2    P5    G2    G3 PHINT    AX   ALL

## Prueba

# param <- list()
# 
# param$VAR_NAME <- "PIO CALIBRATED"
# param$ECO <- "IB0001"
# param$P1 <- 250.6
# param$P2 <- 0.500
# param$P5 <- 900
# param$G2 <- 529.7
# param$G3 <- 15.81
# param$PHINT <- 54.90
# 
# file_name <- 'MZCER046.CUL'
# 
# make_CUL(param, file_name)

make_CUL <- function(param, file_name){
  
  VAR_NAME <- param$VAR_NAME
  ECO <- param$ECO
  P1 <- param$P1
  P2 <- param$P2
  P5 <- param$P5
  G2 <- param$G2
  G3 <- param$G3
  PHINT <- param$PHINT
  
  sink(file_name, append = F)

  cat('@VAR#  VRNAME.......... EXPNO   ECO#    P1    P2    P5    G2    G3 PHINT    AX   ALL',  sep = "\n")
  cat(sprintf("%6s %16-s %5s %6s %5.1f %5.3f %5.1f %3.1f %2.2f %2.2f",
              "CI0027", VAR_NAME, ".",  ECO, P1, P2, P5, G2, G3, PHINT))

  sink()
  
  
}


