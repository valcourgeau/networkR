source("R/utils/packages_loader.R")


#' Ensures matrix has non-zero entries set to one.
#' @param nw_topo Matrix (adjacency or laplacian).
#' @return Matrix with non-zero entries set to 1.
#' @examples
#' nw_topo <- c(2,1,1,
#'              0,2,3,
#'              0,0,-1)
#' nw_topo <- matrix(nw_topo, ncol = 3, nrow = 3)
#' EnsureTopo(nw_topo = nw_topo)
#'
#' test_topo <- matrix(1, ncol=3, nrow=3)
#' EnsureTopo(nw_topo = test_topo)
EnsureTopo <- function(nw_topo){
  if(!class(nw_topo) != "matrix"){
    stop('nw_topo should be a matrix.')
  }

  nw_topo[nw_topo != 0] <- 1
  return(nw_topo)
}


#' Row-standardise the given matrix with zero diagonal.
#' @param nw_topo Matrix to standardise (adjacency or laplacian).
#' @examples
#' nw_topo <- c(2,1,1,
#'              0,2,3,
#'              0,0,1)
#' nw_topo <- matrix(nw_topo, ncol = 3, nrow = 3)
#' StdTopo(nw_topo = nw_topo)
#'
#' test_topo <- matrix(1, ncol=3, nrow=3)
#' StdTopo(nw_topo = test_topo)
StdTopo <- function(nw_topo){
  nw_topo <- EnsureTopo(nw_topo = nw_topo)
  diag(nw_topo) <- 0
  std_coeff <- rowSums(nw_topo)
  std_coeff[std_coeff == 0] <- 1

  return(nw_topo/std_coeff)
}

# Example

