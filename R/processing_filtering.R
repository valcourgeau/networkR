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

#' Filters data with respect to given thresholds.
#' Multidimensional version.
#' @param data Matrix of data to be filtered.
#' @param thresholds Continuous threshold for increments.
#' @param diff_values Boolean to return differenciated values.
#' @param one_d Boolean it flag if input is one or multidimensional.
#' @returns Filtered data with respect to increments smaller than
#'     thresholds. Output as same size as data or shorter by one row
#'     if diff_values=TRUE.
#' @examples
#' data <- c(1,2,5,
#'           1,3,6,
#'           1,4,7)
#' data <- t(matrix(data, ncol=3))
#' DataFiltering(data, c(2,3,6))
#' DataFiltering(data, c(2,3,6), diff = T)
DataFiltering <- function(data, thresholds, diff_values=F, one_d=F){
  # Shall we have the first or second point when diff_values=F?
  filtered_data <- diff(data)

  if(one_d){
    N <- length(data)
    indicator_data <- (abs(filtered_data) <= thresholds)
    if(diff_values){
      return(filtered_data * indicator_data)
    }else{
      return(data[-N,] * indicator_data)
    }
  }else{
    N <- length(data[,1])
    under_thres <- t(apply(abs(filtered_data),
                           MARGIN = 1, '<=', thresholds))
    if(diff_values){
      return(filtered_data*under_thres)
    }else{
      return(data[-N,] * under_thres)
    }
  }
}

#' Creates a time increments matrix with the correct
#' number of columns.
#'
#' @param times vector of timestamps.
#' @param ncol number of columns, i.e. dimensions.
#' @returns Matrix of time increments with ncol columns.
#' @examples
#' times <- c(1,2,4,7,11)
#' TimeMatrix(times, ncol = 2)
TimeMatrix <- function(times, ncol){
  if(ncol <= 0){
    stop('ncol should be positive.')
  }
  # creating increments
  times <- diff(times)

  if(one_d == 1){
    return(times)
  }else{
    return(t(matrix(rep(times, each=ncol), nrow=ncol)))
  }
}






