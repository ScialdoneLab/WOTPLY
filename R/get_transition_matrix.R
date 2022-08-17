
#' get_transition_matrix
#'
#' @param path Character string with the path to the folder with the oputput of the function compute_all_transport_maps from pyhton package WOT.
#' @param cluster_t Vector with cluster assignment for cells at time t.
#' The length is equal to the length of \emph{cells_t}.
#' @param level_t_plus Vector with the name of the clusters at time t+1.
#' @param threshold  Numeric value. Only entry of the transition matrix with weight eqaul or above \emph{threshold} are kept.
#' @param cells_t  Character vector with the name of cells at time t for which we want to obtain the transition matrix.
#' @description The output of \emph{compute_all_transport_maps} from pyhton package \emph{WOT} is a matrix. Each entry (i,j) describes the
#' transition probability of cell i at time t towards cluster j at time t+1. From this matrix,
#' the average of the transition probability for all the cells at time t belonging to the same cluster is computed. Finally only the entries of the resulting matrix with above
#' \emph{threshold} are kept. The row names of the final matrix are equal to \emph{level_t_plus}, while the column names are eqaul to
#' the levels of \emph{cluster_t}.
#' @return A matrix with row names equal to \emph{level_t_plus} and column names eqaul to
#' the levels of \emph{cluster_t}.
#' @seealso \url{https://broadinstitute.github.io/wot/}
#' @author Gabriele Lubatti \email{gabriele.lubatti@@helmholtz-muenchen.de}
#'
#'
#'
#' @export get_transition_matrix
#'

get_transition_matrix <- function(path, cluster_t, level_t_plus, threshold, cells_t){


  cluster_t <- factor(cluster_t)
  setwd(path)
  fate_matrix_8 <- read.csv("X.csv",header =F)
  setwd(path)
  col_names <- read.csv("var.csv")
  col_names <- row.names(col_names)
  setwd(path)
  row_names <- read.csv("obs.csv")
  row_names <- as.vector(row_names$X)
  row.names(fate_matrix_8) <- row_names
  colnames(fate_matrix_8) <- col_names

  mean_next <- rep(list(0),length(levels(cluster_t)))
  for ( i in 1:length(levels(cluster_t))){
    fate_small <- fate_matrix_8[cells_t,]
    fate_small <- fate_small[cluster_t==levels(cluster_t)[i],]
    fate_small <- fate_small[,colnames(fate_small)!="Other"]
    mean_day_8 <- apply(fate_small,2,mean)
    mean_next[[i]] <- mean_day_8
    names(mean_next[[i]]) <- c(level_t_plus)
  }

  next_8 <- data.frame(mean_next)
  colnames(next_8) <- levels(cluster_t)

  next_8[next_8<threshold] <-0


  return(next_8)

}




#' convert_names
#'
#' @param new_row Vector with the new row names to assign to \emph{transition_matrix}
#' @param new_col Vector with the new column names to assign to \emph{transition_matrix}
#' @param transition_matrix Output from \emph{get_transition_matrix}.

#' @return A matrix with row names equal to \emph{new_row} and column names eqaul to
#' \emph{new_col}.
#' @author Gabriele Lubatti \email{gabriele.lubatti@@helmholtz-muenchen.de}
#'
#'
#'
#' @export convert_names
#'

convert_names <- function(new_row, new_col, transition_matrix){
  convert_row <- data.frame(row.names(transition_matrix), new_row)

  convert_col <- data.frame(colnames(transition_matrix), new_col)


  row.names(transition_matrix) <- convert_row[,2]
  colnames(transition_matrix) <- convert_col[,2]
  return(transition_matrix)
}






