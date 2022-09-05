#' WOTPLY
#'
#' @param list_transition_matrices List of transition matrices. Each matrix contains the transition probailities from the clusters at time t (on the columns) towards the clusters at time t+1 (on the rows).
#' The matrices can be obtain from funtion \emph{get_transition_matrix}
#' @param selected_stages Vector with the name of the clusters related to the latest time point
#' for which we want to know the connection to clusters at previous time points.
#' @param cluster_label Vector with the cluster information for all the cells from all time points. The length is equal to the one of \emph{time_label}
#' @param time_label  Vector with time information for all the cells from all time points. The length is equal to the one of \emph{cluster_label}
#' @param legend_time Vector with time information with length equal to the number of time points.
#' @param customize_color  Character vector with the name of the colour for each cluster (node) in each time point.
#' @param top_link Integer.Maximum number of links to select between clusters at time t and clusters at time t+1. Links are sorted according to the weigth and then only the \emph{top_link} are kept. If \emph{NULL} (default), all the links are kept.
#' @description A ggnet2 plot is generated showing the connections between \emph{selected_stages} from the latest time point and the clusters from previous time points . The number of columns is equal to the numbers of time points. In each column, the cluster of the
#' corresponding time point is shown as network node. The weight of the links between clusters at time points t and t+1 refelect the weigth of the
#' transition probabilities from \emph{list_transition_matrices}.
#' @return A ggnet2 plot
#' @seealso \url{https://cran.r-project.org/web/packages/GGally/index.html}
#' @author Gabriele Lubatti \email{gabriele.lubatti@@helmholtz-muenchen.de}
#'
#'
#'
#' @export WOTPLY
#'
#'
#'@importFrom utils read.csv
#'@importFrom grDevices hcl


WOTPLY <- function(list_transition_matrices, selected_stages, cluster_label, time_label, legend_time, customize_color, top_link = NULL){

  list_transition_matrices_top <- rep(list(0), length(list_transition_matrices))
  for (i in seq(length(list_transition_matrices), 1)) {
    if (i == length(list_transition_matrices)) {
      transition_matrix_small <- list_transition_matrices[[i]][row.names(list_transition_matrices[[i]]) %in%
                                                                 selected_stages, ]
      transition_matrix_top <- select_top_weights(transition_matrix_small,
                                                  top_link)
      mean_col <- apply(transition_matrix_top, 2, mean)
      list_transition_matrices_top[[i]] <- transition_matrix_top[,
                                                                 mean_col != 0]
    }
    if (i != length(list_transition_matrices)) {
      transition_matrix_small <- list_transition_matrices[[i]][colnames(list_transition_matrices_top[[i +
                                                                                                        1]]), ]
      transition_matrix_top <- select_top_weights(transition_matrix_small,
                                                  top_link)
      mean_col <- apply(transition_matrix_top, 2, mean)
      list_transition_matrices_top[[i]] <- transition_matrix_top[,
                                                                 mean_col != 0]
      if (class(list_transition_matrices_top[[i]]) != "data.frame"){
        list_transition_matrices_top[[i]] <- as.data.frame(transition_matrix_top[,
                                                                                 mean_col != 0])
        colnames(list_transition_matrices_top[[i]]) <- colnames(transition_matrix_top)[mean_col != 0]
        row.names(list_transition_matrices_top[[i]]) <- row.names(transition_matrix_top)
        warning(paste0("From time ",legend_time[i]," to ",legend_time[i+1]," is only possible to select less than ", top_link, " connections"))
      }




    }
  }
  map_color <- data.frame(levels(factor(cluster_label)), customize_color)
  colnames(map_color) <- c("label", "color")
  row.names(map_color) <- map_color$label
  list_color <- rep(list(0), length(list_transition_matrices) +
                      1)
  list_time <- rep(list(0), length(list_transition_matrices) +
                     1)
  list_label <- rep(list(0), length(list_transition_matrices) +
                      1)
  for (i in seq(length(list_color), 1)) {
    if (i != length(list_color)) {
      list_color[[i]] <- map_color[colnames(list_transition_matrices_top[[i]]),
                                   2]
    }
    if (i == length(list_color)) {
      list_color[[i]] <- map_color[row.names(list_transition_matrices_top[[i -
                                                                             1]]), 2]
    }
  }
  for (i in seq(length(list_time), 1)) {
    if (i != length(list_time)) {
      list_time[[i]] <- rep(legend_time[i], length(colnames(list_transition_matrices_top[[i]])))
    }
    if (i == length(list_time)) {
      list_time[[i]] <- rep(legend_time[i], length(row.names(list_transition_matrices_top[[i -
                                                                                             1]])))
    }
  }
  for (i in seq(length(list_label), 1)) {
    if (i != length(list_label)) {
      list_label[[i]] <- colnames(list_transition_matrices_top[[i]])
    }
    if (i == length(list_label)) {
      list_label[[i]] <- row.names(list_transition_matrices_top[[i -
                                                                   1]])
    }
  }
  color_all_small <- unlist(list_color)
  day_all_small <- unlist(list_time)
  time_all_small <- unlist(list_label)
  unisco_all <- rep(list(0), length(list_time))
  for (i in 1:length(unisco_all)) {
    unisco_all[[i]] <- rep(0, length(day_all_small))
    if (i != 1) {
      unisco_all[[i]][day_all_small == legend_time[i -
                                                     1]] <- 0.1
    }
  }
  unisco_all_complete <- rep(list(0), length(list_time))
  for (i in 1:length(unisco_all_complete)) {
    unisco_all_complete[[i]] <- rep(list(0), sum(day_all_small ==
                                                   legend_time[i]))
  }
  for (i in 1:length(unisco_all_complete)) {
    time_point <- legend_time[i]
    for (j in 1:sum(day_all_small == time_point)) {
      unisco_all_complete[[i]][[j]] <- unisco_all[[i]]
      if (i != 1) {
        unisco_all_complete[[i]][[j]][unisco_all[[i]] ==
                                        0.1] <- as.numeric(list_transition_matrices_top[[i -
                                                                                           1]][j, ])
      }
    }
  }
  provo <- data.frame(unisco_all_complete)
  bip <- network::network(as.matrix(provo))
  bip <- network::network(as.matrix(provo), directed = TRUE)
  GGally::ggnet2(bip, color = "mode")
  coordinate_x <- rep(list(0), length(legend_time))
  step <- 3
  end <- step * length(coordinate_x)
  position_x <- seq(step, end, by = step)
  for (i in 1:length(coordinate_x)) {
    coordinate_x[[i]] <- rep(position_x[[i]], sum(day_all_small ==
                                                    legend_time[i]))
  }
  day_x_all <- unlist(coordinate_x)
  coordinate_y <- rep(list(0), length(legend_time))
  for (i in 1:length(coordinate_y)) {
    coordinate_y[[i]] <- seq(1, 10, length.out = sum(day_all_small ==
                                                       legend_time[i]))
  }
  day_y_all <- unlist(coordinate_y)
  all_valori <- c(day_x_all, day_y_all)
  matrix_position <- matrix(all_valori, nrow = length(colnames(provo)),
                            ncol = 2)
  pesi_final <- rep(list(0), length(list_transition_matrices_top))
  for (i in 1:length(pesi_final)) {
    pesi_final[[i]] <- as.numeric(as.matrix(list_transition_matrices_top[[i]]))[as.numeric(as.matrix(list_transition_matrices_top[[i]])) !=
                                                                                  0]
  }
  pesi_new_small <- unlist(pesi_final)
  bip <- network::network(as.matrix(provo), directed = TRUE)
  GGally::ggnet2(bip, mode = matrix_position, size = 4, color = color_all_small,
                 label = time_all_small, edge.size = pesi_new_small, arrow.size = 12,
                 arrow.gap = 0.025, label.size = 3)
  }







#' select_top_weights
#' @inheritParams WOTPLY
#' @inheritParams convert_names
#' @return A matrix
#' @author Gabriele Lubatti \email{gabriele.lubatti@@helmholtz-muenchen.de}
#'
#'
#'
#' @export select_top_weights
#'

select_top_weights <- function(transition_matrix, top_link = NULL){

  if (is.null(top_link)){
    pesi_fine <- transition_matrix
  }

  if(!is.null(top_link)){
    important_value <- sort(as.matrix(transition_matrix), decreasing = T)[1:top_link]



    pesi_fine <- matrix(0,nrow = nrow(transition_matrix), ncol(transition_matrix))
    pesi_fine <- transition_matrix
    pesi_fine[pesi_fine < important_value[top_link]] <- 0}
  return(pesi_fine)
}



#' gg_color_hue
#' @noRd
gg_color_hue = function (n)
{
  hues <- seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

