# WOTPLY
WOTPLY (Waddington-OT analysis PLot) is an R package that shows the connections between selected clusters from the latest time point and the clusters from all the previous time points. The transition matrices between time point t and t+1 are obtained from Waddington-OT analysis (python package [wot](https://broadinstitute.github.io/wot/)).

## Installation

You can install the development version from [GitHub](https://github.com/) with:

```r
devtools::install_github("ScialdoneLab/WOTPLY",auth_token="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",ref="main")
```

## Getting started 
The main function of the package is  **WOTPLY**


### WOTPLY

```r
WOTPLY(list_transition_matrices, selected_stages, cluster_label, time_label, legend_time, customize_color, top_link = NULL)

```
requires as input:

1. **list_transition_matrices**: List of transition matrices. Each matrix contains the transition probailities from the clusters at time t (on the columns) towards the clusters at time t+1 (on the rows). The matrices can be obtain from funtion **get_transition_matrix**
2. **selected_stages**: Vector with the name of the clusters related to the latest time point for which we want to know the connection to clusters at previous time points.
3. **cluster_label** Vector with the cluster information for all the cells from all time points. The length is equal to the one of **time_label**
4. **time_label** Vector with time information for all the cells from all time points. The length is equal to the one of **cluster_label**
5. **legend_time** Vector with time information with length equal to the number of time points. 
6. **customize_color** Character vector with the name of the colours for each cluster (node) in each time point. 
7. **top_link** Integer.Maximum number of links to select between clusters at time t and clusters at time t+1. Links are sorted according to the weigth and then only the **top_link** are kept. If **NULL** (default), all the links are kept.

A ggnet2 plot is generated showing the connections between **selected_stages** from the latest time point and the clusters from previous time points . The number of columns is equal to the numbers of time points. In each column, the cluster of the
corresponding time point is shown as network node. The weight of the links between clusters at time points t and t+1 refelect the weigth of the
transition probabilities from **list_transition_matrices**.

## Example 
Below an example of input using the development version of **WOTPLY** from GitHub
The analysis performed on a single cell RNA seq dataset from a time-course of iPS reprogramming. The data are from day 10, day 12, day 14, day 16 and day 18 from [Schiebinger et al. 2019](https://www.cell.com/cell/fulltext/S0092-8674(19)30039-X).
Below the transition matrices bewteen time points t and t+1 are loaded. Each matrix is the output of **get_transition_matrix** and was built starting from **compute_all_transport_maps** function from pyhton package [wot](https://broadinstitute.github.io/wot/)). See ?**get_transition_matrix** for more info.

```r
load(system.file("extdata", "cluster_label.Rda", package = "WOTPLY"))
load(system.file("extdata", "time_label.Rda", package = "WOTPLY"))

load(system.file("extdata", "transition_matrix_3.Rda", package = "WOTPLY"))
load(system.file("extdata", "transition_matrix_4.Rda", package = "WOTPLY"))
load(system.file("extdata", "transition_matrix_6.Rda", package = "WOTPLY"))
load(system.file("extdata", "transition_matrix_8.Rda", package = "WOTPLY"))

```

```r
list_transition_matrices <- list(transition_matrix_3, transition_matrix_4, transition_matrix_6, transition_matrix_8)
selected_stages <- c(3,6,7)
top_link <- 3

legend_time <- c("day2", "day3", "day4", "day6", "day8")

customize_color <- c("#808080", "#6495ED", "#00BFFF", "#0000FF", "#87CEFA", "#4169E1", "#87CEEB", "#B22222", "#DC143C", "#FF0000", "#FF6347", "#FF7F50", "#CD5C5C", "#F08080", "#E9967A", "#FA8072", "#FFA07A", "#FFFF00", "#F0E68C", "#FFE4C4", "#2E8B57", "#00FF00", "#CD853F", "#EE82EE")
```


The output of WOTPLY function shows the connections between **selected_stages** from the latest time point and the clusters from previous time points. The number of columns is equal to the numbers of time points. In each column, the cluster of the
corresponding time point is shown as network node. The weight of the links between clusters at time points t and t+1 refelects the weigth of the
transition probabilities from **list_transition_matrices**.
It is possible to change the maximum number of links to select between clusters at time t and clusters at time t+1. Links are sorted according to the weigth and then only the **top_link** are kept. If **NULL** (default), all the links are kept.


```r
WOTPLY(list_transition_matrices = list_transition_matrices, selected_stages = selected_stages, cluster_label,time_label = time_label, legend_time = legend_time, customize_color = customize_color, top_link = NULL)
```
<img src="https://github.com/ScialdoneLab/WOTPLY/blob/main/figures/WOTPLY_1.png" width="500" height="500">

```r
WOTPLY(list_transition_matrices = list_transition_matrices, selected_stages = selected_stages, cluster_label, time_label = time_label, legend_time = legend_time, customize_color = customize_color, top_link = 3)
```
<img src="https://github.com/ScialdoneLab/WOTPLY/blob/main/figures/WOTPLY_2.png" width="500" height="500">



## Vignette

The following vignette is available and completely reproducible. 

It can be accessed within R with:
```r
utils::vignette("WOTPLY_vignette")
```




## Contributions and Support
Contributions in the form of feedback, comments, code and bug report are welcome.
* For any contributions, feel free to fork the source code and [submit a pull requests](https://docs.github.com/en/pull-requests/collaborating-with-pull-requests/proposing-changes-to-your-work-with-pull-requests/creating-a-pull-request-from-a-fork).
* Please report any issues or bugs here: https://github.com/ScialdoneLab/WOTPLY/issues.
Any questions and requests for support can also be directed to the package maintainer (gabriele[dot]lubatti[at]helmholtz-muenchen[dot]de).


