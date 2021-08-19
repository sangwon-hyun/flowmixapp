##' Print the alpha table, as a plot, or as a table.
##'
##' @param bestres One flowmix object.
print_alpha <- function(bestres, type){
  alpha = bestres$alpha %>% t()
  numclust = ncol(alpha)
  alpha = alpha[-(rownames(alpha) == "intp"), ]
  colnames(alpha) = paste0("clust-", (1:numclust) %>% sapply(toString))
  max_abs = max(abs(alpha))
  cols <- colorRampPalette(RColorBrewer::brewer.pal(11, "RdBu"))(11)
  brk <- lattice::do.breaks(c(-max_abs, max_abs), 11)
  if(type == "plot"){
    p = drawmat_precise_ggplot(alpha, colours = cols) + ggtitle("Alpha coefficients") ##+ xlab("cluster-dimension")
    p = p + scale_fill_gradient2(low = ("red"),
                                 mid = "white",
                                 midpoint = 0,
                                 high = ("blue"))
    p = p + geom_hline(yintercept = 1:1000 - 0.5, col = rgb(0,0,0,0.3))
    p = p + geom_vline(xintercept = 1:1000 - 0.5, col = rgb(0,0,0,0.3))
    return(p)
    drawmat_precise_barebones(alpha, par.settings = list(fontsize = list(text = 20, points = 4)),
                              col.regions = cols,
                              at = brk,
                              colorkey = list(col = cols,
                                              at = brk),
                              xlab = "Cluster",
                              ylab = "Covariate",
                              main = list('Alpha coefficients', side = 1, line = 0.5))
  }
  if(type == "table"){
    tab = alpha %>% process_table(3) %>% kable(caption = "Alpha coefficients")
    return(tab)
  }
}


##' Plots Beta coefficient.
plot_beta <- function(bestres, cytogram_dimnames = NULL){

  ## Setup
  dimdat = ncol(bestres$beta[[1]])
  numclust = length(bestres$beta)

  ## More setup
  if(is.null(cytogram_dimnames)) cytogram_dimnames = 1:dimdat

  ## Get the coefficients
  beta = bestres$beta %>% do.call(cbind,.)
  colnames(beta) = paste0(rep(1:numclust, each = dimdat), "-", rep(c(cytogram_dimnames), numclust))
  beta = beta[-1, ]
  max_abs = max(abs(beta))
  brk <- lattice::do.breaks(c(-max_abs, max_abs), 11)
  cols <- colorRampPalette(RColorBrewer::brewer.pal(11, "RdBu"))(11)
  p = drawmat_precise_ggplot(beta, colours = cols) + ggtitle("Beta coefficients") ##+ xlab("cluster-dimension")
  p = p + geom_vline(xintercept = dimdat * (1:numclust) + .5, col = rgb(0,0,0,0.3))
  p = p + geom_hline(yintercept = 1:1000 - 0.5, col = rgb(0,0,0,0.3))
  ## p = p + scale_fill_gradientn(colours = cols, guide="colorbar")
  p = p + scale_fill_gradient2(low = ("red"),
                               mid = "white",
                               midpoint = 0,
                               high = ("blue"))
  return(p)

  ## Plot the coefficients
  drawmat_precise_barebones(beta, par.settings = list(fontsize = list(text = 20, points = 4)),
                            col.regions = cols,
                            at = brk,
                            colorkey = list(col = cols,
                                            at = brk),
                            xlab = "Cluster-dimension",
                            ylab = "Covariate",
                            main = list('Beta coefficients', side = 1, line = 0.5))

}



##' @param bestres flowmix object
print_beta <- function(bestres, cytogram_dimnames = NULL){
  numclust = length(bestres$beta)
  dimdat = ncol(bestres$beta[[1]])
  beta = bestres$beta %>% do.call(cbind,.)
  stopifnot(dimdat == 3)
  if(is.null(cytogram_dimnames)) cytogram_dimnames = 1:dimdat

  ## library(kableExtra)
  colnames(beta) = paste0(rep(1:numclust, each = dimdat), "-", rep(c(cytogram_dimnames), numclust))
  tab = beta %>% process_table(3) %>% knitr::kable(format = "html") %>% kableExtra::kable_styling(font_size = 9) %>%
    kableExtra::column_spec(seq(from=1,to=ncol(beta), by=3), border_left = FALSE, border_right = TRUE)##, color=rgb(0,0,0,0.4))

  return(tab)

  ## OLD
  for(iclust in 1:numclust){
    ## cat("####", paste0("Cluster ", iclust))
    columns = (iclust-1) * 3 + (1:3)
    beta %>% process_table(3) %>% .[,columns] %>% `colnames<-`(cytogram_dimnames) %>%
      kable(caption = paste0("Beta coefficients, cluster ", iclust)) %>% print()
  }
  ## end of OLD
}


drawmat_precise_barebones <- function (mat, ...)
{
    if (is.null(colnames(mat))) {
        colnames(mat) <- paste(rep("col\n", ncol(mat)), c(1:ncol(mat)),
            sep = " ")
        rownames(mat) <- paste(rep("row", nrow(mat)), c(1:nrow(mat)),
            sep = " ")
    }
    lattice::levelplot(t(mat[c(nrow(mat):1), ]), las = 2, ...)
}



process_table <- function(mat, digit=3){
  apply(mat, 2, function(mycol){
    sapply(mycol, function(entry) if(entry==0) "." else round(entry, digit))
  })
}


plot_prob_ggplot <- function(bestres, times = NULL){
  prob = bestres$prob
  numclust = ncol(prob)
  colnames(prob) = paste0("clust-", 1:numclust)
  prob = prob %>% as_tibble()
  TT = nrow(prob)
  if(is.null(times)) times = 1:TT
  if(!is.null(times)){
    ## stopifnot(all(times == as_datetime(times)))
    times = times %>% lubridate::as_datetime()
  }
  prob = prob %>% add_column(times = times)
  p = prob %>% pivot_longer(-any_of("times")) %>%
    ggplot() +
    ## facet_wrap(~name) +
    facet_wrap(~factor(name, levels=paste0("clust-", 1:numclust))) +
    geom_line(aes(x=times, y=value))
    ## ylim(c(0,1))

  ## p = p + scale_x_datetime(date_breaks = "6 hour", labels = scales::date_format("%b %d - %H:%M"))
  p = p + theme(axis.text.x = element_text(angle = 90, vjust = 1.0, hjust = 1.0))
  p = p + ggtitle("Cluster probabilities")
  return(p)

}
