##' Read from one vct file (which is one hours' worth of data, at a finer e.g. 3
##' minute level), and fully process this particle-level data. The processing
##' includes (1) taking the relevant columns and high-quality measurements from
##' the VCT files, (2) removing beads (3) removing the edge points, and (3)
##' relabeling dates.
##'
##' @param one_vct_filename File name of a single VCT file.
##'
##' @return One data frame with four columns named (diam, chl, pe, qc).
##' @export
read_from_one_vct_file <- function(one_vct_filename){

  ## Read from the hourly level data file
  df <- arrow::read_parquet(one_vct_filename)

  ## Retain the proper quality (q50) measurements
  df <- df %>% dplyr::filter(q50 == TRUE) %>% dplyr::select(-q50)

  ## Remove the beads
  df = df %>% dplyr::filter(pop_q50 != "beads")

  ## Obtain raw data
  y <- df %>% dplyr::select(diam = contains("diam_mid_q50"),
                            chl = contains("chl"),
                            pe = contains("pe"),
                            qc = contains("Qc_mid_q50")) ##%>% log()
  ## Log transform
  y <- y %>% dplyr::mutate(diam = log(diam),
                           chl = log(chl),
                           pe = log(pe),
                           qc = qc)

  ## Remove edge points
  y <- y %>% process_y(verbose = TRUE) %>% as_tibble()

  ## Get the single hour associated with this file
  date = df %>% dplyr::pull(date) %>% lubridate::ymd_hms() %>% unique() %>%
    lubridate::floor_date(unit = "hours") %>% unique()
  stopifnot(length(date) == 1)

  ## Return the results
  return(list(date = date, tab = y))
}


##' Processes a cytogram (matrix that has the columns chl, pe and diam) by
##' getting rid of points that lie on the boundaries of each axis.
##'
##' @param y Matrix whose rows represent cytogram particles.
##' @param ... Additional arguments.
##'
##' @return Processed matrix.
##'
##' @export
process_y <- function(y, verbose = FALSE){

  ## Basic checks
  if(nrow(y) < 10) return(y)
  stopifnot(all(c("chl", "pe", "diam") %in% colnames(y)))

  ## Setup
  rid.inds = list()

  ## Eliminate boundary points one dimension at a time.
  jjlist = sapply(c("chl", "pe", "diam"), function(myname) grep(myname, names(y), fixed = TRUE))
  for(jj in jjlist){
    rid.inds[[jj]] = identify_boundary_points_1d(y[,jj])
  }
  rid.inds = unique(unlist(rid.inds))

  if(verbose){
    cat(fill = TRUE)
    cat(length(rid.inds), "points on the edge out of", nrow(y), "are deleted", fill=TRUE)
  }
  return(as.matrix(y[-rid.inds,]))
}


##' Takes a 1-column matrix and identifies the ones that are on the boundaries.
##'
##' @param y1d 1d matrix.
##'
##' @return Indices of the points on the boundaries.
identify_boundary_points_1d <- function(y1d){
  y1d = unlist(y1d)
  rng = max(y1d) - min(y1d)
  rid.min.ind = which(y1d == min(y1d))
  ## rid.max.ind = which(y1d == max(y1d))
  rid.max.ind = which(y1d > max(y1d) - rng * 0.005 )
  rid.ind = c(rid.min.ind, rid.max.ind)
  if(length(rid.ind) > 0.1 * length(y1d)){
    print( length(rid.ind) / length(y1d) )
    print('were eliminated!')
  }
  return(unique(rid.ind))
}
