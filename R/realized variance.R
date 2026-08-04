#'@export

######### building the function for the realized variance of the return

# rv function with variable q
RVNWq <- function(datatable, datecolumn, datacolumn, q, method = c('percentage', 'absolute', 'logarithmic')){
  data <- eval(substitute( datatable[,.(datecolumn,datacolumn)]))
  # intra day return vector
  setnames(data, c(1:2), c('date', 'V1'))

  # introducing three different 'methods'
  if(method == 'absolute'){
    y <- data[, diff(V1), by = date]
    y <- y[complete.cases(V1), ]
  }else if(method == 'percentage'){
    y <- data[, Delt(V1), by = date]
    y <- y[complete.cases(V1), ]
  }else if(method == 'logarithmic'){
    y <- data[, Delt(V1, type = 'log'), by = date]
    y <- y[complete.cases(V1), ]
  }
  # regular realized variance for each day
  rv <- y[, sum(V1^2), by = date]
  rv.ac <- rv
  rv.nw <- rv

  for (h in 1:q) {
    # weighting factor
    weight.nw <- 1 - h/(q+1)
    # creating the autocovariance term
    yhead <- y[, head(V1, - h), by = date]
    ytail <- y[, tail(V1, - h), by = date]
    yheadtail <- yhead[, V2 := ytail$V1]
    m <- yheadtail[, sum(V1)/mean(V1), by=date]
    autocov.ac <- yheadtail[, sum(V1*V2), by=date]
    autocov.ac <- autocov.ac[, V1 := V1 * m$V1 / (m$V1 - h)]
    autocov.nw <- yheadtail[, weight.nw*sum(V1*V2), by=date]
    idx <- which(rv.nw$date %in% autocov.nw$date)
    rv.ac <- rv.ac[idx]
    rv.ac <- rv.ac[, V1 := V1 + autocov.ac$V1]
    idx <- which(rv.nw$date %in% autocov.ac$date)
    rv.nw <- rv.nw[idx]
    rv.nw <- rv.nw[, V1 := V1 + autocov.nw$V1]
  }

  # creating a table with all needed inputs and calculating the rvnw for each day
  rvnw <- as.data.table(rv.nw[, date ])
  RVNW <- rvnw[ rv, on=.(V1=date)]
  RVNW <- RVNW[ rv.ac, on=.(V1=date)]
  RVNW <- RVNW[ rv.nw, on=.(V1=date)]

  # setting column names depending on chosen method
  if(method == 'absolute'){
    setnames(RVNW, 1:4, c('date','RV_abs','RVAC_abs','RVNW_abs'))
  }else if(method == 'percentage'){
    setnames(RVNW, 1:4, c('date','RV_perc','RVAC_perc','RVNW_perc'))
  }else if(method == 'logarithmic'){
    setnames(RVNW, 1:4, c('date','RV_log','RVAC_log','RVNW_log'))
  }

  return(RVNW)
}

