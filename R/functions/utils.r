d <- function(distance, selection){
  eval(parse(text = paste("as.matrix(distance)[",
                          selection, "]")))
}

`d<-` <- function(distance, selection, value){
  eval(parse(text = paste("as.matrix(distance)[",
                          selection, "] <- value")))
  as.dist(distance)
}

f <- function (i, j, dist_obj) {
  if (!inherits(dist_obj, "dist")) stop("please provide a 'dist' object")
  n <- attr(dist_obj, "Size")
  valid <- (i >= 1) & (j >= 1) & (i > j) & (i <= n) & (j <= n)
  k <- (2 * n - j) * (j - 1) / 2 + (i - j)
  k[!valid] <- NA_real_
  k
}

SliceExtract_dist <- function (dist_obj, k) {
  if (length(k) > 1) stop("The function is not 'vectorized'!")
  n <- attr(dist_obj, "Size")
  if (k < 1 || k > n) stop("k out of bound!")
  ##
  i <- 1:(k - 1)
  j <- rep.int(k, k - 1)
  v1 <- dist_obj[f(j, i, dist_obj)]
  ##
  i <- (k + 1):n
  j <- rep.int(k, n - k)
  v2 <- dist_obj[f(i, j, dist_obj)]
  ##
  c(v1, 0, v2)
}

randomName <- function(n = 1) {
  a <- do.call(paste0, replicate(5, sample(LETTERS, n, TRUE), FALSE))
  paste0(a, sprintf("%04d", sample(9999, n, TRUE)), sample(LETTERS, n, TRUE))
}

# ----------------------------------------------------------------------------
# |
# | Utilitary function to create a path.
# |
# ----------------------------------------------------------------------------
createPath <- function (strPath = "") {

  if (!file.exists(strPath) && strPath != ""){
    dir.create(file.path(strPath), recursive = TRUE)
  }

  strPath
}

saveNewExecution <- function(executionStatus) {
  currentExecution <- read.csv(here("R/results", executionStatus$Tecnique, executionStatus$IdExecution, "/execution_status.csv"), header = TRUE, stringsAsFactors = FALSE, sep=";")

  allExecutions <- tryCatch(
    {
      allExecutions <- read.csv(here("R/results", "execution_status.csv"), header = TRUE, stringsAsFactors = FALSE, sep=";", fileEncoding="UTF-8-BOM")
      allExecutions <- rbind(allExecutions, currentExecution)
      allExecutions
    },
    error = function(err) {
      allExecutions <- data.frame()
      allExecutions <- rbind(allExecutions, currentExecution)
      allExecutions
    }
  )

  write_excel_csv2(allExecutions, here("R/results", "execution_status.csv", sep=""));
}
