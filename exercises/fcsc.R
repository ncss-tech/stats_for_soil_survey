

cscs <- function(x, vars = NULL, n = NULL, center = TRUE, scale = TRUE, iter.max = 1000) {
  
  if (class(x) == "SpatRaster") {
    df <- terra::as.data.frame(x, xy = TRUE)
  }
  if (class(x) == "RasterStack") {
    df <- as.data.frame(x, xy = TRUE)
  }
  rn <- row.names(df)
  row.names(df) <- NULL
  
  idx <- df[vars] |>
    complete.cases()
  df2 <- df[idx, vars] |>
    scale(center = center, scale = scale)
  
  cl <- df2 |> 
    kmeans(center = n, iter.max = iter.max)
  
  df2 <- cbind(df2, cl = cl$cluster)
  df$cluster <- NA
  df[row.names(df2), "cluster"] <- cl$cluster
  
  n_clus <- cl$centers |> nrow()
  
  d <- fields::rdist(cl$centers, df2[, vars])
  idx2 <- apply(d, 1, which.min)
  idx3 <- row.names(df2)[idx2]
  cl_cen <- df[idx3, ]
  # cl_cen
  
  # idx <- list()
  # for (i in 1:n_clus) {
  #   z <- rbind(cl$centers[i, ], df2[, vars])
  #   d <- dist(z)
  #   idx[[i]] <- which.min(as.matrix(d)[1, -1])
  # }
  # idx <- unlist(idx)
  # 
  # cl_cen <- df2[idx, ]
  # cl_cen
  
  return(cl_cen)
}
