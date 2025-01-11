library(plotly)

extract_point_data <- function(file_path = "test_data.csv"){
    raw_data <- read.csv(file_path) # data frame, each row is a data point, col name x, y, z, ...
    coord_data <- raw_data[, 1:ncol(raw_data)-1]
    color_data <- raw_data[, ncol(raw_data)] # the color column must between 0 and 1
    color_indices <- as.integer(color_data * 256 + 1)
    return(list(coord_data = coord_data, color_data = color_data, color_indices = color_indices))
}

pca_visualize_2d <- function(file_path = "test_data.csv", pc1 = 1, pc2 = 2){
    extract_info = extract_point_data(file_path)
    coord_data <- extract_info$coord_data
    color_data <- extract_info$color_data
    color_indices <- extract_info$color_indices

    pca <- prcomp(coord_data, scale = TRUE, center = TRUE)
    windows()
    plot(
      pca$x[,pc1], pca$x[,pc2], pch = 16, 
      xlab = paste("Principal Component", pc1, sep = " "), 
      ylab = paste("Principal Component", pc2, sep = " "),
      main = "PCA Analysis",
      col = heat.colors(256)[color_indices],
    )
}

pca_visualize_3d <- function(file_path = "test_data.csv", pc1 = 1, pc2 = 2, pc3 = 3){
    extract_info = extract_point_data(file_path)
    coord_data <- extract_info$coord_data
    color_data <- extract_info$color_data
    color_indices <- extract_info$color_indices
    pca <- prcomp(coord_data, scale = TRUE, center = TRUE)
    plot_ly(
        x = pca$x[,pc1], y = pca$x[,pc2], z = pca$x[,pc3], 
        type = "scatter3d", mode = "markers",
        marker = list(
            color = heat.colors(256)[color_indices],
            size = 3
        )
    )
}

reverse_2d_pca <- function(x = 0, y = 0, file_path = "test_data.csv", pc1 = 1, pc2 = 2){
    extract_info = extract_point_data(file_path)
    coord_data <- extract_info$coord_data
    color_data <- extract_info$color_data
    color_indices <- extract_info$color_indices
    pca <- prcomp(coord_data, scale = TRUE, center = TRUE)
    reduce_data <- rep(0, ncol(coord_data))
    reduce_data[pc1] = x
    reduce_data[pc2] = y
    return((reduce_data %*% t(pca$rotation)) * pca$scale + pca$center)
}

reverse_3d_pca <- function(x = 0, y = 0, z= 0, file_path = "test_data.csv", pc1 = 1, pc2 = 2, pc3 = 3){
  extract_info = extract_point_data(file_path)
  coord_data <- extract_info$coord_data
  color_data <- extract_info$color_data
  color_indices <- extract_info$color_indices
  pca <- prcomp(coord_data, scale = TRUE, center = TRUE)
  reduce_data <- rep(0, ncol(coord_data))
  reduce_data[pc1] = x
  reduce_data[pc2] = y
  reduce_data[pc3] = z
  return((reduce_data %*% t(pca$rotation)) * pca$scale + pca$center)
}