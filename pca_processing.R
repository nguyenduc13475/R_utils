args <- commandArgs(trailingOnly = TRUE)
file_path <- args[1]
pc1 <- 1
pc2 <- 2
if (!is.na(args[2])){
    pc1 <- as.integer(args[2])
}
if (!is.na(args[3])){
    pc2 <- as.integer(args[3])
}

raw_data <- read.csv(file_path) # data frame, each row is a data point, col name x, y, z, ...
coord_data <- raw_data[, 1:ncol(raw_data)-1]
color_data <- raw_data[, ncol(raw_data)] # the color column must between 0 and 1
color_indices <- as.integer(color_data * 256)

pca <- prcomp(coord_data, scale = TRUE)
windows()
plot(
  pca$x[,pc1], pca$x[,pc2], pch = 16, 
  xlab = paste("Principal Component", pc1, sep = " "), 
  ylab = paste("Principal Component", pc2, sep = " "),
  main = "PCA Analysis",
  col = heat.colors(nrow(raw_data))[color_indices],
)
Sys.sleep(100000)
dev.off()