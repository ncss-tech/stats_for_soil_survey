sand <- read.table("C:/R_data/sand_example.csv", header = TRUE, sep=",")

names(sand)

str(sand)

plot(depth~sand, data = sand)
