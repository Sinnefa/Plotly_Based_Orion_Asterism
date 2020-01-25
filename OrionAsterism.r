library(plotly)
library(htmlwidgets)
data <- read.csv("hygdata_v3.csv",sep=",")
data[1,]$hr <- -1
data <- data[!is.na(data$hr) & data$hr!="" & data$dist < 100000,]
data$id[is.na(data$id)]<-0


getElbowPoint <- function(x_values, y_values) {
	# Max values to create line
	max_x_x <- max(x_values)
	max_x_y <- y_values[which.max(x_values)]
	max_y_y <- max(y_values)
	max_y_x <- x_values[which.max(y_values)]
	max_df <- data.frame(x = c(max_y_x, max_x_x), y = c(max_y_y, max_x_y))

	# Creating straight line between the max values
	fit <- lm(max_df$y ~ max_df$x)

	# Distance from point to line
	distances <- c()
	for(i in 1:length(x_values)) {
		distances <- c(distances, abs(coef(fit)[2]*x_values[i] - y_values[i] + coef(fit)[1]) / sqrt(coef(fit)[2]^2 + 1^2))
	}
	# Max distance point
	x_max_dist <- x_values[which.max(distances)]
	y_max_dist <- y_values[which.max(distances)]

	return(c(x_max_dist, y_max_dist, max(distances)))
}
showElbowGraph <- function(x_clusters, y_wcss) {
	nb_wcss_values = length(y_wcss)
	extremes_line_coef = (x_clusters[nb_wcss_values] - x_clusters[1]) / (y_wcss[nb_wcss_values] - wcss_values[1])
	extremes_orth_line_coef = -1 / extremes_line_coef
	elbowPoint_orth_proj = c(elbowPoint_info[1] + elbowPoint_info[3]/2, elbowPoint_info[2] + extremes_orth_line_coef * (elbowPoint_info[3]/2))

	plot(x_clusters, y_wcss, type="b", main = 'WCSS value according to the number of clusters', xlab = 'Number of clusters', ylab = 'WCSS value')
	lines(x=c(x_clusters[1], x_clusters[nb_wcss_values]), y=c(y_wcss[1], y_wcss[nb_wcss_values]), type="b", col='green')
	lines(x=c(elbowPoint_info[1], elbowPoint_orth_proj[1]), y=c(elbowPoint_info[2], elbowPoint_orth_proj[2]), type="b", col='red')
}
getWCSSData <- function(X) {
	wcss_values <- vector()
	max_wcss_steps = sqrt(length(X[,1]))
	for(i in 1:max_wcss_steps) {
		print(i)
		wcss_values[i] <- sum(kmeans(X, i, iter.max = 1000)$withinss)
	}
	return(wcss_values)
}
init3DGraph <- function() {
	p<-plot_ly(evaluate = FALSE)
	return(p)
}
show3DGraph <- function(p, x_name, y_name, z_name) {
	layout(p, scene = list(xaxis = list(title = x_name), yaxis = list(title = y_name), zaxis = list(title = z_name)))
}
setTrace <- function(p, x, y, z, n, c, names, mode) {
	p<-add_trace(p, x=as.vector(x),y=as.vector(y),z=as.vector(z), type="scatter3d", mode=mode, name = n, marker = list(size = 2, color = c), text=names)
	return(p)
}

scene <- list(
		camera = list(
				center = list(x= 0.08061799388993592, y= -0.0003358426787613002, z= 0.029901459015662175 ), # orion
				eye = list (x= 0.07836366832725744, y= -0.022661745180340268, z= 0.02799685952087579 ), # orion
				projection = list(type= "perspective")
			)
	)


myB <- list(
  name = "Get Camera Vectors",
  click = htmlwidgets::JS(
    "function(gd) {
      console.log(gd.layout.scene.camera)
    }")
)
myB2 <- list(
  name = "Orion from Behind the Sun",
  click = htmlwidgets::JS(
    "function(gd) {
      var camera = gd.layout.scene.camera;
      camera.center = {x: 0.08061799388993592, y: -0.0003358426787613002, z: 0.029901459015662175 };
      camera.eye = {x: 0.07836366832725744, y: -0.022661745180340268, z: 0.02799685952087579 };
      gd.layout.scene.camera = camera;
      Plotly.relayout(gd, gd.layout);
    }")
)
myB3 <- list(
  name = "Solar System from Orion Belt Center",
  click = htmlwidgets::JS(
    "function(gd) {
      var camera = gd.layout.scene.camera;
      camera.center = {x: 0.09167910708283804, y: -0.0013809338092858083, z: 0.034941803311850365 };
      camera.eye = {x: 0.11108698609448736, y: 0.2930482050930699, z: 0.02235502503533733 };
      gd.layout.scene.camera = camera;
      Plotly.relayout(gd, gd.layout);
    }")
)
myB4 <- list(
  name = "Orion Constellation Spreading",
  click = htmlwidgets::JS(
    "function(gd) {
      var camera = gd.layout.scene.camera;
      camera.center = {x: 0.08152077999668735, y: 0.07753404399196363, z: 0.025662626514569645 };
      camera.eye = {x: 0.1811205966837108, y: -0.07050218793824471, z: 0.11752657502010351 };
      gd.layout.scene.camera = camera;
      Plotly.relayout(gd, gd.layout);
    }")
)


p<-plot_ly(evaluate = FALSE) %>% layout(plot_bgcolor='black') %>% layout(paper_bgcolor='black') %>% layout(scene = scene) %>% config(modeBarButtonsToAdd = list(myB2,myB3,myB4))

p<-add_trace(p, x=as.vector(data$x),y=as.vector(data$y),z=as.vector(data$z), type="scatter3d", mode="markers", marker = list(size = 0.5, color = "rgb(150, 150, 150)"), text=data$hd, name="HR Stars")
p<-add_trace(p, x=as.vector(data[1,]$x),y=as.vector(data[1,]$y),z=as.vector(data[1,]$z), type="scatter3d", mode="markers", marker = list(size = 4, color = "orange"), text=data$hd, name="Sun")
p<-add_trace(p, x=as.vector(data[data$con=="Ori",]$x),y=as.vector(data[data$con=="Ori",]$y),z=as.vector(data[data$con=="Ori",]$z), type="scatter3d", mode="markers", marker = list(size = 1, color = "white"), name="Orion Constellation")

orion <- c("2061",

"2124","2159","2199","2159","2047","2159","2124","2199","2130","2199","2124","2061", # Arm

"1879","1790",

"1543","1544","1570","1544","1543","1552","1567","1601","1567","1552","1543","1790", #Bow

"1852",

"1903","1948","1903","1852", # Belt

"1713","2004","1948","2061")
p<-add_trace(p, x=as.vector(data[match(orion, data$hr),]$x),y=as.vector(data[match(orion, data$hr),]$y),z=as.vector(data[match(orion, data$hr),]$z), type="scatter3d", mode="lines+markers", marker = list(size = 2, color = "blue"), name="Orion Asterism", text=data[match(orion, data$hr),]$proper, line=list(color = 'lightblue', width = 1))
p<-add_trace(p, x=as.vector(data[data$hr=="1903",]$x),y=as.vector(data[data$hr=="1903",]$y),z=as.vector(data[data$hr=="1903",]$z), type="scatter3d", mode="markers", marker = list(size = 3, color = "blue"), text=data$hd, name="Alnilam")

saveWidget(p, file=paste0( getwd(), paste0("/universe.html")))

p


