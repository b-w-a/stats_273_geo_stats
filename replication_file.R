

# read data 
a <- read.csv("https://raw.githubusercontent.com/b-w-a/stats_273_geo_stats/master/replication.csv", header = T, stringsAsFactors = F)
q <- gstat(id="z", formula=z~1, locations=~x+y, data=a) # make gstat object 

plot(variogram(q)) # plot the variogram 
# here is where I am conused on the values to use in the vgm object

v.fit <- fit.variogram(variogram(q), vgm(.25,"Sph",1, 0.25))
# this returns a warning. 

plot(variogram(q),v.fit) # this does not look right


x.range <- range(a$x) 
y.range <- range(a$y)
grd <- expand.grid(x=seq(from=x.range[1], to=x.range[2], by=.01), y=seq(from=y.range[1], to=y.range[2], by=.01)) 


pr_ok <- krige(id = "c1",z~1, locations=~x+y, model=v.fit, data=a, newdata=grd)
pr_ok %>% head()

#Set values < 0 equal to zero, and values > 1 equal to 1:
for(i in 1:length(pr_ok$c1.pred) ){if(pr_ok$c1.pred[i] <0) {pr_ok$c1.pred[i]=0}}

for(i in 1:length(pr_ok$c1.pred) ){if(pr_ok$c1.pred[i] >1) {pr_ok$c1.pred[i]=1}}


#Collapse the predicted values into a matrix: 
qqq <- matrix(pr_ok$c1.pred, 
              length(seq(from=x.range[1], to=x.range[2], by=.01)), 
              length(seq(from=y.range[1], to=y.range[2], by=.01))) 

#Raster map:
image(seq(from=x.range[1], to=x.range[2], by=.01), 
      seq(from=y.range[1],to=y.range[2], by=.01), qqq, 
      xlab="Westto East",ylab="South to North", main="Predicted values") 

#Add the observed data points on the raster map:
points(a)

# 
# 
# #We want to assign "NA" values for the region outside the observed data #points:
# X <- seq(from=x.range[1], to=x.range[2], by=.01)
# Y <- seq(from=y.range[1], to=y.range[2], by=.01)
# 
# dz.mask <- function(grid, pts, x.dom, y.dom, window, mitre=2) {
#   N <- length(pts[ ,1]) ; mask <- array( NA, dim(grid) )
#   for(j in 1:N) {
#     dx <- abs(x.dom - pts$x[j])
#     dy <- abs(y.dom - pts$y[j])
#     d.Mx <- tcrossprod( dx , rep(1, length(dy)) )^mitre + 
#       tcrossprod( rep(1, length(dx)), dy )^mitre
#     mask[ d.Mx < window^mitre ] <- FALSE
#   }
#   return(mask+grid)
# }
# 
# qqq.masked <- dz.mask(qqq, a, X, Y, 250)
# 
# image(X,Y,qqq.masked)
# points(a, cex=a$lead/mean(a$lead), pch=19, col="green")
# 
# 
# #Add contour lines:
# contour(seq(from=x.range[1], to=x.range[2], by=.01), 
#         + seq(from=y.range[1],to=y.range[2], by=.01), qqq.masked, 
#         levels=seq(0, 1, by=0.1), add=TRUE, col="black", labcex=1) 
