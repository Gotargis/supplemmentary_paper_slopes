library(tidyverse) 
library(patchwork)
library(dtwclust) 

#### Folder with the data
#Defines the path to the folder containing the data files 
pathtofiles = "C:/Users/Admin/Desktop/data/" #input data: deceleration of each participant in each trial. Files format '.txt'

#### Import data  ####
#files were named as 'ALBdownhill1_dec.txt': ALB - participants' initials, downhill - slope condition, 1 - trial;
list = list.files(path = pathtofiles, full.names = F) #lists the files in the specified folder
list = data.frame(str_split(list, "_")) #splits the filenames based on underscores 
list = data.frame(t(list)) #constructs a data frame from the split parts
data = NULL
#loop through the list of files, reads each CSV file, processes the data, and appends it to 'data'
for (i in c(1:nrow(list))){
  file = paste0(pathtofiles, list$X1[i],"_",list$X2[i])
  filedata = read.table(file, header = FALSE, col.names = "deceleration")
  filedata = filedata %>%
    mutate(center_value = scale(deceleration, scale = F)[,1], #centering
           init_value = deceleration - deceleration[1], #calculating distances
           frame = c(1:length(filedata$deceleration)),
           id = list$X1[i],
           dist = -4)
  for (j in c(2:length(filedata$deceleration))){
    filedata$dist[j] = filedata$dist[j-1] + (filedata$deceleration[j-1]*0.016)
  }
  data = rbind(data,filedata)
  rm(filedata, file)
}

#### Clustering ####
# Linear interpolation to have TS with same length
data_clus = data.frame(dist = seq(-4,0,by = .2)) #linear interpolation (every .05s) to ensure that all time series have the same length
for (i in c(1 : length(unique(data$id)))){
  filedata = subset(data, id == unique(data$id)[i])
  appdata = data.frame(approx(filedata$dist,
                              filedata$center_value, 
                              xout = data_clus$dist,
                              rule = 2, method = "linear", ties = mean))
  colnames(appdata) = c("dist", unique(data$id)[i])
  data_clus = cbind(data_clus, appdata %>% select(-dist))
  rm(appdata,filedata)
}
data_clus = data_clus %>% select(-dist)
# Clustering
set.seed(101)
clust.pam <- tsclust(t(data_clus), type="partitional", #'tsclust' function performs time series clustering
                     k=2L:3L, #modify the '3' by the number of clusters you want +1. Eg., if you want to test the data in 4 clusters, you enter the number 5. You should test several numbers of cluster to find the number that is more appropriate to your study. 
                     distance="dtw", 
                     centroid="pam")
# Statistics to evaluate the best number of cluster
test = data.frame(t(sapply(clust.pam, FUN = cvi))) #calculates cluster validity indices (cvi) for each clustering and stores them in 'test'
#it will return you the best number of cluster. 


# Viz of the results
# Use coord_cartesian to set y-axis limits
plot(clust.pam[[2-1]]) +
  coord_cartesian(ylim = c(-2, 3))

# Save the plot
ggsave("best_clustering.png",
       units = "in", width = 10, height = 5)
