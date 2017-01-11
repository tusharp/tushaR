#uncomment line number 2 and 41 to run this snippet.
#data(iris)

#function to calculate distance between the chosen center and all the elements in the dataset.
calc.dist<-function(dataset,center,varno){
  distance.from.center<-apply(X = dataset,FUN = function(t,ref,varno){
                        dist(matrix(data = c(t,ref),byrow = T,ncol = varno),method = "euclidean",upper = T)
                      },dataset[center,],varno,MARGIN = 1)
  return(distance.from.center)
}

#function that returns the k-centers calculated based on the probability depending on distance from nearest centers
choose.center<-function(dataset,k){
  #getting row and column size
  data.row=nrow(dataset) 
  data.col=ncol(dataset)
  #generating a sequence to use as an index representative of the vectors
  sequence<-seq(1:data.row)
  #initialising a vector with length equal to the k value provided
  initial.centers<-numeric(k)
  #randomly choosing an index from the sequence vector as center1
  initial.centers[1]=sample(sequence,size = 1)
  #initiating a matrix with zeros to store probability for each index
  prob.dists<-matrix(data = numeric((k-1)*(data.row)),ncol = (k-1))
  
  count=1
  while(count<k){
  dists<-calc.dist(dataset,initial.centers[count],data.col)
  prob.dists[,count]<-dists/(sum(dists))
  if(count==1){
    initial.centers[count+1]=sample(x = sequence,prob = prob.dists[,count],size = 1)
  }else{
    probs=apply(prob.dists[,1:count],MARGIN = 1,FUN = min,na.rm=T)   
    initial.centers[count+1]=sample(x = sequence,prob = probs,size = 1)
  }
    count=count+1
  }
  return(initial.centers)
}

#centers=choose.center(iris[,1:4],3)