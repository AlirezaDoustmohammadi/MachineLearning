
#read csv file
dat=read.csv("ML_2021.csv",header = TRUE)

#show table
fix(dat)

#sample numbers
dim(dat)[1]

test_set=dim(dat)[1]/5

#distance vector
dist_val=c()
dist_val_indx=c()
nearest_dis=c()
nearest_dis_indx=c()
#prediction vecotr
pred_val=c()

#computing diatance

#bin
#(0*21)+1:(21*1)
#(1*21)+1:(21*2)
#(2*21)+1:((21*3)
#(3*21)+1:(21*4)
#(4*21)+1:dim(dat)[1]

#bin index
k=0
#knn
knn=5

while (k<5) {
  for (test_sample in (test_set*k+1):(test_set*(k+1))) {
    
    for (train_sample in 1:dim(dat)[1]) {
      if (! train_sample %in% (test_set*k+1):(test_set*(k+1))) {
        dis=sqrt(sum((dat[test_sample,1:dim(dat)[2]-1]-dat[train_sample,1:dim(dat)[2]-1])^2))
        dist_val=c(dist_val,dis)   
        dist_val_indx=c(dist_val_indx,train_sample)
      }
    }
    for (indx in 1:knn) {
      min_dis=min(dist_val)
      #find min-dist index
      min_indx=match(min_dis,dist_val)
      
      nearest_dis=c(nearest_dis,min_dis)
      nearest_dis_indx=c(nearest_dis_indx,dist_val_indx[min_indx])
      
      #change value
      dist_val[min_indx]=max(dist_val)
    }
    
    #pridiction
    knn_lable=c(dat[nearest_dis_indx,dim(dat)[2]])
    pred_lable=max(knn_lable)
    pred_val=c(pred_val,pred_lable)  
    
    #clear data from distance vector
    dist_val=c() 
    dist_val_indx=c()  
    nearest_dis=c()
    nearest_dis_indx=c()
  }
  k=k+1
  
  print(k)  
}

true_prediction=0

for (indx in 1:dim(dat)[1]) {
  if (dat[indx,dim(dat)[2]]==pred_val[indx]) {
    true_prediction=true_prediction+1
  }
}

#acc
acc=(true_prediction/dim(dat)[1])*100
