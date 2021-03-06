
#read csv file
dat=read.csv("ML_2021.csv",header = TRUE)

#show table
#fix(dat)

#sample numbers
#dim(dat)[1]

#distance vector
dist_val=c()
dist_val_indx=c()
nearest_dis=c()
nearest_dis_indx=c()
#prediction vecotr
pred_val=c()

#computing diatance
#knn
knn=5

#random sample inde generator
sample=c(1:dim(dat)[1])
checked_sample=c()
unchecked_sample=c()

#acc vector
acc_vector=c()

while (length(checked_sample)<length(sample)) {
  
  #random generator
  unchecked_sample=sample[! sample %in% checked_sample]
  test_sample_vector=unchecked_sample[sample.int(length(unchecked_sample),21)]
  checked_sample=c(checked_sample,test_sample_vector)
  train_sample_vector=sample[! sample %in% test_sample_vector] 
    
  for (test_sample in test_sample_vector) {
    
    for (train_sample in train_sample_vector) {
        dis=sqrt(sum((dat[test_sample,1:dim(dat)[2]-1]-dat[train_sample,1:dim(dat)[2]-1])^2))
        dist_val=c(dist_val,dis)   
        dist_val_indx=c(dist_val_indx,train_sample)
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
  true_prediction=0
  for (indx in 1:length(test_sample_vector)) {
    if (dat[test_sample_vector[indx],dim(dat)[2]]==pred_val[indx]) {
      true_prediction=true_prediction+1
    }
  }
  #acc
  acc=(true_prediction/length(test_sample_vector))*100
  acc_vector=c(acc_vector,acc)
  #clear prediction table
  pred_val=c()
}


final_acc=mean(acc_vector)

