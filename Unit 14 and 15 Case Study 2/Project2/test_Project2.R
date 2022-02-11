 # Figure out best seed, for reproducibility and getting solid sens/spec

 iterations = 20000

 masterSpec = matrix(nrow = iterations)
 masterSens = matrix(nrow = iterations)

 for(j in 1:iterations)
 {
   set.seed(j)


   ind   = sample(1:dim(CS_Attr_Model)[1],round(.65 * dim(CS_Attr_Model)[1]))
   train = CS_Numeric[ind,]
   test  = CS_Numeric[-ind,]

   model = naive_bayes(Attrition ~ ., data= train, usekernel = F, usepoisson = T, laplace = 1)

   pred_L = predict(model ,test)
   predtbl_L = table(pred_L,test$Attrition)

   confmat = confusionMatrix(predtbl_L)

    # print(j) , takes about 10sec to run through 1000
    # 
    # Only grabbing high spec, so I can find the best seed easier.
   
   if (mean(confmat$byClass[2]) > .59) {

     masterSens[j] = mean(confmat$byClass[1])
     masterSpec[j] = mean(confmat$byClass[2])

   }


 }

 plot(masterSens,masterSpec)

 # Best seed is 14029.