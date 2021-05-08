   #Classification Trees.
   
   library('dplyr')
   
   tdata <- read.csv('Market.csv')
   
   tdata
   str(tdata)
   
   # gender, marital_status, education, nb_depend_child, employ_status, spouse_work, 
   # residential_status, product and purchase can be categories.
   
   #Checking for missing or NA values in the dataset
   
   tdata %>% summarise(length(gender),length(age),length(marital_status),length(education),length(nb_depend_child),
                       length(employ_status),length(yrs_current_job),length(yrs_employed),length(net_income),
                       length(spouse_work),length(spouse_income),length(residential_status),length(yrs_current_address),
                       length(product),length(purchase))
   
   levels(as.factor(is.na(tdata)))
   
   #find the levels in the categories identified above.
   
   # Converting the categories to factors.
   
   tdata1 <- tdata %>% mutate(gender = factor(gender), marital_status = factor(marital_status), 
                               education = factor(education), nb_depend_child = factor(nb_depend_child), 
                               employ_status = factor(employ_status), spouse_work = factor(spouse_work),  
                               residential_status = factor(residential_status), product = factor(product), 
                               purchase = factor(purchase))
   
   
   #Calculating the descriptive statistics for numerical columns
   
     library(psych)
   
     tdata1 %>% filter(purchase == 'yes') %>% select(age, yrs_current_job, yrs_employed, net_income, 
                     spouse_income, yrs_current_address) %>% describe()

# Descriptive Analytics

  #Categorical Columns

    #gender bar chart
    

    plot(tdata1 %>% filter(purchase == 'yes') %>% select(gender))
    
    tdata1 %>% filter(purchase == 'yes',gender == 'F') %>% summarise(length(gender))

    #marital_status bar plot
    

    plot(tdata1 %>% filter(purchase == 'yes') %>% select(marital_status))
    
    tdata1 %>% filter(purchase == 'yes', marital_status == 'widowed') %>% summarise(length(gender))
    
    #education bar plot
    
    plot(tdata1 %>% filter(purchase == 'yes') %>% select(education))
    
    tdata1 %>% filter(purchase == 'yes', education == 'univ') %>% summarise(length(education))
    
    #nb_depend_child bar plot
    
    plot(tdata1 %>% filter(purchase == 'yes') %>% select(nb_depend_child))
    
    tdata1 %>% filter(purchase == 'yes', nb_depend_child == '3') %>% summarise(length(nb_depend_child))
    
    #employ_status bar plot
    
    plot(tdata1 %>% filter(purchase == 'yes') %>% select(employ_status))
    
    tdata1 %>% filter(purchase == 'yes', employ_status == 'unemployed') %>% summarise(length(employ_status))
    
    #spouse_work bar plot
    
    plot(tdata1 %>% filter(purchase == 'yes') %>% select(spouse_work))
    
    tdata1 %>% filter(purchase == 'yes', spouse_work == 'no') %>% summarise(length(spouse_work))
    
    #residential status bar plot
    
    plot(tdata1 %>% filter(purchase == 'yes') %>% select(residential_status))
    
    tdata1 %>% filter(purchase == 'yes', residential_status == 'w_parents') %>% summarise(length(residential_status))
    
    #product bar plot
    
    plot(tdata1 %>% filter(purchase == 'yes') %>% select(product))
    
    tdata1 %>% filter(purchase == 'yes', product == 'stocks') %>% summarise(length(product))
    

 #Numerical Columns
    
    install.packages('Hmisc')
    library(Hmisc)
   
   #plotting histograms for the numerical columns
      
      hist.data.frame(tdata1 %>% filter(purchase == 'yes') %>% 
      select(age,yrs_current_job, yrs_employed, net_income, 
      spouse_income, yrs_current_address))
      
# Information gain
      
      install.packages('FSelector')
      library(FSelector)
      
      information.gain(purchase ~.,training1,unit='log2')
      
      
      
#Creating training and testing data
      
      library(caret)
      
      randm1 <- createDataPartition(tdata1$purchase, p=0.75, list=F)
      training1 <- tdata1[randm1,]
      testing1 <- tdata1[-randm1,]
      
      
      str(training1)
      str(testing1)
      
      
      
#CART (Classification and Regression Tree) 
      
      install.packages('rpart')
      library(rpart)
      
      class_tree <- rpart(purchase ~.,data=training1)
      class_tree
      
      #plotting the tree
      
      install.packages('rpart.plot')
      library(rpart.plot)
      
      rpart.plot(class_tree,main='Tree for Purchase', extra=108) 
      
      t_pred <- predict(class_tree, newdata = testing1, type="class")
      
      # the accuracy of the model
        
       #confusion matrix
        library(e1071)
        
        confusionMatrix(t_pred,testing1$purchase)
  
      

#C5.0 
        
      install.packages("C50")
      library('C50')
      
      c_tree <- C5.0(purchase ~.,data=training1)
      c_tree
      plot(c_tree)
      
      t_pred <- predict(c_tree, newdata = testing1, type ='class')
      
      confusionMatrix(t_pred, testing1$purchase)
  
# Naive Bayes # 
      
      n <- naiveBayes(purchase~.,data=training1)
      
      
      t_pred <- predict(n,newdata = testing1, type='class')
      
      
      confusionMatrix(t_pred,testing1$purchase,)   
      
      
      
#svm 
      s <- svm(purchase~.,data=training1)
      
      
      t_pred <- predict(s, newdata = testing1, type='class')     
      
      confusionMatrix(t_pred,testing1$purchase,) 
 
      

#Random Forest 
     
      library("randomForest")
      
      r <- randomForest(purchase~.,data =training1)
      
      plot(r)
      
      t_pred <- predict(r, newdata = testing1)
      
      confusionMatrix(t_pred,testing1$purchase)


#Applying k-Fold Cross Validation to Decision Tree Model
      
      
      #Creating the number of folds
      folds_dt <- createFolds(training1$purchase, k = 10)
      
      #Creating the function to train and test the model on each fold of data
      
      #Random Forest
      
         cv_dt <- lapply(folds_dt, function(x) {
            #Creating the training fold without test fold
            training_fold <- training1[-x,]
            test_fold <- training1[x,]
            #Building the learner model for each of the 10 unique training fold
            market_dt_kfold <- randomForest(purchase~., data=training_fold)
            #Making predictions on each unique test fold
            predict_test_dt_kfold <- predict(market_dt_kfold, newdata = test_fold, type = "class")
            #Creating confusion matrix for each unique test fold
            cm_dt <- table(predict_test_dt_kfold, factor(test_fold$purchase))
            #Calculating accuracy for each fold
            accuracy_dt <- (cm_dt[1,1] + cm_dt[2,2]) / (cm_dt[1,1] + cm_dt[2,2] + cm_dt[1,2] + cm_dt[2,1])
            return(accuracy_dt)
         }) 
         cv_dt
         
         #Taking the mean of all the 10 accuracy values to determine the ultimate model accuracy
         mean(as.numeric(cv_dt))  
      
      
      #C5.0
         
         cv_dt <- lapply(folds_dt, function(x) {
            #Creating the training fold without test fold
            training_fold <- training1[-x,]
            test_fold <- training1[x,]
            #Building the learner model for each of the 10 unique training fold
            market_dt_kfold <- C5.0(purchase~., data=training_fold)
            #Making predictions on each unique test fold
            predict_test_dt_kfold <- predict(market_dt_kfold, newdata = test_fold, type = "class")
            #Creating confusion matrix for each unique test fold
            cm_dt <- table(predict_test_dt_kfold, factor(test_fold$purchase))
            #Calculating accuracy for each fold
            accuracy_dt <- (cm_dt[1,1] + cm_dt[2,2]) / (cm_dt[1,1] + cm_dt[2,2] + cm_dt[1,2] + cm_dt[2,1])
            return(accuracy_dt)
         }) 
         cv_dt
         
         #Taking the mean of all the 10 accuracy values to determine the ultimate model accuracy
         mean(as.numeric(cv_dt))  
      
      
      #SVM
         
         cv_dt <- lapply(folds_dt, function(x) {
            #Creating the training fold without test fold
            training_fold <- training1[-x,]
            test_fold <- training1[x,]
            #Building the learner model for each of the 10 unique training fold
            market_dt_kfold <- svm(purchase~., data=training_fold)
            #Making predictions on each unique test fold
            predict_test_dt_kfold <- predict(market_dt_kfold, newdata = test_fold, type = "class")
            #Creating confusion matrix for each unique test fold
            cm_dt <- table(predict_test_dt_kfold, factor(test_fold$purchase))
            #Calculating accuracy for each fold
            accuracy_dt <- (cm_dt[1,1] + cm_dt[2,2]) / (cm_dt[1,1] + cm_dt[2,2] + cm_dt[1,2] + cm_dt[2,1])
            return(accuracy_dt)
         }) 
         cv_dt
         
         #Taking the mean of all the 10 accuracy values to determine the ultimate model accuracy
         mean(as.numeric(cv_dt))  
         
      
#Using the C5.0 classifier c_tree generated previously on the market pred dataset
      
      pdata <- read.csv('Market_pred.csv')
      
      # Converting the categories to factors.
      
      pdata1 <- pdata %>% mutate(gender = factor(gender), marital_status = factor(marital_status), 
                                 education = factor(education), nb_depend_child = factor(nb_depend_child), 
                                 employ_status = factor(employ_status), spouse_work = factor(spouse_work),  
                                 residential_status = factor(residential_status), product = factor(product), 
                                 purchase = factor(purchase))
      
      p_pred <- predict(c_tree, newdata = pdata1, type ='class')
      p_pred
      
      pdata1$purchase <- p_pred
      
      str(pdata1)
      
      
      
      
      