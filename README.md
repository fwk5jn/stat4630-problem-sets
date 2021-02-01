# stat4630-problem-sets
code from different problem sets

        '''
        #fwk5jn#
        #prob 6#########################
        set.seed(1337)

        p6<- function(xval, beta0, beta1, sigma)
        {y= beta0 + beta1*xval + rnorm(length(xval), mean=0, sd=sigma)
        return(y)}

        xs<- seq(-10,10,by=0.1)

        y6<-p6(xval=xs, beta0 = 3, beta1 = 0.2, sigma = 1)
        y6
        regp6<-lm(y6~xs)
        regp6

        #I would expect the values of my slope and intercept to be close to intercept=3
        #and slope=0.2 based on the fact that the sample size in this example n=200 is large.

        #partb#########################

        summary(regp6)
        #I can verify that my function is written correctly because my p-value is
        #very small.

        #partc#########################
        plot(regp6)
        plot(resid(regp6))

        #after observing the residual plot I can verify that my function is written correctly
        #because the residuals are spread out evenly and consistently.

        #fwk5jn#
        #7##############################
        #Y = sin(X) + 0.2X + error######
        #a##############################
        # Write a function that simulates Y = sin(X) + 0.2X + error. This function should be
        #similar to the function you wrote in question 6.####
        set.seed(20)
        prob7<- function(x, sigma)
        {
          length7<- length(x)
          y7<- array(0, length7)

          for (i in 1:length7)
          {
            error = rnorm(1, mean = 0, sd = sigma)
            y[i] = sin(x) + 0.2 * x[i] + error
          }
          return(y)
        }
        # Generate your values for X using seq(-10,10,by=0.1)###
        x7 <- seq(-10,10,by=0.1)
        # Simulate values for Y using your function from 7a and the values of X from
        #7(b)##################
        repetitions <- 10000

        simulation7 <- array(0, repetitions)

        prediction7 <- array(0, repetitions)

        for (i in 1:repetitions)
        {
          y <- prob7(x7, sigma = 1)
          #run regression#######
          regression7 <- lm(y~x7)

          x0 <- as.numeric(regression7$coeff[1])

          x1 <- as.numeric(regression7$coeff[2])
          # Simulate a new data point y0 at x0 = 7. Be sure to store this value###

          simulation7i <- prob7(7, sigma = 1)

          prediction7i <- x0 + x1 * 7
        }

        #calculate the expected test MSE###########
        testmse <- mean((simulation7 - prediction7)^2)
        testmse
        averagepredict <- mean(prediction7)
        averagepredict
        actualvalue7 <- sin(7) + 0.2 * 7

        #calculate each of the three sources of error, and list their values###

        variance7 <- mean((prediction7 - averagepred7)^2)
        variance7
        bias7 <- ((actualvalue7 - averagepred7)^2)
        bias7
        variance_error7 <- mean((simulation7 - actualvalue7)^2)
        variance_error7
        #add all of the errors#########

        mseanderr<- variance7 + bias7 + variance_error7
        mseanderr

        # hw2

        # prob6
        library(ISLR)
        ?ifelse
        Auto
        # a ##################################
        hilow<- ifelse(Auto$mpg > median(Auto$mpg), 1, 0)
        #hilow

        # b ##################################
        # We would not want to include the mpg variable as it would be redundant, especially if we
        # plan to utilize hilow to predict mpg. This is an example of well-separation and if we try
        # running a regression with both these variables, R would not run this command.

        # c ##################################
        boxweight<- boxplot(Auto$weight~hilow)

        boxyear<- boxplot(Auto$year~hilow)


        #the boxplot shows me that cars with higher weight and that are older,
        #generally have a greater mpg which would be above the median mpg of 22.75
        #mpg. This may seem intuitive but it is nice to be able to use the boxplot
        #to confirm.
        #boxweight
        #boxyear

        # d ###################################
        set.seed(10)
        hilow_data<- data.frame(Auto,hilow)
        sample.data<-sample.int(nrow(hilow_data), floor(.50*nrow(hilow_data)), replace = F)
        train<-hilow_data[sample.data, ]
        test<-hilow_data[-sample.data, ]
        train
        result_train<-glm(hilow~weight+year, family=binomial, data=train)
        result_train
        preds<-predict(result_train,newdata=test, type="response")
        preds

        # e ####################################
        library(ROCR)
        rates<-prediction(preds, test$hilow)
        roc_result<-performance(rates,measure="tpr", x.measure="fpr")
        roc_result

        plot(roc_result, main="ROC Curve")
        lines(x = c(0,1), y = c(0,1), col="red")

        #the ROC curve we obtained here tells us that our test data is better
        #than random.

        # f #####################################
        confusion.mat<-table(test$hilow,preds > 0.5)
        confusion.mat
        overall.error<- (confusion.mat[1,2] + confusion.mat[2,1]) /sum(confusion.mat)
        overall.error

        # stat 4630
        # prob 1
        # fwk5jn

        x <- c(70, 75, 80, 80, 85, 90)
        y <- c(75, 82, 80, 86, 90, 91)

        lin.reg.1 <- lm(y ~ x)

        res_prob_1 <- residuals(lin.reg.1)
        mse_prob_1 <- (1/length(res_prob_1))*sum((res_prob_1)^2)

        res.test <- NULL

        for (i in 1:6){

          x.training.set <- x[-i]
          y.training.set <- y[-i]
          lin.reg.training <- lm(y.training.set ~ x.training.set)
          beta_zero <- as.numeric(lin.reg.training$coefficients[1])
          beta_one <- as.numeric(lin.reg.training$coefficients[2])
          y.hat <- beta_zero + beta_one * x[i]
          res.test[i] <- (y[i] - y.hat)
        }

        LOOCV_mse <- (1/6)*sum(res.test^2)
        # stat 4630
        # prob 2
        # fwk5jn

        data=load("hw3_bootstrap_samples.RData")
        boot.samples

        # part_a

        #?quantile

        gpa_15students = c(3.68, 3.42, 3.23, 3.87, 3.91, 3.67, 2.89, 3.14, 3.70, 3.56, 3.49, 3.02, 3.16, 3.76, 3.41)
        p2a<- quantile(gpa_15students)

        interquartile = p2a[4]-p2a[2]
        interquartile

        # part_b

        # USE LAB 5 TO FIND BOOTSTRAP
        quarts = NULL

        for (i in 1:50){
          boot.quart<- quantile(boot.samples[1:15, i])
          quarts[i]<- boot.quart[4]-boot.quart[2]
        }
        mean.quartiles<- mean(quarts)
        n=50
        std.error<- sd(quarts)
        quarts
        mean.quartiles
        std.error

        # part_c
        tval = qt(0.96, n-1)
        upper = mean.quartiles + tval*std.error
        upper
        lower = mean.quartiles - tval*std.error
        lower
        # stat 4630
        # prob 6
        # fwk5jn

        library(MASS)  
        library(ISLR)
        # part a
        hilow <-ifelse(Auto$mpg > median(Auto$mpg), 1, 0)

        data1 <- data.frame(Auto,hilow)
        data1
        data2 <- data.frame(data1)[, -c(1,6,8)]
        data2

        # part b
        boxplot(Auto$cylinders ~data2$hilow, xlab ="mpg", ylab= "cylinders")
        boxplot(Auto$displacement ~ data2$hilow, xlab ="mpg", ylab= "displace")
        boxplot(Auto$horsepower ~data2$hilow, xlab ="mpg", ylab= "hp")
        boxplot(Auto$weight ~data2$hilow, xlab = "mpg", ylab = "weight") 
        boxplot(Auto$year ~data2$hilow, xlab ="mpg", ylab= "year")
        boxplot(Auto$acceleration ~data2$hilow, xlab ="mpg", ylab= "acc")

        # part c

        # based on my the graphical summaries I produced in part b I would use the predictors:
        # cylinders, displacement, horsepower, weight and year, to classify if a vehicle has high
        # or low gas milage.

        # part d
        hilow <-ifelse(Auto$mpg > median(Auto$mpg), 1, 0)
        Auto<-data.frame(Auto,hilow)
        sample<-sample.int(nrow(Auto), floor(.50*nrow(Auto)), replace = F)
        train<-Auto[sample, ]
        test<-Auto[-sample, ]

        Auto_LDA <- lda(hilow ~ horsepower + cylinders + weight + year + displacement, data = train)

        training_lda <- predict(Auto_LDA, train)
        train_lda_confusion <- table(train$hilow, training_lda$posterior[,2] > 0.5)

        test_lda<- predict(Auto_LDA, test)
        test_lda_confusion <- table(test$hilow, test_lda$posterior[,2] > 0.5)

        qda.Auto <- qda(hilow ~ horsepower + cylinders + weight + year + displacement, train)
        qda.Auto

        training_qda <- predict(qda.Auto, train)
        train_qda_confusion <- table(train$hilow, training_qda$posterior[,2] > 0.5)

        test_qda <- predict(qda.Auto, test) #predictions on test data
        test_qda_confusion <- table(train$hilow, test_qda$posterior[,2] > 0.5)

        errorRateQDATrain <- (train_qda_confusion[1,2] + train_qda_confusion[2,1]) /sum(train_qda_confusion)
        errorRateQDATrain
        errorRateLDATrain <- (train_lda_confusion[1,2] + train_lda_confusion[2,1]) /sum(train_lda_confusion)
        errorRateLDATrain
        errorRateQDATest <- (test_qda_confusion[1,2] + test_qda_confusion[2,1]) /sum(test_qda_confusion)
        errorRateQDATest
        errorRateLDATest <- (test_lda_confusion[1,2] + test_lda_confusion[2,1]) /sum(test_lda_confusion)
        errorRateLDATest

        # part e
        # The training data for both are pretty similar. But the test data QDA is substatially higher
        # than the LDA. Therefore, conclude that LDA is preferable.

        # part f
        create 
        for (i in 1:1000){
          sample<-sample.int(nrow(Auto), floor(.50*nrow(Auto)), replace = F)
          train<-Auto[sample, ]
          test<-Auto[-sample, ]

          Auto_LDA <- lda(hilow ~ horsepower + cylinders + weight + year + displacement, data = train)
          training_lda <- predict(Auto_LDA, train)
          train_lda_confusion <- table(train$hilow, training_lda$posterior[,2] > 0.5)

          test_lda<- predict(Auto_LDA, test)
          test_lda_confusion <- table(test$hilow, test_lda$posterior[,2] > 0.5)

          qda.Auto <- qda(hilow ~ horsepower + cylinders + weight + year + displacement, train)
          qda.Auto

          training_qda <- predict(qda.Auto, train)
          train_qda_confusion <- table(train$hilow, training_qda$posterior[,2] > 0.5)

          test_qda <- predict(qda.Auto, test)
          test_qda_confusion <- table(train$hilow, test_qda$posterior[,2] > 0.5)

          errorRateQDATrain[i] <- (train_qda_confusion[1,2] + train_qda_confusion[2,1]) /sum(train_qda_confusion)

          errorRateLDATrain[i] <- (train_lda_confusion[1,2] + train_lda_confusion[2,1]) /sum(train_lda_confusion)

          errorRateQDATest[i] <- (test_qda_confusion[1,2] + test_qda_confusion[2,1]) /sum(test_qda_confusion)

          errorRateLDATest[i] <- (test_lda_confusion[1,2] + test_lda_confusion[2,1]) /sum(test_lda_confusion)
        }

        # part g

        mean(errorRateLDATrain)

        mean(errorRateQDATrain)

        mean(errorRateLDATest)

        mean(errorRateQDATest)

        # again based on the above results it appears that LDA is preferable to QDA
        # stat4630
        # prob4
        # fwk5jn

        # a
        # i believe that ridge regression will preform better in terms of model
        # accuracy. This is because the data set only has five predictors.
        # With a small number of predictors it is likely that multicolliniarity
        # may be present.

        # b
        library(glmnet)
        # head(swiss)
        set.seed(1888)
        sample.data=sample.int(nrow(swiss), floor(.50*nrow(swiss)), replace = F)

        train=swiss[sample.data, ]
        test=swiss[-sample.data, ]

        x=model.matrix(Fertility~.,swiss)[,-1]
        y=swiss$Fertility

        x.train=x[sample.data,]
        x.test=x[-sample.data,]
        y.train=y[sample.data]
        y.test=y[-sample.data]

        # c
        # To choose an appropriate value for the thresh argument in the glmnet
        # function I would apply a plug and chug method. I would use small values
        # for thresh and observe the effect this has on the coefficent values.

        # d
        set.seed(2019)

        ridge.mod=glmnet(x, y, alpha = 0, lambda = 0, thresh = 1e-14)
        coefficients(ridge.mod)

        cv.out=cv.glmnet(x.train,y.train,alpha=0)
        plot(cv.out)
        bestlam=cv.out$lambda.min
        bestlam

        # e
        ridge.pred=predict(ridge.mod,s=bestlam,newx=x.test)
        mean((ridge.pred-y.test)^2)

        # f
        lasso.r=glmnet(x, y, alpha = 1, lambda=0,thresh = 1e-14)
        coefficients(lasso.r)
        summary(lasso.r)

        cv.out=cv.glmnet(x.train,y.train,alpha=1)
        plot(cv.out)

        bestlam=cv.out$lambda.min
        bestlam

        lasso.pred=predict(ridge.mod, s=bestlam, newx = x.test)
        mean((lasso.pred-y.test)^2)

        # g
        ridge.pred.0=predict(ridge.mod,s=0,newx=x.test)
        mean((ridge.pred.0-y.test)^2)

        # h
        # lasso has best accuracy

        # i
        grid=10^seq(10,-2,length=100)

        out.all=glmnet(x,y,alpha=0,lambda=grid,thresh = 1e-14)
        plot(out.all, xvar = "lambda")
        abline(v=log(bestlam), lty=2)
        legend("bottomright", lwd = 1, col = 1:6, legend = colnames(x), cex = .7)

        out.all1=glmnet(x,y,alpha=1,lambda=grid,thresh = 1e-14)
        plot(out.all, xvar = "lambda")
        abline(v=log(bestlam), lty=2)
        legend("bottomright", lwd = 1, col = 1:6, legend = colnames(x), cex = .7)
        # stat4630
        # prob5
        # fwk5jn

        # a
        setwd("C:/Users/firem/Desktop/University of Virginia/fall19/STAT 4630/hw4")
        students<- read.table("students.txt", header = T)
        # head(students)
        attach(students)

        # install.packages("tree")
        library(tree)
        # install.packages("randomForest")
        library(randomForest)
        # install.packages("gbm")
        library(gbm)
        students1<- data.frame(students)[, -c(1)]

        set.seed(2013)
        sample<-sample.int(nrow(students1), floor(.50*nrow(students1)), replace = F)
        train<-students1[sample, ]
        test<-students1[-sample, ]

        # b

        ols<- lm(GPA~., data = train)
        ols.pred<- predict.lm(ols, newx= test)
        mean((ols.pred-test$GPA)^2)


        # c
        tree.class.train<-tree(GPA~., data=train)
        summary(tree.class.train)
        plot(tree.class.train)
        text(tree.class.train, cex=0.75, pretty=0)

        # d
        tree.pred.test<-predict(tree.class.train, newdata=test)
        pred.test<-test[,"GPA"]

        mean((pred.test-tree.pred.test)^2)

        # e
        set.seed(1)
        cv.class<-cv.tree(tree.class.train, K=10)
        cv.class
        trees.num.class<-cv.class$size[which.min(cv.class$dev)]
        trees.num.class

        # f
        prune.class<-prune.tree(tree.class.train, best=trees.num.class)
        prune.class
        plot(prune.class)
        text(prune.class, cex=0, pretty=0)

        # g

        yhat<- predict(prune.class, newdata = test)
        class.test= -test[,"GPA"]
        mean((class.test-yhat)^2)

        # h
        set.seed(2)
        bag.class=randomForest(GPA~., data = train, mtry = 7, importance = TRUE)
        predict.bag=predict(bag.class, newdata = test)
        mean((class.test-predict.bag)^2)
        importance(bag.class)

        # i
        forrest.class=randomForest(GPA~., data = train, mtry = 3, importance = TRUE)
        importance(forrest.class)

        predict.forrest=predict(forrest.class, newdata = test)
        mean((class.test-predict.forrest)^2)

        dummy_var<- ifelse(students1$GPA=="NO", 0, 1)

        new.data= data.frame(students1[,-1], dummy_var)

        train_i=new.data[students1,]
        test_i=new.data[-students1,]

        predict_dummy=test_i$dummy

        boost.class<-gbm(dummy_var~., data=train_i, distribution="bernoulli",shrinkage = 0.0001, n.trees=5000)
        summary(boost.class)
        predict.boost<-predict(boost.class, newdata=test_i, shrinkage= 0.0001, n.trees=5000, type = "response")
        mean((predict.boost - class.test)^2)

        #fwk5jn
        #stat4630

        #problem 4
        pr.out<-prcomp(swiss, scale=TRUE)
        #loading vec
        pr.out$rotation

        #biplot
        biplot(pr.out, scale=0)

        pr.var<-pr.out$sdev^2
        pr.var

        pve<-pr.var/sum(pr.var)
        pve

        par(mfro=c(1,2))
        plot(pve, xlab="Principal Component", ylab="Proportion of Variance Explained", main="Scree Plot", ylim=c(0,1))
        plot(cumsum(pve), xlab="Principal Component", ylab="Cumulative Proportion of Variance Explained", main="Cumulative Proportion", ylim=c(0,1))

        #problem 5
        #install.packages("dendextend")
        library(dendextend)


        #parta
        mtcars.s<-scale(mtcars)

        #partb
        hc.complete<-hclust(dist(mtcars.s), method="complete")
        plot(hc.complete)

        #partc single link
        hc.single<-hclust(dist(mtcars.s), method="single")
        plot(hc.single)

        #partd avg link
        hc.average<-hclust(dist(mtcars.s), method="average")
        plot(hc.average)

        #parte 2 clust
        dend.complete.col4<-color_labels(hc.complete, k=2)
        plot(dend.complete.col4, main="HC with Complete Linkage, 2 Clusters")
        dend.single.col4<-color_labels(hc.single, k=2)
        plot(dend.single.col4, main="HC with Single Linkage, 2 Clusters")
        dend.average.col4<-color_labels(hc.average, k=2)
        plot(dend.average.col4, main="HC with Average Linkage, 2 Clusters")

        #partf 3 clust
        dend.complete.col4<-color_labels(hc.complete, k=3)
        plot(dend.complete.col4, main="HC with Complete Linkage, 3 Clusters")
        dend.single.col4<-color_labels(hc.single, k=3)
        plot(dend.single.col4, main="HC with Single Linkage, 3 Clusters")
        dend.average.col4<-color_labels(hc.average, k=3)
        plot(dend.average.col4, main="HC with Average Linkage, 3 Clusters")

        #partg 4 clust
        dend.complete.col4<-color_labels(hc.complete, k=4)
        plot(dend.complete.col4, main="HC with Complete Linkage, 4 Clusters")
        dend.single.col4<-color_labels(hc.single, k=4)
        plot(dend.single.col4, main="HC with Single Linkage, 4 Clusters")
        dend.average.col4<-color_labels(hc.average, k=4)
        plot(dend.average.col4, main="HC with Average Linkage, 4 Clusters")


        #parth complete link
        grps.com<-cutree(hc.complete,3)
        x<-data.frame(mtcars.s,grps.com)
        gp1.c<-subset(x, grps.com==1)
        gp2.c<-subset(x, grps.com==2)
        gp3.c<-subset(x, grps.com==3)

        nrow(gp1.c)
        nrow(gp2.c)
        nrow(gp3.c)

        apply(gp1.c[,c(1:9)],2,mean)
        apply(gp2.c[,c(1:9)],2,mean)
        apply(gp3.c[,c(1:9)],2,mean)


        #parti single link
        grps.single<-cutree(hc.single,3)
        x<-data.frame(mtcars.s,grps.single)
        gp1.c<-subset(x, grps.single==1)
        gp2.c<-subset(x, grps.single==2)
        gp3.c<-subset(x, grps.single==3)

        nrow(gp1.c)
        nrow(gp2.c)
        nrow(gp3.c)

        apply(gp1.c[,c(1:9)],2,mean)
        apply(gp2.c[,c(1:9)],2,mean)
        apply(gp3.c[,c(1:9)],2,mean)

        #partj avg link
        grps.avg<-cutree(hc.average,3)
        x<-data.frame(mtcars.s,grps.avg)
        gp1.c<-subset(x, grps.avg==1)
        gp2.c<-subset(x, grps.avg==2)
        gp3.c<-subset(x, grps.avg==3)

        nrow(gp1.c)
        nrow(gp2.c)
        nrow(gp3.c)

        apply(gp1.c[,c(1:9)],2,mean)
        apply(gp2.c[,c(1:9)],2,mean)
        apply(gp3.c[,c(1:9)],2,mean)
        '''
