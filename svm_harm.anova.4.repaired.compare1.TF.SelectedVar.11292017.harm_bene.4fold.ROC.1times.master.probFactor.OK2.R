unlink(".RData") 
library(caret)
library(ROCR)
require(plyr)

compute_auc <- function(A,B, mod){
        preProcValues <- preProcess(A, method = c("center", "scale"))
        newdata <- predict(preProcValues, A)
        pred <- predict(mod$finalModel, newdata=newdata, type="prob")
	pred1 <- prediction(pred[,2], (as.numeric(B)-1))
        auc <- performance(pred1, "auc")@y.values[[1]]
        return(auc)
}


require(ggplot2)

compute_perf <- function(A,B, mod){
        preProcValues <- preProcess(A, method = c("center", "scale"))
        newdata <- predict(preProcValues, A)
        pred <- predict(mod$finalModel, newdata=newdata, type="prob")
        pred1 <- prediction(pred[,2], (as.numeric(B)-1))
        perf <- performance(pred1,"tpr","fpr")
        return(perf)
}


generateFolds <- function(x,k){
    n <- nrow(x)
    x$folds <- rep(1:k,length.out = n)[sample(n,n)]
    x
}

set.seed(2)
load("final_score_max_exist_onezero_harm_bene.all.finalscore.08292017.Updnexist.one_zero.harm_bene.RData")
final_score_max_exist_onezero_harm_bene_for_1to1 <- final_score_max_exist_onezero_harm_bene[final_score_max_exist_onezero_harm_bene$V34==1,]

n_harm_bene <- nrow(final_score_max_exist_onezero_harm_bene_for_1to1)


times=1 	#####  number of times to select no_harm and make 121
rep=1		#####  number of repetition of fold-cv
fold=4 		#####


n_master_test = n_harm_bene/fold


png(file="ROC.test2.png", width = 5, height = 5, units = 'in',res=600)

auc <- c()
auc5 <- c()

for(k in 1:times){ 

	master_test <- final_score_max_exist_onezero_harm_bene_for_1to1[sample(1:nrow(final_score_max_exist_onezero_harm_bene_for_1to1), n_master_test, replace=FALSE, ), ]

	for_cv <- final_score_max_exist_onezero_harm_bene_for_1to1[-(as.numeric(rownames(master_test))),]
	n_for_cv <- nrow(for_cv)

	final_score_max_exist_onezero_no_harm <- final_score_max_exist_onezero_harm_bene[final_score_max_exist_onezero_harm_bene$V34==0,]
	final_score_max_exist_onezero_no_harm_for_1to1_for_master_test <- final_score_max_exist_onezero_no_harm[sample(1:nrow(final_score_max_exist_onezero_no_harm), n_master_test, replace=FALSE, ), ]
	for_sample_for_cv_no_harm <- final_score_max_exist_onezero_no_harm[-(as.numeric(rownames(final_score_max_exist_onezero_no_harm_for_1to1_for_master_test))), ]
	no_harm_for_cv_1to1 <- for_sample_for_cv_no_harm[sample(1:nrow(for_sample_for_cv_no_harm), n_for_cv, replace=FALSE, ), ]

	final_score_max_exist_onezero_harm_bene_for_cv <- rbind(for_cv, no_harm_for_cv_1to1)
	final_score_max_exist_onezero_harm_bene_for_master_test <- rbind(master_test, final_score_max_exist_onezero_no_harm_for_1to1_for_master_test)

	print(k)
 	        control <- trainControl(method="repeatedcv", number=4, repeats=3, classProb=TRUE)
		A <- final_score_max_exist_onezero_harm_bene_for_cv[,c(3,4,5,6,7,8,9,14,15)]
                B <- final_score_max_exist_onezero_harm_bene_for_cv [,34]
                B <- as.factor(B)
		levels(B) <- c("nonDDI", "DDI")	
		mod_for_cv_all <- train(A,B,method='svmRadial',tuneLength=5,trControl=control,preProcess=c("center","scale"), tuneGrid = data.frame(.C = c(.25, .5, 1),.sigma = .05) )
                A <- final_score_max_exist_onezero_harm_bene_for_master_test[,c(3,4,5,6,7,8,9,14,15)] 
                B <- final_score_max_exist_onezero_harm_bene_for_master_test[,34]
		B <- factor(B)
		levels(B) <- c("nonDDI", "DDI")
		perf <- compute_perf(A,B, mod_for_cv_all)
		if(k==1){
                	plot(perf, col=1, lty=3,lwd=2,  xaxs="i", yaxs="i")
                }else{
               		plot(perf, add = TRUE, col = 1, lty=3, lwd=2)
                }
		auc <- c(auc,compute_auc(A,B, mod_for_cv_all))
		print(varImp(mod_for_cv_all, scale = FALSE)) 	
		
		A <- final_score_max_exist_onezero_harm_bene_for_cv[,c(3,4,5,6,7,8,9)]            
                B <- final_score_max_exist_onezero_harm_bene_for_cv [,34]
                B <- as.factor(B)
                levels(B) <- c("nonDDI", "DDI")
		mod_for_cv_all <- train(A,B,method='svmRadial',tuneLength=5,trControl=control,preProcess=c("center","scale"), tuneGrid = data.frame(.C = c(.25, .5, 1),.sigma = .05) )
                A <- final_score_max_exist_onezero_harm_bene_for_master_test[,c(3,4,5,6,7,8,9)]            
                B <- final_score_max_exist_onezero_harm_bene_for_master_test[,34]
                B <- as.factor(B)
                levels(B) <- c("nonDDI", "DDI")
		perf5 <- compute_perf(A,B, mod_for_cv_all)
                plot(perf5, add = TRUE, col = 5 , lty=3, lwd=2)
		auc5 <- c(auc5,compute_auc(A,B, mod_for_cv_all))

		
}

print(auc)
print(mean(auc))
print(sd(auc))
print(auc5)
print(mean(auc5))
print(sd(auc5))

abline(0,1,lwd=3,col="lightgray", lty=2)

dev.off()
