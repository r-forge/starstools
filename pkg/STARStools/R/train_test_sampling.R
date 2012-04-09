#' train_test_sampling
#' @param data TODO
#' @param factor TODO
#' @param train_p TODO
#' @param test_p TODO
#' @export


train_test_sampling <- function(data,factor,train_p=0.5,test_p=0.5)
{
	if(train_p+test_p>1.0)
	{
		print("Train plus test cannot exceed 1.0")
		return()
	}
	# First determine the number of samples per factor
	factor_sample=table(factor)
	factor_names=row.names(factor_sample)
	
	data_ids=1:length(data[,1])
	
	single_factor_training_test=function(data,factor,train_p,test_p,single_factor)
	{
		single_factor_data=data[factor==single_factor,]
		single_factor_N=length(single_factor_data[,1])
		single_factor_id=1:single_factor_N
		train_p_N=floor(train_p*single_factor_N)
		test_p_N=floor(test_p*single_factor_N)
		single_factor_train_test_sample=sample(single_factor_id,size=train_p_N+test_p_N,replace=FALSE)
		train_ids=single_factor_train_test_sample[1:train_p_N]
		test_ids=single_factor_train_test_sample[(train_p_N+1):(train_p_N+test_p_N)]
		single_factor_data$train_test=NA
		
		single_factor_data$train_test[train_ids]="train"
		single_factor_data$train_test[test_ids]="test"
		return(single_factor_data)
	}
	
	data_w_train_test=list_to_data.frame(mapply(single_factor_training_test,single_factor=factor_names,
			MoreArgs=list(data=data,factor=factor,train_p=train_p,test_p=test_p),SIMPLIFY=FALSE))
	return(data_w_train_test)
}
