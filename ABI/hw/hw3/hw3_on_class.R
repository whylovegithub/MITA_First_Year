library(fastDummies)

ad = Advertisement_State
ad_state_dummy <- dummy_cols(ad,select_columns = "State")
ad_state_dummy

lm1 = lm(sales_state~State_CT+State_DL + State_MD + State_NJ + State_NY,data=ad_state_dummy)
summary(lm1)
