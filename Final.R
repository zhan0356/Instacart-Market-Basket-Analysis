##### Project Information ---------------------------------------------------------------------------------------------
#Title: Instacart Market Basket Analysis
#URL: https://www.kaggle.com/c/instacart-market-basket-analysis
#Group Member: 
#Arijit Banerjee	:G1700694F 
#Lyu Yifei	:G1700692A 
#Zhang Siyuan	:G1700585C 
#Zhong Qishuai	:G1700630H




##### Segment 1: Load library -----------------------------------------------------------------------------------------
library(data.table)
library(dplyr)
library(tidyr)
library(ggplot2)
library(treemap)




##### Segment 2: Load datasets ----------------------------------------------------------------------------------------
#Assign raw datasets path according to the system setting
path <- "C:\\Users\\ADMIN\\RInAction\\AWAssignmentData"

aisles <- fread(file.path(path, "aisles.csv"))
departments <- fread(file.path(path, "departments.csv"))
orderp <- fread(file.path(path, "order_products__prior.csv"))
ordert <- fread(file.path(path, "order_products__train.csv"))
orders <- fread(file.path(path, "orders.csv"))
products <- fread(file.path(path, "products.csv"))




##### Segment 3: Methodology and Implementation -----------------------------------------------------------------------
####  Data Pre-processing -----------------------------------------------------------------------------------
###   Data Pre-processing_Reshape -----------------------------------------------------------------
aisles$aisle <- as.factor(aisles$aisle)
departments$department <- as.factor(departments$department)
orders$eval_set <- as.factor(orders$eval_set)
products$product_name <- as.factor(products$product_name)


###   Data Pre-processing_Null Value --------------------------------------------------------------
days_since_prior_order_01 <- replace(orders$days_since_prior_order, is.na(orders$days_since_prior_order), median(orders$days_since_prior_order, na.rm = TRUE))#Generate a new vector which contains the processed value of days_since_prior_order
as.data.frame(days_since_prior_order_01) 
orders<-cbind(select(orders,-days_since_prior_order),days_since_prior_order_01)#Replace the column days_since_prior_order
colnames(orders)[7] <- "days_since_prior_order" #rename it
rm(days_since_prior_order_01)



####  Data Exploration and Feature Engineer -----------------------------------------------------------------
###   Data Exploration and Feature Engineer_Product Level -----------------------------------------
##    Join three datasets: products, aisles, departments ==> products -------------------
products <- products %>% 
  inner_join(aisles) %>% inner_join(departments) %>% 
  select(-aisle_id, -department_id)
rm(aisles, departments)
#     Add userid to dataset ordert to ensure all the userids from ordert are covered by orders
ordert$user_id <- orders$user_id[match(ordert$order_id, orders$order_id)]


##    Join orders and orderp by order_id to form a new dataset orders -------------------
orders_products <- orders %>% inner_join(orderp, by = "order_id")

rm(orderp)
gc()


##    Images on product level -----------------------------------------------------------
#     Plot_1, when do they order: order_hour_of_day --- order count
orders %>% 
  ggplot(aes(x=order_hour_of_day)) + 
  geom_histogram(stat="count",fill="dimgrey")

#     Plot_2, order size: order size --- order count
orders_products%>%
  group_by(order_id)%>%
  summarise(basket_size=last(add_to_cart_order))%>%
  ggplot(aes(x=basket_size))+
  geom_histogram(stat="count",fill="lightcoral") + 
  geom_rug()+
  coord_cartesian(xlim=c(0,80))

#     Plot_3,days since last order: days_since_prior_order --- order count
orders %>% 
  ggplot(aes(x=days_since_prior_order)) + 
  geom_histogram(stat="count",fill="dimgrey")

#     Plot_4, order time within a single day categorized by weekday: hour of the day --- order count
p0 <- ggplot(orders[order_dow == 0, ], aes(x = order_hour_of_day)) +
  geom_bar(fill = c(rep("grey25", 14), "gold", rep("grey25", 9))) +
  theme_minimal() +
  theme(axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        legend.position = "none",
        panel.grid.major = element_blank()) +
  labs(y = "Day 0")

p1 <- ggplot(orders[order_dow == 1, ], aes(x = order_hour_of_day)) +
  geom_bar(fill = c(rep("grey25", 10), "gold", rep("grey25", 13))) +
  theme_minimal() +
  theme(axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        legend.position = "none",
        panel.grid.major = element_blank()) +
  labs(y = "Day 1")

p2 <- ggplot(orders[order_dow == 2, ], aes(x = order_hour_of_day)) +
  geom_bar(fill = c(rep("grey25", 10), "gold", rep("grey25", 13))) +
  theme_minimal() +
  theme(axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        legend.position = "none",
        panel.grid.major = element_blank()) +
  labs(y = "Day 2")

p3 <- ggplot(orders[order_dow == 3, ], aes(x = order_hour_of_day)) +
  geom_bar(fill = c(rep("grey25", 10), "gold", rep("grey25", 13))) +
  theme_minimal() +
  theme(axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        legend.position = "none",
        panel.grid.major = element_blank()) +
  labs(y = "Day 3")

p4 <- ggplot(orders[order_dow == 4, ], aes(x = order_hour_of_day)) +
  geom_bar(fill = c(rep("grey25", 10), "gold", rep("grey25", 13))) +
  theme_minimal() +
  theme(axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        legend.position = "none",
        panel.grid.major = element_blank()) +
  labs(y = "Day 4")

p5 <- ggplot(orders[order_dow == 5, ], aes(x = order_hour_of_day)) +
  geom_bar(fill = c(rep("grey25", 10), "gold", rep("grey25", 13))) +
  theme_minimal() +
  theme(axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        legend.position = "none",
        panel.grid.major = element_blank()) +
  labs(y = "Day 5")

p6 <- ggplot(orders[order_dow == 6, ], aes(x = order_hour_of_day)) +
  geom_bar(fill = c(rep("grey25", 14), "gold", rep("grey25", 9))) +
  theme_minimal() +
  theme(axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        legend.position = "none",
        panel.grid.major = element_blank()) +
  labs(y = "Day 6",
       x = "Hour of the Day")

gridExtra::grid.arrange(p0, p1, p2, p3, p4, p5, p6, ncol = 1)

rm(p0,p1,p2,p3,p4,p5,p6)
gc()


##    prd: dataset for all products based on table orders_products ----------------------
prd <- orders_products %>%
#Arrange according to user_id, order_number, product_id respectively
#Group rows by user_id and product_id to consolidate the times of purchase for each user_id/product_id
#Add the results as a new column product_time
arrange(user_id, order_number, product_id) %>%
group_by(user_id, product_id) %>%
mutate(product_time = row_number()) %>%
ungroup() %>%
group_by(product_id) %>%
summarise(
  prod_orders = n(),#total purchase time by product_id
  prod_interval = mean(days_since_prior_order,na.rm = T),#average days since last order by product_id
  prod_reorders = sum(reordered),#reorder time by product_id
  prod_first_orders = sum(product_time == 1),#total number of first purchase by product_id
  prod_second_orders = sum(product_time == 2),#total number of second purchase by prouct_id
  prod_further_orders = sum(product_time >= 3),
  prod_mean_hour_of_day = mean(order_hour_of_day),
  prod_mean_order_dow = mean(order_dow)
  )

prd$prod_reorder_probability <- prd$prod_second_orders / prd$prod_first_orders
prd$prod_reorder_times <- prd$prod_reorders / prd$prod_first_orders
prd$prod_reorder_ratio <- prd$prod_reorders / prd$prod_orders


##    Images on product level -----------------------------------------------------------
#     Plot_5&6, Treemap 
treeMap_products <- prd %>% inner_join(products, by="product_id")

treemap(treeMap_products,index=c("department","aisle"),vSize="prod_orders",title="orders_by_department",palette="Set3",border.col="#FFFFFF")

treemap(treeMap_products,index=c("department","aisle"),vSize="prod_reorders",title="reorders_by_department",palette="Set3",border.col="#FFFFFF")

rm(treeMap_products)
gc()

#     Plot_7, top 10 products which are purchased for only time: product_name --- product_first_orders
#     Create a subset of prd
pro_onetime<-subset(prd, prod_reorders==0)
pro_onetime <- pro_onetime %>%inner_join(products) %>% 
  select(-prod_second_orders)

pro_onetime<-pro_onetime%>%
  arrange(desc(pro_onetime$prod_first_orders))
pro_onetimes<-pro_onetime[1:10, c("prod_first_orders","product_name")]

pro_onetimes%>%
  ggplot(aes(x=reorder(product_name,-prod_first_orders),y=prod_first_orders))+
  geom_col(fill="lightcoral")+
  xlab('product name')+
  theme(axis.text.x = element_text(angle = 90))

#     Plot_8, top 10 best-selling products: product_name --- product count
#     Create a subset of prd
tmp <- orders_products %>% 
  group_by(product_id) %>% 
  summarize(count = n()) %>% 
  top_n(10, wt = count) %>%
  left_join(select(products,product_id,product_name),by="product_id") %>%
  arrange(desc(count)) 

tmp %>% 
  ggplot(aes(x=reorder(product_name,-count), y=count))+
  geom_bar(stat="identity",fill="cyan3")+
  theme(axis.text.x=element_text(angle=90, hjust=1),axis.title.x = element_blank())

rm(tmp)

#     Plot_9, top ten products  prod_reorder_ratio: product_name --- product count
#     Create a subset of prd
tmp <- prd %>% 
  top_n(10, wt =  prod_reorder_ratio) %>%
  left_join(select(products,product_id,product_name),by="product_id") %>%
  arrange(desc( prod_reorder_ratio))

tmp %>% 
  ggplot(aes(x=reorder(product_name,-prod_reorder_ratio), y=prod_reorder_ratio))+
  geom_bar(stat="identity",fill="cyan3")+
  theme(axis.text.x=element_text(angle=90, hjust=1),axis.title.x = element_blank())

#     Assign tmp to topReoRatio for future usage
topReoRatio<-select(tmp,product_id,prod_reorder_ratio,product_name)
rm(tmp)

#     Delete unnecessary of prd
prd <- prd %>% select(-prod_reorders, -prod_first_orders, -prod_second_orders)
gc()



###   Data Exploration and Feature Engineer_User Level -------------------------------------------- 
##    Create dataset users --------------------------------------------------------------
users <- orders %>%
  filter(eval_set == "prior") %>%
  group_by(user_id) %>%
  summarise(
    user_orders = max(order_number),# number of orders
    user_period = sum(days_since_prior_order, na.rm = T),
    user_mean_days_since_prior = mean(days_since_prior_order, na.rm = T),
    user_mean_hour_of_day = mean(order_hour_of_day),
    user_mean_order_dow = mean(order_dow)
  )


##    Create dataset us -----------------------------------------------------------------
us <- orders_products %>%
  group_by(user_id) %>%
  summarise(
    user_total_products = n(),# Total number of products purchased by user_id
    user_reorder_ratio = sum(reordered == 1) / sum(order_number > 1),#ratio of reordered products by user_id
    user_distinct_products = n_distinct(product_id),#numer of distinct products purchased by user_id
    user_distinct_ratio= user_distinct_products/user_total_products
  )


##    Images on user level --------------------------------------------------------------
#     Plot_10: number of orders for each user: user_orders --- user count
users%>%
  ggplot(aes(x=user_orders))+geom_bar(fill="dimgrey")

#     Plot_11: average user_reorder_ratio
us %>% 
  ggplot(aes(x=user_reorder_ratio)) + 
  geom_point(stat="count",color="lightcoral")

#     Merge users and us
users <- users %>% inner_join(us)
users$user_distinct_ratio <- users$user_distinct_products / users$user_total_products
users$user_average_basket <- users$user_total_products / users$user_orders

#     Plot_12: user mean days since prio orders --- user average basket size
subuser <- users %>% sample_n(1000)
subuser %>% 
  ggplot(aes(x=reorder(user_mean_days_since_prior, -user_average_basket), y=user_average_basket)) + 
  geom_point(stat="identity")+
  theme(axis.ticks.x = element_blank())

#     Delete unnecessary columns
users<-users%>%select(-user_total_products)
us <- orders %>%
  filter(eval_set != "prior") %>%
  select(user_id, order_id, eval_set,order_hour_of_day,
         time_since_last_order = days_since_prior_order)

users <- users %>% inner_join(us)

rm(us,orders)
gc()



###   Data Exploration and Feature Engineer_Reconstruction ----------------------------------------
data <- orders_products %>%
  group_by(user_id, product_id) %>% 
  summarise(
    up_orders = n(),#maximum of row number
    up_first_order = min(order_number),#Sequence of the first order for each product product/user
    up_last_order = max(order_number),#Sequence of last order for each product/user
    up_average_cart_position = mean(add_to_cart_order),
    up_average_order_time = mean(order_hour_of_day)
  )

gc()
data <- data %>% 
  inner_join(prd, by = "product_id") %>%
  inner_join(users, by = "user_id")


##    Plot_13, p_average_cart_position and top 10 reorder ratio:Product name --- average cart position
#     Create a vector to map top 10 products with highest reorder ratio
c <- topReoRatio[,1]
c <- unlist(c, recursive = TRUE, use.names = FALSE)

#     Create subset set which only contains product_id, average cart position and product name
tmp <- orders_products %>%
  group_by(product_id) %>% 
  summarise(p_average_cart_position = mean(add_to_cart_order)) %>%
  left_join(select(products,product_name,product_id),by="product_id")%>%
  arrange(-desc( p_average_cart_position))

#     Dump out all the rows for products with highest reorder ratio
bestSellCart <- tmp[tmp$product_id %in% c,] %>%
  left_join(select(topReoRatio,product_id,prod_reorder_ratio),by="product_id")

bestSellCart %>% 
  ggplot(aes(x=reorder(product_name,-p_average_cart_position), y=p_average_cart_position))+
  geom_bar(stat="identity",fill="cyan3")+
  theme(axis.text.x=element_text(angle=90, hjust=1),axis.title.x = element_blank())

rm(tmp, topReoRatio,orders_products)

gc()


#     purchase count for each product by user / total purchase count by user
data$up_order_rate <- data$up_orders / data$user_orders

data$up_orders_since_last_order <- data$user_orders - data$up_last_order
#     frequency of occurence of each product from first order until last order
data$up_order_rate_since_first_order <- data$up_orders / (
  data$user_orders - data$up_first_order + 1)

data <- data %>% 
  left_join(select(ordert,user_id, product_id, reordered), 
            by = c("user_id", "product_id"))



###   Data Exploration and Feature Engineer_Train dataset and test dataset-------------------------
train <- as.data.frame(data[data$eval_set == "train",])
train$eval_set <- NULL
train$user_id <- NULL
train$product_id <- NULL
train$order_id <- NULL
train$reordered[is.na(train$reordered)] <- 0

test <- as.data.frame(data[data$eval_set == "test",])
test$eval_set <- NULL
test$user_id <- NULL
test$reordered <- NULL

rm(data)
gc()




####  Model -------------------------------------------------------------------------------------------------
###   Logistic regression -------------------------------------------------------------------------
##    get log regression first and add it back as a feature -----------------------------
set.seed(1234)
trains <- train %>% sample_frac(0.1)   #if use selected feature table, this should be changed to new selected feature table

LG <- glm(trains$reordered ~.,family=binomial(link='logit'),data=trains)
summary(LG)
pre=predict(LG,type='response')
trains<-cbind(trains,pre)

pre2=predict(LG,newdata=test,type='response')
test<-cbind(test,pre2)
gc()



###   XGBoost -------------------------------------------------------------------------------------
library(xgboost)
##    Input parameters
params <- list(
  "objective"           = "reg:logistic",  #corresponding learning objective: logistic regression
  "eval_metric"         = "logloss",       #evaluation metrics for validation data: negative log-likelihood
  "eta"                 = 0.1,             #learning_rate: step size shrinkage used in update to prevents overfitting
  "max_depth"           = 6,               #maximum depth of a tree
  "min_child_weight"    = 10,              #minimum sum of instance weight (hessian) needed in a child
  "gamma"               = 0.7,             #minimum loss reduction required to make a further partition on a leaf node of the tree. 
  "subsample"           = 0.76,            #subsample ratio of the training instance.
  "colsample_bytree"    = 0.95,            #subsample ratio of columns when constructing each tree
  "alpha"               = 2e-05,           #L1 regularization term on weights, increase this value will make model more conservative
  "lambda"              = 10               #L2 regularization term on weights, increase this value will make model more conservative
)


##    Build model -----------------------------------------------------------------------
X <- xgb.DMatrix(as.matrix(trains %>% select(-reordered)), label = trains$reordered)
model <- xgboost(data = X, params = params, nrounds = 80)

importance <- xgb.importance(colnames(X), model = model)
xgb.ggplot.importance(importance)


##    Apply Model to Predict ------------------------------------------------------------
Y <- xgb.DMatrix(as.matrix(test %>% select(-order_id, -product_id)))
test$reordered <- predict(model, Y)
test$reordered <- (test$reordered > 0.21) * 1


###   Parameter Tuning ----------------------------------------------------------------------------
library(lattice)
library(caret)
##    Build parameter combinations ------------------------------------------------------
xgb_grid_1 = expand.grid(
  nrounds = 10,
  eta = c(0.1, 0.2, 0.3, 0.4),
  max_depth = c( 4, 5, 6, 7),
  min_child_weight = c(7, 8, 9 ,10),
  subsample = c(0.74, 0.76, 0.78),
  colsample_bytree = c(0.94,0.95,0.96),
  gamma = c(0,1 , 2 , 4)
)


##    Input parameters to measure the performance of grid search ------------------------
xgb_trcontrol_1 = trainControl(
  method = "cv",                                          #The resampling method: cv---cross validation
  number = 2,                                             #number of folds or number of resampling iterations
  verboseIter = TRUE,                                     #A logical for printing a training log.
  returnData = FALSE,                                     #A logical for saving the data
  returnResamp = "all",                                   #save losses across all models
  classProbs = TRUE,                                      #set to TRUE for AUC to be computed
  summaryFunction = twoClassSummary,                      #A function to compute performance metrics across resamples.
  allowParallel = TRUE                                    #A logical for parallel backend
)


##    Train for xgbtree algorithm -------------------------------------------------------
#     The training process will last for hours depending on your setting
#     Once the training is finished, the suggested parameters will be output on console window
#     reordered need to converted to factor and give names to levels
trains$reordered = as.factor(trains$reordered)
levels(trains$reordered) = make.names(levels(as.factor(trains$reordered)))
xgb_train_1 = train(
  x = as.matrix(trains %>% select(-reordered)),
  y = trains$reordered,
  trControl = xgb_trcontrol_1,
  tuneGrid = xgb_grid_1,
  method = "xgbTree"                                      # specify the method to be trained     
)





##### Segment 4: Output -----------------------------------------------------------------------------------------------
submission <- test %>%
  filter(reordered == 1) %>%
  group_by(order_id) %>%
  summarise(
    products = paste(product_id, collapse = " ")
  )

missing <- data.frame(
  order_id = unique(test$order_id[!test$order_id %in% submission$order_id]),
  products = "None"
)

submission <- submission %>% bind_rows(missing) %>% arrange(order_id)
#     Set your own output path
write.csv(submission, file = "submitfinal.csv", row.names = F)