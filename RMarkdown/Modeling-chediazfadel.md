---
title: "Capstone Modeling"
author: "Che Diaz Fadel"
date: "2023-07-27"
output: 
  html_document:
    keep_md: true
    number_sections: yes
    toc: yes
    theme: darkly
    highlight: tango
    fig_width: 15
    fig_height: 10
---



# Support Vector Machine

Support Vector Machine (SVM) seemed like a favorable candidate given it’s high potential for accuracy while still boasting good generalizability. While this may be true, run times proved to be immense, particularly when using the polynomial kernel since it generates many more dimensions (than compared to say RBF kernel) because it not only considers each feature but also their interactions to a certain degree. This can lead to a more accurate classifier if these interactions are important in your data, but it can also cause the SVM to become slow and potentially overfit if the degree of the polynomial is set too high.

# Initial setup

> Load packages and data, define custom functions


```r
# Load packages 
library(caret)
library(kernlab)
library(rminer)
library(matrixStats)
library(knitr)
library(tidyverse)
library(doParallel)

# Set options 
options(tibble.print_max = 40,
        tibble.print_min = 24,
        width = 100,
        pillar.min_title_chars = 15)

# Load data 
app_train <- read_csv("../data/application_train.csv")
bureau_df <- read_csv("../data/bureau.csv")
desired_columns <- read_csv("../data/desired_columns.csv") # DF of handpicked features decided by the group
```

# Standard group dataset for modeling

We decided as a group to use the same dataset when developing our models in order to make apples-to-apples comparisons when determining the best model and features. Below is the standardization I provided and that was used by the group. 2 aggregations of the *bureau.csv* data were provided but ultimately we decided to use `bureau_agg_b`.


```r
# Identify numerically encoded categorical variables ----
num2fac <- app_train %>%
  select(where(is.numeric)) %>%
  mutate(across(everything(), as.character)) %>%
  select(where(~ all(!grepl("\\.|-", .)))) %>%
  #select(-c(own_car_age, hour_appr_process_start, matches("^(obs|def|amt|cnt)"))) %>%
  select(-c(OWN_CAR_AGE, HOUR_APPR_PROCESS_START, matches("^(OBS|DEF|CNT)"))) %>%
  colnames() 

# Create data frame ----
app_train1 <- app_train %>%
  # Select columns from 'desired_columns.csv'
  select(desired_columns$ColumnName) %>% 
  # Handling numerically encoded categorical variables
  mutate(across(c(where(is.character), all_of(num2fac)), factor), 
         # Fixing invalid NA's
         across(c(CODE_GENDER, ORGANIZATION_TYPE), 
                ~case_when(. != "XNA" ~ .)),
         # Replacing NA's in social circle columns with 0
         across(contains("SOCIAL_CIRCLE"), ~replace_na(., 0)),
         # Replacing NA's with 0
         across(c(AMT_REQ_CREDIT_BUREAU_HOUR, AMT_REQ_CREDIT_BUREAU_DAY,
                  AMT_REQ_CREDIT_BUREAU_WEEK, AMT_REQ_CREDIT_BUREAU_MON,
                  AMT_REQ_CREDIT_BUREAU_QRT, AMT_REQ_CREDIT_BUREAU_YEAR),
                ~replace_na(., factor("0"))),
         # Fixing unusual `DAYS_EMPLOYED` values
         DAYS_EMPLOYED = case_when(DAYS_EMPLOYED <= 0 ~ DAYS_EMPLOYED),
         # Creating ordinal version of `OWN_CAR_AGE`
         OWN_CAR_AGE2 = cut(OWN_CAR_AGE,
                            c(0, 5, 10, 15, Inf),
                            right = FALSE),
         OWN_CAR_AGE2 = case_when(FLAG_OWN_CAR == "N" ~ factor("No Car Owned"),
                                  .default = OWN_CAR_AGE2)) %>%
  # Creating aggregate variable from the `EXT_SOURCE_*` variables
  rowwise() %>%
  mutate(AVG_EXT_SCORE = mean(c_across(contains("EXT_")), na.rm = TRUE),
         .after = EXT_SOURCE_3) %>%
  ungroup() %>%
  # Removing rows with NA's in below columns
  filter(if_all(c(FLAG_OWN_CAR, AMT_GOODS_PRICE, AMT_ANNUITY, 
                  CNT_FAM_MEMBERS, DAYS_LAST_PHONE_CHANGE),
                ~!is.na(.)))

# Aggregate bureau data ----
# __ Wide version data frame
bureau_agg_a <- bureau_df %>%
  # Seemed to be the most relevant credit types
  filter(CREDIT_TYPE %in% c("Consumer credit","Credit card","Car loan","Mortgage",
                            "Microloan","Loan for business development","Another type of loan"),
         # Ensure credit was actually given
         AMT_CREDIT_SUM > 0,
         # Thought this made most sense
         CREDIT_ACTIVE %in% c("Active", "Closed"),
         # It's unclear if they convert nominal currency values so kept only most common
         CREDIT_CURRENCY == "currency 1") %>%
  # Clean credit type values to use as column names
  mutate(CREDIT_TYPE = gsub(" +", "_", tolower(CREDIT_TYPE))) %>%
  group_by(SK_ID_CURR, CREDIT_TYPE) %>%
  summarise(avg_credit = mean(AMT_CREDIT_SUM),
            # Decided that average of ratios made more sense than ratio of averages
            avg_overage = mean(AMT_CREDIT_MAX_OVERDUE/AMT_CREDIT_SUM, na.rm = TRUE)) %>%
  pivot_wider(names_from = CREDIT_TYPE,
              values_from = c(avg_credit, avg_overage)) %>%
  ungroup()

# __ Version in EDA ----
bureau_agg_b <- bureau_df %>%
  group_by(SK_ID_CURR) %>%
  summarise(avg_days_credit = mean(DAYS_CREDIT, na.rm = TRUE),
            avg_credit_day_overdue = mean(CREDIT_DAY_OVERDUE, na.rm = TRUE),
            avg_days_credit_enddate = mean(DAYS_CREDIT_ENDDATE, na.rm = TRUE),
            avg_amt_credit_max_overdue = mean(AMT_CREDIT_MAX_OVERDUE, na.rm = TRUE),
            avg_cnt_credit_prolong = mean(CNT_CREDIT_PROLONG, na.rm = TRUE),
            avg_amt_credit_sum = mean(AMT_CREDIT_SUM, na.rm = TRUE),
            avg_amt_credit_sum_debt = mean(AMT_CREDIT_SUM_DEBT, na.rm = TRUE),
            avg_amt_credit_sum_limit = mean(AMT_CREDIT_SUM_LIMIT, na.rm = TRUE),
            avg_amt_credit_sum_overdue = mean(AMT_CREDIT_SUM_OVERDUE, na.rm = TRUE))

#Convert bureau_agg_b$SK_ID_CURR to factor
bureau_agg_b$SK_ID_CURR <- as.factor(bureau_agg_b$SK_ID_CURR)

#Join app_train1 and bureau_agg_b
app_train2 <- app_train1 %>% inner_join(bureau_agg_b)

# Cleanup
remove(app_train, bureau_agg_a, bureau_agg_b, bureau_df, desired_columns, num2fac)
```

# Data Preparation


```r
# Importing specific output for dislpay to reduce notebook runtime
fe_cvcomp1 <- readRDS("../data/fe_cvcomp1.RDS")
svm_final_confmat <- readRDS("../data/svm_final_confmat.RDS")
```



```r
# Trimming data 
app_train3 <- app_train2 %>%
  rename_with(tolower) %>%
  # Removing NA's in certain variables
  filter(if_all(c(code_gender, own_car_age2, avg_ext_score, avg_amt_credit_sum), ~!is.na(.))) %>%
  # Reducing levels in certain variables
  mutate(across(c(occupation_type, name_type_suite), ~replace_na(as.character(.), "unknown")),
         # convert to numeric
         across(matches("^amt_"), as.numeric),
         # Enforce explicit NA's
         code_gender = factor(code_gender, levels = c("F", "M")),
         days_employed = replace_na(days_employed, 10),
         organization_type = replace_na(occupation_type, "none"),
         own_car_age = replace_na(own_car_age, -1),
         avg_amt_credit_sum_debt = replace_na(avg_amt_credit_sum_debt, 0),
         # Median impute
         across(where(~any(is.na(.)) & is.numeric(.)) & !contains("avg_amt_credit_sum_debt"),
                ~replace_na(., median(., na.rm = TRUE))),
         # Modify binary encoding
         across(where(~n_distinct(.) == 2),
                ~case_when(. == .[order(.)[1]] ~ 0,
                           .default = 1)),
         # Convert factor to character
         across(where(is.factor), as.character)) %>%
  select(-c(housetype_mode, sk_id_curr, own_car_age, flag_mobil, flag_emp_phone, flag_work_phone, 
            weekday_appr_process_start, hour_appr_process_start, amt_req_credit_bureau_hour, 
            amt_req_credit_bureau_day,  amt_req_credit_bureau_week,  amt_req_credit_bureau_mon,
            amt_req_credit_bureau_qrt,  days_last_phone_change, contains("flag_document")))

# Create dummy variables
app_train4 <- dummyVars("~ .", app_train3) %>%
  predict(app_train3) %>%
  as_tibble() %>%
  select(where(~n_distinct(.) > 1)) 

# Making splits 
set.seed(123)
train_index <- createDataPartition(app_train4$target, p = 0.8, list = FALSE)

test_sk_id_curr <- app_train2 %>%
  rename_with(tolower) %>%
  filter(if_all(c(code_gender, own_car_age2, avg_ext_score, avg_amt_credit_sum), ~!is.na(.))) %>%
  filter(!row_number() %in% train_index) %>%
  pull(sk_id_curr)

xtrain <- as.data.frame(app_train4[train_index,]) %>%
  mutate(across(where(~n_distinct(.) == 2), factor))

xtest <- as.data.frame(app_train4[-train_index,])  %>%
  mutate(across(where(~n_distinct(.) == 2), factor))
```

I opted to create dummy variables since each level of each factor must be present in test/train set and every fold in k-fold cross validation. Some levels of some factors are not prevalent enough when using large k.

# Modeling Process

## Custom `ksvm` Cross Validation Function

This function performs cross validation of a `ksvm` model per user specifications and optionally returns the model and performance metrics as a data frame.

-   `df`: Data frame to train model with\
-   `tv`: Target variable found in `df`\
-   `kern`: the kernel function used in training and predicting. This parameter can be set to any function, of class kernel, which computes the inner product in feature space between two vector arguments (see `kernlab::kernels`).
kernlab provides the most popular kernel functions which can be used by setting the kernel parameter to the following strings:\
    -   `rbfdot` Radial Basis kernel "Gaussian"\
    -   `polydot` Polynomial kernel\
    -   `vanilladot` Linear kernel\
    -   `tanhdot` Hyperbolic tangent kernel\
    -   `laplacedot` Laplacian kernel\
    -   `besseldot` Bessel kernel\
    -   `anovadot` ANOVA RBF kernel\
    -   `splinedot` Spline kernel\
    -   `stringdot` String kernel\
-   `c`: `C` parameter in `ksvm`, cost of constraint violation (default: 1); the ‘C’-constant of the regularization term in the Lagrange formulation.\
-   `n_folds`: integer, number of folds in k-fold cross-validation.\
-   `seed`: integer, random seed passed to `set.seed`.\
-   `give.model`: logical, should model object be returned?\ 


```r
ksvm_cv <- \(df, tv, kern, c = 1, n_folds = 1, seed = 123, give.model = TRUE){
  # if n_folds <= 1, don't return cv performance tibble
  if (n_folds > 1) {
    # create folds
    set.seed(seed)
    folds <- createFolds(pull(df, {{tv}}), n_folds)
    
    cv_res <- lapply(folds, \(x){
      test <- df[x,]
      train <- df[-x,]
      
      fmodel <- ksvm(pull(train, {{tv}}) ~ ., data = train, kernel = kern, C = c)
      fpred <- predict(fmodel, test)
      
      cm <- confusionMatrix(fpred, pull(test, {{tv}}))
      
      return(c(cm$overall[1], cm$byClass))
    })
    # Performance tibble using metrics from caret's `confusionMatrix`
    perf_df <- tibble(cv_res) %>%
      unnest_wider(cv_res) %>%
      summarise(across(everything(),
                       list(avg = ~mean(., na.rm = TRUE),
                            sd = ~sd(., na.rm = TRUE)))) %>%
      pivot_longer(everything(),
                   names_to = c(".value", "stat"),
                   names_pattern = "(.*)_(.*)")
    
    return(perf_df)
  } else {
    train <- df
    
    fmodel <- ksvm(pull(train, {{tv}}) ~ ., data = train, kernel = kern, C = c)
    fpred <- predict(fmodel, train)
    
    cm <- confusionMatrix(fpred, pull(train, {{tv}}))
    
    if(give.model){
      return(
        list(
          perf_metrics = c(cm$overall[1], cm$byClass),
          confmat = cm,
          preds = fpred,
          ksvm_model = fmodel
        )
      )
    } else {
      return(
        list(
          perf_metrics = c(cm$overall[1], cm$byClass),
          confmat = cm,
          preds = fpred
        )
      )
    }
  }
}
```

## Parallel Cross Validation

Given the scale of the data, it was crucial to train using parallel processing. I used `foreach` from the `parallel` package to accomplish this.


```r
# Parameter grid for two different kernels
kgrid <- expand.grid(kernel = c("polydot"),
                     C = c(0.01, 2, 3)) %>%
  bind_rows(expand.grid(kernel = c("rbfdot"),
                        C = c(1, 5, 20, 60, 200)
  )) %>%
  as_tibble() %>%
  mutate(kernel = as.character(kernel))

# Use smaller sample to decrease outrageous run times
set.seed(123)
che <- xtrain %>%
  slice_sample(n = 20000)

cl <- makeCluster(8) # utilize 8 cores
registerDoParallel(cl)

(xtime1 <- Sys.time()) # log start time 
fe_cvcomp <- foreach(i = 1:nrow(kgrid), .packages = c("tidyverse", "caret", "kernlab"), .combine = "bind_rows", .verbose = TRUE) %dopar% {
  fetime1 <- Sys.time() # log iteration start time
  x <- ksvm_cv(che, target, kern = kgrid$kernel[i], c = kgrid$C[i], n_folds = 5, give.model = FALSE) 
  fetime2 <- Sys.time() # log iteration end time
  ttime <- fetime2 - fetime1 # log iteration run time
  
  xdf <- tibble(kernel = kgrid$kernel[i],
                C = kgrid$C[i],
                sec = as.numeric(ttime, units = "secs"),
                x)
  
  xdf
}
xtime2 <- Sys.time() # log end time

stopCluster(cl)
(the_time <- xtime2 - xtime1) # display total run time

fe_cvcomp1 <- fe_cvcomp %>%
  mutate(mins = sec/60,
         .after = sec) 
```


```r
fe_cvcomp1
```

```
## # A tibble: 16 × 17
##    kernel     C   sec  mins stat  Accuracy Sensitivity Specificity `Pos Pred Value` `Neg Pred Value`
##    <chr>  <dbl> <dbl> <dbl> <chr>    <dbl>       <dbl>       <dbl>            <dbl>            <dbl>
##  1 polyd…  1e-2 2265. 37.7  avg   0.660       0.689        0.321           0.922              0.0815
##  2 polyd…  1e-2 2265. 37.7  sd    0.0924      0.109        0.110           0.00359            0.0104
##  3 polyd…  2e+0 2373. 39.6  avg   0.712       0.755        0.213           0.918              0.0623
##  4 polyd…  2e+0 2373. 39.6  sd    0.127       0.150        0.140           0.00189            0.0163
##  5 polyd…  3e+0 2392. 39.9  avg   0.638       0.662        0.360           0.922              0.0882
##  6 polyd…  3e+0 2392. 39.9  sd    0.169       0.197        0.164           0.00770            0.0133
##  7 rbfdot  1e+0  305.  5.09 avg   0.921       1            0               0.921            NaN     
##  8 rbfdot  1e+0  305.  5.09 sd    0.000112    0            0               0.000112          NA     
##  9 rbfdot  5e+0  490.  8.17 avg   0.921       0.999        0.00127         0.921              0.117 
## 10 rbfdot  5e+0  490.  8.17 sd    0.000209    0.000192     0.00173         0.000195           0.162 
## 11 rbfdot  2e+1  563.  9.38 avg   0.918       0.997        0.00127         0.921              0.0356
## 12 rbfdot  2e+1  563.  9.38 sd    0.000671    0.000654     0.00173         0.000214           0.0512
## 13 rbfdot  6e+1  673. 11.2  avg   0.916       0.994        0.00443         0.921              0.0596
## 14 rbfdot  6e+1  673. 11.2  sd    0.000975    0.000783     0.00480         0.000445           0.0651
## 15 rbfdot  2e+2  876. 14.6  avg   0.912       0.989        0.0101          0.921              0.0754
## 16 rbfdot  2e+2  876. 14.6  sd    0.00194     0.00217      0.00347         0.000337           0.0238
## # ℹ 7 more variables: Precision <dbl>, Recall <dbl>, F1 <dbl>, Prevalence <dbl>,
## #   `Detection Rate` <dbl>, `Detection Prevalence` <dbl>, `Balanced Accuracy` <dbl>
```

## 8 Fold Majority Class Classifier

Through the tuning process, I learned very quickly that SVM is very computationally expensive. To solve this I cut the train set into 8 folds, made 8 models and predictions and the majority prediction became the final prediction. In the case of a tie, the prediction is set to 0 since it's the majority class for the dataset.


```r
# Make folds
set.seed(123)
train_folds <- createFolds(xtrain$target, 8)

# 8 core parallel processing
cl <- makeCluster(8)
registerDoParallel(cl)

(xtime1 <- Sys.time())
mmc <- foreach(i = 1:8, .packages = c("tidyverse", "caret", "kernlab"), .combine = "c") %dopar% {
  xdf <- xtrain[train_folds[[i]], ]
  
  xmodel <- ksvm(target ~ ., data = xdf, kernel = "polydot", C = 3)
  xpreds <- predict(xmodel, xtest)
  
  xlist <- list(fold = i, model = xmodel, preds = xpreds)
  
  xlist
  
}
xtime2 <- Sys.time()
(ttime <- xtime2 - xtime1)

stopCluster(cl)

final_preds <- tibble(x1 = names(mmc),
                      x2 = mmc) %>%
  filter(x1 == "preds") %>%
  mutate(x1 = paste0(x1, row_number())) %>%
  pivot_wider(names_from = x1,
              values_from = x2) %>%
  unnest(cols = c(preds1:preds8)) %>%
  rowwise() %>%
  mutate(n0 = sum(c_across(preds1:preds8) == 0)) %>%
  ungroup() %>%
  mutate(pred = case_when(n0 >= 4 ~ 0,
                          .default = 1),
         sk_id_curr = test_sk_id_curr) %>%
  select(sk_id_curr, pred)
```

# Model Performance


```r
svm_final_confmat <- confusionMatrix(factor(final_preds$pred), xtest$target)
```

Given the business case, it seems wise to optimize around maximizing Specificity (if target class = 0) since the cost of default likely far exceeds the opportunity cost of a new customer. This model finished with an unimpressive 86.3% accuracy and a surprisingly hard to achieve 9.75% Specificity.


```r
svm_final_confmat
```

```
## Confusion Matrix and Statistics
## 
##           Reference
## Prediction     0     1
##          0 45041  3677
##          1  3516   397
##                                           
##                Accuracy : 0.8633          
##                  95% CI : (0.8604, 0.8663)
##     No Information Rate : 0.9226          
##     P-Value [Acc > NIR] : 1.00000         
##                                           
##                   Kappa : 0.0255          
##                                           
##  Mcnemar's Test P-Value : 0.05922         
##                                           
##             Sensitivity : 0.92759         
##             Specificity : 0.09745         
##          Pos Pred Value : 0.92452         
##          Neg Pred Value : 0.10146         
##              Prevalence : 0.92259         
##          Detection Rate : 0.85579         
##    Detection Prevalence : 0.92565         
##       Balanced Accuracy : 0.51252         
##                                           
##        'Positive' Class : 0               
## 
```
