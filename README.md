# **<ins>University of Utah MSBA Summer 2023 Capstone</ins>** 
[![](https://img.shields.io/badge/R-RMarkdown_Notebooks-276DC3?logo=R)](https://github.com/chediazfadel/msba_capstone/tree/main/RMarkdown) [![](https://img.shields.io/badge/R-HTML_Notebooks-276DC3?logo=R)](https://github.com/chediazfadel/msba_capstone/tree/main/HTML)

## Business Problem and Project Objective

[Home Credit](https://www.homecredit.ph) is interested in augmenting its customer base to those without existing credit history. Bringing on this population bears associated risks, namely the uncertainty of a potential customer’s ability to repay loans offered by Home Credit. This project aims to minimize the onboarding of customers who will default on their loans by leveraging the transactional data collected by Home Credit. This will be done by employing a supervised classification model designed to maximize overall accuracy, and accuracy relative to the “no information rate” (the proportion of the most prevalent class).

Being able to swiftly and accurately identify which prospects to avoid will decrease customer onboarding costs and increase overall profitability of accounts. Home Credit will be able to streamline acquisition of favorable customers with fewer resources.

The materials delivered will be a model which accepts a list of prospective customers and their associated data, and outputs whether each prospect should be offered a loan. This project will be facilitated by a predefined group of University of Utah MSBA students and consists of two key milestones: exploratory data analysis and modelling. Progress of these milestones will be reported by June 18th, 2023 and July 9th, 2023 respectively. We intend to deliver final materials by July 26th, 2023.

## Group Solution 
Our EDA uncovered major class imbalance in the target variable, where the majority class (no loan repayment difficulties) comprised 91.9% of the observations. Ability to detect the other 8.1% of cases was of primary interest given that they pose the greatest risk to Home Credit. To address this, we developed four classification models tuned for maximum sensitivity (where difficulty to repay is the positive class) and overall accuracy. The models trained were Logistic Regression, Random Forest, Support Vector Machine, and XGBoost.

| Model | Accuracy | Sensitivity |
| ----------- | ----------- | ----------- |
| Logistic Regression | 69% | 67.2% |
| Random Forest | 92.2% | 0.1% |
| Support Vector Machine | 86.3% | 9.75% |
| XGBoost | 91% | 19.5% |

We selected XGBoost for our final model given its balance between our chosen performance metrics. This model identified the following to be the five most predictive metrics:
- External Source (2, 3)
  - The top two performing features were nondescript metrics created by a source outside Home Credit.
- Duration client has been employed.
- Number of days before application the client change their registration.
- Client age.


## Personal Contribution
### Data standardization
The entire dataset consists of seven tables, totaling 219 variables, and the primary training dataset contains 307,511 observations. Given our time and computational constraints, trimming and cleaning the data down into a manageable form was critical. We also wanted to use the same data to train each of our models for compatibility  purposes and to be able to make apples-to-apples model performance comparisons. I provided this standardized set to the group for modeling by distilling aspects of our individual EDA's into a single simplified script that could be easily shared.

### EDA [![](https://img.shields.io/badge/R-EDA-276DC3?logo=R)](https://github.com/chediazfadel/msba_capstone/blob/main/EDA-chediazfadel.md)

We split certain EDA task among the group members and I took on data cleaning, variable encoding verification, and inspecting scale of missing variables.

Missing values were very widespread and while we waited until modeling to decide how to handle them, we mostly employed median imputation. I noticed that there were many categorical variables in this table which are encoded numerically. Evaluating categorical (specifically nominal) variables as numeric is seriously problematic when modeling. Some categorical variables included a category denoting whether or not the data was missing instead of allowing the value be explicitly missing. This lead to intuitively binary variables to have three classes which would obviously be undesirable for modeling.

### Modeling [![](https://img.shields.io/badge/R-Modeling-276DC3?logo=R)](https://github.com/chediazfadel/msba_capstone/blob/main/Modeling-chediazfadel.md)

I elected to train a **Support Vector Machine (SVM)** model given its high potential for accuracy while still boasting good generalizability. While this may be true, run times proved to be immense, particularly when using the polynomial kernel since it generates many more dimensions (than compared to say RBF kernel) because it not only considers each feature but also their interactions to a certain degree. This can lead to a more accurate classifier if these interactions are important in your data, but it can also cause the SVM to become slow and potentially overfit if the degree of the polynomial is set too high.

My final SVM model was not very predictive and making improvements in each iteration was difficult due to the incredible training time. In the end, we decided to present on a different model but I enjoyed the process of applying a problem to a new model and learning from my mistakes along the way.

## Business Value
While the final model we presented on can certainly be improved upon, there is still some value to be gleaned from it. 

Based on the most predictive features, it's clear that it would be worth while for Home Credit to get access to more resources from the "External Source". In reality, I have no idea how realistic this prospect is but it is worth an attempt anyway. The final XGB model boasted a 19.5% Specificity (positive class = difficulty repaying) meaning that compared to a model which simply predicts the majority class, our model could provide Home Credit with a 19.5% average reduction in lending risk. Based on the average credit amount and average normalized interest rate of those found in the data to have difficulty repaying their loans, this equates to about $44,960 in average loss reduction per client.

$$ AVG Credit = C = 193915$$

$$ AVG Interest Rate = R = 193915$$

$$ Specificity = TPR = 0.195$$

$$ Average Loss Reduction Per Client = \Bigl(C \times \left(1 + R\right)\Bigr) \times TPR$$

## Difficulties
The depth and breadth of this data was massive. Cleaning and validating alone proved to be fairly complex, especially since in my opinion the provided data descriptions left much to be desired. These hardships paled in comparison to the time it took to train models. After attempting a "everything but the kitchen sink" approach to feature selection and hyperparameter tuning and still not having a model after 24+ hours, it was clear a more nuanced approach was required. Even training on what seemed to be the minimally acceptable amount of variables and hyperparamters took considerable time and seriously limited our ability to experiment with new methods, combinations of features, and paramater grids after evaluation.

## What Was Learned

I found this to be a very enriching experience. This is the first time I've collaborated on a data project in gitHub and I definitely gained a much better understanding of best practices that will surely be useful in my career. This was also the first time I've trained an SVM model on a real dataset and I'm certainly more familiar with how it works and the benefits and limitations of utilizing certain kernels. I've also walked away with different tools to put in my belt after reading my team members' code and suggested code provided by the instructors. I look forward to employing what I've learned during this project to further excel my career.


