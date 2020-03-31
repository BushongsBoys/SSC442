# Lab 4 

# Install libraries 
library(kernlab)
library(boot)
library(caret)

# load data 
data("spam")
tibble:: as.tibble(spam)

# Explore type column 
is.factor(spam$type)
levels(spam$type)

# Create test and training data 
set.seed(42)
# spam_idx = sample(nrow(spam), round(nrow(spam) / 2))
spam_idx = sample(nrow(spam), 1000)
spam_trn = spam[spam_idx, ]
spam_tst = spam[-spam_idx, ]

# Fit 4 regression models 
fit_caps = glm(type ~ capitalTotal,
               data = spam_trn, family = binomial)
fit_selected = glm(type ~ edu + money + capitalTotal + charDollar,
                   data = spam_trn, family = binomial)
fit_additive = glm(type ~ .,
                   data = spam_trn, family = binomial)
fit_over = glm(type ~ capitalTotal * (.),
               data = spam_trn, family = binomial, maxit = 50)


mean(ifelse(predict(fit_caps) > 0, "spam", "nonspam") != spam_trn$type)
mean(ifelse(predict(fit_selected) > 0, "spam", "nonspam") != spam_trn$type)
mean(ifelse(predict(fit_additive) > 0, "spam", "nonspam") != spam_trn$type)
mean(ifelse(predict(fit_over) > 0, "spam", "nonspam") != spam_trn$type)

# Run cross fold validation
set.seed(1)
cv.glm(spam_trn, fit_caps, K = 10)$delta[1]
cv.glm(spam_trn, fit_selected, K = 5)$delta[1]
cv.glm(spam_trn, fit_additive, K = 5)$delta[1]
cv.glm(spam_trn, fit_over, K = 5)$delta[1]

# confusion matrix 
make_conf_mat = function(predicted, actual) {
  table(predicted = predicted, actual = actual)
}

# Give us predicted values (same output, different ways) 
caps_tst_pred = ifelse(predict(fit_caps, spam_tst) > 0,
                           "spam",
                           "nonspam")

selected_tst_pred = ifelse(predict(fit_selected, spam_tst) > 0,
                           "spam",
                           "nonspam")

additive_tst_pred = ifelse(predict(fit_additive, spam_tst) > 0,
                       "spam",
                       "nonspam")

over_tst_pred = ifelse(predict(fit_over, spam_tst) > 0,
                           "spam",
                           "nonspam")

# Create confusion matrices for eac
caps_matrix = make_conf_mat(predicted = caps_tst_pred, actual = spam_tst$type)
mean(caps_tst_pred != spam_tst$type)
sensitivity(caps_matrix)
specificity(caps_matrix)

selected_matrix = make_conf_mat(predicted = selected_tst_pred, actual = spam_tst$type)
mean(selected_tst_pred != spam_tst$type)
sensitivity(selected_matrix)
specificity(selected_matrix)

additive_matrix = make_conf_mat(predicted = additive_tst_pred, actual = spam_tst$type)
mean(additive_tst_pred != spam_tst$type)
sensitivity(additive_matrix)
specificity(additive_matrix)

over_matrix = make_conf_mat(predicted = over_tst_pred, actual = spam_tst$type)
mean(over_tst_pred != spam_tst$type)
sensitivity(over_matrix)
specificity(over_matrix)

