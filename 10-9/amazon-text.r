library(tokenizers)
library(smodels) # devtools::install_github("statsmaths/smodels")
library(dplyr)
library(ggplot2)
library(stopwords)
library(Matrix)
library(glmnet)

load("amazon-reviews-y2k.rda")

# Get the review helpfulness.
helpful <- reviews$helpful %>% 
  strsplit(",") %>%
  unlist() %>%
  as.numeric %>%
  matrix(byrow=TRUE, ncol=2)

# Keep the reviews where at least 100 people
# characterized the review.
reviews <- reviews[ helpful[,2] >= 100,]
helpful <- helpful[ helpful[,2] >= 100 ,]

prop_helpful <- helpful[,1] / helpful[,2]

# Get the tokens for each of the reviews.
X <- tokenize_words(reviews$reviewText) %>%
  term_list_to_df %>%
  term_df_to_matrix

# Get rid of the stopwords.
X <- X[, !(colnames(X) %in% stopwords())]


intercept <- mean(prop_helpful)
print(intercept)

gfit <- cv.glmnet(X, prop_helpful)

# Plot the out-of-sample mean square errors.
resid_est <- as_tibble(gfit[c("cvm", "cvup", "cvlo", "lambda")]) %>%
  mutate(llambda = log(lambda)) %>%
  arrange(llambda)

ggplot(resid_est, aes(x=llambda, y=cvm, ymin=cvlo, ymax=cvup)) +
  geom_errorbar() + theme_minimal() + geom_point(aes(color="red")) +
  scale_colour_discrete(guide = FALSE) + ylab("Mean-Squared Error") +
  xlab(expression("log"~(lambda)))

beta <- gfit$glmnet.fit$beta[,20]
beta[beta != 0]
