#=========================================================================
# Econ 613 A3
# Yuqi Zhou
#=========================================================================
 
library(tidyverse)
library(readr)
library(ggplot2)
library(mlogit)
library(snow)
library(nloptr)
library(boot)
library(data.table)
library(nnet)

datjss <- read_csv("Desktop/ECON 613/a3/Data/datjss.csv")
datsss <- read_csv("Desktop/ECON 613/a3/Data/datsss.csv")
datstu_v2 <- read_csv("Desktop/ECON 613/a3/Data/datstu_v2.csv")
datstu_v2 <- datstu_v2 %>%
  filter(score!='NA'&rankplace!='NA')
datsss <- datsss %>%
  distinct(schoolcode,.keep_all = TRUE)

#Exercise 1 Basic Statistics
#(a)
school <- datsss$schoolcode
school_num <- length(school)
# Number of schools is 898.
student_num <- nrow(datstu_v2)
# Number of students is 160935.
program_total <- c(datstu_v2$choicepgm1,datstu_v2$choicepgm2,datstu_v2$choicepgm3,
                   datstu_v2$choicepgm4,datstu_v2$choicepgm5,datstu_v2$choicepgm6)
program <- unique(program_total)
program_num <- length(program)
# Number of programs is 32.

#(b)
choices <- matrix(nrow = 898,ncol = 32)
for (iter_col in 1:32){
  for (iter_row in 1:898){
    choices[iter_row,iter_col] = paste(program[iter_col],school[iter_row],sep = ';')
  }          
}
choices
# The school code associated with each shcool and the given programs are listed.

#(c)
count <- 0
code_district <- data.frame(datsss$schoolcode,datsss$sssdistrict)
for (i in 1:160935) {
  found <- FALSE
  v <- c(datstu_v2$schoolcode1[i],datstu_v2$schoolcode2[i],datstu_v2$schoolcode3[i],
         datstu_v2$schoolcode4[i],datstu_v2$schoolcode5[i],datstu_v2$schoolcode6[i])
  for(j in 1:6) {
    if (is.na(v[j]) == FALSE) {
      row_num <- which(code_district$datsss.schoolcode == v[j])
      outcome <- is.element(code_district[row_num,2],datstu_v2$jssdistrict[i])
      for (k in outcome) {
        if (k == TRUE) {
          count <- count + 1
          found <- TRUE
          break
        }
      }
    }
    if (found == TRUE) {break}
  }
}
count
# There are 117503 students applied for at least one senior high schools in the same district to home.

#(d)
ad_count <- integer(898)
school_admitted <- data.frame(school,ad_count)
for (i in 1:160935) {
  ranking <- datstu_v2$rankplace[i]
  v <- c(datstu_v2$schoolcode1[i],datstu_v2$schoolcode2[i],datstu_v2$schoolcode3[i],
         datstu_v2$schoolcode4[i],datstu_v2$schoolcode5[i],datstu_v2$schoolcode6[i])
  if (ranking != 99 && is.na(ranking) == FALSE) {
    code <- v[ranking]
    which_row <- which(school_admitted$school == code)
    count <- school_admitted[which_row,2] 
    school_admitted[which_row,2] <- count + 1
  }
}

#(e)
ad_score <- rep(9999, times=898)
cutoff <- data.frame(school,ad_score)
for (i in 1:160935) {
  ranking <- datstu_v2$rankplace[i]
  program <- c(datstu_v2$schoolcode1[i],datstu_v2$schoolcode2[i],datstu_v2$schoolcode3[i],
               datstu_v2$schoolcode4[i],datstu_v2$schoolcode5[i],datstu_v2$schoolcode6[i])
  if (ranking != 99 && is.na(ranking) == FALSE) {
    code <- program[ranking]
    which_row <- which(cutoff$school == code)
    if (cutoff[which_row,2] > datstu_v2$score[i]) {
      cutoff[which_row,2] <- datstu_v2$score[i]  
    }
  }
}


#(f)
score_sum <- integer(898)
av_score <- data.frame(school,score_sum)
for (i in 1:160935) {
  program <- c(datstu_v2$schoolcode1[i],datstu_v2$schoolcode2[i],datstu_v2$schoolcode3[i],
               datstu_v2$schoolcode4[i],datstu_v2$schoolcode5[i],datstu_v2$schoolcode6[i])
  ranking <- datstu_v2$rankplace[i]
  if (ranking != 99 && is.na(ranking) == FALSE) {
    code <- program[ranking]
    which_row <- which(av_score$school == code)
    av_score[which_row,2] <- av_score[which_row,2] + datstu_v2$score[i]
  }
}
av_score$score_sum <- av_score$score_sum / school_admitted$ad_count


#Exercise 2 Data
scl_pgm_chocies <- c(t(choices))
datsss_clean <- datsss[!duplicated(datsss$schoolcode), ]
district_vec <- rep(datsss_clean$sssdistrict, each = 32)
lat_vec <- rep(datsss_clean$ssslat, each = 32)
long_vec <- rep(datsss_clean$ssslong, each = 32)
cutoff_vec <- rep(cutoff$ad_score, each = 32)
quality_vec<- rep(av_score$score_sum, each = 32)
size_vec <- rep(school_admitted$ad_count, each = 32)
scl_level <- data.frame(scl_pgm_chocies,district_vec,lat_vec,long_vec,cutoff_vec,quality_vec,size_vec)


#Exercise 3 Distance
dat <- merge(datstu_v2,datjss,by = "jssdistrict")
jsslong <- dat$point_x
jsslat <- dat$point_y
ssslong1 <- matrix(nrow = 160935, ncol = 6)
ssslat1 <- matrix(nrow = 160935,ncol = 6)
for (i in 1:160935) {
  program <- c(datstu_v2$schoolcode1[i],datstu_v2$schoolcode2[i],datstu_v2$schoolcode3[i],
               datstu_v2$schoolcode4[i],datstu_v2$schoolcode5[i],datstu_v2$schoolcode6[i])
  for (j in 1:6) {
    if (is.na(program[j]) == FALSE) {
      row_num <- which(datsss_clean$schoolcode == program[j])
      ssslong1[i,j] <- datsss_clean$ssslong[row_num]
      ssslat1[i,j] <- datsss_clean$ssslat[row_num]
    }
  }
}

dist <- matrix(nrow = 160935, ncol = 6)
for (i in 1:160935) {
  for (j in 1:6) {
  dist[i,j] <- sqrt((69.172*(ssslong1[i,j]-jsslong[i])*cos(jsslat[i]/57.3))^2+(69.712*(ssslat1[i,j]-jsslat[i]))^2)
  }
}


#Exercise 4  Dimensionality Reduction
#(a)
code_pgm = strsplit(scl_level$scl_pgm_chocies, split = ";")
separated <- data.frame(matrix(unlist(code_pgm), ncol = 2, byrow = T))
scode_rev = substr(separated$X2,1,3)
scl_level <- scl_level %>% 
  mutate(scode_rev, pgm_rev = separated$X1)


#(b)
scl_level <- scl_level %>% mutate(pgm_rev = case_when(separated$X1 == "General Arts" | separated$X1 == "Visual Arts" ~ "Arts",
                                                      separated$X1 == "Business" | separated$X1 == "Home Economics" ~ "Economics",
                                                      separated$X1 == "General Science" ~ "Science",
                                                      TRUE ~ "Others"))
#(c)
scl_level <- scl_level %>% mutate(choice_rev = paste(scode_rev,pgm_rev, sep = '^_^'))

#(d)
# Add cutoff_rev to scl_level
scl_level = scl_level %>%
  group_by (scode_rev) %>%
  mutate (cutoff_rev=min(cutoff_vec))

# Add quality_rev to scl_level
scl_level = scl_level %>%
  group_by (scode_rev) %>%
  mutate (quality_rev = mean(quality_vec, na.rm = TRUE))

#(e)
top_std  <- datstu_v2 %>%
  arrange(desc(datstu_v2$score)) %>%
  slice_head(n = 20000)
# Now we have the data of 20000 highest score students.

# Exercise 5 First Model
top_std <- top_std %>% 
  mutate(schoolcode_first = substr(top_std$schoolcode1, 1, 3),
         program_first = case_when(top_std$choicepgm1 == "General Arts" | top_std$choicepgm1 == "Visual Arts" ~ "Arts",
                                   top_std$choicepgm1 == "Business" | top_std$choicepgm1 == "Home Economics" ~ "Economics",
                                   top_std$choicepgm1 == "General Science" ~ "Science",
                                   TRUE ~ "Others"),
         choice_rev = paste(schoolcode_first, program_first, sep = '^_^'))

school_simp <- scl_level %>% 
  select(scode_rev, choice_rev, cutoff_rev, quality_rev)
top_std <- top_std %>% 
  left_join(school_simp, by = "choice_rev")

set.seed(1000)
x <- sample(1:nrow(top_std),300)
my_sample <- top_std[x,]
my_sample$choice_rev <- as.factor(my_sample$choice_rev)
my_sample$choice_rev <- as.numeric(my_sample$choice_rev)


#Likelihood Function for Multinomial Logit
mlogit_like_fun = function(param,data) {
  score <- data$score
  ch <- data$choice_rev
  ni <- nrow(data)
  nj <- length(unique(ch))
  out <- mat.or.vec(ni,nj)
  pn1 <- param[1:nj - 1]
  pn2 <- param[(nj):(2 * nj - 1)]
  out[,1] <- rep(0,ni)
  for (j in 2:nj) {
    out[,j] <- pn1[j-1] + score*pn2[j-1]
  }
  prob  = exp(out)       
  prob  = sweep(prob, MARGIN = 1, FUN = "/", STATS = rowSums(prob))
  prob_c = NULL
  for (i in 1:ni) {
    prob_c[i] = prob[i,ch[i]]
  }
  prob_c[prob_c > 0.999999] = 0.999999
  prob_c[prob_c < 0.000001] = 0.000001
  like = sum(log(prob_c))
  return(-like)
}
multi_model <- multinom(choice_rev~score,data=my_sample,maxit=10000)
machine_reg <- summary(multi_model)
machine_reg$coefficients

set.seed(123)
start <- as.vector(machine_reg$coefficients) + runif(76, -0.02, 0.02)

result <- optim(start, fn = mlogit_like_fun, method="BFGS", 
              control = list(trace = 6, REPORT = 1, maxit = 10000), data = my_sample, hessian=TRUE)
par_n <- result$par


#Now let's look at Marginal Effect 
multilogit_prob = function(param,data) {
  score <- data$score
  choice <- data$choice_rev
  ni <- nrow(data)
  nj <- length(unique(choice))
  out <- mat.or.vec(ni, nj)
  pn1 <- param[1:nj - 1]
  pn2 <- param[(nj):(2 * nj - 1)]
  out[, 1] <- rep(0, ni)
  for (j in 2:nj) {
    out[, j] = pn1[j - 1] + score * pn2[j - 1]
  }
  prob_sum <- apply(exp(out), 1, sum)      
  prob <- exp(out)/prob_sum
  return(prob)
}

probij_matrix <- multilogit_prob(par_n, my_sample)
mb <- c(0, par_n[39:76])

multilogit <- matrix(0, nrow = 300, ncol = 39)
for (i in 1:300) {
  beta_bar <- sum(probij_matrix[i, ] * mb)
  multilogit[i, ] <- probij_matrix[i, ] * (mb - beta_bar)
}

multilogit <- apply(multilogit, 2, mean)
multilogit <- as.data.frame(multilogit)
colnames(multilogit) <- 'Marginal Effect'


#Exercise 6 Second Model
clogit_like_fun = function(param, data) {
  quality <- data$quality_rev
  choice <- data$choice_rev
  ni <- nrow(data)
  nj <- length(unique(choice))
  out <- mat.or.vec(ni, nj)
  unique_data <- distinct(data, choice_rev, .keep_all = TRUE)
  unique_choice_quality <- unique_data[order(unique_data$choice_rev), ]$quality_rev
  
  pn1 <- param[1:nj - 1]
  out[, 1] <- rep(0, ni)
  
  for (j in 1:nj) {
    out[, j] <- pn1[j] + param[nj] * unique_choice_quality[j] 
  }
  prob <- exp(out)
  prob <- sweep(prob, MARGIN = 1, FUN = "/", STATS = rowSums(prob))
  prob_c <- NULL
  for (i in 1:ni) {
    prob_c[i] <- prob[i, choice[i]]
  }
  prob_c[prob_c > 0.999999] <- 0.999999
  prob_c[prob_c < 0.000001] <- 0.000001
  like <- sum(log(prob_c))
  return(-like)
}

set.seed(123)
start_2 <- runif(76, -0.02, 0.02)
result2 <- optim(start_2,fn = clogit_like_fun, method = "BFGS",
                 control = list(trace = 6, REPORT = 1, maxit = 10000), data = my_sample, hessian = TRUE)
par_n2 <- result2$par 

# Now let's take a look at the Marginal Effects
clogit_matrix = function(param, data) {
  quality <- data$quality_rev
  choice <- data$choice_rev
  ni <- nrow(data)
  nj <- length(unique(choice))
  out <- mat.or.vec(ni, nj)
  
  uni_data <- distinct(data, choice_rev, .keep_all = TRUE)
  uni_choice_quality <- uni_data[order(uni_data$choice_rev), ]$quality_rev
  pn1 <- param[1:nj - 1]
  out[, 1] <- rep(0, ni)
  
  for (j in 1:nj) {
    out[, j] <- pn1[j] + param[nj] * uni_choice_quality[j]
  }
  prob_sum <- apply(exp(out), 1, sum)      
  prob <- exp(out)/prob_sum
  return(prob)
}

cond_matrix <- clogit_matrix(result2$par, my_sample)
mb2 <- c(0, par_n2[39:76])
condlogit <- matrix(0, nrow = 300, ncol = 39)
for (i in 1:300) {
  for (j in 1:39) {
      condlogit[i, j] = cond_matrix[i, j] * (1 - cond_matrix[i, j] * par_n2[39])
    }
}
condlogit <- apply(condlogit, 2:3, mean)
condlogit <- as.data.frame(condlogit)

#Exercise 7
#(a)
# I would say the conditional logit model is better than the first one. 
# The test scores differ from each individual while quality is the same.
# When rule out the "Others" program choice, we are reducing the choices. 

#(b)

#(c)

excluding <- my_sample %>% filter(program_first != "Others")
result3 <- optim(start_2, fn = clogit_like_fun, method = "BFGS", 
                 control = list(trace = 6, REPORT = 1, maxit = 10000), data = my_sample, hessian = TRUE)
par_n3 <- result3$par
clogit_like_fun(par_m3, my_sample)
clogit_prob_matrix(par_m3, my_sample)


