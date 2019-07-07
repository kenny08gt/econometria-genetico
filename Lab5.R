library(ISLR)
library(MASS)
library(caret)
library(dplyr)


##### Separacion de la salida y entrada

get_chromosome <- function(train,label,x){
  col_names <- names(train)
  inputs<-
    setdiff(col_names,label)
  cromosoma<-
    sample(c(0,1),size = length(inputs),replace = TRUE )
  if(sum(cromosoma)==0){
    cromosoma<-rep(1,length(inputs)) 
  }
  output <- cromosoma
  return(output)
}

get_fitness <- function(chromosome,train,test,label){
  print(chromosome)
  col_names <- names(train)
  inputs<-
    setdiff(col_names,label)
  chromosome <- as.logical(chromosome)
  formula<-paste0(inputs[chromosome],collapse = '+')
  formula <- paste0(label,"~",formula)
  fit <- lm(formula,data=train)
  pred<-predict(fit,test)
  error_square <- sum((pred-test$medv)^2)
  return(error_square)
}

get_fitness_1_x <- function(chromosome,train,test,label){
  print(chromosome)
  col_names <- names(train)
  inputs<-
    setdiff(col_names,label)
  chromosome <- as.logical(chromosome)
  exes <- inputs[chromosome]
  formula<-paste0("I(1/",exes,")", collapse = '+')
  formula <- paste0(label,"~",formula)
  fit <- lm(formula,data=train, na.action = na.exclude)
  pred<-predict(fit,test)
  error_square <- sum((pred-test$medv)^2)
  return(error_square)
}

get_fitness_x_2 <- function(chromosome,train,test,label){
  print(chromosome)
  col_names <- names(train)
  inputs<-
    setdiff(col_names,label)
  chromosome <- as.logical(chromosome)
  exes <- inputs[chromosome]
  formula<-paste0("I(",exes,"^2)", collapse = '+')
  formula <- paste0(label,"~",formula)
  fit <- lm(formula,data=train, na.action = na.exclude)
  pred<-predict(fit,test)
  error_square <- sum((pred-test$medv)^2)
  return(error_square)
}


get_fitness_x_3 <- function(chromosome,train,test,label){
  print(chromosome)
  col_names <- names(train)
  inputs<-
    setdiff(col_names,label)
  chromosome <- as.logical(chromosome)
  exes <- inputs[chromosome]
  formula<-paste0("I(",exes,"^3)", collapse = '+')
  formula <- paste0(label,"~",formula)
  fit <- lm(formula,data=train, na.action = na.exclude)
  pred<-predict(fit,test)
  error_square <- sum((pred-test$medv)^2)
  return(error_square)
}


crossover <- function(parents){
  p1 <- parents[[1]]
  p2 <- parents[[2]]
  chromosome_len <- length(p1)
  mask1<-rep(0,ceiling(chromosome_len-chromosome_len/2))
  mask2 <- rep(1,chromosome_len/2)
  mask_last_half <-c(mask1,mask2)
  mask1<-rep(1,ceiling(chromosome_len-chromosome_len/2))
  mask2 <- rep(0,chromosome_len/2)
  mask_first_half <- c(mask1,mask2)
  child1 <- mask_first_half*p1+mask_last_half*p2
  child2 <- mask_first_half*p2+mask_last_half*p1
  return(list(child1,child2) )
}


select_mating_parents <- function(x,pop_size,roullete,population){
  sum_fit_p<-
    sample(1:sum(roullete$rank),size = 1 )
  pindex<-
    roullete %>% 
    filter(cumsum_rank<sum_fit_p) %>%
    nrow()
  p1<-roullete[pindex+1,] %>% pull(parent)
  sum_fit_p<-
    sample(1:sum(roullete$rank),size = 1 )
  pindex<-
    roullete %>% 
    filter(cumsum_rank<sum_fit_p) %>%
    nrow()
  p2<-roullete[pindex+1,] %>% pull(parent)
  return(population[c(p1,p2)])
}

# mutation <- function(child,rate=0.01){
#   bit_candidate<-
#   sample(1:length(child),size=1)
#   prob <- runif(1)
#   if(prob<=rate){
#     if(child[bit_candidate]==0){
#       child[bit_candidate]<-1
#     }else{
#       child[bit_candidate]<-0
#     }
#   }
#   return(child)
# }


mutation <- function(child, rate=0.01){
  mask <- sample(c(1,0), length(child), replace = TRUE, prob = c(rate, 1-rate))
  mutation.child <- xor(child, mask)*1.0
  return(mutation.child)
}

### Train and Test Creation

train_index <- createDataPartition(Boston$medv,p=0.75,list = F)
train <- Boston[train_index,]
test <- Boston[-train_index,]

pop_size <- 100

population<-
  lapply(1:pop_size, get_chromosome, train=train, label="medv")


fitness <- lapply(population, 
                  get_fitness , 
                  train=train,test=test, label="medv")


roullete<-
  tibble(parent=1:pop_size, fitness= fitness %>% unlist()) %>%
  arrange(desc(fitness))


roullete$rank <- 1:nrow(roullete)

roullete <-
  roullete %>% 
  mutate(cumsum_rank = cumsum(rank))

# undebug(select_mating_parents)
# select_mating_parents(1,pop_size=pop_size,
#                       roullete=roullete, 
#                       population=population)


mating_parents<-
  lapply(1:100, select_mating_parents,
         pop_size=pop_size,
         roullete=roullete, 
         population=population)

children<-
  lapply(mating_parents, crossover)

children<-
  children %>% unlist(recursive = F) 

children<-
  children[1:pop_size]

new_population<-
  lapply(children, mutation, rate=0.15)

