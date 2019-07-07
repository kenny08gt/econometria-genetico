train_index <- createDataPartition(Boston$medv,p=0.75,list = F)
train <- Boston[train_index,]
train[train == 0] <- 0.000000000001

test <- Boston[-train_index,]
test[test == 0] <- 0.000000000001

pop_size <- 1000
population<-
  lapply(1:pop_size, get_chromosome, train=train, label="medv")



for(i in 1:1){
  print(paste0("generation= ",i,collapse="") )
  fitness <- lapply(population, 
                    get_fitness_x_2 , 
                    train=train,test=test, label="medv")
  
  
  roullete<-
    tibble(parent=1:pop_size, fitness= fitness %>% unlist()) %>%
    arrange(desc(fitness))
  
  
  roullete$rank <- 1:nrow(roullete)
  
  roullete <-
    roullete %>% 
    mutate(cumsum_rank = cumsum(rank))
  
  mating_parents<-
    lapply(1:(2*pop_size), select_mating_parents,
           pop_size=pop_size,
           roullete=roullete, 
           population=population)
  
  children<-
    lapply(mating_parents, crossover)
  
  children<-
    children %>% unlist(recursive = F)
  
  children<-
    children[1:pop_size]
  
  top_parent <- roullete %>% tail(1) %>% pull(parent)
  top_parent_fitness <- roullete %>% tail(1) %>% pull(fitness)
  print(population[[top_parent]])
  print(paste0("top parent fitness ",top_parent_fitness,collapse = " ") )
  
  
  population<-
    lapply(children, mutation, rate=0.1)
  
  
}


