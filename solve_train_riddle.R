

# On = 1
# Off = 0
init_train <- function(num_cars) {
  return (sample(0:1,num_cars, replace=T))
}

record_start_car <- function(train) {
  return(train[1])
}

move_right_one_and_count_one <- function(current_car,train,cars_passed) {
  print(paste("Moves:",cars_passed))
  current_car <- current_car + 1
  print(paste("Current Car:",current_car))
  return(current_car)
}

actual_car <- function(train,current_car) {
  return(ifelse(current_car %% length(train) == 0,length(train),current_car %% length(train)))
}

is_different <- function(start_car,train,current_car) {
  return(train[actual_car(train,current_car)]!=start_car)
}

change_light <- function(train,current_car) {
  current_car <- actual_car(train,current_car)
  light <- ifelse(train[current_car]==0,1,0)
  train[current_car] <- light
  print(train)
  return(train)
}

move_left_one_and_subtract_one <- function(current_car,train,cars_passed) {
  print(paste("Moves:",cars_passed))
  current_car <- current_car - 1
  print(paste("Current Car:",current_car))
  return(current_car)
}
  
move_and_decide <- function(start_car,train,current_car,cars_passed) {
  current_car <- move_right_one_and_count_one(current_car,train,cars_passed)
  cars_passed <- cars_passed + 1
  if (!is_different(start_car,train,current_car)) {
    train <- change_light(train,current_car)
    potential_count <- current_car - 1 
    while (current_car > 1) {
      current_car <- move_left_one_and_subtract_one(current_car,train,cars_passed)
      cars_passed <- cars_passed + 1
    }
    if (is_different(start_car,train,current_car)) {
      return(print(paste0("Number of cars in train: ",potential_count,", figured out in: ",cars_passed," moves.")))
    }
  }
  move_and_decide(start_car,train,current_car,cars_passed)
}

## MAIN
train <- init_train(20)
start_car <- record_start_car(train)
cars_passed <- 0
current_car <- 1
move_and_decide(start_car,train,current_car,cars_passed)
