library(fTrading)
final_data <- function(filename){
  
  ## initial data
  data <- read.csv(file=filename,head=TRUE,sep=",")
  ## larger index, larger time it will be
  data <- data[order(data$Date),]
  
  ## remove 0 volume
  invalid <- data$Volume != 0
  data <- data[invalid, ]
  rm(invalid)  
  
  ## return final data
  data
}

get_k <- function(data, lag_1){
  k <- fpkTA(data$Close, data$High, data$Low, lag_1)
}

get_d <- function(data, lag_1, lag_2){
  d <- fpdTA(data$Close, data$High, data$Low, lag_1, lag_2)
}

get_5_day_det <- function()
{
  filename <- 'table.csv'
  data <- final_data(filename)
  
  ## calculate return based on three days
  lag_3 <- 3
  up <- data$Close[(1+lag_3):length(data$Close)]
  down <- data$Close[2:(length(data$Close)-lag_3+1)]
  
  ## future return calculated from first day up to last fourth day
  rt <- round((up/down-1)*100)
  
  
  ## calculate det for each day using data dim
  v_det = c()
  for (i in 1:length(data$Date)-19)
  {
    base_p <- (data$Close[i]+data$High[i]+data$Low[i]+data$Open[i])/100.0
    base_v <- data$Volume[i]/100.0
    temp_v <- c()
    weight <- 0.618
    for (j in 0:19)
    {
      temp_v <- c(temp_v, data$Open[i+j]/base_p)
      temp_v <- c(temp_v, data$High[i+j]/base_p)
      temp_v <- c(temp_v, data$Low[i+j]/base_p)
      temp_v <- c(temp_v, data$Close[i+j]/base_p)
      temp_v <- c(temp_v, data$Volume[i+j]/base_v)
      temp_v <- temp_v*weight
    }
    temp_matrix <- matrix(temp_v, nrow=10,ncol=10)
    temp_matrix <- log(temp_matrix)*100
    v_det <- c(v_det, det(temp_matrix))
  }
  color <- rt > 0
  colour <- ifelse(color, 'red','green')
  print (v_det[1:30])
  plot(v_det[1:length(v_det)],rt[1:length(v_det)], xlim=range(-1e10, 1e10), col=colour, cex=.5)
  
}

f <- function(lag_1, lag_2)
{
  filename <- 'table.csv'
  data <- final_data(filename)
  lag_3 <- 3
  k <- get_k(data, lag_1)
  d <- get_d(data, lag_1, lag_2)
  up <- data$Close[(1+lag_3):length(data$Close)]
  down <- data$Close[2:(length(data$Close)-lag_3+1)]
  rt <- round((up/down-1)*100)
  color <- rt > 0
  colour <- ifelse(color, 'red','green')
  plot(k[1:length(rt)],d[1:length(rt)], col=colour,cex=.1)
}