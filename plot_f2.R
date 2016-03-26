###############################################
# plot_f.R
# This file include the function for plot
#
# "state_latlon.csv" is needed.
#
# lc_heatmap(data, state = state, location = location, title)
#' @param data: A summary data with the variable interested named ratio
#' @param state: state is a data.frame provided by this file, please set it to state
#' @param location: location is provided by this file, please set it to location
#' @param title: title of the plot
#' @return a plot for ratio information over geography
#'
#'
#@ coeff_plot(list.l, status, sig)
#' @param list.l: the list of logistic regressions
#' @param status: a vector constains the status of loan
#' @param sig: true if a p-value plot is requested
#' @return a plot for the parameters' coefficient values or significant values
#'
#'
#' mis_plot <- function(true,pred)
#' @param a vector of true classification
#' @param a vector of prediction classification
#' @return a plot of the classification result
#'
################################################
# heatmap
########################
# Caculate the ratio
lc_ratio <- function(data, status = "Fully_Paid"){
  lc_geoSum <- data %>% group_by(addr_state) %>%  
    dplyr::summarize(ratio =
                       sum(loan_status == status)/length(loan_status))
  lc_geoSum <- as.data.frame(lc_geoSum)
  return(lc_geoSum)
}


# get the abbreviation and full name online
URL <- "http://state.1keydata.com/state-abbreviations.php"
tables = readHTMLTable(URL, stringsAsFactors = FALSE)
list <- tables[[2]]
list <- data.frame(full = c(list$V1, list$V3), abb = c(list$V2, list$V4))
state <- list[-27, ][-1, ]
row.names(state) <- 1 : length(state[,1])




## drop the unnecessary areas 
dropArea <- function(x, state){
  test_that("The data should include variable addr_state", { # should include addr_state
    expect_equal(is.null(x$addr_state), F)
  })
  index_in <- which(x$addr_state %in% state$abb)
  x <- x[index_in,]
  #index_ex <- which(state$abb %in% x$addr_state)
  #state_ex <- state$abb[-index_ex]
  #x_not <- data.frame(addr_state = state_ex, ratio = rep(0,length(state_ex)))
  #x <- rbind(x, x_not)
  row.names(x) <- 1:length(x[,1])
  return(x)
}

## Make every alphabet to lower case and attach the full name of states
low <- function(x, state){
  index <- grep(pattern = as.character(x$addr_state), as.character(state$abb))
  x$region <- state$full[index]
  x$region <- tolower(x$region)
  return(x)
}

Low <- function(data, state){
  test_that("data should include variable addr_state", { # should include addr_state
    expect_equal(is.null(data$addr_state), F)
  })
  test_that("state should include variable abb", { # state should include abb
    expect_equal(is.null(state$abb), F)
  })
  y <- data %>% ddply(~addr_state, 
                      function(x){low(x, state)})
  return(y)
}

# Get Average Latitude and Longitude for US States
location <- read.csv("state_latlon.csv")
names(location)[1] <- c("addr_state")

loc <- function(x, location){
  test_that("data should include variable addr_state", { # should include addr_state
    expect_equal(is.null(x$addr_state), F)
  })
  x <- merge(x, location, by = "addr_state")
  return(x)
}

# map part
heatmap <- function(x, title){
  test_that("data should include variable named addr_state", { # should include addr_state
    expect_equal(is.null(x$addr_state), F)
  })
  test_that("data should include variable named ratio", { # should include ratio
    expect_equal(is.null(x$ratio), F)
  })
  states <- map_data("state")
  map.df <- merge(states,x, by="region", all.x=T)
  map.df <- map.df[order(map.df$order),]
  myPlot <- ggplot(map.df, aes(x=long,y=lat,group=group))+
    geom_polygon(aes(fill= ratio))+
    geom_path()+ 
    scale_fill_gradientn(colours=rev(heat.colors(20)),na.value="grey90")+
    geom_text(aes(x=longitude, y=latitude, label=addr_state), 
              size=3)+
    ggtitle(title)+
    labs(x = "Longitude", y = "Latitude") +
    coord_map()
  return(myPlot)
}

lc_heatmap <- function(data, state, location, title){
  y <- dropArea(data, state)
  y <- Low(y, state)
  y <- loc(y, location)
  return(heatmap(y, title))  
}


################################################
#Logistic Regression
##################################################

# logistic significant level plot
coeff_plot1 <- function(log.f, sig = F, sig.value = 0.1){
  ## significance value
  plotdata <- summary(log.f)$coefficients[,4]
  # normalized paramter estimation
  plotdata1 <- summary(log.f)$coefficients[,1]/summary(log.f)$coefficients[,2]
  # order the data, select the significance value
  plotdata <- data.frame(var = names(plotdata), value = plotdata) %>%
    mutate(var = reorder(x = var, X = value, min)) %>%
    subset(value < sig.value)
  plotdata1 <- data.frame(var = names(plotdata1), value = plotdata1) %>%
    mutate(var = reorder(x = var, X = value, min)) %>% 
    subset(var %in% plotdata$var)
  if(sig){
    myplot <- ggplot(data=plotdata,
                     aes(x=var, y=value, colour=var)) +
      geom_point() +
      geom_hline(yintercept = 0.05, colour = "red", linetype = 3) +
      labs(y = "p - value", title = "Significant Value Plot")
  }else{
    myplot <- ggplot(data=plotdata1,
                     aes(x=var, y=value, colour=var)) +
      geom_point() +
      geom_hline(yintercept = 0, colour = "red", linetype = 3) +
      labs(y = "Normalized Parameter Estimated value", title = "Normalized Parameter Estimated Value Plot")
  }
  return(myplot)
}


