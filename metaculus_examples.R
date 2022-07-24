library( httr)
library(lubridate)
library( tidyverse)
source( "metaculus.R")

## 
## Examples using Metaculus helper functions -----
##

metaculus_reset_session()
metaculus_authenticate()

##
##  Monkeypox
##

question_num <- 10979
qdata <- metaculus_question_data( question_num )


# good content on raw qdata structure
qdata$author_name
qdata$title
qdata$title_short

qdata$description

metaculus_pred <-bind_rows( 
  metaculus_prediction_df( qdata, "community_prediction"),
  metaculus_prediction_df( qdata, "metaculus_prediction")
)

ggplot( metaculus_pred, aes( x, y=cdf, col = prediction_type)) +
  geom_line() +
  scale_x_continuous( trans="log10")

prediction_ts <- metaculus_prediction_ts(qdata)

ggplot( prediction_ts, aes( x=t )) +
  #  geom_boxplot( aes( ymin=low, lower=q1, middle=q2, upper=q3, ymax=high))
  geom_ribbon( aes( ymin=q1, ymax=q3), col="black", fill = "purple", alpha=0.5) +
  geom_line( aes( y=q2 ), lty="dashed") +
  ggtitle( qdata$title_short)


## what's in the histogram?  the same as the prediction?

ph <- qdata$prediction_histogram

v1 <- map_dbl( ph, ~ .[[1]])
v2 <- map_dbl( ph, ~ .[[2]])
v3 <- map_dbl( ph, ~ .[[3]])

qplot( v1, v2 )
qplot( v1, v3 )

# for( n in names(qdata))
#   cat( paste0( "qdata$", n, "\n"))


##
## Date-predictions
##

question_num <- 11861
qdata <- metaculus_question_data( question_num )

qdata$title

metaculus_pred <-bind_rows( 
  metaculus_prediction_df( qdata, "community_prediction"),
  metaculus_prediction_df( qdata, "metaculus_prediction")
)

# x is now a datetime

ggplot( metaculus_pred, aes( x, y=cdf, col = prediction_type)) +
  geom_line() +
  ggtitle( qdata$title)


##
## Binary outcome prediction
##


question_num <- 8888
qdata <- metaculus_question_data( question_num )


metaculus_pred <-bind_rows( 
  metaculus_prediction_df( qdata, "community_prediction"),
  metaculus_prediction_df( qdata, "metaculus_prediction")
)


ggplot( metaculus_pred, aes( x=x, y=pdf, col = prediction_type, fill=prediction_type)) +
  geom_col() +
  ggtitle( qdata$title)

ggplot( metaculus_pred, aes( x=x, y=cdf, col = prediction_type)) +
  geom_line()+
  ggtitle( qdata$title)



##
## Examples from API document -----
##

## In[1]
api_url <- "https://www.metaculus.com/api2"
username <- "superforecaster99" # or whatever your username is
password <- rstudioapi::askForPassword( "Enter your password: ")
# you can also just type your password into the buffer, but probably you
# don't want to commit that to code

## In[2]
login <- list(
  username = username,
  password = password
)

res <- POST( paste0( api_url, "/accounts/login/"), body = login)

status_code( res)  # should be 200
content( res )     # should be a list with component "user_id"

## In[3]
res <- GET( paste0( api_url, "/questions/5327/"))
qdata <- content( qdata)

names( qdata )
qdata$title

## In[4]
qdata$possibilities

## In[5]
rescale_prediction0 <- function( x, possibilities )
{
  ymin <- possibilities$scale$min
  ymax <- possibilities$scale$max
  deriv_ratio <- possibilities$scale$deriv_ratio
  
  if( possibilities$format == "date")
  {
    ymin <- as.numeric( as_datetime( ymin))
    ymax <- as.numeric( as_datetime( ymax))
  }
  
  if( deriv_ratio != 1)
  {
    # Convert to log scale
    # This is defined such that the ratio of derivatives
    # (dx2/dx) evaluated at x=1 and x=0 is precisely equal
    # to "deriv_ratio" and x=0 and x-1 remain fixed
    x2 = ( deriv_ratio^x - 1 )/(deriv_ratio-1)
  } else {
    x2=x
  }
  
  out <- ymin + (ymax-ymin) * x2
  
  if( possibilities$format == "date")
    out <- as_datetime( out )
  
  out
}

# In[6]
qdata$metaculus_prediction$history[1:5]

# I eyeballed the output, and it seems to match
# EXCEPT the times are whole integers (seconds) whereas in the example
# the times are real numbers (microseconds)

# In[7]

y <- unlist(qdata$metaculus_prediction$full$y)
x <- seq( 0,1,length.out = length(y))
x2 <- rescale_prediction0( x, qdata$possibilities)

qplot( x2, y, main = qdata$title, geom="line",xlab = "Metaculus prediction (PDF)")

# In[8]

# As the example says, "To calculate the cumulative distribution function,
# we just need to integrate the PDF using the trapezoid rule."

metaculus_cdf0 <- function( mp)
{
  y <- unlist( mp$y)
  ly <- length( y)
  y_avg <- ( y[-1] + y[-ly ] )/2
  cy <- c( 0, cumsum( y_avg))
  cy <- cy / last( cy )
  
  high <- mp$high
  low  <- mp$low
  if( is.null( high))
    high <- 1
  if( is.null( low))
    low <- 0
  
  cy * (high-low) + low
}

cy <- metaculus_cdf0( qdata$metaculus_prediction$full )

x  <- seq( 0,1,length.out = length(cy))
x2 <- rescale_prediction0( x, qdata$possibilities)

qplot( x2, cy, main = qdata$title, geom="line",xlab = "Metaculus prediction (CDF)")
