
metaculus_api_url <- "https://www.metaculus.com/api2"



metaculus_reset_session <- function()
{
  # if you need to reset the session
  httr::handle_reset( "https://www.metaculus.com")
}


metaculus_authenticate <- function( username= NA, password = NA)
{
  if( is.na( username ))
    username <- rstudioapi::showPrompt( title = "Username", message = "Username", default = "" )
  
  if( is.na( password ))  
    password <- rstudioapi::askForPassword( "Enter your password: ")
  
  login <- list(
    username = username,
    password = password
  )
  
  res <- POST( paste0( metaculus_api_url, "/accounts/login/"), body = login)
  
  if( status_code( res) !=200 )
    stop( paste0( "Can't authenticate: status code ", status_code( res ),"\n",content(res) ))
  
  content( res )$user_id # should be a list with component "user_id"
}


metaculus_question_data <- function( question_num )
{
  res <- GET( paste0( metaculus_api_url, "/questions/", question_num,"/"))
  
  if( status_code(res) != 200)
    stop( paste0( "error getting question data: status code", status_code( res)))
  content( res)
}


metaculus_rescale_prediction <- function( x, possibilities )
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



# As the example says, "To calculate the cumulative distribution function,
# we just need to integrate the PDF using the trapezoid rule."

metaculus_cdf <- function( mp)
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


metaculus_pred_binary <- function( qdata, prediction_type )
{
  # check to see if this is a single point or a range of predictions
  # is the metaculus_prediction always a point?  
  pred <- qdata[[prediction_type]]$full
  
  if( is.numeric(pred) )
    return( 
      tibble( 
        x=c( 0, pred, 1 ), 
        pdf =  c( 0,1,0), 
        cdf =  c( 0,1,1), 
        prediction_type=prediction_type 
      )
    )
  
  # predictions of 0 and 100% are not allowed, we tack them on the ends
  # assume the other y's correspond to predictions evenly spaced in (0, 100%)
  
  pdf <- c( 0, unlist( pred$y),0)
  x   <- seq( 0,1,length.out=length(pdf))
  
  # maybe this is off by 1?  Is CDF P(X <=c) or P(X<c)?
  cdf <- cumsum( pdf)/sum(pdf) 
  
  tibble( x=x, pdf=pdf, cdf=cdf, prediction_type=prediction_type)  
}


metaculus_pred_continuous <- function( qdata, prediction_type )
{
  y <- unlist(qdata[[ prediction_type ]]$full$y)
  x <- seq( 0,1,length.out = length(y))
  x_rescale <- metaculus_rescale_prediction( x, qdata$possibilities)
  
  cy <- metaculus_cdf( qdata[[ prediction_type ]]$full )
  
  tibble( x=x_rescale, pdf = y, cdf = cy, prediction_type=prediction_type )
}

metaculus_ts_component <- function( qdata, list_component, rescale = T )
{
  out <- map_dbl( qdata$prediction_timeseries, ~ .[[list_component]]) 
  
  if( rescale )
    out <- metaculus_rescale_prediction(  out, qdata$possibilities)
  
  out
}


metaculus_prediction_ts <- function( qdata )
{
  t    <- metaculus_ts_component( qdata, "t", rescale = F  ) %>% as_datetime()
  low  <- metaculus_ts_component( qdata, c("community_prediction", "low") )
  q1   <- metaculus_ts_component( qdata, c("community_prediction", "q1") )
  q2   <- metaculus_ts_component( qdata, c("community_prediction", "q2") )
  q3   <- metaculus_ts_component( qdata, c("community_prediction", "q3") )
  high <- metaculus_ts_component( qdata, c("community_prediction", "high") )
  
  num_predictions <- metaculus_ts_component( qdata, "num_predictions", rescale=F )
  
  # there's a bunch of stuff on the component "distribution"
  # we'll skip it
  
  tibble(
    t = t,
    low = low,
    q1 = q1,
    q2 = q2,
    q3 = q3,
    high = high,
    num_predictions = num_predictions
  )
  
)


metaculus_prediction_df <- function( qdata, prediction_type = "community_prediction" )
{
  if( is.null( qdata[[prediction_type]]))
    stop( paste0( "no prediction type '", prediction_type,"' found"))
  
  if( qdata$possibilities$type == "continuous" )
    return( metaculus_pred_continuous( qdata, prediction_type ))
  
  if( qdata$possibilities$type == "binary" )
    return( metaculus_pred_binary( qdata, prediction_type ))   
  
  stop( paste0( "don't know how to handle possibility type '", qdata$possibilities$type, "'" ))    
}
