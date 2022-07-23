# metaculus
Some handy functions for downloading metaculus forecasts in R

This uses the metaculus api (https://www.metaculus.com/api2) to download the pdf and cdf of predictions.  It also can handle some forecast history timeseries.  The key is translating the interval [0,1] to whatever possibilities exist for the given question. 
