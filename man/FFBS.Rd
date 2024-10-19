\name{FFBS}
\title{
FFBS Algorithm
}
\description{
Forward Filtering and Backward Sampling algorithm for Linear Gaussian State-Space Model.
Prediction and Smoothing can be implemented for the univariate time-series model.
}
\usage{
FF_Gaussian(Covariate_mat = NA, target_variable_vec, pred_length = NA, M_pred = 1000,
            data_type, trend_type = "slope", seasonality = TRUE ,
            component_size_daily_weekly = 3,
            component_size_daily_yearly = 12,
            component_size_weekly_yearly = 6,
            prior_trend = NA , prior_seasonality_sd = NA,
            prior_covariate_sd_vec = NA,
            n0 = 10 , s0 = NA)
BS_Gaussian(res_FF_Gaussian , M_BS = 1000, print_BS = TRUE)
extract_BS(res_BS_Gaussian, data_type)
plot_BS_mean(res_extract_BS , data_type, xlim_weekly = NA  , plot_type , coef_nth = NA)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Covariate_mat}{
  (p,T+pred_length)-dimensional numeric matrix which contains the covariate time series data where T is the length of the target time series (target_variable_vec) and p is the number of covariates.
  }
  \item{target_variable_vec}{
  T-dimensional numeric vector, which is the target variable.
  }
  \item{pred_length}{
  The length for prediction. The default is NA (no prediction).
  }
  \item{M_pred}{
  The particle size of the prediction.
  }
  \item{data_type}{
  The variable that affects seasonality. Choose either "daily" or "weekly".
  }
 \item{trend_type}{
  The variable that affects trend. Choose either "intercept" or "slope". If "intercept" is selected, the trend term is just a random walk intercept. If "slope" is selected, the trend term is a random walk intercept with random walk slope. The default is "slope".
  }
  \item{seasonality}{
  logical. If seasonality is TRUE, the seasonality component is used in the State-Space Model. The default is TRUE, but if you use covariates with cyclicity, then FALSE is recommended.
  }
  \item{component_size_daily_weekly}{
  This sets how many terms of the Fourier series are used to approximate weekly cyclicity (for daily data).
  }
  \item{component_size_daily_yearly}{
  This sets how many terms of the Fourier series are used to approximate yearly cyclicity (for daily data).
  }
  \item{component_size_weekly_yearly}{
  This sets how many terms of the Fourier series are used to approximate yearly cyclicity (for weekly data).
  }
  \item{prior_trend}{
  The prior for the trend component. The default is NA and the prior is automatically set based on the target variable. If you want to set the prior for the trend, prior_trend must be a list object which containes the prior mean vector in prior_trend[[1]] and the prior standard deviation vector in prior_trend[[2]].
  }
  \item{prior_seasonality_sd}{
  Numeric scalar. The prior standard deviation for the seasonality component. The default is NA and the prior is automatically set based on the target variable.
  }
  \item{prior_covariate_sd_vec}{
  p-dimensional numeric vector. The prior standard deviation for the coefficients of the covariates. The default is NA and the prior is automatically set based on the target variable.
  }
  \item{n0}{
  The prior sample size for the gamma evolution. The default is recommended.
  }
  \item{s0}{
  The prior variance for the gamma evolution. The default is NA and the prior is automatically set based on the target variable. The default is recommended.
  }
  \item{res_FF_Gaussian}{
  The output of FF_Gaussian().
  }
  \item{M_BS}{
  The particle size of posterior sampling.
  }
  \item{print_BS}{
  logical. The default is TRUE, and it continuously outputs to the console how far posterior sampling has progressed.
  }
  \item{res_BS_Gaussian}{
  The output of BS_Gaussian().
  }
  \item{res_extract_BS}{
  The ouput of extract_BS().
  }
  \item{xlim_weekly}{
  xlim for the weekly seasonality plot for the daily data.
  }
  \item{plot_type}{
  "trend", "seasonality", or "coef". Select which component is plotted.
  }
  \item{coef_nth}{
  Select which coefficient is plotted. The default is NA, but you must select coef_nth when plot_type == "coef".
  }
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%
}
\references{
[1]  Fr¨uhwirth-Schnatter, S., 1994. Data augmentation and dynamic linear models. Journal of Time
 Series Analysis 15, 183–202.

[2] West, M., Harrison, P. J., 1997. Bayesian Forecasting & Dynamic Models, 2nd Edition. Springer
 Verlag.
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
## data generation
T = 104*7
pred_length = 21
cova1 = rnorm(n = T + pred_length)
cova2 = rnorm(n = T + pred_length)
data_all = 5*cova1 + 4 * cova2 + (1:(T+pred_length))*0.1 +
           1*sin(2*pi*1/365.25 * (1:(T+pred_length))) + 1.4*cos(2*pi*2/365.25 * (1:(T+pred_length))) +
           1.7*sin(2*pi*3/365.25 * (1:(T+pred_length))) + 8*cos(2*pi*4/365.25 * (1:(T+pred_length))) +
           rep(c(1,1,1,2,1,4,5) , 104+3) + rnorm(n = T+pred_length, sd = 0.1)
data = data_all[1:T]
Covariate_mat = rbind(cova1 , cova2)

## Forward Filtering
res_FF = FF_Gaussian(Covariate_mat =  Covariate_mat,
                     target_variable_vec = data, data_type = "daily" , pred_length = pred_length ,
                     trend_type = "slope")

## plot the prediction
start = 650
plot(data_all[start:length(data_all)], ylab = NA, xlab = NA, main = "prediction")
points((length(data_all) - start+1  -20):((length(data_all) - start+1)) , colMeans(res_FF$y_pred_mat) , col = "red",
       pch = 0)

## Backward Sampling
res_BS = BS_Gaussian(res_FF_Gaussian = res_FF)
post_list = extract_BS(res_BS_Gaussian = res_BS , data_type = "daily")
## plot the smoothing distribution
plot_BS_mean(res_extract_BS = post_list, data_type = "daily" ,
             plot_type = "trend")
plot_BS_mean(res_extract_BS = post_list, data_type = "daily" ,
             plot_type = "seasonality" , xlim_weekly = c(1,7))
plot_BS_mean(res_extract_BS = post_list, data_type = "daily" ,
             plot_type = "coef", coef_nth = 1)
plot_BS_mean(res_extract_BS = post_list, data_type = "daily" ,
             plot_type = "coef", coef_nth = 2)
}
