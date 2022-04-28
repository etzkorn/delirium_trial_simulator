#' Simulate joint recurrent event and survival data
#'
#' @description Simulate times of recurrent events and terminal events using joint model. Recurrent events have weibull gap times.
#'
#' @param par0 named parameter vector
#' @param n number of individuals in data
#'
#' @return tibble with variables


simulate.competing.data <- function(n=1200, K = 28,
									par0 = c(shapeR = 0.34, scaleR = 6.64,
                  shapeM = 2, scaleM = 20.5,
                  shapeD = 1.51, scaleD = 12.23,
                  sigma = 0.24,
                  alphaM = 0,
                  alphaD = 0,
                  betaR = 0,
                  betaM = 0,
                  betaD = 0)){
set.seed(654321)
tibble(
id = 1:n,
trt = sample(rep(0:1, length = n)),
w = rnorm(n, 0, par0["sigma"]),
T1 = rweibRH(n,
	 shape = par0["shapeM"],
	 scale = par0["scaleM"],
	 rh = exp(w*par0["alphaM"] + par0["betaM"] * trt)),
T2 = rweibRH(n,
	 shape = par0["shapeD"],
	 scale = par0["scaleD"],
	 rh = exp(w*par0["alphaD"] + par0["betaD"] * trt)),
y = pmin(T1, T2, K),
terminal1 = as.numeric(T1 < T2 & T1 < K),
terminal2 = as.numeric(T2 < T1 & T2 < K),
t =  map2(w,trt,
          ~tibble(
          	t = rweibRH(50,
          	            shape = par0["shapeR"],
          	            scale = par0["scaleR"],
          	            rh = exp(.x + par0["betaR"] * .y)) %>%
		  	    rbind(rep(1, 50)) %>%
		  	    cumsum,
		  	event = rep(1:0, length = 100)
		  	)
	)
)%>%
dplyr::select(-T1, -T2, -w) %>%
unnest(t) %>%
group_by(id) %>%
mutate(tstart = c(0, t[-n()])) %>%
mutate(terminal1 = terminal1*(t > y),
	   terminal2 = terminal2*(t > y),
	   event = event*(t < y),
	   t = ifelse(t > y, y, t)) %>%
filter(tstart < t) %>%
ungroup %>%
group_by(id) %>%
group_modify(
	~ tibble(day = 1:28,
			 # for each day 1:28, we grab the delirium status of the interval that the
			 # day falls into. if there is no corresponding interval, we get an NA.
			 delirium = sapply(1:28,
			 				  function(d){
			 				  		status <- .x$event[(d-.5)>.x$tstart &(d-.5)<.x$t]
			 				  		if(length(status)==0) return(NA) else return(status)
			 				  	}),
			 state = ifelse(is.na(delirium)&sum(.x$terminal1)>0, "Dead",
			 			   ifelse(is.na(delirium)&sum(.x$terminal2)>0, "Discharged",
			 			   	   ifelse(delirium==1, "Delirium", "No Delirium"))),
			 losic = min(c(28,day[is.na(delirium)])),
			 died = max(.x$terminal1),
			 discharged = max(.x$terminal2),
			 trt = .x$trt[1])
)

}

rweibRH <- function(n, shape ,scale , rh){
	rweibull(n, shape = shape, scale = scale * rh^(-1/shape))
}
