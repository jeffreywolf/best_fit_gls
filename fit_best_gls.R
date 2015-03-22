library(nlme)
fit_best_gls = function(expression, data){
	print("In fit best gls")
	#print(expr)
	gls_gaus = gls(
		expression, 
		correlation = corGaus(form= ~gx+gy, nugget=T),
		method='ML',
		data=data
	)
	print("Finished fitting GLS with Gaussian Autocorrellogram")
	gls_sph = gls(
		expression, 
		correlation = corSpher(form= ~gx+gy, nugget=T),
		method='ML',
		data=data
	)
	print("Finished fitting GLS with Spherical Autocorrellogram")
	gls_exp = gls(
		expression, 
		correlation = corExp(form= ~gx+gy, nugget=T),
		method='ML',
		data=data
	)
	print("Finished fitting GLS with Exponential Autocorrellogram")

	# Select best model
	best= gls_gaus
	if(best$logLik > gls_exp$logLik){
		best = gls_exp
	}
	if(best$logLik > gls_sph$logLik){
		best = gls_sph
	}
	return(best)
}