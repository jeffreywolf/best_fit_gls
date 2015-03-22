library(nlme)
fit_best_gls = function(expression, spform=~x+y, data){
	print("In fit best gls")
	#print(expr)
	gls_gaus = gls(
		expression, 
		correlation = corGaus(form= spform, nugget=T),
		method='ML',
		data=data
	)
	print("Finished fitting GLS with Gaussian Autocorrellogram")
	gls_sph = gls(
		expression, 
		correlation = corSpher(form= spform, nugget=T),
		method='ML',
		data=data
	)
	print("Finished fitting GLS with Spherical Autocorrellogram")
	gls_exp = gls(
		expression, 
		correlation = corExp(form= spform, nugget=T),
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

# Note:
# spform is a formula such as: ~x+y
# that specifies the 2D coordinates used to represent spatial location
# in a projected coordinate system 