*! version 1.0.0  17may2021
program _gmm_dripw, sortpreserve
	version 14
	syntax varlist if [fweight iweight pweight],			///
						at(name)                            ///
						ty(string)							///
						y(string)							///
						timevar(string)						///					
						groupvar(string)					///
						[									///
						pscore(string)						///
						t0(string)							///	
						*                                   ///
						]

	tempvar mup mub muw1 muw0 muatt Fx Fxc fx dy w0var tif wimp            

	tokenize `varlist'
	
	local bat  `1'
	local bt   `2'
	local by   `3'
	local bw0  `4'
	local bw1  `5'
	
	quietly {
		matrix score double `muatt'  = `at' `if', eq(#1) 
		matrix score double `mup'    = `at' `if', eq(#2) 
		matrix score double `mub'    = `at' `if', eq(#3) 
		matrix score double `muw0'   = `at' `if', eq(#4) 
		matrix score double `muw1'   = `at' `if', eq(#5) 
		
		// Generating ancillary variables 
		
		bysort `groupvar' (`timevar'): generate double `dy'=`y'[2]-`y'[1] `if'
	
		if ("`pscore'"=="") {
			generate double `Fx'  = logistic(`mup')
			generate double `Fxc' = logistic(-`mup')
			generate double `fx'  = `Fx'/(1 + exp(`mup'))	
		}
		else if ("`pscore'"=="probit") {
			generate double `Fx'  = normal(`mup')
			generate double `Fxc' = normal(-`mup')
			generate double `fx'  = normalden(`mup') 
		}
		else if ("`pscore'"=="imp") {
			replace `bt' = `ty' - (`ty'==0)*exp(`mup') `if'
			generate double `Fx'    = logistic(`mup')
			generate double `Fxc'   = logistic(-`mup')	
		}
		
		generate double `w0var' = `Fx' * (1-`ty')/(`Fxc')
		
		// Compute scores 
		generate double `wimp' = `w0var'
		
		if ("`pscore'"!="imp") {
			replace `bt'    = (`ty'*`fx'/`Fx'- (1-`ty')*`fx'/`Fxc') `if' 
			replace `wimp'  = 1 
		}
		
		replace `by'    = `wimp'*(1-`ty')*(`dy'-`mub') `if'
		replace `bw0'   = `w0var' - `muw0' `if'
		replace `bw1'   = `ty'    - `muw1' `if'
		replace `bat'   = 	///
			(`ty'/`muw1'- `w0var'/`muw0')*(`dy'-`mub') - `muatt' `if'
	}

end
