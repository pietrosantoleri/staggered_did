*! version 1.0.0  17may2021
program _gmm_regipw, sortpreserve
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
						reg(string)							///
						*                                   ///
						]

	tempvar mup mub mub1 mub0 muw1 muw0 muatt Fx Fxc fx dy w0var	///
			tif wimp             
	tokenize `varlist'

	if ("`reg'"=="reg") {
	    local pscore "none"
		local bat  `1'
		local by1  `2'
		local by0  `3'
		quietly matrix score double `muatt'  = `at' `if', eq(#1) 
		quietly matrix score double `mub1'   = `at' `if', eq(#2) 
		quietly matrix score double `mub0'   = `at' `if', eq(#3) 
	}
	else if ("`reg'"=="stdipw") {
		local bat  `1'
		local bt   `2'
		local by1  `3'
		local by0  `4'
		quietly matrix score double `muatt'  = `at' `if', eq(#1) 
		quietly matrix score double `mup'    = `at' `if', eq(#2) 
		quietly matrix score double `mub1'   = `at' `if', eq(#3) 
		quietly matrix score double `mub0'   = `at' `if', eq(#4) 
	}
	
	quietly {

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
		
		if ("`pscore'"!="imp" & "`reg'"!="reg") {
			replace `bt'    = (`ty'*`fx'/`Fx'- (1-`ty')*`fx'/`Fxc') `if' 
		}
				
		if ("`reg'"==""|"`reg'"=="reg")  {
			replace `by1'   = (`ty')*(`dy'-`mub1') `if'
			replace `by0'   = (1-`ty')*(`dy'-`mub0') `if'
			replace `bat'   = ((`mub1' - `mub0') - `muatt')*`ty' `if'
		}
		else {
				generate double `muw0' = (1-`ty')*`Fx'/`Fxc' `if'
				replace `by1'          = (`ty')*(`dy'-`mub1') `if'
				replace `by0'          = (`muw0')*(`dy'-`mub0') `if'
				replace `bat'          = ///
					((`mub1' - `mub0') - `muatt')*`ty' `if'
		}
	}

end
