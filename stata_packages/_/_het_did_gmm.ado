*! version 1.0.0  17may2021

program define _het_did_gmm, rclass sortpreserve
        syntax varlist(numeric fv ts) [if] [in]                 ///
                        [fweight iweight pweight aweight],      ///
                        [                               		///
                            groupvar(string)					///
							psvars(string)						///
							estimator(string)					///
							treatvar(string)					///
							timevar(string)						///
							probit								///
							csdid								///
							noCONstant                     		///
							ITERate(integer 5)					///
							vce(passthru)						///
							touse2(string)						///
							pscore(string)						///
							*									///
                        ]

	// !! constant per equation 
	// !! make parmlist a separate program 
	// !! handling weights ?? verify Fernando vs GMM 
	// !! conditioning on t==0 ?? robust vs cluster cond t=0 
	// !! unbalanced samples 
		
	marksample touse 

	tempname init b V 
	
	// Getting weights 
	
	if "`weight'" != "" {
		local wgt [`weight' `exp']
	}

	// depvar and varlist 
	
	gettoken lhs rhs : varlist
    _fv_check_depvar `lhs'
	
	fvexpand `rhs'
	local xvars "`r(varlist)'"
	
	// Parse pscore()
	
	_Parse_pscore, `pscore'
	
	// Getting initial values for gmm 
	
	_Init_Values if `touse' `wgt', estimator(`estimator') 	///
					  pscore(`pscore')						///
					  xvars(`xvars') 						///
					  treatvar(`treatvar')					///
					  groupvar(`groupvar')					///
					  lhs(`lhs') timevar(`timevar')
	
	matrix `init' = r(init)

	// Defining which estimator, parameters, instruments, and equations
	
	_Eq_List_gmm, xvars(`xvars') `constant'	estimator(`estimator')
	local parms "`s(parms)'"
	local eqlist "`s(eqlist)'"
	
	if ("`vce'"=="") {
		local vce vce(cluster `groupvar')
	}
						
	if ("`estimator'"=="imp"|"`estimator'"=="dripw") {
		local gmmest "dripw"
		if ("`estimator'"=="imp") {
			local pscore "imp"
		}
		local eq1 "treatment"
		local eq2 "outcome"
	}
	if ("`estimator'"=="reg") {
		local reg "reg"
		local gmmest regipw
		local eq1 "outcome1"
		local eq2 "outcome0"
	}	
	
	local eqtwo 		instruments(`eq1':`xvars', `constant')		///
						instruments(`eq2': `xvars', `constant')	
						
	if ("`estimator'"=="stdipw") {
		local reg "stdipw"
		local gmmest regipw		
		local eq1 "treatment"
		local eqtwo instruments(`eq1':`xvars', `constant')
	}

	di ""
	gmm _gmm_`gmmest' if `touse' `wgt',								///
					    ty(`treatvar')								///
					    y(`lhs')									///
						groupvar(`groupvar')						///
						timevar(`timevar')							///
					    from(`init')								///
						equations(`eqlist')							///
						parameters(`parms')							///
						`eqtwo'										///
                        quickderivatives							///
                        winit(unadjusted, independent) onestep		///
						conv_maxiter(`iterate') `vce' 				///
						valueid("EE criterion")	pscore(`pscore')	///	
						iterlogonly	reg(`reg') nn(`nn') nnt(`nnt')
						
		matrix `b' = e(b)
		matrix `V' = e(V)		

		_Re_Stripe, treatvar(`treatvar')
		local stripe "`s(stripe)'"
		matrix colname `b' =  `stripe'
		matrix colname `V' = `stripe'
		matrix rowname `V' = `stripe'
		quietly replace `touse2' = e(sample)
		local N = e(N)
        return local vcetype "`e(vcetype)'"
		if ("`e(vce)'"=="cluster") {
			local N_clust = e(N_clust)
			return scalar N_clust = `N_clust'
			return local vce "`e(vce)'"
			return local clustvar "`e(clustvar)'"
		}
		return matrix b = `b'
		return matrix V = `V'
		return scalar N = `N'
end

program define _Re_Stripe, sclass
	syntax [anything], [treatvar(string)]
	local cols: colfullnames e(b)
	local uno: word 1 of `cols'
	local stripe0: list cols - uno
	local stripe "ATET:r1vs0.`treatvar' `stripe0'"
	sreturn local stripe "`stripe'"
end 

program define _Param_list, sclass
	syntax [anything], [vars(string) pnom(string) noCONstant] 
	local k: list sizeof vars
	local parmlist ""
	forvalues i=1/`k' {
	    local x: word `i' of `vars'
	    local parmlist "`parmlist' `pnom':`x'"
	}
	if ("`constant'"=="") {
		local parmlist "`parmlist' `pnom':_cons"
	}
	sreturn local parmlist "`parmlist'"
end 

program define Stri_PE_S, sclass
	syntax [anything], [ tvars(string) xvars(string) tr(string)]
	
	local kx: list sizeof xvars
	local kt: list sizeof tvars 
	local stripe "ATET:r1vs0.`tr'"
	
	forvalues i=1/`kt' {
			local tv: word `i' of tvars 
			local stripe "`stripe' treatment:`tv'"
	}
	forvalues i=1/`kx' {
			local xv: word `i' of xvars 
			local stripe "`stripe' treatment:`xv'"
	}
	sreturn local stripe "`stripe'"
end

program define _Init_Values, rclass sortpreserve
	syntax [anything] [if][in]								///
					  [fweight iweight pweight aweight], 	///
					  [										///
					  estimator(string) 					///
					  pscore(string) 						///
					  xvars(string) 						///
					  treatvar(string)						///
					  groupvar(string)						///
					  lhs(string)							///
					  timevar(string)						///
					  ]

	marksample touse 
	
	if "`weight'" != "" {
		local wgt [`weight' `exp']
	}
	
	tempvar dy pscore wgt0 dyhat att pscore2 ynew consnew dyhat1 dyhat0	///
			sample0 sample1 samplet samplef 
	tempname bps by init muw muatt b V by0 by1
	
	quietly bysort `groupvar' (`timevar'): ///
		generate double `dy'=`lhs'[2]-`lhs'[1] if `touse'
	
	markout `touse' `dy'
	
	local pest logit 
	if ("`pscore'"=="probit") {
	    local pest probit
	}
	if ("`pscore'"=="imp"|"`estimator'"=="imp") {	
		quietly mlexp (`treatvar'*{xb:`xvars' _cons}- ///
			(`treatvar'==0)*exp({xb:}) ) if `touse' `wgt'			
		matrix `bps' = e(b)		
	}
	
	if ("`estimator'"=="dripw") {
		quietly `pest' `treatvar' `xvars' if `touse' `wgt'
		matrix `bps' = e(b)
		quietly predict double `pscore' if `touse', pr 
		quietly reg `dy' `xvars' if `treatvar'==0 & `touse' `wgt'
		matrix `by' = e(b)
		quietly predict double `dyhat' if `touse'
		
		quietly gen double `wgt0' = `pscore' * (1 - `treatvar')/(1 - `pscore')
		quietly mean `wgt0' `treatvar' if `touse' `wgt'
		matrix `muw' = e(b)
		
		quietly generate double `att'= ///
			(`treatvar'/`muw'[1,2]-`wgt0'/`muw'[1,1])*(`dy'-`dyhat') if `touse'

		quietly mean `att' if `touse' `wgt'
		matrix `muatt' = e(b)
		
		matrix `init' = `muatt', `bps', `by', `muw'
	}
	if ("`estimator'"=="imp") {	
		quietly predictnl double `pscore2'=logistic(xb()) if `touse'
		quietly generate double `wgt0'=	///
			((`pscore2'*(1-`treatvar')))/(1-`pscore2')
		
		quietly mean `wgt0' `treatvar' if `touse' `wgt'
		matrix `muw' = e(b)
		local k: list sizeof xvars
		local k = `k'
		quietly generate double `ynew'    = `dy'*sqrt(`wgt0')
		quietly generate double `consnew' = sqrt(`wgt0')
		forvalues i=1/`k' {
			local x: word `i' of `xvars'
			tempvar xvar`i' 
			quietly generate double `xvar`i'' = sqrt(`wgt0')*`x'
			local newvars "`newvars' `xvar`i''"
			local stripes "`stripes' `x'"
		}
		local newvars "`newvars' `consnew'"
		local stripes "`stripes' _cons"

		quietly regress `ynew' `newvars' if (`treatvar'==0 & `touse')	///
			`wgt', nocons
		matrix `by' = e(b)
		matrix colnames `by' = `stripes'
		quietly matrix score double `dyhat' = `by' if `touse'
		quietly generate double `att' =	///
			(`treatvar'/`muw'[1,2]-`wgt0'/`muw'[1,1])*(`dy'-`dyhat') if `touse'
		quietly mean `att' if `touse' `wgt'
		matrix `muatt' = e(b)
		matrix `init' = `muatt', `bps', `by', `muw'	
	}
	if ("`estimator'"=="reg") {	
		quietly reg `dy' `xvars' if `treatvar'==0 & `touse' `wgt'
		matrix `by0' = e(b)	
		quietly predict double `dyhat0' if `touse'
		quietly reg `dy' `xvars' if `treatvar'==1 & `touse' `wgt'
		matrix `by1' = e(b)	
		quietly predict double `dyhat1' if `touse'
		quietly count if `touse' 
		local N = r(N)
		quietly count if `treatvar'==1 & `touse'
		local tn = r(N)
		quietly generate double `att' =	///
			`treatvar'*(`dyhat1' - `dyhat0')*(`N'/`tn')
		quietly mean `att' if `touse' `wgt'
		matrix `muatt' = e(b)
		matrix `init' = `muatt', `by1', `by0'	
	}
	if ("`estimator'"=="stdipw") {
		if ("`pscore'"!="imp") {
			quietly `pest' `treatvar' `xvars' if `touse' `wgt'
			matrix `bps' = e(b)
			quietly predict double `pscore' if `touse', pr 			
		}
		else {
			quietly predictnl double `pscore'=logistic(xb()) if `touse'
		}
		qui generate double `wgt0'    = ((1-`treatvar')*`pscore'/(1-`pscore'))
		qui generate double `ynew'    = `dy'*sqrt(`wgt0')
		qui generate double `consnew' = sqrt(`wgt0')
		quietly regress `ynew' `consnew' if `touse' `wgt', nocons
		local stripes "_cons"
		matrix `by0' = e(b)
		matrix colnames `by0' = `stripes'
		quietly regress `dy' if (`treatvar' & `touse') `wgt'
		matrix `by1' = e(b)
		matrix `muatt'  = `by1' - `by0'
		matrix `init'   = `muatt', `bps', `by1', `by0'
	}
	return matrix init = `init' 
end

program define _Eq_List_gmm, sclass 
	syntax [anything], [xvars(string) estimator(string) noCONstant]
	
	if ("`estimator'"=="dripw"|"`estimator'"=="imp") {
		local eqlist "atet treatment outcome w0 w1"
		_Param_list, vars(`xvars') pnom(treatment) `constant'
		local parmt "`s(parmlist)'"
		_Param_list, vars(`xvars') pnom(outcome) `constant'
		local parmy "`s(parmlist)'"
		local parms "atet:_cons `parmt' `parmy' w0:_cons w1:_cons"
	}
	if ("`estimator'"=="reg") {
		local eqlist "atet outcome1 outcome0"
		_Param_list, vars(`xvars') pnom(outcome1) `constant'
		local parmy1 "`s(parmlist)'"
		_Param_list, vars(`xvars') pnom(outcome0) `constant'
		local parmy2 "`s(parmlist)'"
		local parmy "`parmy2'"
		local parms "atet:_cons `parmy1' `parmy2'"		
	}
	if ("`estimator'"=="stdipw") {
		local eqlist "atet treatment outcome1 outcome0"
		_Param_list, vars(`xvars') pnom(treatment) `constant'
		local parmy1 "`s(parmlist)'"
		_Param_list, vars(`xvars') pnom(outcome0) `constant'
		local parmy2 "`s(parmlist)'"
		local parmy "`parmy2'"
		local parms "atet:_cons `parmy1' outcome1:_cons outcome0:_cons "		
	}
	
	sreturn local parms "`parms'"
	sreturn local parmy "`parmy'"
	sreturn local parmt "`parmt'"
	sreturn local eqlist "`eqlist'"
end

program define _Parse_pscore
	capture syntax [anything], [probit logit imp]
	local rc = _rc
	if (`rc') {
		display as error "invalid {bf:pscore()}"
		display as txt "{p 4 4 2}"                           
		display as smcl as err ///
			"{bf:pscore()} should be one of {bf:probit}, "       ///
			"{bf:logit}, or {bf:imp}"      
			di as smcl as err  "{p_end}"
		exit `rc'
	}
	local muchos "`probit' `logit' `imp'"
	local k: list sizeof muchos
	if (`k'>1) {
		display as error ///
			"only one of {bf:probit}, {bf:logit}, or {bf:imp} is allowed"
		exit 198
	}
end



