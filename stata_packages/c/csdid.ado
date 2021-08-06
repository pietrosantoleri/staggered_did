
** NOTE when Panel is unbalanced (check this somehow.)
** there could be two solutions.
** 1 Using strong balance 
** 2 use semi balance (whenever att_gt exists)
** 3 use weak balance/crossection with cluster.
** Ultimate check. Do thestatistics Once.
*! v1.51  by FRA. Added seed and method. for bootstrap
*! v1.3  by FRA. Overhaul Change in how Weights are estimated. All effects are now in Mata.
* All effects in mata. Quick!
*! v1.03 by FRA. Adding Balance checks
*! v1.02 by FRA. added touse
*! v1.01 by FRA. Make sure we have at least 1 not treated period.
// and check that if no Never treated exist, we add extra variable for dummies.
// exclude if There are no treated observations
* v1 by FRA csdid Added all tests! and ready to play
* v0.2 by FRA csdid or Multiple periods did. Adds Notyet

* v0.1 by FRA mpdid or Multiple periods did.
* This version implements did::attgp
* Logic. Estimate the ATT of the base (first) year against all subsequent years
* using data BY groups
** assumes all years are available. For now
 
/*program csdid_check,
syntax varlist(fv ) [if] [in] [iw], /// Basic syntax  allows for weights
										[Ivar(varname)]  ///
										Time(varname)    ///
										[gvar(varname)]  /// Ppl either declare gvar
										[trvar(varname)] /// or declare treat var, and we create gvar. Only works if Panel data
										[att_gt]  ///
										[notyet] /// att_gt basic option. May prepare others as Post estimation
										[method(str) ]  // This allows other estimators
	
end*/
*capture program drop sdots
*capture program drop csdid
program sdots
syntax , [mydots(integer 10) maxdots(int 50) bad]
	
	if mod(`mydots',`maxdots')==0 {
		if "`bad'"==""	display "." 
		else display "x" 
	}
	else {
		if "`bad'"==""	display "." _c
		else display "x" _c
	}
end


program csdid, sortpreserve eclass
        version 14
        if replay() {
                if `"`e(cmd)'"' != "csdid" { 
                        error 301
                }
                else {
                        Display `0'
                }
                exit
        }
		if runiform()<.001 {
				easter_egg
		}
		
		capture which drdid
		if _rc!=0 {
			display in red "Program drdid.ado not found. Make sure you have a copy of it installed." as text
			exit 101
		}
		csdid_r `0'
		
        *ereturn local cmdline `"drdid `0'"'
end

 program define Display
                syntax [, bmatrix(passthru) vmatrix(passthru) *]
                
        _get_diopts diopts rest, `options'
        local myopts `bmatrix' `vmatrix'        
                if ("`rest'"!="") {
                                display in red "option {bf:`rest'} not allowed"
                                exit 198
                }
                
                _S_Me_thod
                local omodel "`s(omodel)'"
                local tmodel "`s(tmodel)'"

                
                if ("`e(method)'"!="all") {
                        _coef_table_header, title(Difference-in-difference with Multiple Time Periods) 
                        noi display as text "Outcome model  : {res:`omodel'}"
                        noi display as text "Treatment model: {res:`tmodel'}"
                }
				
				if ("`e(vcetype)'"=="WBoot") {
					if "`e(clustvar)'"!="" {
						
						display as text "(Std. err. adjusted for" ///
						as result %9.0gc e(N_clust) ///
						as text " clusters in " as result e(clustvar) as text ")"
					}
                    csdid_table, `diopts'
                }
                else {
                    _coef_table,  `diopts' `myopts' neq(`e(neqr)')
                }
                
 
end


program define _S_Me_thod, sclass
        if ("`e(method)'"=="drimp") {
                local tmodel "inverse probability tilting"
                local omodel "weighted least squares"
        }
        if ("`e(method)'"=="dripw") {
                local tmodel "inverse probability"
                local omodel "least squares"
        }
		if ("`e(method)'"=="aipw") {
                local tmodel "inverse probability"
                local omodel "weighted mean"
        }
        if ("`e(method)'"=="reg") {
                local tmodel "none"
                local omodel "regression adjustment"
        }
        if ("`e(method)'"=="stdipw") {
                local tmodel "stabilized inverse probability"
                local omodel "weighted mean"
        }

        sreturn local omodel "`omodel'"
        sreturn local tmodel "`tmodel'"
end

 
program _Parse2_wboot, rclass
	syntax, [reps(integer 999) wbtype(str) rseed(str) cluster(str)]
	return scalar reps    = `reps'
	return local seed 	   `rseed'
	return local  cluster  `cluster'
	
	if ("`wbtype'"=="") {
		    return scalar wbtype = 1

		}
	else if ("`wbtype'"=="mammen") {
		    return scalar wbtype = 1
		}
	else if ("`wbtype'"=="rademacher") {
		    return scalar wbtype = 2
		}
	else if ("`wbtype'"!="rademacher" & "`wbtype'"!="mammen") {
		    display as error "invalid {bf:wbtype()}"
			di as txt "{p 4 4 2}"                           
			di as smcl as err ///
			"{bf:wbtype()} should be one of {bf:mammen} or " ///
			"{bf:rademacher}."      
			di as smcl as err  "{p_end}"
			exit 198 
		}	
		
end 

program _Parse_Wildboot, rclass 
	syntax 			, [			///
					   WBOOT1				///
					   WBOOT(str)			///
					   reps(integer 999) 	///
					   rseed(string) 		///
					   wbtype(string)		///
					   cluster(string)		///
					   ]

	marksample touse 
	if ("`wboot1'"=="" & "`wboot'"!="") {
	    _Parse2_wboot, `wboot'
		return scalar reps= `r(reps)'
		return local  seed `r(seed)'
		return scalar wbtype= `r(wbtype)'
		return local  cluster `r(cluster)'
		
	}
	else if ("`wboot1'"!="") {
		return scalar reps = `reps'
		return local seed 	   `rseed'
		if ("`wbtype'"=="") {
		    local wbtypen = 1
		}
		else if ("`wbtype'"=="mammen") {
		    local wbtypen = 1
		}
		else if ("`wbtype'"=="rademacher") {
		    local wbtypen = 2
		}
		else if ("`wbtype'"!="rademacher" & "`wbtype'"!="mammen") {
		    display as error "invalid {bf:wbtype()}"
			di as txt "{p 4 4 2}"                           
			di as smcl as err ///
			"{bf:wbtype()} should be one of {bf:mammen} or " ///
			"{bf:rademacher}."      
			di as smcl as err  "{p_end}"
			exit 198 
		}
		return scalar wbtype = `wbtypen' 
		if ("`cluster'"!="") {
		    tempvar nclust wncl0
			capture confirm numeric variable `cluster'
			local rc = _rc
			if (`rc') {
				capture destring `rest', generate(`nclust')
				local rc = _rc 
				if (`rc') {
					display in red "option {bf:cluster()} incorrectly specified"
					exit 198
				}
				capture confirm numeric variable `nclust'
				local rc = _rc 
				if (`rc') {
					display in red "option {bf:cluster()} incorrectly specified"
					exit 198
				}
			}
		}
		return local cluster `cluster'
	}
end 

program issaverif, 
	syntax, [saverif(str) replace]
	if "`saverif'"!="" {
	    capture confirm new file "`saverif'.dta" 
		if _rc!=0 {
			if "`replace'"=="" {
			    display in red "File `saverif'.dta already exists"
				display in red "Please use a different file name or use -replace-"
				exit 602
			}
			else {
			    display "File `saverif'.dta will be replaced"
			}
		}
		else {
		    display "File `saverif'.dta will be used to save all RIFs"
		}
	}
end

program csdid_r, sortpreserve eclass
	syntax varlist(fv ) [if] [in] [iw], 			/// Basic syntax  allows for weights
							[Ivar(varname)] 		///
							Time(varname)  			///
							Gvar(varname)  			/// Ppl either declare gvar
							[cluster(varname)] 		/// 
							[notyet] 				/// att_gt basic option. May prepare others as Post estimation
							[saverif(name) replace ] ///
							[method(str) bal(str)	 ///
							agg(str)  				///
							WBOOT(str) 				///
							WBOOT1					///
							reps(int 999) 			///
							wbtype(str)  			/// Hidden option
							rseed(str)				/// set seed
							Level(int 95)			/// CI level
							]  // This allows other estimators
				
	marksample touse
	** First determine outcome and xvars
	gettoken y xvar:varlist
	markout `touse' `ivar' `time' `gvar' `y' `xvar' `cluster'
	tempname cband
	** Parsing WBOOT
	_Parse_Wildboot, wboot(`wboot') `wboot1' reps(`reps') wbtype(`wbtype') rseed(`rseed') cluster(`cluster')
	
	if "`wboot'`wboot1'"!="" {
	    local cluster 	`r(cluster)'
		local ocluster 	`r(cluster)'
		local seed 		`r(seed)'
		if "`wbtype'"=="" local owbtype mammen
		local owbtype   `wbtype' 
		local wbtype 	`r(wbtype)'
		local reps 		`r(reps)'
		local vcetype 	WBoot
	}
	else {
	    local wbtype 1
	}
	if "`seed'"!="" set seed `seed'
	** Original cluster
	local ocluster 	`cluster'
	** Confirm if new file exists
	issaverif, saverif(`saverif') `replace'
	
	
    if "`agg'"=="" {
		local agg attgt
	}
	
	if !inlist("`agg'","attgt","simple","pretrend","group","calendar","event") {
		display in red "Aggregation not Allowed"
		exit 10
	}
	
	if "`tyet'"=="" {
		qui:count if `touse' & `gvar'==0
		if r(N)==0 {
			local tyet notyet
			display "No never treated observations found. Using Not yet treated data"
		}	
	}
	
	** checking for Min gvar being larger than min time
	qui:sum `time' if `touse', meanonly 
	local mintime=r(min)
	qui:sum `gvar' if `touse' & `gvar'>0, meanonly 
	local mingvar=r(min)
	
	if `mintime'>=`mingvar' {
		display "Units always treated found. These will be excluded"
		qui:replace `touse'=0 if (`gvar'<=`mintime') & (`gvar'>0) & `touse'
	}
	
	
	** determine time0
	if "`time0'"=="" {
	    qui:sum `time' if `touse'
		local time0 `r(min)'
	}
	
	** Check if panel is Full Balance
	qui {
	    // Only if panel
	    if "`ivar'"!="" {
			** First check if data is Panel
			tempname ispan
			
			capture xtset
			if _rc!=0 {
				qui:xtset `ivar' `time'
				qui:xtset ,  clear
			}
			
			
			if "`cluster'"!="" {
				_xtreg_chk_cl2 `cluster' `ivar'
			}
			
			tempname isbal
			mata:isbalanced("`ivar'","`touse'","`isbal'")

			if scalar(`isbal')!=1 {
			    display in red "Panel is not balanced"
				if "`bal'"=="full" {
					display in red "Will use Only observations fully balanced (Max periods observed)"
					replace `touse'=0 if `x1'<`mxcol'
				}
				if "`bal'"=="" {
					display in red "Will use observations with Pair balanced (observed at t0 and t1)"
				}
				if "`bal'"=="unbal" {
					display in red "Will use all observations (as if CS)"
					local cluster `ivar'
					local ivar    
				}
			}	
		}
	}
	
	** prepare loops over gvar.
	*local att_gt att_gt
	tempvar tr
	tempname gtt
	
	qui:gen byte `tr'=`gvar'!=0 if `gvar'!=.
	qui:levelsof `gvar' if `gvar'>0       & `touse', local(glev)
	qui:levelsof `time' if `time'>`time0' & `touse', local(tlev)
	
	** Checks If Gvar is ever untreated
	if `:word 1 of `glev'' < `:word 1 of `tlev'' {
	    display in red "All observations require at least 1 not treated period"
		display in red "See cross table below, and verify All Gvar have at least 1 not treated period"
		tab `time' `gvar' if `touse'
		error 1
	}
 		tempname b v
 		foreach i of local glev {		
			local time1 = `time0'
 
		    foreach j of local tlev {
				local mydots=`mydots'+1
				* Stata quirk: `notyet' is called `tyet' because "no" is removed
			    if "`tyet'"=="" {
				    ** This implements the Never treated
					qui:capture:drdid `varlist' if inlist(`gvar',0,`i') ///
								 & inlist(`time',`time1',`j') ///
								 & `touse' [`weight'`exp'], ///
								ivar(`ivar') time(`time') treatment(`tr') ///
								`method' stub(__) replace
					if _rc!=0 local	bad bad
					if _rc == 1 {
						display in red "Stopped by user"
						capture drop `vlabrif'
						capture drop __att
						error 11882
					}
					
					matrix `gtt'=nullmat(`gtt')\[`i',`time1',`j',_rc]
					sdots, mydots(`mydots') `bad'
					local bad
					local eqname `eqname' g`i'
					local colname `colname'  t_`time1'_`j'
					capture drop _g`i'_`time1'_`j'
					
				    capture   confirm variable __att
					if 	_rc==111	qui:      gen _g`i'_`time1'_`j'=.
					else 		    ren __att     _g`i'_`time1'_`j'
					
					local vlabrif `vlabrif' _g`i'_`time1'_`j'
					if `j'<`i' local time1 = `j'					
					
				}
				else if "`tyet'"!="" {
				    ** This will implement not yet treated.////////////////////
					//////////////////////////////////////
					qui:replace `tr'=`gvar'==`i' if `touse'
					*local time1 = min(`i'-1, `j'-1)
					* Use as controls those never treated and those not treated by time `j'>`i'
					qui:capture:drdid `varlist' if (`gvar'==0 | `gvar'==`i' | `gvar'> `j') ///
													& inlist(`time',`time1',`j') ///
					                                & `touse' [`weight'`exp'], ///
													ivar(`ivar') time(`time') treatment(`tr') `method' stub(__) replace
					
					if _rc!=0 local	bad bad
					if _rc == 1 {
						display in red "Stopped by user"
						capture drop `vlabrif'
						capture drop __att
						error 11882
					}
					matrix `gtt'=nullmat(`gtt')\[`i',`time1',`j',_rc]
					sdots, mydots(`mydots') `bad'
					local bad
					local eqname `eqname' g`i'
					local colname `colname'  t_`time1'_`j'
					capture drop _g`i'_`time1'_`j'					
				    capture confirm variable __att
					if 	_rc==111	qui:gen _g`i'_`time1'_`j'=.
					else 			ren __att    _g`i'_`time1'_`j'
					local vlabrif `vlabrif' _g`i'_`time1'_`j'
					if `j'<`i' local time1 = `j'	
				}
				local rifvar `rifvar' _g`i'_`time1'_`j'
			}
		}
		** names for Weights To be changed
		
		foreach i of local glev {
			local neqr = `neqr'+1
			foreach j of local tlev {
				local eqname `eqname' wgt
				local colname `colname'  w`i'_`j'				
			}
		}
////////////////////////////////////////////////////////////////////////////////
		
		preserve
			qui:notes drop _all
			tempvar wgtt
			if "`exp'"!="" clonevar `wgtt'`exp'
			else gen byte `wgtt'=1
			qui:keep if `touse'
			qui:keep `ivar' `time' `gvar' `vlabrif' `wgtt' `cluster'
			local oivar `ivar'
			if "`ivar'"=="" {
			   	qui:gen double ivar=_n
				label var ivar "indicator"
				local ivar ivar
			}
			
			collapse   `time'  `gvar' `vlabrif' `wgtt' `cluster', by(`ivar' ) fast
			ren `wgtt' __wgt__
			label var  __wgt__  "Weight Variable"
			*qui:levelsof `gvar' if `gvar'!=0, local(gglev) 
			foreach i of local glev {
				foreach j of local tlev {
					fvexpand _g`i'_*_`j'
					qui:gen double w`i'_`j'=0
					qui:replace w`i'_`j'=1 if (`r(varlist)'!=.) & (`gvar'==`i')
					local lvl_gvar `lvl_gvar' w`i'_`j'
				}
			}
			*** Organizing all data.
			*** first Gvars
			if "`cluster'"!="" {
				qui:egen long _cl_var=group(`cluster')
				*qui:sort _cl_var
				label var _cl_var "Effective cluster"
				local cluster _cl_var
			}
			
			/// saving RIF
			note: Data created with -csdid-. Contains all -RIFs- associated with model estimation. 
			note: cmdline csdid `0'
			note: time0 `time0'
			note: tlvls `tlev'
			note: glvls `glev'
			note: clvar `cluster'
			note: rifgt `vlabrif'
			note: rifwt `lvl_gvar'
			note: colname `colname'
			note: eqname  `eqname'
			note: cmd  csdid
			
			/// parsing notes.
			forvalues i = 3/8 {
				local  `:char _dta[note`i']'
			}
			
			/// "Option will define the following
			//  1 simple
			//  2 dynamic
			//  3 calendar
			//  4 group
			//  5 attgt
			//  6 pretrend
			//  7 all 
			tempname b1 b2 b3 b4 b5 
			tempname s1 s2 s3 s4 s5 
			
			** New idea. Hacerlo todo desde makerif	
			*mata:makerif("`rifgt'","`rifwt'","__wgt__","`b'","`v'","`cluster' ")
			*save extra, replace
			local ci `level'/100
			if "`wboot'`wboot1'"!="" local wboot wboot
			
			noisily mata: makerif2("`rifgt'" , "`rifwt'","__wgt__","`agg'",  ///
								  "`time0'","`glvls'","`tlvls'", ///
									"`b1'",  /// `b2' `b3' `b4' `b5' `b6'
									"`s1'",  ///  `s2' `s3' `s4' `s5' `s6'
									"`clvar' ","`wboot' ", "`cband'", /// 
									`ci', `reps', `wbtype')
									
			** Time0 `time0'		
			if "`saverif'"!="" {
			    save `saverif', replace
			}
		restore
		
		capture drop `vlabrif'
////////////////////////////////////////////////////////////////////////////////
	if "`agg'"=="attgt" {
	
		matrix colname `b1'=`colname'
		matrix coleq   `b1'=`eqname'
		matrix colname `s1'=`colname'
		matrix coleq   `s1'=`eqname'
		matrix rowname `s1'=`colname'
		matrix roweq   `s1'=`eqname'
	}
		matrix colname b_attgt=`colname'
		matrix coleq   b_attgt=`eqname'
		matrix colname V_attgt=`colname'
		matrix coleq   V_attgt=`eqname'
		matrix rowname V_attgt=`colname'
		matrix roweq   V_attgt=`eqname'
		
	*** Mata to put all together.
 
	ereturn post `b1' `s1'
	ereturn local cmd 		csdid
	ereturn local cmdline 	csdid `0'
	
	ereturn local estat_cmd csdid_estat
	ereturn matrix b_attgt  b_attgt 
	ereturn matrix V_attgt  V_attgt 
	ereturn matrix gtt  	`gtt'
	ereturn local agg     	`agg'
	ereturn local glev 		`glev'
	ereturn local tlev 		`tlev'
	ereturn local time0		`time0'
	ereturn local rif 		`rifvar'
	ereturn local ggroup 	`gvar'
	ereturn local id	 	`ivar'
	ereturn scalar neqr	=	`neqr'
	ereturn local riffile	`saverif'
	ereturn local method 	`method'
	if "`ocluster'"!="" {
		ereturn local clustvar 	`ocluster'
		ereturn scalar N_clust = `=scalar(clnm)'
	}
	/// Add here Cluster var and number of Clusters.
	if  "`tyet'"=="" ereturn local control_group "Never Treated"
	if  "`tyet'"!="" ereturn local control_group "Not yet Treated"
	
	if "`vcetype'"=="WBoot" {
		ereturn local  seed	  `seed'
		ereturn local wbtype `wbtype'
		ereturn scalar reps   =		`reps'
		matrix colname `cband'= b se t ll ul
		ereturn matrix cband  	`cband'
		ereturn local vcetype 	WBoot
	}
		    
		
	Display
	display "Control: `e(control_group)'" 
	display _n "See Callaway and Sant'Anna (2020) for details"
end 

/// This can be used for aggregation. Creates the matrixes we need.


program define easter_egg
                display "{p}This is just for fun. Its my attempt to an Easter Egg within my program. {p_end}" _n /// 
                "{p} Also, if you are reading this, it means you are lucky," ///
                "only 0.1% of people using this program will see this message. {p_end}" _n ///
                "{p} This program was inspired by challenge post by Scott Cunningham. " ///
                "It is the second part of Pedro, Brantly and Juns's contribution to the DID world{p_end} " _n  ///
                "{p} Remember One Difference is good, and 2x2 DiD is twice as good!. " ///
				" Just dont confuse it with DnD (Dungeons and Dragons){p_end} "
end


mata:
 vector event_list(real matrix glvl, tlvl){
 	real matrix toreturn
	real scalar i,j
	toreturn=J(1,0,.)
	for(i=1;i<=cols(glvl);i++) {
		for(j=1;j<=cols(tlvl);j++) {
			toreturn=toreturn,(tlvl[j]-glvl[i])
		}
	}
	return(uniqrows(toreturn')')
 }
// Next task. 
// amek all elements separete RIF_siple RIF event, etc
// Think how to save all elements.
		
void makerif2(string scalar rifgt_ , rifwt_ , wgt_, agg, 
				time0, glvl_, tlvl_, bb_, ss_, clvar_, wboot_ , cband, 
				real scalar ci, reps, wbtype ) {	
    real matrix rifgt , rifwt, wgt, t0, glvl, tlvl
	real scalar i,j,k,h
	rifgt	= st_data(.,rifgt_)
	rifwt  	= st_data(.,rifwt_)
	wgt   	= st_data(.,wgt_)
	wgt	  	= wgt:/mean(wgt)
	rifwt	= rifwt:*wgt
	rifwt	= rifwt:/rowsum(mean(rifwt))
	wgt		=.
	
	/// pg here is just a dummy
	// stp1 all together?? No
	//all=att_gt,pg
	// stp2 get Mean(RIF) 
	// This just rescales the IFs RIF's to make the statistics later.
	real matrix mean_y, exp_factor
	mean_y     = colsum(rifgt):/colnonmissing(rifgt)
	_editmissing(mean_y,0)
	exp_factor = (rows(rifgt):/colnonmissing(rifgt))
	rifgt      = rifgt:-mean_y
	_editmissing(rifgt,0)
	_editmissing(exp_factor,0)
	rifgt      =rifgt:*exp_factor:+mean_y
	
	mean_y     =colsum(rifwt):/colnonmissing(rifwt)
	_editmissing(mean_y,0)
	exp_factor = (rows(rifwt):/colnonmissing(rifwt))
	rifwt      =rifwt:-mean_y
	_editmissing(rifwt,0)
	_editmissing(exp_factor,0)
	rifwt      =rifwt:*exp_factor:+mean_y
    st_store(.,tokens(rifgt_+" "+rifwt_),(rifgt,rifwt))
	/// Extra stuff 
	t0   = strtoreal(tokens(time0))	
	glvl = strtoreal(tokens(glvl_))	
	tlvl = strtoreal(tokens(tlvl_))	
	
    real matrix ag_rif, ag_wt
	real matrix bb, VV, VV1, aux
	real vector ind_gt, ind_wt
	string matrix coleqnm
	/////////////////////////////////////////
	// Always make attgt, even if not shown. 
 
	//VV-VV1
	
	if (wboot_==" ") {
		make_tbl( (rifgt,rifwt) ,bb,VV,clvar_, wboot_ ,VV1 , cband, ci, reps, wbtype)
		st_matrix("b_attgt",bb)
		st_matrix("V_attgt",VV)
	}
	else             {
		make_tbl( (rifgt,rifwt) ,bb,VV,clvar_, " "    ,VV1, cband, ci, reps, wbtype )
		st_matrix("b_attgt",bb)
		st_matrix("V_attgt",VV)
		make_tbl( (rifgt,rifwt) ,bb,VV,clvar_, wboot_ ,VV1 , cband, ci, reps, wbtype)
		
	}
	
	//if (wboot_!=" ") make_tbl( (rifgt,rifwt) ,bb,VV,clvar_, wboot_ , VV1)
	/////////////////////////////////////////
	if (agg=="simple") {
		k=0
		ind_gt=J(1,0,.)
		ind_wt=colsum(abs(rifgt))
 
		for(i=1;i<=cols(glvl);i++) {
			for(j=1;j<=cols(tlvl);j++) {
				k++
				// G <= T
 				if ( (glvl[i]<=tlvl[j]) & (ind_wt[k]!=0) ) {
					ind_gt=ind_gt,k
				}
 			}
		}
		// Above gets the Right elements Below, aggregates them
		ag_rif = rifgt[.,ind_gt]
		ag_wt  = rifwt[.,ind_gt]
		aux = aggte(ag_rif, ag_wt)
		make_tbl(aux ,bb,VV,clvar_, wboot_ , VV1, cband, ci, reps, wbtype)
		coleqnm = "ATT"
	}
	/////////////////////////////////////////simple pretrend
	real scalar dfchi, flag
	if (agg=="pretrend") {
		k=0
		ind_gt=J(1,0,.)
  		ind_wt=colsum(abs(rifgt))

		for(i=1;i<=cols(glvl);i++) {
			for(j=1;j<=cols(tlvl);j++) {
				k++
 				if ( (glvl[i]>tlvl[j]) & (ind_wt[k]!=0) ) {
					//ag_rif=ag_rif, rifgt[.,k]
					ind_gt=ind_gt,k
 				}
 			}
		}
		real scalar nn2
		ag_rif = rifgt[.,ind_gt]
		nn2 = rows(ag_rif)^2
		bb = nn2 * mean(ag_rif) * 
			 invsym(crossdev(ag_rif,mean(ag_rif),ag_rif,mean(ag_rif))) * 
			 mean(ag_rif)'
		dfchi = cols(ag_rif)	
	 	
	}
	/////////////////////////////////////////simple pretrend group
	if (agg=="group") {
		// i groups j time
		k=0
		ind_wt=colsum(abs(rifgt))
	
		aux    =J(rows(rifwt),0,.)
		coleqnm=""
		/// ag_wt=J(rows(rifwt),0,.)
		for(i=1;i<=cols(glvl);i++) {
		ind_gt=J(1,0,.)
		
		flag=0
			ag_rif=J(rows(rifwt),0,.)
			for(j=1;j<=cols(tlvl);j++) {
				k++
 				if ((glvl[i]<=tlvl[j]) & (ind_wt[k]!=0)) {
					//ag_rif=ag_rif, rifgt[.,k]
					flag=1
					ind_gt=ind_gt,k
 				}
 			}
			if (flag==1)  {
				coleqnm=coleqnm+sprintf(" G%s",strofreal(glvl[i]))
				ag_rif = rifgt[.,ind_gt]
				ag_wt  = rifwt[.,ind_gt]
 				aux = aux, aggte(ag_rif, ag_wt)
			}	
		}
		// get table elements		
		make_tbl(aux ,bb,VV,clvar_, wboot_ ,VV1, cband, ci, reps, wbtype)
	}	
	/////////////////////////////////////////

	if (agg=="calendar") {
		// i groups j time
		aux =J(rows(rifwt),0,.)
		coleqnm=""
		ind_wt=colsum(abs(rifgt))

		for(h=1;h<=cols(tlvl);h++){
			k=0
			flag=0
			ind_gt=J(1,0,.)
			//ind_wt=J(1,0,.)	
			/// ag_wt=J(rows(rifwt),0,.)
 
			for(i=1;i<=cols(glvl);i++) {
				for(j=1;j<=cols(tlvl);j++) {
					k++
					if ( (glvl[i]<=tlvl[j]) & (tlvl[h]==tlvl[j]) & (ind_wt[k]!=0) ){
						//ag_rif=ag_rif, rifgt[.,k]
						//ag_wt =ag_wt , rifwt[.,i]
						ind_gt=ind_gt,k
						ind_wt=ind_wt,i						
						if (flag==0) coleqnm=coleqnm+sprintf(" T%s",strofreal(tlvl[h]))
						flag=1
					}
				}
				
			}
			if (flag==1) {
				ag_rif = rifgt[.,ind_gt]
				ag_wt  = rifwt[.,ind_gt]			
				aux = aux, aggte(ag_rif, ag_wt)
			}
		}	
		// get table elements		
		make_tbl(aux ,bb,VV,clvar_, wboot_ ,VV1, cband, ci, reps, wbtype )
	}
	
	if (agg=="event") {
		// i groups j time
		real matrix evnt_lst
		evnt_lst=event_list(glvl,tlvl)
		coleqnm=""
		ind_wt=colsum(abs(rifgt))

		aux =J(rows(rifwt),0,.)
		for(h=1;h<=cols(evnt_lst);h++){
			k=0
			flag=0
			ind_gt=J(1,0,.)
 			/// ag_wt=J(rows(rifwt),0,.)
 
			for(i=1;i<=cols(glvl);i++) {
				for(j=1;j<=cols(tlvl);j++) {
					k++					
					if ( ( (glvl[i]+evnt_lst[h])==tlvl[j] )  & (ind_wt[k]!=0) ) {	
						//ag_rif=ag_rif, rifgt[.,k]
						//ag_wt =ag_wt , rifwt[.,i]
						ind_gt=ind_gt,k
						ind_wt=ind_wt,i							
						if (flag==0) {
							if (evnt_lst[h]< 0) coleqnm=coleqnm+sprintf(" T%s" ,strofreal(evnt_lst[h]))
							if (evnt_lst[h]==0) coleqnm=coleqnm+" T+0"
							if (evnt_lst[h]> 0) coleqnm=coleqnm+sprintf(" T+%s",strofreal(evnt_lst[h]))
						}
						flag=1
					}
				}
				
			}
			if (flag==1) {
				ag_rif = rifgt[.,ind_gt]
				ag_wt  = rifwt[.,ind_gt]			
				aux = aux, aggte(ag_rif, ag_wt)
			}	
		}	
		// get table elements		
		make_tbl(aux ,bb,VV,clvar_, wboot_ ,VV1, cband, ci, reps, wbtype)
	}
	
	st_matrix(bb_,bb)
	st_matrix(ss_,VV)
	
	
	if (agg!="attgt") {
		stata("matrix colname "+bb_+" ="+coleqnm)
		stata("matrix colname "+ss_+" ="+coleqnm)
		stata("matrix rowname "+ss_+" ="+coleqnm)
	}
	
}

void make_tbl(real matrix rif,bb,VV, clv , wboot ,VV1 , string scalar cband_,
			  real scalar ci, reps, wbtype){
	real matrix aux, nobs, clvar
	real scalar cln
	bb=mean(rif)
	nobs=rows(rif)
 
	// simple
	if ((clv==" ") & (wboot==" ")) {	
		VV=quadcrossdev(rif,bb,rif,bb):/ (nobs^2) 
	}
	// cluster std
	if ((clv!=" ") & (wboot==" ")) {
		clvar=st_data(.,clv)
		clusterse((rif:-bb),clvar,VV)
	}
	real matrix cband
	// wboot no cluster
	if ( wboot!=" ") {
		mboot(rif,bb, VV, cband, clv, VV1,  ci, reps, wbtype)
		st_matrix(cband_, cband)
	}
	if (clv!=" ") {
	    cln = rows(uniqrows(st_data(.,clv)))
	}
	st_numscalar("clnm",cln)

	
 } 

void clusterse(real matrix iiff, cl, V){
    /// estimates Clustered Standard errors
    real matrix ord, xcros, ifp, info, vv 
	//1st get the IFS and CL variable. 
	//iiff = st_data(.,rif,touse)
	//cl   = st_data(.,clvar,touse)
	// order and sort them, Make sure E(IF) is zero.
	ord  = order(cl,1)
	//iiff = iiff:-mean(iiff)
	iiff = iiff[ord,]
	cl   = cl[ord,]
	// check how I cleaned data!
	info  = panelsetup(cl,1)
	// faster Cluster? Need to do this for mmqreg
	ifp   = panelsum(iiff,info)
	xcros = quadcross(ifp,ifp)
	real scalar nt, nc
	nt=rows(iiff)
	nc=rows(info)
	V =	xcros/(nt^2)
 
	// Esto es para ver como hacer clusters.
	//*nc/(nc-1)
	//st_matrix(V,    vv)
	//st_numscalar(ncl, nc)
	//        ^     ^
	//        |     |
	//      stata   mata
}

void isbalanced(string ivar, touse, isbal) {
    real matrix xx, info
	xx= st_data(.,ivar,touse)
	xx=sort(xx,1)
	info=panelsetup(xx,1)
	info=info[,2]:-info[,1]:+1
	st_numscalar(isbal,max(info)==min(info))
}

void ispanel(string itvar, touse, ispan) {
    real matrix xx, info
	xx= st_data(.,itvar,touse)
	info=uniqrows(xx,1)[,3]
	st_numscalar(ispan,max(info))
}

real colvector aggte(real matrix attg, wgt){
	real scalar atte, mn_attg, mn_wgt
	real vector wgtw, attw
	real matrix r1, r2, r3
	mn_attg = mean(attg)
	mn_wgt  = mean(wgt)
	atte = sum(mn_attg:*mn_wgt):/sum(mn_wgt)
	wgtw = (mn_wgt ) :/sum(mn_wgt)
	attw = (mn_attg) :/sum(mn_wgt)
	r1   = (wgtw:*(attg:-mn_attg))
	r2   = (attw:*(wgt :-mn_wgt ))
	r3   = (wgt :- mn_wgt) :* (atte :/ sum(mn_wgt) )
	return(rowsum(r1):+rowsum(r2):-rowsum(r3):+atte)
    
}

/////////////////////////////////////////////////////////
real matrix mboot_did(real matrix rif, mean_rif, real scalar reps, bwtype) {
	real matrix yy, bsmean
	yy=rif:-mean_rif
 	bsmean=J(reps,cols(yy),0)
	real scalar i,n, k1, k2
	n=rows(yy)
	k1=((1+sqrt(5))/(2*sqrt(5)))
	k2=0.5*(1+sqrt(5)) 
	// WBootstrap:Mammen 
	if (bwtype==1) {			
		for(i=1;i<=reps;i++){
			bsmean[i,]=mean(yy:*(k2:-sqrt(5)*(rbinomial(n,1,1,k1))) )	
		}
	}
	
	else if (bwtype==2) {
		for(i=1;i<=reps;i++){
			bsmean[i,]=mean(yy:*(1:-2*rbinomial(n,1,1,0.5) ) )	
		}
	}
	
	return(bsmean)
}

real matrix mboot_didc(real matrix rif, mean_rif, real scalar reps, bwtype, clv) {
	real matrix yy, bsmean
	yy=rif:-mean_rif
 	bsmean=J(reps,cols(yy),0)
	real scalar i,n, k1, k2, nn
	n=rows(yy)
	k1=((1+sqrt(5))/(2*sqrt(5)))
	k2=0.5*(1+sqrt(5)) 
	real matrix sclv, wmult
	sclv=uniqrows(clv)
	nn=rows(sclv)
	if (bwtype==1) {			
		for(i=1;i<=reps;i++){
		    wmult=(rbinomial(nn,1,1,k1))
			bsmean[i,]=mean(yy:*(k2:-sqrt(5)*wmult[clv] ) )	
		}
	}
	
	else if (bwtype==2) {
		for(i=1;i<=reps;i++){
		    wmult=(rbinomial(nn,1,1,0.5))
			bsmean[i,]=mean(yy:*(1:-2* wmult[clv] ) )	
		}
	}
	
	return(bsmean)
}
 
real matrix iqrse(real matrix y) {
    real scalar q25,q75
	q25=ceil((rows(y)+1)*.25)
	q75=ceil((rows(y)+1)*.75)
	real scalar j
	real matrix iqrs
	iqrs=J(1,cols(y),0)
	for(j=1;j<=cols(y);j++){
	    y=sort(y,j)
		iqrs[,j]=(y[q75,j]-y[q25,j]):/(invnormal(.75)-invnormal(.25) )
	}
	return(iqrs)
}

real vector qtp(real matrix y, real scalar p) {
    real scalar k, i, q
	real matrix yy, qq
	qq=J(1,0,.)
	k = cols(y)
	for(i=1;i<=k;i++){
		yy=sort(y[,i],1)
		q=ceil((rows(yy)+1)*p)    
		qq=qq,yy[q,]
	}
    
	return(qq)
}

real vector qtp2(real matrix y, real scalar p) {
    real scalar k, i, q
	real matrix yy, qq
	qq=J(1,0,.)
	k = cols(y)
	y=rowmax(y)
	for(i=1;i<=k;i++){
		yy=sort(y,1)
		q=ceil((rows(yy)+1)*p)    
		qq=qq,yy[q,]
	}
    
	return(qq)
}
void mboot(real matrix rif,mean_rif, vv, cband, string scalar clv, real matrix vv1, 
			real scalar ci, reps, wbtype) {
    
    real matrix fr
	//real scalar reps, wbtype
	//reps   = 999
	//wbtype =   1
	//ci     = 0.95
	real matrix ifse , ccb
	// this gets the Bootstraped values
	if (clv ==" ") {
		fr=mboot_did(rif,mean_rif, reps, wbtype)
		ifse = iqrse(fr)
		// this gets Tvalue
		cband=( mean_rif',
				ifse',
				mean_rif':/ifse',
				mean_rif':-qtp(abs(fr :/ ifse),ci)':* ifse' ,  
				mean_rif':+qtp(abs(fr :/ ifse),ci)':* ifse'   )
	}
	else {
 		clvar=st_data(.,clv)
		fr=mboot_didc(rif,mean_rif, reps, wbtype, clvar)
		
		ifse = iqrse(fr)
		cband=( mean_rif',
				ifse',
				mean_rif':/ifse',
				mean_rif':-qtp(abs(fr :/ ifse),ci)':* ifse' ,  
				mean_rif':+qtp(abs(fr :/ ifse),ci)':* ifse'   )
	}
	vv=quadcross(ifse,ifse):*I(cols(ifse))
	vv1=quadcross(fr,fr)/rows(fr)
	//sqrt(variance(fr))
	//st_matrix(vv,iqrse(fr)^2)
	//st_matrix(cband,ccb)
	
}


end


