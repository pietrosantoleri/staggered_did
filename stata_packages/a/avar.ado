*! avar 1.0.07  28July2015
*! authors cfb/mes
*! shell based on ranktest by fk/mes
*! see end of file for version comments

if c(version) < 12 {
* avar uses livreg2 Mata library.
* Ensure Mata library is indexed if new install.
* Not needed for Stata 12+ since ssc.ado does this when installing.
	capture mata: mata drop m_calckw()
	capture mata: mata drop m_omega()
	capture mata: mata drop ms_vcvorthog()
	capture mata: mata drop s_vkernel()
	mata: mata mlib index
}

program define avar, rclass sortpreserve
	if _caller() < 11 {
		avar9 `0'
		return add
		exit
	}
	version 11.2
	local lversion 01.0.07

	if replay() {
		syntax [, VERsion]
		di in gr "`lversion'"
		return local version `lversion'
		exit	
	}

	if substr("`1'",1,1)== "," {
		if "`2'"=="version" {
			di in ye "`lversion'"
			return local version `lversion'
			exit
		}
		else {
di as err "invalid syntax"
			exit 198
		}
	}

* If varlist 1 or varlist 2 have a single element, parentheses optional

	if substr("`1'",1,1)=="(" {
		GetVarlist `0'
		local y `s(varlist)'
		local K : word count `y'
		local 0 `"`s(rest)'"'
		sret clear
	}
	else {
		local y `1'
		local K 1
		mac shift 1
		local 0 `"`*'"'
	}

	if substr("`1'",1,1)=="(" {
		GetVarlist `0'
		local z `s(varlist)'
		local L : word count `z'
		local 0 `"`s(rest)'"'
		sret clear
	}
	else {
		local z `1'
		local L 1
		mac shift 1
* Need to reinsert comma before options (if any) for -syntax- command to work
		local 0 `", `*'"'
	}

	syntax [if] [in] [aw fw pw iw/] [, partial(varlist ts) demean NOConstant	/*
		*/	ROBust cluster(varlist) BW(string) kernel(string) 					/*
		*/	kiefer dkraay(integer 0) center smata(name) 						/*
		*/	Tvar(varname) Ivar(varname) sw psd(string) dofminus(integer 0) ]

* Check partial list
	local partial_in_z : list partial & z
	if "`partial_in_z'" ~= "" {
di as err "error: variable(s) `partial_in_z' also appear in (varlist2)"
		exit 198
	}

* Check psd option
	if "`psd'"~="" & "`psd'"~="psda" & "`psd'"~="psd0" {
di as err "error: invalid psd option `psd'"
		exit 198
	}

* demean implies nocons
	if "`demean'"~="" {
		local noconstant "noconstant"
	}
* demean can't be combined with partial
	if "`demean'"~="" & "`partial'"~="" {
di as err "error: incompatible options - partial() and demean"
		exit 198
	}

* Note that by tsrevar-ing here, subsequent disruption to the sort doesn't matter
* for TS operators.
	tsrevar `y'
	local vl1 `r(varlist)'
	tsrevar `z'
	local vl2 `r(varlist)'
	tsrevar `partial'
	local partial `r(varlist)'

	marksample touse
	markout `touse' `vl1' `vl2' `partial' `cluster', strok

* Constant automatically added to varlist2
	if "`noconstant'"=="" {
		tempvar one
		gen byte `one' = 1
		if "`partial'"~="" {
* Constant added to partial-out list
			local partial "`partial' `one'"
		}
		else {
* Constant added to varlist2
			local vl2 "`vl2' `one'"
		}
	}

	foreach vn of varlist `vl1' {
		tempvar tv
		qui gen double `tv' = .
		local tempvl1 "`tempvl1' `tv'"
	}
	foreach vn of varlist `vl2' {
		tempvar tv
		qui gen double `tv' = .
		local tempvl2 "`tempvl2' `tv'"
	}

* Stock-Watson and cluster imply robust.
	if "`sw'`cluster'" ~= "" {
		local robust "robust"
	}

	tempvar wvar
	if "`weight'" == "fweight" | "`weight'"=="aweight" {
		local wtexp `"[`weight'=`exp']"'
		gen double `wvar'=`exp'
	}
	if "`fsqrt(wf)*(wvar^0.5):*'" == "fweight" & "`kernel'" !="" {
		di in red "fweights not allowed (data are -tsset-)"
		exit 101
	}
	if "`weight'" == "fweight" & "`sw'" != "" {
		di in red "fweights currently not supported with -sw- option"
		exit 101
	}
	if "`weight'" == "iweight" {
		if "`robust'`cluster'`bw'" !="" {
			di in red "iweights not allowed with robust, cluster, AC or HAC"
			exit 101
		}
		else {
			local wtexp `"[`weight'=`exp']"'
			gen double `wvar'=`exp'
		}
	}
	if "`weight'" == "pweight" {
		local wtexp `"[aweight=`exp']"'
		gen double `wvar'=`exp'
		local robust "robust"
	}
	if "`weight'" == "" {
* If no weights, define neutral weight variable
		qui gen byte `wvar'=1
	}


* Every time a weight is used, must multiply by scalar wf ("weight factor")
* wf=1 for no weights, fw and iw, wf = scalar that normalizes sum to be N if aw or pw
	sum `wvar' if `touse' `wtexp', meanonly
* Weight statement
	if "`weight'" ~= "" {
di in gr "(sum of wgt is " %14.4e `r(sum_w)' ")"
	}
	if "`weight'"=="" | "`weight'"=="fweight" | "`weight'"=="iweight" {
* If weight is "", weight var must be column of ones and N is number of rows.
* With fw and iw, effective number of observations is sum of weight variable.
		local wf=1
		local N=r(sum_w)
	}
	else if "`weight'"=="aweight" | "`weight'"=="pweight" {
* With aw and pw, N is number of obs, unadjusted.
		local wf=r(N)/r(sum_w)
		local N=r(N)
	}
	else {
* Should never reach here
di as err "avar error - misspecified weights"
		exit 198
	}

***********************************************************

* Need tvar for markout with time-series stuff
* Data must be tsset for time-series operators in code to work
* User-supplied tvar checked if consistent with tsset
	capture tsset
	if "`tvar'" == "" {
		local tvar "`r(timevar)'"
	}
	else if "`tvar'"!="`r(timevar)'" {
di as err "invalid tvar() option - data already -tsset-"
		exit 5
	}
* If no panel data, ivar will still be empty
	if "`ivar'" == "" {
		local ivar "`r(panelvar)'"
	}
	else if "`ivar'"!="`r(panelvar)'" {
di as err "invalid ivar() option - data already -tsset-"
		exit 5
	}
* tdelta missing if version 9 or if not tsset			
	if "`r(tdelta)'" != "" {
		local tdelta = `r(tdelta)'
	}
	else {
		local tdelta=1
	}

***********************************************************

* dkraay(bw) = clustering on time-series var in a panel + kernel-robust
* Default is zero
	if `dkraay' ~= 0 {
		if "`ivar'" == "" | "`tvar'" == "" {
di as err "invalid use of dkraay option - must use tsset panel data"
			exit 198
		}
		local bw "`dkraay'"
		if "`cluster'" == "" {
			local cluster "`tvar'"
		}
		else if "`cluster'" ~= "`tvar'" {
di as err "invalid use of dkraay option - must cluster on `tvar' (or omit cluster option)"
			exit 198
		}
	}

***********************************************************

* HAC estimation.
* If bw is omitted, default `bw' is empty string.
* If bw or kernel supplied, check/set `kernel'.
* Macro `kernel' is also used for indicating HAC in use.

* Check it's a valid kernel and replace with unabbreviated kernel name; check bw.
* Automatic kernel selection allowed by ivreg2 but not ranktest so must trap.
* s_vkernel is in livreg2 mlib.
	if "`bw'"=="auto" {
di as err "invalid bandwidth in option bw() - must be real > 0"
		exit 198
	}
	else if "`bw'`kernel'"~="" {
		mata: s_vkernel("`kernel'", "`bw'", "`ivar'")
		local kernel `r(kernel)'
		local bw = `r(bw)'
	}
	else {
		local bw = 0
	}

***********************************************************


* Set local macro T and check that bw < (T-1)
* Also make sure only used sample is checked
	if "`tvar'" ~= "" {
		sum `tvar' if `touse', meanonly
		local T = r(max)-r(min) + 1
		local T1 = `T' - 1
		if (`bw' > (`T1'/`tdelta')) & (`bw' ~= -1)  {
di as err "invalid bandwidth in option bw() - cannot exceed timespan of data"
			exit 198
		}
	}

***********************************************************

* kiefer VCV = kernel(tru) bw(T) and no robust with tsset data
	if "`kiefer'" ~= "" {
		if "`ivar'" == "" | "`tvar'" == "" {
di as err "invalid use of kiefer option - must use tsset panel data"
			exit 198
		}
		if	"`robust'" ~= "" {
di as err "incompatible options: kiefer and robust"
			exit 198
		}
		if	"`kernel'" ~= "" & "`kernel'" ~= "Truncated" {
di as err "incompatible options: kiefer and bw/kernel"
			exit 198
		}
		if	(`bw'~=0) & (`bw' ~= `T'/`tdelta') {
di as err "incompatible options: kiefer and bw"
			exit 198
		}
		local kernel "Truncated"
		local bw=`T'
	}

***********************************************************

* If kernel-robust, confirm tsset and check for gaps
		if `bw' != 0 {
* Data must be tsset for time-series operators in code to work
			capture tsset
			if "`r(timevar)'" == "" {
di as err "must tsset data and specify timevar"
				exit 5
			}
			tsreport if `touse', panel
			if `r(N_gaps)' != 0 {
di in gr "Warning: time variable " in ye "`tvar'" in gr " has " /*
*/ in ye "`r(N_gaps)'" in gr " gap(s) in relevant range"
			}
		}

***********************************************************

	if "`psd0'"~="" & "`psda'"~="" {
di as err "cannot use psd0 and psda options together"
		exit 198
	}
* Macro psd has either psd0, psda or is empty
	local psd "`psd0'`psda'"

	if "`sw'"~="" {
		capture xtset
		if "`ivar'" == "" {
			local ivar "`r(panelvar)'"
		}
		else if "`ivar'"!="`r(panelvar)'" {
di as err "invalid ivar() option - data already tsset or xtset"
			exit 5
		}
* Exit with error if ivar is neither supplied nor tsset nor xtset
		if "`ivar'"=="" {
di as err "Must -xtset- or -tsset- data or specify -ivar- with -sw- option"
			exit 198
		}
		qui describe, short varlist
		local sortlist "`r(sortlist)'"
		tokenize `sortlist'
		if "`ivar'"~="`1'" {
di as err "Error - dataset must be sorted on panel var with -sw- option"
			exit 198
		}
	}

***********************************************************

* Arrives as string, convert to boolean
	local center	="`center'"=="center"

***********************************************************

* Create variable used for getting lags etc. in Mata
	tempvar tindex
	qui gen `tindex'=1 if `touse'
	qui replace `tindex'=sum(`tindex') if `touse'

********** CLUSTER SETUP **********************************************

* Mata code requires data are sorted on (1) the first var cluster if there
* is only one cluster var; (2) on the 3rd and then 1st if two-way clustering,
* unless (3) two-way clustering is combined with kernel option, in which case
* the data are tsset and sorted on panel id (first cluster variable) and time
* id (second cluster variable).
* Second cluster var is optional and requires an identifier numbered 1..N_clust2,
* unless combined with kernel option, in which case it's the time variable.
* Third cluster var is the intersection of 1 and 2, unless combined with kernel
* opt, in which case it's unnecessary.
* Sorting on "cluster3 cluster1" means that in Mata, panelsetup works for
* both, since cluster1 nests cluster3.
* Note that it is possible to cluster on time but not panel, in which case
* cluster1 is time, cluster2 is empty and data are sorted on panel-time.
* Note also that if no kernel-robust, sorting will disrupt any tsset-ing,
* but data are tsrevar-ed earlier to avoid any problems.
	if "`cluster'"!="" {
		local clopt "cluster(`cluster')"
		tokenize `cluster'
		local cluster1 "`1'"
		local cluster2 "`2'"
		if "`kernel'"~="" {
* kernel requires either that cluster1 is time var and cluster2 is empty
* or that cluster1 is panel var and cluster2 is time var.
* Either way, data must be tsset and sorted for panel data.
			if "`cluster2'"~="" {
* Allow backwards order
				if "`cluster1'"=="`tvar'" & "`cluster2'"=="`ivar'" {
					local cluster1 "`2'"
					local cluster2 "`1'"
				}
				if "`cluster1'"~="`ivar'" | "`cluster2'"~="`tvar'" {
di as err "Error: cluster kernel-robust requires clustering on tsset panel & time vars."
di as err "       tsset panel var=`ivar'; tsset time var=`tvar'; cluster vars=`cluster1',`cluster2'"
					exit 198
				}
			}
			else {
				if "`cluster1'"~="`tvar'" {
di as err "Error: cluster kernel-robust requires clustering on tsset time variable."
di as err "       tsset time var=`tvar'; cluster var=`cluster1'"
					exit 198
				}
			}
		}
* Simple way to get quick count of 1st cluster variable without disrupting sort
* clusterid1 is numbered 1.._Nclust1.
		tempvar clusterid1
		qui egen `clusterid1'=group(`cluster1') if `touse'
		sum `clusterid1' if `touse', meanonly
		if "`cluster2'"=="" {
			local N_clust=r(max)
			local N_clust1=.
			local N_clust2=.
			if "`kernel'"=="" {
* Single level of clustering and no kernel-robust, so sort on single cluster var.
* kernel-robust already sorted via tsset.
				sort `cluster1'
			}
		}
		else {
			local N_clust1=r(max)
			if "`kernel'"=="" {
				tempvar clusterid2 clusterid3
* New cluster id vars are numbered 1..N_clust2 and 1..N_clust3
				qui egen `clusterid2'=group(`cluster2') if `touse'
				qui egen `clusterid3'=group(`cluster1' `cluster2') if `touse'
* Two levels of clustering and no kernel-robust, so sort on cluster3/nested in/cluster1
* kernel-robust already sorted via tsset.
				sort `clusterid3' `cluster1'
				sum `clusterid2' if `touse', meanonly
				local N_clust2=r(max)
			}
			else {
* Need to create this only to count the number of clusters
				tempvar clusterid2
				qui egen `clusterid2'=group(`cluster2') if `touse'
				sum `clusterid2' if `touse', meanonly
				local N_clust2=r(max)
* Now replace with original variable
				local clusterid2 `cluster2'
			}
			local N_clust=min(`N_clust1',`N_clust2')
		}
	}

************************************************************************************************

* Note that bw is passed as a value, not as a string
	mata: m_avar(	"`vl1'",			/*
				*/	"`vl2'",			/*
				*/	"`partial'",		/*
				*/	"`demean'",			/*
				*/	"`wvar'",			/*
				*/	"`weight'",			/*
				*/	`wf',				/*
				*/	`N',				/*
				*/	"`touse'",			/*
				*/	"`robust'",			/*
				*/	"`clusterid1'",		/*
				*/	"`clusterid2'",		/*
				*/	"`clusterid3'",		/*
				*/	`bw',				/*
				*/	"`tvar'",			/*
				*/	"`ivar'",			/*
				*/	"`tindex'",			/*
				*/	`tdelta',			/*
				*/	`center',			/*
				*/	`dofminus',			/*
				*/	"`kernel'",			/*
				*/	"`sw'",				/*
				*/	"`psd'",			/*
				*/	"`tempvl1'",		/*
				*/	"`tempvl2'",		/*
				*/	"`smata'")

	local N `r(N)'
	if "`cluster'"~="" {
		return scalar N_clust = `N_clust'
	}
	if "`cluster2'"~="" {
		return scalar N_clust1 = `N_clust1'
		return scalar N_clust2 = `N_clust2'
	}
	return scalar N = `N'

* Add _cons to list of varnames in z.
	if "`partial'"=="" & "`noconstant'"=="" {
		local z "`z' _cons"
	}

	if `K' > 1 {
		foreach en of local y {
* Remove "." from equation name
			local en1 : subinstr local en "." "_", all
			foreach vn of local z {
				local cn "`cn' `en1':`vn'"
			}
		}
	}
	else {
		foreach vn of local z {
			local cn "`cn' `vn'"
		}
	}
	tempname S
	mat `S'=r(S)
	matrix colnames `S' = `cn'
	matrix rownames `S' = `cn'
	di as result "(obs=`r(N)')"
	mat list `S', noheader
	return matrix S `S'
end

* Adopted from -canon-
program define GetVarlist, sclass 
	sret clear
	gettoken open 0 : 0, parse("(") 
	if `"`open'"' != "(" {
		error 198
	}
	gettoken next 0 : 0, parse(")")
	while `"`next'"' != ")" {
		if `"`next'"'=="" { 
			error 198
		}
		local list `list'`next'
		gettoken next 0 : 0, parse(")")
	}
	sret local rest `"`0'"'
	tokenize `list'
	local 0 `*'
	sret local varlist "`0'"
end


*******************************************************************************
*************************** BEGIN MATA CODE ***********************************
*******************************************************************************

// capture required in case called by ver 9 or 10
// (must load before forking to avar9)
cap version 11.2
mata:

// ********* MATA CODE SHARED BY ivreg2 AND ranktest       *************** //
// ********* 1. struct ms_vcvorthog                        *************** //
// ********* 2. m_omega                                    *************** //
// ********* 3. m_calckw                                   *************** //
// ********* 4. s_vkernel                                  *************** //
// *********************************************************************** //

// For reference:
// struct ms_vcvorthog {
// 	string scalar	ename, Znames, touse, weight, wvarname
// 	string scalar	robust, clustvarname, clustvarname2, clustvarname3, kernel
// 	string scalar	sw, psd, ivarname, tvarname, tindexname
// 	real scalar		wf, N, bw, tdelta, dofminus
//  real scalar		center
// 	real matrix		ZZ
// 	pointer matrix	e
// 	pointer matrix	Z
// 	pointer matrix	wvar
// }

void m_avar(	string scalar vl1,
				string scalar vl2,
				string scalar partial,
				string scalar demean,
				string scalar wvarname,
				string scalar weight,
				scalar wf,
				scalar N,
				string scalar touse,
				string scalar robust,
				string scalar clustvarname,
				string scalar clustvarname2,
				string scalar clustvarname3,
				scalar bw,
				string scalar tvarname,
				string scalar ivarname,
				string scalar tindexname,
				scalar tdelta,
				scalar center,
				scalar dofminus,
				string scalar kernel,
				string scalar sw,
				string scalar psd,
				string scalar tempvl1,
				string scalar tempvl2,
				string scalar Sname)
{

	if (Sname~="") {
		rmexternal(Sname)
		p = crexternal(Sname)		// pointer to global where S will be left in Mata if requested
	}
// tempx, tempy and tempz are the Stata names of temporary variables that will be changed
	if (partial~="") {
		tempx=tokens(partial)
	}
	tempy=tokens(tempvl1)
	tempz=tokens(tempvl2)

	st_view(y=.,.,tokens(vl1),touse)
	st_view(z=.,.,tokens(vl2),touse)
	st_view(yhat=.,.,tempy,touse)
	st_view(zhat=.,.,tempz,touse)
	st_view(mtouse=.,.,tokens(touse),touse)
	st_view(wvar=.,.,tokens(wvarname),touse)
	noweight=(st_vartype(wvarname)=="byte")

// Note that we now use wf*wvar instead of wvar
// because wvar is raw weighting variable and
// wf*wvar normalizes so that sum(wf*wvar)=N.

// Set up Y and Z variables
		yhat[.,.] = y
		zhat[.,.] = z

// Demean Y variables if requested
	if (demean~="") {
		yhat[.,.] = yhat :- mean(yhat, wf*wvar)
		zhat[.,.] = zhat :- mean(zhat, wf*wvar)
	}

// Partial out the X variables
// Note that this is entered if there is a constant,
// i.e., the X variables are centered
	if (partial~="") {
		st_view(x=.,.,tempx,touse)
		xx = quadcross(x, wf*wvar, x)
		xy = quadcross(x, wf*wvar, yhat)
		xz = quadcross(x, wf*wvar, zhat)

		by = invsym(xx)*xy
		bz = invsym(xx)*xz

		yhat[.,.] = yhat-x*by
		zhat[.,.] = zhat-x*bz
	}

	K=cols(y)
	L=cols(z)

	zhzh = quadcross(zhat, wf*wvar, zhat)

// ************************************************************************************* //
// shat calculated using struct and programs m_omega, m_calckw shared with ivreg2        //

	struct ms_vcvorthog scalar vcvo


	vcvo.ename			= tempy		// ivreg2 has = ename //
	vcvo.Znames			= tempz		// ivreg2 has = Znames //
	vcvo.touse			= touse
	vcvo.weight			= weight
	vcvo.wvarname		= wvarname
	vcvo.robust			= robust
	vcvo.clustvarname	= clustvarname
	vcvo.clustvarname2	= clustvarname2
	vcvo.clustvarname3	= clustvarname3
	vcvo.kernel			= kernel
	vcvo.sw				= sw
	vcvo.psd			= psd
	vcvo.ivarname		= ivarname
	vcvo.tvarname		= tvarname
	vcvo.tindexname		= tindexname
	vcvo.wf				= wf
	vcvo.N				= N
	vcvo.bw				= bw
	vcvo.tdelta			= tdelta
	vcvo.center			= center
	vcvo.dofminus		= dofminus
	vcvo.ZZ				= zhzh		// ivreg2 has = st_matrix(ZZmatrix) //
	
	vcvo.e		= &yhat				// ivreg2 has = &e	//
	vcvo.Z		= &zhat				// ivreg2 has = &Z //
	vcvo.wvar	= &wvar

	shat=m_omega(vcvo)
	if (Sname~="") {
		*p = shat		// save S in global if requested
	}
	st_matrix("r(S)", shat)
	st_numscalar("r(N)", N)
	if (clustvarname~="") {
		st_numscalar("r(N_clust)", N_clust)
	}
	if (clustvarname2~="") {
		st_numscalar("r(N_clust2)", N_clust2)
	}
// end of program
}


end

* Version notes
* 1.0.00  Shell of program based on ranktest 1.3.02
* 1.0.01  Added support for dkraay, keifer(.) options
* 1.0.02  Added option for leaving S behind in Mata
* 1.0.03  29Aug13. Now handles case where y is anything, not necessarily residuals
* 1.0.04  01Sep13. Revised psd option to be in line with actest syntax
* 1.0.05  01Jan14. Fixed reporting bug with 2-way clustering and kernel-robust that
*         would give wrong count for 2nd cluster variable.
* 1.0.06  22Jan15. Promotion to version 11; forks to avar9 if version<=10; requires
*         capture before "version 11.2" in Mata section since must load before forking.
* 1.0.07  27July15. Added support for -center- (centered moments).
