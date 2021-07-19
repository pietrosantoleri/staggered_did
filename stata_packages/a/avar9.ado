*! avar9 1.0.06 22Jan2015
*! based on avar 1.0.05 and livreg2 Mata library 1.1.07
*! authors cfb/mes
*! based on ranktest by fk/mes
*! see end of file for version comments

program define avar9, rclass sortpreserve
	version 9.2
	local lversion 01.0.06

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
		*/	kiefer dkraay(integer 0) smata(name) 								/*
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
* Automatic kernel selection allowed by ivreg2 but not here so must trap.

	if "`bw'"=="auto" {
di as err "invalid bandwidth in option bw() - must be real > 0"
		exit 198
	}
	else if "`bw'`kernel'"~="" {
		mata: s_avar9_vkernel("`kernel'", "`bw'", "`ivar'")
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
	mata: m_avar9(	"`vl1'",			/*
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

version 9.2
mata:


// ********* struct ms_avar9_vcvorthog ******************* //
struct ms_avar9_vcvorthog {
	string scalar	ename, Znames, touse, weight, wvarname
	string scalar	robust, clustvarname, clustvarname2, clustvarname3, kernel
	string scalar	sw, psd, ivarname, tvarname, tindexname
	real scalar		wf, N, bw, tdelta, dofminus
	real matrix		ZZ
	pointer matrix	e
	pointer matrix	Z
	pointer matrix	wvar
}

void m_avar9(	string scalar vl1,
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
				bw,
				string scalar tvarname,
				string scalar ivarname,
				string scalar tindexname,
				tdelta,
				dofminus,
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

	struct ms_avar9_vcvorthog scalar vcvo


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
	vcvo.dofminus		= dofminus
	vcvo.ZZ				= zhzh		// ivreg2 has = st_matrix(ZZmatrix) //
	
	vcvo.e		= &yhat				// ivreg2 has = &e	//
	vcvo.Z		= &zhat				// ivreg2 has = &Z //
	vcvo.wvar	= &wvar

	shat=m_avar9_omega(vcvo)
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

// *********************************************************************** //
// **************** SUPPORT CODE (prev in livreg2.mlib ******************* //
// *********************************************************************** //


// ************************* s_avar9_vkernel ***************************** //
// Program checks whether kernel and bw choices are valid.
// s_avar9_vkernel is called from Stata.
// Arguments are the kernel name (req), bandwidth (req) and ivar name (opt).
// All 3 are strings.
// Returns results in r() macros.
// r(kernel) - name of kernel (string)
// r(bw) - bandwidth (scalar)

void s_avar9_vkernel(	string scalar kernel,
						string scalar bwstring,
						string scalar ivar
				)
{

// Check bandwidth
	if (bwstring=="auto") {
		bw=-1
	}
	else {
		bw=strtoreal(bwstring)
		if (bw==.) {
			printf("{err}bandwidth option bw() required for HAC-robust estimation\n")
			exit(102)
		}
		if (bw<=0) {
			printf("{err}invalid bandwidth in option bw() - must be real > 0\n")
			exit(198)
		}
	}
	
// Check ivar
	if (bwstring=="auto" & ivar~="") {
			printf("{err}Automatic bandwidth selection not available for panel data\n")
			exit(198)
	}

// Check kernel
// Valid kernel list is abbrev, full name, whether special case if bw=1
// First in list is default kernel = Barlett
	vklist = 	(	("", "bartlett", "0")
				\	("bar", "bartlett", "0")
				\	("bartlett", "bartlett", "0")
				\	("par", "parzen", "0")
				\	("parzen", "parzen", "0")
				\	("tru", "truncated", "1")
				\	("truncated", "truncated", "1")
				\	("thann", "tukey-hanning", "0")
				\	("tukey-hanning", "tukey-hanning", "0")
				\	("thamm", "tukey-hamming", "0")
				\	("tukey-hamming", "tukey-hamming", "0")
				\	("qua", "quadratic spectral", "1")
				\	("qs", "quadratic spectral", "1")
				\	("quadratic-spectral", "quadratic spectral", "1")
				\	("quadratic spectral", "quadratic spectral", "1")
				\	("dan", "danielle", "1")
				\	("danielle", "danielle", "1")
				\	("ten", "tent", "1")
				\	("tent", "tent", "1")
			)
	kname=strltrim(strlower(kernel))
	pos = (vklist[.,1] :== kname)

// Exit with error if not in list
	if (sum(pos)==0) {
		printf("{err}invalid kernel\n")
		exit(198)
		}

	vkname=strproper(select(vklist[.,2],pos))
	st_global("r(kernel)", vkname)
	st_numscalar("r(bw)",bw)

// Warn if kernel is type where bw=1 means no lags are used
	if (bw==1 & select(vklist[.,3],pos)=="0") {
		printf("{result}Note: kernel=%s", vkname)
		printf("{result} and bw=1 implies zero lags used.  Standard errors and\n")
		printf("{result}      test statistics are not autocorrelation-consistent.\n")
	}
}  // end of program s_avar9_vkernel


// ************************ m_avar9_omega ************************************** //

real matrix m_avar9_omega(struct ms_avar9_vcvorthog scalar vcvo) 
{
	if (vcvo.clustvarname~="") {
		st_view(clustvar, ., vcvo.clustvarname, vcvo.touse)
		info = panelsetup(clustvar, 1)
		N_clust=rows(info)
		if (vcvo.clustvarname2~="") {
			st_view(clustvar2, ., vcvo.clustvarname2, vcvo.touse)
			if (vcvo.kernel=="") {
				st_view(clustvar3, ., vcvo.clustvarname3, vcvo.touse) // needed only if not panel tsset
			}
		}
	}

	if (vcvo.kernel~="") {
		st_view(t,    ., st_tsrevar(vcvo.tvarname),  vcvo.touse)
		T=max(t)-min(t)+1
	}

	if ((vcvo.kernel=="Bartlett") | (vcvo.kernel=="Parzen") | (vcvo.kernel=="Truncated") ///
		 | (vcvo.kernel=="Tukey-Hanning")| (vcvo.kernel=="Tukey-Hamming")) {
		window="lag"
	}
	else if ((vcvo.kernel=="Quadratic Spectral") | (vcvo.kernel=="Danielle") | (vcvo.kernel=="Tent")) {
		window="spectral"
	}
	else if (vcvo.kernel~="") {
// Should never reach this point
printf("\n{error:Error: invalid kernel}\n")
		exit(error(3351))
	}

	L=cols(*vcvo.Z)
	K=cols(*vcvo.e)		// ivreg2 always calls with K=1; ranktest may call with K>=1.

// Covariance matrices
// shat * 1/N is same as estimated S matrix of orthog conditions

// Block for homoskedastic and AC.  dof correction if any incorporated into sigma estimates.
	if ((vcvo.robust=="") & (vcvo.clustvarname=="")) {
// ZZ is already calculated as an external
		ee = quadcross(*vcvo.e, vcvo.wf*(*vcvo.wvar), *vcvo.e)
		sigma2=ee/(vcvo.N-vcvo.dofminus)
		shat=sigma2#vcvo.ZZ
		if (vcvo.kernel~="") {
			if (window=="spectral") {
				TAU=T/vcvo.tdelta-1
			}
			else {
				TAU=vcvo.bw
			}
			tnow=st_data(., vcvo.tindexname)
			for (tau=1; tau<=TAU; tau++) {
				kw = m_avar9_calckw(tau, vcvo.bw, vcvo.kernel)
				if (kw~=0) {						// zero weight possible with some kernels
													// save an unnecessary loop if kw=0
													// remember, kw<0 possible with some kernels!
					lstau = "L"+strofreal(tau)
					tlag=st_data(., lstau+"."+vcvo.tindexname)
					tmatrix = tnow, tlag
					svar=(tnow:<.):*(tlag:<.)		// multiply column vectors of 1s and 0s
					tmatrix=select(tmatrix,svar)	// to get intersection, and replace tmatrix
// if no lags exist, tmatrix has zero rows.
					if (rows(tmatrix)>0) {
// col 1 of tmatrix has row numbers of all rows of data with this time period that have a corresponding lag
// col 2 of tmatrix has row numbers of all rows of data with lag tau that have a corresponding ob this time period.
// Should never happen that fweights or iweights make it here,
// but if they did the next line would be sqrt(wvari)*sqrt(wvari1) [with no wf since not needed for fw or iw]
						wv = (*vcvo.wvar)[tmatrix[.,1]]		///
									:* (*vcvo.wvar)[tmatrix[.,2]]*(vcvo.wf^2)	// inner weighting matrix for quadcross
						sigmahat = quadcross((*vcvo.e)[tmatrix[.,1],.],   wv ,(*vcvo.e)[tmatrix[.,2],.])	///
									/ (vcvo.N-vcvo.dofminus)					// large dof correction
						ZZhat    = quadcross((*vcvo.Z)[tmatrix[.,1],.], wv, (*vcvo.Z)[tmatrix[.,2],.])
						ghat = sigmahat#ZZhat
						shat=shat+kw*(ghat+ghat')
					}
				}	// end non-zero kernel weight block
			}	// end tau loop
		}  // end kernel code
// Note large dof correction (if there is one) has already been incorporated
	shat=shat/vcvo.N
	}  // end homoskedastic, AC code

// Block for robust HC and HAC but not Stock-Watson and single clustering.
// Need to enter for double-clustering if one cluster is time.
	if ( (vcvo.robust~="") & (vcvo.sw=="") & ((vcvo.clustvarname=="")		///
			| ((vcvo.clustvarname2~="") & (vcvo.kernel~="")))  ) {
		if (K==1) {										// simple/fast where e is a column vector
			if ((vcvo.weight=="fweight") | (vcvo.weight=="iweight")) {
				wv = (*vcvo.e:^2) :* *vcvo.wvar
			}
			else {
				wv = (*vcvo.e :* *vcvo.wvar * vcvo.wf):^2		// wf needed for aweights and pweights
			}
			shat=quadcross(*vcvo.Z, wv, *vcvo.Z)		// basic Eicker-Huber-White-sandwich-robust vce
		}
		else {											// e is a matrix so must loop
			shat=J(L*K,L*K,0)
			for (i=1; i<=rows(*vcvo.e); i++) {
				eZi=((*vcvo.e)[i,.])#((*vcvo.Z)[i,.])
				if ((vcvo.weight=="fweight") | (vcvo.weight=="iweight")) {
// wvar is a column vector. wf not needed for fw and iw (=1 by dfn so redundant).
					shat=shat+quadcross(eZi,eZi)*((*vcvo.wvar)[i])
				}
				else {
					shat=shat+quadcross(eZi,eZi)*((*vcvo.wvar)[i] * vcvo.wf)^2	//  **** ADDED *vcvo.wf
				}
			}
		}
		if (vcvo.kernel~="") {
// Spectral windows require looping through all T-1 autocovariances
			if (window=="spectral") {
				TAU=T/vcvo.tdelta-1
			}
			else {
				TAU=vcvo.bw
			}
			tnow=st_data(., vcvo.tindexname)
			for (tau=1; tau<=TAU; tau++) {
				kw = m_avar9_calckw(tau, vcvo.bw, vcvo.kernel)
				if (kw~=0) {						// zero weight possible with some kernels
													// save an unnecessary loop if kw=0
													// remember, kw<0 possible with some kernels!
					lstau = "L"+strofreal(tau)
					tlag=st_data(., lstau+"."+vcvo.tindexname)
					tmatrix = tnow, tlag
					svar=(tnow:<.):*(tlag:<.)		// multiply column vectors of 1s and 0s
					tmatrix=select(tmatrix,svar)	// to get intersection, and replace tmatrix

// col 1 of tmatrix has row numbers of all rows of data with this time period that have a corresponding lag
// col 2 of tmatrix has row numbers of all rows of data with lag tau that have a corresponding ob this time period.
// if no lags exist, tmatrix has zero rows
					if (rows(tmatrix)>0) {
						if (K==1) {										// simple/fast where e is a column vector
// wv is inner weighting matrix for quadcross
							wv   = (*vcvo.e)[tmatrix[.,1]] :* (*vcvo.e)[tmatrix[.,2]]		///
								:* (*vcvo.wvar)[tmatrix[.,1]] :* (*vcvo.wvar)[tmatrix[.,2]] * (vcvo.wf^2)
							ghat = quadcross((*vcvo.Z)[tmatrix[.,1],.], wv, (*vcvo.Z)[tmatrix[.,2],.])
						}
						else {										// e is a matrix so must loop
							ghat=J(L*K,L*K,0)
							for (i=1; i<=rows(tmatrix); i++) {
								wvari =(*vcvo.wvar)[tmatrix[i,1]]
								wvari1=(*vcvo.wvar)[tmatrix[i,2]]
								ei    =(*vcvo.e)[tmatrix[i,1],.]
								ei1   =(*vcvo.e)[tmatrix[i,2],.]
								Zi    =(*vcvo.Z)[tmatrix[i,1],.]
								Zi1   =(*vcvo.Z)[tmatrix[i,2],.]
								eZi =ei#Zi
								eZi1=ei1#Zi1
// Should never happen that fweights or iweights make it here, but if they did
// the next line would be ghat=ghat+eZi'*eZi1*sqrt(wvari)*sqrt(wvari1)
// [without *vcvo.wf since wf=1 for fw and iw]
								ghat=ghat+quadcross(eZi,eZi1)*wvari*wvari1 * (vcvo.wf^2)	// ADDED * (vcvo.wf^2)
							}
						}
						shat=shat+kw*(ghat+ghat')
					}	// end non-zero-obs accumulation block
				}	// end non-zero kernel weight block
			}	// end tau loop
		}  // end kernel code
// Incorporate large dof correction if there is one
	shat=shat/(vcvo.N-vcvo.dofminus)
	}  // end HC/HAC code

	if (vcvo.clustvarname~="") {
// Block for cluster-robust
// 2-level clustering: S = S(level 1) + S(level 2) - S(level 3 = intersection of levels 1 & 2)
// Prepare shat3 if 2-level clustering
		if (vcvo.clustvarname2~="") {
			if (vcvo.kernel~="") {	// second cluster variable is time
									// shat3 was already calculated above as shat
				shat3=shat*(vcvo.N-vcvo.dofminus)
			}
			else {					// calculate shat3
									// data were sorted on clustvar3-clustvar1 so
									// clustvar3 is nested in clustvar1 and Mata panel functions
									// work for both.
				info3 = panelsetup(clustvar3, 1)
				if (rows(info3)==rows(*vcvo.e)) {	// intersection of levels 1 & 2 are all single obs
													// so no need to loop through row by row
					if (K==1) {										// simple/fast where e is a column vector
						wv = (*vcvo.e :* *vcvo.wvar * vcvo.wf):^2
						shat3=quadcross(*vcvo.Z, wv, *vcvo.Z)		// basic Eicker-Huber-White-sandwich-robust vce
					}
					else {											// e is a matrix so must loop
						shat3=J(L*K,L*K,0)
						for (i=1; i<=rows(*vcvo.e); i++) {
							eZi=((*vcvo.e)[i,.])#((*vcvo.Z)[i,.])
							shat3=shat3+quadcross(eZi,eZi)*((*vcvo.wvar)[i] * vcvo.wf)^2	//  **** ADDED *vcvo.wf
							}
						}
				}
				else {								// intersection of levels 1 & 2 includes some groups of obs
					N_clust3=rows(info3)
					shat3=J(L*K,L*K,0)
					for (i=1; i<=N_clust3; i++) {
						esub=panelsubmatrix(*vcvo.e,i,info3)
						Zsub=panelsubmatrix(*vcvo.Z,i,info3)
						wsub=panelsubmatrix(*vcvo.wvar,i,info3)
						wv = esub :* wsub * vcvo.wf
						if (K==1) {							// simple/fast where e is a column vector
							eZ = quadcross(1, wv, Zsub)		// equivalent to colsum(wv :* Zsub)
						}
						else {
							eZ = J(1,L*K,0)
							for (j=1; j<=rows(esub); j++) {
								eZ=eZ+(esub[j,.]#Zsub[j,.])*wsub[j,.] * vcvo.wf	//  **** ADDED *vcvo.wf
							}
						}
						shat3=shat3+quadcross(eZ,eZ)
					}
				}
			}
		}

// 1st level of clustering, no kernel-robust
// Entered unless 1-level clustering and kernel-robust
		if (!((vcvo.kernel~="") & (vcvo.clustvarname2==""))) {
			shat=J(L*K,L*K,0)
			for (i=1; i<=N_clust; i++) {		// loop through clusters, adding Z'ee'Z
												// for indiv cluster in each loop
				esub=panelsubmatrix(*vcvo.e,i,info)
				Zsub=panelsubmatrix(*vcvo.Z,i,info)
				wsub=panelsubmatrix(*vcvo.wvar,i,info)
				if (K==1) {						// simple/fast if e is a column vector
					wv = esub :* wsub * vcvo.wf
					eZ = quadcross(1, wv, Zsub)		// equivalent to colsum(wv :* Zsub)
				}
				else {
					eZ=J(1,L*K,0)
					for (j=1; j<=rows(esub); j++) {
						eZ=eZ+(esub[j,.]#Zsub[j,.])*wsub[j,.]*vcvo.wf	//  **** ADDED *vcvo.wf
					}
				}
				shat=shat+quadcross(eZ,eZ)
			}	// end loop through clusters
		}

// 2-level clustering, no kernel-robust
		if ((vcvo.clustvarname2~="") & (vcvo.kernel=="")) {
			imax=max(clustvar2)					// clustvar2 is numbered 1..N_clust2
			shat2=J(L*K,L*K,0)
			for (i=1; i<=imax; i++) {			// loop through clusters, adding Z'ee'Z
												// for indiv cluster in each loop
				svar=(clustvar2:==i)			// mimics panelsubmatrix but doesn't require sorted data
				esub=select(*vcvo.e,svar)		// it is, however, noticably slower.
				Zsub=select(*vcvo.Z,svar)
				wsub=select(*vcvo.wvar,svar)
				if (K==1) {						// simple/fast if e is a column vector
					wv = esub :* wsub * vcvo.wf
					eZ = quadcross(1, wv, Zsub)		// equivalent to colsum(wv :* Zsub)
				}
				else {
					eZ=J(1,L*K,0)
					for (j=1; j<=rows(esub); j++) {
						eZ=eZ+(esub[j,.]#Zsub[j,.])*wsub[j,.]*vcvo.wf	//  **** ADDED *vcvo.wf
					}
				}
				shat2=shat2+quadcross(eZ,eZ)
			}
		}

// 1st level of cluster, kernel-robust OR
// 2-level clustering, kernel-robust and time is 2nd cluster variable
		if (vcvo.kernel~="") {
			shat2=J(L*K,L*K,0)
// First, standard cluster-robust, i.e., no lags.
			i=min(t)
			while (i<=max(t)) {  				// loop through all T clusters, adding Z'ee'Z
												// for indiv cluster in each loop
				eZ=J(1,L*K,0)
				svar=(t:==i)					// select obs with t=i
				if (colsum(svar)>0) {			// there are obs with t=i
					esub=select(*vcvo.e,svar)
					Zsub=select(*vcvo.Z,svar)
					wsub=select(*vcvo.wvar,svar)
					if (K==1) {						// simple/fast if e is a column vector
						wv = esub :* wsub * vcvo.wf
						eZ = quadcross(1, wv, Zsub)		// equivalent to colsum(wv :* Zsub)
					}
					else {
// MISSING LINE IS NEXT
						eZ=J(1,L*K,0)
						for (j=1; j<=rows(esub); j++) {
							eZ=eZ+(esub[j,.]#Zsub[j,.])*wsub[j,.]*vcvo.wf	//  **** ADDED *vcvo.wf
						}
					}
					shat2=shat2+quadcross(eZ,eZ)
				}
				i=i+vcvo.tdelta
			} // end i loop through all T clusters

// Spectral windows require looping through all T-1 autocovariances
			if (window=="spectral") {
				TAU=T/vcvo.tdelta-1
			}
			else {
				TAU=vcvo.bw
			}

			for (tau=1; tau<=TAU; tau++) {
				kw = m_avar9_calckw(tau, vcvo.bw, vcvo.kernel)	// zero weight possible with some kernels
															// save an unnecessary loop if kw=0
															// remember, kw<0 possible with some kernels!
				if (kw~=0) {
					i=min(t)+tau*vcvo.tdelta				// Loop through all possible ts (time clusters)
					while (i<=max(t)) {						// Start at earliest possible t
						svar=t[.,]:==i						// svar is current, svar1 is tau-th lag
						svar1=t[.,]:==(i-tau*vcvo.tdelta)	// tau*vcvo.tdelta is usually just tau
						if ((colsum(svar)>0)				// there are current & lagged obs
								& (colsum(svar1)>0))	 {
							wv  = select((*vcvo.e),svar)  :* select((*vcvo.wvar),svar)  * vcvo.wf
							wv1 = select((*vcvo.e),svar1) :* select((*vcvo.wvar),svar1) * vcvo.wf
							Zsub =select((*vcvo.Z),svar)
							Zsub1=select((*vcvo.Z),svar1)
							if (K==1) {						// simple/fast, e is column vector
								eZsub = quadcross(1, wv, Zsub)		// equivalent to colsum(wv :* Zsub)
								eZsub1= quadcross(1, wv1, Zsub1)	// equivalent to colsum(wv :* Zsub)
							}
							else {
								eZsub=J(1,L*K,0)
								for (j=1; j<=rows(Zsub); j++) {
									wvj =wv[j,.]
									Zj  =Zsub[j,.]
									eZsub=eZsub+(wvj#Zj)
								}
								eZsub1=J(1,L*K,0)
								for (j=1; j<=rows(Zsub1); j++) {
									wv1j =wv1[j,.]
									Z1j  =Zsub1[j,.]
									eZsub1=eZsub1+(wv1j#Z1j)
								}
							}
							ghat=quadcross(eZsub,eZsub1)
							shat2=shat2+kw*(ghat+ghat')
						}
						i=i+vcvo.tdelta
					}
				}	// end non-zero kernel weight block
			}	// end tau loop

// If 1-level clustering, shat2 just calculated above is actually the desired shat
			if (vcvo.clustvarname2=="") {
				shat=shat2
			}
		}

// 2-level clustering, completion
// Cameron-Gelbach-Miller/Thompson method:
// Add 2 cluster variance matrices and subtract 3rd
		if (vcvo.clustvarname2~="") {
			shat = shat+shat2-shat3
		}		

// Note no dof correction required for cluster-robust
	shat=shat/vcvo.N
	} // end cluster-robust code

	if (vcvo.sw~="") {
// Stock-Watson adjustment.  Calculate Bhat in their equation (6).  Also need T=panel length.
// They define for balanced panels.  Since T is not constant for unbalanced panels, need
// to incorporate panel-varying 1/T, 1/(T-1) and 1/(T-2) as weights in summation.

		st_view(ivar, ., st_tsrevar(vcvo.ivarname), vcvo.touse)
		info_ivar = panelsetup(ivar, 1)

		shat=J(L*K,L*K,0)
		bhat=J(L*K,L*K,0)
		N_panels=0
		for (i=1; i<=rows(info_ivar); i++) {
			esub=panelsubmatrix(*vcvo.e,i,info_ivar)
			Zsub=panelsubmatrix(*vcvo.Z,i,info_ivar)
			wsub=panelsubmatrix(*vcvo.wvar,i,info_ivar)
			Tsub=rows(esub)
			if (Tsub>2) {			// SW cov estimator defined only for T>2
				N_panels=N_panels+1
				sigmahatsub=J(K,K,0)
				ZZsub=J(L*K,L*K,0)
				shatsub=J(L*K,L*K,0)
				for (j=1; j<=rows(esub); j++) {
					eZi=esub[j,1]#Zsub[j,.]
					if ((vcvo.weight=="fweight") | (vcvo.weight=="iweight")) {
						shatsub=shatsub+quadcross(eZi,eZi)*wsub[j]*vcvo.wf
						sigmahatsub=sigmahatsub + quadcross(esub[j,1],esub[j,1])*wsub[j]*vcvo.wf
						ZZsub=ZZsub+quadcross(Zsub[j,.],Zsub[j,.])*wsub[j]*vcvo.wf
					}
					else {
						shatsub=shatsub+quadcross(eZi,eZi)*((wsub[j]*vcvo.wf)^2)
						sigmahatsub=sigmahatsub + quadcross(esub[j,1],esub[j,1])*((wsub[j]*vcvo.wf)^2)
						ZZsub=ZZsub+quadcross(Zsub[j,.],Zsub[j,.])*((wsub[j]*vcvo.wf)^2)
					}
				} // end loop through j obs of panel i
				shat=shat + shatsub*(Tsub-1)/(Tsub-2)
				bhat=bhat + ZZsub/Tsub#sigmahatsub/(Tsub-1)/(Tsub-2)
			}
		} // end loop through i panels

// Note that Stock-Watson incorporate an N-n-k degrees of freedom correction in their eqn 4
// for what we call shat.  We use only an N-n degrees of freedom correction, i.e., we ignore
// the k regressors.  This is because this is an estimate of S, the VCV of orthogonality conditions,
// independently of its use to obtain an estimate of the variance of beta.  Makes no diff aysmptotically.
// Ignore dofminus correction since this is explicitly handled here.
// Use number of valid panels in denominator (SW cov estimator defined only for panels with T>2).
		shat=shat/(vcvo.N-N_panels)
		bhat=bhat/N_panels
		shat=shat-bhat
	} // end Stock-Watson block

	_makesymmetric(shat)

// shat may not be positive-definite.  Use spectral decomposition to obtain an invertable version.
// Extract Eigenvector and Eigenvalues, replace EVs, and reassemble shat.
// psda option: Stock-Watson 2008 Econometrica, Remark 8, say replace neg EVs with abs(EVs).
// psd0 option: Politis (2007) says replace neg EVs with zeros.
	if (vcvo.psd~="") {
		symeigensystem(shat,Evec,Eval)
		if (vcvo.psd=="psda") {
			Eval = abs(Eval)
		}
		else {
			Eval = Eval + (abs(Eval) - Eval)/2
		}
		shat = Evec*diag(Eval)*Evec'
		_makesymmetric(shat)
	}

	return(shat)

} // end of program m_avar9_omega

// *********************************************************************** //
// *********************************************************************** //

real scalar m_avar9_calckw(	real scalar tau,
							real scalar bw,
							string scalar kernel) 
	{
				karg = tau / bw
				if (kernel=="Truncated") {
					kw=1
				}
				if (kernel=="Bartlett") {
					kw=(1-karg)
				}
				if (kernel=="Parzen") {
					if (karg <= 0.5) {
						kw = 1-6*karg^2+6*karg^3
					}
					else {
						kw = 2*(1-karg)^3
					}
				}
				if (kernel=="Tukey-Hanning") {
					kw=0.5+0.5*cos(pi()*karg)
				}
				if (kernel=="Tukey-Hamming") {
					kw=0.54+0.46*cos(pi()*karg)
				}
				if (kernel=="Tent") {
					kw=2*(1-cos(tau*karg)) / (karg^2)
				}
				if (kernel=="Danielle") {
					kw=sin(pi()*karg) / (pi()*karg)
				}
				if (kernel=="Quadratic Spectral") {
					kw=25/(12*pi()^2*karg^2) /*
						*/ * ( sin(6*pi()*karg/5)/(6*pi()*karg/5) /*
						*/     - cos(6*pi()*karg/5) )
				}
				return(kw)
	}  // end kw

// *********************************************************************** //
// *********************************************************************** //

end

* Version notes
* 1.0.00  Shell of program based on ranktest 1.3.02
* 1.0.01  Added support for dkraay, keifer(.) options
* 1.0.02  Added option for leaving S behind in Mata
* 1.0.03  29Aug13. Now handles case where y is anything, not necessarily residuals
* 1.0.04  01Sep13. Revised psd option to be in line with actest syntax
* 1.0.05  01Jan14. Fixed reporting bug with 2-way clustering and kernel-robust that
*         would give wrong count for 2nd cluster variable.
* 1.0.06  22Jan15. First version of avar9. Mata library now internal with names incorporating "_avar9_".

* Version notes for imported version of Mata library
* 1.1.01     First version of library.
*            Contains struct ms_vcvorthog, m_omega, m_calckw, s_vkernel.
*            Compiled in Stata 9.2 for compatibility with ranktest 1.3.01 (a 9.2 program).
* 1.1.02     Add routine cdsy. Standardized spelling/caps/etc. of QS as "Quadratic Spectral"
* 1.1.03     Corrected spelling of "Danielle" kernel in m_omega()
* 1.1.04     Fixed weighting bugs in robust and cluster code of m_omega where K>1
* 1.1.05     Added whichlivreg2(.) to aid in version control
* 1.1.06     Fixed remaining weighting bug (see 1.1.04) in 2-way clustering when interection
*            of clustering levels is groups
* 1.1.07     Fixed HAC bug that crashed m_omega(.) when there were no obs for a particular lag
