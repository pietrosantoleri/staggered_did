*! v1 Command for estimating estats from RIF file
program adde, eclass
        ereturn `0'
end

program addr, rclass
        return `0'
end

program adds, rclass
        return `0'
end

program csdid_stats,  
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
 
		
		csdid_est `0'
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


program csdid_est, rclass
	syntax [anything],  [   WBOOT(str) 				///
							WBOOT1					///
							reps(int 999) 			///
							wbtype(str)  			/// Hidden option
							rseed(str)				/// set seed
							Level(int 95)			/// CI level
							window(str)             /// 
							] [estore(name) esave(name) replace]
	
	local  `:char _dta[note11]'
	
	if "`cmd'"!="csdid" {
	    display "This was not created after csdid"
		exit 10
	}
	
	forvalues i = 3/10 {
		local  `:char _dta[note`i']'
	}
	
	tempname cband
	tempname b1 b2 b3 b4 b5 
	tempname s1 s2 s3 s4 s5 
	
	local agg `anything'
	
	if "`agg'"=="" local agg event
	
	if !inlist("`agg'","attgt","simple","group","calendar","event")  {
		display "Option `agg' not allowed"
		exit 10
	}
	
	//////////////////////////////////
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
	
	//////////////////////////////////
	*matrix `cband'=1
	** New idea. Hacerlo todo desde makerif	
	*mata:makerif("`rifgt'","`rifwt'","__wgt__","`b'","`v'","`cluster' ")
	local ci = `level'/100
	
	noisily mata: makerif2("`rifgt'" , "`rifwt'","`agg'",  ///
						    "`glvls'","`tlvls'", ///
							"`b1'",  /// `b2' `b3' `b4' `b5' `b6'
							"`s1'",  ///  `s2' `s3' `s4' `s5' `s6'
							"`clvar' ", "`vcetype' ", "`cband'", /// 
									`ci', `reps', `wbtype', "`window'")
	tempname b V
	matrix `b' = `b1'
	matrix `V' = `s1'
	
	if "`agg'"=="attgt" {
		matrix colname `b'=`colname'
		matrix coleq   `b'=`eqname'
		matrix colname `V'=`colname'
		matrix coleq   `V'=`eqname'
		matrix rowname `V'=`colname'
		matrix roweq   `V'=`eqname'
		if "`wboot'"!="" {
			matrix colname `cband'=b se t ll ul
			matrix rowname `cband'=`colname'
			matrix roweq   `cband'=`eqname'
		}
	}
	
	foreach i of local glvls {
			local neqr = `neqr'+1			
	}
		
	capture:est store `lastreg'	
	ereturn clear
	adde post `b' `V'
	adde local cmd 	     csdid
	adde local cmd2	     estat
	adde local cmdline   estat `agg'
	adde local estat_cmd csdid_estat
	adde local agg		 `agg'
	adde scalar neqr =   `neqr'

	if "`wboot'`wboot1'"!="" {
		adde local vcetype "WBoot"
		tempname ccband
		matrix `ccband'=`cband'
		adde matrix cband `ccband'
	}
	if "`estore'"!="" est store `estore'
	if "`esave'" !="" est save  `esave', `replace'
	
	Display
	matrix rtb=r(table)
 	capture:qui:est restore `lastreg'
	
	return matrix table = rtb
	return matrix b `b1'
	return matrix  V `s1'
	return local agg `agg'
  	if "`wboot'"!="" {
		return matrix cband `cband'
	}
	return list
end			

 program define Display
                syntax [, bmatrix(passthru) vmatrix(passthru) *]
 		 
        _get_diopts diopts rest, `options'
        local myopts `bmatrix' `vmatrix'        
                if ("`rest'"!="") {
                                display in red "option {bf:`rest'} not allowed"
                                exit 198
                }
 				if ("`e(vcetype)'"=="WBoot") {
                    csdid_table, `diopts'
                 }
                else {
                    _coef_table,  `diopts' `myopts' neq(`e(neqr)')
                }
                
 
end

mata:

 vector event_list(real matrix glvl, tlvl,window){
 	real matrix toreturn, toreturn2
	real scalar i,j
	toreturn=J(1,0,.)
	toreturn2=J(1,0,.)
	for(i=1;i<=cols(glvl);i++) {
		for(j=1;j<=cols(tlvl);j++) {
			toreturn=toreturn,(tlvl[j] -glvl[i])
		}
	}
	toreturn=uniqrows(toreturn')'
	 
	if (cols(window)==0) return(toreturn)
	else {
	    for(i=1;i<=cols(toreturn);i++){
		    if  ( (toreturn[i]>=window[1]) & (toreturn[i]<=window[2]) )    toreturn2=toreturn2,toreturn[i]
		}
		 
		return(toreturn2)
	}
 }
// Next task. 
// amek all elements separete RIF_siple RIF event, etc
// Think how to save all elements.
		
void makerif2(string scalar rifgt_ , rifwt_ , agg, 
				glvl_, tlvl_, bb_, ss_, clvar_, wboot , cband_,
				real scalar ci, reps, wbtype, wnw ) {	
	// wnw Window				
    real matrix rifgt , rifwt, wgt, t0, glvl, tlvl
	real scalar i,j,k,h, wndw
	rifgt	= st_data(.,rifgt_)
	rifwt  	= st_data(.,rifwt_)
	wndw=strtoreal(tokens(wnw))

	/// pg here is just a dummy
	// stp1 all together?? No
	//all=att_gt,pg
	// stp2 get Mean(RIF) 
	// This just rescales the IFs RIF's to make the statistics later.
	
	glvl = strtoreal(tokens(glvl_))	
	tlvl = strtoreal(tokens(tlvl_))	
	
    real matrix ag_rif, ag_wt
	real matrix bb, VV, aux
	real vector ind_gt, ind_wt
	string matrix coleqnm
	real scalar flag 
	/////////////////////////////////////////
	// Always make attgt, even if not shown. 
	if (agg=="attgt") {
		make_tbl( (rifgt,rifwt) ,bb,VV,clvar_,wboot, cband_, ci, reps, wbtype)
 	}
	/////////////////////////////////////////
	if (agg=="simple") {
		k=0
		ind_gt=J(1,0,.)
		// to verify is combination exists
		ind_wt=colsum(abs(rifgt))
 
		for(i=1;i<=cols(glvl);i++) {
			for(j=1;j<=cols(tlvl);j++) {
				k++
				// G <= T
 				if ((glvl[i]<=tlvl[j]) & (ind_wt[k]!=0)) {
					//ag_rif=ag_rif, rifgt[.,k]
					//ag_wt =ag_wt , rifwt[.,i]
					ind_gt=ind_gt,k

				}
 			}
		}
		// Above gets the Right elements Below, aggregates them
		ag_rif = rifgt[.,ind_gt]
		ag_wt  = rifwt[.,ind_gt]
		aux = aggte(ag_rif, ag_wt)
		make_tbl(aux ,bb,VV,clvar_,wboot, cband_,ci, reps, wbtype)
		coleqnm = "ATT"
	}
	/////////////////////////////////////////
	
	if (agg=="group") {
		// i groups j time
		k=0
		
		aux    =J(rows(rifwt),0,.)
		coleqnm=""
		ind_wt=colsum(abs(rifgt))

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
		make_tbl(aux ,bb,VV,clvar_,wboot, cband_,ci, reps, wbtype)
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
		
			/// ag_wt=J(rows(rifwt),0,.)
 
			for(i=1;i<=cols(glvl);i++) {
				for(j=1;j<=cols(tlvl);j++) {
					k++
					if ( (glvl[i]<=tlvl[j]) & (tlvl[h]==tlvl[j]) & (ind_wt[k]!=0) ){
						//ag_rif=ag_rif, rifgt[.,k]
						//ag_wt =ag_wt , rifwt[.,i]
						ind_gt=ind_gt,k
						//ind_wt=ind_wt,i						
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
		make_tbl(aux ,bb,VV,clvar_,wboot, cband_,ci, reps, wbtype)
	}
	
	if (agg=="event") {
		// i groups j time
		real matrix evnt_lst
		evnt_lst=event_list(glvl,tlvl,wndw)
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
					if ( ((glvl[i]+evnt_lst[h])==tlvl[j])  & (ind_wt[k]!=0)  ) {	
						//ag_rif=ag_rif, rifgt[.,k]
						//ag_wt =ag_wt , rifwt[.,i]
						ind_gt=ind_gt,k
						//ind_wt=ind_wt,i							
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
				aux = aux, aggte(ag_rif, ag_wt, )
			}
		}	
		// get table elements		
		make_tbl(aux ,bb,VV,clvar_,wboot, cband_, ci, reps, wbtype)
	}
	
	st_matrix(bb_,bb)
	st_matrix(ss_,VV)

	if (agg!="attgt") {
		stata("matrix colname "+bb_+" ="+coleqnm)
		stata("matrix colname "+ss_+" ="+coleqnm)
		stata("matrix rowname "+ss_+" ="+coleqnm)
		if (wboot!=" ") {
			stata("matrix colname "+cband_+" ="+"b se t ll ul")
			stata("matrix rowname "+cband_+" ="+coleqnm)
		}		
	}
	
}

void make_tbl(real matrix rif,bb,VV, clv , string  scalar wboot, cband_,
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
		clusterse((rif:-bb),clvar,VV,cln)		
	}
	real matrix cband
	// wboot no cluster
	if (wboot!=" ") {
		mboot(rif,bb, VV, cband, clv, ci, reps, wbtype)
		st_matrix(cband_,cband)
	}
 } 

void clusterse(real matrix iiff, cl, V, real scalar cln){
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
	cln=nc
	// Esto es para ver como hacer clusters.
	//*nc/(nc-1)
	//st_matrix(V,    vv)
	//st_numscalar(ncl, nc)
	//        ^     ^
	//        |     |
	//      stata   mata
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
 
void mboot(real matrix rif,mean_rif, vv, cband, string scalar clv,
			real scalar ci, reps, wbtype) {
    //, real scalar reps, bwtype, ci 
    real matrix fr
	///real scalar reps, wbtype
	///reps   = 999
	///wbtype =   1
	///ci     = 0.95
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
		// this gets Tvalue
		
		cband=( mean_rif',
				ifse',
				mean_rif':/ifse',
				mean_rif':-qtp(abs(fr :/ ifse),ci)':* ifse' ,  
				mean_rif':+qtp(abs(fr :/ ifse),ci)':* ifse'   )
				
	}
	vv=quadcross(ifse,ifse):*I(rows(ifse))
	//sqrt(variance(fr))
	//st_matrix(vv,iqrse(fr)^2)
	//st_matrix(cband,ccb)
}


end

