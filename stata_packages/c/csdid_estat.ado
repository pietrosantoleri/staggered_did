*! v1.5 FRA Change from nlcom to mata
*! v1.2 FRA Adds the options for simple and calendar so it doesnt depend on consecutive years
* also adds the option window
*! v1 FRA Adds Safeguards to csdid calendar. Calendar starts after treatment
** Estat command for aggregators
program csdid_estat, sortpreserve  
version 14
        if "`e(cmd)'" != "csdid" {
                error 301
        }
		gettoken key rest : 0, parse(", ")
		if "`e(vcetype)'"=="WBoot" {
		    display "Test will be based on asymptotic VCoV"
			display "{p}If you want aggregations based on WB, use option saverif() ad csdid_stats{p_end}"
		}
		if inlist("`key'","attgt","simple","pretrend","group","calendar","event","all") {
			csdid_`key'  `rest'
		}
		else {
		    display in red "Option `key' not recognized"
			error 199
		}
		
end

program csdid_all, sortpreserve rclass
	csdid_pretrend
	csdid_simple
	csdid_group
	csdid_calendar
	csdid_event
end

program csdid_pretrend, sortpreserve rclass
	clrreturn
	display "Pretrend Test. H0 All Pre-treatment are equal to 0"
	mata:csdid_pretrend("e(b_attgt)","e(V_attgt)","`e(glev)'","`e(tlev)'")
	display "chi2(`r(df)') = `r(chi2)'"
	display "p-value       = `r(pchi2)'"
end

program clrreturn, rclass
        exit
end

program csdid_attgt,  rclass sortpreserve
	syntax, [estore(name) esave(name) replace]
 	display "ATT GT with WBOOT SE (alternative method)"
	tempname lastreg
	tempvar b V table
	matrix `b' = e(b_attgt)
	matrix `V' = e(V_attgt)
	matrix r_b_ = e(b_attgt)
	matrix r_V_ = e(V_attgt)
	
	capture:est store `lastreg'	
	ereturn clear
	adde post `b' `V'
	adde local cmd 	   estat
	adde local cmdline estat attgt
	if "`estore'"!="" est store `estore'
	if "`esave'" !="" est save  `esave', `replace'
	_coef_table
	matrix rtb=r(table)
	qui:est restore `lastreg'
	return matrix table = rtb
	return matrix b = r_b_
	return matrix V = r_V_
end
	
program csdid_simple,  rclass sortpreserve
	syntax, [estore(name) esave(name) replace]
 	display "Average Treatment Effect on Treated"
	mata:csdid_simple("e(b_attgt)","e(V_attgt)","`e(glev)'","`e(tlev)'")
	tempname lastreg
	tempvar b V table
	matrix `b' = r_b_
	matrix `V' = r_V_
	matrix colname `b' = ATT
	matrix colname `V' = ATT
	matrix rowname `V' = ATT
	capture:est store `lastreg'	
	ereturn clear
	adde post `b' `V'
	adde local cmd 	   estat
	adde local cmdline estat simple
	if "`estore'"!="" est store `estore'
	if "`esave'" !="" est save  `esave', `replace'
	_coef_table
	matrix rtb=r(table)
	qui:est restore `lastreg'
	return matrix table = rtb
	return matrix b = r_b_
	return matrix V = r_V_
end

program csdid_group, sortpreserve rclass
	syntax, [estore(name) esave(name) replace]
  	display "ATT by group"
	mata:csdid_group("e(b_attgt)","e(V_attgt)","`e(glev)'","`e(tlev)'")
	tempname lastreg
	tempvar b V table
	matrix `b' = r_b_
	matrix `V' = r_V_
	
	capture:est store `lastreg'	
	ereturn clear
	adde post `b' `V'
	adde local cmd 	   estat
	adde local cmdline estat group
	if "`estore'"!="" est store `estore'
	if "`esave'" !="" est save  `esave', `replace'
	_coef_table
	matrix rtb=r(table)
	qui:est restore `lastreg'
	return matrix table = rtb
	return matrix b = r_b_
	return matrix V = r_V_
end

program csdid_calendar, sortpreserve rclass
	syntax, [estore(name) esave(name) replace]
  	display "ATT by Calendar Period"
	mata:csdid_calendar("e(b_attgt)","e(V_attgt)","`e(glev)'","`e(tlev)'")
	tempname lastreg
	tempvar b V table
	matrix `b' = r_b_
	matrix `V' = r_V_
	
	capture:est store `lastreg'	
	ereturn clear
	adde post `b' `V'
	adde local cmd 	   estat
	adde local cmdline estat calendar
	if "`estore'"!="" est store `estore'
	if "`esave'" !="" est save  `esave', `replace'
	_coef_table
	matrix rtb=r(table)
	qui:est restore `lastreg'
	return matrix table = rtb
	return matrix b = r_b_
	return matrix V = r_V_
	
end
 
program csdid_event, sortpreserve rclass
	syntax, [estore(name) esave(name) replace window(str) ]
   	display "ATT by Periods Before and After treatment"
	display "Event Study:Dynamic effects"
	if "`window'"!="" {
		numlist "`window'", min(2) max(2) sort integer
		local window `r(numlist)'
	}
 
	mata:csdid_event("e(b_attgt)","e(V_attgt)","`e(glev)'","`e(tlev)'","`window'")
	tempname lastreg
	tempvar b V table
	matrix `b' = r_b_
	matrix `V' = r_V_
	
	capture:est store `lastreg'	
	ereturn clear
	adde post `b' `V'
	adde local cmd 	   estat
	adde local cmdline estat event
	if "`estore'"!="" est store `estore'
	if "`esave'" !="" est save  `esave', `replace'
	_coef_table
	matrix rtb=r(table)
	qui:est restore `lastreg'
	return matrix table = rtb
	return matrix b = r_b_
	return matrix V = r_V_
end 


program adde, eclass
        ereturn `0'
end

program addr, rclass
        return `0'
end

mata
void csdid_group(string scalar bb_, vv_, gl_, tl_){
    real matrix b, v , ii, jj, glvl, tlvl
	
	glvl = strtoreal(tokens(gl_));tlvl = strtoreal(tokens(tl_))	
	b=st_matrix(bb_);v=st_matrix(vv_)
	real scalar k, i, j, flag
	string scalar coleqnm
	ii=(1..(cols(glvl)*cols(tlvl))),(cols(glvl)*cols(tlvl)):+(1..cols(glvl))#J(1,cols(tlvl),1)
     
	real matrix br, bw
	br=b[1,(1..(cols(ii)/2))]
	bw=b[1,((cols(ii)/2+1)..cols(ii))]
	ii=(1..(cols(glvl)*cols(tlvl)))
	
	k=0
	coleqnm=""
	real matrix iii
	iii=J(0,cols(ii),.)
	/// ag_wt=J(rows(rifwt),0,.)
	for(i=1;i<=cols(glvl);i++) {
	    ii=ii*0
		flag = 0
		for(j=1;j<=cols(tlvl);j++) {
			k++
			if ((glvl[i]<=tlvl[j]) & (b[k]!=0) ) {
				//ag_rif=ag_rif, rifgt[.,k]
				ii[k]=1
				flag=1
			}
		}
		if (flag==1) {
			iii=iii\ii
			coleqnm=coleqnm+sprintf(" G%s",strofreal(glvl[i]))	
		}
	}
	 
	real matrix r1, r2
	r1 = (bw :* iii):/rowsum(bw :* iii)
	r2 = (br :* iii):/rowsum(bw :* iii):-rowsum((br:*iii):*(bw:*iii)):/(rowsum(bw :* iii):^2)
	r2 = r2:*iii
	real matrix bbb, vvv
	bbb=rowsum(br :* bw:*iii):/rowsum(bw :* iii)
 
	vvv=makesymmetric((r1,r2)*v*(r1,r2)')
	
	
	st_matrix("r_b_",bbb')
	st_matrix("r_V_",vvv)
	
	stata("matrix colname r_b_ ="+coleqnm)
	stata("matrix colname r_V_ ="+coleqnm)
	stata("matrix rowname r_V_ ="+coleqnm)

}
 
 
void csdid_calendar(string scalar bb_, vv_, gl_, tl_){
    real matrix b, v , ii, jj, glvl, tlvl
	glvl = strtoreal(tokens(gl_));tlvl = strtoreal(tokens(tl_))	
	b=st_matrix(bb_);v=st_matrix(vv_)
	real scalar k, i, j, h, flag
	string scalar coleqnm
	ii=(1..(cols(glvl)*cols(tlvl))),(cols(glvl)*cols(tlvl)):+(1..cols(glvl))#J(1,cols(tlvl),1)

	//v=v[ii,ii]
	//b=b[ii]
	real matrix br, bw
	br=b[1,(1..(cols(ii)/2))]
	bw=b[1,((cols(ii)/2+1)..cols(ii))]
	ii=(1..(cols(glvl)*cols(tlvl)))

	coleqnm=""
	real matrix iii
	iii=J(0,cols(ii),.)
	
	for(h=1;h<=cols(tlvl);h++){
		k=0
		flag=0
		ii=ii*0
		for(i=1;i<=cols(glvl);i++) {
			for(j=1;j<=cols(tlvl);j++) {
				k++
				if ((glvl[i]<=tlvl[j]) & (tlvl[h]==tlvl[j]) & (b[k]!=0) ){
					ii[k] = 1
					if (flag==0) coleqnm=coleqnm+sprintf(" T%s",strofreal(tlvl[h]))
					flag=1
				}
			}
		}
		if (flag == 1) iii=iii\ii
	}
	real matrix r1, r2
	r1 = (bw :* iii):/rowsum(bw :* iii)
	r2 = (br :* iii):/rowsum(bw :* iii):-rowsum((br:*iii):*(bw:*iii)):/(rowsum(bw :* iii):^2)
	r2 = r2:*iii
	real matrix bbb, vvv
	bbb=rowsum(br :* bw:*iii):/rowsum(bw :* iii)
 
	vvv=makesymmetric((r1,r2)*v*(r1,r2)')
	
	st_matrix("r_b_",bbb')
	st_matrix("r_V_",vvv)
	
	stata("matrix colname r_b_ ="+coleqnm)
	stata("matrix colname r_V_ ="+coleqnm)
	stata("matrix rowname r_V_ ="+coleqnm)
	}

void csdid_pretrend(string scalar bb_, vv_, gl_, tl_){
    real matrix b, v , ii, glvl, tlvl
	glvl = strtoreal(tokens(gl_));tlvl = strtoreal(tokens(tl_))	
	b=st_matrix(bb_);v=st_matrix(vv_)
	real scalar k, i, j
	k=0;ii=J(1,0,.)
	for(i=1;i<=cols(glvl);i++) {
		for(j=1;j<=cols(tlvl);j++) {
			k++
			if ( (glvl[i]>tlvl[j]) & (b[k]!=0) )  {
				ii=ii,k				
			}
		}
	}
	real matrix bb, vv
	bb=b[ii];vv=v[ii,ii]
	real scalar chi2, df
	chi2=bb*invsym(vv)*bb'
	df = cols(bb)
	st_numscalar("r(chi2)",chi2)
	st_numscalar("r(df)",df)
	st_numscalar("r(pchi2)",chi2tail(df,chi2))
}

 
void csdid_simple(string scalar bb_, vv_, gl_, tl_) {
	real matrix b, v , ii, jj, glvl, tlvl
	glvl = strtoreal(tokens(gl_));tlvl = strtoreal(tokens(tl_))	
	b=st_matrix(bb_);v=st_matrix(vv_)
	
	real scalar k, i, j
	k=0
	real matrix br, bw
	ii=(1..2*(cols(glvl)*cols(tlvl)))
	br=b[1,(1..(cols(ii)/2))]
	bw=b[1,((cols(ii)/2+1)..cols(ii))]
	ii=(1..(cols(glvl)*cols(tlvl)))
  
	ii=ii*0

	for(i=1;i<=cols(glvl);i++) {
		for(j=1;j<=cols(tlvl);j++) {
			k++
			if ((glvl[i]<=tlvl[j]) & (b[k]!=0)) {
				ii[k] = 1
				//jj=jj,i
			}
		}
	}
 	real matrix r1, r2
	r1 = (bw :* ii):/rowsum(bw :* ii)
	r2 = (br :* ii):/rowsum(bw :* ii):-rowsum((br:*ii):*(bw:*ii)):/(rowsum(bw :* ii):^2)
	r2 = r2:*ii
 	real matrix bbb, vvv
 
	bbb=rowsum(br :* bw:*ii):/rowsum(bw :* ii)
 	vvv=makesymmetric((r1,r2)*v*(r1,r2)')
	
 	st_matrix("r_b_",bbb)
	st_matrix("r_V_",vvv)
}

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
  
 void csdid_event(string scalar  bb_, vv_, gl_, tl_, wnw){
    real matrix b, v , ii, jj, glvl, tlvl, wndw
	glvl = strtoreal(tokens(gl_));tlvl = strtoreal(tokens(tl_))	
	b=st_matrix(bb_);v=st_matrix(vv_)
	wndw=strtoreal(tokens(wnw))
	 
	real matrix evnt_lst
	evnt_lst=event_list(glvl,tlvl,wndw)
 	real scalar k, i, j, h, flag
	string scalar coleqnm
	ii=(1..(cols(glvl)*cols(tlvl))),(cols(glvl)*cols(tlvl)):+(1..cols(glvl))#J(1,cols(tlvl),1)

	//v=v[ii,ii]
	//b=b[ii]
	real matrix br, bw
	br=b[1,(1..(cols(ii)/2))]
	bw=b[1,((cols(ii)/2+1)..cols(ii))]
	ii=(1..(cols(glvl)*cols(tlvl)))

	coleqnm=""
	real matrix iii
	iii=J(0,cols(ii),.)
	
	for(h=1;h<=cols(evnt_lst);h++){
		k=0
		flag=0
		ii=ii*0
		for(i=1;i<=cols(glvl);i++) {
			for(j=1;j<=cols(tlvl);j++) {
				k++
				if ( ( (glvl[i]+evnt_lst[h])==tlvl[j] ) & (b[k]!=0) ) {	
					//ag_rif=ag_rif, rifgt[.,k]
					//ag_wt =ag_wt , rifwt[.,i]
					ii[k] = 1						
					if (flag==0) {
						if (evnt_lst[h]< 0) coleqnm=coleqnm+sprintf(" T%s" ,strofreal(evnt_lst[h]))
						if (evnt_lst[h]==0) coleqnm=coleqnm+" T"
						if (evnt_lst[h]> 0) coleqnm=coleqnm+sprintf(" T+%s",strofreal(evnt_lst[h]))
					}
					flag=1
				}
			}
		}
		if (flag == 1) iii=iii\ii
	}
	real matrix r1, r2
	r1 = (bw :* iii):/rowsum(bw :* iii)
	r2 = (br :* iii):/rowsum(bw :* iii):-rowsum((br:*iii):*(bw:*iii)):/(rowsum(bw :* iii):^2)
	r2 = r2:*iii
	real matrix bbb, vvv
	bbb=rowsum(br :* bw:*iii):/rowsum(bw :* iii)
 
	vvv=makesymmetric((r1,r2)*v*(r1,r2)')
	
	st_matrix("r_b_",bbb')
	st_matrix("r_V_",vvv)
	
	stata("matrix colname r_b_ ="+coleqnm)
	stata("matrix colname r_V_ ="+coleqnm)
	stata("matrix rowname r_V_ ="+coleqnm)
	}
end

 
 
program stuff
************ Estat
** estat Pretrend
qui{
test ([g2006]t_2003_2004=0 ) ( [g2006]t_2004_2005=0) ( [g2007]t_2003_2004=0) ( [g2007]t_2004_2005=0) ( [g2007]t_2005_2006=0)

local pretrend
foreach i in 2004 2006 2007 {		
	foreach j in 2004 2005 2006 2007 {
		local time1 = min(`i'-1, `j'-1)
		if `j'<`i' local pretrend `pretrend' ([g`i']t_`time1'_`j'=0)
	}
}
test `pretrend'
}
** estat simple 
qui{
local simple
local simple `simple' (simple: ( ( 
foreach j in 2004 2005 2006 2007 {
	foreach i in 2004 2006 2007 {		
		local time1 = min(`i'-1, `j'-1)
		if (`i'<=`j') {
			local simple `simple' [g`i']t_`time1'_`j'*[wgt]w`i'+
			local wcl      `wcl' 	  [wgt]w`i'+
		}
	}
}
local simple `simple' 0)/(`wcl'0)))
display "`simple'"
nlcom  `simple'
nlcom (Simple: ((_b[g2004:t_2003_2004]+
				_b[g2004:t_2003_2005]+
				_b[g2004:t_2003_2006]+
				_b[g2004:t_2003_2007])*_b[wgt:w2004]+ 
				(_b[g2006:t_2005_2006]
				+_b[g2006:t_2005_2007])*_b[wgt:w2006]
				+_b[g2007:t_2006_2007]*_b[wgt:w2007])/(_b[wgt:w2004]*4+_b[wgt:w2006]*2+_b[wgt:w2007]) )
}
** estat ** group
qui {
local group
foreach i in 2004 2006 2007 {		
    local group `group' (g`i': ( ( 
	local cnt=0
	foreach j in 2004 2005 2006 2007 {
		local time1 = min(`i'-1, `j'-1)
		if (`i'<=`j') {
		    		local cnt=`cnt'+1
 			local group `group' [g`i']t_`time1'_`j'+
		}
	}
	local group `group' 0)/`cnt'))
}
display "`group'"
nlcom `group'
}
** Calendar Year Needs to consider Weights
qui {
local calendar
** J if for Year i for G
foreach j in 2004 2005 2006 2007 {
    local calendar `calendar' (t`j': ( ( 
	macro drop _wcl
	foreach i in 2004 2006 2007 {		
		local cnt=0    
		local time1 = min(`i'-1, `j'-1)
		if (`i'<=`j') {
 			local calendar `calendar' [g`i']t_`time1'_`j'*[wgt]w`i'+
			local wcl      `wcl' 	  [wgt]w`i'+
 
		}
	}
	local calendar `calendar' 0)/(`wcl'0)))
}
nlcom `calendar'

nlcom 	(t2004: _b[g2004:t_2003_2004]) ///
		(t2005: _b[g2004:t_2003_2005]) ///
		(t2006: (_b[g2004:t_2003_2006]*_b[wgt:w2004] + _b[g2006:t_2005_2006]*_b[wgt:w2006])/(_b[wgt:w2004]+_b[wgt:w2006]))	 ///
		(t2007: (_b[g2004:t_2003_2007]*_b[wgt:w2004] + _b[g2006:t_2005_2007]*_b[wgt:w2006] + _b[g2007:t_2006_2007]*_b[wgt:w2007] )/(_b[wgt:w2004]+_b[wgt:w2006]+_b[wgt:w2007]))	 , noheader 
}
*** Events Study
qui {
local evnt0
local wcl
** J if for Year i for G

local emin `tmin'-`gmax'
local emax `tmax'-`gmin'
local emin -3
local emax 3
forvalues e = -3/3 {
 
	local e_t `=cond(sign(`e')<0,"_", )'`=abs(`e')'
	local wcl 
	local evnt0 `evnt0' (E`e_t': ( ( 
	display in red "E`e_t'"
	foreach j in 2004 2005 2006 2007 {
		foreach i in 2004 2006 2007 {	
		    
 			local time1 = min(`i'-1, `j'-1)

				if `i'+`e'==`j' {
				    *display "g:`i' ; t: `time1' ; t1:`j'"
					local evnt0 `evnt0'    [g`i']t_`time1'_`j'*[wgt]w`i'+
					local wcl      `wcl' 	  [wgt]w`i'+				    
				}
			
		}
	}
	local evnt0 `evnt0' 0)/(`wcl'0)))
}
 
nlcom `evnt0'
`evnt0'
 


nlcom (E_3: ( _b[g2007:t_2003_2004]*_b[wgt:w2007]) /// 
			/(_b[wgt:w2007])) ///  
	(E_2: ( _b[g2006:t_2003_2004]*_b[wgt:w2006]+ ///
	         _b[g2007:t_2004_2005]*_b[wgt:w2007]) /// 
			/(_b[wgt:w2006]+_b[wgt:w2007])) /// 
			(E_1: ( _b[g2006:t_2004_2005]*_b[wgt:w2006]+ ///
	         _b[g2007:t_2005_2006]*_b[wgt:w2007]) /// 
			/(_b[wgt:w2006]+_b[wgt:w2007])) ///   
(E0: ( _b[g2004:t_2003_2004]*_b[wgt:w2004]+ ///
	         _b[g2006:t_2005_2006]*_b[wgt:w2006]+ ///
	         _b[g2007:t_2006_2007]*_b[wgt:w2007]) /// 
			/(_b[wgt:w2004]+_b[wgt:w2006]+_b[wgt:w2007]))	///   
	(E1: ((_b[g2004:t_2003_2005])*_b[wgt:w2004]+ ///
	   (_b[g2006:t_2005_2007])*_b[wgt:w2006])/(_b[wgt:w2004]+_b[wgt:w2006]))	///
	(E2: ((_b[g2004:t_2003_2006])*_b[wgt:w2004]) /(_b[wgt:w2004]))	   ///
	(E3: ((_b[g2004:t_2003_2007])*_b[wgt:w2004]) /(_b[wgt:w2004]))	  
	
}
end
