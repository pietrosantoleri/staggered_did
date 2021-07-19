{smcl}
{* *! version 1.0.01  29july2015}{...}
{cmd:help avar}
{hline}

{title:Title}

{p2colset 5 16 18 2}{...}
{p2col:{hi: avar} {hline 2}}Asymptotic covariance estimation for iid and non-iid data
robust to heteroskedasticity, autocorrelation, 1- and 2-way clustering,
common cross-panel autocorrelated disturbances, etc.{p_end}
{p2colreset}{...}

{marker syntax}{...}
{title:Syntax}

{phang}
Single-equation context:

{p 8 14 2}
{cmd:avar}
{it:evarname} {it:(zvarlist)}
[{it:weight}] [{cmd:if} {it:exp}] [{cmd:in} {it:range}]
{bind:[{cmd:,} {it:vce_options}} {it:misc_options}]

{phang}
Multiple-equation context:

{p 8 14 2}
{cmd:avar}
{it:(evarlist)} {it:(zvarlist)}
[{it:weight}] [{cmd:if} {it:exp}] [{cmd:in} {it:range}]
{bind:[{cmd:,} {it:vce_options}} {it:misc_options}]

{synoptset 20}{...}
{synopthdr:vce_options}
{synoptline}
{synopt:{cmdab:r:obust}}
heteroskedastic-robust VCE
{p_end}
{synopt:{cmdab:cl:uster}{cmd:(}{it:cvarlist}{cmd:)}}
1- or 2-way cluster- and heteroskedasticity-robust VCE
{p_end}
{synopt:{cmd:bw(}{it:#}{cmd:)}}
bandwidth for kernel-robust (AC or HAC) VCE
{p_end}
{synopt:{cmd:kernel(}{it:string}{cmd:)}}
kernel for kernel-robust (AC or HAC) VCE
{p_end}
{synopt:{cmd:dkraay(}{it:#}{cmd:)}}
VCE robust to autocorrelated cross-panel disturbances with bandwidth={it:#}
{p_end}
{synopt:{cmd:kiefer}}
VCE robust to within-panel autocorrelation
{p_end}
{synopt:{cmd:center}}
use centered moments
{p_end}
{synoptline}
{p2colreset}{...}

{synoptset 20}{...}
{synopthdr:misc_options}
{synoptline}
{synopt:{cmd:partial(}{it:pvarlist}{cmd:)}}
partial-out (project-out) variables in {it:pvarlist} from variables in {it:evarlist} and {it:zvarlist};
constant also partialled-out
{p_end}
{synopt:{cmdab:noc:onstant}}
suppress constant from {it:(zvarlist)}
{p_end}
{synopt:{cmd:demean}}
center (demean) variables in {it:evarlist} and {it:zvarlist} (implies {it:noconstant})
{p_end}
{synopt:{cmd:smata(}{it:mname}{cmd:)}}
name for avar matrix left in Mata environment if requested
{p_end}
{synoptline}
{p2colreset}{...}

{pstd}
{opt avar} may be used with time-series or panel data,
in which case the data must be tsset first; see help {helpb tsset}.

{pstd}
All varlists may contain time-series operators,
but factor variables are not currently supported; see help varlist.


{title:Contents}

{phang}{help avar##description:Description}{p_end}
{phang}{help avar##summary_vce:Summary of VCE options}{p_end}
{phang}{help avar##use_examples:Examples of usage}{p_end}
{phang}{help avar##rep_examples:Replication examples}{p_end}
{phang}{help avar##saved_results:Saved results}{p_end}
{phang}{help avar##references:References}{p_end}
{phang}{help avar##citation:Citation of avar}{p_end}


{marker description}{...}
{title:Description}

{pstd}
{opt avar} is a routine for estimating {it:S}, the asymptotic variance of {it:(1/N)*Z'e},
where {it:Z} is an {it:N}x{it:L} matrix of L variables,
{it:e} is an {it:N}x{it:p} matrix of p variables,
and {it:N} is the sample size.
Typically, {it:S} would be used to form a sandwich-type estimate of the variance of an estimator,
where {it:S} is the "filling" of the sandwich.

{pstd}
{opt avar} can estimate VCEs for single and multiple equations
that are robust to various violations of the assumption of iid data,
including heteroskedasticity, autocorrelation, 1- and 2-way clustering, common cross-panel disturbances, etc.
It supports time-series and panel data.

{pstd}
For example, in the most basic application - linear regression -
{it:e} is a {it:N}x{it:1} vector of residuals from an OLS estimation
and {it:Z} is the {it:N}x{it:L} matrix of L regressors.
The variance of the OLS estimator {it:var(beta)} can be estimated by the "sandwich" {it:N*DSD},
where the "filling" {it:S} is the estimate of the asymptotic variance of {it:(1/N)*Z'e}
and the "bread" is {bind:{it:D=(1/N)inv(X'X)}}.
This estimate of the variance of the OLS estimator is as robust as {it:S}:
if {it:S} is robust to heteroskedasticity, so will be {it:var(beta)};
if {it:S} is robust to 2-way clustering, so will be {it:var(beta)}; etc.

{pstd}
In the multi-equation context,
{it:e} is a {it:N}x{it:p} matrix of residuals from p different estimations.
{opt avar} will return an estimate of the asymptotic variance
including the covariances across equations.
For example, {opt avar} can estimate an {it:S}
that is consistent with the {it:var(beta)} reported by
the Stata routines {helpb sureg}, {helpb reg3} and {helpb suest};
see the examples below for illustrations.
{bf:Important:} in its current implementation,
{opt avar} uses {it:listwise deletion}.
This means it will only use observations for which 
there are no missing values in {it:any} of the variables in {it:elist}.
(This is the behavior of {helpb sureg} and {helpb reg3}
but not of {helpb suest}.)

{pstd}
The {opt partial(pvarlist)} option may be used to partial out (project out)
the additional variables {it:pvarlist}
from the variables in {it:evarlist} and {it:zvarlist}.
Note that these variables should {it:not} be included in {it:zvarlist}.
A constant is automatically partialled out as well.
The {opt demean} option can be used if only the constant is to be partialled out.

{pstd}
{opt avar} uses the Mata library {opt livreg2}
used by {helpb ivreg2}, {helpb ranktest} and related routines.
For a more detailed discussion of the various VCE options
see the {helpb ivreg2} help file.
For a discussion of the theory behind
the estimation of asymptotic variances in the econometric context,
see Hayashi (2000), especially chapters 2, 4 and 6.


{marker summary_vce}{...}
{title:Summary of VCE options}

{p2col 5 27 28 0: {it:robust}}
heteroskedastic-consistent (HC) or "robust" VCE
{p_end}

{p2col 5 27 28 0: {it:bw(#)}}
autocorrelation-consistent (AC) VCE with bandwidth={it:#}
{p_end}

{p2col 5 27 28 0: {it:robust} {it:bw(#)}}
heteroskedastic- and autocorrelation-consistent (HAC) VCE with bandwidth={it:#}
{p_end}

{p2col 5 27 28 0: {it:kernel(string)}}
kernel used in AC and HAC estimation;
default kernel is Bartlett;
supported kernels (abbreviations) are Bartlett (bar);
Truncated (tru); Parzen (par); Tukey-Hanning (thann); Tukey-Hamming (thamm); Danielle (dan);
Tent (ten); and Quadratic-Spectral (qua or qs)
{p_end}

{p2col 5 27 28 0: {it:cluster(cvar)}}
VCE robust to heteroskedasticity and within-group correlation, where {it:varname} defines the group;
may be combined with {it:bw(#)} if data are {opt tsset} with {opt cvar} as the time variable
{p_end}

{p2col 5 27 28 0: {it:cluster(cvar1 cvar2)}}
2-way cluster-robust VCE, robust to heteroskedasticity and correlation within groups defined by {it:var1} and {it:var2};
may be combined with {it:bw(#)} if data are {opt tsset} with either {opt cvar1} or {opt cvar2} as the time variable
{p_end}

{p2col 5 27 28 0: {it:dkraay(#)}}
VCE robust to autocorrelated cross-panel disturbances with bandwidth=# as per Driscoll & Kraay (1998);
equivalent to {it:cluster(tvar)} + {it:bw(#)} where {it:tvar} is the time variable as per {helpb tsset}
{p_end}

{p2col 5 27 28 0: {it:kiefer}}
VCE robust to within-group autocorrelation (but {it:not} heteroskedasticity) as per Kiefer (1980);
equivalent to {it:kernel(tru)} + {it:bw(#)} where {it:#} is the full length of the panel
{p_end}

{p2col 5 27 28 0: {it:center}}
use centered moments (sample moments with mean zero), i.e., instead of Z'e use (Z'e-mean(Z'e)),
as per e.g. Hall (2005) pp. 131-8 and 145-8
{p_end}


{title:Examples (nb: examples can also be found in the supplementary certification do file)}

{pstd}{bf:Setup}{p_end}

{pstd}Set up artificial dataset{p_end}

{phang2}. {stata "clear"}{p_end}
{phang2}. {stata "set obs 100"}{p_end}
{phang2}. {stata "set seed 12345"}{p_end}
{phang2}. {stata "gen double y1 = runiform()"}{p_end}
{phang2}. {stata "gen double y2 = runiform()"}{p_end}
{phang2}. {stata "gen double x1 = runiform()"}{p_end}
{phang2}. {stata "gen double x2 = runiform()"}{p_end}
{phang2}. {stata "gen double z1 = runiform()"}{p_end}
{phang2}. {stata "gen double z2 = runiform()"}{p_end}

{pstd}OLS and IV residuals{p_end}

{phang2}. {stata "qui reg y1 x1 x2"}{p_end}
{phang2}. {stata "predict double e1, resid"}{p_end}
{phang2}. {stata "qui reg y2 x1 x2"}{p_end}
{phang2}. {stata "predict double e2, resid"}{p_end}
{phang2}. {stata "qui reg y1 x1"}{p_end}
{phang2}. {stata "predict double es1, resid"}{p_end}
{phang2}. {stata "qui reg y2 x2"}{p_end}
{phang2}. {stata "predict double es2, resid"}{p_end}
{phang2}. {stata "qui ivreg y1 (x1 x2 = z1 z2)"}{p_end}
{phang2}. {stata "predict double eiv1, resid"}{p_end}
{phang2}. {stata "qui ivreg y2 (x1 x2 = z1 z2)"}{p_end}
{phang2}. {stata "predict double eiv2, resid"}{p_end}

{pstd}Cross-section, time and panel variables{p_end}

{phang2}. {stata "gen int id1 = _n"}{p_end}
{phang2}. {stata "gen int t1  = _n"}{p_end}
{phang2}. {stata "gen int id2 = ceil(_n/5)"}{p_end}
{phang2}. {stata "gen int t2  = 5-(id2*5-t1)"}{p_end}

{marker use_examples}{...}
{pstd}{bf:Basic usage}{p_end}

{phang2}. {stata "avar e1 (x1 x2)"}{p_end}

{phang2}. {stata "avar e1 (x1 x2), robust"}{p_end}

{phang2}. {stata "avar e1 (x1 x2), cluster(id2) partial(z1)"}{p_end}

{phang2}. {stata "avar (e1 e2) (x1 x2), robust"}{p_end}

{phang2}. {stata "avar e1 (x1 x2), smata(my_avar_matrix)"}{p_end}
{phang2}. {stata "mata: my_avar_matrix"}{p_end}

{marker rep_examples}{...}
{pstd}{bf:Replication examples}{p_end}

{pstd}Replicate VCV for OLS+robust (note small-sample correction used by Stata's regress){p_end}

{phang2}. {stata "reg y1 x1 x2, rob"}{p_end}
{phang2}. {stata "mat V1 = e(V)"}{p_end}
{phang2}. {stata "mat accum XX=x1 x2"}{p_end}
{phang2}. {stata "mat Sxx=XX*1/r(N)"}{p_end}
{phang2}. {stata "mat Sxxi=syminv(Sxx)"}{p_end}
{phang2}. {stata "avar e1 (x1 x2), rob"}{p_end}
{phang2}. {stata "mat S=r(S)"}{p_end}
{phang2}. {stata "mat V2 = Sxxi*S*Sxxi*1/r(N)"}{p_end}
{phang2}. {stata "mat V2 = V2 * e(N)/e(df_r)"}{p_end}
{phang2}. {stata "mat list V1"}{p_end}
{phang2}. {stata "mat list V2"}{p_end}

{pstd}Replicate VCV for OLS+Newey-West (note small-sample correction){p_end}

{phang2}. {stata "tsset t1"}{p_end}
{phang2}. {stata "newey y1 x1 x2, lag(3)"}{p_end}
{phang2}. {stata "mat V1 = e(V)"}{p_end}
{phang2}. {stata "mat accum XX=x1 x2"}{p_end}
{phang2}. {stata "mat Sxx=XX*1/r(N)"}{p_end}
{phang2}. {stata "mat Sxxi=syminv(Sxx)"}{p_end}
{phang2}. {stata "avar e1 (x1 x2), rob bw(4) kernel(bartlett)"}{p_end}
{phang2}. {stata "mat S=r(S)"}{p_end}
{phang2}. {stata "mat V2 = Sxxi*S*Sxxi*1/r(N)"}{p_end}
{phang2}. {stata "mat V2 = V2 * e(N)/e(df_r)"}{p_end}
{phang2}. {stata "mat list V1"}{p_end}
{phang2}. {stata "mat list V2"}{p_end}

{pstd}Replicate VCV for SUR estimator: 2 eqns, exactly-identified, iid errors{p_end}

{phang2}. {stata "sureg (y1 x1 x2) (y2 x1 x2)"}{p_end}
{phang2}. {stata "mat V1 = e(V)"}{p_end}
{phang2}. {stata "mat accum XX=x1 x2"}{p_end}
{phang2}. {stata "mat Sxx=XX*1/r(N)"}{p_end}
{phang2}. {stata "mat Sxxi=syminv(Sxx)"}{p_end}
{phang2}. {stata "avar (e1 e2) (x1 x2)"}{p_end}
{phang2}. {stata "mat S=r(S)"}{p_end}
{phang2}. {stata "global cn : colfullnames S"}{p_end}
{phang2}. {stata "mat KSxxi= I(2)#Sxxi"}{p_end}
{phang2}. {stata "mat V2 = KSxxi*S*KSxxi*1/r(N)"}{p_end}
{phang2}. {stata "mat colnames V2=$cn"}{p_end}
{phang2}. {stata "mat rownames V2=$cn"}{p_end}
{phang2}. {stata "mat list V1"}{p_end}
{phang2}. {stata "mat list V2"}{p_end}

{pstd}Replicate VCV for suest: 2 eqns, different regressors, robust (note small-sample correction){p_end}

{phang2}. {stata "reg y1 x1"}{p_end}
{phang2}. {stata "est store eq_1"}{p_end}
{phang2}. {stata "reg y2 x2"}{p_end}
{phang2}. {stata "est store eq_2"}{p_end}
{phang2}. {stata "qui suest eq_1 eq_2"}{p_end}
{phang2}. {stata "mat V1 = e(V)"}{p_end}
{phang2}. {stata "mat V1a = V1[1..2,1..2]"}{p_end}
{phang2}. {stata "mat V1b = V1[4..5,1..2]"}{p_end}
{phang2}. {stata "mat V1c = V1[4..5,4..5]"}{p_end}
{phang2}. {stata "mat V1 = (V1a, V1b') \ (V1b, V1c)"}{p_end}
{phang2}. {stata "avar (es1 es2) (x1 x2), rob"}{p_end}
{phang2}. {stata "mat S=r(S)"}{p_end}
{phang2}. {stata "mat Sa=S[1..1,1..6]"}{p_end}
{phang2}. {stata "mat Sb=S[3..3,1..6]"}{p_end}
{phang2}. {stata "mat Sc=S[5..5,1..6]"}{p_end}
{phang2}. {stata "mat Sd=S[6..6,1..6]"}{p_end}
{phang2}. {stata "mat S = Sa \ Sb \ Sc \ Sd"}{p_end}
{phang2}. {stata "mat Sa=S[1..4,1..1]"}{p_end}
{phang2}. {stata "mat Sb=S[1..4,3..3]"}{p_end}
{phang2}. {stata "mat Sc=S[1..4,5..5]"}{p_end}
{phang2}. {stata "mat Sd=S[1..4,6..6]"}{p_end}
{phang2}. {stata "mat S = Sa , Sb , Sc , Sd"}{p_end}
{phang2}. {stata "mat accum XX1=x1"}{p_end}
{phang2}. {stata "mat Sxx1=XX1*1/r(N)"}{p_end}
{phang2}. {stata "mat Sxx1i=syminv(Sxx1)"}{p_end}
{phang2}. {stata "mat accum XX2=x2"}{p_end}
{phang2}. {stata "mat Sxx2=XX2*1/r(N)"}{p_end}
{phang2}. {stata "mat Sxx2i=syminv(Sxx2)"}{p_end}
{phang2}. {stata "mat KSxxi= (Sxx1i, J(2,2,0)) \ (J(2,2,0), Sxx2i)"}{p_end}
{phang2}. {stata "mat V2 = KSxxi*S*KSxxi*1/r(N)"}{p_end}
{phang2}. {stata "mat V2 = V2 * e(N)/(e(N)-1)"}{p_end}
{phang2}. {stata "global cn : colfullnames S"}{p_end}
{phang2}. {stata "mat colnames V2=$cn"}{p_end}
{phang2}. {stata "mat rownames V2=$cn"}{p_end}
{phang2}. {stata "mat list V1"}{p_end}
{phang2}. {stata "mat list V2"}{p_end}

{pstd}Replicate VCV for suest: 2 eqns, exactly-identified, cluster-robust (note small-sample correction){p_end}

{phang2}. {stata "reg y1 x1 x2"}{p_end}
{phang2}. {stata "est store eq_1"}{p_end}
{phang2}. {stata "reg y2 x1 x2"}{p_end}
{phang2}. {stata "est store eq_2"}{p_end}
{phang2}. {stata "suest eq_1 eq_2, cluster(id2)"}{p_end}
{phang2}. {stata "mat V1 = e(V)"}{p_end}
{phang2}. {stata "mat V1a = V1[1..3,1..3]"}{p_end}
{phang2}. {stata "mat V1b = V1[5..7,1..3]"}{p_end}
{phang2}. {stata "mat V1c = V1[5..7,5..7]"}{p_end}
{phang2}. {stata "mat V1 = (V1a, V1b') \ (V1b, V1c)"}{p_end}
{phang2}. {stata "mat accum XX=x1 x2"}{p_end}
{phang2}. {stata "mat Sxx=XX*1/r(N)"}{p_end}
{phang2}. {stata "mat Sxxi=syminv(Sxx)"}{p_end}
{phang2}. {stata "avar (e1 e2) (x1 x2), rob cluster(id2)"}{p_end}
{phang2}. {stata "mat S=r(S)"}{p_end}
{phang2}. {stata "global cn : colfullnames S"}{p_end}
{phang2}. {stata "mat KSxxi= I(2)#Sxxi"}{p_end}
{phang2}. {stata "mat V2 = KSxxi*S*KSxxi*1/r(N)"}{p_end}
{phang2}. {stata "mat V2 = V2 * e(N_clust)/(e(N_clust)-1)"}{p_end}
{phang2}. {stata "mat colnames V2=$cn"}{p_end}
{phang2}. {stata "mat rownames V2=$cn"}{p_end}
{phang2}. {stata "mat list V1"}{p_end}
{phang2}. {stata "mat list V2"}{p_end}

{pstd}Replicate VCV for efficient IV with iid error (note small-sample correction){p_end}

{phang2}. {stata "ivreg y1 (x1 x2 = z1 z2)"}{p_end}
{phang2}. {stata "mat V1 = e(V)"}{p_end}
{phang2}. {stata "mat accum ZZ=z1 z2"}{p_end}
{phang2}. {stata "mat Szz=ZZ*1/r(N)"}{p_end}
{phang2}. {stata "mat Szzi=syminv(Szz)"}{p_end}
{phang2}. {stata "mat accum A=x1 x2 z1 z2"}{p_end}
{phang2}. {stata "mat A1 = A[3..5,1..2]"}{p_end}
{phang2}. {stata "mat A2 = A[3..5,5..5]"}{p_end}
{phang2}. {stata "mat Szx = (A1 , A2)*1/r(N)"}{p_end}
{phang2}. {stata "avar eiv1 (z1 z2)"}{p_end}
{phang2}. {stata "mat S=r(S)"}{p_end}
{phang2}. {stata "mat Si=syminv(S)"}{p_end}
{phang2}. {stata "mat V2 = syminv(Szx'*Si*Szx)*1/r(N)"}{p_end}
{phang2}. {stata "mat V2 = V2 * e(N)/e(df_r)"}{p_end}
{phang2}. {stata "mat list V1"}{p_end}
{phang2}. {stata "mat list V2"}{p_end}

{pstd}Replicate VCV for inefficient IV with robust SEs (note small-sample correction){p_end}

{phang2}. {stata "ivreg y1 (x1 x2 = z1 z2), rob"}{p_end}
{phang2}. {stata "mat V1 = e(V)"}{p_end}
{phang2}. {stata "mat accum ZZ=z1 z2"}{p_end}
{phang2}. {stata "mat Szz=ZZ*1/r(N)"}{p_end}
{phang2}. {stata "mat Szzi=syminv(Szz)"}{p_end}
{phang2}. {stata "mat accum A=x1 x2 z1 z2"}{p_end}
{phang2}. {stata "mat A1 = A[3..5,1..2]"}{p_end}
{phang2}. {stata "mat A2 = A[3..5,5..5]"}{p_end}
{phang2}. {stata "mat Szx = (A1 , A2)*1/r(N)"}{p_end}
{phang2}. {stata "avar eiv1 (z1 z2), rob"}{p_end}
{phang2}. {stata "mat S=r(S)"}{p_end}
{phang2}. {stata "mat Si=syminv(S)"}{p_end}
{phang2}. {stata "mat V2 = syminv(Szx'*Szzi*Szx)*(Szx'*Szzi*S*Szzi*Szx)*syminv(Szx'*Szzi*Szx)*1/r(N)"}{p_end}
{phang2}. {stata "mat V2 = V2 * e(N)/e(df_r)"}{p_end}
{phang2}. {stata "mat list V1"}{p_end}
{phang2}. {stata "mat list V2"}{p_end}


{marker saved_results}{...}
{title:Saved results}

{pstd}
{cmd:avar} saves the following in {cmd:r()}:

{synoptset 15 tabbed}{...}
{p2col 5 15 19 2: Scalars}{p_end}
{synopt:{cmd:r(N)}}sample size{p_end}

{synoptset 15 tabbed}{...}
{p2col 5 15 19 2: Matrices}{p_end}
{synopt:{cmd:r(S)}}estimate of the asymptotic variance{p_end}

{synoptset 15 tabbed}{...}
{p2col 5 15 19 2: Mata}{p_end}
{synopt:{cmd:mname}}(optional) estimate of the asymptotic variance{p_end}

{p2colreset}{...}


{marker references}{...}
{title:References}

{phang}
Driscoll, J.C. and Kraay, A. 1998. Consistent Covariance Matrix Estimation With Spatially Dependent Panel Data.
Review of Economics and Statistics. Vol. 80, No. 4, pp. 549-560.
{p_end}

{phang}
Hayashi, F. Econometrics. 2000. Princeton: Princeton University Press.
{p_end}

{phang}
Kiefer, N.M.  1980.  Estimation of Fixed Effect Models for Time Series of Cross-Sections with
Arbitrary Intertemporal Covariance.  Journal of Econometrics, Vol. 14, No. 2, pp. 195-202.
{p_end}

{phang}
Hall, A.R. Generalized Method of Moments.  2005.  Oxford: Oxford University Press.
{p_end}

{marker citation}{...}
{title:Citation of avar}

{pstd}{opt avar} is not an official Stata command. It is a free contribution
to the research community, like a paper. Please cite it as such: {p_end}

{phang}Baum, C.F., Schaffer, M.E. 2013.
avar: Asymptotic covariance estimation for iid and non-iid data
robust to heteroskedasticity, autocorrelation, 1- and 2-way clustering,
common cross-panel autocorrelated disturbances, etc.
{browse "http://ideas.repec.org/c/boc/bocode/XXX.html":http://ideas.repec.org/c/boc/bocode/XXX.html}{p_end}


{title:Authors}
	
	Christopher F Baum, Boston College, USA
	baum@bc.edu

	Mark E Schaffer, Heriot-Watt University, UK
	m.e.schaffer@hw.ac.uk


{title:Also see}

{p 7 14 2}
Help:  {helpb ivreg2}{p_end}

