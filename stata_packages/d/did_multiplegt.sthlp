{smcl}
{* *! version 1  2018-07-27}{...}
{viewerjumpto "Syntax" "did_multiplegt##syntax"}{...}
{viewerjumpto "Description" "did_multiplegt##description"}{...}
{viewerjumpto "Options" "did_multiplegt##options"}{...}
{viewerjumpto "Examples" "did_multiplegt##examples"}{...}
{viewerjumpto "Saved results" "did_multiplegt##saved_results"}{...}

{title:Title}

{p 4 8}{cmd:did_multiplegt} {hline 2} Estimation in Difference-in-Difference (DID) designs with multiple groups and periods.{p_end}

{marker syntax}{...}
{title:Syntax}

{p 4 8}{cmd:did_multiplegt Y G T D} {ifin} 
[{cmd:,}
{cmd:robust_dynamic} 
{cmd:dynamic(}{it:#}{cmd:)}
{cmd:{ul:av}erage_effect}
{cmd:placebo(}{it:#}{cmd:)}
{cmd:{ul:long}diff_placebo}
{cmd:{ul:joint}testplacebo}
{cmd:controls(}{it:varlist}{cmd:)} 
{cmd:trends_nonparam(}{it:varlist}{cmd:)} 
{cmd:trends_lin(}{it:varlist}{cmd:)}
{cmd:count_switchers_contr}
{cmd:{ul:reca}t_treatment(}{it:varlist}{cmd:)}
{cmd:{ul:thresh}old_stable_treatment(}{it:#}{cmd:)}
{cmd:weight(}{it:varlist}{cmd:)}
{cmd:switchers(}{it:string}{cmd:)}
{cmd:if_first_diff(}{it:string}{cmd:)}
{cmd:count_switchers_tot}
{cmd:discount(}{it:#}{cmd:)}
{cmd:breps(}{it:#}{cmd:)}
{cmd:cluster(}{it:varname}{cmd:)}
{cmd:covariances} 
{cmd:seed(}{it:#}{cmd:)}
{cmd:graphoptions(}{it:string}{cmd:)}
{cmd:{ul:sav}e_results(}{it:path}{cmd:)}]{p_end}

{synoptset 28 tabbed}{...}

{marker description}{...}
{title:Description}

{p 4 4}{cmd:did_multiplegt} estimates the effect of a treatment on an outcome, using group- (e.g. county- or state-) level panel data with multiple groups and periods. 
The panel of groups may be unbalanced: not all groups have to be observed at every period (see FAQ section for more info on that). The treatment need not be 
binary.{p_end}

{p 4 4} If the {cmd:robust_dynamic} option is not specified, 
the command computes the DID_M estimator introduced in de Chaisemartin and D'Haultfoeuille (2020a), 
which can be used in sharp designs where the treatment varies at the group*time level. DID_M is a weighted average, across time periods t and treatment values d, 
of DID estimators comparing the t-1 to t outcome evolution, 
in groups whose treatment changes from d to some other value from t-1 to t, and in groups whose treatment is equal to d at both dates.
DID_M estimates the effect of the current treatment on the outcome, in groups whose treatment switches. If there are no dynamic effects,
meaning that past treatments do not affect the current outcome, DID_M is unbiased even if the treatment effect is heterogeneous across groups or over time. 
If there are dynamic effects, DID_M may be biased, except if the treatment is binary and 
the design is staggered, meaning that groups can switch in but not out of the treatment.{p_end}

{p 4 4}If the {cmd:robust_dynamic} option is specified, the command computes the DID_l estimators introduced in de Chaisemartin and D'Haultfoeuille (2020b),
which can be used in sharp designs, and in some fuzzy designs where the treatment varies within group*time, see de Chaisemartin and D'Haultfoeuille (2020b) for further details. 
DID_l is a weighted average, across time periods t and possible values of the treatment d, of DIDs comparing the t-l-1 to t outcome evolution, 
in groups with a treatment equal to d at the start of the panel and whose treatment changed for the first time in t-l, the first-time switchers, 
and in groups with a treatment equal to d from period 1 to t, the not-yet switchers.
DID_l estimates the effect of having switched treatment for the first time l periods ago. The DID_l estimators are unbiased under heterogeneous and dynamic effects.
DID_0 is always computed by the command, and if the {cmd:dynamic(}{it:L}{cmd:)} option is specified, the DID_l estimators are also computed for l in {1,...,L}.{p_end}

{p 4 4}The command can also compute placebo estimators, that can be used to test the non-anticipation, strong exogeneity, and parallel trends assumptions underlying the DID_M and DID_l estimators.{p_end}

{p 4 8}{cmd:Y} is the outcome variable.{p_end}

{p 4 8}{cmd:G} is the group variable.{p_end}

{p 4 8}{cmd:T} is the time period variable. The command assumes that the time variable is evenly spaced (e.g.: the panel is at the yearly level, and no year is missing for all groups). When it is not (e.g.: the panel is at the yearly level, but three consecutive years are missing for all groups), the command may still be used, 
though it may require a bit of tweaking. In such instances, contacting the command's authors may be advisable.{p_end}

{p 4 8}{cmd:D} is the treatment variable.

{marker options}{...}
{title:Options}

{p 4 8}{cmd:robust_dynamic}: if that option is not specified, the estimators in de Chaisemartin and D'Haultfoeuille (2020a) are computed. 
If it is specified, the estimators in de Chaisemartin and D'Haultfoeuille (2020b) are computed. {p_end}

{p 4 8}{cmd:dynamic(}{it:#}{cmd:)} gives the number of dynamic treatment effects to be estimated. This option can only be used when the {cmd:robust_dynamic} option is specified. 
DID_l, the estimator of the lth dynamic effect, compares first-time switchers' and not-yet switchers' outcome evolution, from the last period before first-time switchers' treatment changes to the lth period after that change. 
With a balanced panel of groups, the maximum number of dynamic effects one can estimate can be determined as follows. For each value of the treatment d, start by
computing the difference between the last period at which at least one group has had treatment d since period 1, and the first period at which a group with treatment d at period 1 changed 
its treatment. Then, the maximum number of dynamic effects is equal to the maximum of those values, across all possible values of the treatment. 
With an unbalanced panel of groups (e.g.: counties appear or disappear over time if the data is a county-level panel), this method can still be used
to derive an upper bound of the maximum number of dynamic effects one can estimate. See de Chaisemartin and D'Haultfoeuille (2020b) for further details.
{p_end}

{p 4 8}{cmd:average_effect}: when the {cmd:robust_dynamic} option is specified, DID_l estimates 
the effect of having switched treatment for the first time l periods ago. In a staggered design with a binary treatment, groups that have switched treatment l periods ago have gone from untreated to treated,
and have remained treated thereafter. Accordingly, DID_l estimates the cumulative effect of having been treated for l+1 periods.
Outside of staggered designs, some initially untreated groups may switch to being treated and may remain treated thereafter, 
other groups may revert to being untreated just after that first switch, other groups
may alternate between treated and untreated, etc. Then, DID_l cannot be directly converted into an effect per unit of treatment received. If the {cmd:average_effect} option is specified, 
the command computes an estimator of the change in outcome created by a one-unit change in treatment. This estimator is constructed in three steps. First, the command computes a weighted average of the DID_l estimators,
giving to each estimator a weight proportional to the number of switchers DID_l applies to. Second, the command computes a weighted average of estimators similar to the DID_l, except that the outcome variable is replaced by the treatment.
This weighted average estimates the effect of first switches on the treatments that units receive after their first switch. Finally, the command computes the ratio of these
two estimators. This ratio estimates the ``intention-to-treat'' effect of first switches on the outcome, and scales it by the ``first-stage'' effect of first switches on the treatments received thereafter. Accordingly, 
it estimates some average of the change in outcome created by a one-unit change in treatment. See de Chaisemartin and D'Haultfoeuille (2020b) for further details. 
When {cmd:average_effect} is specified, the number of dynamic effects requested should be greater than or equal to 1.{p_end}

{p 4 8}{cmd:placebo(}{it:#}{cmd:)} gives the number of placebo estimators to be estimated. When the {cmd:robust_dynamic} option is not specified, the lth placebo compares switchers' and non switchers' 
outcome evolution from the l+1th to the lth period before switchers' treatment changes, in the sample of groups whose treatment does not change from the l+1th to the last period before the switch. 
When the {cmd:robust_dynamic} option is specified, placebo estimators compare first-time switchers' and not-yet switchers' outcome evolution, before first-time switchers' treatment changes. 
The exact comparisons made are described in the discussion of the {cmd:longdiff_placebo} option below. The number of placebos requested can be at most equal to the number of time periods in the data minus 2, though most
often only a smaller number of placebos can be computed. See de Chaisemartin and D'Haultfoeuille (2020b) for further details.{p_end}

{p 4 8}{cmd:longdiff_placebo}: this option can be used when the {cmd:robust_dynamic} option is specified. 
When this option is specified, the lth placebo compares first-time switchers' and not-yet switchers' outcome evolution, from the last period before first-time switchers' treatment changes to the l+1th period before that change
(this comparison goes from the future towards the past, to be consistent with event-study regressions where everything is relative to the period prior to the treatment change). Thus, the lth placebo assesses if parallel trends
holds over l+1 periods, the number of periods over which parallel trends has to hold for the lth dynamic effect to be unbiased. 
When this option is not specified, the lth placebo compares first-time switchers' and not-yet switchers' outcome evolution, from the l+1th to the lth period before first-time switchers' treatment changes. 
Thus, the lth placebo assesses if parallel trends holds over 2 consecutive periods, l periods before switchers switch. Such first-difference placebos may be useful to test the no-anticipation assumption. 
If parallel trends holds but the treatment, say, one period ahead has an effect on the current outcome, one should find that the first first-difference placebo differs from 0, but others do not. 
See de Chaisemartin and D'Haultfoeuille (2020b) for further details. 
When the {cmd:longdiff_placebo} and {cmd:dynamic} options are requested, the number of placebos requested cannot be larger than the number of dynamic effects.{p_end}

{p 4 8}{cmd:jointtestplacebo}: when this option is specified, 
the command computes the p-value of a joint test that all the placebos requested are equal to 0, and stores it in {cmd:e()}.
This option can be used when the computation of strictly more than one placebo is requested, 
and when the {cmd:covariances} and {cmd:breps(}{it:#}{cmd:)} options are specified.{p_end}

{p 4 8}{cmd:controls(}{it:varlist}{cmd:)} gives the names of all the control variables to be included in the estimation. 
The DID_M and DID_l estimators with controls are similar to those without controls, except that the first-difference of the outcome is replaced by residuals from regressions 
of the first-difference of the outcome on the first-differences of the controls and time fixed effects. Those regressions are estimated in the sample of control (g,t)s: (g,t)s 
such that group g's treatment does not change from t-1 to t for the DID_M estimator, and (g,t)s such that group g's treatment has not changed till period t for the DID_l estimators. 
Those regressions are also estimated separately for each possible value of the treatment, or for each set of treatment values binned together if the {cmd:recat_treatment(}{it:varlist}{cmd:)} option
is used. Estimators with controls are unbiased even if groups experience differential trends,
provided such differential trends can be fully explained by a linear model in covariates changes. See de Chaisemartin and D'Haultfoeuille (2020b) for further details.
When this option is specified, the {cmd:reghdfe}, {cmd:ftools}, and {cmd:moremata} packages have to be installed.{p_end}

{p 4 8}{cmd:trends_nonparam(}{it:varlist}{cmd:)}: when this option is specified, the DID_M (resp. DID_l) estimator is a weighted average of DIDs comparing 
switchers and non switchers (resp. first-time switchers and not yet switchers) with the same value of {it:varlist}.
Estimators with the {cmd:trends_nonparam(}{it:varlist}{cmd:)} option are unbiased even if groups experience differential trends,
provided all groups with the same value of {it:varlist} experience parallel trends. {it:varlist} can only include one categorical variable, and that variable must be strictly coarser than the group variable. 
For instance, if one works with a county*year data set and one wants to allow for state-specific trends, then one should write {cmd:trends_nonparam(}state{cmd:)}, 
where state is the state identifier. When this option is specified, the {cmd:reghdfe}, {cmd:ftools}, and {cmd:moremata} packages have to be installed.{p_end}

{p 4 8}{cmd:trends_lin(}{it:varlist}{cmd:)}: when this option is specified, fixed effects for each value of {it:varlist} are included as controls when residualizing the first-difference 
of the outcome. This 
is equivalent to allowing for {it:varlist}-specific linear trends. 
Estimators with the {cmd:trends_lin(}{it:varlist}{cmd:)} option are unbiased even if the outcome evolution in each group follows its own linear trend, provided that between each pair of 
consecutive periods, all groups' potential outcomes experience the same deviation from their {it:varlist}-specific linear trends. 
{it:varlist} can only include one categorical variable, and that variable must be weakly coarser than the group variable. 
For instance, if one works with a village*year data set and one wants to allow for village-specific linear trends, one should write {cmd:trends_lin(}village{cmd:)}, 
where village is the village identifier. When this option is specified, the {cmd:reghdfe}, {cmd:ftools}, and {cmd:moremata} packages have to be installed.{p_end}

{p 4 8}{cmd:count_switchers_contr}: when this option is specified, the command counts the number of switchers for which counterfactual trends are estimated accounting for the 
controls requested in {cmd:trends_nonparam(}{it:varlist}{cmd:)} or {cmd:trends_lin(}{it:varlist}{cmd:)}. 
Assume one works with a county*year data set, to study the effect of a treatment taking values in {0,1,...10}. 
Then, assume that one wants to compute the DID_M estimator, allowing for state-specific trends. 
There may be a county going from, say, 8 to 9 units of treatment between t-1 and t, while no other county in the same state receives 8 units of treatment at both dates. 
Then, one cannot estimate that county's counterfactual trend controlling for state-specific trends. 
Instead, the command uses counties from other states receiving 8 units of treatment at both dates to estimate that county's counterfectual trend.
Similarly, assume one would like to allow for county-specific linear trends, but that same county is never observed with 8 units of treatment
at two counsecutive dates. Then, the linear trend of that county's outcome with 8 units of treatment cannot be estimated. 
Instead, to estimate that county's linear trend
the command uses the average linear trend across all counties observed with 8 units of treatment at at least two counsecutive dates.   
The {cmd:count_switchers_contr} option allows one to know for how many switchers counterfactual trends cannot be estimated accounting for the 
controls requested in {cmd:trends_nonparam(}{it:varlist}{cmd:)} or {cmd:trends_lin(}{it:varlist}{cmd:)} and have to be estimated differently. When {cmd:count_switchers_contr} is specified, 
{cmd:trends_nonparam(}{it:varlist}{cmd:)} or {cmd:trends_lin(}{it:varlist}{cmd:)} should also be specified.{p_end}

{p 4 8}{cmd:recat_treatment(}{it:varlist}{cmd:)} bins some values of the treatment together when determining the groups whose outcome evolution are compared. 
This option may be useful when the treatment takes many values, or some values are rare in the sample. 
For instance, assume that treatment D takes the values 0, 1, 2, 3, and 4. One would like to compute the DID_M estimator, but few observations have a treatment equal to 2. 
Then, there may be a pair of consecutive time periods where one group goes from 2 to 3 units 
of treatment but no group has a treatment equal to 2 at both dates. To avoid losing that observation, one can create a variable D_recat 
that takes the same value when D=1 or 2 (e.g.: D_recat=(D>=1)+(D>=3)+(D>=4)), 
and then specify the {cmd:recat_treatment(}D_recat{cmd:)} option. 
Then, the command can also use groups with a treatment equal to 1 at two consecutive dates as controls for groups going from 2 to 3 units of treatment, 
thus making it more likely that all switchers have a non empty set of controls. 
When the {cmd:recat_treatment(}{it:varlist}{cmd:)} option is specified, the DID_M and DID_l estimators rely on the assumption that within the sets of treatment values grouped
together, the treatment effect is constant over time (but it can still vary between groups). In the example, the effect of going from 1 to 2 units of treatment should remain constant over time, while the effect of going from 
1 or 2 to 3 or 4 units of treatment can vary over time.{p_end}

{p 4 8}{cmd:threshold_stable_treatment(}{it:#}{cmd:)}: this option may be useful when the treatment is continuous, or takes a large 
number of values. For instance, the DID_M estimator uses as controls groups whose treatment does not change between consecutive time periods. 
With a continuous treatment, there may not be any pair of consecutive time periods between which the treatment of at least one group 
remains perfectly stable. For instance, if the treatment is rainfall and one uses a county*year data set, there is probably not a 
single county*year whose rainfall is exactly the same as in the same county in the previous year. 
Then, one needs to specify the {cmd:threshold_stable_treatment(}{it:#}{cmd:)} option, with {it:#} a positive real number. 
For each pair of consecutive time periods, the command will use counties whose rainfall changed in absolute value by less than {it:#} as 
controls. {it:#} should be large enough so that there are counties whose rainfall levels change by less than {it:#} between two consecutive years, 
but it should be small enough so that a change in rainfall of {it:#} would be unlikely to affect the outcome.{p_end}

{p 4 8}{cmd:weight(}{it:varlist}{cmd:)} gives the name of a variable to be used to weight the data. For instance, if one works with a district*year data set 
and one wants to weight the estimation by each district*year's population, one should write {cmd:weight(}population{cmd:)}, where population is the population in each district*year.{p_end}

{p 4 8}{cmd:switchers(}{it:string}{cmd:)}: with a binary treatment, one may be interested in estimating the treatment effect only among switchers or first-time switchers that go from untreated to treated. 
In that case, one should specify the {cmd:switchers(}{it:in}{cmd:)} option. Conversely, one may be interested in estimating the treatment effect only among switchers or first-time switchers that go from treated to untreated. 
In that case, one should specify the {cmd:switchers(}{it:out}{cmd:)} option. With a non-binary treatment and if the {cmd:robust_dynamic} option is not specified, with the {cmd:switchers(}{it:in}{cmd:)} 
(resp. {cmd:switchers(}{it:out}{cmd:)}) option the command estimates the treatment effect among switchers whose treatment increases (resp. decreases) between t-1 and t. 
With a non-binary treatment and if the {cmd:robust_dynamic} option is specified, with the {cmd:switchers(}{it:in}{cmd:)} 
(resp. {cmd:switchers(}{it:out}{cmd:)}) option the command estimates the treatment effect among first-time switchers whose average treatment over the duration of the panel is higher (resp. lower) than if they had never switched.{p_end}

{p 4 8}{cmd:if_first_diff(}{it:string}{cmd:)}: when this option is specified, the command deletes observations that do not meet the condition specified in {it:string}, 
after having aggregated the data at the (g,t) level
and computed all the first- and long-differences of the treatment and of the outcome necessary to estimate the placebos and dynamic effects requested. 
This option may for instance be useful when one estimates
dynamic effects, and one would like all the dynamic effects to apply to the same groups, see the FAQ section below. This option can also be useful when one wants to compute the
DIDM estimator, controlling for other treatments that may change over the panel, as proposed by de Chaisemartin and D'Haultfoeuille (2020c), see the FAQ section below as well.{p_end}

{p 4 8}{cmd:count_switchers_tot}: when this option is specified, the command counts the number of switchers of first-time switchers for which each of the instantaneous and
dynamic effects requested are observed in the data. This number may be larger than the number of switchers of first-time switchers for which each effect can be estimated.
For instance, assume treatment is binary, the data has 10 periods, the {cmd:robust_dynamic} and {cmd:dynamic(}{it:5}{cmd:)} options are specified, and all groups initially untreated have been treated at least once at period 7.
Then, from period 7 onwards there is no not-yet switcher group one can use to estimate the treatment effects of groups that switched from untreated to treated. 
Accordingly, for a group treated for the first time at period 2, the dynamic effect 5 periods after her first switch 
is observed in the data but cannot be estimated.{p_end}

{p 4 8}{cmd:discount(}{it:#}{cmd:)}: this option can be used if one wants to discount the treatment effects and the treatments occurring later in the panel. For instance, if {cmd:discount(}{it:0.95}{cmd:)} is specified, the command
will use a discount factor of 0.95. See de Chaisemartin and D'Haultfoeuille (2020b) for further details.{p_end}

{p 4 8}{cmd:breps(}{it:#}{cmd:)} gives the number of bootstrap replications to be used in the computation of estimators' standard errors. 
If that option is not specified, the command does not compute estimators' standard errors.{p_end}

{p 4 8}{cmd:cluster(}{it:varname}{cmd:)} computes the standard errors of the estimators using a block bootstrap at the {it:varname} level. Only one clustering variable is allowed. A common practice
in DID analysis is to cluster standard errors at the group level. If the {cmd:cluster(}{it:varname}{cmd:)} option is not specified, the bootstrap is still automatically clustered at the group*time level, because the command
aggregates the data at that level before running the bootstrap. Accordingly, clustering should be at a coarser level than the group*time level.{p_end}

{p 4 8}{cmd:covariances}: if this option and the {cmd:breps(}{it:#}{cmd:)} option are specified, the command computes the covariances between all the pairs of instantaneous and dynamic effects requested, 
and between all the pairs of placebos requested. This option can be useful to assess if the DID_l estimators significantly differ across l. 
For instance, assume that one wants to test if the instantaneous effect DID_0 differs from the dynamic effect DID_1. One can specify the {cmd:covariances} option, use the fact that Var(DID_0-DID_1)=V(DID_0)+V(DID_1)-2cov(DID_0,DID_1) to compute the
 standard error of DID_0-DID_1, and finally assess if this difference is significant.{p_end}

{p 4 8}{cmd:seed(}{it:#}{cmd:)} sets the seed to be used in the bootstrap replications, to ensure results can be reproduced.

{p 4 8}{cmd:graphoptions(}{it:string}{cmd:)}: as explained below, when the {cmd:breps(}{it:#}{cmd:)} option is specified, the command produces a graph. 
One can use the {cmd:graphoptions(}{it:string}{cmd:)} option to modify the appearance of that graph. Options requested have to follow the syntax of Stata {cmd:twoway_options}.
Do not use quotation marks for text passed into the arguments of {cmd:twoway_options}. For instance, if you want the title of your graph to be "Graph to convince
skeptical referee", you should type {cmd:graphoptions(}title(Graph to convince skeptical referee){cmd:)}.{p_end}

{p 4 8}{cmd:save_results(}{it:path}{cmd:)}: if this option and the {cmd:breps(}{it:#}{cmd:)} options are specified, the command saves the estimators requested, their standard error, their 95% confidence interval, and the number of observations used in the estimation in a separate data set, at the location specified in {it:path}.{p_end}

{hline}

{marker FAQ}{...}
{title:FAQ}

{p 4 4}{it:When does the command produce a table, and what does the table contain?}

{p 4 4}If the option breps has been specified, the command returns a table with all the estimated treatment effects and placebos, 
their standard errors, their 95% confidence intervals, the number of observations used in the estimation, 
and the number of switchers the effects and placebos apply to. The average effect only appears in the table 
if the {cmd:covariances} option is specified. The p-value from the joint test that all placebos are equal to 0 is not shown in the table, 
but it is stored in e().{p_end}

{p 4 4}{it:When does the command produce a graph, and how is the graph constructed?}

{p 4 4}If the option breps has been specified, the command returns a graph with all the estimated treatment effects and placebos, 
and their 95% confidence intervals constructed using a normal approximation. 
An exception is when dynamic effects and first-difference placebos are requested. 
Then, the command does not produce a graph, because placebos are first-difference estimators, while dynamic effects are long-difference estimators, 
so they are not really 
comparable. When dynamic effects and long-difference placebos are requested, everything is relative to the period prior to first-switches, referred to as period -1. 
Accordingly, the first placebo is shown at period -2 on the graph.{p_end}

{p 4 4} {it:My group-level panel is unbalanced: some groups (e.g. counties) are not observed in every year. Can I still use the command?} 

{p 4 4} You can. A frequent case of unbalancendess is when some groups are not observed over the full duration of the panel, 
but all groups are observed at evenly-spaced intervals over the period where they are observed. 
For instance, your data may be a yearly county-level panel from 1990 to 2000, 
where some counties appear after 1990 while some exit before 2000, but from the
year they appear till the year they exit, all counties are observed every year.
Then, there is nothing special you have to do, you can run the command as is.  

{p 4 4} A more complicated case is when some groups are observed at unevenly spaced intervals. 
For instance, a county is observed in 1990, 1991, 1993, 1994, etc. If you wish to compute the DID_M estimator, there is
nothing special you have to do: that estimator only relies on first-differences, so it will just not be able 
to use the 1993 observation for that county. If you wish to compute the DID_l estimators, the years where that county is observed will
be used in the estimation, 
except if that county's 1991 and 1993 treatments differ, and that county had never changed treatment prior to 1991. 
Then, the year when that county's treatment changed for the first time is not known, 
so the conservative approach implemented by default is to drop it from the
estimation. If many groups are observed at unevenly spaced intervals, this approach may strongly reduce the sample size. 
In that case, you may consider filling the group-level panel's holes, using Stata's fillin command. Then, you need
to replace a group's missing treatments by the value of that same group's treatment, in the first year after those missing years 
where the treatment is not missing. In the example above, this amounts to replacing the county's 1992 treatment by its 1993 value.
Then, that county will be included in the estimation. However, the estimation will assume that the 
county's treatment changed for the first time from 1991 to 1992, and not from 1992 to 1993. 
Accordingly, the county's estimated effect in 1993 will be considered as a dynamic effect 1 period after the first treatment change, 
while it may be an instantaneous effect. 
Without that assumption, that group cannot be used in the computation of the DID_l estimators, which all require that a group's outcome
be observed in the last year before its treatment changes.

{p 4 4} Finally, it may be the case that the data is fully missing at one or several time periods. 
For instance, you have data for 1990, 1991, and 1993, but 1992 is missing for every group. If you wish to compute the DID_l estimators, it is important to fill the gap in the data,
as otherwise the estimation will assume that 1991 and 1993 are as far apart as 1990 and 1991. There are two ways of doing so.
First, you can append to your data a data set identical to your 1991 data, but with the year equal to 1992, 
and the outcome missing for every observation. This is a conservative solution, where no
first treatment change occurring between 1991 and 1993 will be used in the estimation, which may be reasonable because the year in which the change occurred is 
effectively unknown. 
Second, you can append to your data a data set identical to your 1993 data, with the year equal to 1992, 
and the outcome missing for every observation. 
Then, the first treatment changes occurring between 1991 and 1993 will be used in the estimation, assuming they all took place between 1991 and 1992.  

{p 4 4} {it: Can I make sure that all the dynamic effects I estimate apply to the same groups?} 

{p 4 4} With a binary treatment and a balanced panel of groups such that at least one group
remains untreated and at least one group remains treated throughout the panel, this can be achieved as follows. Assume one estimates two dynamic effects. Then, let {it:Ylead2} 
denote the lead of order 2 of the outcome variable. If one writes {cmd:if_first_diff(}{it:Ylead2!=.}{cmd:)} in the command's options, groups for which only the instantaneous
or first dynamic effect can be estimated are discarded, and the instantaneous effect and the first two dynamic
effects apply to the same groups. If the number of observations per group does not change over time, the number of observations these three effects apply to will also be the same.

{p 4 4} {it: The number of observations per group (e.g.: counties' populations) is changing over time, and differentially so across groups (e.g.: some counties' populations grow more than others). Accordingly, groups whose number of observations grow more receive more weight in the estimates of long- than short-run effects. Can I avoid such compositional changes?} 

{p 4 4} Yes you can. If your data is not aggregated at the group*period level, start by aggregating the outcome treatment and control variables at that level, and create a variable equal to the number of observations
in each group*period. Then create a variable time_invariant_weight equal, for each group*period, to the average number of observations in the group across all periods of the panel. 
Finally, just weight the estimation by time_invariant_weight.{p_end}

{p 4 4} {it: How many control variables can I include in the estimation?}

{p 4 4} The DID_M and DID_l estimators with control variables are similar to those without controls, except that the first-difference of the outcome is replaced by residuals from regressions 
of the first-difference of the outcome on the first-differences of the controls and time fixed effects. Those regressions are estimated in the sample of control (g,t)s: (g,t)s 
such that group g's treatment does not change from t-1 to t for the DID_M estimator, and (g,t)s such that group g's treatment has not changed till period t for the DID_l estimators. 
Those regressions are also estimated separately for each possible value of the treatment, or for each set of treatment values binned together if the {cmd:recat_treatment(}{it:varlist}{cmd:)} option
is used. If the treatment takes values 0, 1, 2, 3, and 4, one regression is estimated 
for control (g,t)s with a treatment equal to 0, one regression is estimated for control (g,t)s with a treatment equal to 1, etc. The number of control variables
needs to be significantly smaller than the number of control (g,t)s in each of those regressions. Otherwise, those regressions will overfit and produce noisy estimates.
If the number of observations is lower than the number of variables in one of those regressions, the command will run but not all the control variables can be accounted for, 
and an error message will let you know about that.{p_end}
 
{p 4 4} {it: Can I estimate the effect of a treatment, controlling for other treatments that may also change over the panel?}

{p 4 4} The command can compute the DIDM estimator, controlling for other treatments that may change over the panel, as proposed by de Chaisemartin and D'Haultfoeuille (2020c).
In that case, let {it:othertreat} be a variable containing the other treatment, and let 
{it:fd_othertreat} be the first difference of that other treatment. To compute the estimator proposed by de Chaisemartin and D'Haultfoeuille (2020c), 
one should write {cmd:if_first_diff(}{it:fd_othertreat==0}{cmd:)} {cmd:trends_nonparam(}{it:othertreat}{cmd:)} in the command's options. 
See de Chaisemartin and D'Haultfoeuille (2020c) for further details.{p_end}

{p 4 4} {it: How can I use the command to estimate heterogeneous treatment effects?} 

{p 4 4} The command can easily be used to investigate treatment effect heterogeneity with respect to group-level time-invariant variables. 
Let X be one such variable, and let us assume X is binary. Then you can just run the command twice, adding first "if X==0" and then "if X==1" at the end, to separately estimate 
the effects in groups with X=0/X=1. Assuming you are clustering your ses at the group level, to test if the effects significantly differ in the two groups 
you can just use the sum of the estimated variances in the X==0 and X==1 subgroups to estimate the variance of the difference between two effects. 
If your data is at a more disaggregated level than groups and X is a time-invariant characteristic also at a more disaggregated level 
(you have X=0 and X=1 observations within the same group), then you can still use the same procedure to estimate heterogeneous effects, but you 
will need to bootstrap the command yourself to estimate the variance of the differences between the effects, if you want your ses to be clustered at the group level.  

{hline}

{marker saved_results}{...}
{title:Saved results}

{p 4 8}In what follows, let {it:k} denote the number specified in the {cmd:placebo(}{it:#}{cmd:)} option, 
and let {it:j} denote the number specified in the {cmd:dynamic(}{it:#}{cmd:)} option. {cmd:did_multiplegt} saves the following in {cmd:e()}:

{p 4 8}{cmd:e(effect_0)}: estimated effect of the treatment at the time period when switchers or first-time switchers switch.{p_end}

{p 4 8}{cmd:e(N_effect_0)}: number of observations used in the estimation of {cmd:e(effect_0)}. This number is the number of first differences of the outcome and of the treatment used in the estimation.{p_end}

{p 4 8}{cmd:e(N_switchers_effect_0)}: number of switchers or first-time switchers {cmd:e(effect_0)} applies to.{p_end}

{p 4 8}{cmd:e(N_switchers_effect_0_tot)}: number of switchers or first-time switchers in the data, if the {cmd:count_switchers_tot} option is specified.{p_end}

{p 4 8}{cmd:e(N_switchers_effect_0_contr)}: number of switchers or first-time switchers whose counterfactual trend at the time of their switch is estimated accounting for the requested controls, if the {cmd:count_switchers_contr} option is specified.{p_end}

{p 4 8}{cmd:e(se_effect_0)}: estimated standard error of {cmd:e(effect_0)}, if the option {cmd:breps(}{it:#}{cmd:)} has been specified.{p_end}

{p 4 8}{cmd:e(effect_l)}: estimated effect l periods after first-time switchers have switched treatment for the first time, for all l in 1, ..., j.{p_end}

{p 4 8}{cmd:e(N_effect_l)}: number of observations used in the estimation of {cmd:e(effect_l)}. This number is the number of long differences of the outcome and of the treatment used in the estimation.{p_end}

{p 4 8}{cmd:e(N_switchers_effect_l)}: number of first-time switchers {cmd:e(effect_l)} applies to.

{p 4 8}{cmd:e(N_switchers_effect_l_tot)}: number of first-time switchers observed l periods after their first switch, if the {cmd:count_switchers_tot} option is specified.{p_end}

{p 4 8}{cmd:e(N_switchers_effect_l_contr)}: number of first-time switchers whose counterfactual trend l periods after their first switch is estimated accounting for the requested controls, if the {cmd:count_switchers_contr} option is specified.{p_end}

{p 4 8}{cmd:e(se_effect_l)}: estimated standard error of {cmd:e(effect_l)}, if the option {cmd:breps(}{it:#}{cmd:)} has been specified.{p_end}

{p 4 8}{cmd:e(placebo_l)}: estimated placebo l, for all l in 0, 1, ..., k.{p_end}

{p 4 8}{cmd:e(N_placebo_l)}: number of observations used in the estimation of {cmd:e(placebo_l)}. This number is the number of first differences of the outcome and of the treatment used in the estimation.{p_end}

{p 4 8}{cmd:e(N_switchers_placebo_l)}: number of switchers or first-time switchers {cmd:e(placebo_l)} applies to.{p_end}

{p 4 8}{cmd:e(N_switchers_placebo_l_tot)}: number of switchers or first-time switchers observed l periods before their first switch, if the {cmd:count_switchers_tot} option is specified.{p_end}

{p 4 8}{cmd:e(N_switchers_placebo_l_contr)}: number of switchers or first-time switchers whose counterfactual trend l periods before their switch is estimated accounting for the requested controls, if the {cmd:count_switchers_contr} option is specified.{p_end}

{p 4 8}{cmd:e(se_placebo_l)}: estimated standard error of {cmd:e(placebo_l)}, if the option {cmd:breps(}{it:#}{cmd:)} has been specified.{p_end}

{p 4 8}{cmd:e(p_jointplacebo)}: p-value of test that all placebos are equal to 0, if more than one placebo requested, and if the option {cmd:jointtestplacebo} has been specified.{p_end}

{p 4 8}{cmd:e(cov_effects_m_l)}: estimated covariance between {cmd:e(effect_m)} and {cmd:e(effect_l)}, for all 0<=m<l<=j, if the options {cmd:covariances(}{it:#}{cmd:)} and {cmd:breps(}{it:#}{cmd:)} have been specified.{p_end}

{p 4 8}{cmd:e(cov_placebo_m_l)}: estimated covariance between {cmd:e(placebo_m)} and {cmd:e(placebo_l)}, for all 1<=m<l<=k, if the options {cmd:covariances(}{it:#}{cmd:)} and {cmd:breps(}{it:#}{cmd:)} have been specified, 
and at least 2 placebos have been requested.{p_end}

{p 4 8}{cmd:e(effect_average)}: average of the {cmd:e(effect_l)}s computed, scaled by the difference between the average number of treatment units received by first-time switchers after their first switch
and the number of treatment units they would have received if they had never switched. Computed only if the {cmd:average_effect} option has been specified.{p_end}

{p 4 8}{cmd:e(N_effect_average)}: number of observations used in the estimation of {cmd:e(effect_average)}. This number is the number of first differences of the outcome and of the treatment used in the estimation.{p_end}

{p 4 8}{cmd:e(N_switchers_effect_average)}: number of first-time switchers {cmd:e(effect_average)} applies to.

{p 4 8}{cmd:e(se_effect_average)}: estimated standard error of {cmd:e(effect_average)}, if the options {cmd:covariances(}{it:#}{cmd:)} and {cmd:breps(}{it:#}{cmd:)} have been specified.

{p 4 8}{cmd:e(estimates)}: column vector containing all the point estimates and placebos requested, if the option {cmd:breps(}{it:#}{cmd:)} has been specified.

{p 4 8}{cmd:e(variances)}: column vector containing the variances of all the point estimates and placebos requested, if the option {cmd:breps(}{it:#}{cmd:)} has been specified.

{p 4 8}{cmd:e(cmd)}: macro equal to "did_multiplegt", the name of the command.{p_end}

{hline}

{marker Example}{...}
{title:Example: estimating the effect of union membership on wages, using the same panel of workers as in Vella and Verbeek (1998)}

{p 4 8}ssc install bcuse{p_end}
{p 4 8}bcuse wagepan{p_end}
{p 4 8}did_multiplegt lwage nr year union, placebo(1) breps(50) cluster(nr){p_end}
{p 4 8}did_multiplegt lwage nr year union, robust_dynamic dynamic(1) placebo(1) breps(50) cluster(nr){p_end}

{hline}

{title:References}

{p 4 8}de Chaisemartin, C and D'Haultfoeuille, X (2020a). American Economic Review, vol. 110, no. 9. 
{browse "https://www.aeaweb.org/articles?id=10.1257/aer.20181169":Two-Way Fixed Effects Estimators with Heterogeneous Treatment Effects}.{p_end}
{p 4 8}de Chaisemartin, C and D'Haultfoeuille, X (2020b).
{browse "https://papers.ssrn.com/sol3/papers.cfm?abstract_id=3731856":Difference-in-Differences Estimators of Intertemporal Treatment Effects}.{p_end}
{p 4 8}de Chaisemartin, C and D'Haultfoeuille, X (2020c).
{browse "https://papers.ssrn.com/sol3/papers.cfm?abstract_id=3751060":Two-way fixed effects regressions with several treatments}.{p_end}
{p 4 8}Vella, F. and Verbeek, M. 1998. Journal of Applied Econometrics 13(2), 163–183. 
{browse "https://onlinelibrary.wiley.com/doi/abs/10.1002/(SICI)1099-1255(199803/04)13:2%3C163::AID-JAE460%3E3.0.CO;2-Y":Whose wages do unions raise? a dynamic model of unionism and wage rate determination for young men}.{p_end}


{title:Authors}

{p 4 8}Clément de Chaisemartin, University of California at Santa Barbara, Santa Barbara, California, USA.
{browse "mailto:clementdechaisemartin@ucsb.edu":clementdechaisemartin@ucsb.edu}.{p_end}
{p 4 8}Xavier D'Haultfoeuille, CREST, Palaiseau, France.
{browse "mailto:xavier.dhaultfoeuille@ensae.fr":xavier.dhaultfoeuille@ensae.fr}.{p_end}

