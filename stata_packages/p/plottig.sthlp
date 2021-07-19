{smcl}
{* 19feb2019}{...}
{hline}
help for {hi:scheme_plottig}{right:(Version 1.3.0)}
{hline}

{title:Title}
Scheme description: plottig & plottigblind


{* index schemes}{...}
{title:Scheme description:  plottig and plottigblind}

{p 4 4 2}
The plottig scheme family is: 

	{it:schemename}{col 25}Foreground{col 38}Background{col 50}description
	{hline 80}
	{cmd:plottig}{…}
{col 25}color{...}
{col 38}white{...}
{col 50}colors on gray
	{cmd:plottigblind}{…}
{col 25}color{...}
{col 38}white{...}
{col 50}colorblind friendly colors on gray
	{hline 80}


{title:Syntax}

{p 4 4 2}
For instance, you might type

{p 8 16 2}
{cmd:. graph}
...{cmd:,}
...
{cmd:scheme(plottig)}

{p 8 16 2}
{cmd:. set}
{cmd:scheme}
{cmd: plottig}
[{cmd:,}
{cmdab:perm:anently}
]

{p 4 4 2}
See help {help scheme_option} and help {help set_scheme}.


{title:Description}

{p 4 4 2}
Schemes determine the overall look of a graph; see help {help schemes}.

{p 4 4 2}
The scheme {cmd:plottig} uses 14 new colors and a gray background. It heavily relies upon the insights from Cleveland, 1985 and on the ggplot scheme for the statistical software R.  

{p 4 4 2}
The scheme {cmd:plottigblind} relies upon the same design as {cmd:plottig} but instead uses 7 colors which are distinguishable for colorblind people and have been introduced in {cmd:plotplainblind}.


{title:Remarks}

{p 4 4 2}
The schemes {cmd:plottig} and {cmd:plottigblind} have a gray background tinting; y-axis labels are horizontal; gridlines are white; gridlines are drawn for the x- and y-axis; gridlines are medium; x- and y-axis are not plotted; markers are medium; lines are medium; fonts of labels are medium; legends are not framed; legends appear on the right hand side of the figure; keylabels of legends are medium large; legends rely on rows first; intensity of pie- and bar figures is reduced to 50; borderlines of histograms and bar are thin; marker symbols are reordered to assure that points in scatters are less often overlapping.  

{p 4 4 2}
On top of that, the scheme {cmd:plottigblind} introduces 14 new colors to stata. Inspiration for the colors comes from {browse "http://colorbrewer.org/"} and the ggplot package. Thus, they may be used like any other color in stata. E.g.: 

{p 8 16 2}
{cmd:. line}
{cmd:x}
{cmd:y}
{cmd:,}
{cmdab:lcolor(plr1)}
 

{p 4 4 2}
The 14 colors are: 

{p 8 16 2}
{cmd:plr1}
{cmd:plr2}
{cmd:plb1}
{cmd:plb2}
{cmd:plb3}
{cmd:plg1}
{cmd:plg2}
{cmd:plg3}
{cmd:pll1}
{cmd:pll2}
{cmd:pll3}
{cmd:ply1}
{cmd:ply2}
{cmd:ply3}

{p 4 4 2}
More details on the schemes may be found in the working paper indicated below.


{title:References}

{p 4 8 2}
Bischof, D. 2015. Figure Schemes for Decent Stata Figures: plottig & plottig. {browse "http://danbischof.wordpress.com/publications/"}

{p 4 8 2}
Okabe, M. & Kei Ito. Color Universal Design (CUD) - How to make figures and presentations that are friendly to Colorblind people. {browse "http://jfly.iam.u-tokyo.ac.jp/color/"}

{p 4 8 2}
Cleveland, William S. 1985. The Elements of Graphing Data. Monterey: Wadsworth Advanced Books and Software.

{title:Author}

{p 4 4 2}
{browse „bischof@ipz.uzh.ch“:Daniel Bischof}, Department of Political Science,
University of Zurich, CH.


{title:Also see}

{p 4 14 2}
Online:  help for {help schemes}; {it:{help scheme_option}}, {help set_scheme}
{p_end}


