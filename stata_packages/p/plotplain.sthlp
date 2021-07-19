{smcl}
{* 19feb2019}{...}
{hline}
help for {hi:scheme_plotplain}{right:(Version 1.3.0)}
{hline}

{title:Title}
Scheme description: plotplain & plotplainblind


{* index schemes}{...}
{title:Scheme description:  blind and colorblind}

{p 4 4 2}
The plotplain scheme family is: 

	{it:schemename}{col 25}Foreground{col 38}Background{col 50}description
	{hline 80}
	{cmd:plotplain}{…}
{col 25}black{...}
{col 38}white{...}
{col 50}black on white
	{cmd:plotplainblind}{…}
{col 25}color{...}
{col 38}white{...}
{col 50}colorblind friendly colors on white
	{hline 80}


{title:Syntax}

{p 4 4 2}
For instance, you might type

{p 8 16 2}
{cmd:. graph}
...{cmd:,}
...
{cmd:scheme(plotplain)}

{p 8 16 2}
{cmd:. set}
{cmd:scheme}
{cmd: plotplain}
[{cmd:,}
{cmdab:perm:anently}
]

{p 4 4 2}
See help {help scheme_option} and help {help set_scheme}.


{title:Description}

{p 4 4 2}
Schemes determine the overall look of a graph; see help {help schemes}.

{p 4 4 2}
The scheme {cmd:plotplain} uses shades of gray and shapes of lines to distinguish plotted subgroups in a figure. 

{p 4 4 2}
The scheme {cmd:plotplainblind} relies upon the same design as {cmd:plotplain} but introduces 7 new colors which should also be distinguishable for colorblind 
people. 


{title:Remarks}

{p 4 4 2}
The schemes {cmd:plotplain} and {cmd:plotplainblind} have no background tinting; y-axis labels are horizontal; gridlines are dots; gridlines are drawn for the x- and y-axis; gridlines are thin; axis line end at with last label marker and start with first label marker; x- and y-axis are in gray; markers are all small; lines are medium thin; fonts of labels are small; legends are not framed; legends appear on the right hand side of the figure; keylabels of legends are medium large; legends rely on rows first; plotregions are omitted; intensity of pie- and bar figures is reduced to 50; borderlines of histograms and bar are thin; marker symbols are reordered to assure that points in scatters are less often overlapping.  

{p 4 4 2}
On top of that, the scheme {cmd:plotplainblind} introduces 7 new colors to stata. Furthermore, the scheme only uses these seven colors in case figures are drawn for more than two subgroups. In case the user only specifies that the figure should be plotted across two subgroups, one group will appear in black and the other group in gray. All colors are chosen in order to be distinguishable for colorblind people. Colorblindness is not a total loss of color vision. Colorblind people can recognize a wide ranges of colors. But certain ranges of colors are hard to distinguish (see Okabe & Ito below). After installing plotplainblind, the seven colors are also available in the default stata color palette. Thus, they may be used like any other color in stata. E.g.: 

{p 8 16 2}
{cmd:. line}
{cmd:x}
{cmd:y}
{cmd:,}
{cmdab:lcolor(reddish)}
 

{p 4 4 2}
The seven colors are: 

{p 8 16 2}
{cmd:ananas}
{cmd:sky}
{cmd:turquoise}
{cmd:reddish}
{cmd:vermillion}
{cmd:sea}
{cmd:orangebrown}

{p 4 4 2}
More details on the schemes may be found in the working paper indicated below.


{title:References}

{p 4 8 2}
Bischof, D. 2015. Figure Schemes for Decent Stata Figures: plotplain & plottig. {browse "http://danbischof.wordpress.com/publications/"}

{p 4 8 2}
Okabe, M. & Kei Ito. Color Universal Design (CUD) - How to make figures and presentations that are friendly to Colorblind people. {browse "http://jfly.iam.u-tokyo.ac.jp/color/"}


{title:Author}

{p 4 4 2}
{browse „bischof@ipz.uzh.ch“:Daniel Bischof}, Department of Political Science,
University of Zurich, CH.


{title:Also see}

{p 4 14 2}
Online:  help for {help schemes}; {it:{help scheme_option}}, {help set_scheme}
{p_end}


