{smcl}
{* 19feb2019}{...}
{hline}
help for {hi:scheme_plotplain}{right:(Version 1.3.0)}
{hline}

{title:Title}
Scheme description: blindschemes


{* index schemes}{...}
{title:Scheme description:  blind and colorblind}

{p 4 4 2}
Blindschemes is a package containing four Stata figure schemes. For more details please check the respective helpfiles. The plotplain scheme family is: 

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
Please consult the respective helpfiles for each figure scheme: 

{p 8 16 2}
{cmd:. help}
{cmd: plotplain}

{p 8 16 2}
{cmd:. help}
{cmd: plotplainblind}

{p 8 16 2}
{cmd:. help}
{cmd: plottig}

{p 8 16 2}
{cmd:. help}
{cmd: plottigblind}


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

{p 4 4 2}
The scheme {cmd:plottig} uses 14 new colors and a gray background. It heavily relies upon the insights from Cleveland, 1985 and on the ggplot scheme for the statistical software R.  

{p 4 4 2}
The scheme {cmd:plottigblind} relies upon the same design as {cmd:plottig} but instead uses 7 colors which are distinguishable for colorblind people and have been introduced in {cmd:plotplainblind}.



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


