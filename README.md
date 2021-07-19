# staggered_did

The folder contains a Stata project that uses a simulated dataset and estimates event studies using TWFE OLS and the following recently developed estimators: `csdid`, `did2s`, `eventstudyinteract`, `did_multiplegt`, `did_imputation`, `stackedev`. For further details on these estimators and related packages see this [page](https://github.com/asjadnaqvi/Diff-in-Diff-Notes/blob/main/README.md) by [Asjad Naqvi](https://asjadnaqvi.github.io)[<img width="12px" src="https://cdn.jsdelivr.net/npm/simple-icons@v5/icons/twitter.svg"/>](https://twitter.com/asjadnaqvi).

This code is a slightly modified version of a [script](https://github.com/borusyak/did_imputation/blob/main/five_estimators_example.do) originally written by [Kirill Borusyak](https://sites.google.com/view/borusyak/home)[<img width="12px" src="https://cdn.jsdelivr.net/npm/simple-icons@v5/icons/twitter.svg"/>](https://twitter.com/borusyak) and later [adapted](https://www.dropbox.com/s/p5i94ryf4h9o335/five_estimators_example_adapted.do?dl=0) by [David Burgherr](https://www.lse.ac.uk/International-Inequalities/People/David-Burgherr) [<img width="12px" src="https://cdn.jsdelivr.net/npm/simple-icons@v5/icons/twitter.svg" />](https://twitter.com/d_burgherr).

## Guidance

Download the entire zipped folder and open the Stata project `staggered.stpr`. Two scripts will appear in the project manager:

- `scripts/0.run_file.do` sets up the environment and calls `scripts/1.staggered_did_analysis.do` to compute the estimates. There is no need to adjust paths, nor downloading the user-written packages as they are already contained in `stata_packages`. It suffices to run `scripts/0.run_file.do` to obtain the estimates.

- `scripts/1.staggered_did_analysis.do` contains the DID estimations and produces two event-study plots for different time-horizons which will be saved in the folder `output`.


