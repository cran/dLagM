## Version 1.0.6 (14 September 2018)
  * The show.summary argument has dropped from the functions dlm(), ardlDlm(), koyckDlm(), and polyDlm(). Now, a call to the classical summary() function summarises the model.
  * All the forecast functions have collected under the forecast() method to improve usability of the package.
  * To remove the model elements, matrices and vectors have been replaced by list interface. Please see the help files for details.
  * Added a functionality to remove lag 0 of any independent series.
  * Improved documentation and errors fixed.
  * Reduced number of dependencies.
  * Improved some functions.

## Version 1.0.7 (26 September 2018)
  * A bug in pre-model fitting checks fixed.

## Version 1.0.8 (15 September 2018)
* A bug in the error messages of pre-model fitting checks for ARDL models fixed.

## Version 1.0.9 (21 September 2018)
* A bug in dlm() function fixed.
* More examples added.
* Removed the limitation on the maximum number of lags can be fitted with finiteDLMauto() function.

## Version 1.0.12 (29 April 2019)
* ARDL output displays variable names instead of X and Ys.
* Fixed a minor bug in ardlDlm().
* It's possible to remove intercept from the ARDL and DL models.
* Added the implementation of ARDL bound tests.

## Version 1.0.15 (17 July 2019)
* A bug in forecast() function fixed. Now, function works smoothly when the intercept is removed.
* Added a new dataset called wheat.

## Version 1.0.17 (13 August 2019)
* An improvement in user interface.
* An new function to implement standard deviation analysis for running correlations.
* Added the CUSUM of squared recursive residuals plot to ARDL bounds testing statibiltiy diagnostics.
* Added the RESET test to ARDL bounds testing diagnostics.
* Enhanced the wheat dataset.

## Version 1.0.18 (18 October 2019)
* New goodness-of-fit measures implemented.
* Added full search over all possile model to specify lag orders for ARDL bounds testing.
* Added normality tests for the residuals in ARDL bounds testing.
* Improved the output of ARDL bounds testing.

## Version 1.0.19 (24 October 2019)
* Added a warning message.
* Added new explanations to help document.
* Fixed a bug. 

## Version 1.0.21 (17 January 2020)
* Changed the output structure of DLMs.
* Added straightforward calls to residuals(), coef(), and fitted() functions.
* Added seaLevelTemp and sunspotTemp datasets
* Updated the examples in the documentation.
* Removed "start" argument from rolCorPlot() function which need ts objects now.

## Version 1.1.1 (28 February 2020)
* Added citation information.
* Fixed some outputs.
* Fixed minor bugs.

## Version 1.1.4 (13 July 2020)
* Fixed the singularity problem with ARDL bounds test under some cases.
* Added the option of running the ARDL bounds test with Newey-West estimator.

## Version 1.1.5 (27 January 2021)
* Fixed  a minor output issue.

## Version 1.1.6 (09 July 2021)
* Added a new dataset called GrainProduction.

## Version 1.1.7 (23 February 2022)
* Fixed a bug in forecasting with ARDL models. Appreciate the tesing effort of Dr. Rogério Porto (https://orcid.org/0000-0002-6663-9531) to identify the bug.

## Version 1.1.8 (09 May 2022)
* Fixed a bug in forecasting with ARDL models. Appreciate the tesing effort of Mr. Arthur Welle to identify the bug.

