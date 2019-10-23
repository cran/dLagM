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
* A bug fixed. 
