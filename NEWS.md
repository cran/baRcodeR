# baRcodeR 0.1.7 

Thanks to rwalke for adding "Linear2" to the option list (Linear, Matrix). This will produce Code 128 extended linear barcodes, allowing for particular 1-D barcode patterns that are distinct from "Linear"

Thanks to Theirry() for pointing out that we need to change qrcode::qr_code() from (deprecated) qrcode_gen


# baRcodeR 0.1.6

At the request of a COVID-2019 research group, we have added an option to allow non-encoded text to appear with linear & 2D barcodes. These have been added to custom_create_PDF():

* alt_text -- adds human-readable text that is NOT encoded in the digital barcode
* denote -- characters used to denote non-encoded text

These are considered advanced features that should be used cautiously, and therefore they are not made available through the Addins GUI. 

# baRcodeR 0.1.5

Bugs:

- fixed errors in tests due to r-devel switching to using stringsAsFactor = FALSE as default
- fixed broken link in readme


# baRcodeR 0.1.4

Bugs and Improvements:

- tests have been added for user prompts and the RStudio addin with shinytest
- vignettes describing how to use the add-in and the command line are now organized separately from the restructured README
- `custom_create_PDF()` page generation should now be faster (helpful when making sheets for hundreds of labels)
- the command prompts have been restructured for a more menu-like selection for yes/no questions.
- other minor changes in addition to the major ones outlined above as suggested by reviews at rOpenSci documented [here](https://github.com/ropensci/software-review/issues/338)
- the baRcodeR package is now a part of [rOpenSci](https://ropensci.org/) and the documentation is online (here)[https://docs.ropensci.org/baRcodeR/]

# baRcodeR 0.1.3

Bugs and Improvements:

- major bug fix for linear barcodes that occasionally created unscannable barcodes.
- added documentation on how to create alternative formatting of labels (e.g. spaces, line breaks)
- added padding for labels which were single character or blank
- 2-page cheatsheet now available as addin 

# baRcodeR 0.1.2

New Feature:

- In response to a user request, there is now an option to print linear (code 128 set B) barcodes. 

# baRcodeR 0.1.1

Bugs and Improvements: 

- create_PDF() function will replace all underscores in text with dashes. Underscores are not specified in the encoding dictionary of `qcrode` and will throw errors.
- x_space and y_space parameters are now limited between 0 and 1 for easier use. These parameters are used to position text on the printed labels.
- Font size is no longer limited and is now measured as points. Font size is automatically reduced if text code is too long for the printed labels.

New Features:

- label_width and label_height parameters specify the width and height of the label to enable alleys (i.e. gaps) between physical labels.


-----------------


# baRcodeR 0.1.0

This is the first official release of the package.