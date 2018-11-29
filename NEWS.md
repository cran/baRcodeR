# baRcodeR 0.1.1

Bugs and Improvements: 

- create_PDF() function will replace all underscores in text with dashes. Underscores are not specified in the encoding dictionary of `qcrode` and will throw errors.
- x_space and y_space parameters are now limited between 0 and 1 for easier use. THese parameters are used to position text on the printed labels.
- Font size is no longer limited and is now measured as points. Font size is automatically reduced if text code is too long for the printed labels.

New Features:

- label_width and label_height parameters specify the width and height of the label to enable alleys (i.e. gaps) between physical labels.


-----------------


# baRcodeR 0.1.0

This is the first official release of the package.