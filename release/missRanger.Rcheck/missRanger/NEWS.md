# missRanger 2.0.1

* Added formula interface to specify which variables to impute (those on the left hand side) and those used to do so (those on the right hand side). Here some examples:

- `. ~ .` (default): Use all variables to impute all variables. Note that only those with missing values will be imputed.

- `. ~ .` - ID: Use all variables except `ID` to impute all missing values.

- `Species ~ Sepal.Width`: Use `Sepal.Width` to impute `Species`. Only works if `Sepal.Width` does not contain missing values.


