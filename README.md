Expert Choice
================
Jed Stephens
31 March 2020

[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/webp)](https://cran.r-project.org/package=ExpertChoice)
[![CRAN RStudio mirror downloads](http://cranlogs.r-pkg.org/badges/webp)](https://cran.r-project.org/package=ExpertChoice)

# Welcome\!

`ExpertChoice` has two vignettes to help you get started. The
Theoretical Introduction to `ExpertChoice` focusses on the theory for
designing efficiently for experiments, conjoint and discrete choice. The
Practical Introduction to `ExpertChoice` aims to explain how to use this
R package. It also gives two worked examples. The documents reflect each
other. For more detail keep reading.

The need for the `ExpertChoice` R package emerged from the
methodological desire to implement a discrete choice experiment in my
research. There exists a lack of comprehensive open source software to
assist in the design of discrete choice experiments. Currently there are
three R packages on CRAN that have some overlap with `ExpertChoice`:
[choiceDes](https://CRAN.R-project.org/package=choiceDes),
[idefix](https://CRAN.R-project.org/package=idefix) and
[support.CEs](https://CRAN.R-project.org/package=support.CEs). Two of
these packages are no longer under active development and some of the
functions have not been maintained and consequently no longer work. Two
packages also lack documentation making it difficult for all but experts
in this field to use. `ExpertChoice` provides a unified framework
suitable for a first time learner to understand how to design an
experiment and convert this experiment into a discrete choice. Its scope
is also wider and more current than the above alternate packages.

Theoretical introduction to `ExpertChoice` is the first vignette: its
objective is to explain the theory of experimental design and discrete
choice design. It focusses on explaining how efficiently measure tests
play an important role in the designing process. The silver object
choice experiment, analysed in my dissertation, is one of the two
examples in this vignette. A hypothetical choice experiment on a
restaurant is another.

The second vignette, Practical introduction to `ExpertChoice`, provides
a worked example of both experimental designs. The worked examples make
it clear how this procedure could be adapted for the reader’s own
experiment. Some of the more advanced functionality of the package is
explored in particular with the restaurant example.

`ExpertChoice` now provides a unified open source alternative to many
routines previously only available in SAS and Ngene.
