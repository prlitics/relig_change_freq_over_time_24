# How Many Change (or Leave) Their Religion as they Grow-Up?

## An investigation and visualization of the spiritual trajectories of American Adults using the General Social Survey.

This is the code directory for this [Medium](https://prlicari.medium.com/how-many-change-or-leave-their-religion-as-they-grow-up-780487e0c347) post looking at the proportion of Americans that change religions from the ones that they were raised in---and whether certain religions tend to be "better" at "retaining" individuals. It also compares these trends today (well, at least 2018) to 1998. 

## Data
The data for this project come from the General Social Survey (GSS)'s cumulative data file. I am using 1972-2022, release 2a as provided by the incredible [gssr](https://kjhealy.github.io/gssr/) package. As such, I do not include the data here as one can acquire it themselves through that package.

## Reproducibility. 

This project uses the {renv} and {targets} packages to assist in reproducibility. Anyone looking to replicate this analysis (once they've gotten the data) can reproduce the analyses by running renv::restore() and targets::tar_make(). This analysis was conducted in R 4.4.0 on Windows 11.

{targets} encourages a functional approach to projects. To get a visual look at the workflow, you can view the "targets_pipeline.html" file. The functions are within the "R" directory under "functions.R".

## License
This is a CC BY 4.0 licensed project. You can read the LICENSE.md document for further detail but, basically: You're free to use this code more or less however you want. But if you're going to use some/parts/all of it in your own work, please just be sure you cite it.
