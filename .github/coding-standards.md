
# Coding standards

Maintaining consistent code style and ensuring code quality are crucial in collaborative programming. Clean code formatted in a familiar style also helps you spot errors more efficiently and simplifies the review process for others.

Note that stylers are opinionated, they will reformat your code - and sometimes you won't like the output. But there's a benefit to conformity. Consistent formatting reduces "diff noise" - the before/after differences in code due to formatting changes. This makes version control diffs cleaner and much easier to read. Moreover, you (and your colleagues) will develop a kind of muscle memory for this kind of formatting, which will make it easier to read in the long run.   


## What is Pre-commit
Pre-commit is a (python based) framework for managing and maintaining multi-language pre-commit hooks. It allows you to automatically run code quality checks every time you make a commit, ensuring that only clean, well-formatted code is added to your repository.  

## Installation process

The pre-commit framework can be installed using Miniconda (a python installer, sorry), which makes it easy to manage dependencies across different environments. Follow these steps to set up pre-commit with R-specific hooks.

1. Install miniconda from <a href="https://docs.anaconda.com/miniconda/">here</a>. Taking **GREAT CARE** to tick *Add Miniconda3 to my PATH environment variable* as you go through the steps. In short, this will get you python, conda, and pip. For more information on what this is, see <a href="https://github01.frontier.local/Frontier/ds-setup/blob/master/python-setup.md">How to Setup your PC for Data Science</a>. 
2. Check it's working by putting `python --version` and then `conda --version` into GitBash. 
3. Use conda to install pre-commit: go to GitBash and throw in `conda install -c conda-forge pre-commit
4. Install the required R Packages: 
``` r
install.packages(c("precommit", "styler", "lintr"))
```
5. Ensure you have a `.pre-commit-config.yaml` file in your project repo. If you've used this repo as a template, you will do. If you haven't, add one with `precommit::use_precommit()` - you'll probably want to make changes to the default. 
6. Finally, in your terminal (GitBash, or the RStudio terminal), cd into your projects root directory, and run `pre-commit install`.

Steps 1-4 are a one-time thing. Steps 4 & 5 will need to be run every time you set up or clone a new repo. 

## Take it for a spin

Why not give it a go: once you've been through the set up process, paste this ugly code into a script and try to commit it. Notice that it reformats the line into tidyverse styling, then makes you re-do the `git add`, staging the change it made on your behalf.

``` r
mtcars|>group_by(mpg)|>summarise(mean(mpg))
```

## What else is it doing

Below is a summary of the hooks that are applied if you take my `.pre-commit-config.yaml` file as given. For now, it's designed to be minimally invasive.

1. Style R Files with Tidyverse Style (`styler`)
The `style-files` hook automatically formats R code according to the Tidyverse style guide using the `styler` package. This ensures consistent formatting across the project.

2. Lint R Files (`lintr`)
The `lintr` hook checks R code for common issues, such as syntax errors and non-idiomatic code. This helps maintain code quality and adherence to R coding standards.

3. Check for Parsable R Files (`parsable-R`)
This hook ensures that the R code in your files is valid and parsable. Syntax errors will prevent the commit from going through.

4. Prevent Browser and Debug Statements in R Code
The `no-browser-statement` and `no-debug-statement` hooks prevent you from accidentally committing debugging functions (like `browser()` and `debug()`) in your R code.

5. Ensure README Files are Rendered Correctly (`readme-rmd-rendered`)
This hook ensures that your `README.Rmd` file is properly rendered into its `.md` counterpart to keep project documentation up to date.

 6. Check for Large Files (`check-added-large-files`)
This hook blocks commits that include files larger than 200KB (as specified by `--maxkb=200`). This helps avoid adding unnecessarily large files that could bloat the repository. If you *really* need to commit something larger, use download `git lfs` (large file storage).

7. Ensure Files End with a Newline (`end-of-file-fixer`)
This hook ensures that all files (excluding `.Rd` files) end with a newline character, which is a best practice for maintaining clean and portable code files.

8. Forbid Committing Certain R Artifacts (`forbid-to-commit`)
The local hook prevents you from committing common R artifacts such as `.Rhistory`, `.RData`, `.Rds`, and `.rds` files, which are typically not intended to be shared or version-controlled.

# What pre-commit doesn't do

Stylers and lintrs can enforce things like snake_case for variable names, two space indentation, or maximum line lengths. But they can't do everything, in particular, here are some ground rules.

#### 1. Make your code a comment

In practice, this means using meaningful dataframe, variable and function names. Consider the difference between the following two chunks.

``` r

new_df <- df |> # Take the company dataframe, group it by business unit.
  group_by(variable) |>
  # Calculate the average revenue by business unit and store the output in a new dataframe. 
  summarise(mean = mean(value))
```
The code isn't terrible, it's clear from the comment what is going on. But you lose no clarity and save half the time with:

``` r
reveune_by_practice <- frontier_financials |>
  group_by(practice_area) |>
  summarise(mean_revenue = mean(revenue))
```

In short, use comments to explain the *why* not the *what*. You can use them to document edge cases or to justify assumptions, especially hard coded ones! The point is, comments aren't there as an alternative to writing clear code. 

#### 2. Follow variable name conventions

+ Include units as a suffix: `distance_km`, `height_meters`, `revenue_usd_k`
+ Use prefix to indicate types, particularly for temporary data, Booleans or dummy variables: `interim_results`, `is_car`, `has_training`
+ Only use well-known abbreviations: `revenue_avg_dollars` is fine, but `rev_avg_dols` isn't.
+ Try to name your dataframe and variables in a way that makes it obvious what's in each row, for example `sales_data <- data.frame(product_id, revenue_avg_usd, sale_date)`

#### 3. Breakdown complex logic with vectorised operations 

Stylers can reformat complex and deeply nested if-else structures or very long dplyr pipelines, but that won't necessarily make them easy to read or understand.

Instead of writing everything in a single block, break down your logic into smaller, reusable blocks or functions with descriptive names. If you find yourself writing a lot of for-loops, it’s usually a sign that you'll benefit from using vectorised functions like `sapply()` or `lapply()`. A more powerful and flexible option often comes through the advanced tools provided by the purrr package, such as `map()` for lists or `map_df()` for dataframes.
