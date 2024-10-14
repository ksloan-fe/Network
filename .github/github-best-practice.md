# GitHub Best Practice

Author: Justin Beresford

Last Edit: September 2024

## Repo contents

+ Use a descriptive and clear repository name. Including the Project Code name at the start makes using the command line tricky: I suggest using code_name and code_number, for example `trucks-P17-2380`.

+ Create a well-written `README.md`, including set up instructions. 

+ Use a `.gitignore`. Most importantly, don't allow tracking or tmp files like `.RData`, or - as a rule - large data files like `.csv` or `.parquet`. Instead, store both input and output data in the relevant Z-drive project folder. 

+ Use .RProfile to set persistent file paths. This runs every time you restart R (press <kbd>Ctrl</kbd> + <kbd>Shift</kbd> + <kbd>F10</kbd> to do so). Restarting will wipe your environment, and the call to `config.packages.R` will reload your libraries. Restart regularly; don't start your scripts with `rm(list=ls())`.

## Repo structure

Note that no data (raw, processed, or outputs) are stored here. The repo is cloned locally/privately, whereas the data is shared and saved on the z-drive. 

```
swift_entry_exit/
├── README.md
├── data_cleaning/
│   ├── script_example.r
│   └── script_example.r
├── analysis/
│   ├── script_example.R
│   └── script_example.R
├── config/
│   ├── packages.R
│   └── settings.R
├── .github/
│   └── CODEOWNERS
├── .gitignore
├── .Rprofile
└── .pre-commit-config.yaml

```

## Summary of the GitHub Workflow

1. **Clone the repo**: you can clone the repo anywhere that is private to you. We recommend `z/Resources/Personal/YourName/repos` or if working locally `~/repos`. 

2. **Branch:** create a new branch off `main` for your work. Do this with `git checkout -b new_branch_example_name`. Make the branch name descriptive of the task you're working on. Make sure to `git pull` from main before doing so, to ensure you're on the latest version of the branch. 

3. **Add & Commit regularly:** as you work, make regular commits for standalone tasks with meaningful messages. 

4. **Pull Request:** Push your branch to the remote repository and open a Pull Request, and leave a description of what the PR includes. 

5. **Review the PR:** Get feedback from your fellow CODEOWNERS, revise as necessary before merging. 

6. **Merge:** Once approved, merge the pull request into `main`. Then `git checkout  main` and `git fetch` and `git pull`. 


## Commit best practice

Commit regularly, ideally several times per day. 

Use meaningful commmit messages. You don't need to go mad, but something like "Add function to calculate daily sales averages" will do. 

One commit per logical change: if you have to write "add function to caluculate daily sales averages and fix bug in the..." then you probably should've had two commits. 

## Branch protection

Ideally the `main` branch on your repo is locked down (see setup instructions in the `README.md`). With this in place, users can only contribute to `main` via Pull Request, and the PR must be approved by someone else in the github team which is listed in CODEOWNERS file. Note that users with `admin` privilages can be given the right to bypass this requirement (ask yourself whether you want this before allowing it). 

## How to review a Pull Request

When you receive a notification of a new Pull Request (PR), start by reading the PR description to understand the purpose and scope of the changes. Click on the "Files changed" tab to view the specific code modifications. As you review the code, you can add inline comments by clicking the "+" icon next to the line numbers. After completing your review, submit your feedback directly on GitHub by choosing to either:
+ Approve the PR, 
+ Request changes, or 
+ Leave comments for further discussion. With branch protection in place, these conversations will have to be resolved before you can merge to main. 

It's important to review the PR thoroughly. At a minimum, pull the branch and run the code from top to bottom to check for bugs. Instead of (or as well as) sending feedback through Teams or in person, use GitHub's commenting system to keep all communication centralised and easily accessible - both for future reference and team-wide transparency. 


Ideally, you provide detailed, constructive (and kind!) comments  comments that address specific lines or sections of code. But don't focus solely on bugs, code style or formatting; take a step back to assess whether the changes make sense within the broader context of the project. Consider the logic, functionality, and potential impact on existing code. 
