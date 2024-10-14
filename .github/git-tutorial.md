# GitHub Tutorial

Author: Justin Beresford

Last Edit: October 2024

## Requirements

You have git working, via the terminal[^1]. Follow this section of the <a href="https://github01.frontier.local/Frontier/ds-setup/blob/master/pc-setup.md#git">data science set-up</a> instructions if you haven't. If you don't know, try putting `git config --global --list` into GitBash. If your name and email address shows up, you can move on. 


[^1]: If you want to use the desktop, follow the excellent guide on the DS wiki.  

## Navigating via the terminal 

#### GitBash, in our case

There are a handful of few key commands you're going to want to memorise.
+ **P**rint **w**orking **d**rive: open GitBash and type in `pwd`, it'll show you which file path you are located in

+ **L**i**s**t contents of folder: next put in `ls`, it'll show you the files in the folder.

+ **C**hange **d**irectory: you can `cd` to your home folder with `cd ~` or to the root of the z with `cd /z`. Put in `ls` to check you've arrived, and from there, try typing `cd res` and hitting tab. It should default to  `cd Resources/` and the point is, if you have the msucle memory & memorise the path, you can tab very quickly through menus *("keep your hands on the keyboard")*.

+ **M**a**k**e a **dir**ectory: to make a new folder (directory), `cd` into the folder you want to make a subfolder in, then `mkdir new_folder_name`. You should probably have a repos folder in your root, and you can do this with `mkdir ~/repos`

+ Make a file, then open it: `touch file_name.txt` will make a file called file_name.txt. Even cooler, `nano file_name.txt` will check if it exists, open it in a text editor if it does, or create and open it if it doesn't. 

+ **R**e**m**ove a file or folder: `rm file_name.txt` will delete the file you just make. You can delete entire folders in a similar way, but take great care! 

:warning: `rm`, and indeed the terminal in general, doesn't come with any pop-ups or undo options :warning:

## Getting started: how to clone a repo?

Make a repos folder, probably in here, noting that the terminal doesn't particularly enjoy spaces in file paths[^2].

[^2]: Unforuntately the Z: drive is littered with them

``` bash
mkdir /z/Resources/Personal/Your \Name/repos
```
Navigate to the URL for this GitHub repo (if you're reading this, you're already on it), hit code, and then copy/paste the HTTPS link. Then back to GitBash, `cd` into your `repos` folder and put in

```
git clone https://github01.frontier.local/Frontier/template_r.git
```
Try `ls` to satisfy yourself that the folder has appeared, then `cd` into `template_r` and `ls` again, to confirm the contents of the folder have appeared.

Because you followed the ds-setup guide, git is initialised and `git status` should bring up something like:

``` git
git status
On branch main
Your branch is up to date with 'origin/main'.
```

If you have VSCode installed, you can then do `code .` to open VSCode to the repo.

## The basics: contributing to a repo

Now you've got a repo cloned. Try making a change to a file. There are three steps to memorise to make this change appear on GitHub:
1. `git add .` to add everything to your 'staging area'
1. `git commit -m "a descriptive message"` to commit the changes in your staging area to your local respository
1. `git push` to push your local repository into the remote repository (i.e. to GitHub). 

If someone else makes and pushes a change, pulling it down to your working directory is much easier, just `git pull`. When you've got those four memorised, you're most of the way there. 

<img src="https://cloudstudio.com.au/wp-content/uploads/2021/06/GitWorkflow-4-768x495.png">

## The best bit: branching and merging

If you're working by yourself - or in parallel with someone else, such that you don't touch eachothers scripts - the basics give you everything you need. When you start working collaboratively, branching and merging becomes essential. 

The idea here is that nobody can push directly to the `main` branch. You should work in your own branch, then raise a "Pull Request" to merge your branch into `main`. The Pull Request triggers an email, requesting a code review. If all is well, your team mate approves the change, and you can then merge your branch into main. The aim is to drive of culture of small and continual QA, as oppsoed to the almost impossible task of needing to review hundreds or thousands of lines of code after the analysis has finished. 

<img src="https://www.nobledesktop.com/image/gitresources/git-branches-merge.png">

The steps to branching and merging are:
1. `git checkout -b example_branch_name`. This does two things, creates the branch and checks it out. The output from a call to `git status` will confirm that you're in this new branch. If you want to go back to main, `git checkout main`, the difference being that we dropped the create part of the call, `-b`.
1. Make your changes, then add, commit and push to GitHub as many times as you like. Safe in the knowledge that you can't break anything on the main branch.  
1. When you're ready for a review, head over to GitHub and hit Pull Request <a href="https://github01.frontier.local/Frontier/template_r/pulls">e.g. here</a>. Then do New Pull Request, and pick `main` as base and your branch as `compare`. The `File changes` tab will pick out the differences between your branch at the main. 
1. Request a review in the top right hand side. 
1. When the review has been approved, hit the merge button and (optionally) delete your branch.
1. Go back to RStudio, `git checkout main` and `git pull` to update the main branch in your working directory. 
