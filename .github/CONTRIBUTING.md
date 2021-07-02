# Contributing to kubrand

This outlines how to propose a change to kubrand. 
For more detailed info about contributing to this, and other tidyverse packages, please see the
[**development contributing guide**](https://rstd.io/tidy-contrib). 

## Fixing typos

You can fix typos, spelling mistakes, or grammatical errors in the documentation directly using the GitHub web interface, as long as the changes are made in the _source_ file. 
This generally means you'll need to edit [roxygen2 comments](https://roxygen2.r-lib.org/articles/roxygen2.html) in an `.R`, not a `.Rd` file. 
You can find the `.R` file that generates the `.Rd` by reading the comment in the first line.

## Bigger changes

If you want to make a bigger change, it's a good idea to first file an issue and make sure someone from the team agrees that it’s needed. 
If you’ve found a bug, please file an issue that illustrates the bug with a minimal 
[reprex](https://www.tidyverse.org/help/#reprex) (this will also help you write a unit test, if needed).

### Pull request process

*   Fork the package and clone onto your computer. If you haven't done this before, we recommend using `usethis::create_from_github("bvancilku/kubrand", fork = TRUE)`.

*   Install all development dependences with `devtools::install_dev_deps()`, and then make sure the package passes R CMD check by running `devtools::check()`. 
    If R CMD check doesn't pass cleanly, it's a good idea to ask for help before continuing. 
*   Create a Git branch for your pull request (PR). We recommend using `usethis::pr_init("brief-description-of-change")`.

*   Make your changes, commit to git, and then create a PR by running `usethis::pr_push()`, and following the prompts in your browser.
    The title of your PR should briefly describe the change.
    The body of your PR should contain `Fixes #issue-number`.

*  For user-facing changes, add a bullet to the top of `NEWS.md` (i.e. just below the first header). Follow the style described in <https://style.tidyverse.org/news.html>.

### Code style

*   New code should follow the tidyverse [style guide](https://style.tidyverse.org). 
    You can use the [styler](https://CRAN.R-project.org/package=styler) package to apply these styles, but please don't restyle code that has nothing to do with your PR.  

*  We use [roxygen2](https://cran.r-project.org/package=roxygen2), with [Markdown syntax](https://cran.r-project.org/web/packages/roxygen2/vignettes/rd-formatting.html), for documentation.  

*  We use [testthat](https://cran.r-project.org/package=testthat) for unit tests. 
   Contributions with test cases included are easier to accept.  

## Code of Conduct

Please note that the kubrand project is released with a
[Contributor Code of Conduct](CODE_OF_CONDUCT.md). By contributing to this
project you agree to abide by its terms.

## Resources for setting up software

### Installing Tools

#### Visual Studio Code

1.  Install before Git.
2.  Download: <https://code.visualstudio.com/download>
    1.  When installing, use these settings:
        1.  Under "Other":
            1.  "Add 'Open with Code' action to Windows Explorer file
                context menu"
            2.  "Add 'Open with Code' action to Windows Explorer
                directory context menu"
            3.  Can use other options too.
3.  After installation:
    1.  How to opt out of Microsoft collecting data:
        <https://code.visualstudio.com/docs/supporting/faq#_how-to-disable-telemetry-reporting>
4.  Documentation: <https://code.visualstudio.com/docs>

#### Git (Version Control)

1.  Install after Visual Studio Code.
2.  Download: <https://git-scm.com/>
    1.  When installing, use these settings:
        1.  In Select Components: Click "Check daily for Git for Windows
            updates"
        2.  Instead of Vim, Set the editor to "Use Visual Studio Code as
            Git's default editor"
        3.  Use Recommended "Git from the command line and also from
            3rd-party software".
        4.  In "Configuring the line ending conversions": Select
            "Checkout as-is, commit Unix-style line endings"
3.  After: When you open a Git Bash it should use MINGW64.
4.  Documentation: <https://git-scm.com/doc>

#### R

1.  Download Base R:
    1.  <https://www.r-project.org/>
    2.  <https://cran.r-project.org/mirrors.html>
    3.  Local Mirror: University of Kansas, Lawrence, KS
        <https://rweb.crmda.ku.edu/cran/>
    4.  For Windows machines:
        1.  <https://rweb.crmda.ku.edu/cran/bin/windows/base/>
        2.  <https://rweb.crmda.ku.edu/cran/bin/windows/base/old/>
    5.  Download version latest in 4.0 series (e.g., as of June 2021 it is 4.0.5)
    6.  Use default options.
2.  Download Rtools:
    1.  For newest version:
        <https://cran.r-project.org/bin/windows/Rtools/>
        or mirror <https://rweb.crmda.ku.edu/cran/bin/windows/Rtools/>
    2.  Use Recommended version.
    3.  Options:
        1.  Select "Extras to build R itself".
        2.  Select "Add rtools to system PATH".
3.  Documentation: <https://www.r-project.org/> (See the "Documentation"
    section on lower left.)

#### RStudio (GUI for R)

1.  Install after installing R.
2.  Download: <https://rstudio.com/products/rstudio/download/>
    1.  Download Free Version of RStudio Desktop.
    2.  Default options.
3.  Documentation: <https://docs.rstudio.com/>

### Change Computer Settings

#### Set and use the correct home directory

On Windows it is necessary to set a home directory locally, as your IT
may set it to the user's network drive, which doesn't work well for many
tools.

1.  Create HOME environment variable.
    1.  Open a File Explorer &gt; Right click on "This PC" &gt;
        Properties &gt; Advanced system settings &gt; Environment
        variables &gt; New User Variable
    2.  Create new user variable `HOME` to
        `C:\Users\<your_username_here>\<your_OneDrive_here>\Documents`.
        1.  If using a `HOME` set to a folder in OneDrive, will need to
            make these adjustments:
            1.  After creating a "git" folder in the OneDrive Documents
                folder, R Click "git" folder and select "Always keep on
                this device" -- so that files always stay locally on
                machine.
        2.  Previously we used
            `C:\Users\<your_username_here>\Documents`.
2.  IF NEEDED: Create start up file for Bash that sets variables &
    changes directories (e.g. automatically starting Git Bash in the git
    folder).
    1.  Edit your `.bashrc` in Visual Studio Code:
        1.  Run in a Git Bash prompt:
            1.  mkdir \~/git
            2.  code \~/.bashrc
        2.  Type in files:
            1.  cd \~/git

### Using Git

In Git Bash prompt:

##### Create git folder for the first time

``` bash
echo $HOME   # Check HOME directory.
ls           # See files in current directory. If Bash shows "MINGW64 ~" then you should be in HOME.
mkdir ~/git  # Creates "git" folder in your HOME directory.
```

NOTE: If you create a "git" folder in a OneDrive Documents folder (not
always recommmended), in Windows Explorer, R Click "git" folder and
select "Always keep on this device" -- so that files always stay locally
on machine.

##### Setting user settings

``` bash
git config --global user.name "{your full name}"  # Setting your git username.
git config --global user.email "{your email}"  # Set email.
git config --list  # Check that it worked.
```

##### Cloning for the first time

``` bash
cd ~/git     # Changes directory to git folder.
git clone "https://github.com/bvancilku/kubrand.git"  # Cloning a git drive.
```

##### Pulling from git after the first time

``` bash
cd ~/git/kubrand
git pull
```

##### Help on Git

``` bash
git config
git config --help  # Launches the help in a webpage version.
```


