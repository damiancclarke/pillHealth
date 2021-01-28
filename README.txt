--------------------------------------------------------------------------------
README for "Access to the Emergency Contraceptive Pill and Women's
              Reproductive Health: Evidence from Public Reform in Chile"
Damian Clarke & Viviana Salinas
yyyy-mm-dd:2021-01-28
--------------------------------------------------------------------------------
This folder contains four sub-directories:
|_ data:    contains all data used in the paper
|_ source:  contains all source code used for replication of results
|_ results: tables and figures provided in the paper
|_ log:     a log file produced when running each piece of source code
|_ paper:   the tex file of the paper which can be compiled to display figures

All results in the paper can be replicated by running the programs provided in
the "source" sub-directory, principally the file analysis.do. The only required
change is that one global variable at the top of the file (MAIN) should be poin-
ting to the location of this folder on your computer.  If you wish to generate
the precise format of results from the paper, the tex files located in the paper
directory can be compiled, providing the complete rendered output.

All source files are written for Stata version >=15.0.

The main replication file is called "analysis.do".  To replicate all results,
this script should be run from within the "source" sub-directory. This can be run
in Stata, after changing the MAIN global at the top of the script.  This should be
changed to indicate the location of these replication materials on your computer.
For example, if the replication materials are located in a folder called
"/home/replication", the line in each script defining the MAIN global should be
set as:

    global MAIN "/home/replication/"

No other changes are necessary.  A number of external user-written ados are
required, which can be installed from Stata's SSC.  These are installed
automatically if not available, provided that internet connectivity is
available. If a required ado file is not installed and internet connectivity is
not available, the script will fail to run until either the ado has been
installed and the scripts are re-run, or internet connectivity is available and
the script is re-run. 

Tables are all exported as tex files, and figures are all exported as eps files
or pdf files. If you desire to replicate the precise format of these tables and
figures as presented in the paper, you can compile the tex file in the paper
directory using the following command:

  xelatex morningAfterPillHealth-replication.tex

For queries regarding data generation or other replication details, contact
Damian Clarke at

  dclarke [AT] fen.uchile.cl

or

  damian.clarke [AT] protonmail.com.
    
