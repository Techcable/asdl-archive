Archive of [ASDL](http://asdl.sourceforge.net/) (imported from SourceForge CVS)
================================================================
Hi! For those of you here in the 20th century, this may appear strange.

This is an archived repo for [ASDL](http://asdl.sourceforge.net/): "The Zephyr Abstract Syntax Description Lanuguage".

Most of the interesting parts are in the `asdlGen` subdir. Not sure what the rest is......

The [original academic paper](https://www.cs.princeton.edu/research/techreps/TR-554-97) on ASDL was written in *1997*.

The code in this repo is incredibly old. The most recent commit was 2002.

It was imported from SourceForge CVS over to Git & Github.

## Modern usage
ASDL retains relevance because [it is still ued to describe the Python AST](https://github.com/python/cpython/blob/v3.10.0/Parser/Python.asdl).

See this [blog post for more details on what ASDL is](https://www.oilshell.org/blog/2016/12/11.html) (and why it's still useful). It is written by the [Oil Shell](https://www.oilshell.org/) people.

This is the only Git commit actually written by a human.
All the other commits were automatically generated from the CVS respository (see below).

## Importing
I had to import it to git with [cvs2svn 2.5.0](https://github.com/mhagger/cvs2svn) (also supports cvs2git).

The sequence of comands to import was as follows (directly from my shell history):
````shell
wget "https://github.com/mhagger/cvs2svn/releases/download/2.5.0/cvs2svn-2.5.0.tar.gz"
tar -tf cvs2svn-2.5.0.tar.gz
tar -xf cvs2svn-2.5.0.tar.gz # Generates a directory called "cvs2svn-2.5.0"
rsync -ai a.cvs.sourceforge.net::cvsroot/asdl/ cvs # Downloads the CVS repo from SourceFourge
cd cvs2svn-2.5.0/
python2 cvs2git --blobfile=../blob.dat --dumpfile=../dump.dat --encoding=utf8 --encoding=latin1 --retain-conflicting-attic-files ../cvs
cd ..
git init asdl-git
cat blob.dat dump.dat | git --git-dir=asdl-git/.git fast-import
# Tada
git log
git status
````

Then, I wrote this commit (adding a modern readme).
This is the last commit there will ever be, because I'm archiving the repo :)
