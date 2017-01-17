# Repo for Algorithmique des arbres in OCaml
for University of Bordeaux, Bachelor year 2

To clone this repo, just open a terminal on your windows or linux machine and do

    git clone git@github.com:fgolemo/course-2017-ocaml-trees.git

(given that you have git installed)

You can then find the files for the individual exercises number in the root folder. 
So the code for **exercise 9** is called `9-(..).ml` and so on.
Be careful though, because this numbering *might* change as we are updating the syllabus (POLY). 

To compile an individual file, you just run in a terminal

    ocamlbuild FILENAME.native
    
**but `FILENAME` without `.ml`.** So for example

    ocamlbuild 8-insertionsort.native
    
The `.native` ending is important. 
After the compilation was successful (yes, you will get warnings about the bad filename), you can execute the file like this:

    ./FILENAME.native
    
or in our example
  
    ./8-insertionsort.native

---

Each file contains a little example to try the function. If you actually play around with the algorithms, I recommend that you install utop and start up an interactive session (just open a terminal, type in `utop` and hit enter). There you can copy&paste the code from the exercises and try them out more interactively.

