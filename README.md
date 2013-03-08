
Feature-Oriented Programming with Object Algebras
=================================================

This repository contains the source code artifacts belonging to the
paper _Feature-Oriented Programming with Object Algebras_, to appear
in the proceedings of the
[27th European Conference on Object-Oriented Programming (ECOOP'13)](http://www.lirmm.fr/ecoop13/).

Copyright (c) 2012-2013
 
- [Bruno C.d.S. Oliveira](mailto:oliveira@comp.nus.edu.sg)   
- [Tijs van der Storm](mailto:storm@cwi.nl)
- [Alex Loh](mailto:alexloh@cs.utexas.edu)
- [William R. Cook](mailto:wcook@cs.utexas.edu)

This Scala IDE Project contains the following artifacts:
	
- [oalg.algebra.core](https://github.com/tvdstorm/oalgcomp/tree/master/src/oalg/algebra/core)
  The generic object algebra combinators implemented using reflection.
	   
- [oalg.algebra.demo](https://github.com/tvdstorm/oalgcomp/tree/master/src/oalg/algebra)
  Case studies illustrating the applicability of object algebra
  composition.

- [oalg.algebra.paper](https://github.com/tvdstorm/oalgcomp/tree/master/src/oalg/algebra)
  Small examples of the paper on object algebra composition.
		 

## Notes on running the Scala code

The source code of _Feature-Oriented Programming with Object Algebras_
is made available in the following Git repository:

- https://github.com/tvdstorm/oalgcomp.git

The most convenient way to explore the source code and run the demo
applications is to clone this repository in the Eclipse IDE
(http://www.eclipse.org) with the Scala IDE installed
(http://scala-ide.org/). If you haven't installed Eclipse, or the
Scala IDE already, please consult the section below on how to do
this. We have successfully tested the examples in both Eclipse Indigo
and Juno.

To check out the the project and run the demos, you need to clone the
oalgcomp Git repository. Go to Window->Open Perspective->Other..., and
select Git Repository Exploring. (If this perspective does not show,
please see below how to install EGit.) In the left-hand side of the
Git perspective, click on the link "Clone a Git repository" or click
the button in the tool bar with blue curved arrow. Enter or paste
https://github.com/tvdstorm/oalgcomp.git in the URI text-field. Click
Next, and Next (checking out master) and Finish. The repository should
now be listed under "Git Repositories". Expand the tree view of
"oalgcomp", right-click "Working Directory" and select Import
projects.... Click Next and Finish. After switching back to the Scala
perspective (Window->Open perspective->Other...), the oalgcomp project
should be present in the Package Explorer.

The source tree is organized as follows:

- oalg.algebra.aspects: contains an aspect for circular attribute evaluation
- oalg.algebra.core: contains the core combinators to compose object algebras
- oalg.algebra.demo: the two case-studies from the paper
   * grammar: algebras for executing and analyzing grammars.
   * stacks: the Stack example used in Prehofer's paper on feature-oriented programming.
   
- oalg.algebra.paper: this directory contains the listings from the paper. 

Both demo packages contain a Main.scala file. After opening such a
file in an editor. They can be run by pressing the "Run" button in the
Eclipse tool bar (green play button). Alternatively: richt-click the
editor or the file in the package explorer and select Run as-> Scala
Application from the context menu.

NB: the code requires Scala 2.10.0-RC5 or later. The Scala IDE Eclipse
update sites used contain a suitable version of the Scala compiler.

## Installing Eclipse and the Scala IDE

If you haven't installed Eclipse yet, please download one of the
following Eclipse versions:

- Eclipse Juno:
  http://eclipse.org/downloads/packages/eclipse-ide-java-developers/junosr2
  
- Eclipse Indigo: http://eclipse.org/downloads/packages/eclipse-ide-java-developers/indigosr2

The next step is to install the Scala IDE with Scala 2.10 included. To
do this, start up Eclipse and go to Help->Install new software. Click
Add to add a new update site. Enter "Scala IDE" as name, and paste the
either one of the following URLs in the URL field, depending on which
Eclipse you are running:

- Indigo: http://download.scala-ide.org/sdk/e37/scala210/dev/site/
- Juno: http://download.scala-ide.org/sdk/e38/scala210/dev/site/

Click Ok. From the list of items hosted on the update site check
"Scala IDE for Eclipse". Click Next, Next, Accept License, and
Finish. After some time, you're asked to restart Eclipse. Do
this. When Eclipse comes back up, click Yes when asked to Run Scala
Setup Diagnostics. You're now set to clone the oalgcomp repository and
import our Scala sources.

We have tested the code with the following versions:

- Indigo:
   - eclipse-java-indigo-SR2-linux-gtk-x86_64
   - Scala IDE for Eclipse3.0.0.rc2-2_10-201303061959
- Juno:
   - eclipse-SDK-4.2.2-linux-gtk-x86_64 (Juno)
   - Scala IDE for Eclipse 1.0.0.41

Both were run using Oracle jdk-7u5-linux-x64.

NB: if the Git perspective is not available in Eclipse Juno, install
EGit via Help->Install new software. Use the following Juno update
site: http://download.eclipse.org/releases/juno ; type git in the
filter search box and select the top Eclipse EGit plugin. 


## Running from the command-line

The top-level Makefile can be used to compile the sources from the
command line on Unix-like systems. This requires the location of the
Scala (2.10.0 or later) compiler scalac to be present in the
PATH environment variables.





