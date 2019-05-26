# 3d-lisp
Very basic (lines, triangles) 3D engine in Common Lisp.<br>

# Provided functions
You provide a file with two functions: create-objects and start-program. start-program is run once to initialise your task, while create-objects is run each tick to move around objects.<br>
Within this file, you can set up your own global variables, etc. If you wish to do any calculations periodically, make them a part of create-objects.

Utility functions that you are free to use (as they are free of side effects):<br>
sq<br>
transpose<br>
mv-multiply<br>
pythagoras<br>
distance<br>
perpendicular-base<br>
vector-divide<br>
cross-product<br>
turn<br>
rotated-loc<br>
rotate-around-point<br>
remove-nth<br>
remove-last<br>

Utility functions which have no side effects, but which are dependent on the camera angle:
canvas-loc<br>
real-locs<br>
real-loc<br>
in-fog-p<br>
fog-cutoff-loc<br>
sort-by-fog<br>

These functions rotate the camera:
rot-cam-x<br>
rot-cam-y<br>
rot-cam-z<br>
rot-cam-absolute<br>

These functions belong to the program and should never be called:
run<br>
tick<br>
tick-compute<br>
render<br>
draw-line<br>
draw-triangle<br>
init<br>

Creating and destroying objects is done using the following functions:

add-line<br>
add-triangle<br>
add-cube<br>


[TBD: Global variables, check licence etc.]


# How to install/use
Dependencies: tcl, tk, clisp (available via pacman/apt-get)<br>
Set file "project" as executable<br>
Run file "project" with the path to the file you wish to run in the engine, for example ./project 3d-grav.lisp for the test program provided.

# Used software

Portacle
VirtualBox
Linux Mint
CLISP

# Caveat emptor

Potential issues:<br>
This project was tested on Linux Mint and Ubuntu. It may fail on other systems. A potential cause of such a failure is an unexpected install location of tcl/tk. Make sure that "/usr/bin/wish" is a valid file location.<br>
The engine is capable of displaying 2D shapes using the create-triangle function. However, when two triangles overlap within the field of view, it is not resolved properly. This can cause triangles to appear behind lines when they are not. This is being worked on.<br>
When defining your own functions within your file, avoid redefining functions from the engine files. Their list can be found above.
