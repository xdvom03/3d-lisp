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

Utility functions which have no side effects, but which are dependent on the camera angle:<br>
canvas-loc<br>
real-locs<br>
real-loc<br>
in-fog-p<br>
fog-cutoff-loc<br>
sort-by-fog<br>

These functions rotate the camera:<br>
rot-cam-x<br>
rot-cam-y<br>
rot-cam-z<br>
rot-cam-absolute<br>

These functions belong to the engine and should never be called:<br>
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

# Constants

All coordinates are linked lists of components unless specified otherwise<br>
\*view-center\* = Position of camera in 3D coordinates<br>
*canvas-center* = 2D coordinates of the middle of the canvas printed on<br>
*screen-size* = Side of canvas in pixels<br>
*fog* = Distance of plane that cuts off objects (to prevent objects behind camera from showing up and to block division by zero)<br>
*fov* = FOV in radians (default 1)<br>
*move-step* ;How fast the camera moves per button press<br>
*rot-step* ;How fast the camera rotates - radians per button press<br>
*x-dir* = Unit vector pointing in the camera's x-axis - right (defines orientation in space)<br>
*y-dir* = Unit vector pointing in the camera's y-axis - up (defines orientation in space)<br>
*z-dir* = Unit vector pointing in the camera's z-axis - forward (defines orientation in space)<br>
*norm* = Distance of projective plane<br>

# Data storage
*triangles* = list of triangles<br>
*lines* = list of lines<br>
Data structure of lines, triangles etc. described in source code under respective adding functions

# Tlačítka ovládání English
Arrow keys to move left/right and front/back. Space/Ctrl to go up and down, respectively.


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
