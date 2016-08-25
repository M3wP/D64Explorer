D64 Explorer
============

A GUI application for viewing the contents of a C64/C128 disk image file.

It presently supports images from the 1541 (.D64), 1571 (.D71) or 1581 (.D81).

It supports reading GEOS format disks and GEOS file details.


Copyright (C) 2016, Daniel England.
All Rights Reserved.  Released under the GPL.

This program is free software: you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free Software
Foundation, either version 3 of the License, or (at your option) any later
version.

This program is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
details.

You should have received a copy of the GNU General Public License along with
this program.  If not, see <http://www.gnu.org/licenses/>.


Introduction
------------

I am a C64 and C128 fan.  I loved them as a child and still do today.  I make
use of the Vice emulator on a weekly basis (and very nearly daily these days).

I am always working with disk images for the machines and yet I have had no tool
to manipulate them beyond c1541 that comes with Vice and Vice itself.

I looked around on the Web but there seemed to be no tool for handling 1581
images which I was more and more frequently using.  It seemed to make sense to
me that there should be a single GUI tool or application for handling all of the
image formats.  With an ever pressing need, I decided to write one.  Over a
couple of days, I wrote D64 Explorer.

This application is in a fairly bare state at the present but it does the job
that it suggests it will.  It allows you to browse file information for the
files found on the disk image as well as looking at the raw details of the
directoy, disk sectors and BAM.  I'm pleased to say that it supports viewing
GEOS file details which I believe no other tool/application does.


Usage
-----

The usage is simple.  Select File | Open... to open either a .d64, .d71 or .d81
disk image file.  Files with extra sector error information are supported as are
single or double sided .d71 images.  The sector error information is ignored at
the present time.  Forty (40) track .d64 images are also supported to an extent.
No additional BAM information is supplied for the additional tracks.

Use the View menu to view the additional tools or simply browse the file list
presented.

A number of tabs will be available, each displaying different kinds of detail,
depending upon the file type selected.

If an invalid disk is detected (has the wrong or missing DOS details) then only
the sector viewer will be available for use.

Scratched files are not autmatically viewable at the present time.  This is
because a file type is required before any assumptions can be made about how to
handle any data.  This feature may become available in a future version.  You
can still see the file entry details and make guesses yourself to manually
browse the scratched file contents.


Limitations
-----------

Relative (REL) files are presently unhandled to the extent I would like them to
be.  There is a slightly different format for the Side Sector fork between DOS
versions A and D.  Presently. I haven't got any REL file examples on hand to
check that I have implemented the functionality properly.  This limitation will
be addressed in an upcoming version.

Some people may see it as a limitation that you cannot delete files from or add
files to the image.  The features were not part of my initial requirements list.

However, internally the class used to perform the handling and manipulation of
the disk images has grown, through the development of other applications, to
support simple file allocation and scratching.  It is entirely feasible that the
next version of the D64 Explorer application will have file modification
support.

The sector viewer does not allow you to copy the sector data to the clipboard
like the other tools do.  I need to restructure the sector viewer.  This will
happen soon.


Compiling
---------

You will need Lazarus to compile the application.  Presently, Delphi is
unsupported.  At the time of writing, I am using Lazarus 1.6 but earlier
versions should be supported so long as they have FPC 2.1 or higher.  You can
get Lazarus for your platform from:

        <http://www.lazarus-ide.org/>

All of the Lazarus supported platforms should be supported.  This includes
Windows, Linux and MacOSX in 32 or 64 bit flavours.

To compile, simply open the D64Explorer.lpi file in Lazarus and select
Run | Build.


Futher Information
------------------

This application wouldn't have been possible without the excellent resource at:

        <http://ist.uwaterloo.ca/~schepers/formats.html>


Contact
-------

I can be contacted for further information regarding this application at the
following address:

        mewpokemon {you know what goes here} hotmail {and here} com

Please include the word "D64Explorer" in the subject line.

Thanks for using D64 Explorer!



Daniel England.
