SWEXSYS
=======

The Semantic Web Expert System Shell has been developed in XPCE which is an object-oriented library for building Graphical User Interfaces (GUI's) for symbolic or strongly typed languages and is distributed as a library on top of the hosting Prolog system. The interface between XPCE and the application (host) language is very small and determines the type of the host-language construct passed and translates it into the corresponding XPCE object.

 XPCE provides all semantic elements that can be found in many object-oriented programming languages: classes, objects, methods, instance-variables, inheritance, statements, conditions, iteration, etc. However, XPCE defines the look-and-feel for each of the controllers and as a consequence, XPCE controllers may not behave exactly the same as controllers of other applications in the same windowing environment.

XPCE builds high-level controllers on top of the virtual machine and therefore bypasses the graphical libraries of the hosting system. The same technique is used by many other portable GUI toolkits including Java. The XPCE virtual machine and built-in class library is written in standard ANSI-C and is portable to any machine offering a flat, sufficiently large, memory model (32 or 64 bits). XPCE's graphical classes (including windows, etc.) interface to XPCE Virtual Windows System (VWS). Currently there are VWS implementations for X11 and the Microsoft Win32 API.

On MS-Windows, Prolog programs are normally loaded and started by double-clicking a..pl file. XPCE, being a normal library, does not change this. Note that XPCE can only be used fully with the GUI-based plwin.exe. Using the console-based plcon.exe program only the non-GUI functionality of XPCE is accessible.

The following system requirements represent the system that the software was designed and tested on.

System Requirements

·       SWI-Prolog version 5.2.10

·       XPCE version  6 (bundled with SWI-Prolog)

 Apart from standard functionalities provided by Prolog/XPCE, this software makes use of several other user defined prolog modules such as   rdf_db.pl, export.pl etc.  The code for these modules, in its current form, must be placed in subdirectories as shown below.

Plausible Inference modules (under ‘piec’ subdirectory):

·       piec/piec.pl

·       piec/piec_in.pl

·       piec/piec_core.pl

·       piec/primitive_combine.pl

·       piec/piec_out.pl

RuleML modules (under ‘ruleml’ subdirectory)

·       ruleml/export.pl

·       ruleml/import.pl

Crawler modules (under ‘crawler’ subdirectory)

·       crawler/rdf_crawler3.pl

and so on...

