Clojure-bee
===========

Clojure-bee is a set of packages turning Emacs into a Clojure development environment. 

It has been tested on OSx and Linux but not Windows. 

Important
---------
This project is still in its infancy and really is far from being complete. 
Please let me know if something does not work for you and I'll try my best to help
you out, though I'm neither an Emacs nor a Clojure expert. 

Any feedback is of course well appreciated!

Prerequisits
------------
* Emacs 24 (and possibly other version), [Emacs For OSx](http://emacsformacosx.com/)
* [lein](https://github.com/technomancy/leiningen) must be installed on your system


Installation
------------
These installation instructions are for Linux/OSx only. 

First, backup your current Emacs home directory, usually `~/.emacs.d`. 

    $ mv ~./.emacs.d ~/.emacs.d.bak

Now make sure your in your home folder, clone [clojure-bee](https://github.com/instilled/clojure-bee) 
and rename the folder to `.emacs.d`. 

    $ cd ~
    $ git@github.com:instilled/clojure-bee.git
    $ mv clojure-bee .emacs.d

That's it! Happy coding!

The configuration comes with customized key bindings. The bindings are shown at 
the bottom of the `~/.emacs.d/init.el` file. In fact it is reccomended to read through
all of `init.el`. It provides details on many of the configuration tweaks.  

TODOs
-----
* Add documentation
* Test on Windows
* ... more to come
