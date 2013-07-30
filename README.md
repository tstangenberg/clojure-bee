Clojure-bee
===========

Clojure-bee is a set of packages turning Emacs into a Clojure development environment. 

Important
---------
This project is still in its infancy and really is far from being complete. 
Please let me know if something does not work for you and I'll try my best to help
you out, though I'm neither an Emacs nor a Clojure expert. 

Any feedback is of course well appreciated!

Prerequisits
------------
* Emacs 24 (TODO: is X Emacs required for *nix? Raindbow delimiters can not be loaded if 
  not running under a window manager): 
  * [Emacs For OSx](http://emacsformacosx.com/)
  * [XEmacs](http://www.xemacs.org/)
  * ...
* [lein](https://github.com/technomancy/leiningen) must be installed on your system


Installation
------------
These installation instructions are for Linux/OSx only.  

Clone [clojure-bee](https://github.com/instilled/clojure-bee).

    $ cd ~
    $ git@github.com:instilled/clojure-bee.git
    
You can now run `clojure-bee` by invoking `<path-to-clojure-bee>/clojure-bee.sh`. 

    $ clojure-bee/clojure-bee.sh

On the first run Emacs will download and install all the dependencies. That usually takes
a while. That's it! 

The configuration comes with customized key bindings. The bindings are shown at 
the bottom of the `~/.emacs.d/init.el` file. In fact it is reccomended to read through
all of `init.el`. It provides details on many of the configuration tweaks.  

Happy coding!

TODOs
-----
* Add documentation
* Test on Windows
* ... more to come
