Clojure-bee
===========

Clojure-bee is a set of packages turning Emacs into a Clojure development environment. 
It installs and configures the following (configured) Emacs packages:

* clojure-mode & clojure-test-mode (well, it's about clojure)
* ace-jum-mode (makes navigating a buffer a swift)
* nrepl (the famous nrepl)
* ac-nrepl (auto-complete for clojure)
* nrepl-inspect (inspecting clojure objects)
* undo-tree (it's almost like git for your undo/redo history)
* auto-complete (required by ac-nrepl)
* eproject (create projects!)

`clojure-mode`'s jump to test source and back - `clojure-test-for-fn` and `clojure-test-implementation-for-fn` - 
use a customized version that will look for sources in `maven` like project structure.
If you don't like this you can use the orginal function that come with `clojure-mode` and `clojure-test-mode` 
by uncommenting the respective lines in `<clojure-bee-home>/emacs.d/el-get-clojure-recipes.el`. 

Note
---------
This project is still in its infancy and really is far from being complete.
Please let me know if something does not work for you and I'll try my best to help
you out, though I'm neither an Emacs nor a Clojure expert.

Any feedback is of course well appreciated!

Prerequisits
------------
* Emacs 24 (TODO: is X Emacs required for *nix? Raindbow delimiters can not be loaded if
  not running under a window manager, at least in OSx):
  * [Emacs For OSx](http://emacsformacosx.com/)
  * [XEmacs](http://www.xemacs.org/)
* [lein](https://github.com/technomancy/leiningen) must be installed on your system


Installation
------------
These installation instructions are for Linux/OSx only.

Clone [clojure-bee](https://github.com/instilled/clojure-bee).

    $ cd ~
    $ git clone git@github.com:instilled/clojure-bee.git

You can now run `clojure-bee` by invoking `<path-to-clojure-bee>/clojure-bee.sh`.

    $ ~/clojure-bee/clojure-bee.sh

On the first run Emacs will download and install all the dependencies. That usually takes
a while.

The configuration comes with customized key bindings. The bindings are shown at
the bottom of the `<path-to-clojure-bee/emacs.d/init.el` file. In fact it is reccomended to read through
all of `init.el`. It provides details on many of the configuration tweaks.

Happy coding!

TODOs
-----
* More documentation
* Test on Windows/make it runnable on windows
* Integrate ritz-nrepl (ritz didn't work on my OS x machine)
