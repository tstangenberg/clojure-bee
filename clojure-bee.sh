#/bin/sh
#
# Makes it easy to run Emacs with a different init.el file. The init.el file must
# be customized to support this by providing the following elisp code at its start:
#
#

# Determine the directory of this script.
# See http://stackoverflow.com/questions/630372/determine-the-path-of-the-executing-bash-script
CLOJURE_BEE_PATH="`dirname \"$0\"`"              # relative
CLOJURE_BEE_PATH="`( cd \"$CLOJURE_BEE_PATH\" && pwd )`"  # absolutized and normalized
if [ -z "$CLOJURE_BEE_PATH" ] ; then
  # error; for some reason, the path is not accessible
  # to the script (e.g. permissions re-evaled after suid)
  exit 1  # fail
fi

# Determine path to emacs executable
case "$(uname)" in
    Darwin)
        EMACS="/Applications/Emacs.app/Contents/MacOS/Emacs"
        ;;
    # This really is the fallback case.
    *)
        EMACS="$(which emacs)"
esac

if [ ! -f $EMACS ]; then
    echo "Failed to find Emacs binary."
    echo "Consider fixing '$0' and commiting your changes back to the clojure-bee"
    echo "repository!"
    exit 1
fi

$EMACS -q -l "$CLOJURE_BEE_PATH/emacs.d/init.el"
