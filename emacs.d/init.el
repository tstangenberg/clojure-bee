;;
;; A Emacs init.el that aims at providing a comprehensive Clojure
;; development environment that is easily installed. It SHOULD work
;; on OSx and Linux without any further modification. I don't know
;; about windows.
;;
;; The following emacs packages will be installed the first time emacs
;; is run:
;; * el-get (used to install and configure any other package)
;; * helm
;; * clojure-mode & clojure-test-mode
;; * nrepl
;; * ac-nrepl
;; * ido
;; * undo-tree
;; * auto-complete
;; * recentf
;; * eproject
;; * auto-complete
;;
;; For established key bindings see the end of this file. Bindings for
;; clojure-mode and nrepl see https://github.com/technomancy/clojure-mode,
;; or https://github.com/clojure-emacs/nrepl.el.
;;
;; TODOs:
;;  * enable ritz-nrepl (fails on my machine [OSx] currently)
;;
;; This comes with no warranty. Use at your own risk.
;;

;; Only show errors
;;(setq warning-minimum-level :error)

;; We want to run multiple emacs configuration simultaneously.
;; http://stackoverflow.com/questions/17483598/maintaining-multiple-emacs-configurations-at-the-same-time
;; http://stackoverflow.com/questions/4088681/get-path-to-current-emacs-script-file-when-loaded-with-l-parameter
(setq user-emacs-directory
      (file-name-directory load-file-name))

;;-----------------------------------------------------------------------
;; init.el is system dependant. Provide functions to check for system
;; type.
;;
;; Mac OSx?
(defun system-type-is-darwin ()
  (interactive)
  "Return true if system is darwin-based (Mac OS X)"
  (string-equal system-type "darwin"))

;; GNU/Linux?
(defun system-type-is-gnu ()
  (interactive)
  "Return true if system is GNU/Linux-based"
  (string-equal system-type "gnu/linux"))

(defun touch (path)
  (shell-command (concat "touch " path) nil))

;; Fix the PATH variable
;; Credits to http://www.mail-archive.com/clojure@googlegroups.com/msg36929.html
(defun set-exec-path-from-shell-PATH ()
  (let ((path-from-shell (shell-command-to-string "$SHELL -i -c 'echo $PATH'")))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

(if (system-type-is-darwin)
    (when window-system (set-exec-path-from-shell-PATH)))

;;-----------------------------------------------------------------------
;; Helpers to resolve emacs' config dir and some other default
;; locations.
;;
(when load-file-name
  (setq user-emacs-directory (file-name-directory load-file-name)))

(defun with-conf-dir (path)
  (concat user-emacs-directory path))

(defun with-lib-dir (path)
  "Resolves path relative to '~/.emacs.d/lib' dir."
  (let ((lib-dir (concat user-emacs-directory "lib/")))
    (unless (file-exists-p lib-dir)
      (make-directory lib-dir))
    (concat lib-dir path)))

(defun with-cache-dir (path)
  (let ((cache-dir (concat user-emacs-directory "cache/")))
    (unless (file-exists-p cache-dir)
      (make-directory cache-dir))
    (concat cache-dir path)))

(touch (with-cache-dir "emacs.bmk.el"))

;;-----------------------------------------------------------------------
;; Emacs basics
;;

;; Launching emacs-server (server-start) below is required if you want to
;; open files from the command line.
(setq server-name "clojure-bee-emacs-server")
(server-start)

;; And that's what I put in my ~/.zshrc to run emacs when typing
;; 'e <some-file>' on the command line.
;; ```
;; _editor() {
;;   $EMACS_HOME/bin/emacsclient \
;;            --no-wait \
;;            --socket-name=clojure-bee-emacs-server \
;;            --alternate-editor=$EMACS_HOME/Emacs \
;;            $@ &
;; }
;;
;; # The alias to invoke emacs.
;; alias e='$EDITOR'
;; ```

;; Larger font
(set-default-font "DejaVu Sans Mono-14")

;; Disable bell
(defun my-bell-function ())
(setq ring-bell-function 'my-bell-function)

;; Unbind right alt on Darwin
(if (system-type-is-darwin)
    (progn
      (setq ns-right-alternate-modifier nil))) ;; unbind right alt
;;    (setq mac-option-modifier nil)))    ;; this unbinds both alts

;; Use open-file-other-window instead of starting emacs with split
;; windows. (split-window-horizontally) don't pollute directories with
;; emacs files
(setq backup-directory-alist
      `(("." . ,(with-conf-dir "bak"))))

;; Disable spash-screen at startup
(setq inhibit-splash-screen t)

;; Before attempting to position the window and disabling the tool-bar
;; make sure we're running emacs on a window manager, e.g. EmacsX
;; See http://www.dotemacs.de/multiemacs.html
(if (boundp 'tool-bar-mode)
    (progn
      (tool-bar-mode -1)
      ;; Position and size the windows on startup
      ;;(add-to-list 'default-frame-alist '(left . 0))
      ;;(add-to-list 'default-frame-alist '(top . 0))
      ;;(add-to-list 'default-frame-alist '(height . 55))
      ;;(add-to-list 'default-frame-alist '(width . 205))))
      ))

;; Use space not tab
(setq-default indent-tabs-mode nil)

;; Make whitespaces visible (EOL and empty lines)
(setq-default show-trailing-whitespace t)

;; ???
(setq-default column-number-mode t)

;; Highlight parens
(show-paren-mode)

;; Make search highlight occurences
(setq search-highlight t
      query-replace-highlight t)

;; Enable y/n answers to yes/no
(fset 'yes-or-no-p 'y-or-n-p)

;; When to split the srceen
(setq split-width-threshold 120)

;; Make lines break
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)

;; Configure ido mode. This makes navigating in the minibuffer a swift.
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

;;-----------------------------------------------------------------------
;; Recentf
;;

;; Put recentf.el in cach dir.
(touch (with-cache-dir "recentf.el"))
(setq recentf-save-file
      (with-cache-dir "recentf.el"))

;; How many items to keep in recentf (persistent throught emacs restarts)
(setq recentf-max-saved-items 300)

(require 'recentf)
(recentf-mode 1)

;; Don't show files in recentf that have not been displayed (such as
;; TAGS files)
;; credits: http://emacswiki.org/emacs/RecentFiles#toc16
(defsubst file-was-visible-p (file)
  "Return non-nil if FILE's buffer exists and has been displayed."
  (let ((buf (find-buffer-visiting file)))
    (if buf
        (let ((display-count (buffer-local-value 'buffer-display-count buf)))
          (if (> display-count 0) display-count nil)))))

(defsubst keep-default-and-visible-recentf-p (file)
  "Return non-nil if recentf would, by default, keep FILE, and
FILE has been displayed."
  (if (recentf-keep-default-predicate file)
      (file-was-visible-p file)))

(setq recentf-keep '(keep-default-and-visible-recentf-p))

;; Clean whitespaces on save.
;; (add-hook 'before-save-hook
;;         (lambda ()
;;           (whitespace-cleanup)))

;;-----------------------------------------------------------------------
;; El-get.el
;;

;; Use el-get to manage packages. I prefer it over package.el because
;; it can install from nearly any source, including github.

(add-to-list 'load-path (with-conf-dir "el-get/el-get"))

;; Make el-get verbose. Nice to see if things go wrong
(setq el-get-verbose t)

;; El-get will install itself on the first run.
(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

(add-to-list 'el-get-recipe-path
             (with-conf-dir "el-get-user-recipes"))

;; Local sources. The packages to be installed.
(setq el-get-sources

        ;; A nicer looking colortheme
      '((:name color-theme-railscasts
               :depends (color-theme)
               :after (progn
                        (require 'color-theme-railscasts)
                        ;; cursor-color is overwritten by evil-mode with
                        ;; "black" so we hardcode the railscast value
                        ;; here. Note: This will persist even after changing
                        ;; to a different theme as long as evil-mode is
                        ;; active.
                        (setq evil-default-cursor
                              (list (or (frame-parameter nil 'cursor-color)
                                        "#5A647E")
                                    t))
                        (color-theme-railscasts)))

        ;; A must when working with lisps
        (:name rainbow-delimiters
              :website "https://github.com/jlr/rainbow-delimiters"
              :description "rainbow parentheses"
              :type git
              :url "https://github.com/jlr/rainbow-delimiters"
              :features (rainbow-delimiters)
              :after (progn
                       ;; Make them global
                       (global-rainbow-delimiters-mode)))

        ;; It's not enabled by default. I'm not yet accustomed to
        ;; paredit. Maybe due to vim?
        ;;(:name paredit)

        ;; Think of undo-tree as a git for undo/redo operations.
        (:name undo-tree
               :after (progn
                        (global-undo-tree-mode)))

        ;; Helm is for navigating everywhere. Similar to Sublime's Goto Anywhere but
        ;; IMHO more powerfull. Provides sources for
        ;; e.g. eproject, anything, buffers, files, ...
        (:name helm
               :features (helm-config
                          helm-files))

        ;; Complete as you type with overlays
        (:name auto-complete
               :features (auto-complete)
               :after (progn
                        (setq ac-sources
                              '(ac-source-words-in-buffer
                                ac-source-words-in-same-mode-buffers
                                ac-source-abbrev
                                ac-source-dictionary))
                        (global-auto-complete-mode)))

        ;; Moving around with the keyboard... oh yeah.
        (:name ace-jump-mode
               :features (ace-jump-mode))

        ;; Eprojects lets you define your own projects.
        (:name eproject
               :features (eproject
                          eproject-extras)
               :depends (auto-complete
                         helm)
               :after (progn
                        (require 'helm-eproject)))))

;; Load clojure recipes. This turns emacs in a clojure IDE (more or less).
(load-file (with-conf-dir "el-get-clojure-recipes.el"))

(setq el-get-sources (append el-get-sources (el-get-clojure)))

(setq my-packages
      (mapcar 'el-get-source-name el-get-sources))

(el-get 'sync my-packages)


;;-----------------------------------------------------------------------
;; Keyboard customization
;;
;; http://stackoverflow.com/questions/683425/globally-override-key-binding-in-emacs
;; "This has the added benefit of being able to turn off all my
;; modifications in one fell swoop (just disable the minor mode) in
;; case someone else is driving the keyboard or if I need to see what a
;; default key binding does."
;;
;;
(defvar my/my-keys-minor-mode-map (make-keymap)
  "my/my-keys-minor-mode keymap.")

(define-minor-mode my/my-keys-minor-mode
  "A minor mode so that my key settings override annoying major modes."
  t "" 'my/my-keys-minor-mode-map)

(my/my-keys-minor-mode 1)

;; Let the keybinding "retain precedence, even if subsequently-loaded
;; libraries bring in new keymaps of their own." (same SO link)
(defadvice load (after give-my-keybindings-priority)
  "Try to ensure that my keybindings always have priority."
  (if (not (eq (car (car minor-mode-map-alist)) 'my/my-keys-minor-mode))
      (let ((mykeys (assq 'my/my-keys-minor-mode minor-mode-map-alist)))
        (assq-delete-all 'my/my-keys-minor-mode minor-mode-map-alist)
        (add-to-list 'minor-mode-map-alist mykeys))))
(ad-activate 'load)


(defvar my/my-keys-prefix "C-,"
  "A global convenience prefix which is globally overridden. By
  default, C-c should serve that, but in addition to being C-,
  being easier to type (my opinion), we also step on less toes by
  carving out a completely new namespace.")

;; Adapted from evil-leader/set-key, thanks dan.in.a.bottle
(defun my/my-keys-set-prefixed (key def &rest bindings)
  "Set keys into my/my-keys-minor-mode-map using the 'kbd' and using
my/my-keys-prefix."
  (interactive "kKey: \naCommand: ")
  (while key
    (define-key my/my-keys-minor-mode-map
      (read-kbd-macro
       (concat my/my-keys-prefix " " key))
      def)
    (setq key (pop bindings)
          def (pop bindings))))

;; Navigation
;; Make moving around a swift.
(global-set-key (kbd "C-SPC") 'ace-jump-char-mode)
;;(global-set-key (kbd "C-M-SPC")  'ace-jump-word-mode)
(global-set-key (kbd "M-SPC") 'ace-jump-line-mode)

(my/my-keys-set-prefixed "f"    'ido-find-file-other-window)
(my/my-keys-set-prefixed "C-f"  'ido-find-file)
(my/my-keys-set-prefixed "M-f"  'ffap)

(my/my-keys-set-prefixed "C-t"  'next-multiframe-window)
(my/my-keys-set-prefixed "C-p"  'previous-multiframe-window)

(my/my-keys-set-prefixed "u"    'undo-tree-visualize)


(my/my-keys-set-prefixed "C-,"  'execute-extended-command)

;; Eproject navigation
(my/my-keys-set-prefixed "ei"   'eproject-ibuffer)
(my/my-keys-set-prefixed "ef"   'eproject-find-file)
(my/my-keys-set-prefixed "eo"   'eproject-revisit-project)
(my/my-keys-set-prefixed "eK"   'eproject-kill-project-buffers)

;; Regexp search & navigate in current buffer
(my/my-keys-set-prefixed "af"   'helm-find-files)
(my/my-keys-set-prefixed "ar"   'helm-regexp)

;; This brings up a buffer with only files from within the
;; active eproject
(my/my-keys-set-prefixed "ep"
                         '(lambda() (interactive)
                            (helm
                             :prompt "Switch to: "
                             :candidate-number-limit 100
                             :sources
                             '(helm-eproject-source))))

;; Binds a navigation toolbar
(my/my-keys-set-prefixed "b"
                         '(lambda() (interactive)
                            (helm
                             :prompt "Switch to: "
                             :candidate-number-limit 10
                             :sources
                             '(helm-c-source-recentf
                               helm-c-source-files-in-current-dir

                               ;;anything-c-source-eproject-buffers
                               ;;anything-c-source-buffers+ ; TODO try out

                               helm-c-source-buffers-list

                               ;;anything-c-source-bookmarks
                               ;;helm-c-source-bookmarks

                               ;;helm-c-source-emacs-variables
                               ;;helm-c-source-emacs-functions
                               ))))

;; El-get, my hero
(my/my-keys-set-prefixed "pl"   'el-get-list-packages)
(my/my-keys-set-prefixed "pd"   'el-get-describe)
(my/my-keys-set-prefixed "pc"   'el-get-cd)
(my/my-keys-set-prefixed "pI"   'el-get-install)
(my/my-keys-set-prefixed "pi"   'el-get-init)
(my/my-keys-set-prefixed "pu"   'el-get-update)
(my/my-keys-set-prefixed "pR"   'el-get-remove)

(my/my-keys-set-prefixed "cw"   'whitespace-cleanup)

;; Programming related
(my/my-keys-set-prefixed "cj"   'nrepl-jack-in)
(my/my-keys-set-prefixed "cc"   'comment-or-uncomment-region)


;; Keep track of customizations in its own file
(setq custom-file
      (with-conf-dir "customize.el"))
(touch custom-file)
(load custom-file)
