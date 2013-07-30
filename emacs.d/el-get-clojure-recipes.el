;;
;; El-get recipes turning emacs in a clojure ide.
;; It provides:
;; - clojure-mode & clojure-test-mode (with test lookup function for maven
;;   like project structure)
;; - nrepl
;; - nrepl-inspect (see https://github.com/vitalreactor/nrepl-inspect)
;; - ac-nrepl
;;
;; A few cool resources for clojure development (mainly for shortcuts)
;; - https://github.com/kingtim/nrepl.el
;;

;; Clojure editing mode
(defun el-get-clojure ()
  "El-get recipes for clojure-mode & companions."
  '((:name clojure-mode
           :features (clojure-mode)
           :depends (eproject)
           :after (progn

                    ;; Make eproject recognize clojure projects
                    (define-project-type  
                          clojure
                          (generic)
                          (look-for "project.clj")
                          :irrelevant-files ("target/.*"
                                             ".git/.*"
                                             "#.*#"))

                    ;; Make clojure-test-for-fn lookup the implementation
                    ;; in a Maven like directory structure.
                    (defun clojure-test-for-mvn (namespace)
                      "Returns the path of the test file for the given namespace."
                      (let* ((namespace (clojure-underscores-for-hyphens namespace))
                             (segments (split-string namespace "\\.")))
                        (format "%s/src/test/clojure/%s_test.clj"
                                (file-name-as-directory
                                 (locate-dominating-file buffer-file-name "src/"))
                                (mapconcat 'identity segments "/"))))
                    (setq clojure-test-for-fn 'clojure-test-for-mvn)

                    ;; Enable paredit. Update the :depends section to include
                    ;; paredit if you wish to enable this.
                    ;(add-hook 'clojure-mode-hook 'paredit-mode)

                    ;; Make TODO tag look different
                    (add-hook
                     'clojure-mode-hook
                     ;;'text-mode-hook
                     (lambda ()
                       (font-lock-add-keywords
                        nil
                        '(("\\(FIXME\\|TODO\\|BUG\\):" 1 font-lock-warning-face t)))))))

    ;; Clojure-mode provides clojure-mode and clojure-test-mode.
    ;; Because of a circular dependency (nrepl -> clojure-mode,
    ;; clojure-test-mode -> nrepl) we require two recipes nonetheless.
    (:name clojure-test-mode
           :features (clojure-test-mode)
           :website "https://github.com/technomancy/clojure-mode"
           :description "Emacs support for the Clojure language."
           :type github
           :pkgname "technomancy/clojure-mode"
           :depends (nrepl)
           :after (progn
                    ;; Make clojure-test-implementation-for-fn find the source
                    ;; in standard maven project structure.
                    (defun clojure-test-implementation-for-mvn (namespace)
                      "Returns the path of the src file for the given test namespace."
                      (let* ((namespace (clojure-underscores-for-hyphens namespace))
                             (segments (split-string namespace "\\."))
                             (namespace-end (split-string (car (last segments)) "_"))
                             (namespace-end (mapconcat 'identity (butlast namespace-end 1) "_"))
                             (impl-segments (append (butlast segments 1) (list namespace-end))))
                        (format "%s/src/main/clojure/%s.clj"
                                (locate-dominating-file buffer-file-name "src/")
                                (mapconcat 'identity impl-segments "/"))))

                    (setq clojure-test-implementation-for-fn 'clojure-test-implementation-for-mvn)

                    ;; We want to highlight TODOs in a different color
                    (add-hook
                     'clojure-test-mode-hook
                     ;;'text-mode-hook
                     (lambda ()
                       (font-lock-add-keywords
                        nil
                        '(("\\(FIXME\\|TODO\\|BUG\\):" 1 font-lock-warning-face t)))))))

    ;; The famous nrepl
    (:name nrepl
           :features (nrepl)
           :depends (clojure-mode)
           :after (progn
                    ;; Enable camelcase forward/backward moving in repl
                    ;(add-hook 'nrepl-mode-hook 'subword-mode)

                    ;; Enable paredit. Update the :depends section to include
                    ;; paredit if you wish to enable this.
                    ;(add-hook 'nrepl-mode-hook 'paredit-mode)))
                    ))

    ;; Ritz nrepl
    ;;(:name ritz-nrepl
    ;;       :type github
    ;;       :pkgname "pallet/ritz"
    ;;       :description ""
    ;;       :depends (nrepl)
    ;;       :load-path ("site-lisp/apel" "site-lisp/emu")
    ;;       :after (progn))

    ;; Inspect à la slime-inspect
    (:name nrepl-inspect
           :type github
           :pkgname "vitalreactor/nrepl-inspect"
           :features (nrepl-inspect)
           :depends (nrepl)
           :after (progn
                    (define-key nrepl-mode-map (kbd "C-c C-i") 'nrepl-inspect)))

    ;; Enable ac-nrepl
    (:name ac-nrepl
           :type github
           :pkgname "purcell/ac-nrepl"
           :features (ac-nrepl)
           :depends (auto-complete nrepl clojure-mode)
           :after (progn
                    (add-hook 'clojure-mode-hook 'ac-nrepl-setup)
                    (add-hook 'nrepl-mode-hook 'ac-nrepl-setup)
                    (add-hook 'nrepl-interaction-mode-hook 'ac-nrepl-setup)
                    (add-to-list 'ac-modes 'nrepl-mode)))))
