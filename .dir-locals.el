;; Configure emacs' treatment of tabs.
;;
;; See
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Directory-Variables.html
;; for a description of this file.
;;
;; The 'nil' below applies to all modes.
((nil . ((indent-tabs-mode . t)
        (tab-width . 2)))
 (haskell-mode . (
        ;; Highlight leading space characters, to avoid indenting with
        ;; spaces.
        ;;
        ;; Emacs will prompt you about this, saying it's unsafe, but
        ;; you can permanently store an exception by pressing "!",
        ;; which inserts
        ;;
        ;;   (safe-local-variable-values . (quote ((eval highlight-regexp "^ *"))))
        ;;
        ;; in your ~/.emacs ... except the exception doesn't work, and
        ;; emacs still asks you on each file you open :P
        (eval . (highlight-regexp "^ *")))))
