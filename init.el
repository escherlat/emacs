(setq load-path (cons "~/.emacs.d/lisp" load-path))

(load "nxml-mode/rng-auto.el")

(require 'ido)
(ido-mode t)

;; The following lines are always needed.  Choose your own keys.
;; (add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(setq org-log-done 'time)
(add-hook 'org-mode-hook 'turn-on-font-lock)  ; Org buffers only
(transient-mark-mode 1)

(setq org-agenda-files (list "~/Dropbox/cPanel/Notes"))

;; Better line numbering
(setq linum-format "%d ")

;; Colors :)
(require 'color-theme)
(color-theme-initialize)
(color-theme-comidia)

(require 'setnu)
(setnu-mode t)

;; Show column number in mode line
(column-number-mode)

;; Use CPerl mode instead
(cperl-mode)
(defalias 'perl-mode 'cperl-mode)
(setq-default indent-tabs-mode nil)
;; Use 4 space indents via cperl mode
(custom-set-variables
  '(cperl-close-paren-offset -4)
  '(cperl-continued-statement-offset 4)
  '(cperl-indent-level 4)
  '(cperl-indent-parens-as-block t)
  '(cperl-tab-always-indent t)
)

;;http://gist.github.com/128694
;; perltidy region with <strg><c> <t>
(defmacro mark-active ()
  "Xemacs/emacs compatibility macro"
  (if (boundp 'mark-active)
      'mark-active
    '(mark)))
 
(defun perltidy ()
 "execute perltidy for the selected region or the current buffer"
 (interactive)
 ; save-excursion doesn't work somehow... so:
 (let ((orig-line (line-number-at-pos (point))))
   (unless (mark-active) (mark-whole-buffer))
   (shell-command-on-region (point) (mark) "perltidy -q" nil t)
   (princ "Tidied" t)
   ;; TODO Check for perltidy.ERR
   (goto-line orig-line)))
(global-set-key "\C-ct" 'perltidy)
 
;;(global-set-key "\C-ct" 'perltidy)
 
(defun perl-tidy-full ()
  (interactive)
  (save-excursion
    (mark-whole-buffer)
    (perltidy)))
 
(global-set-key "\C-c T" 'perl-tidy-full)

; Set nxml-mode for certain extensions
  (setq auto-mode-alist
        (append (list
                 '("\\.xml" . nxml-mode)
                 '("\\.xsl" . nxml-mode)
                 '("\\.xsd" . nxml-mode)
                 '("\\.rng" . nxml-mode)
                 '("\\.xhtml" . nxml-mode))
                auto-mode-alist))
(add-hook 'nxml-mode-hook 'turn-on-auto-fill)

(defun pek-vc-git-annotate-command (file buf &optional rev)
 (let ((name (file-relative-name file)))
   ;; 'short' date, instead of iso; see modified regex below to
   ;;   get coloring to work
   (vc-git-command buf 'async name "blame" "--date=short" rev)))

(defun pek-vc-git-annotate-time ()
 (and (re-search-forward
       ;; HASH           (     1.year    -  2.month   -  3.day                                                                                                                                                   
       "[0-9a-f]+[^()]+(.* \\([0-9]+\\)-\\([0-9]+\\)-\\([0-9]+\\)" nil t)
      (vc-annotate-convert-time
       ;; send 0 for sec, min, hour                                                                                                                                                                              
       (apply #'encode-time 0 0 0
              ;; day, month, year from the match above                                                                                                                                                       
              (mapcar (lambda (match)
                        (string-to-number (match-string match)))
                      '(3 2 1))))))

(eval-after-load "vc-git" '(fset 'vc-git-annotate-command 'pek-vc-git-annotate-command))
(eval-after-load "vc-git" '(fset 'vc-git-annotate-time 'pek-vc-git-annotate-time))
