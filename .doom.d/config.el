;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "John Doe"
      user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;;
;; some default settings
;;

;; default mode to text
(setq initial-major-mode (quote text-mode))

;; make neotree toggle a little better
(setq neo-window-fixed-size nil)

;; Set the neo-window-width to the current width of the
;; neotree window, to trick neotree into resetting the
;; width back to the actual window width.
;; Fixes: https://github.com/jaypei/emacs-neotree/issues/262
(eval-after-load "neotree"
    '(add-to-list 'window-size-change-functions
        (lambda (frame)
            (let ((neo-window (neo-global--get-window)))
                (unless (null neo-window)
                    (setq neo-window-width (window-width neo-window)))))))

;;
;; easy mode (cua + shift select)
;;

(delete-selection-mode t)
(cua-mode t)
(setq shift-selection-mode t)
(setq cua-auto-tabify-rectangles nil) ;; Don't tabify after rectangle commands
(transient-mark-mode 1) ;; No region when it is not highlighted
(setq cua-keep-region-after-copy t) ;; Standard Windows behaviour

;; C-c, C-v, C-x
;; C-z == undo
;; C-Y == redo

;;
;; tabs
;;

(setq-default indent-tabs-mode t)
(setq tab-width 4) ; or any other preferred value
(defvaralias 'c-basic-offset 'tab-width)

;;
;; custom keys
;;

(global-set-key (kbd "M-n") 'xah-new-empty-buffer)
(global-set-key (kbd "M-o") 'find-file)
(global-set-key (kbd "M-g") 'keyboard-quit)

(global-set-key (kbd "M-S-<up>") 'shrink-window)
(global-set-key (kbd "M-S-<down>") 'enlarge-window)
(global-set-key (kbd "M-S-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "M-S-<right>") 'enlarge-window-horizontally)

(global-set-key [M-up] 'windmove-up)
(global-set-key [M-down] 'windmove-down)
(global-set-key [M-left] 'windmove-left)
(global-set-key [M-right] 'windmove-right)

(global-set-key (kbd "M-'") 'my-vsplit)
(global-set-key (kbd "M-5") 'my-hsplit)
(global-set-key (kbd "M-d") 'delete-window)
(global-set-key (kbd "M-l") 'ibuffer) ;; or buffer-menu
(global-set-key (kbd "M-s") 'save-buffer)
(global-set-key (kbd "M-n") 'xah-new-empty-buffer)
(global-set-key (kbd "M-q") 'save-buffers-kill-terminal)

(global-set-key (kbd "C-<left>") 'backword-word)
(global-set-key (kbd "C-<right>") 'forward-word)
(global-set-key (kbd "C-<up>") 'scroll-up-command)
(global-set-key (kbd "C-<down>") 'scroll-down-command)

(global-set-key (kbd "M-,") 'indent-rigidly)
(global-set-key (kbd "M-.") 'indent-rigidly)

(global-set-key (kbd "M-f") 'isearch-forward)
;; C-s == search, then repeat to keep searching for term

;;
;; app shortcuts
;;

(global-set-key [f9] 'neotree-toggle)
(global-set-key [f10] '+eshell/toggle)
(global-set-key [f11] '+eshell)
(global-set-key [f12] 'ansi-term)

;;
;; library functions
;;

(defun xah-new-empty-buffer ()
	"Create a new empty buffer.
	New buffer will be named “untitled” or “untitled<2>”, “untitled<3>”, etc.
	It returns the buffer (for elisp programing).
	URL `http://ergoemacs.org/emacs/emacs_new_empty_buffer.html'
	Version 2017-11-01"
	(interactive)
	(let (($buf (generate-new-buffer "untitled")))
		(switch-to-buffer $buf)
		(funcall initial-major-mode)
		(setq buffer-offer-save t)
		$buf
	)
)

(defun my-vsplit ()
	""
	(interactive)
	(split-window-below)
	(let ((buffer (generate-new-buffer "untitled")))
		(switch-to-buffer buffer)
	)
)

(defun my-hsplit ()
	""
	(interactive)
	(split-window-right)
	(let ((buffer (generate-new-buffer "untitled")))
		(switch-to-buffer buffer)
	)
)

;;
;; TODO
;;

(defun shift-text (distance)
  (if (use-region-p)
      (let ((mark (mark)))
        (save-excursion
          (indent-rigidly (region-beginning)
                          (region-end)
                          distance)
          (push-mark mark t t)
          (setq deactivate-mark nil)))
    (indent-rigidly (line-beginning-position)
                    (line-end-position)
                    distance)))

(defun shift-right (count)
  (interactive "p")
  (shift-text count))

(defun shift-left (count)
  (interactive "p")
  (shift-text (- count)))

(defun x-shift-left ()
	(interactive)
	(shift-text -1)
)

(defun x-shift-right ()
	(interactive)
	(shift-text 1)
)

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
