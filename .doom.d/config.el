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



;;
;; focusing maps
;;

(map! :leader
    (:prefix ("w" . "extend")
      :desc "focus-up"
      "<up>" #'evil-window-up))

(map! :leader
      :desc "focus-up" "<up>" #'evil-window-up)

(map! :leader
    (:prefix ("w" . "extend")
      :desc "focus-down"
      "<down>" #'evil-window-down))

(map! :leader
      :desc "focus-down" "<down>" #'evil-window-down)

(map! :leader
    (:prefix ("w" . "extend")
      :desc "focus-left"
      "<left>" #'evil-window-left))

(map! :leader
      :desc "focus-left" "<left>" #'evil-window-left)

(map! :leader
    (:prefix ("w" . "extend")
      :desc "focus-right"
      "<right>" #'evil-window-right))

(map! :leader
      :desc "focus-right" "<right>" #'evil-window-right)



(defun my-vsplit ()
	""
	(interactive)
	(evil-window-split)
	;;(let ((buffer (generate-new-buffer "untitled")))
	;;	(switch-to-buffer buffer)
	;;)
)


;;
;; split maps
;;

(map! :leader
    (:prefix ("w" . "extend")
      :desc "split to bottom"
      "\"" #'my-vsplit))

(map! :leader
    (:prefix ("w" . "extend")
      :desc "split to right"
      "%" #'evil-window-vsplit))

(map! :leader
      :desc "split-below" "'" #'evil-window-split)
(map! :leader
      :desc "split-below" "\"" #'evil-window-split)

(map! :leader
      :desc "split-right" "5" #'evil-window-vsplit)
(map! :leader
      :desc "split-right" "%" #'evil-window-vsplit)



;;
;; resize maps
;;

(map! :leader
    (:prefix ("w" . "extend")
      :desc "increase-height"
      "=" #'evil-window-increase-height))

(map! :leader
    (:prefix ("w" . "extend")
      :desc "decrease-height"
      "-" #'evil-window-decrease-height))

(map! :leader
    (:prefix ("w" . "extend")
      :desc "increase-width"
      "." #'evil-window-increase-width))

(map! :leader
    (:prefix ("w" . "extend")
      :desc "decrease-height"
      "," #'evil-window-decrease-width))

(global-set-key (kbd "<M-up>") 'shrink-window)
(global-set-key (kbd "<M-down>") 'enlarge-window)
(global-set-key (kbd "<M-left>") 'shrink-window-horizontally)
(global-set-key (kbd "<M-right>") 'enlarge-window-horizontally)


;;
;; editing maps
;;
(global-set-key (kbd "<C-left>") 'beginning-of-line)
(global-set-key (kbd "<C-right>") 'end-of-line)

;;(global-set-key (kbd "<C-n>") 'evil-buffer-new)
;; # https://stackoverflow.com/questions/24720593/setting-shiftarrow-keys-to-select-text-in-emacs-prelude
(global-unset-key (vector (list 'shift 'left)))
(global-unset-key (vector (list 'shift 'right)))
(global-unset-key (vector (list 'shift 'up)))
(global-unset-key (vector (list 'shift 'down)))
(setq prelude-guru nil)
(setq shift-selection-mode t)

(delete-selection-mode t)
(cua-mode +1)
(setq cua-keep-region-after-copy t) ;; Standard Windows behaviour

;; windows like copy/paste/cut/undo/redo
;; https://emacs.stackexchange.com/questions/24496/integrate-cua-mode-with-evil-insert-state
(define-key evil-insert-state-map (kbd "C-c") 'cua-copy-region)
(define-key evil-insert-state-map (kbd "C-v") 'cua-paste)
(define-key evil-insert-state-map (kbd "C-x") 'cua-cut-region)
(define-key evil-insert-state-map (kbd "C-z") 'undo-tree-undo)
(define-key evil-insert-state-map (kbd "C-y") 'undo-tree-redo)

;; https://stackoverflow.com/questions/25791605/emacs-how-do-i-create-a-new-empty-buffer-whenever-creating-a-new-frame
(defun lunaryorn-new-buffer-frame ()
  "Create a new frame with a new empty buffer."
  (interactive)
  (let ((buffer (generate-new-buffer "untitled")))
    (set-buffer-major-mode buffer)
    (display-buffer buffer '(display-buffer-pop-up-frame . nil))))

(defun jaxdad-generate-new-buffer ()
	"generates a new 'untitled' buffer"
	(interactive)
	(generate-new-buffer "untitled")
)

;;(global-set-key (kbd "<C-n>") #'jaxdad-generate-new-buffer)

;;
;; application maps
;;

;; https://www.emacswiki.org/emacs/NeoTree
;; https://emacs.stackexchange.com/questions/37678/neotree-window-not-resizable
(global-set-key [f9] 'neotree-toggle)
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
(global-set-key [f10] '+eshell/toggle)
(global-set-key [f11] '+eshell)
(global-set-key [f12] 'ansi-term)

