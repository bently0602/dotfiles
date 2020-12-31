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

(map! :leader
    (:prefix ("w" . "extend")
      :desc "focus-up"
      "<up>" #'evil-window-up))

(map! :leader
    (:prefix ("w" . "extend")
      :desc "focus-down"
      "<down>" #'evil-window-down))

(map! :leader
    (:prefix ("w" . "extend")
      :desc "focus-left"
      "<left>" #'evil-window-left))

(map! :leader
    (:prefix ("w" . "extend")
      :desc "focus-right"
      "<right>" #'evil-window-right))


(defun my-vsplit ()
	""
	(interactive)
	(evil-window-split)
	;;(let ((buffer (generate-new-buffer "untitled")))
	;;	(switch-to-buffer buffer)
	;;)
)

(map! :leader
    (:prefix ("w" . "extend")
      :desc "split to bottom"
      "\"" #'my-vsplit))

(map! :leader
    (:prefix ("w" . "extend")
      :desc "split to right"
      "%" #'evil-window-vsplit))



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


(global-set-key (kbd "<M-S-up>") 'shrink-window)
(global-set-key (kbd "<M-S-down>") 'enlarge-window)
(global-set-key (kbd "<M-S-left>") 'shrink-window-horizontally)
(global-set-key (kbd "<M-S-right>") 'enlarge-window-horizontally)

(global-set-key (kbd "<M-up>") 'evil-window-up)
(global-set-key (kbd "<M-down>") 'evil-window-down)
(global-set-key (kbd "<M-left>") 'evil-window-left)
(global-set-key (kbd "<M-right>") 'evil-window-right)

;;(global-set-key (kbd "<C-\">") 'evil-window-split)
;;(global-set-key (kbd "<C-%>") 'evil-window-vsplit)
;;(global-set-key (kbd "<C-'>") 'evil-window-split)
;;(global-set-key (kbd "<C-5>") 'evil-window-vsplit)

(global-set-key (kbd "<C-S-down>") 'evil-window-split)
(global-set-key (kbd "<C-S-right>") 'evil-window-vsplit)

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



;; (global-set-key (kbd "C-c +") #'evil-window-increase-height)

;; https://stackoverflow.com/questions/25791605/emacs-how-do-i-create-a-new-empty-buffer-whenever-creating-a-new-frame
(defun lunaryorn-new-buffer-frame ()
  "Create a new frame with a new empty buffer."
  (interactive)
  (let ((buffer (generate-new-buffer "untitled")))
    (set-buffer-major-mode buffer)
    (display-buffer buffer '(display-buffer-pop-up-frame . nil))))

;;(global-set-key (kbd "C-c n") #'lunaryorn-new-buffer-frame)

;; https://www.emacswiki.org/emacs/NeoTree
(global-set-key [f1] 'neotree-toggle)
(global-set-key [f11] 'neotree)

;;
(global-set-key [f2] '+eshell/toggle)
(global-set-key [f12] '+eshell)


