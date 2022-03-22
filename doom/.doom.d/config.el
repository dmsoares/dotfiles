;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Decio Soares"
      user-mail-address "decio.msoares@gmail.com")

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
(setq doom-theme 'doom-gruvbox)

(setq doom-font (font-spec :family "mononoki" :size 16)
      doom-variable-pitch-font (font-spec :family "Ubuntu")) ; inherits `doom-font''s :size
(after! doom-themes
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t))


;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Documents/Org/")
(setq org-agenda-files (directory-files-recursively "~/Documents/Org/" "\.org$"))

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)


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


;; Disable evil-snipe
(after! evil-snipe
  (evil-snipe-mode -1))

;; Window splits config
;; Change focus to new window
(setq evil-vsplit-window-right t
      evil-split-window-below t)

;; Org Roam
(setq org-roam-directory "~/Documents/Roam")

(use-package! websocket
  :after org-roam)

(use-package! org-roam-ui
  :after org-roam
  :hook (org-roam . org-roam-ui-mode)
  :config
)

;; Lsp
;; Mappings
(map!
 :after company
 :map company-active-map
 "RET" nil
 "<return>" nil
 [tab] #'company-complete-selection
 "TAB" #'company-complete-selection)
;; Haskell
(after!
  (setq lsp-haskell-formatting-provider "stylish-haskell"))
(setq haskell-stylish-on-save t)

;; Settings
(setq lsp-ui-doc-enable nil)

;; Emacsclient workspaces
;; use main workspace when starting new emacsclient
(after! persp-mode
  (setq persp-emacsclient-init-frame-behaviour-override "main"))
