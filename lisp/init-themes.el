;;; init-themes.el --- Defaults for themes -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(straight-use-package 'color-theme-sanityinc-solarized)
(straight-use-package 'color-theme-sanityinc-tomorrow)
;; Doom themes - excellent collection of high-quality themes
(straight-use-package 'doom-themes)

;; Don't prompt to confirm theme safety. This avoids problems with
;; first-time startup on Emacs > 26.3.
(setq custom-safe-themes t)

;; If you don't customize it, this is the theme you get.
(setq-default custom-enabled-themes '(doom-oceanic-next))

;; Ensure that themes will be applied even if they have not been customized
(defun reapply-themes ()
  "Forcibly load the themes listed in `custom-enabled-themes'."
  (dolist (theme custom-enabled-themes)
    (unless (custom-theme-p theme)
      (load-theme theme)))
  (custom-set-variables `(custom-enabled-themes (quote ,custom-enabled-themes))))

(add-hook 'after-init-hook 'reapply-themes)

;; Configure doom-themes
(after-load 'doom-themes
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)

  ;; Corrects (and improves) org-mode's native fontification
  (doom-themes-org-config))

;; Custom theme adjustments
(defun kyle/customize-oceanic-comments ()
  "Make comments lighter in the oceanic theme for better contrast."
  (when (member 'doom-oceanic-next custom-enabled-themes)
    (custom-set-faces
     ;; Make comments lighter - using a brighter gray for better contrast
     '(font-lock-comment-face ((t (:foreground "#8fa5b8" :slant italic))))
     '(font-lock-comment-delimiter-face ((t (:foreground "#8fa5b8"))))
     ;; Also lighten doc strings to be consistent
     '(font-lock-doc-face ((t (:foreground "#9fb5c8" :slant italic))))
     ;; Line numbers area - medium grey background
     '(line-number ((t (:foreground "#6c7b89" :background "#2b3440"))))
     '(line-number-current-line ((t (:foreground "#8fa5b8" :background "#2b3440" :weight bold))))
     ;; Fringe (left/right sidebars)
     '(fringe ((t (:background "#2b3440"))))
     ;; Magit diff hunk headers (@@...@@) - change from purple to grey
     '(magit-diff-hunk-heading-highlight ((t (:foreground "#8fa5b8" :background "#222932")))))))

;; Apply comment customization after theme loads
(add-hook 'after-init-hook 'kyle/customize-oceanic-comments)

;;------------------------------------------------------------------------------
;; Theme switching functions - easy access to the best themes
;;------------------------------------------------------------------------------
(defun light ()
  "Activate a light color theme."
  (interactive)
  (setq custom-enabled-themes '(sanityinc-tomorrow-day))
  (reapply-themes))

(defun dark ()
  "Activate a dark color theme."
  (interactive)
  (setq custom-enabled-themes '(sanityinc-tomorrow-bright))
  (reapply-themes))

(defun super-dark ()
  "Activate the previous dark color theme (sanityinc-tomorrow-bright)."
  (interactive)
  (setq custom-enabled-themes '(sanityinc-tomorrow-bright))
  (reapply-themes))

(defun nord ()
  "Activate the Doom Nord theme - beautiful blue-gray palette, easy on the eyes."
  (interactive)
  (setq custom-enabled-themes '(doom-nord))
  (reapply-themes))

(defun gruvbox ()
  "Activate the Doom Gruvbox theme - warm, retro colors with excellent contrast."
  (interactive)
  (setq custom-enabled-themes '(doom-gruvbox))
  (reapply-themes))

(defun doom-one ()
  "Activate the Doom One theme - inspired by Atom One Dark, very popular."
  (interactive)
  (setq custom-enabled-themes '(doom-one))
  (reapply-themes))

(defun oceanic ()
  "Activate the Doom Oceanic Next theme - ocean-inspired blue theme."
  (interactive)
  (setq custom-enabled-themes '(doom-oceanic-next))
  (reapply-themes))

(defun material ()
  "Activate the Doom Material theme - Google Material Design colors."
  (interactive)
  (setq custom-enabled-themes '(doom-material))
  (reapply-themes))

(defun intellij ()
  "Activate the JetBrains Darcula-inspired theme."
  (interactive)
  ;; We can add back jetbrains-darcula-theme if you want to keep it as an option
  (setq custom-enabled-themes '(doom-one))
  (reapply-themes))


(when (straight-use-package 'dimmer)
  (setq-default dimmer-fraction 0.15)
  (add-hook 'after-init-hook 'dimmer-mode)
  ;; TODO: file upstream as a PR
  (after-load 'dimmer
    ;; Exclude magit buffers to prevent "wrong-type-argument listp default" error
    ;; This is a known issue with dimmer + magit-delta interaction
    (setq dimmer-buffer-exclusion-regexps '("^magit.*" "^\\*Messages\\*"))
    (advice-add 'frame-set-background-mode :after (lambda (&rest args) (dimmer-process-all)))))


(provide 'init-themes)
;;; init-themes.el ends here
