;;; doom-config.el --- Doom Emacs configuration for Kawa Code

;; Kawa Code DEV configuration for Doom Emacs
;; Add this to your ~/.doom.d/config.el file

;; Method 1: Load from a specific path (for development)
;; Replace "/path/to/ca.emacs" with the actual path to your ca.emacs directory
(let ((kawacode-path "~/Code/kawa.emacs"))  ; Adjust this path
  (when (file-exists-p kawacode-path)
    (add-to-list 'load-path kawacode-path)
    (require 'kawacode)))

;; Method 2: Enable Kawa Code by default
;; (kawacode-mode 1)

;; Method 3: Enable for specific modes
;; (add-hook 'prog-mode-hook #'kawacode-mode)

;; Optional: Configure settings
;; (setq kawacode-debug t)
;; (setq kawacode-update-delay 0.3)

;; Optional: Add keybindings
;; (map! :leader
;;       (:prefix ("a" . "kawacode")
;;        "r" #'kawacode-refresh
;;        "l" #'kawacode-show-log-buffer))
