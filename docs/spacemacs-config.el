;;; spacemacs-config.el --- Spacemacs configuration for Kawa Code

;; Kawa Code DEV configuration for Spacemacs
;; Add this to your ~/.spacemacs file in the dotspacemacs/user-config section

;; Method 1: Load from a specific path (for development)
;; Replace "/path/to/ca.emacs" with the actual path to your ca.emacs directory
(let ((kawacode-path "~/Code/kawa.emacs"))  ; Adjust this path
  (when (file-exists-p kawacode-path)
    (add-to-list 'load-path kawacode-path)
    (require 'kawacode)))

;; Method 2: If you want to enable Kawa Code by default
;; Uncomment the following line:
;; (kawacode-mode 1)

;; Method 3: If you want to enable it only for specific modes
;; (add-hook 'prog-mode-hook #'kawacode-mode)

;; Optional: Configure Kawa Code settings
;; (setq kawacode-debug t)  ; Enable debug mode
;; (setq kawacode-update-delay 0.3)  ; Faster updates

;; Optional: Add keybindings to Spacemacs
;; (spacemacs/set-leader-keys "a r" 'kawacode-refresh)
;; (spacemacs/set-leader-keys "a l" 'kawacode-show-log-buffer)
