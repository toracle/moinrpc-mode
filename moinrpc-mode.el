;;; package --- moinmoin xml-rpc client
;;; Commentary:
;;; some comment

;;; Code:

(require 'xml-rpc)
(require 'thingatpt)
(require 's)

(require 'moinrpc-buffer)
(require 'moinrpc-render)
(require 'moinrpc-conf)
(require 'moinrpc-common)


;;; Wiki setting


(define-derived-mode moinrpc-page-mode outline-mode
  (setq mode-name "moinrpc-page-mode")

  (setq outline-regexp "[=\f]+")

  (local-set-key (kbd "C-x C-s") 'moinrpc-save-page)
  (local-set-key (kbd "C-x C-f") 'moinrpc-helm-find-page)
  (local-set-key (kbd "C-c C-f") 'moinrpc-find-page)
  (local-set-key (kbd "C-c C-o") 'moinrpc-open-wikilink-at-point)
  (local-set-key (kbd "C-c C-l") 'moinrpc-insert-wikilink)
  (local-set-key (kbd "C-c C-a") 'moinrpc-list-attachments)
  (local-set-key (kbd "C-c t 1") 'moinrpc-wrap-title-level-1)
  (local-set-key (kbd "C-c t 2") 'moinrpc-wrap-title-level-2)
  (local-set-key (kbd "C-c t 3") 'moinrpc-wrap-title-level-3)
  (local-set-key (kbd "C-c t 4") 'moinrpc-wrap-title-level-4)
  (local-set-key (kbd "C-c t 5") 'moinrpc-wrap-title-level-5)
  (local-set-key (kbd "C-c m r") 'moinrpc-search-backlinks)
  (local-set-key (kbd "C-c m s") 'moinrpc-search-pages)
  (local-set-key (kbd "M-RET") 'org-meta-return)
  (local-set-key (kbd "TAB") 'moinrpc-cycle))


(define-derived-mode moinrpc-list-mode fundamental-mode
  (setq mode-name "moinrpc-list-mode")

  (local-set-key (kbd "q") 'quit-window)
  (local-set-key (kbd "C-x C-f") 'moinrpc-helm-find-page)
  (local-set-key (kbd "C-c C-f") 'moinrpc-find-page)
  (local-set-key (kbd "C-c C-r") 'moinrpc-recent-changes)
  (local-set-key (kbd "C-c m s") 'moinrpc-search-pages)
  (local-set-key (kbd "TAB") 'forward-button)
  (local-set-key (kbd "<backtab>") 'backward-button))


(define-derived-mode moinrpc-attachment-mode fundamental-mode
  (setq mode-name "moinrpc-attachment-mode")

  (local-set-key (kbd "C-c C-r") 'moinrpc-recent-changes)
  (local-set-key (kbd "q") 'quit-window)
  (local-set-key (kbd "TAB") 'forward-button)
  (local-set-key (kbd "<backtab>") 'backward-button)
  (local-set-key (kbd "a") 'moinrpc-upload-attachment)
  (local-set-key (kbd "d") 'moinrpc-delete-attachment)
  (local-set-key (kbd "g") 'moinrpc-list-attachments))


(define-derived-mode moinrpc-search-mode fundamental-mode
  (setq mode-name "moinrpc-search-mode")

  (local-set-key (kbd "C-c C-r") 'moinrpc-recent-changes)
  (local-set-key (kbd "q") 'quit-window)
  (local-set-key (kbd "TAB") 'forward-button)
  (local-set-key (kbd "<backtab>") 'backward-button))


(define-derived-mode moinrpc-front-mode fundamental-mode
  (setq mode-name "moinrpc-front-mode")

  (local-set-key (kbd "q") 'quit-window)
  (local-set-key (kbd "C-c C-r") 'moinrpc-recent-changes)
  (local-set-key (kbd "C-x C-f") 'moinrpc-helm-find-page)
  (local-set-key (kbd "C-c C-f") 'moinrpc-find-page)
  (local-set-key (kbd "C-c C-n") 'moinrpc-new-wiki-setting)
  (local-set-key (kbd "C-c m r") 'moinrpc-search-backlinks)
  (local-set-key (kbd "C-c m s") 'moinrpc-search-pages)
  (local-set-key (kbd "TAB") 'forward-button)
  (local-set-key (kbd "<backtab>") 'backward-button))


(define-derived-mode moinrpc-main-mode fundamental-mode
  (setq mode-name "moinrpc-main-mode")

  (local-set-key (kbd "q") 'quit-window)
  (local-set-key (kbd "g") 'moinrpc-main-page)
  (local-set-key (kbd "C-c C-n") 'moinrpc-new-wiki-setting)
  (local-set-key (kbd "TAB") 'forward-button)
  (local-set-key (kbd "<backtab>") 'backward-button))


(provide 'moinrpc-mode)
;;; moinrpc-mode.el ends here
