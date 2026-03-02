;;; moinrpc-mode.el --- MoinMoin XML-RPC client for Emacs
;; -*- lexical-binding: t -*-
;; Author: Jeongsoo Park <toracle@gmail.com>
;; URL: https://github.com/toracle/moinrpc-mode
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (xml-rpc "1.0") (s "1.12.0") (thingatpt "1.0"))
;; Keywords: convenience xml moinmoin
;; SPDX-License-Identifier: MIT

;;; Commentary:
;; Emacs client for MoinMoin wiki via XML-RPC.

;;; Code:

(require 'xml-rpc)
(require 'cl-lib)
(require 'thingatpt)
(require 's)

(require 'moinrpc-common)
(require 'moinrpc-conf)
(require 'moinrpc-xmlrpc)
(require 'moinrpc-buffer)
(require 'moinrpc-render)


;;; Wiki setting


;;;###autoload
(define-derived-mode moinrpc-page-mode outline-mode
  "Major mode for viewing/editing MoinMoin wiki pages.")

(define-key moinrpc-page-mode-map (kbd "C-x C-s") 'moinrpc-save-page)
(define-key moinrpc-page-mode-map (kbd "C-x C-f") 'moinrpc-helm-find-page)
(define-key moinrpc-page-mode-map (kbd "C-c C-f") 'moinrpc-find-page)
(define-key moinrpc-page-mode-map (kbd "C-c C-o") 'moinrpc-open-wikilink-at-point)
(define-key moinrpc-page-mode-map (kbd "C-c C-l") 'moinrpc-insert-wikilink)
(define-key moinrpc-page-mode-map (kbd "C-c C-a") 'moinrpc-list-attachments)
(define-key moinrpc-page-mode-map (kbd "C-c t 1") 'moinrpc-wrap-title-level-1)
(define-key moinrpc-page-mode-map (kbd "C-c t 2") 'moinrpc-wrap-title-level-2)
(define-key moinrpc-page-mode-map (kbd "C-c t 3") 'moinrpc-wrap-title-level-3)
(define-key moinrpc-page-mode-map (kbd "C-c t 4") 'moinrpc-wrap-title-level-4)
(define-key moinrpc-page-mode-map (kbd "C-c t 5") 'moinrpc-wrap-title-level-5)
(define-key moinrpc-page-mode-map (kbd "C-c m r") 'moinrpc-search-backlinks)
(define-key moinrpc-page-mode-map (kbd "C-c m s") 'moinrpc-search-pages)
(define-key moinrpc-page-mode-map (kbd "C-y") 'moinrpc-yank)
(define-key moinrpc-page-mode-map (kbd "M-RET") 'org-meta-return)
(define-key moinrpc-page-mode-map (kbd "TAB") 'moinrpc-cycle)


;;;###autoload
(define-derived-mode moinrpc-list-mode fundamental-mode
  "Mode for listing wiki entries.")

(define-key moinrpc-list-mode-map (kbd "q") 'quit-window)
(define-key moinrpc-list-mode-map (kbd "C-x C-f") 'moinrpc-helm-find-page)
(define-key moinrpc-list-mode-map (kbd "C-c C-f") 'moinrpc-find-page)
(define-key moinrpc-list-mode-map (kbd "C-c C-r") 'moinrpc-recent-changes)
(define-key moinrpc-list-mode-map (kbd "C-c m s") 'moinrpc-search-pages)
(define-key moinrpc-list-mode-map (kbd "TAB") 'forward-button)
(define-key moinrpc-list-mode-map (kbd "<backtab>") 'backward-button)
(define-key moinrpc-list-mode-map (kbd "n") 'forward-button)
(define-key moinrpc-list-mode-map (kbd "p") 'backward-button)


;;;###autoload
(define-derived-mode moinrpc-attachment-mode fundamental-mode
  "Mode for listing attachments.")

(define-key moinrpc-attachment-mode-map (kbd "C-c C-r") 'moinrpc-recent-changes)
(define-key moinrpc-attachment-mode-map (kbd "q") 'quit-window)
(define-key moinrpc-attachment-mode-map (kbd "TAB") 'forward-button)
(define-key moinrpc-attachment-mode-map (kbd "<backtab>") 'backward-button)
(define-key moinrpc-attachment-mode-map (kbd "n") 'forward-button)
(define-key moinrpc-attachment-mode-map (kbd "p") 'backward-button)
(define-key moinrpc-attachment-mode-map (kbd "a") 'moinrpc-upload-attachment)
(define-key moinrpc-attachment-mode-map (kbd "d") 'moinrpc-delete-attachment)
(define-key moinrpc-attachment-mode-map (kbd "g") 'moinrpc-list-attachments)


;;;###autoload
(define-derived-mode moinrpc-search-mode fundamental-mode
  "Mode for search results.")

(define-key moinrpc-search-mode-map (kbd "C-c C-r") 'moinrpc-recent-changes)
(define-key moinrpc-search-mode-map (kbd "q") 'quit-window)
(define-key moinrpc-search-mode-map (kbd "TAB") 'forward-button)
(define-key moinrpc-search-mode-map (kbd "<backtab>") 'backward-button)


;;;###autoload
(define-derived-mode moinrpc-front-mode fundamental-mode
  "Front page mode.")

(define-key moinrpc-front-mode-map (kbd "q") 'quit-window)
(define-key moinrpc-front-mode-map (kbd "C-c C-r") 'moinrpc-recent-changes)
(define-key moinrpc-front-mode-map (kbd "C-x C-f") 'moinrpc-helm-find-page)
(define-key moinrpc-front-mode-map (kbd "C-c C-f") 'moinrpc-find-page)
(define-key moinrpc-front-mode-map (kbd "C-c C-n") 'moinrpc-new-wiki-setting)
(define-key moinrpc-front-mode-map (kbd "C-c m r") 'moinrpc-search-backlinks)
(define-key moinrpc-front-mode-map (kbd "C-c m s") 'moinrpc-search-pages)
(define-key moinrpc-front-mode-map (kbd "TAB") 'forward-button)
(define-key moinrpc-front-mode-map (kbd "<backtab>") 'backward-button)


;;;###autoload
(define-derived-mode moinrpc-main-mode fundamental-mode
  "Main mode for moinrpc.")

(define-key moinrpc-main-mode-map (kbd "q") 'quit-window)
(define-key moinrpc-main-mode-map (kbd "g") 'moinrpc-main-page)
(define-key moinrpc-main-mode-map (kbd "C-c C-n") 'moinrpc-new-wiki-setting)
(define-key moinrpc-main-mode-map (kbd "TAB") 'forward-button)
(define-key moinrpc-main-mode-map (kbd "<backtab>") 'backward-button)


(provide 'moinrpc-mode)
;;; moinrpc-mode.el ends here
