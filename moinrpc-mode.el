;;; package --- moinmoin xml-rpc client
;;; Commentary:
;;; some comment

;;; Code:

(require 'xml-rpc)
;(require 'moinmoin-mode)
(require 'thingatpt)
(require 's)

(require 'moinrpc-buffer)
(require 'moinrpc-conf)


;;; Wiki setting

(defun moinrpc-create-wiki-setting-i ()
  "."
  (interactive)
  (let* ((wiki-alias (read-string "Wiki alias: "))
         (xmlrpc-endpoint (read-string "XML-RPC endpoint: "))
         (username (read-string "Username: ")))
    (moinrpc-create-wiki-setting wiki-alias
                                 xmlrpc-endpoint
                                 username)))


(defvar moinrpc-regex-bracket-wikilink
  "\\[\\[[^|]+?\\]\\]")


(defvar moinrpc-regex-wikilink
  "\\([A-Z][a-z]+\\)+?")


(defun moinrpc-wikilink-p ()
  "."
  (thing-at-point-looking-at moinrpc-regex-wikilink
                             100))


(defun moinrpc-bracket-wikilink-p ()
  "."
  (thing-at-point-looking-at moinrpc-regex-bracket-wikilink
                             100))


(defun moinrpc-wikilink-at-point ()
  "."
  (cond ((moinrpc-bracket-wikilink-p)
         (let ((wikilink-bracket (buffer-substring (match-beginning 0)
                                                   (match-end 0))))
           (substring wikilink-bracket 2 -2)))
        ((moinrpc-wikilink-p)
         (buffer-substring (match-beginning 0)
                           (match-end 0)))))


(defun moinrpc-rel-wikilink-to-abs (wikilink parent)
  (if (or (s-starts-with? "/" wikilink)
          (s-starts-with? ".." wikilink))
      (format "%s%s" parent wikilink)
    wikilink))


(defun moinrpc-open-wikilink-at-point ()
  "."
  (interactive)
  (let ((wikilink (moinrpc-wikilink-at-point))
        (pagename nil))
    (when wikilink
      (moinrpc-get-or-create-page-buffer (moinrpc-rel-wikilink-to-abs
                                          wikilink
                                          moinrpc-buffer-local-current-pagename)))))


(defun moinrpc-wrap-title-level-1 ()
  (interactive)
  (moinrpc-wrap-title-level-n 1))


(defun moinrpc-wrap-title-level-2 ()
  (interactive)
  (moinrpc-wrap-title-level-n 2))


(defun moinrpc-wrap-title-level-3 ()
  (interactive)
  (moinrpc-wrap-title-level-n 3))


(defun moinrpc-wrap-title-level-4 ()
  (interactive)
  (moinrpc-wrap-title-level-n 4))


(defun moinrpc-wrap-title-level-n (level)
  (let ((m (point-marker)))
    (beginning-of-line)
    (dotimes (_ level)
             (insert "="))
    (insert " ")
    (end-of-line)
    (insert " ")
    (dotimes (_ level)
             (insert "="))
    (goto-char m)
  ))


(defun moinrpc-main-page ()
  "Create a wiki list buffer."
  (interactive)
  (with-current-buffer
      (get-buffer-create "*moinrpc*")
    (read-only-mode -1)
    (erase-buffer)
    (insert "MoinRPC Wiki List")
    (newline)
    (newline)
    (dolist (wiki-alias (moinrpc-get-keys *moinrpc-wiki-settings*))
      (insert " * ")
      (insert-button wiki-alias
		     'follow-link "\C-m"
		     'action 'moinrpc-helm-find-page)
      (newline))
    (read-only-mode)
    (moinrpc-main-mode)
    (setq-local moinrpc-buffer-local-current-wiki
                (cdr (assoc *moinrpc-current-wiki* *moinrpc-wiki-settings*)))
    (switch-to-buffer "*moinrpc*")
    t
  ))


(define-derived-mode moinrpc-page-mode outline-mode
  (setq mode-name "moinrpc-page-mode")

  (setq outline-regexp "[=\f]+")
;  (moinmoin-mode)
  (local-set-key (kbd "C-x C-s") 'moinrpc-save-current-buffer)
  (local-set-key (kbd "C-x C-f") 'helm-moinrpc-find-page)
  (local-set-key (kbd "C-c C-f") 'moinrpc-find-page)
  (local-set-key (kbd "C-c C-o") 'moinrpc-open-wikilink-at-point)
  (local-set-key (kbd "C-c t 1") 'moinrpc-wrap-title-level-1)
  (local-set-key (kbd "C-c t 2") 'moinrpc-wrap-title-level-2)
  (local-set-key (kbd "C-c t 3") 'moinrpc-wrap-title-level-3)
  (local-set-key (kbd "C-c t 4") 'moinrpc-wrap-title-level-4)
  (local-set-key (kbd "M-RET") 'org-meta-return)
  (local-set-key (kbd "TAB") 'org-cycle)
  (local-set-key (kbd "C-c a") 'moinrpc-list-attachment))


(define-derived-mode moinrpc-list-mode fundamental-mode
  (setq mode-name "moinrpc-list-mode")

  (local-set-key (kbd "<tab>") 'forward-button)
  (local-set-key (kbd "<backtab>") 'backward-button))


(define-derived-mode moinrpc-attachment-mode fundamental-mode
  (setq mode-name "moinrpc-attachment-mode")

  (local-set-key (kbd "<tab>") 'forward-button)
  (local-set-key (kbd "<backtab>") 'backward-button)
  (local-set-key (kbd "a") 'moinrpc-upload-attachment)
  (local-set-key (kbd "g") 'moinrpc-fill-list-attachment))


(define-derived-mode moinrpc-main-mode fundamental-mode
  (setq mode-name "moinrpc-mode")

  (local-set-key (kbd "g") 'moinrpc-main-page)
  (local-set-key (kbd "C-x C-f") 'helm-moinrpc-find-page)
  (local-set-key (kbd "C-c C-n") 'moinrpc-new-wiki-setting)
  (local-set-key (kbd "C-c C-f") 'moinrpc-find-page)
  (local-set-key (kbd "<tab>") 'forward-button)
  (local-set-key (kbd "<backtab>") 'backward-button))

(provide 'moinrpc-mode)
;;; moinrpc-mode.el ends here
