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


(defun moinrpc-bracket-wikilink-p ()
  "."
  (thing-at-point-looking-at "\\[\\[[^|]+?\\]\\]" 100))


(defun moinrpc-wikilink-at-point ()
  "."
  (let ((wikilink-bracket nil)
        (wikilink nil))
    (when (moinrpc-bracket-wikilink-p)
      (setq wikilink-bracket (buffer-substring (match-beginning 0) (match-end 0)))
      (setq wikilink (substring wikilink-bracket 2 -2)))))


(defun moinrpc-open-wikilink-at-point ()
  "."
  (interactive)
  (let ((wikilink (moinrpc-wikilink-at-point))
        (pagename nil))
    (when wikilink
      (if (or (s-starts-with? "/" wikilink)
              (s-starts-with? ".." wikilink))
	  (setq pagename (format "%s%s" current-pagename wikilink))
	(setq pagename wikilink))
      (moinrpc-get-or-create-page-buffer pagename))))


(defun moinrpc-main-page ()
  "."
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
    (make-variable-buffer-local 'moinrpc-buffer-local-current-wiki)
    (setq moinrpc-buffer-local-current-wiki (cdr (assoc *moinrpc-current-wiki* *moinrpc-wiki-settings*)))
    (switch-to-buffer "*moinrpc*")
    t
  ))


(define-derived-mode moinrpc-page-mode outline-mode
  (defvar moinrpc-buffer-local-current-wiki nil)
  (setq outline-regexp "[=\f]+")
  (make-variable-buffer-local 'moinrpc-buffer-local-current-wiki)

  (defvar moinrpc-buffer-local-current-pagename nil)
  (make-variable-buffer-local 'moinrpc-buffer-local-current-pagename)
;  (moinmoin-mode)
  (setq mode-name "moinrpc-page-mode")
  (local-set-key (kbd "C-x C-s") 'moinrpc-save-current-buffer)
  (local-set-key (kbd "C-x C-f") 'helm-moinrpc-find-page)
  (local-set-key (kbd "C-c C-f") 'moinrpc-find-page)
  (local-set-key (kbd "C-c C-o") 'moinrpc-open-wikilink-at-point)
  (local-set-key (kbd "M-RET") 'org-meta-return)
  (local-set-key (kbd "TAB") 'org-cycle))


(define-derived-mode moinrpc-main-mode fundamental-mode
  (defvar moinrpc-buffer-local-current-wiki nil)
  (make-variable-buffer-local 'moinrpc-buffer-local-current-wiki)

  (defvar moinrpc-buffer-local-current-pagename nil)
  (make-variable-buffer-local 'moinrpc-buffer-local-current-pagename)

  (setq mode-name "moinrpc-mode")
  (local-set-key (kbd "g") 'moinrpc-main-page)
  (local-set-key (kbd "C-x C-f") 'helm-moinrpc-find-page)
  (local-set-key (kbd "C-c C-n") 'moinrpc-new-wiki-setting)
  (local-set-key (kbd "C-c C-f") 'moinrpc-find-page)
  (local-set-key (kbd "<tab>") 'forward-button)
  (local-set-key (kbd "<backtab>") 'backward-button))

(provide 'moinrpc-mode)
;;; moinrpc-mode.el ends here
