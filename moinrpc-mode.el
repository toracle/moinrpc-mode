;;; package --- moinmoin xml-rpc client
;;; Commentary:
;;; some comment

;;; Code:

(require 'xml-rpc)
;(require 'moinmoin-mode)
(require 'thingatpt)
(require 's)

(require 'moinrpc-buffer)


;; Variables

(defvar moinrpc-wiki-settings nil)
(defvar moinrpc-current-wiki nil)

;;; Wiki setting

(defvar moinrpc-settings-file (concat user-emacs-directory "remote-moin"))

(when
    (file-readable-p moinrpc-settings-file)
  (load moinrpc-settings-file))

(defun moinrpc-create-wiki-setting (wiki-alias xmlrpc-endpoint username)
  "WIKI-ALIAS XMLRPC-ENDPOINT USERNAME."
  (let
      ((xmlrpc-api-token nil)
       (wiki-setting nil))
    (setq wiki-setting
	  (list
	   (cons 'wiki-alias wiki-alias)
	   (cons 'xmlrpc-endpoint xmlrpc-endpoint)
	   (cons 'username username)))
    (setq xmlrpc-api-token (moinrpc-get-auth-token wiki-setting))
    (list
     (cons 'wiki-alias wiki-alias)
     (cons 'xmlrpc-endpoint xmlrpc-endpoint)
     (cons 'xmlrpc-api-token xmlrpc-api-token)
     (cons 'username username))))

(defun moinrpc-create-wiki-setting-i ()
  "."
  (interactive)
  (let
      ((wiki-alias (read-string "Wiki alias: "))
       (xmlrpc-endpoint (read-string "XML-RPC endpoint: "))
       (username (read-string "Username: ")))
    (moinrpc-create-wiki-setting wiki-alias xmlrpc-endpoint username)))

(defun moinrpc-add-wiki-setting-to-global (wiki-setting)
  "WIKI-SETTING."
  (let
      ((wiki-alias (cdr (assoc 'wiki-alias wiki-setting)))
       (wiki-settings nil))
    (message (format "%S" moinrpc-wiki-settings))
    (message (format "%S" wiki-alias))
    (when
	(not (eq moinrpc-wiki-settings nil))
	(progn
	  (setq wiki-settings (assq-delete-all wiki-alias moinrpc-wiki-settings))
	  (message (format "%S" wiki-settings))))
    (add-to-list 'wiki-settings (cons wiki-alias wiki-setting))
    (setq moinrpc-wiki-settings wiki-settings)
    (setq moinrpc-current-wiki wiki-alias)
    (moinrpc-save-wiki-settings)))

(defun moinrpc-save-wiki-settings ()
  "."
  (with-current-buffer
      (find-file-noselect moinrpc-settings-file)
    (erase-buffer)
    (insert (format "(setq moinrpc-wiki-settings '%S)" moinrpc-wiki-settings))
    (newline)
    (insert (format "(setq moinrpc-current-wiki %S)" moinrpc-current-wiki))
    (save-buffer)
    t
    ))

(defun moinrpc-new-wiki-setting ()
  "."
  (interactive)
  (moinrpc-add-wiki-setting-to-global (moinrpc-create-wiki-setting-i))
  (moinrpc-save-wiki-settings)
  (moinrpc-main-page))

(defun moinrpc-get-keys (list)
  "LIST."
  (let
      ((i 0)
       (size (list-length list))
       (keys nil))
    (while (< i size)
      (let
	  ((key nil))
	(setq key (car (nth i list)))
	(setq i (+ i 1))
	(add-to-list 'keys key)))
    keys))


(defun moinrpc-bracket-wikilink-p ()
  "."
  (thing-at-point-looking-at "\\[\\[[^|]+?\\]\\]" 100))


(defun moinrpc-wikilink-at-point ()
  "."
  (let
      ((wikilink-bracket nil)
       (wikilink nil))
    (when
	(moinrpc-bracket-wikilink-p)
      (setq wikilink-bracket (buffer-substring (match-beginning 0) (match-end 0)))
      (setq wikilink (substring wikilink-bracket 2 -2)))))
    

(defun moinrpc-open-wikilink-at-point ()
  "."
  (interactive)
  (let
      (
       (wikilink (moinrpc-wikilink-at-point))
       (pagename nil))
    (when
	wikilink
      (if
	  (or
	   (s-starts-with? "/" wikilink)
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
    (dolist (wiki-alias (moinrpc-get-keys moinrpc-wiki-settings))
      (insert " * ")
      (insert-button wiki-alias
		     'follow-link "\C-m"
		     'action 'moinrpc-helm-find-page)
      (newline)
      )
    (read-only-mode)
    (moinrpc-main-mode)
    (make-variable-buffer-local 'moinrpc-buffer-local-current-wiki)
    (setq moinrpc-buffer-local-current-wiki (cdr (assoc moinrpc-current-wiki moinrpc-wiki-settings)))
    (switch-to-buffer "*moinrpc*")
    t
  ))

(define-derived-mode moinrpc-page-mode fundamental-mode
  (defvar moinrpc-buffer-local-current-wiki nil)
  (make-variable-buffer-local 'moinrpc-buffer-local-current-wiki)

  (defvar moinrpc-buffer-local-current-pagename nil)
  (make-variable-buffer-local 'moinrpc-buffer-local-current-pagename)
;  (moinmoin-mode)
  (setq mode-name "moinrpc-page-mode")
  (local-set-key (kbd "C-x C-s") 'moinrpc-save-current-buffer)
;  (local-set-key (kbd "C-x C-f") 'moinrpc-find-buffer)
  (local-set-key (kbd "C-x C-f") 'helm-moinrpc-find-page)
  (local-set-key (kbd "C-c C-o") 'moinrpc-open-wikilink-at-point)
  )

(define-derived-mode moinrpc-main-mode fundamental-mode
  (defvar moinrpc-buffer-local-current-wiki nil)
  (make-variable-buffer-local 'moinrpc-buffer-local-current-wiki)

  (defvar moinrpc-buffer-local-current-pagename nil)
  (make-variable-buffer-local 'moinrpc-buffer-local-current-pagename)

  (setq mode-name "moinrpc-mode")
  (local-set-key (kbd "n") 'moinrpc-new-wiki-setting)
  (local-set-key (kbd "g") 'moinrpc-main-page)
  (local-set-key (kbd "C-x C-f") 'helm-moinrpc-find-page)
  )

(provide 'moinrpc-mode)
;;; moinrpc-mode.el ends here
