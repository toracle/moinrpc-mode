;;; package --- moinmoin xml-rpc client
;;; Commentary: 

;;; Code: 

(require 'xml-rpc)
(require 'moinmoin-mode)
(require 'thingatpt)
(require 's)

(require 'moinrpc-xmlrpc)
(require 'moinrpc-buffer)


;; Variables

(defvar moinrpc-buffer-prefix "*moin: ")
(defvar moinrpc-buffer-postfix "*")

(defvar moinrpc-wiki-settings nil)
(defvar moinrpc-current-wiki nil)

(defvar moinrpc-xmlrpc-api-token nil)


;;; Wiki setting

(defvar moinrpc-settings-file (concat user-emacs-directory "remote-moin"))

(when
    (file-readable-p moinrpc-settings-file)
  (load moinrpc-settings-file))

(defun moinrpc-create-wiki-setting (wiki-alias xmlrpc-endpoint username)
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
  (interactive)
  (let
      ((wiki-alias (read-string "Wiki alias: "))
       (xmlrpc-endpoint (read-string "XML-RPC endpoint: "))
       (username (read-string "Username: ")))
    (moinrpc-create-wiki-setting wiki-alias xmlrpc-endpoint username)))

(defun moinrpc-add-wiki-setting-to-global (wiki-setting)
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
  (interactive)
  (moinrpc-add-wiki-setting-to-global (moinrpc-create-wiki-setting-i))
  (moinrpc-save-wiki-settings)
  (moinrpc-main-page))

(defun moinrpc-get-keys (list)
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

(defun helm-moinrpc-find-page ()
  (interactive)
  (let
      ((all-pages (moinrpc-get-list-content current-wiki)))
    (helm :sources '(
		     ((name . "All wiki pages")
		      (candidates . all-pages)
		      (action . (("Open" . moinrpc-get-or-create-page-buffer))))
		     ((name . "fallback")
		      (dummy)
		      (action . (("Create" . (lambda (pagename) (progn
								  (let
								      ((buffer nil))
								    (setq buffer (moinrpc-create-page-buffer current-wiki pagename))
								    (moinrpc-fill-page-buffer-content buffer))))
				  )))))
	  :prompt "Find Page: "
	  :buffer "*helm-moinrpc-find-pages*"
	  )))

(defun moinrpc-helm-find-page (button)
  (let
      (
       (wiki-alias (button-label button))
       (current-wiki nil))
    (setq current-wiki (cdr (assoc wiki-alias moinrpc-wiki-settings)))
    (helm-moinrpc-find-page)))


(defun moinrpc-main-page ()
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
      (insert-button wiki-alias 'follow-link "\C-m" 'action 'moinrpc-helm-find-page)
      (newline)
      )
    (read-only-mode)
    (moinrpc-main-mode)
    (make-variable-buffer-local 'current-wiki)
    (setq current-wiki (cdr (assoc moinrpc-current-wiki moinrpc-wiki-settings)))
    (switch-to-buffer "*moinrpc*")
    t
  ))

(define-derived-mode moinrpc-page-mode fundamental-mode
  (moinmoin-mode)
  (setq mode-name "moinrpc-page-mode")
  (local-set-key (kbd "C-x C-s") 'moinrpc-save-current-buffer)
;  (local-set-key (kbd "C-x C-f") 'moinrpc-find-buffer)
  (local-set-key (kbd "C-x C-f") 'helm-moinrpc-find-page)
  )

(define-derived-mode moinrpc-main-mode fundamental-mode
  (setq mode-name "moinrpc-mode")
  (local-set-key (kbd "n") 'moinrpc-new-wiki-setting)
  (local-set-key (kbd "g") 'moinrpc-main-page)
  (local-set-key (kbd "C-x C-f") 'helm-moinrpc-find-page)
  )

(provide 'moinrpc-mode)
;;; moin.el ends here
