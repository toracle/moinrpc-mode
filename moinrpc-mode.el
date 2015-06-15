;;; package --- moinmoin xml-rpc client
;;; Commentary: 

;;; Code: 

(require 'xml-rpc)
(require 'moinmoin-mode)


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
    (moinrpc-create-wiki-settings wiki-alias xmlrpc-endpoint username)))

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
  (moinrpc-add-wiki-setting-to-global (moinrpc-create-wiki-setting-i))
  (moinrpc-save-wiki-settings))


;; Raw wiki-xmlrpc functions

(defun moinrpc-check-xmlrpc-response (response)
  (let
      ((response-status (car response)))
    (when
	(not (string= (car response-status) "SUCCESS"))
      (when
	  (and
	   (string= "INVALID" (cdr (assoc "faultCode" response-status)))
	   (string= "Invalid token." (cdr (assoc "faultString" response-status))))
	(moinrpc-get-auth-token)
	(moinrpc-save-wiki-settings)))))
	
(defun moinrpc-encode-xml-rpc-multi-each-method (method-name &rest params)
  (list
   (cons "methodName" method-name)
   (cons "params" (vconcat params))))

(defun moinrpc-encode-xml-rpc-multi-method (wiki method-name &rest params)
  (list
    (moinrpc-encode-xml-rpc-multi-each-method "applyAuthToken" (cdr (assoc 'xmlrpc-api-token wiki)))
    (apply 'moinrpc-encode-xml-rpc-multi-each-method method-name params)))

(defun moinrpc-xml-rpc-multi-method-call (wiki method-name &rest params)
  (let
      ((response nil)
       (url (cdr (assoc 'xmlrpc-endpoint wiki)))
       (call-message (apply 'moinrpc-encode-xml-rpc-multi-method
			    wiki
			    method-name
			    params)))
    (setq response
	  (xml-rpc-method-call url
			       'system.multicall
			       call-message))
    (moinrpc-check-xmlrpc-response response)
    (caar (cdr response))))

(defun moinrpc-set-auth-token-to-current (token wiki-setting)
    (setq wiki-setting (assq-delete-all 'xmlrpc-api-token wiki-setting))
    (add-to-list 'wiki-setting (cons 'xmlrpc-api-token token))
    )

(defun moinrpc-get-auth-token (wiki)
  (let
      ((token nil)
       (password (read-passwd "Password: ")))
    (setq token
	  (xml-rpc-method-call (cdr (assoc 'xmlrpc-endpoint wiki))
			       'getAuthToken
			       (cdr (assoc 'username wiki))
			       password))
    ))
    
(defun moinrpc-get-page-content (wiki pagename)
  "Return raw wiki content string of a page.
Specify WIKI-ALIAS with a PAGENAME."
  (moinrpc-xml-rpc-multi-method-call wiki
				  "getPage"
				  pagename))

(defun moinrpc-save-page-content (wiki pagename content)
  "Save wiki content with a pagename."
  (moinrpc-xml-rpc-multi-method-call wiki
				  "putPage"
				  pagename
				  content))

(defun moinrpc-get-list-content (wiki)
  "Return a list of all page names from WIKI."
  (moinrpc-xml-rpc-multi-method-call wiki
				  "getAllPages"))

;; Buffer related

(defun moinrpc-buffer-name (pagename)
  "Return moin buffer name for a PAGENAME."
  (concat moinrpc-buffer-prefix pagename moinrpc-buffer-postfix))

(defun moinrpc-strip-text-properties (txt)
  (set-text-properties 0 (length txt) nil txt)
  txt)

(defun moinrpc-create-page-buffer (wiki pagename)
  (with-current-buffer
      (get-buffer-create (moinrpc-buffer-name pagename))
    (erase-buffer)
    (moinrpc-page-mode)
    (switch-to-buffer (current-buffer))
    (make-variable-buffer-local 'current-wiki)
    (make-variable-buffer-local 'current-pagename)
    (setq current-wiki wiki)
    (setq current-pagename pagename)
    (current-buffer)))

(defun moinrpc-fill-page-buffer-content (buffer)
  (with-current-buffer
      buffer
    (insert (moinrpc-get-page-content current-wiki current-pagename))
    (set-buffer-modified-p nil)))

(defun moinrpc-create-list-buffer (wiki)
    (with-current-buffer
	(get-buffer-create (moinrpc-buffer-name "List Pages"))
      (read-only-mode -1)
      (erase-buffer)
      (make-variable-buffer-local 'current-wiki)
      (setq current-wiki wiki)
      (insert
       (mapconcat 'concat (moinrpc-get-list-content current-wiki) "\n"))
      (read-only-mode)
      (current-buffer)))

(defun moinrpc-create-main-buffer ()
  (with-current-buffer
      (get-buffer-create "*moinrpc*")
    (switch-to-buffer (current-buffer))
    (erase-buffer)
    (insert (format "%S" moinrpc-wiki-settings))
    (read-only-mode)
  ))

(defun moinrpc-save-current-buffer ()
  (interactive)
  (moinrpc-save-page-content current-wiki
			  current-pagename
			  (moinrpc-strip-text-properties (buffer-string)))
  (set-buffer-modified-p nil)
  (current-buffer))

(defun moinrpc-get-or-create-page-buffer (pagename)
  (let
      ((buffer-name (moinrpc-buffer-name pagename))
       (buffer nil))
    (if
	(null (get-buffer buffer-name))
	(progn
	  (setq buffer (moinrpc-create-page-buffer current-wiki pagename))
	  (moinrpc-fill-page-buffer-content buffer))
      (setq buffer (get-buffer (moinrpc-buffer-name pagename))))
    (switch-to-buffer buffer)))

(defun moinrpc-find-page ()
  (interactive)
  (let
      ((pagename (read-string "Find page: ")))
    (moinrpc-get-or-create-page-buffer pagename)))

(defun helm-moinrpc-get-or-create-page-buffer (candidate)
  (stringp candidate)
  (moinrpc-get-or-create-page-buffer candidate)
  )

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
	  :buffer "*helm-moin-find-pages*"
	  )))

(defun moinrpc ()
  (with-current-buffer
      (get-buffer-create "*moinrpc*")
    (read-only-mode -1)
    (erase-buffer)
    (insert "MoinRPC Wiki List")
    (read-only-mode)
    (moinrpc-page-mode)
    (make-variable-buffer-local 'current-wiki)
    (setq current-wiki (cdr (assoc moinrpc-current-wiki moinrpc-wiki-settings)))
    (switch-to-buffer "*moinrpc*")
  ))

(define-derived-mode moinrpc-page-mode fundamental-mode
  (moinmoin-mode)
  (setq mode-name "moinrpc-page-mode")
  (local-set-key (kbd "C-x C-s") 'moinrpc-save-current-buffer)
;  (local-set-key (kbd "C-x C-f") 'moinrpc-find-buffer)
  (local-set-key (kbd "C-x C-f") 'helm-moinrpc-find-page)
  )

(define-derived-mode moinrpc-mode fundamental-mode
  (setq mode-name "moinrpc-mode")
  (moinrpc-create-main-buffer)
  (local-set-key (kbd "c") 'moinrpc-create-wiki-settings)
  )

(provide 'moinrpc-mode)
;;; moin.el ends here
 
