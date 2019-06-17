;; Variables

(defvar *moinrpc-wiki-settings* nil)
(defvar *moinrpc-current-wiki* nil)
(defvar *moinrpc-settings-file* (concat user-emacs-directory "remote-moin"))

(when (file-readable-p *moinrpc-settings-file*)
  (load *moinrpc-settings-file*))


(defun moinrpc-save-wiki-settings ()
  "Save wiki settings to file."
  (with-current-buffer (find-file-noselect *moinrpc-settings-file*)
    (erase-buffer)
    (insert (format "(setq *moinrpc-wiki-settings* '%S)" *moinrpc-wiki-settings*))
    (newline)
    (insert (format "(setq *moinrpc-current-wiki* %S)" *moinrpc-current-wiki*))
    (save-buffer)
    t))


(defun moinrpc-new-wiki-setting ()
  "Add a new wiki settings."
  (interactive)
  (moinrpc-add-wiki-setting-to-global (moinrpc-create-wiki-setting-i))
  (moinrpc-save-wiki-settings)
  (moinrpc-main-page))


(defun moinrpc-make-wiki-conf (wiki-alias &optional xmlrpc-endpoint username xmlrpc-api-token)
  "Return new wiki settings data structure."
  (list (cons 'wiki-alias wiki-alias)
	(cons 'xmlrpc-endpoint xmlrpc-endpoint)
	(cons 'username username)
	(cons 'xmlrpc-api-token xmlrpc-api-token)))


(defun moinrpc-set-wiki-conf (wiki key new-value)
  (setf (cdr (assoc key wiki)) new-value)
  wiki)


(defun moinrpc-get-wiki-conf (wiki key)
  (cdr (assoc key wiki)))


(defun moinrpc-create-wiki-setting (wiki-alias xmlrpc-endpoint username)
  "WIKI-ALIAS XMLRPC-ENDPOINT USERNAME."
  (let ((xmlrpc-api-token nil)
        (wiki-setting nil))
    (setq wiki-setting (moinrpc-make-wiki-conf wiki-alias
                                               xmlrpc-endpoint
                                               username))
    (setq xmlrpc-api-token
          (moinrpc-get-auth-token wiki-setting))
    (moinrpc-make-wiki-conf wiki-alias
                            xmlrpc-endpoint
                            username
                            xmlrpc-api-token)))


(defun moinrpc-add-wiki-setting-to-global (wiki-setting)
  "WIKI-SETTING."
  (let ((wiki-alias (cdr (assoc 'wiki-alias wiki-setting)))
        (wiki-settings nil))
    (message (format "%S" *moinrpc-wiki-settings*))
    (message (format "%S" wiki-alias))
    (when (not (eq *moinrpc-wiki-settings* nil))
      (progn (setq wiki-settings (assq-delete-all wiki-alias *moinrpc-wiki-settings*))
             (message (format "%S" wiki-settings))))
    (add-to-list 'wiki-settings (cons wiki-alias wiki-setting))
    (setq *moinrpc-wiki-settings* wiki-settings)
    (setq *moinrpc-current-wiki* wiki-alias)
    (moinrpc-save-wiki-settings)))


(defun moinrpc-get-keys (alist)
  "Returns keys of ALIST."
  (mapcar 'car alist))

(provide 'moinrpc-conf)
