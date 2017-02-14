(defun moinrpc-make-wiki-conf (wiki-alias &optional xmlrpc-endpoint username xmlrpc-api-token)
  (list (cons 'wiki-alias wiki-alias)
	(cons 'xmlrpc-endpoint xmlrpc-endpoint)
	(cons 'username username)
	(cons 'xmlrpc-api-token xmlrpc-api-token)))


(defun moinrpc-set-wiki-conf (wiki key new-value)
  (setf (cdr (assoc key wiki)) new-value)
  wiki)

(defun moinrpc-get-wiki-conf (wiki key)
  (cdr (assoc key wiki)))

(provide 'moinrpc-conf)

