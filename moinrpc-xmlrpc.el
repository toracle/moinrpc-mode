;;; package --- moinmoin xml-rpc client
;;; Commentary: 

;;; Code: 


;; Raw wiki-xmlrpc functions

(defun moinrpc-check-xmlrpc-response (response wiki)
  (let
      ((response-status (car response)))
    (if
	(string= (car response-status) "SUCCESS")
	(if
	    (assoc "faultCode" response-status)
	    (when
		(= 1 (cdr (cdr (assoc "faultCode" response-status))))
	      (when
		  (string= "Invalid token." (cdr (assoc "faultString" response-status)))
		(moinrpc-get-auth-token wiki)
		(moinrpc-save-wiki-settings))
	      nil)
	  nil)
      t)))
	
(defun moinrpc-encode-xml-rpc-multi-each-method (method-name &rest params)
  (list
   (cons "methodName" method-name)
   (cons "params" (vconcat params))))

(defun moinrpc-encode-xml-rpc-multi-method (wiki method-name &rest params)
  "Construct and encode multi method call type to WIKI with a METHOD-NAME and PARAMS."
  (list
    (moinrpc-encode-xml-rpc-multi-each-method "applyAuthToken" (cdr (assoc 'xmlrpc-api-token wiki)))
    (apply 'moinrpc-encode-xml-rpc-multi-each-method method-name params)))

(defun moinrpc-xml-rpc-multi-method-call (wiki method-name &rest params)
  "XML-RPC method call to WIKI with a METHOD-NAME and PARAMS."
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

    (moinrpc-check-xmlrpc-response response wiki)
    (caar (cdr response))))


(defun moinrpc-set-auth-token-to-current (token wiki-setting)
  "Set access TOKEN to WIKI-SETTING."
    (setq wiki-setting (assq-delete-all 'xmlrpc-api-token wiki-setting))
    (add-to-list 'wiki-setting (cons 'xmlrpc-api-token token))
    )

(defun moinrpc-get-auth-token (wiki)
  "Prompt password and get access token of given WIKI using it."
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
Specify WIKI with a PAGENAME."
  (moinrpc-xml-rpc-multi-method-call wiki
				  "getPage"
				  pagename))

(defun moinrpc-save-page-content (wiki pagename content)
  "Save WIKI content with a PAGENAME and CONTENT."
  (moinrpc-xml-rpc-multi-method-call wiki
				  "putPage"
				  pagename
				  content))

(defun moinrpc-get-list-content (wiki)
  "Return a list of all page names from WIKI."
  (let
      ((content nil))
    (setq content (moinrpc-xml-rpc-multi-method-call wiki
						     "getAllPages"))
    (sort content 'string<)))

(provide 'moinrpc-xmlrpc)
;;; moinrpc-xmlrpc.el ends here
