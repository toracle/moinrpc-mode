(require 'moinrpc-xmlrpc)

(defvar *moinrpc-xmlrpc-test-call-history* nil)

(defvar *fixture-wiki*
  '((wiki-alias . "Wooridle")
    (xmlrpc-endpoint . "https://wiki.net/?action=xmlrpc2")
    (username . "testuser")
    (xmlrpc-api-token . "api-token")))


(defun dummy-xml-rpc-method-call (server-url method &rest params)
  (add-to-list '*moinrpc-xmlrpc-test-call-history*
               (list :server-url server-url
                     :method method
                     :params params)))


(defmacro with-dummy-xml-rpc-call (&rest body)
  `(progn
     (advice-add 'xml-rpc-method-call :override 'dummy-xml-rpc-method-call)
     (setf *moinrpc-xmlrpc-test-call-history* nil)
     ,@body
     (advice-remove 'xml-rpc-method-call 'dummy-xml-rpc-method-call)))


(ert-deftest moinrpc-get-page-content-fire-request ()
  (with-dummy-xml-rpc-call
   (moinrpc-get-page-content *fixture-wiki* "TestPage")
   (should (equal *moinrpc-xmlrpc-test-call-history*
                  '((:server-url
                     "https://wiki.net/?action=xmlrpc2"
                     :method
                     system.multicall
                     :params (((("methodName" . "applyAuthToken")
                                ("params" . ["api-token"]))
                               (("methodName" . "getPage")
                                ("params" . ["TestPage"]))))))))))


(ert-deftest moinrpc-save-page-content-fire-request ()
  (with-dummy-xml-rpc-call
   (moinrpc-save-page-content *fixture-wiki* "TestPage" "Test content")
   (should (equal *moinrpc-xmlrpc-test-call-history*
                  '((:server-url
                     "https://wiki.net/?action=xmlrpc2"
                     :method
                     system.multicall
                     :params (((("methodName" . "applyAuthToken")
                                ("params" . ["api-token"]))
                               (("methodName" . "putPage")
                                ("params" . ["TestPage" "Test content"]))))))))))


(ert-deftest moinrpc-get-list-content-fire-request ()
  (with-dummy-xml-rpc-call
   (moinrpc-get-list-content *fixture-wiki*)
   (should (equal *moinrpc-xmlrpc-test-call-history*
                  '((:server-url
                     "https://wiki.net/?action=xmlrpc2"
                     :method
                     system.multicall
                     :params (((("methodName" . "applyAuthToken")
                                ("params" . ["api-token"]))
                               (("methodName" . "getAllPages")
                                ("params" . []))))))))))
