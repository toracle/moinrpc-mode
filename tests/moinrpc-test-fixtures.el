(defun my-fixture (body)
  (unwind-protect
      (let ((xml-rpc-method-call #'moinrpc-mock-xml-rpc-method-call)

            (*moinrpc-fixture-xml-rpc-record* nil)

            (*moinrpc-fixture-response-get-pages*
             '(("SUCCESS")
               (("Page1" "Page2" "Page3"))))

             (*moinrpc-fixture-response-error-not-found*
               '(("SUCCESS")
                 (("faultCode" . 1)
                  ("faultString" . "No such page was found."))))

             (*moinrpc-fixture-response-error-invalid-token*
               '(("SUCCESS")
                 (("faultCode" . 1)
                  ("faultString" . "Invalid token."))))

             (*moinrpc-fixture-response-error-no-attribute*
              '(("SUCCESS")
                (("faultCode" . 1)
                 ("faultString" . "<type ’exceptions.AttributeError’>:’str’ object has no attribute ’data’"))))

             (*moinrpc-fixture-wiki*
               '(("testwiki"
                  (wiki-alias . "wooridle")
                  (xmlrpc-endpoint . "https://wiki.net/?action=xmlrpc2")
                  (xmlrpc-api-token . "testtoken")
                  (username . "myuser"))))

             (*moinrpc-fixture-wiki-setting*
               '((wiki-alias . "wooridle")
                 (xmlrpc-endpoint . "https://wiki.net/?action=xmlrpc2")
                 (xmlrpc-api-token . "testtoken")
                 (username . "myuser")))
             (funcall body)))))

(provide 'moinrpc-test-fixtures)
