(require 'moinrpc-test-fixtures)

(ert-deftest moinrpc-make-wiki-conf ()
  (my-fixture
   (lambda ()
     (should (equal (moinrpc-make-wiki-conf
                     "testwiki")
		    '((wiki-alias . "testwiki")
		      (xmlrpc-endpoint . nil)
		      (username . nil)
		      (xmlrpc-api-token . nil))))
     (should (equal (moinrpc-make-wiki-conf
                     "testwiki"
                     "https://mywiki.com/xmlrpc2")
		    '((wiki-alias . "testwiki")
		      (xmlrpc-endpoint . "https://mywiki.com/xmlrpc2")
		      (username . nil)
		      (xmlrpc-api-token . nil))))
     (should (equal (moinrpc-make-wiki-conf
                     "testwiki"
                     "https://mywiki.com/xmlrpc2" "testuser")
		    '((wiki-alias . "testwiki")
		      (xmlrpc-endpoint . "https://mywiki.com/xmlrpc2")
		      (username . "testuser")
		      (xmlrpc-api-token . nil)))))))


(ert-deftest moinrpc-get-wiki-conf ()
  (my-fixture
   (lambda ()
     (should (equal (moinrpc-get-wiki-conf
                     *moinrpc-fixture-wiki-setting*
                     'xmlrpc-api-token)
		    "testtoken")))))


(ert-deftest moinrpc-set-wiki-conf ()
  (my-fixture
   (lambda ()
     (should
      (let ((wiki-setting (copy-list *moinrpc-fixture-wiki-setting*)))
        (moinrpc-set-wiki-conf wiki-setting 'xmlrpc-api-token "testtoken2")
        (equal wiki-setting
	       '((wiki-alias . "wooridle")
	         (xmlrpc-endpoint . "https://wiki.net/?action=xmlrpc2")
	         (xmlrpc-api-token . "testtoken2")
	         (username . "myuser"))))))))

(provide 'moinrpc-conf-test)
