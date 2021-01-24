(require 'moinrpc-mode)
(require 'moinrpc-common)
(require 'moinrpc-conf)
(require 'moinrpc-test-fixtures)


(defvar *moinrpc-fixture-xml-rpc-record* nil)

(defvar *moinrpc-fixture-response-get-pages* nil)

(defvar *moinrpc-fixture-response-error-not-found* nil)

(defvar *moinrpc-fixture-response-error-invalid-token* nil)

(defvar *moinrpc-fixture-wiki* nil)

(defvar *moinrpc-fixture-wiki-setting* nil)


(defun moinrpc-fixture-record-xml-rpc-response (response)
  (setq *moinrpc-fixture-xml-rpc-record*
        (-insert-at (length *moinrpc-fixture-xml-rpc-record*)
                    response
                    *moinrpc-fixture-xml-rpc-record*)))


(defun moinrpc-mock-xml-rpc-method-call (url method &rest params)
  (pop *moinrpc-fixture-xml-rpc-record*))


(ert-deftest moinrpc-get-keys ()
  (my-fixture
   (lambda ()
     (should (equal (moinrpc-get-keys '((key1 . value1)
                                        (key2 . value2)
                                        (key3 . value3)))
                    '(key1 key2 key3))))))


(ert-deftest moinrpc-xmlrpc-response-valid-p ()
  (my-fixture
   (lambda ()
     (should (equal (moinrpc-xmlrpc-response-valid-p
                     *moinrpc-fixture-response-get-pages*) t))
     (should (equal (moinrpc-xmlrpc-response-valid-p
                     *moinrpc-fixture-response-error-not-found*) t)))))


(ert-deftest moinrpc-xmlrpc-error-cause-to-type ()
  (my-fixture
   (lambda ()
     (should (equal (moinrpc-xmlrpc-error-cause-to-type "No such page was found.")
                    :NOT-FOUND))
     (should (equal (moinrpc-xmlrpc-error-cause-to-type "Invalid token.")
                    :INVALID-TOKEN))
     (should (equal (moinrpc-xmlrpc-error-cause-to-type "Unknown error.")
                    nil)))))


(ert-deftest moinrpc-xmlrpc-response-error-type ()
  (my-fixture
   (lambda ()
     (should (equal (moinrpc-xmlrpc-response-error-type
                     *moinrpc-fixture-response-error-not-found*)
                    :NOT-FOUND))
     (should (equal (moinrpc-xmlrpc-response-error-type
                     *moinrpc-fixture-response-error-invalid-token*)
                    :INVALID-TOKEN))
     (should (equal (moinrpc-xmlrpc-response-error-type
                     *moinrpc-fixture-response-get-pages*)
                    nil)))))


(defun moinrpc-on-error-mockup (response wiki)
  nil)


(ert-deftest moinrpc-rel-wikilink-to-abs ()
  (should (equal (moinrpc-rel-wikilink-to-abs "WikiLink" nil)
                 "WikiLink"))
  (should (equal (moinrpc-rel-wikilink-to-abs "..WikiLink" "Parent")
                 "Parent..WikiLink"))
  (should (equal (moinrpc-rel-wikilink-to-abs "/WikiLink" "Parent")
                 "Parent/WikiLink")))


(provide 'moinrpc-mode-test)
