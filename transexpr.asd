(defsystem #:transexpr
  :description "Tools to write transpilers into any blub language."
  :author "Minori Yamashita <ympbyc@gmail.com>"
  :license "MIT"
  #+asdf-unicode :encoding #+asdf-unicode :utf-8
  :depends-on ()
  :serial t
  :components ((:file "package")
	       (:file "transexpr")))
