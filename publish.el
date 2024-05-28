(require 'ox-publish)

(let ((org-publish-project-alist '(("org"
				    :base-directory "."
				    :publishing-directory "."
				    :publishing-function org-html-publish-to-html
				    :exclude "README.org"))))
  (org-publish-all))
