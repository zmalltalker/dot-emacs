(defun gt-extract-text-from-node (node)
  "Extract the text value of a node, the last element of the list"
  (car (reverse node)))

(defun gt-simple-node-value (node attribute-name)
  "Extract the text value of the first element of an XML node
Given a node with this structure:
<person>
<name>John</name>
</person>
when called with gt-simple-node-value node 'name would return John
"
  (gt-extract-text-from-node (car (xml-get-children node attribute-name)))
)

(defun gt-merge-request-url (merge-request project-name repository-name)
  "Construct a URI from a merge request. Some assumptions:
- Host is https://gitorious.org/
- The canonical URL is used (/project/repo)
- Project name can be passed along"
  (setq id (gt-simple-node-value merge-request 'id))
  (concat "https://gitorious.org/" project-name "/" repository-name "/merge_requests/" id)
)

(defun gt-build-merge-request (merge-request)
  "Build a list of attributes for a merge-request node in the form
(id summary status uri)
"
  (setq summary (gt-simple-node-value merge-request 'summary))
  (setq id (gt-extract-text-from-node (car (xml-get-children merge-request 'id))))
  (setq status (gt-simple-node-value merge-request 'status))
  (setq uri (gt-merge-request-url merge-request "gitorious" "mainline"))
  (list id summary status uri)
)
(defun gt-parse-merge-requests (filename)
  "Extract a list of merge requests from filename"
  (setq doc (car (xml-parse-file filename)))
  (setq merge-requests (xml-get-children doc 'merge-request))
  (loop for merge-request in merge-requests
	collect (gt-build-merge-request merge-request)
	))

(defun gt-download-file (url filename)
  (let ((url (or url
                 (read-string "Enter download URL: "))))
    (let ((download-buffer (url-retrieve-synchronously url)))
      (save-excursion
        (set-buffer download-buffer)
        ;; we may have to trim the http response
        (goto-char (point-min))
        (re-search-forward "^$" nil 'move)
        (forward-char)
        (delete-region (point-min) (point))
        (write-file filename)))))

(defun gt-fetch-merge-requests (project repository status)
  "Load all merge requests for repository in project with a given status"
  (setq mr-url (format "http://gitorious.org/%s/%s/merge_requests.xml?status=%s" project repository status))
  (message (concat "Looking up " mr-url))
  (setq xml-file "/tmp/mr.xml")
  (gt-download-file mr-url xml-file)
  (gt-parse-merge-requests xml-file)
)

(defun gt-display-merge-requests ()
  (interactive)
  (setq project (read-string "Enter project name: "))
  (setq repository (read-string "Enter repository name: "))
  (setq status (read-string "Enter status: "))
  (setq merge-requests (gt-fetch-merge-requests project repository status))
  (setq mr-buffer (get-buffer-create "*merge-requests"))
  (switch-to-buffer mr-buffer)
  (goto-char (point-min))
  (insert (format "MERGE REQUESTS IN %s/%s WITH STATUS %s\n" project repository status))
  (insert "*****************************************************\n")
  (while merge-requests 
    (setq merge-request (car merge-requests))
    (insert (concat "- " (car merge-request)))
    (setq merge-request (cdr merge-request))
    (insert (concat ": " (car merge-request) "\n"))
    (setq merge-request (cdr merge-request))
    (setq merge-requests (cdr merge-requests))
    )
)

(provide 'merge-requests)
