(defun my/todo ()
  (interactive)
  (let* ((val (read-string "Priority (default 1000): "))
         (prio-to-insert (if (string= val "") "1000" val))
         (cs (or comment-start "// "))
         (ce (or comment-end "")))
    (insert "\n")
    (insert (format "%s=====================%s\n" cs ce))
    (insert (format "%sTODO :" cs))
    (let ((todo-pos (point)))
      (insert (if (string= ce "") "" (format " %s" ce)))
      (insert "\n")
      (insert (format "%sPrio : %s%s" cs prio-to-insert ce))
      (insert "\n")
      (insert (format "%s=====================%s\n" cs ce))
      (goto-char todo-pos))))

(defun my/fetch-todos (search-dir)
  "Search for todos in SEARCH-DIR using rg and return a list of (prio msg file line)."
  (let* ((default-directory search-dir)
         (output (shell-command-to-string "rg -n --no-heading -B 1 \"Prio : [0-9]+\" ."))
         (todos '())
         (lines (split-string output "\n")))
    (dotimes (i (length lines))
      (let ((current-line (nth i lines)))
        (when (string-match ":\\([0-9]+\\):.*Prio : *\\([0-9]+\\)" current-line)
          (let ((prio (string-to-number (match-string 2 current-line)))
                (prev-line (if (> i 0) (nth (1- i) lines) nil)))
            (when (and prev-line (string-match "^\\(.*\\)-\\([0-9]+\\)-.*TODO : *\\(.*\\)" prev-line))
              (let ((file (match-string 1 prev-line))
                    (line (string-to-number (match-string 2 prev-line)))
                    (msg (match-string 3 prev-line)))
                (setq msg (replace-regexp-in-string " *\\*/ *$" "" msg))
                (push (list prio msg (expand-file-name file search-dir) line) todos)))))))
    (sort todos (lambda (a b) (< (car a) (car b))))))

(defun my/grep-todos-jump ()
  "Jump to the todo on the current line."
  (interactive)
  (let ((data (get-text-property (point) 'todo-data)))
    (if data
        (let ((file (car data))
              (line (cdr data)))
          (find-file file)
          (goto-line line))
      (message "No todo found on this line."))))

(defun my/grep-todos ()
  (interactive)
  (let* ((dir (read-directory-name "Search directory: "))
         (search-dir (expand-file-name dir))
         (todos (my/fetch-todos search-dir)))
    (let ((buf (get-buffer-create "*Todo List*")))
      (switch-to-buffer buf)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "%-8s | %-40s | %s\n" "Priority" "Task" "File"))
        (insert (make-string 80 ?-))
        (insert "\n")
        (dolist (item todos)
          (let ((prio (nth 0 item))
                (msg (nth 1 item))
                (file (nth 2 item))
                (line (nth 3 item)))
            (insert (propertize (format "%-8d | %-40s | %s\n" 
                                        prio 
                                        msg 
                                        (file-relative-name file search-dir))
                                'todo-data (cons file line)
                                'help-echo (format "File: %s\nLine: %d" file line))))))
      (use-local-map (let ((map (make-sparse-keymap)))
                       (define-key map (kbd "RET") 'my/grep-todos-jump)
                       map))
      (setq buffer-read-only t))))

(defun my/delete-todo-block ()
  "Delete the todo block at point. 
   Identifies Prio line, optional TODO line above, and optional separators."
  (beginning-of-line)
  (if (not (save-excursion (re-search-forward "Prio :" (line-end-position) t)))
      (message "Error: 'Prio :' marker not found on this line.")
    (let ((start (line-beginning-position))
          (end (line-beginning-position 2))) ;; Default: delete just the Prio line + newline

      ;; 1. Check for TODO line above
      (save-excursion
        (when (zerop (forward-line -1))
          (when (re-search-forward "TODO :" (line-end-position) t)
            (setq start (line-beginning-position))
            ;; 2. Check for Top Separator above TODO
            (when (zerop (forward-line -1))
              (when (re-search-forward "=====" (line-end-position) t)
                (setq start (line-beginning-position)))))))

      ;; 3. Check for Bottom Separator below Prio
      (save-excursion
        (when (zerop (forward-line 1))
          (when (re-search-forward "=====" (line-end-position) t)
            (setq end (line-beginning-position 2)))))

      (delete-region start end)
      (message "Todo block deleted."))))

(defvar-local my/todo-search-dir nil
  "Directory used for the current todo list.")

(defun my/complete-todo-action ()
  "Delete the todo at point and refresh the list."
  (interactive)
  (let ((data (get-text-property (point) 'todo-data))
        (dir my/todo-search-dir))
    (if data
        (let ((file (car data))
              (line (cdr data)))
          (find-file file)
          (goto-line line)
          ;; The 'line' points to the TODO line. We need to find the Prio line below it.
          (if (re-search-forward "Prio :" nil t)
              (progn
                (my/delete-todo-block)
                (save-buffer)
                (message "Completed task in %s" file)
                (my/complete-todo dir))
            (message "Error: Could not find 'Prio :' block for this task.")))
      (message "No todo found on this line."))))

(defun my/complete-todo (&optional directory)
  "Show todo list for completion. Press RET to delete/complete the item."
  (interactive)
  (let* ((dir (or directory (read-directory-name "Search directory: ")))
         (search-dir (expand-file-name dir))
         (todos (my/fetch-todos search-dir)))
    (let ((buf (get-buffer-create "*Complete Todo*")))
      (switch-to-buffer buf)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (setq-local my/todo-search-dir search-dir)
        (insert (propertize "PRESS RET TO COMPLETE (DELETE) A TASK\n\n" 'face '(:weight bold :foreground "red")))
        (insert (format "%-8s | %-40s | %s\n" "Priority" "Task" "File"))
        (insert (make-string 80 ?-))
        (insert "\n")
        (dolist (item todos)
          (let ((prio (nth 0 item))
                (msg (nth 1 item))
                (file (nth 2 item))
                (line (nth 3 item)))
            (insert (propertize (format "%-8d | %-40s | %s\n" 
                                        prio 
                                        msg 
                                        (file-relative-name file search-dir))
                                'todo-data (cons file line)
                                'help-echo (format "File: %s\nLine: %d" file line))))))
      (use-local-map (let ((map (make-sparse-keymap)))
                       (define-key map (kbd "RET") 'my/complete-todo-action)
                       map))
      (setq buffer-read-only t))))
