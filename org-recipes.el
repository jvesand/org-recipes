;;; org-recipes.el --- A code snippet manager with Org and Consult/Vertico
;;
;; Filename: org-recipes.el
;; Description: A code snippet manager with Org and Consult/Vertico
;; Author: Tu, Do Hoang <tuhdo1710@gmail.com>
;; URL      : https://github.com/tuhdo/semantic-refactor
;; Maintainer: Johan Sandberg
;; Created: March 11, 2017
;; Version: 0.3
;; Package-Requires: ((emacs "24.4") consult embark org)
;; Keywords: tools
;; Compatibility: GNU Emacs: 24.4+
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;; This package collects code snippets under the inner-most Org heading. It
;; provides the following features:
;;
;; - List all Org headings with at least a code snippet. Each entry can be
;; visited for viewing.
;;
;; - Insert a code snippet into the current buffer. The description text is
;; stripped and all code snippets are concantenated into a single snippet and
;; then it is nisert into current buffer.
;;
;; Updated to use Consult and Embark instead of Helm.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:
(require 'subr-x)
(require 'thingatpt)
(require 'consult)
(require 'embark)
(require 'cl-lib)

(defcustom org-recipes-directory "~/org-recipes"
  "Source directory that org-recipes will search for code snippets in every Org files."
  :type 'list
  :group 'org-recipes)

(defvar org-recipes-history nil
  "History for org-recipes completion.")

(defmacro org-recipes--get-heading-face (headline)
  `(intern-soft (concat "org-level-" (number-to-string (org-element-property :level ,headline)))))

(defmacro org-recipes--get-file (c)
  `(nth 0 ,c))

(defmacro org-recipes--get-line (c)
  `(nth 1 ,c))

(defmacro org-recipes--get-src-blocks (c)
  `(nth 2 ,c))

(defmacro org-recipes--get-real (candidate)
  `(cdr ,candidate))

(defmacro org-recipes--src-string (src-block)
  `(org-element-property :value ,src-block))

(defun org-recipes ()
  "Select and act on org recipe snippets using consult."
  (interactive)
  (let* ((candidates (org-recipes--get-candidates))
         (annotated-candidates (mapcar (lambda (c)
                                        (let ((display (car c))
                                              (data (cdr c)))
                                          (put-text-property 0 1 'org-recipes-data data display)
                                          (cons display data)))
                                      candidates))
         (selected (consult--read annotated-candidates
                                 :prompt "Recipe: "
                                 :category 'org-recipe
                                 :history 'org-recipes-history
                                 :lookup #'consult--lookup-cdr)))
    (when selected
      (org-recipes--persistent-view selected))))

(defvar org-recipes-embark-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "i") #'org-recipes--insert)
    (define-key map (kbd "c") #'org-recipes--copy)
    (define-key map (kbd "v") #'org-recipes--persistent-view)
    map))

(add-to-list 'embark-keymap-alist '(org-recipe . org-recipes-embark-map))

(defun org-recipes--persistent-view (c)
  (let ((data (if (listp c) c (get-text-property 0 'org-recipes-data c))))
    (find-file (org-recipes--get-file data))
    (goto-line (org-recipes--get-line data))
    (org-show-subtree)))

(defun org-recipes-insert-current ()
  "Insert recipe at point using consult selection."
  (interactive)
  (let* ((candidates (org-recipes--get-candidates))
         (annotated-candidates (mapcar (lambda (c)
                                        (cons (car c) (cdr c)))
                                      candidates))
         (selected (consult--read annotated-candidates
                                 :prompt "Insert recipe: "
                                 :category 'org-recipe
                                 :history 'org-recipes-history
                                 :lookup #'consult--lookup-cdr)))
    (when selected
      (org-recipes--insert selected))))

(defun org-recipes-copy-current ()
  "Copy recipe using consult selection."
  (interactive)
  (let* ((candidates (org-recipes--get-candidates))
         (annotated-candidates (mapcar (lambda (c)
                                        (cons (car c) (cdr c)))
                                      candidates))
         (selected (consult--read annotated-candidates
                                 :prompt "Copy recipe: "
                                 :category 'org-recipe
                                 :history 'org-recipes-history
                                 :lookup #'consult--lookup-cdr)))
    (when selected
      (org-recipes--copy selected))))

(defun org-recipes--insert (c)
  (let ((data (if (listp c) c (get-text-property 0 'org-recipes-data c))))
    (maphash (lambda (file src-str)
               (let ((file-empty (string-equal file "empty")))
                 (with-current-buffer (if file-empty
                                          (current-buffer)
                                        (find-file-noselect file))
                   (save-excursion
                     (beginning-of-line)
                     (let ((start (point)))
                       (insert src-str)
                       (indent-region start (point))
                       (unless file-empty (save-buffer)))))))
             (org-recipes--distribute-src-blocks-strings (org-recipes--get-src-blocks data)))))

(defun org-recipes--copy (c)
  (let* ((data (if (listp c) c (get-text-property 0 'org-recipes-data c)))
         (src-blocks (org-recipes--get-src-blocks data))
         (dist-table (org-recipes--distribute-src-blocks-strings src-blocks)))
    (maphash (lambda (file src-str)
               (let ((file-empty (string-equal file "empty")))
                 (ignore-errors
                   (with-current-buffer (if file-empty
                                            (current-buffer)
                                          (find-file-noselect file))
                     (kill-new src-str)
                     (unless file-empty (save-buffer))))))
             dist-table)))

(defmacro org-recipes--src-data-get-file (s)
  `(car ,s))

(defmacro org-recipes--src-data-get-ignore (s)
  `(cadr ,s))

(defmacro org-recipes--src-data-pre-recipe-list (s)
  `(caddr ,s))

(defmacro org-recipes--src-data-post-recipe-list (s)
  `(cadddr ,s))

(defmacro org-recipes--src-data-get-str (s)
  `(cadddr (cdr ,s)))

(defun org-recipes--distribute-src-blocks-strings (src-blocks)
  (let* ((dist-table (make-hash-table :test #'equal)))
    (mapcar (lambda (s)
              (let* ((src-data (org-recipes--process-src-block s))
                     (pre-recipe-list (org-recipes--src-data-pre-recipe-list src-data))
                     (post-recipe-list (org-recipes--src-data-post-recipe-list src-data))
                     (file (org-recipes--src-data-get-file src-data))
                     (ignore (org-recipes--src-data-get-ignore src-data))
                     (new-str (org-recipes--src-data-get-str src-data))
                     (old-str (gethash file dist-table))
                     (pre-str (with-temp-buffer
                                (mapcar (lambda (r)
                                          (when-let ((candidates (org-recipes--get-candidates r)))
                                            (org-recipes--insert-candidates candidates)
                                            (newline)))
                                        pre-recipe-list)
                                (buffer-string)))
                     (post-str (with-temp-buffer
                                 (mapcar (lambda (r)
                                           (when-let ((candidates (org-recipes--get-candidates r)))
                                             (newline)
                                             (org-recipes--insert-candidates candidates)))
                                         post-recipe-list)
                                 (buffer-string))))
                (unless ignore
                  (puthash file
                           (if old-str
                               (concat pre-str old-str new-str post-str)
                             (concat  pre-str new-str post-str))
                           dist-table)
                  )))
            src-blocks)
    dist-table))

(defun org-recipes--process-src-block (s)
  "docstring"
  (let* ((src-parameters (org-recipes--filter-src-parameters
                          (org-babel-parse-header-arguments (org-element-property :parameters s))
                          '(:file :pre-recipe :post-recipe :ignore)))
         (file (cdr (assoc :file src-parameters)))
         (ignore (cdr (assoc :ignore src-parameters)))
         (pre-recipe-list (cdr (assoc :pre-recipe src-parameters)))
         (post-recipe-list (cdr (assoc :post-recipe src-parameters)))
         (src-string (org-recipes--src-string s)))
    (list (or file "empty") ignore pre-recipe-list post-recipe-list src-string)))

(defun org-recipes--filter-src-parameters (src-params key-list)
  (cl-remove-if (lambda (s)
                  (not (member (car s) key-list))) src-params))

(defun org-recipes--get-candidates (&optional recipe)
  (-flatten-n
   1
   (delq
    nil
    (mapcar (lambda (f)
              (org-recipes--collect-snippets f recipe))
            (append (directory-files-recursively org-recipes-directory "")
                    (when (featurep 'org-wiki)
                      (mapcar (lambda (f)
                                (concat org-wiki-location "/" f))
                              (org-wiki--page-files))))))))

(defun org-recipes--collect-snippets (f &optional recipe)
  (let* ((org-buf (find-file-noselect f))
         (target-major-modes (org-recipes--get-target-major-modes org-buf))
         (cur-major-mode major-mode)
         (headline-major-modes))
    (when (or (null target-major-modes)
              (member cur-major-mode target-major-modes))
      (with-current-buffer org-buf
        (org-element-map (org-element-parse-buffer 'element) 'headline
          (lambda (headline)
            (setq headline-major-modes (mapcar #'org-recipes--string-to-mode
                                               (org-recipes--split-mode-list (org-element-property :LANG headline))))
            (unless headline-major-modes (setq headline-major-modes (list cur-major-mode)))
            (when (member cur-major-mode headline-major-modes)
              (let* ((src-blocks (delq nil (org-element-map headline 'src-block
                                             (lambda (s)
                                               (when (or headline-major-modes
                                                         (eq cur-major-mode
                                                             (org-recipes--string-to-mode (org-element-property :language s))))
                                                 s)))))
                     (symbol (org-element-property :SYMBOL headline))
                     (src-blocks-parent (org-element-map headline 'headline 'identity))
                     (linum (line-number-at-pos
                             (org-element-property :begin headline))))
                (when (and src-blocks
                           (eq (length src-blocks-parent) 1)
                           (or (null recipe)
                               (equal symbol (symbol-name recipe))))
                  (cons (concat
                                (concat (number-to-string linum) ":")
                                " "
                                (when symbol (propertize (concat  "[" symbol "]  ") 'face 'font-lock-type-face))
                                (org-recipes--get-parent-string headline)
                                (propertize (org-element-interpret-data (org-element-property :title headline)) 'face (org-recipes--get-heading-face headline)))
                        (list f
                              linum
                              src-blocks)))))
            ))
        ))))

(defun org-recipes--get-parent-string (headline)
  (when-let ((parent (org-element-property :parent headline))
             (parent-str (org-element-property :raw-value parent)))
    (concat (org-recipes--get-parent-string parent)
            (propertize parent-str 'face (org-recipes--get-heading-face parent))
            " / ")))

(defun org-recipes-dwim ()
  (interactive)
  (if-let ((recipe-list (list-at-point)))
      (org-recipes--process-recipes recipe-list)
    (when-let ((symbol (symbol-at-point))
               (candidates (org-recipes--get-candidates symbol)))
      (org-recipes--delete-thing-at-point symbol)
      (let ((start (point)))
        (org-recipes--insert-candidates candidates)
        (indent-region start (point))
        (newline)))))

(defun org-recipes--process-recipes (recipe-list &optional deleted)
  (let ((start (point))
        (deleted deleted))
    (mapcar (lambda (r)
              (when-let ((candidates (org-recipes--get-candidates r)))
                (unless deleted
                  (org-recipes--delete-thing-at-point recipe-list)
                  (setq deleted t))
                (org-recipes--insert-candidates candidates)
                (newline)))
            recipe-list)
    (indent-region start (point))))

(defun org-recipes--insert-candidates (candidates)
  (mapcar (lambda (c)
            (org-recipes--insert (org-recipes--get-real c)))
          candidates))

(defun org-recipes--delete-thing-at-point (thing)
  (cond ((listp thing)
         (if (looking-at "\(")
             (kill-sexp)
           (backward-up-list)
           (kill-sexp)))
        ((symbolp thing)
         (condition-case nil
             (beginning-of-sexp)
           (error nil))
         (mark-sexp)
         (delete-region (region-beginning) (region-end)))))

(defun org-recipes--get-target-major-modes (buffer)
  (with-current-buffer buffer
    (save-excursion
      (beginning-of-buffer)
      (when (search-forward "#+MODE:" nil t)
        (when-let ((mode-keyword (org-element-property :value (org-element-at-point))))
          (beginning-of-line)
          (mapcar #'org-recipes--string-to-mode
                  (org-recipes--split-mode-list (org-element-property :value (org-element-at-point)))))))))

(defun org-recipes--split-mode-list (mode-list)
  (when mode-list
    (split-string (replace-regexp-in-string "[ ]*" "" mode-list) ",")))

(defun org-recipes--string-to-mode (m)
  (intern-soft (concat m "-mode")))

(provide 'org-recipes)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; org-recipes.el ends here
;; End:
