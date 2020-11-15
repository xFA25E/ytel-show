;;; ytel-show.el --- Show youtube video description from ytel  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Valeriy Litkovskyy

;; Author: Valeriy Litkovskyy
;; Keywords: multimedia

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:


;;;; IMPORTS

(require 'files)
(require 'window)
(require 'simple)
(require 'subr)
(require 'subr-x)
(require 'seq)
(require 'ytel)


;;;; TYPES

(cl-defstruct (ytel-show--video (:constructor ytel-show--video-create)
                                (:copier nil))
  title thumbnail-data description published
  length views likes dislikes author-id)

(cl-defstruct (ytel-show--author (:constructor ytel-show--author-create)
                                 (:copier nil))
  name thumbnail-data subs)


;;;; VARIABLES

(defvar ytel-show--video-fields
  (string-join
   '("videoId" "title"
     "videoThumbnails(url,width,height)"
     "description" "published" "lengthSeconds"
     "viewCount" "likeCount" "dislikeCount"
     "author" "authorId" "authorThumbnails" "subCountText")
   ","))

(defvar-local ytel-show--video-ids nil)
(defvar-local ytel-show--index 0)

(defvar ytel-show-image-max-width 600.0)
(defvar ytel-show-image-max-height 350.0)

(defvar ytel-show--videos-cache (make-hash-table :test 'equal :size 20))
(defvar ytel-show--authors-cache (make-hash-table :test 'equal :size 20))

(defvar ytel-show-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "n" #'ytel-show-next-video)
    (define-key map "p" #'ytel-show-previous-video)
    (define-key map "G" #'ytel-show-reload-video-data)
    map)
  "Keymap for `ytel-show-mode'.")


;;;; FUNCTIONS

(defun ytel-show--current-video-id ()
  (aref ytel-show--video-ids ytel-show--index))

(defun ytel-show--query-video (id)
  (ytel--API-call (concat "videos/" id) `(("fields" ,ytel-show--video-fields))))


;;;;; THUMBNAILS

(defun ytel-show--filter-thumbnails (thumbnails)
  (cl-flet ((valid-thumbnail-p (thumbnail)
             (let-alist thumbnail
               (and (numberp .width) (< 0 .width)
                    (numberp .height) (< 0 .height)
                    (stringp .url) (not (string-empty-p .url))
                    (string-match-p (rx bos "http" (? "s") "://") .url)
                    (not (string-match-p (regexp-quote ytel-invidious-api-url) .url))))))
    (seq-filter #'valid-thumbnail-p thumbnails)))

(defun ytel-show--select-thumbnail (thumbnails)
  (cl-case (length thumbnails)
    (0 nil)
    (1 (elt thumbnails 0))
    (t (cl-labels ((height (e) (cdr (assq 'height e)))
                   (cmp (e1 e2) (<= (height e1) (height e2)))
                   (pred (acc elm) (if (cmp acc elm) elm acc)))
         (seq-reduce #'pred (seq-subseq thumbnails 1) (elt thumbnails 0))))))

(defun ytel-show--thumbnail-load-data (thumbnail)
  (when thumbnail
    (cl-flet ((scale (width height)
               (when (or (< ytel-show-image-max-width width)
                         (< ytel-show-image-max-height height))
                 (list :scale (min (/ ytel-show-image-max-width width)
                                   (/ ytel-show-image-max-height height))))))
      (let-alist thumbnail
        (let ((buffer (url-retrieve-synchronously .url)))
          (unwind-protect
              (let ((data (with-current-buffer buffer
                            (goto-char (point-min))
                            (search-forward "\n\n")
                            (buffer-substring (point) (point-max)))))
                (apply #'create-image data nil t (scale .width .height)))
            (kill-buffer buffer)))))))

(defun ytel-show--process-thumbnails (thumbnails)
  (thread-last thumbnails
    ytel-show--filter-thumbnails
    ytel-show--select-thumbnail
    ytel-show--thumbnail-load-data))


;;;;; UPDATE

(defun ytel-show--update-video (video query-response)
  (let-alist query-response
    (setf
     (ytel-show--video-thumbnail-data video) (ytel-show--process-thumbnails .videoThumbnails)
     (ytel-show--video-title video) .title
     (ytel-show--video-description video) .description
     (ytel-show--video-published video) .published
     (ytel-show--video-length video) .lengthSeconds
     (ytel-show--video-views video) .viewCount
     (ytel-show--video-likes video) .likeCount
     (ytel-show--video-dislikes video) .dislikeCount
     (ytel-show--video-author-id video) .authorId))
  video)

(defun ytel-show--update-author (author query-response)
  (let-alist query-response
    (setf
     (ytel-show--author-thumbnail-data author) (ytel-show--process-thumbnails .authorThumbnails)
     (ytel-show--author-name author) .author
     (ytel-show--author-subs author) .subCountText))
  author)

(defun ytel-show--update-cache (id)
  (let* ((query-response (ytel-show--query-video id))
         (video (ytel-show--update-video
                 (or (gethash id ytel-show--videos-cache)
                     (puthash id (ytel-show--video-create)
                              ytel-show--videos-cache))
                 query-response))
         (author-id (ytel-show--video-author-id video))
         (author (ytel-show--update-author
                  (or (gethash author-id ytel-show--authors-cache)
                      (puthash author-id (ytel-show--author-create)
                               ytel-show--authors-cache))
                  query-response)))
    `((id . ,id) (video . ,video) (author . ,author))))

(defun ytel-show--update-index (add &optional cycle)
  (let ((new-index (+ add ytel-show--index))
        (length (length ytel-show--video-ids)))
    (cond
     (cycle
      (setq ytel-show--index (mod new-index length)))
     ((<= 0 new-index (1- length))
      (setq ytel-show--index new-index))
     (t (message "Border!")))))


;;;;; DATA
(defun ytel-show--video-data (id)
  (if-let* ((video (gethash id ytel-show--videos-cache))
            (author (gethash (ytel-show--video-author-id video)
                             ytel-show--authors-cache)))
      `((id . ,id) (video . ,video) (author . ,author))
    (ytel-show--update-cache id)))


;;;;; DRAW

(defun ytel-show--draw-data (data)
  (let* ((id (cdr (assq 'id data)))
         (video (cdr (assq 'video data)))
         (title (ytel-show--video-title video))
         (thumbnail-data (ytel-show--video-thumbnail-data video))
         (description (ytel-show--video-description video))
         (published (ytel-show--video-published video))
         (length (ytel-show--video-length video))
         (views (ytel-show--video-views video))
         (likes (ytel-show--video-likes video))
         (dislikes (ytel-show--video-dislikes video))
         (author-id (ytel-show--video-author-id video))

         (author (cdr (assq 'author data)))
         (name (ytel-show--author-name author))
         (author-thumbnail-data (ytel-show--author-thumbnail-data author))
         (subs (ytel-show--author-subs author)))
    (insert (format "%s | %s\n" id title))
    (when thumbnail-data
      (insert-image thumbnail-data)
      (insert "\n"))
    (insert (format "Length: %s | Published: %s | Views: %s | Likes: %s | Dislikes: %s\n%s %s %s\n%s\n"
                    length published views likes dislikes
                    author-id name subs description))
    (when author-thumbnail-data
      (insert-image author-thumbnail-data)
      (insert "\n"))))


;;;; COMMANDS

(defun ytel-show-revert-buffer (&rest _)
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (ytel-show--draw-data (ytel-show--video-data (ytel-show--current-video-id)))
    (goto-char (point-min))))

(defun ytel-show-next-video (&optional cycle)
  (interactive "P")
  (ytel-show--update-index 1 cycle)
  (ytel-show-revert-buffer))

(defun ytel-show-previous-video (&optional cycle)
  (interactive "P")
  (ytel-show--update-index -1 cycle)
  (ytel-show-revert-buffer))

(defun ytel-show-reload-video-data ()
  (interactive)
  (ytel-show--update-cache (ytel-show--current-video-id))
  (ytel-show-revert-buffer))

(defun ytel-show (video-ids index)
  (interactive (if (not (derived-mode-p 'ytel-mode))
                   (error "Not in the ytel buffer")
                 (list (vconcat (seq-map #'ytel-video-id ytel-videos))
                       (1- (line-number-at-pos)))))
  (switch-to-buffer (get-buffer-create "*ytel-show*"))
  (unless (derived-mode-p 'ytel-show-mode)
    (ytel-show-mode))
  (setq ytel-show--video-ids video-ids ytel-show--index index)
  (ytel-show-revert-buffer))

(define-derived-mode ytel-show-mode special-mode "Ytel-Show"
  "Mode for blah blah"
  ;; :group not-yet
  (setq-local revert-buffer-function #'ytel-show-revert-buffer))

(provide 'ytel-show)
;;; ytel-show.el ends here
