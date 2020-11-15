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

(require 'ytel)

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
    map)
  "Keymap for `ytel-mode'.")

(defun ytel-show--current-video-id ()
  (aref ytel-show--video-ids ytel-show--index))

(cl-defstruct (ytel-show-video (:constructor ytel-show-video--create)
                               (:copier nil))
  (title "" :read-only t)
  (thumbnail-data nil :read-only t)
  (description "" :read-only t)
  (published 0 :read-only t)
  (length 0 :read-only t)
  (views 0 :read-only t)
  (likes 0 :read-only t)
  (dislikes 0 :read-only t)
  (author-id "" :read-only t))

(cl-defstruct (ytel-show-author (:constructor ytel-show-author--create)
                                (:copier nil))
  (name "" :read-only t)
  (thumbnail-data "" :read-only t)
  (subs "" :read-only t))

(defun ytel-show--video-query (id)
  (ytel--API-call (concat "videos/" id) `(("fields" ,ytel-show--video-fields))))

(defun ytel-show--best-thumbnail (thumbnails)
  (cl-labels ((compare
               ((h1 . u1) (h2 . u2))
               (and (<= h1 h2) (not (string-match-p (regexp-quote ytel-invidious-api-url) u2))))
              (vars (e) (let-alist e (cons .height .url)))
              (pred (acc elm) (if (compare (vars acc) (vars elm)) elm acc)))
    (seq-reduce #'pred (subseq thumbnails 1) (aref thumbnails 0))))

(defun ytel-show--thumbnail-data (thumbnail)
  (cl-flet ((furl (u) (if (string-match-p (rx bos "http" (? "s") ":") u) u (concat "https:" u)))
            (scale (width height)
                   (when (or (< ytel-show-image-max-width width)
                             (< ytel-show-image-max-height height))
                     (list :scale (min (/ ytel-show-image-max-width width)
                                       (/ ytel-show-image-max-height height))))))
    (let-alist thumbnail
      (unless (string-empty-p .url)
        (let ((buffer (url-retrieve-synchronously (furl .url))))
          (unwind-protect
              (let ((data (with-current-buffer buffer
                            (goto-char (point-min))
                            (search-forward "\n\n")
                            (buffer-substring (point) (point-max)))))
                (apply #'create-image data nil t (scale .width .height)))
            (kill-buffer buffer)))))))

(defun ytel-show--cache (id)
  (let-alist (ytel-show--video-query id)
    (let ((video (ytel-show-video--create
                  :title .title
                  :thumbnail-data
                  (ytel-show--thumbnail-data
                   (ytel-show--best-thumbnail .videoThumbnails))
                  :description .description
                  :published .published
                  :length .lengthSeconds
                  :views .viewCount
                  :likes .likeCount
                  :dislikes .dislikeCount
                  :author-id .authorId))
          (author (ytel-show-author--create
                   :name .author
                   :thumbnail-data
                   (ytel-show--thumbnail-data
                    (ytel-show--best-thumbnail .authorThumbnails))
                   :subs .subCountText)))
      (setf (gethash .videoId ytel-show--videos-cache) video
            (gethash .authorId ytel-show--authors-cache) author)
      `((video . ,video) (author . ,author)))))

(defun ytel-show--video-data (id)
  (if-let ((video (gethash id ytel-show--videos-cache)))
      `((video . ,video)
        (author . ,(gethash (ytel-show-video-author-id video)
                            ytel-show--authors-cache)))
    (ytel-show--cache id)))

(defun ytel-show--insert-data (data)
  (let* ((video (cdr (assq 'video data)))
         (title (ytel-show-video-title video))
         (thumbnail-data (ytel-show-video-thumbnail-data video))
         (description (ytel-show-video-description video))
         (published (ytel-show-video-published video))
         (length (ytel-show-video-length video))
         (views (ytel-show-video-views video))
         (likes (ytel-show-video-likes video))
         (dislikes (ytel-show-video-dislikes video))
         (author-id (ytel-show-video-author-id video))

         (author (cdr (assq 'author data)))
         (name (ytel-show-author-name author))
         (author-thumbnail-data (ytel-show-author-thumbnail-data author))
         (subs (ytel-show-author-subs author)))
    (insert title "\n")
    (when thumbnail-data (insert-image thumbnail-data))
    (insert "\n")
    (insert (format "Length: %s | Published: %s | Views: %s | Likes: %s | Dislikes: %s\n"
                    length published views likes dislikes)
            author-id " " name " " subs "\n"
            description "\n")
    (when author-thumbnail-data (insert-image author-thumbnail-data))
    (insert "\n")))

(defun ytel-show-revert-buffer (&rest _)
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (ytel-show--insert-data (ytel-show--video-data (ytel-show--current-video-id)))
    (goto-char (point-min))))

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

(defun ytel-show--update-index (add &optional cycle)
  (let ((new-index (+ add ytel-show--index))
        (length (length ytel-show--video-ids)))
    (cond
     (cycle
      (setq ytel-show--index (mod new-index length)))
     ((<= 0 new-index (1- length))
      (setq ytel-show--index new-index))
     (t (message "Border!")))))

(defun ytel-show-next-video (&optional cycle)
  (interactive "P")
  (ytel-show--update-index 1 cycle)
  (ytel-show-revert-buffer))

(defun ytel-show-previous-video (&optional cycle)
  (interactive "P")
  (ytel-show--update-index -1 cycle)
  (ytel-show-revert-buffer))

(define-derived-mode ytel-show-mode special-mode "Ytel-Show"
  "Mode for blah blah"
  ;; :group not-yet
  (setq-local revert-buffer-function #'ytel-show-revert-buffer))

(provide 'ytel-show)
;;; ytel-show.el ends here
