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

;; Ytel-Show: view youtube video details in emacs
;;
;; This package is dependent on `YTEL' package.
;;
;;
;; Easy setup:
;;
;; (define-key ytel-mode-map (kbd "RET") #'ytel-show)
;;
;;
;; Usage:
;;
;; n/p - to switch videos
;; G - to recache video information
;;
;;
;; Note:
;;
;; Viewed videos are set with `YTEL-SHOW' command.  In order to go to next page
;; videos, you should first switch page in `YTEL' buffer and then call
;; `ytel-show' again.
;;
;;
;; Caveats:
;;
;; Sometimes json-reader will fail to parse a response from invidious instance
;; or the connection will timeout.  You will see an almost empty buffer.
;; Usually, hitting the "G" (`YTEL-SHOW-RELOAD-VIDEO-DATA') key a couple of
;; times helps.  Currently, I don't have a solution for this.

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
  "Cached video data."
  title thumbnail-data description published
  length views likes dislikes author-id)

(cl-defstruct (ytel-show--author (:constructor ytel-show--author-create)
                                 (:copier nil))
  "Cached author data."
  name thumbnail-data subs)


;;;; VARIABLES


;;;;; INTERNAL

(defvar-local ytel-show--video-ids nil
  "A vector of stored video ids.")

(defvar-local ytel-show--index 0
  "Current video index in `YTEL-SHOW--VIDEO-IDS'.")

(defvar ytel-show--video-fields
  (string-join
   '("videoId" "title"
     "videoThumbnails(url,width,height)"
     "description" "published" "lengthSeconds"
     "viewCount" "likeCount" "dislikeCount"
     "author" "authorId" "authorThumbnails" "subCountText")
   ",")
  "Requested fields in a query to indidious instance.  Used to save bandwidth.")

(defvar ytel-show--videos-cache (make-hash-table :test 'equal :size 20)
  "Hash-table with cached video data.")

(defvar ytel-show--authors-cache (make-hash-table :test 'equal :size 20)
  "Hash-table with cached author data.")


;;;;; MODE

(defvar ytel-show-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "n" #'ytel-show-next-video)
    (define-key map "p" #'ytel-show-previous-video)
    (define-key map "G" #'ytel-show-reload-video-data)
    map)
  "Keymap for `YTEL-SHOW-MODE'.")


;;;;; CUSTOM

(defgroup ytel-show nil
  "View youtube video details from ytel."
  :group 'applications)

(defcustom ytel-show-image-max-width 600.0
  "Max image width.  This variable is used to scale images in buffer."
  :type 'float
  :group 'ytel-show)

(defcustom ytel-show-image-max-height 350.0
  "Max image height.  This variable is used to scale images in buffer."
  :type 'float
  :group 'ytel-show)


;;;; FUNCTIONS

(defun ytel-show--current-video-id ()
  "Get currently showed video id."
  (aref ytel-show--video-ids ytel-show--index))

(defun ytel-show--query-video (id)
  "Query invidous instance for video data by id."
  (ytel--API-call (concat "videos/" id) `(("fields" ,ytel-show--video-fields))))


;;;;; THUMBNAILS

(defun ytel-show--filter-thumbnails (thumbnails)
  "Filter `THUMBNAILS' from invidious response.  `THUMBNAILS' is a vector of
alists with keys: url, width and height.  Return a sequence with alists with the
following properties:

  url

    non empty string that tharts with \"https?://\" and does not includes
    `YTEL-INVIDIOUS-API-URL'

  width and height

    number greater than 0"
  (cl-flet ((valid-thumbnail-p (thumbnail)
             (let-alist thumbnail
               (and (numberp .width) (< 0 .width)
                    (numberp .height) (< 0 .height)
                    (stringp .url) (not (string-empty-p .url))
                    (string-match-p (rx bos "http" (? "s") "://") .url)
                    (not (string-match-p (regexp-quote ytel-invidious-api-url) .url))))))
    (seq-filter #'valid-thumbnail-p thumbnails)))

(defun ytel-show--select-thumbnail (thumbnails)
  "Select thumbnail from `THUMBNAILS' with the biggest height."
  (cl-case (length thumbnails)
    (0 nil)
    (1 (elt thumbnails 0))
    (t (cl-labels ((height (e) (cdr (assq 'height e)))
                   (cmp (e1 e2) (<= (height e1) (height e2)))
                   (pred (acc elm) (if (cmp acc elm) elm acc)))
         (seq-reduce #'pred (seq-subseq thumbnails 1) (elt thumbnails 0))))))

(defun ytel-show--thumbnail-load-data (thumbnail)
  "Load image data from url and scale it to satisfy `YTEL-SHOW-IMAGE-MAX-WIDTH'
and `YTEL-SHOW-IMAGE-MAX-HEIGHT'."
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
  "Filter thumbnails | Select | Load"
  (thread-last thumbnails
    ytel-show--filter-thumbnails
    ytel-show--select-thumbnail
    ytel-show--thumbnail-load-data))


;;;;; UPDATE

(defun ytel-show--update-video (video query-response)
  "Update `VIDEO' struct with the data from `QUERY-RESPONSE'.  Return updated
`VIDEO'."
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
  "Update `AUTHOR' struct with the data from `QUERY-RESPONSE'.  Return updated
`AUTHOR'."
  (let-alist query-response
    (setf
     (ytel-show--author-thumbnail-data author) (ytel-show--process-thumbnails .authorThumbnails)
     (ytel-show--author-name author) .author
     (ytel-show--author-subs author) .subCountText))
  author)

(defun ytel-show--update-cache (id)
  "Update video's and author's cached data by video `ID' by querying invidious
instance.  Return an alist with the following keys:

  id - video `ID'

  video - video struct with cached data

  author - author struct with cached data"
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
  "Update `YTEL-SHOW--INDEX' by adding `ADD' to it.  If `CYCLE' is non-nil then
the addition is performed with modulus."
  (let ((new-index (+ add ytel-show--index))
        (length (length ytel-show--video-ids)))
    (cond
     (cycle
      (setq ytel-show--index (mod new-index length)))
     ((<= 0 new-index (1- length))
      (setq ytel-show--index new-index)))))


;;;;; DATA

(defun ytel-show--video-data (id)
  "Get cached data by video `ID'.  If the video is not cached, then query the
invidious instance.  Return value is described in `YTEL-SHOW--UPDATE-CACHE'."
  (if-let* ((video (gethash id ytel-show--videos-cache))
            (author (gethash (ytel-show--video-author-id video)
                             ytel-show--authors-cache)))
      `((id . ,id) (video . ,video) (author . ,author))
    (ytel-show--update-cache id)))


;;;;; DRAW

(defun ytel-show--draw-data (data)
  "Draw video `DATA' to the buffer.  `DATA' is the value returned from
`YTEL-SHOW--VIDEO-DATA'."
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
  "Revert current Ytel-Show buffer.  Erase the buffer and redraw current video's
data."
  (interactive)
  (let ((inhibit-read-only t)
        (id (ytel-show--current-video-id)))
    (message "Loading %s..." id)
    (erase-buffer)
    (ytel-show--draw-data (ytel-show--video-data id))
    (goto-char (point-min))
    (message "Showing %s..." id)))

(defun ytel-show-next-video (&optional cycle)
  "Switch to the next video in `YTEL-SHOW--VIDEO-IDS' and draw it."
  (interactive "P")
  (let ((inx ytel-show--index))
    (ytel-show--update-index 1 cycle)
    (if (= inx ytel-show--index)
        (message "Wall! Use argument to cycle!")
      (ytel-show-revert-buffer))))

(defun ytel-show-previous-video (&optional cycle)
  "Switch to the previous video in `YTEL-SHOW--VIDEO-IDS' and draw it."
  (interactive "P")
  (let ((inx ytel-show--index))
    (ytel-show--update-index -1 cycle)
    (if (= inx ytel-show--index)
        (message "Wall! Use argument to cycle!")
      (ytel-show-revert-buffer))))

(defun ytel-show-reload-video-data ()
  "Recache current video's data and redraw the current buffer."
  (interactive)
  (message "Reloading %s..." (ytel-show--current-video-id))
  (ytel-show--update-cache (ytel-show--current-video-id))
  (ytel-show-revert-buffer))

;;;###autoload
(defun ytel-show (video-ids index)
  "Show video information in the Ytel-Show buffer.  This is the main entry
function for this package.  Interactively, it must be called from a `YTEL'
buffer.

`VIDEO-IDS' is a vector of strings with youtube video ids to show.  `INDEX' is
the index of some video in `VIDEO-IDS' that will be cached and displayed first."
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
  "Mode for displaying youtube video information."
  :group 'ytel-show
  (setq-local revert-buffer-function #'ytel-show-revert-buffer))

(provide 'ytel-show)

;;; ytel-show.el ends here
