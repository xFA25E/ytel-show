;;; ytel-show.el --- View youtube video information from ytel  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Valeriy Litkovskyy

;; Author: Valeriy Litkovskyy
;; Keywords: youtube multimedia
;; Version: 0.1.0
;; URL: https://github.com/xFA25E/ytel-show
;; Package-Requires: ((ytel "0.1.0") (emacs "26.1"))

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

;; Ytel-Show: view youtube video information in Emacs
;;
;; This package is dependent on `YTEL' package.
;;
;;
;; Easy setup:
;;
;; (define-key ytel-mode-map (kbd "RET") #'ytel-show)
;;
;; OR
;;
;; (use-package ytel-show
;;   :after ytel
;;   :bind (:map ytel-mode-map ("RET" . ytel-show)))
;;
;;
;; Usage:
;;
;; n/p - switch videos
;; G - recache video information
;;
;;
;; Customization:
;;
;; Default buffer name: `YTEL-SHOW-DEFAULT-BUFFER-NAME'.
;;
;; Thumbnail sizes: `YTEL-SHOW-IMAGE-MAX-WIDTH' `YTEL-SHOW-IMAGE-MAX-HEIGHT'
;;
;; Faces: `YTEL-SHOW-VIDEO-LIKES-FACE' `YTEL-SHOW-AUTHOR-SUBS-FACE'
;;
;;
;; Extensibility:
;;
;; You can easily extend this package by giving `YTEL-SHOW' function a `BUFFER'
;; argument and a vector of `VIDEO-IDS'.  Your videos will be display in a new
;; buffer.  For example: you could customize `BROWSE-URL-BROWSER-FUNCTION' to
;; detect youtube video urls and display them using `YTEL-SHOW'.
;;
;;
;; Note:
;;
;; Viewed videos are set with `YTEL-SHOW' command.  In order to load videos from
;; other *ytel* pages, you should switch page in *ytel* buffer and call
;; `YTEL-SHOW' again.
;;
;;
;; Caveats:
;;
;; Sometimes json-reader will fail to parse a response from invidious instance
;; or the connection will timeout.  You will see an almost empty buffer.
;; Usually,hitting the /G/ (`YTEL-SHOW-RELOAD-VIDEO-DATA') key a couple of times
;; helps.
;;
;; Sometimes invidous instance will send empty author thumbnail urls, so they
;; will not be shown.
;;
;; Currently, I don't have a solution for these problems.

;;; Code:


;;;; IMPORTS

(require 'files)
(require 'simple)
(require 'subr-x)
(require 'seq)
(require 'shr)
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
  (string-join '("videoId" "title" "videoThumbnails"
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

(defcustom ytel-show-default-buffer-name "*ytel-show*"
  "Default buffer name used when the `BUFFER' argument in `YTEL-SHOW' is nil."
  :type 'string
  :group 'ytel-show)

(defcustom ytel-show-image-max-width 800.0
  "Max image width.  This variable is used to scale images in buffer."
  :type 'float
  :group 'ytel-show)

(defcustom ytel-show-image-max-height 400.0
  "Max image height.  This variable is used to scale images in buffer."
  :type 'float
  :group 'ytel-show)

(defface ytel-show-video-likes-face
  '((t :inherit font-lock-doc-face))
  "Face used for the video likes and dislikes."
  :group 'ytel-show)

(defface ytel-show-author-subs-face
  '((t :inherit font-lock-string-face))
  "Face used for the author subs."
  :group 'ytel-show)


;;;; FUNCTIONS

(defun ytel-show--current-video-id ()
  "Get currently showed video id."
  (aref ytel-show--video-ids ytel-show--index))

(defun ytel-show--query-video (id)
  "Query invidous instance for video data by `ID'."
  (ytel--API-call (concat "videos/" id) `(("fields" ,ytel-show--video-fields))))


;;;;; THUMBNAILS

(defun ytel-show--repair-thumbnail-urls (thumbnails)
  "Repair broken urls in `THUMBNAILS'.
Some urls might be without \"https:\" or \"https://i.ytimg.com\" prefix.  Detect
these urls and try to repair them."
  (let* ((id (ytel-show--current-video-id))
         (single-slash-rx (rx bos "/vi/" (literal id) "/maxres.jpg" eos)))
    (seq-map
     (lambda (thumbnail)
       (let-alist thumbnail
         (cond ((string-match-p single-slash-rx .url)
                (setf (alist-get 'url thumbnail) (concat "https://i.ytimg.com" .url)))
               ((string-match-p (rx bos "//yt3.ggpht.com") .url)
                (setf (alist-get 'url thumbnail) (concat "https:" .url)))))
       thumbnail)
     thumbnails)))

(defun ytel-show--filter-thumbnails (thumbnails)
  "Filter `THUMBNAILS' from invidious response.
`THUMBNAILS' is a vector of alists with keys: url, width and height.  Return a
sequence with alists with the following properties:

  url

    non empty string that tharts with \"https?://\" and does not includes
    `YTEL-INVIDIOUS-API-URL'

  width and height

    number greater than 0"
  (seq-filter
   (lambda (thumbnail)
     (let-alist thumbnail
       (and (numberp .width) (< 0 .width)
            (numberp .height) (< 0 .height)
            (stringp .url) (not (string-empty-p .url))
            (string-match-p (rx bos "http" (? "s") "://") .url)
            (not (string-match-p (regexp-quote ytel-invidious-api-url) .url)))))
   thumbnails))

(defun ytel-show--retrieve-image (url)
  "Synchronously retrieve image from `URL'.
On 404 return :not-found."
  (let ((buffer (url-retrieve-synchronously url)))
    (unwind-protect
        (with-current-buffer buffer
          (goto-char (point-min))
          (if (search-forward "HTTP/1.1 404" nil t)
              :not-found
            (search-forward "\n\n")
            (buffer-substring (point) (point-max))))
      (kill-buffer buffer))))

(defun ytel-show--valid-image-sizes-p (width height)
  "Check whether `WIDTH' and `HEIGHT' are smaller than max allowed values."
  (or (< ytel-show-image-max-width width) (< ytel-show-image-max-height height)))

(defun ytel-show--scale-images-sizes (width height)
  "Scale `WIDTH' and `HEIGHT' to fit max allowed values."
  (min (/ ytel-show-image-max-width width) (/ ytel-show-image-max-height height)))

(defun ytel-show--thumbnail-load-data (thumbnail)
  "Load image data from `THUMBNAIL's url.
Scale it to satisfy `YTEL-SHOW-IMAGE-MAX-WIDTH' and
`YTEL-SHOW-IMAGE-MAX-HEIGHT'."
  (when thumbnail
    (let-alist thumbnail
      (let ((data (ytel-show--retrieve-image .url)))
        (cond ((eq :not-found data) data)
              ((ytel-show--valid-image-sizes-p .width .height)
               (create-image
                data nil t :scale (ytel-show--scale-images-sizes .width .height)))
              (t (create-image data nil t)))))))

(defun ytel-show--select-thumbnail (thumbnails)
  "Select thumbnail from `THUMBNAILS' with the biggest height."
  (seq-reduce
   (lambda (thumbnail next)
     (if (<= (alist-get 'height thumbnail) (alist-get 'height next))
         next
       thumbnail))
   (seq-subseq thumbnails 1) (elt thumbnails 0)))

(defun ytel-show--delete-thumbnail (thumbnail thumbnails)
  "Delete element from `THUMBNAILS' matching `THUMBNAIL's url."
  (cl-delete (alist-get 'url thumbnail) thumbnails
             :test #'string-equal
             :key (lambda (thumbnail) (alist-get 'url thumbnail))))

(defun ytel-show--get-thumbnail (thumbnails)
  "Select thumbnail from `THUMBNAILS' and load it's data."
  (cl-case (length thumbnails)
    (0 nil)
    (1 (elt thumbnails 0))
    (t (let* ((thumbnail (ytel-show--select-thumbnail thumbnails))
              (data (ytel-show--thumbnail-load-data thumbnail)))
         (if (and (not (eq :not-found data)) data) data
           (ytel-show--get-thumbnail
            (ytel-show--delete-thumbnail thumbnail thumbnails)))))))

(defun ytel-show--process-thumbnails (thumbnails)
  "Repair `THUMBNAILS' | filter | select | load."
  (thread-last thumbnails
    ytel-show--repair-thumbnail-urls
    ytel-show--filter-thumbnails
    ytel-show--get-thumbnail))


;;;;; UPDATE

(defun ytel-show--update-video (video query-response)
  "Update `VIDEO' struct with the data from `QUERY-RESPONSE'.
Return updated `VIDEO'."
  (let-alist query-response
    (when-let ((thumbnail (ytel-show--process-thumbnails .videoThumbnails)))
      (setf (ytel-show--video-thumbnail-data video) thumbnail))

    (setf (ytel-show--video-title video) .title
          (ytel-show--video-description video) .description
          (ytel-show--video-published video) .published
          (ytel-show--video-length video) .lengthSeconds
          (ytel-show--video-views video) .viewCount
          (ytel-show--video-likes video) .likeCount
          (ytel-show--video-dislikes video) .dislikeCount
          (ytel-show--video-author-id video) .authorId))
  video)

(defun ytel-show--update-author (author query-response)
  "Update `AUTHOR' struct with the data from `QUERY-RESPONSE'.
Return updated `AUTHOR'."
  (let-alist query-response
    (when-let ((thumbnail (ytel-show--process-thumbnails .authorThumbnails)))
      (setf (ytel-show--author-thumbnail-data author) thumbnail))

    (setf (ytel-show--author-name author) .author
          (ytel-show--author-subs author) .subCountText))
  author)

(defun ytel-show--update-cache (id)
  "Update cached data by video `ID'.
Return an alist with the following keys:

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
  "Update `YTEL-SHOW--INDEX' by adding `ADD' to it.
If `CYCLE' is non-nil then the addition is performed with modulus."
  (let ((new-index (+ add ytel-show--index))
        (length (length ytel-show--video-ids)))
    (cond
     (cycle
      (setq ytel-show--index (mod new-index length)))
     ((<= 0 new-index (1- length))
      (setq ytel-show--index new-index)))))


;;;;; DATA

(defun ytel-show--video-data (id)
  "Get cached data by video `ID'.
If the video is not cached, then query the invidious instance.  Return value is
described in `YTEL-SHOW--UPDATE-CACHE'."
  (if-let* ((video (gethash id ytel-show--videos-cache))
            (author (gethash (ytel-show--video-author-id video)
                             ytel-show--authors-cache)))
      `((id . ,id) (video . ,video) (author . ,author))
    (ytel-show--update-cache id)))


;;;;; DRAW

(defun ytel-show--draw-url (url title)
  "Drow `URL' using shr with `TITLE' at current point."
  (let ((point (point)))
    (insert (format "%s" title))
    (make-text-button
     point (point)
     'follow-link t 'mouse-face 'highlight 'shr-url url 'keymap shr-map)))

(defun ytel-show--format-video-likes-dislikes (likes dislikes)
  "Format `LIKES' and `DISLIKES' before inserting them to buffer."
  (let ((s (format "[likes:%s/%s]" likes dislikes)))
    (propertize s 'face 'ytel-show-video-likes-face)))

(defun ytel-show--format-author-subs (subs)
  "Format `SUBS' before inserting them to buffer."
  (propertize (format "(%s)" subs) 'face 'ytel-show-author-subs-face))

(defun ytel-show--draw-data (data)
  "Draw video `DATA' to the buffer.
`DATA' is the value returned from `YTEL-SHOW--VIDEO-DATA'."
  (let* ((id (alist-get 'id data))

         ;; video
         (video (alist-get 'video data))
         (title (ytel-show--video-title video))
         (thumbnail-data (ytel-show--video-thumbnail-data video))
         (description (ytel-show--video-description video))
         (published (ytel-show--video-published video))
         (length (ytel-show--video-length video))
         (views (ytel-show--video-views video))
         (likes (ytel-show--video-likes video))
         (dislikes (ytel-show--video-dislikes video))
         (author-id (ytel-show--video-author-id video))

         ;; author
         (author (alist-get 'author data))
         (name (ytel-show--author-name author))
         (author-thumbnail-data (ytel-show--author-thumbnail-data author))
         (subs (ytel-show--author-subs author)))

    (ytel-show--draw-url
     (concat "https://www.youtube.com/watch?v=" id) (or title "no title"))
    (insert "  -  ")

    (when name
      (ytel-show--draw-url (concat "https://www.youtube.com/channel/" author-id) name) (insert " "))

    (when subs
      (insert (ytel-show--format-author-subs subs) "\n"))

    (when length
      (insert (ytel--format-video-length length) "  -  "))

    (when published
      (insert (ytel--format-video-published published) "  -  "))

    (when views
      (insert (ytel--format-video-views views) "  -  "))

    (when (or likes dislikes)
      (insert (ytel-show--format-video-likes-dislikes likes dislikes) "\n"))

    (when thumbnail-data
      (insert-image thumbnail-data) (insert "\n"))

    (when description
      (insert description "\n"))

    (when author-thumbnail-data
      (insert-image author-thumbnail-data) (insert "\n"))))


;;;; COMMANDS

(defun ytel-show-revert-buffer (&rest _)
  "Revert current Ytel-Show buffer.
Erase the buffer and redraw current video's data."
  (interactive)
  (let ((inhibit-read-only t)
        (id (ytel-show--current-video-id)))
    (message "Loading %s..." id)
    (erase-buffer)
    (ytel-show--draw-data (ytel-show--video-data id))
    (goto-char (point-min))
    (message "Showing %s..." id)))

(defun ytel-show-next-video (&optional cycle)
  "Switch to the next video in `YTEL-SHOW--VIDEO-IDS' and draw it.
With overflow, cycle if `CYCLE' is provided."
  (interactive "P")
  (let ((inx ytel-show--index))
    (ytel-show--update-index 1 cycle)
    (if (= inx ytel-show--index)
        (message "Wall! Use argument to cycle!")
      (ytel-show-revert-buffer))))

(defun ytel-show-previous-video (&optional cycle)
  "Switch to the previous video in `YTEL-SHOW--VIDEO-IDS' and draw it.
With overflow, cycle if `CYCLE' is provided."
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
(defun ytel-show (video-ids index &optional buffer)
  "Show video information in the Ytel-Show buffer.
This is the main entry function for this package.  Interactively, it must be
called from a `YTEL' buffer.

`VIDEO-IDS' is a vector of strings with youtube video ids to show.

`INDEX' is the index of some video in `VIDEO-IDS' that will be cached and
displayed first.

If `BUFFER' is nil, then `YTEL-SHOW-DEFAULT-BUFFER-NAME' is used.  To display
videos from `VIDEO-IDS'."
  (interactive (if (not (derived-mode-p 'ytel-mode))
                   (error "Not in the ytel buffer")
                 (list (vconcat (seq-map #'ytel-video-id ytel-videos))
                       (1- (line-number-at-pos)))))

  (unless (and (vectorp video-ids)
               (seq-every-p (lambda (s) (and (stringp s) (= 11 (length s)))) video-ids)
               (integerp index) (<= 0 index (1- (length video-ids))))
    (error "Invalid arguments to `YTEL-SHOW'"))

  (switch-to-buffer
   (get-buffer-create (or buffer ytel-show-default-buffer-name)))

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
