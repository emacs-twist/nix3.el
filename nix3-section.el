;;; nix3-section.el --- Magit sections -*- lexical-binding: t -*-

;; Copyright (C) 2022,2023 Akira Komamura

;; Author: Akira Komamura <akira.komamura@gmail.com>
;; URL: https://github.com/emacs-twist/nix3.el

;; This file is not part of GNU Emacs.

;;; License:

;; This library is free software; you can redistribute it and/or
;; modify it under the terms of the GNU Lesser General Public
;; License as published by the Free Software Foundation; either
;; version 2.1 of the License, or (at your option) any later version.
;;
;; This library is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; Lesser General Public License for more details.
;;
;; You should have received a copy of the GNU Lesser General Public
;; License along with this library; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

;;; Commentary:

;; FIXME

;;; Code:

(require 'magit-section)

(defmacro nix3-section-with-keymap (map &rest body)
  (declare (indent 1))
  `(let ((start (point)))
     (prog1 (progn
              ,@body)
       (let ((ov (make-overlay start (point))))
         (overlay-put ov 'keymap ,map)))))

(defmacro nix3-section--heading (body)
  `(propertize ,body 'face 'magit-section-heading))

(defmacro nix3-section-dlist (indent-level &rest rows)
  (declare (indent 1))
  (let* ((width (1+ (cl-loop for header in (mapcar #'car rows)
                             maximize (length header))))
         (indent-string (make-string indent-level ?\ ))
         (th (format "%s%%-%ds" indent-string width)))
    `(progn
       ,@(mapcar (pcase-lambda (`(,header ,visible ,exp))
                   `(when ,visible
                      (magit-insert-section ('row ,header)
                        (insert (nix3-section--heading (format ,th ,header)))
                        ,exp
                        (newline))))
                 rows))))

(provide 'nix3-section)
;;; nix3-section.el ends here
