;;; convert-music --- Convert video clips to audio only
;;; Copyright © 2019 Oleg Pykhalov <go.wigust@gmail.com>
;;;
;;; This file is part of convert-music.
;;;
;;; convert-music is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; convert-music is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with convert-music.  If not, see <http://www.gnu.org/licenses/>.


;; This file contains machinery convert all my video clips to audio
;; only.  To do that, run:
;;
;;  guile -l guile/convert-music.scm -c exit

(use-modules (ice-9 readline)
             (srfi srfi-1)
             (srfi srfi-26)
             (ice-9 ftw)
             (ice-9 match))

(define (list-files dir)
  (filter (match-lambda
            ((name _) name)
            (_ #f))
          (file-system-tree dir)))

(for-each (lambda (file) (match file
                      ((n ... ext)
                       (system* "ffmpeg"
                                "-i" (string-append "/src/music/" (string-join file "."))
                                "-vn"
                                "-f" "webm"
                                (string-append "/src/music/webm/" (apply string-append n) "." "webm")))))
          (map (cut string-split <> #\.)
               (map (match-lambda ((file _ ...) file))
                    (list-files (getcwd)))))
