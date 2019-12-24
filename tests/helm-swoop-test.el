;;; helm-swoop-test.el --- Test definitions for helm-swoop  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Helm-Swoop Community

;; Author: Naoya Yamashita <conao3@gmail.com>
;; License: GPL-3.0
;; Homepage: https://github.com/emacsorphanage/helm-swoop

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

;; Test definitions for `helm-swoop'.


;;; Code:

(require 'buttercup)
(require 'helm-swoop)

(describe "A suite"
  (it "contains a spec with an expectation"
    (expect t :to-be t)))

;; (provide 'helm-swoop-test)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; helm-swoop-test.el ends here
