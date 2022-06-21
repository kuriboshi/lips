#
# alias -- functions defining the alias commands
#
# Copyright 1989, 2020, 2022 Krister Joas.
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#

(setq aliaslist)

(defineq
  (alias
   (nlambda (a . b)
     (cond ((and (null a) (null b))
            (mapc aliaslist
                  '(lambda (x)
                    (prin2 x)
                    (prin1 "	")
                    (print (getprop x 'alias)))))
           ((null b)
            (print (getprop a 'alias)))
           (t (putprop a 'alias b)
              (cond
                ((memb a aliaslist))
                (t (setq aliaslist (cons a aliaslist))))))))
  (unalias
   (nlambda (a)
     (cond ((null a))
           (t (remprop a 'alias)))))
  )
