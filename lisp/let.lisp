#
# let -- the let function
#
# Copyright 1992, 2020, 2022 Krister Joas.
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

(defineq
  (let (nlambda (init . seq)
         (eval (cons (cons 'lambda
                           (cons (vlis init) seq))
                     (alis init)))))

  (vlis (lambda (u)
          (cond
           ((null u) nil)
           ((atom (car u)) (cons (car u) (vlis (cdr u))))
           (t (cons (caar u) (vlis (cdr u)))))))

  (alis (lambda (u)
          (cond
           ((null u) nil)
           ((atom (car u)) (cons nil (alis (cdr u))))
           (t (cons (car (cdar u))
                    (alis (cdr u))))))))
