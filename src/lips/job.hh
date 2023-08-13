//
// Lips, lisp shell.
// Copyright 1988, 2020-2023 Krister Joas
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
//

#include <string>

#include <lisp/lisp.hh>

namespace lisp::job
{

struct stat_t { int stat; };

struct job_t
{
  int jobnum = 0;          // Job number
  int procid = 0;          // Process id
  int status = 0;          // Return value
  std::string wdir;        // Working directory
  lisp::lisp_t exp;        // Job expression
  bool background = false; // true means job runs in bg
  bool running = false;    // true if running
};

void printjob(const job_t& job);
job_t createjob(int pid);
void recordjob(job_t job, bool bg);
void collectjob(int pid, stat_t stat);
void printdone();
void printjobs();
job_t* findjob(std::function<bool(const job_t&)> f);

}
