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

#include <list>
#include <filesystem>
#include <sys/wait.h>
#include <csignal>
#include <cstdlib>
#include <fmt/format.h>
#include <lisp/lisp.hh>

#include "job.hh"
#include "top.hh"

// Routines for managing jobs. Jobs are saved on a linked list joblist. When a
// job exits, storage associated with it are released unless it was run in
// background in which case it is saved on another list, cjoblist. This list is
// freed when a background job exits, signaling its parent with a SIGCHLD.

namespace
{
// NOLINTBEGIN(cppcoreguidelines-avoid-non-const-global-variables)
// List of jobs
std::list<lisp::job::job_t> joblist;
// List of collected jobs
std::list<lisp::job::job_t> cjoblist;
// NOLINTEND(cppcoreguidelines-avoid-non-const-global-variables)
} // namespace

namespace lisp::job
{
///
/// @brief printjob Print the job information.
///
/// @details Prints the job information pointed to by JOB in the following format:
/// [n]   Status (other info)  (command line)
/// Where 'n' is the job number.
/// Status is either
/// - Running if the job is still running.
/// - Done if job has exited.
/// - The signal name if the job is stopped or received a signal.
/// If the job produced a core dump that information is printed.
/// Finally, the command or expression is printed.
///
/// @param job The job.
///
void printjob(const job_t& job)
{
  std::string buffer = fmt::format("[{}]  {} ", job.jobnum, job.procid);
  if(job.running)
    buffer += "Running";
  else if(WIFEXITED(job.status))
    buffer += "Done";
  else if(WIFSTOPPED(job.status))
    buffer += strsignal(WSTOPSIG(job.status));
  else
  {
    buffer += strsignal(WTERMSIG(job.status));
    if(WCOREDUMP(job.status))
      buffer += " (core dumped)";
  }
  buffer += "\t";
  primout()->format(buffer);
  print(job.exp, io::output::PRIMARY);
}

///
/// @brief createjob Create a job_t structure and initialize it.
///
/// @param pid The process id.
///
/// @return An initialized job_t structure.
///
job_t createjob(int pid)
{
  job_t job;
  if(!joblist.empty())
    job.jobnum = joblist.front().jobnum + 1;
  else
    job.jobnum = 1;
  job.procid = pid;
  job.status = 0;
  job.wdir = std::filesystem::current_path().string();
  job.exp = top::input_exp;
  job.running = true;
  return job;
}

///
/// @brief recordjob Register a job.
///
/// @details Register a job with process id PID in the linked list of jobs. If
/// BG is true, job is registered as running in background.
///
/// @param pid Process id.
/// @param bg True if registering a background process.
///
/// @return True if all went well, false otherwise.
///
void recordjob(job_t job, bool bg)
{
  job.background = bg;
  joblist.push_front(job);
}

///
/// @brief collectjob Update job list with exit status.
///
/// @details Updates job list with PID as process id, and STAT as exit status.
///
/// @param pid Process id.
/// @param stat Exit status.
///
void collectjob(int pid, stat_t stat)
{
  for(auto job = joblist.begin(); job != joblist.end(); ++job)
  {
    if(job->procid == pid)
    {
      job->running = false;
      job->status = stat.stat;
      if(WIFSTOPPED(job->status))
        printjob(*job);
      else
      {
        if(!job->background && WIFSIGNALED(job->status) && WTERMSIG(job->status) != SIGINT)
          printjob(*job);   // Print if not interrupted
        if(job->background) // When running in background, save on another list to be
        {
          // Collected when signaled with SIGCHLD
          cjoblist.push_front(*job);
        }
        job = joblist.erase(job);
      }
      break;
    }
  }
}

/// @brief printdone Sweeps CJOBLIST and prints each job it frees.
void printdone()
{
  for(const auto& job: cjoblist)
    printjob(job);
  cjoblist.clear();
}

void printjobs()
{
  for(const auto& job: joblist)
    printjob(job);
}

job_t* findjob(std::function<bool(const job_t&)> f)
{
  for(auto& j: joblist)
  {
    if(f(j))
      return &j;
  }
  return nullptr;
}

} // namespace lisp::job
