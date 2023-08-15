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

namespace lisp::job
{
static std::list<job_t> joblist;  // List of jobs
static std::list<job_t> cjoblist; // List of collected jobs

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
  else if(WIFEXITED(job.status)) // NOLINT
    buffer += "Done";
  else if(WIFSTOPPED(job.status))              // NOLINT
    buffer += strsignal(WSTOPSIG(job.status)); // NOLINT
  else
  {
    buffer += strsignal(WTERMSIG(job.status)); // NOLINT
    if(WCOREDUMP(job.status))                  // NOLINT
      buffer += " (core dumped)";
  }
  buffer += "\t";
  primout()->format(buffer);
  print(job.exp, false);
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

}
