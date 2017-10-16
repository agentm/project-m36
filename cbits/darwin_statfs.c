#include <sys/param.h>
#include <sys/mount.h>
#include <stdio.h>

#ifndef _PROJECT_M36_STATFS_
#define _PROJECT_M36_STATFS_

int cDarwinFSJournaled(const char* path)
{
  struct statfs s = {0};
  int ret = statfs(path, &s);
  if(ret < 0) 
    {
      /* error */
      return ret;
    }
  else
    {
      return s.f_flags & MNT_JOURNALED;
    }
}
#endif
