#ifndef STRUTIL_INCLUDED
#define STRUTIL_INCLUDED 1

#include <string.h>
#include <assert.h>
#define fricas_sprintf_to_buf1(buff, format, arg1) \
  do { \
     size_t fricas_sprintf_buffer_size = sizeof(buff); \
     int fricas_sprintf_actual_size = \
        snprintf(buff, fricas_sprintf_buffer_size, format, arg1); \
     assert(fricas_sprintf_actual_size >= 0 && \
            fricas_sprintf_actual_size < fricas_sprintf_buffer_size); \
  } while(0)

#define fricas_sprintf_to_buf2(buff, format, arg1, arg2) \
  do { \
     size_t fricas_sprintf_buffer_size = sizeof(buff); \
     int fricas_sprintf_actual_size = \
        snprintf(buff, fricas_sprintf_buffer_size, format, arg1, arg2); \
     assert(fricas_sprintf_actual_size >= 0 && \
            fricas_sprintf_actual_size < fricas_sprintf_buffer_size); \
  } while(0)

#define fricas_sprintf_to_buf3(buff, format, arg1, arg2, arg3) \
  do { \
     size_t fricas_sprintf_buffer_size = sizeof(buff); \
     int fricas_sprintf_actual_size = \
        snprintf(buff, fricas_sprintf_buffer_size, format, arg1, arg2, arg3); \
     assert(fricas_sprintf_actual_size >= 0 && \
            fricas_sprintf_actual_size < fricas_sprintf_buffer_size); \
  } while(0)

#endif
