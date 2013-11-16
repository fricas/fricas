#include <stdlib.h>
#include <CoreFoundation/CoreFoundation.h>

int main(int argc, const char *argv[]) {
  CFBundleRef bundle;
  CFMutableStringRef path;
  CFStringRef prefix;
  CFStringRef resources;
  CFStringEncoding encoding;

  bundle = CFBundleGetMainBundle();
  prefix = CFURLCopyFileSystemPath(
               CFBundleCopyBundleURL(bundle),
               kCFURLPOSIXPathStyle);
  resources = CFURLCopyFileSystemPath(
                  CFBundleCopyResourcesDirectoryURL(bundle),
                  kCFURLPOSIXPathStyle);
  encoding = CFStringGetSystemEncoding();

  path = CFStringCreateMutable(kCFAllocatorDefault, 0);

  CFStringAppend(path, prefix);
  CFStringAppendCString(path, "/", encoding);
  CFStringAppend(path, resources);

  setenv("FRICAS_PREFIX", CFStringGetCStringPtr(path, encoding), 1);

  system("/usr/X11/bin/xterm"
         " -e \"${FRICAS_PREFIX}/bin/fricas\"");
  return 0;
}
