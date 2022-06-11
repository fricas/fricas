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
  CFStringAppendCString(path, "/usr/local/", encoding);

  setenv("FRICAS_PREFIX", CFStringGetCStringPtr(path, encoding), 1);

  system("open -a Terminal.app"
         " -n --env FRICAS_PREFIX=\"${FRICAS_PREFIX}\""
         " \"${FRICAS_PREFIX}/bin/fricas\"");
  return 0;
}
