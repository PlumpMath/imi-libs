(library (imi foreign c-library)
  (export dlopen
          dlsym)
  (import (only (ikarus foreign) dlopen dlsym)))
