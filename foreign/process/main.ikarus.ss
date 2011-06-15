(library (imi foreign process)
  (export system
          process
          process-nonblocking
          waitpid
          wstatus-pid
          wstatus-exit-status
          wstatus-received-signal
          kill)
  (import (ikarus ipc))

  )
