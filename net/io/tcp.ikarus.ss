(library (imi net io tcp)
  (export tcp-connect
          tcp-server-socket
          accept-connection
          close-tcp-server-socket)
  (import (ikarus ipc)))
