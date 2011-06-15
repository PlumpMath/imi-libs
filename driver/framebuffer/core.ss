(library (imi driver framebuffer core)
  (export make-bmap
          bmap-width
          bmap-height
          bmap-off-x
          bmap-off-y
          bmap-max-x
          bmap-max-y
          bmap-sub
          bmap-data

          new-bmap
          sub-bmap
          free-bmap

          bmap-set-pixel
          bmap-get-pixel
          
          fbdev-init-return-information
          fbdev-open
          fbdev-close
          
          fbgraphics-move
          fbgraphics-rect
          fbgraphics-filled-rect
          fbgraphics-circle
          fbgraphics-filled-circle
          fbgraphics-line)
  (import (imi foreign import-c-library))

  (import-c-library (imi driver framebuffer clibs libfbgraphics)

    ;;; clibs/bmap.h

    (struct bmap
      (uint width height)
      (uint off-x off-y)
      (uint max-x max-y)
      (char sub)
      (pointer data))


    ; bmap* new_bmap (size_t width, size_t height)
    (pointer new_bmap (uint uint))

    ; bmap* sub_bmap (bmap* obj, size_t off_x, size_t off_y, size_t max_x, size_t max_y)
    (pointer sub_bmap (pointer uint uint uint uint))

    ; void free_bmap (bmap* obj)
    (void free_bmap (pointer))


    ; void bmap_set_pixel (bmap* obj, int x, int y
    ;                      char a, char g, char b)
    (void bmap_set_pixel (pointer int int
                                  char char char))

    ; void bmap_get_pixel (bmap* obj, int x, int y
    ;                      char* a, char* g, char* b)
    (void bmap_get_pixel (pointer int int
                                  pointer pointer pointer))



    ;;; clibs/fbdriver.h

    ; static const char* fbdev_init_return_information[]
    (pointer * fbdev_init_return_information)

    ; bmap* fbdev_open(char* fbfile)
    (pointer fbdev_open (pointer))

    ; void fbdev_close(bmap* fb)
    (void fbdev_close (pointer))



    ;;; clibs/fbgraphics.h

    ; void fbgraphics_move (bmap* obj,
    ;                       int left, int top,
    ;                       int right, int bottom,
    ;                       int xdir, int ydir)
    (void fbgraphics_move (pointer int int int int int int))

    ; void fbgraphics_rect (bmap* obj,
    ;                       int left, int top,
    ;                       int right, int bottom,
    ;                       char r, char g, char b)
    (void fbgraphics_rect (pointer int int int int char char char))

    ; void fbgraphics_filled_rect (bmap* obj,
    ;                              int left, int top,
    ;                              int right, int bottom,
    ;                              char r, char g, char b)
    (void fbgraphics_filled_rect (pointer int int int int char char char))

    ; void fbgraphics_circle (bmap* obj,
    ;                         int left, int top,
    ;                         int right, int bottom,
    ;                         char r, char g, char b)
    (void fbgraphics_circle (pointer int int int int char char char))

    ; void fbgraphics_filled_circle (bmap* obj,
    ;                                int left, int top,
    ;                                int right, int bottom,
    ;                                char r, char g, char b)
    (void fbgraphics_filled_circle (pointer int int int int char char char))

    ; void fbgraphics_line (bmap* obj,
    ;                       int x0, int y0,
    ;                       int x1, int y1,
    ;                       char r, char g, char b)
    (void fbgraphics_line (pointer int int int int char char char))

    )

  )
