#ifndef FB_FBGRAPHICS_H
#define FB_FBGRAPHICS_H

#include "bmap.h"

void fbgraphics_move (bmap* obj,
                      int left, int top,
                      int right, int bottom,
                      int xdir, int ydir);

void fbgraphics_rect (bmap* obj,
                      int left, int top,
                      int right, int bottom,
                      char r, char g, char b);

void fbgraphics_filled_rect (bmap* obj,
                             int left, int top,
                             int right, int bottom,
                             char r, char g, char b);

void fbgraphics_circle (bmap* obj,
                        int left, int top,
                        int right, int bottom,
                        char r, char g, char b);

void fbgraphics_filled_circle (bmap* obj,
                               int left, int top,
                               int right, int bottom,
                               char r, char g, char b);

void fbgraphics_line (bmap* obj,
                      int x0, int y0,
                      int x1, int y1,
                      char r, char g, char b);

#endif
