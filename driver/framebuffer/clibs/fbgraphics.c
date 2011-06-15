#include "fbgraphics.h"

#include <malloc.h>
#include <string.h>
#include <math.h>

void
fbgraphics_move (bmap* obj,
                 int left, int top,
                 int right, int bottom,
                 int xdir, int ydir)
{
    size_t line_length;

    if (left + xdir > obj->max_x)
        xdir = obj->max_x - left;
    else if (left < xdir)
        xdir = left;

    if (top + ydir > obj->max_y)
        ydir = obj->max_y - top;
    else if (top < ydir)
        ydir = top;

    if (right + xdir > obj->max_x)
        right = obj->max_x - xdir;
    else if (right < xdir)
        return;

    if (top + ydir > obj->max_y)
        top = obj->max_y - ydir;
    else if (top < ydir)
        ydir = top;

    if (right > left)
        return;
    if (top > bottom)
        return;

    line_length = PIXEL_SIZE * (right - left);

    if (ydir < 0)
    {
        for (int y = top; y <= bottom; y++)
        {
            memcpy (bmap_pixel (obj, left+xdir, y+ydir),
                    bmap_pixel (obj, left, y),
                    line_length);
        }
    }
    else if (ydir == 0)
    {
        char* temparray = (char *)malloc (line_length);
        for (int y = top; y <= bottom; y++)
        {
            memcpy (temparray,
                    bmap_pixel (obj, left, y),
                    line_length);
            memcpy (bmap_pixel (obj, left+xdir, y),
                    temparray,
                    line_length);
        }
        free (temparray);
    }
    else
    {
        for (int y = bottom; y >= top; y--)
        {
            memcpy (bmap_pixel (obj, left+xdir, y+ydir),
                    bmap_pixel (obj, left, y),
                    line_length);
        }
    }
}

void
fbgraphics_rect (bmap* obj,
                 int left, int top,
                 int right, int bottom,
                 char r, char g, char b)
{
    int l = left < 0 ? 0 : left,
        t = top < 0 ? 0 : top,
        e = right > obj->max_x ? obj->max_x : right,
        d = bottom > obj->max_y ? obj->max_y : bottom;

    if (left >= 0)
    {
        if (right <= obj->max_x)
        {
            for (int y = t; y <= d; y++)
            {
                bmap_set_pixel (obj, l, y, r, g, b);
                bmap_set_pixel (obj, e, y, r, g, b);
            }
        }
        else
        {
            for (int y = t; y <= d; y++)
            {
                bmap_set_pixel (obj, l, y, r, g, b);
            }
        }
    }
    else
    {
        if (right <= obj->max_x)
        {
            for (int y = t; y <= d; y++)
            {
                bmap_set_pixel (obj, e, y, r, g, b);
            }
        }
    }

    if (top >= 0)
    {
        if (bottom <= obj->max_y)
        {
            for (int x = l; x <= e; x++)
            {
                bmap_set_pixel (obj, x, t, r, g, b);
                bmap_set_pixel (obj, x, d, r, g, b);
            }
        }
        else
        {
            for (int x = l; x <= e; x++)
            {
                bmap_set_pixel (obj, x, t, r, g, b);
            }
        }
    }
    else
    {
        if (bottom <= obj->max_y)
        {
            for (int x = l; x <= e; x++)
            {
                bmap_set_pixel (obj, x, d, r, g, b);
            }
        }
    }
}

void
fbgraphics_filled_rect (bmap* obj,
                        int left, int top,
                        int right, int bottom,
                        char r, char g, char b)
{
    size_t line_length;
    char *color = (char *)malloc (PIXEL_SIZE);

    bmap_write_color (color, r, g, b);

    left = left < 0 ? 0 : left;
    top = top < 0 ? 0 : top;
    right = right > obj->max_x ? obj->max_x : right;
    bottom = bottom > obj->max_y ? obj->max_y : bottom;

    if (left > right || top > bottom)
        return;

    line_length = PIXEL_SIZE * (right - left);

    for (int x = left; x <= right; x++)
    {
        memcpy (bmap_pixel (obj, x, top),
                color,
                PIXEL_SIZE);
    }
    free (color);
    color = bmap_pixel (obj, left, top);
    for (int y = top + 1; y <= bottom; y++)
    {
        memcpy (bmap_pixel (obj, left, y),
                color,
                line_length);
    }
}

void
fbgraphics_circle (bmap* obj,
                   int left, int top,
                   int right, int bottom,
                   char r, char g, char b)
{
    //TODO
}

void
fbgraphics_filled_circle (bmap* obj,
                          int left, int top,
                          int right, int bottom,
                          char r, char g, char b)
{
    //TODO
}

void
fbgraphics_line (bmap* obj,
                 int x0, int y0,
                 int x1, int y1,
                 char r, char g, char b)
{
    double xpitch = (double) (y1-y0) / (x1-x0);
    double ypitch = (double) (x1-x0) / (y1-y0);

    //TODO xpitch > 1 => use ypitch
}
