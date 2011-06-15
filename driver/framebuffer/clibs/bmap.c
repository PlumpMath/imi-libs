#include "bmap.h"

#include <malloc.h>

bmap*
new_bmap (size_t width, size_t height)
{
    bmap* obj = malloc (sizeof(bmap));
    obj->data = malloc (PIXEL_SIZE * width * height);
    obj->width = width;
    obj->height = height;
    obj->sub = 0;
    obj->off_x = 0;
    obj->off_y = 0;
    obj->max_x = width;
    obj->max_y = height;
    return obj;
}

bmap*
sub_bmap (bmap* super, size_t off_x, size_t off_y, size_t max_x, size_t max_y)
{
    bmap* obj = malloc (sizeof(bmap));
    obj->data = super->data;
    obj->width = super->width;
    obj->height = super->height;
    obj->sub = 1;
    obj->off_x = super->off_x + off_x;
    obj->off_y = super->off_y + off_y;
    obj->max_x = max_x - off_x;
    obj->max_y = max_y - off_y;
    return obj;
}

void
free_bmap (bmap* obj)
{
    if (! obj->sub)
        free (obj->data);
    free (obj);
}


size_t
bmap_location (bmap* obj, int x, int y)
{
    return ((x + obj->off_x) +
            (y + obj->off_y) * obj->width)
           * PIXEL_SIZE;
}

char*
bmap_pixel (bmap* obj, int x, int y)
{
    return obj->data + bmap_location (obj, x, y);
}

void
bmap_write_color_a (char *pixel, char r, char g, char b, double a)
{
    double ia = 1 - a;
    pixel[R] = (r * a) + (pixel[R] * ia);
    pixel[G] = (g * a) + (pixel[G] * ia);
    pixel[B] = (b * a) + (pixel[B] * ia);
}

void
bmap_write_color (char *pixel, char r, char g, char b)
{
    pixel[R] = r;
    pixel[G] = g;
    pixel[B] = b;
}

void
bmap_set_pixel_a (bmap* obj, int x, int y, char r, char g, char b, double a)
{
    bmap_write_color_a (bmap_pixel (obj, x, y),
                        r, g, b, a);
}

void
bmap_set_pixel (bmap* obj, int x, int y, char r, char g, char b)
{
    bmap_write_color (bmap_pixel (obj, x, y),
                      r, g, b);
}

void
bmap_get_pixel (bmap* obj, int x, int y, char* r, char* g, char* b)
{
    char *pixel = bmap_pixel (obj, x, y);

    *r = pixel[R];
    *g = pixel[G];
    *b = pixel[B];
}
