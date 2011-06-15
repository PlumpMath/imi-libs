#ifndef FB_BMAP_H
#define FB_BMAP_H

#define PIXEL_SIZE (sizeof(char) * 4)
#define R 2
#define G 1
#define B 0
#define A 3

#include <stddef.h>

typedef struct
{
    size_t width, height;
    size_t off_x, off_y;
    size_t max_x, max_y;
    char sub;
    char* data;
} bmap;

bmap* new_bmap  (size_t width, size_t height);
bmap* sub_bmap (bmap* obj, size_t off_x, size_t off_y, size_t max_x, size_t max_y);
void  free_bmap (bmap* obj);

size_t bmap_location (bmap* obj, int x, int y);
char*  bmap_pixel    (bmap* obj, int x, int y);

void bmap_write_color_a (char *pixel, char r, char g, char b, double a);
void bmap_write_color (char *pixel, char r, char g, char b);

void bmap_set_pixel_a (bmap* obj, int x, int y,
                     char r, char g, char b, double a);
void bmap_set_pixel (bmap* obj, int x, int y,
                     char r, char g, char b);
void bmap_get_pixel (bmap* obj, int x, int y,
                     char* r, char* g, char* b);

#endif
