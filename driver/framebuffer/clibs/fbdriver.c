#include "fbdriver.h"

#include <malloc.h>

bmap*
fbdev_open (char* fbfile)
{
    fbfd = open (fbfile, O_RDWR);
    if (!fbfd)
        return -1;

    struct fb_var_screeninfo vinfo;
    struct fb_fix_screeninfo finfo;

    if (ioctl(fbfd, FBIOGET_FSCREENINFO, &finfo))
        return -2;

    if (ioctl(fbfd, FBIOGET_VSCREENINFO, &vinfo))
        return -3;

    long screensize = vinfo.xres * vinfo.yres * vinfo.bits_per_pixel / 8;

    char* fb = (char *)mmap (0, screensize, PROT_READ | PROT_WRITE, MAP_SHARED, fbfd, 0);
    if ((int)fb == -1)
        return -4;

    bmap *fbdev = malloc(sizeof(bmap));
    fbdev->width = vinfo.xres;
    fbdev->height = vinfo.yres;
    fbdev->data = fb;
    return fbdev;
}

void
fbdev_close (bmap* fb)
{
    munmap (fb->data, fb->width * fb->height * PIXEL_SIZE);
    close (fbfd);
    free (fb);
}
